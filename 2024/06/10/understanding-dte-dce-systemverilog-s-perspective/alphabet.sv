interface uart_io;
    logic dcd;
    logic rxd;
    logic txd;
    logic dtr;
    logic dsr;
    logic rts;
    logic cts;
    logic ri;

    modport dte (
        input dcd,
        input rxd,
        output txd,
        output dtr,
        input dsr,
        output rts,
        input cts,
        input ri
    );

    modport dce (
        output dcd,
        output rxd,
        input txd,
        input dtr,
        output dsr,
        input rts,
        output cts,
        output ri,
        import terminate
    );

    function terminate;
        dcd = 1'b0;
        rxd = 1'b0;
        dsr = 1'b0;
        cts = 1'b0;
    endfunction: terminate
endinterface: uart_io

module null_modem (uart_io a, uart_io b);
    uart_io dangling ();
    uart_mux mux (a, b, dangling, 1'b0);
endmodule: null_modem

module uart_mux (
    uart_io.dce x,
    uart_io.dce a,
    uart_io.dce b,
    input logic select_i
);
    always_comb begin
        x.rxd = (select_i == 1'b0) ? a.txd : b.txd;
        x.cts = (select_i == 1'b0) ? a.rts : b.rts;
        x.dsr = (select_i == 1'b0) ? a.dtr : b.dtr;
        x.dcd = (select_i == 1'b0) ? a.dsr : b.dsr;
        if (select_i == 1'b0) begin
            a.rxd = x.txd;
            a.cts = x.rts;
            a.dsr = x.dtr;
            a.dcd = x.dsr;
            b.terminate();
        end
        else begin
            b.rxd = x.txd;
            b.cts = x.rts;
            b.dsr = x.dtr;
            b.dcd = x.dsr;
            a.terminate();
        end
    end
endmodule: uart_mux

module uart_switch (
    input logic clock_i,
    input logic reset_ni,
    input logic key_i,
    uart_io.dce pc,
    uart_io.dce a,
    uart_io.dce b
);
    logic index = 1'b0;

    logic last_key = 1'b0;
    always @ (posedge clock_i, negedge reset_ni) begin
        if (!reset_ni) begin
            index <= 1'b0;
        end
        else begin
            if (key_i && !last_key) begin
                index <= !index;
            end
            last_key <= key_i;
        end
    end

    uart_mux mux (pc, a, b, index);
endmodule: uart_switch

module pulser #(
    parameter longint unsigned CYCLES = 'd1,
    parameter longint unsigned KEEP = 'd1,
    parameter longint unsigned INITIAL_PHASE = CYCLES - 'd1
) (
    input logic clock_i,
    input logic clock_enable_i,
    input logic reset_ni,
    output logic pulse_o
);
    localparam WIDTH = $clog2(CYCLES);

    bit [WIDTH-1:0] counter = INITIAL_PHASE;
    always_ff @ (posedge clock_i, negedge reset_ni) begin
        if (!reset_ni) begin
            counter <= INITIAL_PHASE;
        end
        else if (clock_enable_i) begin
            counter <= (counter == CYCLES - 'd1) ? 0 : (counter + 1);
        end
    end

    assign pulse_o = (counter < KEEP);
endmodule: pulser

module uart_tx_fsm (
    input logic clock_i,
    input logic clock_enable_i,
    input logic reset_ni,
    input logic load_i,
    input byte char_i,
    output logic idle_o,
    output logic baud_o
);
    localparam unsigned MAX = $size(char_i) - 1;

    typedef enum {
        STATE_IDLE,
        STATE_START,
        STATE_DATA
    } state_t;
    state_t state = STATE_IDLE;

    bit [2:0] counter = 0;
    byte char;

    assign idle_o = (state == STATE_IDLE);
    always_comb begin
        unique case (state)
            STATE_IDLE: begin
                baud_o = 1'b1;
            end
            STATE_START: begin
                baud_o = 1'b0;
            end
            STATE_DATA: begin
                baud_o = char[0];
            end
        endcase
    end

    always @ (posedge clock_i, negedge reset_ni) begin
        if (!reset_ni) begin
            state <= STATE_IDLE;
            counter <= 'd0;
        end
        else if (clock_enable_i) begin
            unique case (state)
                STATE_IDLE: begin
                    if (load_i) begin
                        state <= STATE_START;
                        char <= char_i;
                        counter <= 'd0;
                    end
                end
                STATE_START: begin
                    state <= STATE_DATA;
                end
                STATE_DATA: begin
                    if (counter == MAX) begin
                        state <= STATE_IDLE;
                    end
                    else begin
                        counter <= counter + 3'd1;
                        char <= { 1'b0, char[7:1] };
                    end
                end
            endcase
        end
    end
endmodule: uart_tx_fsm

module alphabet_generator #(
    parameter byte BASE = "A",
    parameter longint unsigned CLOCK = 12_000_000,
    parameter longint unsigned DIVISOR = (CLOCK / 9_600)
) (
    input logic clock_i,
    input logic reset_ni,
    uart_io.dte uart
);
    logic ticker_enable;
    pulser #(.CYCLES(CLOCK)) ticker (
        .clock_i(clock_i),
        .clock_enable_i(1'b1),
        .reset_ni(reset_ni),
        .pulse_o(ticker_enable)
    );

    logic brg_enable;
    pulser #(.CYCLES(DIVISOR)) brg (
        .clock_i(clock_i),
        .clock_enable_i(1'b1),
        .reset_ni(reset_ni),
        .pulse_o(brg_enable)
    );

    bit [4:0] counter = 'd0;
    bit request = 1'b0;

    logic load = 1'b0;

    always @ (posedge clock_i, negedge reset_ni) begin
        if (!reset_ni) begin
            request <= 1'b0;
            counter <= 'd0;
        end
        else if (ticker_enable) begin
            if (counter == 'd25) begin
                counter <= 'd0;
            end
            else begin
                counter <= counter + 5'd1;
            end
            request <= 1'b1;
        end
        else if (brg_enable) begin
            load <= request;
            request <= 1'b0;
        end
    end

    byte char;
    assign char = BASE + counter;

    uart_tx_fsm fsm (
        .clock_i(clock_i),
        .clock_enable_i(brg_enable),
        .reset_ni(reset_ni),
        .load_i(load),
        .char_i(char),
        .baud_o(uart.txd)
    );
endmodule: alphabet_generator
 
module top (
    input logic clock_i,
    input logic reset_ni,
    input logic key_i,
    uart_io.dce uart
);
    uart_io uart_uppercase ();
    alphabet_generator #(.BASE("A")) uppercase (
        .clock_i(clock_i),
        .reset_ni(reset_ni),
        .uart(uart_uppercase)
    );
    uart_io uart_lowercase ();
    alphabet_generator #(.BASE("a")) lowercase (
        .clock_i(clock_i),
        .reset_ni(reset_ni),
        .uart(uart_lowercase)
    );
    uart_switch switch (
        .clock_i(clock_i),
        .reset_ni(reset_ni),
        .key_i(key_i),
        .pc(uart),
        .a(uart_uppercase),
        .b(uart_lowercase)
    );
endmodule: top