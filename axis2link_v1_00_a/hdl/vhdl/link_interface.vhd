------------------------------------------------------------------------------
-- link_interface.vhd - entity/architecture pair
------------------------------------------------------------------------------
--
-- ***************************************************************************
-- ** Copyright (c) 1995-2012 Xilinx, Inc.  All rights reserved.            **
-- **                                                                       **
-- ** Xilinx, Inc.                                                          **
-- ** XILINX IS PROVIDING THIS DESIGN, CODE, OR INFORMATION "AS IS"         **
-- ** AS A COURTESY TO YOU, SOLELY FOR USE IN DEVELOPING PROGRAMS AND       **
-- ** SOLUTIONS FOR XILINX DEVICES.  BY PROVIDING THIS DESIGN, CODE,        **
-- ** OR INFORMATION AS ONE POSSIBLE IMPLEMENTATION OF THIS FEATURE,        **
-- ** APPLICATION OR STANDARD, XILINX IS MAKING NO REPRESENTATION           **
-- ** THAT THIS IMPLEMENTATION IS FREE FROM ANY CLAIMS OF INFRINGEMENT,     **
-- ** AND YOU ARE RESPONSIBLE FOR OBTAINING ANY RIGHTS YOU MAY REQUIRE      **
-- ** FOR YOUR IMPLEMENTATION.  XILINX EXPRESSLY DISCLAIMS ANY              **
-- ** WARRANTY WHATSOEVER WITH RESPECT TO THE ADEQUACY OF THE               **
-- ** IMPLEMENTATION, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OR        **
-- ** REPRESENTATIONS THAT THIS IMPLEMENTATION IS FREE FROM CLAIMS OF       **
-- ** INFRINGEMENT, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       **
-- ** FOR A PARTICULAR PURPOSE.                                             **
-- **                                                                       **
-- ***************************************************************************
--
------------------------------------------------------------------------------
-- Filename:          link_interface.vhd
-- Version:           1.00.a
-- Description:       User logic.
-- Date:              Tue May 08 15:32:08 2018 (by Create and Import Peripheral Wizard)
-- VHDL Standard:     VHDL'93
------------------------------------------------------------------------------
-- Naming Conventions:
--   active low signals:                    "*_n"
--   clock signals:                         "clk", "clk_div#", "clk_#x"
--   reset signals:                         "rst", "rst_n"
--   generics:                              "C_*"
--   user defined types:                    "*_TYPE"
--   state machine next state:              "*_ns"
--   state machine current state:           "*_cs"
--   combinatorial signals:                 "*_com"
--   pipelined or register delay signals:   "*_d#"
--   counter signals:                       "*cnt*"
--   clock enable signals:                  "*_ce"
--   internal version of output port:       "*_i"
--   device pins:                           "*_pin"
--   ports:                                 "- Names begin with Uppercase"
--   processes:                             "*_PROCESS"
--   component instantiations:              "<ENTITY_>I_<#|FUNC>"
------------------------------------------------------------------------------

-- DO NOT EDIT BELOW THIS LINE --------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned."+";
use ieee.std_logic_unsigned.all;

library proc_common_v3_00_a;
use proc_common_v3_00_a.proc_common_pkg.all;

-- DO NOT EDIT ABOVE THIS LINE --------------------

--USER libraries added here

------------------------------------------------------------------------------
-- Entity section
------------------------------------------------------------------------------
-- Definition of Generics:
--   C_NUM_REG                    -- Number of software accessible registers
--   C_SLV_DWIDTH                 -- Slave interface data bus width
--
-- Definition of Ports:
--   Bus2IP_Clk                   -- Bus to IP clock
--   Bus2IP_Resetn                -- Bus to IP reset
--   Bus2IP_Data                  -- Bus to IP data bus
--   Bus2IP_BE                    -- Bus to IP byte enables
--   Bus2IP_RdCE                  -- Bus to IP read chip enable
--   Bus2IP_WrCE                  -- Bus to IP write chip enable
--   IP2Bus_Data                  -- IP to Bus data bus
--   IP2Bus_RdAck                 -- IP to Bus read transfer acknowledgement
--   IP2Bus_WrAck                 -- IP to Bus write transfer acknowledgement
--   IP2Bus_Error                 -- IP to Bus error response
------------------------------------------------------------------------------

entity link_interface is
  generic
  (
    -- ADD USER GENERICS BELOW THIS LINE ---------------
    --USER generics added here
    -- ADD USER GENERICS ABOVE THIS LINE ---------------
    C_FAMILY                       : string               := "spartan6";
    C_S_AXIS_FIFO_DEPTH            : integer              := 16;
    C_S_AXIS_CLK_FREQ_HZ           : integer              := 100_000_000;
    C_S_AXIS_TDATA_WIDTH           : integer              := 32;

    -- DO NOT EDIT BELOW THIS LINE ---------------------
    -- Bus protocol parameters, do not add to or delete
    C_NUM_REG                      : integer              := 4;
    C_SLV_DWIDTH                   : integer              := 32
    -- DO NOT EDIT ABOVE THIS LINE ---------------------
  );
  port
  (
    -- ADD USER PORTS BELOW THIS LINE ------------------
    --USER ports added here
    aresetn                        : in  std_logic;
    s_axis_aclk                    : in  std_logic;
    s_axis_tdata                   : in  std_logic_vector(C_S_AXIS_TDATA_WIDTH-1 downto 0) := (others => '0');
    s_axis_tvalid                  : in  std_logic:= '0';
    s_axis_tready                  : out std_logic;
    
    Lx_DAT                         : out std_logic_vector(7 downto 0);
    Lx_ACK                         : in std_logic;
    Lx_CLK                         : out std_logic;
    
    -- ADD USER PORTS ABOVE THIS LINE ------------------
    

    -- DO NOT EDIT BELOW THIS LINE ---------------------
    -- Bus protocol ports, do not add to or delete
    Bus2IP_Clk                     : in  std_logic;
    Bus2IP_Resetn                  : in  std_logic;
    Bus2IP_Data                    : in  std_logic_vector(C_SLV_DWIDTH-1 downto 0);
    Bus2IP_BE                      : in  std_logic_vector(C_SLV_DWIDTH/8-1 downto 0);
    Bus2IP_RdCE                    : in  std_logic_vector(C_NUM_REG-1 downto 0);
    Bus2IP_WrCE                    : in  std_logic_vector(C_NUM_REG-1 downto 0);
    IP2Bus_Data                    : out std_logic_vector(C_SLV_DWIDTH-1 downto 0);
    IP2Bus_RdAck                   : out std_logic;
    IP2Bus_WrAck                   : out std_logic;
    IP2Bus_Error                   : out std_logic
    -- DO NOT EDIT ABOVE THIS LINE ---------------------
  );

  attribute MAX_FANOUT : string;
  attribute SIGIS : string;

  attribute SIGIS of Bus2IP_Clk    : signal is "CLK";
  attribute SIGIS of Bus2IP_Resetn : signal is "RST";

end entity link_interface;

------------------------------------------------------------------------------
-- Architecture section
------------------------------------------------------------------------------

architecture IMP of link_interface is
    
  type LINK_PORT_STATE_TYPE         is  (IDLE, LINK_PORT_ST_RD, READ_STRM_DATA, SEND_BYTE1_p, SEND_BYTE2_p, SEND_BYTE3_p, SEND_BYTE4_p,
                                        SEND_BYTE1_n, SEND_BYTE2_n, SEND_BYTE3_n, SEND_BYTE4_n);
  signal link_state                     : LINK_PORT_STATE_TYPE;
  --USER signal declarations added here, as needed for user logic
  signal areset                         : std_logic;
  signal fifo_wr_en                     : std_logic;
  signal rd_data                        : std_logic;
  signal s_axis_tready_o                : std_logic:= '0';
  signal counter                        : std_logic_vector(31 downto 0);
  signal clk_dev                        : std_logic:='0';
  signal Lx_DAT_out                     : std_logic_vector(7 downto 0):= x"00";
  signal Lx_CLK_out                     : std_logic:='0';
  signal Lx_ACK_i                       : std_logic;
  signal rd_strm_data                   : std_logic_vector(C_S_AXIS_TDATA_WIDTH-1 downto 0);
  signal send_link_data                 : std_logic;
  signal status_link_up                 : std_logic;
  signal send_lnk_data_count            : std_logic_vector(32-1 downto 0):= x"0000_0000";
  signal tresh_phase                    : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  ------------------------------------------
  -- Signals for user logic slave model s/w accessible register example
  ------------------------------------------
  signal init_reg                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal divide                         : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_reg2                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_reg3                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_reg_write_sel              : std_logic_vector(3 downto 0);
  signal slv_reg_read_sel               : std_logic_vector(3 downto 0);
  signal slv_ip2bus_data                : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_read_ack                   : std_logic;
  signal slv_write_ack                  : std_logic;

begin
  Lx_CLK <= Lx_CLK_out when ((status_link_up and send_link_data) = '1') else 
            '1' when ((status_link_up and (not send_link_data)) = '1') else '0';
  Lx_CLK_out <= s_axis_aclk when divide = x"0000_0001" else clk_dev;
  areset <= not aresetn;
  Lx_DAT  <= Lx_DAT_out;
  Lx_ACK_i <= Lx_ACK;
  --rd_data <= s_axis_tvalid and s_axis_tready_o;
 -- s_axis_tready <= s_axis_tready_o;
  tresh_phase <=('0' & divide(C_SLV_DWIDTH-1 downto 1)) - x"0000_0001";
  
  
  TREADY_PROC : process (aresetn, s_axis_aclk)
  begin
    if aresetn = '0' then
      s_axis_tready <= '0';
    elsif s_axis_aclk'event and s_axis_aclk = '1' then
        if s_axis_tready_o = '1' then
          s_axis_tready <= '1';
        else
          s_axis_tready <= '0';
        end if;
    end if;
    end process TREADY_PROC;
  
  --USER logic implementation added here
    READ_FIFO_DATA_PROC : process (aresetn, s_axis_aclk)
    begin
    if aresetn = '0' then
      rd_strm_data <= (others => '0');
    elsif s_axis_aclk'event and s_axis_aclk = '1' then
        if s_axis_tready_o = '1' then
          rd_strm_data <= s_axis_tdata;
        else
          rd_strm_data <= rd_strm_data;
        end if;
    end if;
    end process READ_FIFO_DATA_PROC;

    LINK_ST_PORT_PROCESS   :   process(aresetn,s_axis_aclk)
    begin
      if aresetn = '0' then
        link_state  <=  IDLE;
        elsif (s_axis_aclk'event and s_axis_aclk = '1') then
            case link_state is
              when IDLE => 
                  link_state <= LINK_PORT_ST_RD;
              when LINK_PORT_ST_RD => 
                if ((Lx_ACK_i = '1') and (s_axis_tvalid = '1')) then
                  link_state <= READ_STRM_DATA;
                else 
                  link_state <= LINK_PORT_ST_RD;
                end if;
              when READ_STRM_DATA =>
                  send_lnk_data_count <= x"0000_0000";
                  link_state <= SEND_BYTE1_p;
              when SEND_BYTE1_p =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE2_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE1_n;
                else 
                link_state <= SEND_BYTE1_p;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE1_n =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE2_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE2_p;
                else 
                link_state <= SEND_BYTE1_n;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE2_p =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE3_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE2_n;
                else 
                link_state <= SEND_BYTE2_p;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE2_n =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE3_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE3_p;
                else 
                link_state <= SEND_BYTE2_n;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE3_p =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE4_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE4_n;
                else 
                link_state <= SEND_BYTE3_p;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE3_n =>
                if divide = x"0000_0001" then 
                link_state <= SEND_BYTE4_p;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE4_p;
                else 
                link_state <= SEND_BYTE3_n;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE4_p =>
                if divide = x"0000_0001" then 
                link_state <= LINK_PORT_ST_RD;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= SEND_BYTE4_n;
                else 
                link_state <= SEND_BYTE4_p;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when SEND_BYTE4_n =>
                if divide = x"0000_0001" then 
                link_state <= LINK_PORT_ST_RD;
                elsif send_lnk_data_count = tresh_phase then
                send_lnk_data_count <= x"0000_0000";
                link_state <= LINK_PORT_ST_RD;
                else 
                link_state <= SEND_BYTE4_n;
                send_lnk_data_count <= send_lnk_data_count + x"0000_0001";
                end if;
              when others =>
              link_state <= IDLE;
            end case;
      end if;
    end process LINK_ST_PORT_PROCESS;
    
    LINK_PROCESS    :   process (link_state) is
    begin 
	   case link_state is
        when IDLE =>
          send_link_data <= '0';
          status_link_up <= '0';
        when LINK_PORT_ST_RD =>
          send_link_data <= '0';
          status_link_up <= '1';
        when READ_STRM_DATA =>
          s_axis_tready_o <= '1';
        when SEND_BYTE1_p =>
          send_link_data <= '1';
          clk_dev <= '1';
          Lx_DAT_out <= rd_strm_data(31 downto 24);
          s_axis_tready_o <= '0';
        when SEND_BYTE1_n =>
          send_link_data <= '1';
          clk_dev <= '0';
          Lx_DAT_out <= rd_strm_data(31 downto 24);
          s_axis_tready_o <= '0';
        when SEND_BYTE2_p =>
          clk_dev <= '1';
          Lx_DAT_out <= rd_strm_data(23 downto 16);
        when SEND_BYTE2_n =>
          clk_dev <= '0';
          Lx_DAT_out <= rd_strm_data(23 downto 16);
        when SEND_BYTE3_p =>
          clk_dev <= '1';
          Lx_DAT_out <= rd_strm_data(15 downto 8);
        when SEND_BYTE3_n =>
          clk_dev <= '0';
          Lx_DAT_out <= rd_strm_data(15 downto 8);
        when SEND_BYTE4_p =>
          clk_dev <= '1';
          Lx_DAT_out <= rd_strm_data(7 downto 0);
        when SEND_BYTE4_n =>
          clk_dev <= '0';
          Lx_DAT_out <= rd_strm_data(7 downto 0);
        when others =>
          send_link_data <= '0';
          status_link_up <= '0';
      end case;
    end process LINK_PROCESS;
    
  ------------------------------------------
  -- Example code to read/write user logic slave model s/w accessible registers
  -- 
  -- Note:
  -- The example code presented here is to show you one way of reading/writing
  -- software accessible registers implemented in the user logic slave model.
  -- Each bit of the Bus2IP_WrCE/Bus2IP_RdCE signals is configured to correspond
  -- to one software accessible register by the top level template. For example,
  -- if you have four 32 bit software accessible registers in the user logic,
  -- you are basically operating on the following memory mapped registers:
  -- 
  --    Bus2IP_WrCE/Bus2IP_RdCE   Memory Mapped Register
  --                     "1000"   C_BASEADDR + 0x0
  --                     "0100"   C_BASEADDR + 0x4
  --                     "0010"   C_BASEADDR + 0x8
  --                     "0001"   C_BASEADDR + 0xC
  -- 
  ------------------------------------------
  slv_reg_write_sel <= Bus2IP_WrCE(3 downto 0);
  slv_reg_read_sel  <= Bus2IP_RdCE(3 downto 0);
  slv_write_ack     <= Bus2IP_WrCE(0) or Bus2IP_WrCE(1) or Bus2IP_WrCE(2) or Bus2IP_WrCE(3);
  slv_read_ack      <= Bus2IP_RdCE(0) or Bus2IP_RdCE(1) or Bus2IP_RdCE(2) or Bus2IP_RdCE(3);

  -- implement slave model software accessible register(s)
  SLAVE_REG_WRITE_PROC : process( Bus2IP_Clk ) is
  begin

    if Bus2IP_Clk'event and Bus2IP_Clk = '1' then
      if Bus2IP_Resetn = '0' then
        init_reg <= x"AD05_2018"; 
        divide <= x"0000_0001"; 
        slv_reg2 <= (others => '0');
        slv_reg3 <= (others => '0');
      else
        case slv_reg_write_sel is
          when "1000" =>
            init_reg <= init_reg; 
          when "0100" =>
          if Bus2IP_Data /= x"0000_0000" then 
            for byte_index in 0 to (C_SLV_DWIDTH/8)-1 loop
              if ( Bus2IP_BE(byte_index) = '1' ) then
                divide(byte_index*8+7 downto byte_index*8) <= Bus2IP_Data(byte_index*8+7 downto byte_index*8);
              end if;
            end loop;
          else 
            divide <= divide; 
          end if;
          when "0010" =>
            for byte_index in 0 to (C_SLV_DWIDTH/8)-1 loop
              if ( Bus2IP_BE(byte_index) = '1' ) then
                slv_reg2(byte_index*8+7 downto byte_index*8) <= Bus2IP_Data(byte_index*8+7 downto byte_index*8);
              end if;
            end loop;
          when "0001" =>
            for byte_index in 0 to (C_SLV_DWIDTH/8)-1 loop
              if ( Bus2IP_BE(byte_index) = '1' ) then
                slv_reg3(byte_index*8+7 downto byte_index*8) <= Bus2IP_Data(byte_index*8+7 downto byte_index*8);
              end if;
            end loop;
          when others => null;
        end case;
      end if;
    end if;

  end process SLAVE_REG_WRITE_PROC;

  -- implement slave model software accessible register(s) read mux
  SLAVE_REG_READ_PROC : process( slv_reg_read_sel, init_reg, divide, slv_reg2, slv_reg3 ) is
  begin

    case slv_reg_read_sel is
      when "1000" => slv_ip2bus_data <= init_reg;
      when "0100" => slv_ip2bus_data <= divide;
      when "0010" => slv_ip2bus_data <= slv_reg2;
      when "0001" => slv_ip2bus_data <= slv_reg3;
      when others => slv_ip2bus_data <= (others => '0');
    end case;

  end process SLAVE_REG_READ_PROC;

  ------------------------------------------
  -- Example code to drive IP to Bus signals
  ------------------------------------------
  IP2Bus_Data  <= slv_ip2bus_data when slv_read_ack = '1' else
                  (others => '0');

  IP2Bus_WrAck <= slv_write_ack;
  IP2Bus_RdAck <= slv_read_ack;
  IP2Bus_Error <= '0';

end IMP;
