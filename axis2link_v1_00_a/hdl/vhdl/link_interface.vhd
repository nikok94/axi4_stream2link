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

Library UNISIM;
use UNISIM.vcomponents.all;

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
    
    axis_clk_2x                    : in std_logic;
    axis_clk_2x_180                : in std_logic;
    
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
    
  type LINK_PORT_STATE_TYPE         is  (IDLE, LINK_PORT_ST_RD, READ_STRM_DATA, SEND_BYTE1, SEND_BYTE2, SEND_BYTE3, SEND_BYTE4);
  signal link_state                     : LINK_PORT_STATE_TYPE;
  --USER signal declarations added here, as needed for user logic
  signal clk_counter                    : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal divide                         : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal link_rst                       : std_logic;
  signal clk_div                        : std_logic:='0';
  signal clk_div_n                      : std_logic:='1';
  signal link_clk                       : std_logic:='0';
  
  signal clk_div_delay                  : std_logic:='0';
  --signal clk_dev_n                      : std_logic:='1';
  --signal link_clk                       : std_logic:='0';
  signal tready_ind                     : std_logic;
  
  signal clk_div_up                     : std_logic:='0';
  signal next_byte                      : std_logic:='0';
  
  signal strm_data_rd_en                : std_logic:= '0';
  
  signal send_link_data                 : std_logic:= '0';
  signal Lx_CLK_out                     : std_logic:= '0';
  signal Lx_DAT_out                     : std_logic_vector(7 downto 0):= x"00";
  signal Lx_ACK_i                       : std_logic;
  signal rd_strm_data                   : std_logic_vector(C_S_AXIS_TDATA_WIDTH-1 downto 0):= x"0000_0000";
  ------------------------------------------
  -- Signals for user logic slave model s/w accessible register example
  ------------------------------------------
  signal init_reg                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  
  signal slv_reg2                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_reg3                       : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_reg_write_sel              : std_logic_vector(3 downto 0);
  signal slv_reg_read_sel               : std_logic_vector(3 downto 0);
  signal slv_ip2bus_data                : std_logic_vector(C_SLV_DWIDTH-1 downto 0);
  signal slv_read_ack                   : std_logic;
  signal slv_write_ack                  : std_logic;

begin
  Lx_ACK_i <= Lx_ACK;
  link_rst <= slv_reg2(0);
  s_axis_tready <= strm_data_rd_en;

  UP_CLK_DIV_INIT : process (axis_clk_2x)
  begin
  if axis_clk_2x'event and axis_clk_2x = '1' then
    if (aresetn = '0') then
    clk_div_up <= '0';
    clk_div_delay <= '0';
    else
    clk_div_delay <= clk_div;
    end if;
    clk_div_up <= (not clk_div_delay) and clk_div;
  end if;
  next_byte <= clk_div_up;
  end process UP_CLK_DIV_INIT;

  LX_CLK_OUT_PROC : process (axis_clk_2x)
  begin
  if axis_clk_2x'event and axis_clk_2x = '1' then
    if (aresetn = '0' or link_rst = '1') then
    Lx_CLK_out <= '0';
    elsif send_link_data = '1' then 
    Lx_CLK_out <= link_clk;
    else
    Lx_CLK_out <= '1';
    end if;
  Lx_CLK <= Lx_CLK_out;
  end if;
  end process LX_CLK_OUT_PROC;
  
  LX_DAT_OUT_PROC : process (axis_clk_2x)
  begin
  if axis_clk_2x'event and axis_clk_2x = '1' then
    if (aresetn = '0' or link_rst = '1') then
    Lx_DAT <= x"00";
    elsif send_link_data = '1' then 
    Lx_DAT <= Lx_DAT_out;
    else
    Lx_DAT <= x"00";
    end if;
  end if;
  
  end process LX_DAT_OUT_PROC;
      
  DIVIDE_CLK_INST : process (axis_clk_2x_180)
  begin
    if axis_clk_2x_180'event and axis_clk_2x_180 = '1' then
      if aresetn = '0' then 
      clk_div_n <= '1';
      clk_counter <= (others => '0');
      elsif clk_counter = (divide - 1) then 
      clk_counter <= (others => '0');
      clk_div_n <= not clk_div_n;
      else 
      clk_counter <= clk_counter + 1;
      end if;
    end if;
  end process DIVIDE_CLK_INST;
  
  CLK_DEV_SYNC_INST : process (axis_clk_2x)
  begin
   if axis_clk_2x'event and axis_clk_2x = '1' then
     if aresetn = '0' then 
     clk_div <= '0';
     elsif clk_div_n = '1' then
     clk_div <= '1';
     else
     clk_div <= '0';
     end if;
   end if;
  link_clk <= clk_div;
  end process CLK_DEV_SYNC_INST;
  

    LINK_ST_PORT_PROCESS   :   process(axis_clk_2x)
    begin
      if aresetn = '0' then
        link_state <=  IDLE;
        elsif (axis_clk_2x'event and axis_clk_2x = '1') then
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
                  if (strm_data_rd_en = '1' and s_axis_tvalid = '1') then
                  link_state <= SEND_BYTE1;
                  else 
                  link_state <= READ_STRM_DATA;
                  end if;
              when SEND_BYTE1 =>
                if next_byte = '1' then
                link_state <= SEND_BYTE2;
                else
                link_state <= SEND_BYTE1;
                end if;
              when SEND_BYTE2 =>
                if next_byte = '1' then
                link_state <= SEND_BYTE3;
                else
                link_state <= SEND_BYTE2;
                end if;
              when SEND_BYTE3 =>
                if next_byte = '1' then
                link_state <= SEND_BYTE4;
                else
                link_state <= SEND_BYTE3;
                end if;
              when SEND_BYTE4 =>
                if next_byte = '1' then
                link_state <= LINK_PORT_ST_RD;
                else
                link_state <= SEND_BYTE4;
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
          strm_data_rd_en <= '0';
          send_link_data <= '0';
        when LINK_PORT_ST_RD =>
          send_link_data <= '0';
          strm_data_rd_en <= '0';
        when READ_STRM_DATA =>
          rd_strm_data <= s_axis_tdata;
          send_link_data <= '0';
          strm_data_rd_en <= '1';
        when SEND_BYTE1 =>
          send_link_data <= '1';
          strm_data_rd_en <= '0';
          Lx_DAT_out <= rd_strm_data(31 downto 24);
        when SEND_BYTE2 =>
          send_link_data <= '1';
          strm_data_rd_en <= '0';
          Lx_DAT_out <= rd_strm_data(23 downto 16);
        when SEND_BYTE3 =>
          send_link_data <= '1';
          strm_data_rd_en <= '0';
          Lx_DAT_out <= rd_strm_data(15 downto 8);
        when SEND_BYTE4 =>
          send_link_data <= '1';
          strm_data_rd_en <= '0';
          Lx_DAT_out <= rd_strm_data(7 downto 0);
        when others =>
          strm_data_rd_en <= '0';
          send_link_data <= '0';
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
        divide <= x"0000_0004"; 
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
