------------------------------------------------------------------------------
-- axi4_stream_to_link - entity/architecture pair
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
-- Filename:          axi4_stream_to_link
-- Version:           1.00.a
-- Description:       Example Axi Streaming core (VHDL).
-- Date:              Tue Jan 23 15:56:01 2018 (by Create and Import Peripheral Wizard)
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------------
--
--
-- Definition of Ports
-- ACLK              : Synchronous clock
-- ARESETN           : System reset, active low
-- S_AXIS_TREADY  : Ready to accept data in
-- S_AXIS_TDATA   :  Data in 
-- S_AXIS_TLAST   : Optional data in qualifier
-- S_AXIS_TVALID  : Data in is valid
--
-------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Entity Section
------------------------------------------------------------------------------

entity axi4_stream_to_link is
    generic(
    C_LXDATA_WIDTH      : integer := 8
    
    );
    port 
    (
    -- DO NOT EDIT BELOW THIS LINE ---------------------
    -- Bus protocol ports, do not add or delete. 
    ACLK            : in    std_logic;
    ARESETN         : in    std_logic;
    S_AXIS_TREADY   : out   std_logic;
    S_AXIS_TDATA    : in    std_logic_vector(31 downto 0);
    S_AXIS_TLAST    : in    std_logic;
    S_AXIS_TVALID   : in    std_logic;
    -- DO NOT EDIT ABOVE THIS LINE ---------------------
    -- USER PORTS --
    LXACK           : in    std_logic;
    LXCLK           : out   std_logic;
    LXDAT           : out   std_logic_vector(C_LXDATA_WIDTH-1 downto 0)
    );

attribute SIGIS : string; 
attribute SIGIS of ACLK : signal is "Clk"; 

end axi4_stream_to_link;

------------------------------------------------------------------------------
-- Architecture Section
------------------------------------------------------------------------------

-- In this section, we povide an example implementation of ENTITY axi4_stream_to_link
-- that Read all inputs and add each input to the contents of register 'sum' which
-- acts as an accumulator
--
-- You will need to modify this example or implement a new architecture for
-- ENTITY axi4_stream_to_link to implement your coprocessor

architecture EXAMPLE of axi4_stream_to_link is
  -- Total number of input verdata.
   constant NUMBER_OF_INPUT_WORDS  : natural := 8;

   type STATE_TYPE is (Idle, Read_Inputs);

   signal state        : STATE_TYPE;

   -- Accumulator to hold sum of inputs read at any point in time
   signal sum          : std_logic_vector(31 downto 0);

   -- Counters to store the number inputs read
   signal nr_of_reads  : natural range 0 to NUMBER_OF_INPUT_WORDS - 1;

begin
   -- CAUTION:
   -- The sequence in which data are read in should be
   -- consistent with the sequence they are written in the
   -- driver's wefff.c file

   S_AXIS_TREADY  <= '1'   when state = Read_Inputs   else '0';
   The_SW_accelerator : process (ACLK) is
   begin  -- process The_SW_accelerator
    if ACLK'event and ACLK = '1' then     -- Rising clock edge
      if ARESETN = '0' then               -- Synchronous reset (active low)
        -- CAUTION: make sure your reset polarity is consistent with the
        -- system reset polarity
        state        <= Idle;
        sum          <= (others => '0');
        nr_of_reads  <= 0;
      else
        case state is
          when Idle =>
            if (S_AXIS_TVALID = '1') then
              state       <= Read_Inputs;
              nr_of_reads <= NUMBER_OF_INPUT_WORDS - 1;
              sum          <= (others => '0');
            end if;

          when Read_Inputs =>
            if (S_AXIS_TVALID = '1') then
              -- Coprocessor function (Adding) happens here
              sum         <= std_logic_vector(unsigned(sum) + unsigned(S_AXIS_TDATA));
              if (nr_of_reads = 0) then
                state        <= Idle;
              else
                nr_of_reads <= nr_of_reads - 1;
              end if;
            end if;

        end case;
      end if;
    end if;
   end process The_SW_accelerator;
end architecture EXAMPLE;
