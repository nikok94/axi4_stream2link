----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.05.2018 13:35:32
-- Design Name: 
-- Module Name: link_clock_devider - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned."+";
use ieee.std_logic_unsigned."-";
use ieee.std_logic_unsigned.all;


entity link_clock_devider is
    Port ( clk_in       : in std_logic;
           rst          : in std_logic;
           devide_in    : in std_logic_vector(31 downto 0);
           clk_out      : out std_logic
         );
end link_clock_devider;

architecture Behavioral of link_clock_devider is
     signal clk_count                  : std_logic_vector(31 downto 0):= x"0000_0000";
     signal divide_clk                 : std_logic:= '0';
     signal divide                     : std_logic_vector(31 downto 0);

begin

    divide <= devide_in - x"0000_0001";
CLK_DIVIDE_PROCESS : process (clk_in)
    begin
        if rst = '1' then
          clk_count <= (others =>'0');
          divide_clk <= '0';
        elsif clk_in'event and clk_in = '1' then
          if clk_count = ('0' & divide(31 downto 1)) then
          divide_clk <= not divide_clk;
          clk_count <= (others =>'0');
          else
          clk_count <= clk_count + x"0000_0001" ;
          end if;
        end if;
    end process CLK_DIVIDE_PROCESS;
   
   clk_out <= divide_clk;
  
end Behavioral;
