--/******************************************************************************
--
--File name:    
--Rev:          
--Description:  
--
-- (c) Copyright 1995 - 2010 Xilinx, Inc. All rights reserved.
--
-- This file contains confidential and proprietary information
-- of Xilinx, Inc. and is protected under U.S. and 
-- international copyright and other intellectual property
-- laws.
--
-- DISCLAIMER
-- This disclaimer is not a license and does not grant any
-- rights to the materials distributed herewith. Except as
-- otherwise provided in a valid license issued to you by
-- Xilinx, and to the maximum extent permitted by applicable
-- law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
-- WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
-- AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
-- BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
-- INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
-- (2) Xilinx shall not be liable (whether in contract or tort,
-- including negligence, or under any other theory of
-- liability) for any loss or damage of any kind or nature
-- related to, arising under or in connection with these
-- materials, including for any direct, or any indirect,
-- special, incidental, or consequential loss or damage
-- (including loss of data, profits, goodwill, or any type of
-- loss or damage suffered as a result of any action brought
-- by a third party) even if such damage or loss was
-- reasonably foreseeable or Xilinx had been advised of the
-- possibility of the same.
--
-- CRITICAL APPLICATIONS
-- Xilinx products are not designed or intended to be fail-
-- safe, or for use in any application requiring fail-safe
-- performance, such as life-support or safety devices or
-- systems, Class III medical devices, nuclear facilities,
-- applications related to the deployment of airbags, or any
-- other applications that could lead to death, personal
-- injury, or severe property or environmental damage
-- (individually and collectively, "Critical
-- Applications"). Customer assumes the sole risk and
-- liability of any use of Xilinx products in Critical
-- Applications, subject only to applicable laws and
-- regulations governing limitations on product liability.
--
-- THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
-- PART OF THIS FILE AT ALL TIMES.
--*******************************************************************************/

LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;

-- synthesis translate_off
LIBRARY XilinxCoreLib;
-- synthesis translate_on
--library fifo_generator_V9_1;
--use fifo_generator_V9_1.all;

ENTITY axis_async_fifo IS
  GENERIC (
    -------------------------------------------------------------------------
    -- Generic Declarations
    -------------------------------------------------------------------------
    C_FAMILY              : STRING  := "kintex7";
    C_FIFO_DEPTH          : INTEGER := 256;	 
  --  C_PROG_FULL_THRESH    : INTEGER := 128;
    C_DATA_WIDTH          : INTEGER := 52
  --  C_PTR_WIDTH           : INTEGER := 8;
  --  C_MEMORY_TYPE         : INTEGER := 1;
  --  C_COMMON_CLOCK        : INTEGER := 0;
  --  C_IMPLEMENTATION_TYPE : INTEGER := 1;
  --  C_SYNCHRONIZER_STAGE  : INTEGER := 2
    )	; 
  PORT (
    rst       : IN  STD_LOGIC;
    wr_clk    : IN  STD_LOGIC;
    rd_clk    : IN  STD_LOGIC;
--    sync_clk  : IN  STD_LOGIC;
    din       : IN  STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
    wr_en     : IN  STD_LOGIC;
    rd_en     : IN  STD_LOGIC;
    dout      : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
    full      : OUT STD_LOGIC;
    empty     : OUT STD_LOGIC;
    valid     : OUT STD_LOGIC
  );
END axis_async_fifo;

ARCHITECTURE axis_async_fifo_a OF axis_async_fifo IS

  COMPONENT FIFO_GENERATOR_V9_1
  GENERIC (
    -------------------------------------------------------------------------
    -- Generic Declarations
    -------------------------------------------------------------------------
    C_COMMON_CLOCK                          : INTEGER := 0;
    C_COUNT_TYPE                            : INTEGER := 0;
    C_DATA_COUNT_WIDTH                      : INTEGER := 2;
    C_DEFAULT_VALUE                         : STRING  := "";
    C_DIN_WIDTH                             : INTEGER := 8;
    C_DOUT_RST_VAL                          : STRING  := "";
    C_DOUT_WIDTH                            : INTEGER := 8;
    C_ENABLE_RLOCS                          : INTEGER := 0;
    C_FAMILY                                : STRING  := "";
    C_FULL_FLAGS_RST_VAL                    : INTEGER := 1;
    C_HAS_ALMOST_EMPTY                      : INTEGER := 0;
    C_HAS_ALMOST_FULL                       : INTEGER := 0;
    C_HAS_BACKUP                            : INTEGER := 0;
    C_HAS_DATA_COUNT                        : INTEGER := 0;
    C_HAS_INT_CLK                           : INTEGER := 0;
    C_HAS_MEMINIT_FILE                      : INTEGER := 0;
    C_HAS_OVERFLOW                          : INTEGER := 0;
    C_HAS_RD_DATA_COUNT                     : INTEGER := 0;
    C_HAS_RD_RST                            : INTEGER := 0;
    C_HAS_RST                               : INTEGER := 1;
    C_HAS_SRST                              : INTEGER := 0;
    C_HAS_UNDERFLOW                         : INTEGER := 0;
    C_HAS_VALID                             : INTEGER := 0;
    C_HAS_WR_ACK                            : INTEGER := 0;
    C_HAS_WR_DATA_COUNT                     : INTEGER := 0;
    C_HAS_WR_RST                            : INTEGER := 0;
    C_IMPLEMENTATION_TYPE                   : INTEGER := 0;
    C_INIT_WR_PNTR_VAL                      : INTEGER := 0;
    C_MEMORY_TYPE                           : INTEGER := 1;
    C_MIF_FILE_NAME                         : STRING  := "";
    C_OPTIMIZATION_MODE                     : INTEGER := 0;
    C_OVERFLOW_LOW                          : INTEGER := 0;
    C_PRELOAD_LATENCY                       : INTEGER := 1;
    C_PRELOAD_REGS                          : INTEGER := 0;
    C_PRIM_FIFO_TYPE                        : STRING  := "4kx4";
    C_PROG_EMPTY_THRESH_ASSERT_VAL          : INTEGER := 0;
    C_PROG_EMPTY_THRESH_NEGATE_VAL          : INTEGER := 0;
    C_PROG_EMPTY_TYPE                       : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL           : INTEGER := 0;
    C_PROG_FULL_THRESH_NEGATE_VAL           : INTEGER := 0;
    C_PROG_FULL_TYPE                        : INTEGER := 0;
    C_RD_DATA_COUNT_WIDTH                   : INTEGER := 2;
    C_RD_DEPTH                              : INTEGER := 256;
    C_RD_FREQ                               : INTEGER := 1;
    C_RD_PNTR_WIDTH                         : INTEGER := 8;
    C_UNDERFLOW_LOW                         : INTEGER := 0;
    C_USE_DOUT_RST                          : INTEGER := 0;
    C_USE_ECC                               : INTEGER := 0;
    C_USE_EMBEDDED_REG                      : INTEGER := 0;
    C_USE_FIFO16_FLAGS                      : INTEGER := 0;
    C_USE_FWFT_DATA_COUNT                   : INTEGER := 0;
    C_VALID_LOW                             : INTEGER := 0;
    C_WR_ACK_LOW                            : INTEGER := 0;
    C_WR_DATA_COUNT_WIDTH                   : INTEGER := 2;
    C_WR_DEPTH                              : INTEGER := 256;
    C_WR_FREQ                               : INTEGER := 1;
    C_WR_PNTR_WIDTH                         : INTEGER := 8;
    C_WR_RESPONSE_LATENCY                   : INTEGER := 1;
    C_MSGON_VAL                             : INTEGER := 1;
    C_ENABLE_RST_SYNC                       : INTEGER := 1;
    C_ERROR_INJECTION_TYPE                  : INTEGER := 0;
    C_SYNCHRONIZER_STAGE                    : INTEGER := 2;

    -- AXI Interface related parameters start here
    C_INTERFACE_TYPE                        : INTEGER := 0; -- 0: Native Interface; 1: AXI Interface
    C_AXI_TYPE                              : INTEGER := 0; -- 0: AXI Stream; 1: AXI Full; 2: AXI Lite
    C_HAS_AXI_WR_CHANNEL                    : INTEGER := 0;
    C_HAS_AXI_RD_CHANNEL                    : INTEGER := 0;
    C_HAS_SLAVE_CE                          : INTEGER := 0;
    C_HAS_MASTER_CE                         : INTEGER := 0;
    C_ADD_NGC_CONSTRAINT                    : INTEGER := 0;
    C_USE_COMMON_OVERFLOW                   : INTEGER := 0;
    C_USE_COMMON_UNDERFLOW                  : INTEGER := 0;
    C_USE_DEFAULT_SETTINGS                  : INTEGER := 0;

    -- AXI Full/Lite
    C_AXI_ID_WIDTH                          : INTEGER := 0;
    C_AXI_ADDR_WIDTH                        : INTEGER := 0;
    C_AXI_DATA_WIDTH                        : INTEGER := 0;
    C_HAS_AXI_AWUSER                        : INTEGER := 0;
    C_HAS_AXI_WUSER                         : INTEGER := 0;
    C_HAS_AXI_BUSER                         : INTEGER := 0;
    C_HAS_AXI_ARUSER                        : INTEGER := 0;
    C_HAS_AXI_RUSER                         : INTEGER := 0;
    C_AXI_ARUSER_WIDTH                      : INTEGER := 0;
    C_AXI_AWUSER_WIDTH                      : INTEGER := 0;
    C_AXI_WUSER_WIDTH                       : INTEGER := 0;
    C_AXI_BUSER_WIDTH                       : INTEGER := 0;
    C_AXI_RUSER_WIDTH                       : INTEGER := 0;
                                       
    -- AXI Streaming
    C_HAS_AXIS_TDATA                        : INTEGER := 0;
    C_HAS_AXIS_TID                          : INTEGER := 0;
    C_HAS_AXIS_TDEST                        : INTEGER := 0;
    C_HAS_AXIS_TUSER                        : INTEGER := 0;
    C_HAS_AXIS_TREADY                       : INTEGER := 0;
    C_HAS_AXIS_TLAST                        : INTEGER := 0;
    C_HAS_AXIS_TSTRB                        : INTEGER := 0;
    C_HAS_AXIS_TKEEP                        : INTEGER := 0;
    C_AXIS_TDATA_WIDTH                      : INTEGER := 1;
    C_AXIS_TID_WIDTH                        : INTEGER := 1;
    C_AXIS_TDEST_WIDTH                      : INTEGER := 1;
    C_AXIS_TUSER_WIDTH                      : INTEGER := 1;
    C_AXIS_TSTRB_WIDTH                      : INTEGER := 1;
    C_AXIS_TKEEP_WIDTH                      : INTEGER := 1;

    -- AXI Channel Type
    -- WACH --> Write Address Channel
    -- WDCH --> Write Data Channel
    -- WRCH --> Write Response Channel
    -- RACH --> Read Address Channel
    -- RDCH --> Read Data Channel
    -- AXIS --> AXI Streaming
    C_WACH_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logic
    C_WDCH_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logie
    C_WRCH_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logie
    C_RACH_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logie
    C_RDCH_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logie
    C_AXIS_TYPE                             : INTEGER := 0; -- 0 = FIFO; 1 = Register Slice; 2 = Pass Through Logie

    -- AXI Implementation Type
    -- 1 = Common Clock Block RAM FIFO
    -- 2 = Common Clock Distributed RAM FIFO
    -- 11 = Independent Clock Block RAM FIFO
    -- 12 = Independent Clock Distributed RAM FIFO
    C_IMPLEMENTATION_TYPE_WACH              : INTEGER := 0;
    C_IMPLEMENTATION_TYPE_WDCH              : INTEGER := 0;
    C_IMPLEMENTATION_TYPE_WRCH              : INTEGER := 0;
    C_IMPLEMENTATION_TYPE_RACH              : INTEGER := 0;
    C_IMPLEMENTATION_TYPE_RDCH              : INTEGER := 0;
    C_IMPLEMENTATION_TYPE_AXIS              : INTEGER := 0;

    -- AXI FIFO Type
    -- 0 = Data FIFO
    -- 1 = Packet FIFO
    -- 2 = Low Latency Data FIFO
    C_APPLICATION_TYPE_WACH                 : INTEGER := 0;
    C_APPLICATION_TYPE_WDCH                 : INTEGER := 0;
    C_APPLICATION_TYPE_WRCH                 : INTEGER := 0;
    C_APPLICATION_TYPE_RACH                 : INTEGER := 0;
    C_APPLICATION_TYPE_RDCH                 : INTEGER := 0;
    C_APPLICATION_TYPE_AXIS                 : INTEGER := 0;

    -- Enable ECC
    -- 0 = ECC disabled
    -- 1 = ECC enabled
    C_USE_ECC_WACH                          : INTEGER := 0;
    C_USE_ECC_WDCH                          : INTEGER := 0;
    C_USE_ECC_WRCH                          : INTEGER := 0;
    C_USE_ECC_RACH                          : INTEGER := 0;
    C_USE_ECC_RDCH                          : INTEGER := 0;
    C_USE_ECC_AXIS                          : INTEGER := 0;

    -- ECC Error Injection Type
    -- 0 = No Error Injection
    -- 1 = Single Bit Error Injection
    -- 2 = Double Bit Error Injection
    -- 3 = Single Bit and Double Bit Error Injection
    C_ERROR_INJECTION_TYPE_WACH             : INTEGER := 0;
    C_ERROR_INJECTION_TYPE_WDCH             : INTEGER := 0;
    C_ERROR_INJECTION_TYPE_WRCH             : INTEGER := 0;
    C_ERROR_INJECTION_TYPE_RACH             : INTEGER := 0;
    C_ERROR_INJECTION_TYPE_RDCH             : INTEGER := 0;
    C_ERROR_INJECTION_TYPE_AXIS             : INTEGER := 0;

    -- Input Data Width
    -- Accumulation of all AXI input signal's width
    C_DIN_WIDTH_WACH                        : INTEGER := 1;
    C_DIN_WIDTH_WDCH                        : INTEGER := 1;
    C_DIN_WIDTH_WRCH                        : INTEGER := 1;
    C_DIN_WIDTH_RACH                        : INTEGER := 1;
    C_DIN_WIDTH_RDCH                        : INTEGER := 1;
    C_DIN_WIDTH_AXIS                        : INTEGER := 1;

    C_WR_DEPTH_WACH                         : INTEGER := 16;
    C_WR_DEPTH_WDCH                         : INTEGER := 16;
    C_WR_DEPTH_WRCH                         : INTEGER := 16;
    C_WR_DEPTH_RACH                         : INTEGER := 16;
    C_WR_DEPTH_RDCH                         : INTEGER := 16;
    C_WR_DEPTH_AXIS                         : INTEGER := 16;

    C_WR_PNTR_WIDTH_WACH                    : INTEGER := 4;
    C_WR_PNTR_WIDTH_WDCH                    : INTEGER := 4;
    C_WR_PNTR_WIDTH_WRCH                    : INTEGER := 4;
    C_WR_PNTR_WIDTH_RACH                    : INTEGER := 4;
    C_WR_PNTR_WIDTH_RDCH                    : INTEGER := 4;
    C_WR_PNTR_WIDTH_AXIS                    : INTEGER := 4;

    C_HAS_DATA_COUNTS_WACH                  : INTEGER := 0;
    C_HAS_DATA_COUNTS_WDCH                  : INTEGER := 0;
    C_HAS_DATA_COUNTS_WRCH                  : INTEGER := 0;
    C_HAS_DATA_COUNTS_RACH                  : INTEGER := 0;
    C_HAS_DATA_COUNTS_RDCH                  : INTEGER := 0;
    C_HAS_DATA_COUNTS_AXIS                  : INTEGER := 0;

    C_HAS_PROG_FLAGS_WACH                   : INTEGER := 0;
    C_HAS_PROG_FLAGS_WDCH                   : INTEGER := 0;
    C_HAS_PROG_FLAGS_WRCH                   : INTEGER := 0;
    C_HAS_PROG_FLAGS_RACH                   : INTEGER := 0;
    C_HAS_PROG_FLAGS_RDCH                   : INTEGER := 0;
    C_HAS_PROG_FLAGS_AXIS                   : INTEGER := 0;

    C_PROG_FULL_TYPE_WACH                   : INTEGER := 0;
    C_PROG_FULL_TYPE_WDCH                   : INTEGER := 0;
    C_PROG_FULL_TYPE_WRCH                   : INTEGER := 0;
    C_PROG_FULL_TYPE_RACH                   : INTEGER := 0;
    C_PROG_FULL_TYPE_RDCH                   : INTEGER := 0;
    C_PROG_FULL_TYPE_AXIS                   : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_WACH      : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_WDCH      : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_WRCH      : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_RACH      : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_RDCH      : INTEGER := 0;
    C_PROG_FULL_THRESH_ASSERT_VAL_AXIS      : INTEGER := 0;

    C_PROG_EMPTY_TYPE_WACH                  : INTEGER := 0;
    C_PROG_EMPTY_TYPE_WDCH                  : INTEGER := 0;
    C_PROG_EMPTY_TYPE_WRCH                  : INTEGER := 0;
    C_PROG_EMPTY_TYPE_RACH                  : INTEGER := 0;
    C_PROG_EMPTY_TYPE_RDCH                  : INTEGER := 0;
    C_PROG_EMPTY_TYPE_AXIS                  : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH     : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH     : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH     : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH     : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH     : INTEGER := 0;
    C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS     : INTEGER := 0;

    C_REG_SLICE_MODE_WACH                   : INTEGER := 0;
    C_REG_SLICE_MODE_WDCH                   : INTEGER := 0;
    C_REG_SLICE_MODE_WRCH                   : INTEGER := 0;
    C_REG_SLICE_MODE_RACH                   : INTEGER := 0;
    C_REG_SLICE_MODE_RDCH                   : INTEGER := 0;
    C_REG_SLICE_MODE_AXIS                   : INTEGER := 0

    );


  PORT(
    ------------------------------------------------------------------------------
    -- Input and Output Declarations
    ------------------------------------------------------------------------------

    -- Conventional FIFO Interface Signals
    BACKUP                         : IN  STD_LOGIC := '0';
    BACKUP_MARKER                  : IN  STD_LOGIC := '0';
    CLK                            : IN  STD_LOGIC := '0';
    SRST                           : IN  STD_LOGIC := '0';
    RST                            : IN  STD_LOGIC := '0';
    WR_CLK                         : IN  STD_LOGIC := '0';
    WR_RST                         : IN  STD_LOGIC := '0';
    RD_CLK                         : IN  STD_LOGIC := '0';
    RD_RST                         : IN  STD_LOGIC := '0';
    DIN                            : IN  STD_LOGIC_VECTOR(C_DIN_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    WR_EN                          : IN  STD_LOGIC := '0';
    RD_EN                          : IN  STD_LOGIC := '0';

    -- Optional inputs
    PROG_EMPTY_THRESH              : IN  STD_LOGIC_VECTOR(C_RD_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_EMPTY_THRESH_ASSERT       : IN  STD_LOGIC_VECTOR(C_RD_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_EMPTY_THRESH_NEGATE       : IN  STD_LOGIC_VECTOR(C_RD_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_FULL_THRESH               : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_FULL_THRESH_ASSERT        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_FULL_THRESH_NEGATE        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    INT_CLK                        : IN  STD_LOGIC := '0';
    INJECTDBITERR                  : IN  STD_LOGIC := '0';
    INJECTSBITERR                  : IN  STD_LOGIC := '0';

    DOUT                           : OUT STD_LOGIC_VECTOR(C_DOUT_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    FULL                           : OUT STD_LOGIC := '0';
    ALMOST_FULL                    : OUT STD_LOGIC := '0';
    WR_ACK                         : OUT STD_LOGIC := '0';
    OVERFLOW                       : OUT STD_LOGIC := '0';
    EMPTY                          : OUT STD_LOGIC := '1';
    ALMOST_EMPTY                   : OUT STD_LOGIC := '1';
    VALID                          : OUT STD_LOGIC := '0';
    UNDERFLOW                      : OUT STD_LOGIC := '0';
    DATA_COUNT                     : OUT STD_LOGIC_VECTOR(C_DATA_COUNT_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    RD_DATA_COUNT                  : OUT STD_LOGIC_VECTOR(C_RD_DATA_COUNT_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    WR_DATA_COUNT                  : OUT STD_LOGIC_VECTOR(C_WR_DATA_COUNT_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    PROG_FULL                      : OUT STD_LOGIC := '0';
    PROG_EMPTY                     : OUT STD_LOGIC := '1';
    SBITERR                        : OUT STD_LOGIC := '0';
    DBITERR                        : OUT STD_LOGIC := '0';

    -- AXI Global Signal
    M_ACLK                         : IN  STD_LOGIC := '0';
    S_ACLK                         : IN  STD_LOGIC := '0';
    S_ARESETN                      : IN  STD_LOGIC := '1'; -- Active low reset, default value set to 1
    M_ACLK_EN                      : IN  STD_LOGIC := '0';
    S_ACLK_EN                      : IN  STD_LOGIC := '0';

    -- AXI Full/Lite Slave Write Channel (write side)
    S_AXI_AWID                     : IN  STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWADDR                   : IN  STD_LOGIC_VECTOR(C_AXI_ADDR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWLEN                    : IN  STD_LOGIC_VECTOR(8-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWSIZE                   : IN  STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWBURST                  : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWLOCK                   : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWCACHE                  : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWPROT                   : IN  STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWQOS                    : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWREGION                 : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWUSER                   : IN  STD_LOGIC_VECTOR(C_AXI_AWUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_AWVALID                  : IN  STD_LOGIC := '0';
    S_AXI_AWREADY                  : OUT STD_LOGIC := '0';
    S_AXI_WID                      : IN  STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)     := (OTHERS => '0');
    S_AXI_WDATA                    : IN  STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');
    S_AXI_WSTRB                    : IN  STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH/8-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_WLAST                    : IN  STD_LOGIC := '0';
    S_AXI_WUSER                    : IN  STD_LOGIC_VECTOR(C_AXI_WUSER_WIDTH-1 DOWNTO 0)  := (OTHERS => '0');
    S_AXI_WVALID                   : IN  STD_LOGIC := '0';
    S_AXI_WREADY                   : OUT STD_LOGIC := '0';
    S_AXI_BID                      : OUT STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)     := (OTHERS => '0');
    S_AXI_BRESP                    : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0)                  := (OTHERS => '0');
    S_AXI_BUSER                    : OUT STD_LOGIC_VECTOR(C_AXI_BUSER_WIDTH-1 DOWNTO 0)  := (OTHERS => '0');
    S_AXI_BVALID                   : OUT STD_LOGIC := '0';
    S_AXI_BREADY                   : IN  STD_LOGIC := '0';

    -- AXI Full/Lite Master Write Channel (Read side)
    M_AXI_AWID                     : OUT STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');
    M_AXI_AWADDR                   : OUT STD_LOGIC_VECTOR(C_AXI_ADDR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWLEN                    : OUT STD_LOGIC_VECTOR(8-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWSIZE                   : OUT STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWBURST                  : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWLOCK                   : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWCACHE                  : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWPROT                   : OUT STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWQOS                    : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWREGION                 : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWUSER                   : OUT STD_LOGIC_VECTOR(C_AXI_AWUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_AWVALID                  : OUT STD_LOGIC := '0';
    M_AXI_AWREADY                  : IN  STD_LOGIC := '0';
    M_AXI_WID                      : OUT STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)     := (OTHERS => '0');
    M_AXI_WDATA                    : OUT STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');
    M_AXI_WSTRB                    : OUT STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH/8-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_WLAST                    : OUT STD_LOGIC := '0';
    M_AXI_WUSER                    : OUT STD_LOGIC_VECTOR(C_AXI_WUSER_WIDTH-1 DOWNTO 0)  := (OTHERS => '0');
    M_AXI_WVALID                   : OUT STD_LOGIC := '0';
    M_AXI_WREADY                   : IN  STD_LOGIC := '0';
    M_AXI_BID                      : IN  STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)    := (OTHERS => '0');
    M_AXI_BRESP                    : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0)                 := (OTHERS => '0');
    M_AXI_BUSER                    : IN  STD_LOGIC_VECTOR(C_AXI_BUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_BVALID                   : IN  STD_LOGIC := '0';
    M_AXI_BREADY                   : OUT STD_LOGIC := '0';

    -- AXI Full/Lite Slave Read Channel (Write side)
    S_AXI_ARID                     : IN  STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');
    S_AXI_ARADDR                   : IN  STD_LOGIC_VECTOR(C_AXI_ADDR_WIDTH-1 DOWNTO 0) := (OTHERS => '0'); 
    S_AXI_ARLEN                    : IN  STD_LOGIC_VECTOR(8-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARSIZE                   : IN  STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARBURST                  : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARLOCK                   : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARCACHE                  : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARPROT                   : IN  STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARQOS                    : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARREGION                 : IN  STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARUSER                   : IN  STD_LOGIC_VECTOR(C_AXI_ARUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_ARVALID                  : IN  STD_LOGIC := '0';
    S_AXI_ARREADY                  : OUT STD_LOGIC := '0';
    S_AXI_RID                      : OUT STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');       
    S_AXI_RDATA                    : OUT STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH-1 DOWNTO 0) := (OTHERS => '0'); 
    S_AXI_RRESP                    : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0)                := (OTHERS => '0');
    S_AXI_RLAST                    : OUT STD_LOGIC := '0';
    S_AXI_RUSER                    : OUT STD_LOGIC_VECTOR(C_AXI_RUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXI_RVALID                   : OUT STD_LOGIC := '0';
    S_AXI_RREADY                   : IN  STD_LOGIC := '0';

    -- AXI Full/Lite Master Read Channel (Read side)
    M_AXI_ARID                     : OUT STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0)   := (OTHERS => '0');        
    M_AXI_ARADDR                   : OUT STD_LOGIC_VECTOR(C_AXI_ADDR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');  
    M_AXI_ARLEN                    : OUT STD_LOGIC_VECTOR(8-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARSIZE                   : OUT STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARBURST                  : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARLOCK                   : OUT STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARCACHE                  : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARPROT                   : OUT STD_LOGIC_VECTOR(3-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARQOS                    : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARREGION                 : OUT STD_LOGIC_VECTOR(4-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARUSER                   : OUT STD_LOGIC_VECTOR(C_AXI_ARUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_ARVALID                  : OUT STD_LOGIC := '0';
    M_AXI_ARREADY                  : IN  STD_LOGIC := '0';
    M_AXI_RID                      : IN  STD_LOGIC_VECTOR(C_AXI_ID_WIDTH-1 DOWNTO 0) := (OTHERS => '0');        
    M_AXI_RDATA                    : IN  STD_LOGIC_VECTOR(C_AXI_DATA_WIDTH-1 DOWNTO 0) := (OTHERS => '0');  
    M_AXI_RRESP                    : IN  STD_LOGIC_VECTOR(2-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_RLAST                    : IN  STD_LOGIC := '0';
    M_AXI_RUSER                    : IN  STD_LOGIC_VECTOR(C_AXI_RUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXI_RVALID                   : IN  STD_LOGIC := '0';
    M_AXI_RREADY                   : OUT STD_LOGIC := '0';

    -- AXI Streaming Slave Signals (Write side)
    S_AXIS_TVALID                  : IN  STD_LOGIC := '0';
    S_AXIS_TREADY                  : OUT STD_LOGIC := '0';
    S_AXIS_TDATA                   : IN  STD_LOGIC_VECTOR(C_AXIS_TDATA_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXIS_TSTRB                   : IN  STD_LOGIC_VECTOR(C_AXIS_TSTRB_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXIS_TKEEP                   : IN  STD_LOGIC_VECTOR(C_AXIS_TKEEP_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXIS_TLAST                   : IN  STD_LOGIC := '0';
    S_AXIS_TID                     : IN  STD_LOGIC_VECTOR(C_AXIS_TID_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXIS_TDEST                   : IN  STD_LOGIC_VECTOR(C_AXIS_TDEST_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    S_AXIS_TUSER                   : IN  STD_LOGIC_VECTOR(C_AXIS_TUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');

    -- AXI Streaming Master Signals (Read side)
    M_AXIS_TVALID                  : OUT STD_LOGIC := '0';
    M_AXIS_TREADY                  : IN  STD_LOGIC := '0';
    M_AXIS_TDATA                   : OUT STD_LOGIC_VECTOR(C_AXIS_TDATA_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXIS_TSTRB                   : OUT STD_LOGIC_VECTOR(C_AXIS_TSTRB_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXIS_TKEEP                   : OUT STD_LOGIC_VECTOR(C_AXIS_TKEEP_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXIS_TLAST                   : OUT STD_LOGIC := '0';
    M_AXIS_TID                     : OUT STD_LOGIC_VECTOR(C_AXIS_TID_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXIS_TDEST                   : OUT STD_LOGIC_VECTOR(C_AXIS_TDEST_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
    M_AXIS_TUSER                   : OUT STD_LOGIC_VECTOR(C_AXIS_TUSER_WIDTH-1 DOWNTO 0) := (OTHERS => '0');

    -- AXI Full/Lite Write Address Channel Signals
    AXI_AW_INJECTSBITERR           : IN  STD_LOGIC := '0';
    AXI_AW_INJECTDBITERR           : IN  STD_LOGIC := '0';
    AXI_AW_PROG_FULL_THRESH        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WACH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_AW_PROG_EMPTY_THRESH       : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WACH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_AW_DATA_COUNT              : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WACH DOWNTO 0) := (OTHERS => '0');
    AXI_AW_WR_DATA_COUNT           : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WACH DOWNTO 0) := (OTHERS => '0');
    AXI_AW_RD_DATA_COUNT           : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WACH DOWNTO 0) := (OTHERS => '0');
    AXI_AW_SBITERR                 : OUT STD_LOGIC := '0';
    AXI_AW_DBITERR                 : OUT STD_LOGIC := '0';
    AXI_AW_OVERFLOW                : OUT STD_LOGIC := '0';
    AXI_AW_UNDERFLOW               : OUT STD_LOGIC := '0';
    AXI_AW_PROG_FULL               : OUT STD_LOGIC := '0';
    AXI_AW_PROG_EMPTY              : OUT STD_LOGIC := '1';

    -- AXI Full/Lite Write Data Channel Signals
    AXI_W_INJECTSBITERR            : IN  STD_LOGIC := '0';
    AXI_W_INJECTDBITERR            : IN  STD_LOGIC := '0';
    AXI_W_PROG_FULL_THRESH         : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WDCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_W_PROG_EMPTY_THRESH        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WDCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_W_DATA_COUNT               : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WDCH DOWNTO 0) := (OTHERS => '0');
    AXI_W_WR_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WDCH DOWNTO 0) := (OTHERS => '0');
    AXI_W_RD_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WDCH DOWNTO 0) := (OTHERS => '0');
    AXI_W_SBITERR                  : OUT STD_LOGIC := '0';
    AXI_W_DBITERR                  : OUT STD_LOGIC := '0';
    AXI_W_OVERFLOW                 : OUT STD_LOGIC := '0';
    AXI_W_UNDERFLOW                : OUT STD_LOGIC := '0';
    AXI_W_PROG_FULL                : OUT STD_LOGIC := '0';
    AXI_W_PROG_EMPTY               : OUT STD_LOGIC := '1';

    -- AXI Full/Lite Write Response Channel Signals
    AXI_B_INJECTSBITERR            : IN  STD_LOGIC := '0';
    AXI_B_INJECTDBITERR            : IN  STD_LOGIC := '0';
    AXI_B_PROG_FULL_THRESH         : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WRCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_B_PROG_EMPTY_THRESH        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WRCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_B_DATA_COUNT               : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WRCH DOWNTO 0) := (OTHERS => '0');
    AXI_B_WR_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WRCH DOWNTO 0) := (OTHERS => '0');
    AXI_B_RD_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_WRCH DOWNTO 0) := (OTHERS => '0');
    AXI_B_SBITERR                  : OUT STD_LOGIC := '0';
    AXI_B_DBITERR                  : OUT STD_LOGIC := '0';
    AXI_B_OVERFLOW                 : OUT STD_LOGIC := '0';
    AXI_B_UNDERFLOW                : OUT STD_LOGIC := '0';
    AXI_B_PROG_FULL                : OUT STD_LOGIC := '0';
    AXI_B_PROG_EMPTY               : OUT STD_LOGIC := '1';

    -- AXI Full/Lite Read Address Channel Signals
    AXI_AR_INJECTSBITERR           : IN  STD_LOGIC := '0';
    AXI_AR_INJECTDBITERR           : IN  STD_LOGIC := '0';
    AXI_AR_PROG_FULL_THRESH        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RACH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_AR_PROG_EMPTY_THRESH       : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RACH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_AR_DATA_COUNT              : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RACH DOWNTO 0) := (OTHERS => '0');
    AXI_AR_WR_DATA_COUNT           : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RACH DOWNTO 0) := (OTHERS => '0');
    AXI_AR_RD_DATA_COUNT           : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RACH DOWNTO 0) := (OTHERS => '0');
    AXI_AR_SBITERR                 : OUT STD_LOGIC := '0';
    AXI_AR_DBITERR                 : OUT STD_LOGIC := '0';
    AXI_AR_OVERFLOW                : OUT STD_LOGIC := '0';
    AXI_AR_UNDERFLOW               : OUT STD_LOGIC := '0';
    AXI_AR_PROG_FULL               : OUT STD_LOGIC := '0';
    AXI_AR_PROG_EMPTY              : OUT STD_LOGIC := '1';

    -- AXI Full/Lite Read Data Channel Signals
    AXI_R_INJECTSBITERR            : IN  STD_LOGIC := '0';
    AXI_R_INJECTDBITERR            : IN  STD_LOGIC := '0';
    AXI_R_PROG_FULL_THRESH         : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RDCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_R_PROG_EMPTY_THRESH        : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RDCH-1 DOWNTO 0) := (OTHERS => '0');
    AXI_R_DATA_COUNT               : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RDCH DOWNTO 0) := (OTHERS => '0');
    AXI_R_WR_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RDCH DOWNTO 0) := (OTHERS => '0');
    AXI_R_RD_DATA_COUNT            : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_RDCH DOWNTO 0) := (OTHERS => '0');
    AXI_R_SBITERR                  : OUT STD_LOGIC := '0';
    AXI_R_DBITERR                  : OUT STD_LOGIC := '0';
    AXI_R_OVERFLOW                 : OUT STD_LOGIC := '0';
    AXI_R_UNDERFLOW                : OUT STD_LOGIC := '0';
    AXI_R_PROG_FULL                : OUT STD_LOGIC := '0';
    AXI_R_PROG_EMPTY               : OUT STD_LOGIC := '1';

    -- AXI Streaming FIFO Related Signals
    AXIS_INJECTSBITERR             : IN  STD_LOGIC := '0';
    AXIS_INJECTDBITERR             : IN  STD_LOGIC := '0';
    AXIS_PROG_FULL_THRESH          : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_AXIS-1 DOWNTO 0) := (OTHERS => '0');
    AXIS_PROG_EMPTY_THRESH         : IN  STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_AXIS-1 DOWNTO 0) := (OTHERS => '0');
    AXIS_DATA_COUNT                : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_AXIS DOWNTO 0) := (OTHERS => '0');
    AXIS_WR_DATA_COUNT             : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_AXIS DOWNTO 0) := (OTHERS => '0');
    AXIS_RD_DATA_COUNT             : OUT STD_LOGIC_VECTOR(C_WR_PNTR_WIDTH_AXIS DOWNTO 0) := (OTHERS => '0');
    AXIS_SBITERR                   : OUT STD_LOGIC := '0';
    AXIS_DBITERR                   : OUT STD_LOGIC := '0';
    AXIS_OVERFLOW                  : OUT STD_LOGIC := '0';
    AXIS_UNDERFLOW                 : OUT STD_LOGIC := '0';
    AXIS_PROG_FULL                 : OUT STD_LOGIC := '0';
    AXIS_PROG_EMPTY                : OUT STD_LOGIC := '1'

    );
 END COMPONENT;
 
  ATTRIBUTE box_type : STRING;
  ATTRIBUTE box_type OF FIFO_GENERATOR_V9_1 : COMPONENT IS "black_box";
  ATTRIBUTE GENERATOR_DEFAULT : STRING;
  ATTRIBUTE GENERATOR_DEFAULT OF FIFO_GENERATOR_V9_1 : COMPONENT IS
  "generatecore com.xilinx.ip.fifo_generator_V9_1.fifo_generator_V9_1";	 


BEGIN

U0 : FIFO_GENERATOR_V9_1
    GENERIC MAP (
      c_add_ngc_constraint => 0,
      c_application_type_axis => 0,
      c_application_type_rach => 0,
      c_application_type_rdch => 0,
      c_application_type_wach => 0,
      c_application_type_wdch => 0,
      c_application_type_wrch => 0,
      c_axi_addr_width => 32,
      c_axi_aruser_width => 1,
      c_axi_awuser_width => 1,
      c_axi_buser_width => 1,
      c_axi_data_width => 64,
      c_axi_id_width => 4,
      c_axi_ruser_width => 1,
      c_axi_type => 0,
      c_axi_wuser_width => 1,
      c_axis_tdata_width => 64,
      c_axis_tdest_width => 4,
      c_axis_tid_width => 8,
      c_axis_tkeep_width => 4,
      c_axis_tstrb_width => 4,
      c_axis_tuser_width => 4,
      c_axis_type => 0,
      c_common_clock => 0,
      c_count_type => 0,
      c_data_count_width => 9,
      c_default_value => "BlankString",
      c_din_width => 32,
      c_din_width_axis => 1,
      c_din_width_rach => 32,
      c_din_width_rdch => 64,
      c_din_width_wach => 32,
      c_din_width_wdch => 64,
      c_din_width_wrch => 2,
      c_dout_rst_val => "0",
      c_dout_width => 32,
      c_enable_rlocs => 0,
      c_enable_rst_sync => 1,
      c_error_injection_type => 0,
      c_error_injection_type_axis => 0,
      c_error_injection_type_rach => 0,
      c_error_injection_type_rdch => 0,
      c_error_injection_type_wach => 0,
      c_error_injection_type_wdch => 0,
      c_error_injection_type_wrch => 0,
      c_family => C_FAMILY,
      c_full_flags_rst_val => 1,
      c_has_almost_empty => 0,
      c_has_almost_full => 0,
      c_has_axi_aruser => 0,
      c_has_axi_awuser => 0,
      c_has_axi_buser => 0,
      c_has_axi_rd_channel => 0,
      c_has_axi_ruser => 0,
      c_has_axi_wr_channel => 0,
      c_has_axi_wuser => 0,
      c_has_axis_tdata => 0,
      c_has_axis_tdest => 0,
      c_has_axis_tid => 0,
      c_has_axis_tkeep => 0,
      c_has_axis_tlast => 0,
      c_has_axis_tready => 1,
      c_has_axis_tstrb => 0,
      c_has_axis_tuser => 0,
      c_has_backup => 0,
      c_has_data_count => 0,
      c_has_data_counts_axis => 0,
      c_has_data_counts_rach => 0,
      c_has_data_counts_rdch => 0,
      c_has_data_counts_wach => 0,
      c_has_data_counts_wdch => 0,
      c_has_data_counts_wrch => 0,
      c_has_int_clk => 0,
      c_has_master_ce => 0,
      c_has_meminit_file => 0,
      c_has_overflow => 0,
      c_has_prog_flags_axis => 0,
      c_has_prog_flags_rach => 0,
      c_has_prog_flags_rdch => 0,
      c_has_prog_flags_wach => 0,
      c_has_prog_flags_wdch => 0,
      c_has_prog_flags_wrch => 0,
      c_has_rd_data_count => 0,
      c_has_rd_rst => 0,
      c_has_rst => 1,
      c_has_slave_ce => 0,
      c_has_srst => 0,
      c_has_underflow => 0,
      c_has_valid => 1,
      c_has_wr_ack => 0,
      c_has_wr_data_count => 0,
      c_has_wr_rst => 0,
      c_implementation_type => 2,
      c_implementation_type_axis => 1,
      c_implementation_type_rach => 1,
      c_implementation_type_rdch => 1,
      c_implementation_type_wach => 1,
      c_implementation_type_wdch => 1,
      c_implementation_type_wrch => 1,
      c_init_wr_pntr_val => 0,
      c_interface_type => 0,
      c_memory_type => 2,
      c_mif_file_name => "BlankString",
      c_msgon_val => 1,
      c_optimization_mode => 0,
      c_overflow_low => 0,
      c_preload_latency => 1,
      c_preload_regs => 0,
      c_prim_fifo_type => "512x36",
      c_prog_empty_thresh_assert_val => 2,
      c_prog_empty_thresh_assert_val_axis => 1022,
      c_prog_empty_thresh_assert_val_rach => 1022,
      c_prog_empty_thresh_assert_val_rdch => 1022,
      c_prog_empty_thresh_assert_val_wach => 1022,
      c_prog_empty_thresh_assert_val_wdch => 1022,
      c_prog_empty_thresh_assert_val_wrch => 1022,
      c_prog_empty_thresh_negate_val => 3,
      c_prog_empty_type => 0,
      c_prog_empty_type_axis => 0,
      c_prog_empty_type_rach => 0,
      c_prog_empty_type_rdch => 0,
      c_prog_empty_type_wach => 0,
      c_prog_empty_type_wdch => 0,
      c_prog_empty_type_wrch => 0,
      c_prog_full_thresh_assert_val => 509,
      c_prog_full_thresh_assert_val_axis => 1023,
      c_prog_full_thresh_assert_val_rach => 1023,
      c_prog_full_thresh_assert_val_rdch => 1023,
      c_prog_full_thresh_assert_val_wach => 1023,
      c_prog_full_thresh_assert_val_wdch => 1023,
      c_prog_full_thresh_assert_val_wrch => 1023,
      c_prog_full_thresh_negate_val => 508,
      c_prog_full_type => 0,
      c_prog_full_type_axis => 0,
      c_prog_full_type_rach => 0,
      c_prog_full_type_rdch => 0,
      c_prog_full_type_wach => 0,
      c_prog_full_type_wdch => 0,
      c_prog_full_type_wrch => 0,
      c_rach_type => 0,
      c_rd_data_count_width => 9,
      c_rd_depth => 512,
      c_rd_freq => 1,
      c_rd_pntr_width => 9,
      c_rdch_type => 0,
      c_reg_slice_mode_axis => 0,
      c_reg_slice_mode_rach => 0,
      c_reg_slice_mode_rdch => 0,
      c_reg_slice_mode_wach => 0,
      c_reg_slice_mode_wdch => 0,
      c_reg_slice_mode_wrch => 0,
      c_synchronizer_stage => 2,
      c_underflow_low => 0,
      c_use_common_overflow => 0,
      c_use_common_underflow => 0,
      c_use_default_settings => 0,
      c_use_dout_rst => 1,
      c_use_ecc => 0,
      c_use_ecc_axis => 0,
      c_use_ecc_rach => 0,
      c_use_ecc_rdch => 0,
      c_use_ecc_wach => 0,
      c_use_ecc_wdch => 0,
      c_use_ecc_wrch => 0,
      c_use_embedded_reg => 0,
      c_use_fifo16_flags => 0,
      c_use_fwft_data_count => 0,
      c_valid_low => 0,
      c_wach_type => 0,
      c_wdch_type => 0,
      c_wr_ack_low => 0,
      c_wr_data_count_width => 9,
      c_wr_depth => 512,
      c_wr_depth_axis => 1024,
      c_wr_depth_rach => 16,
      c_wr_depth_rdch => 1024,
      c_wr_depth_wach => 16,
      c_wr_depth_wdch => 1024,
      c_wr_depth_wrch => 16,
      c_wr_freq => 1,
      c_wr_pntr_width => 9,
      c_wr_pntr_width_axis => 10,
      c_wr_pntr_width_rach => 4,
      c_wr_pntr_width_rdch => 10,
      c_wr_pntr_width_wach => 4,
      c_wr_pntr_width_wdch => 10,
      c_wr_pntr_width_wrch => 4,
      c_wr_response_latency => 1,
      c_wrch_type => 0
    )
  PORT MAP (
    RST                            => rst,
    WR_CLK                         => wr_clk,
    RD_CLK                         => rd_clk,
    DIN                            => din,
    WR_EN                          => wr_en,
    RD_EN                          => rd_en,
    DOUT                           => dout,
    FULL                           => full,
    EMPTY                          => empty,
    PROG_FULL                      => open,

    BACKUP                         => '0',
    BACKUP_MARKER                  => '0',
--    CLK                            => sync_clk,
    SRST                           => '0',
    WR_RST                         => '0',
    RD_RST                         => '0',
    PROG_EMPTY_THRESH              => (others => '0'),
    PROG_EMPTY_THRESH_ASSERT       => (others => '0'),
    PROG_EMPTY_THRESH_NEGATE       => (others => '0'),
    PROG_FULL_THRESH               => (others => '0'),
    PROG_FULL_THRESH_ASSERT        => (others => '0'),
    PROG_FULL_THRESH_NEGATE        => (others => '0'),
    INT_CLK                        => '0',
    INJECTDBITERR                  => '0',
    INJECTSBITERR                  => '0',
    ALMOST_FULL                    => OPEN,
    WR_ACK                         => OPEN,
    OVERFLOW                       => OPEN,
    ALMOST_EMPTY                   => OPEN,
    VALID                          => valid,
    UNDERFLOW                      => OPEN,
    DATA_COUNT                     => OPEN,
    RD_DATA_COUNT                  => OPEN,
    WR_DATA_COUNT                  => OPEN,
    PROG_EMPTY                     => OPEN,
    SBITERR                        => OPEN,
    DBITERR                        => OPEN,
    M_ACLK                         => '0',
    S_ACLK                         => '0',
    S_ARESETN                      => '0',
    M_ACLK_EN                      => '0',
    S_ACLK_EN                      => '0',
    S_AXI_AWID                     => (others => '0'),
    S_AXI_AWADDR                   => (others => '0'),
    S_AXI_AWLEN                    => (others => '0'),
    S_AXI_AWSIZE                   => (others => '0'),
    S_AXI_AWBURST                  => (others => '0'),
    S_AXI_AWLOCK                   => (others => '0'),
    S_AXI_AWCACHE                  => (others => '0'),
    S_AXI_AWPROT                   => (others => '0'),
    S_AXI_AWQOS                    => (others => '0'),
    S_AXI_AWREGION                 => (others => '0'),
    S_AXI_AWUSER                   => (others => '0'),
    S_AXI_AWVALID                  => '0',
    S_AXI_AWREADY                  => OPEN,
    S_AXI_WID                      => (others => '0'),
    S_AXI_WDATA                    => (others => '0'),
    S_AXI_WSTRB                    => (others => '0'),
    S_AXI_WLAST                    => '0',
    S_AXI_WUSER                    => (others => '0'),
    S_AXI_WVALID                   => '0',
    S_AXI_WREADY                   => OPEN,
    S_AXI_BID                      => OPEN,
    S_AXI_BRESP                    => OPEN,
    S_AXI_BUSER                    => OPEN,
    S_AXI_BVALID                   => OPEN,
    S_AXI_BREADY                   => '0',
    M_AXI_AWID                     => OPEN,
    M_AXI_AWADDR                   => OPEN,
    M_AXI_AWLEN                    => OPEN,
    M_AXI_AWSIZE                   => OPEN,
    M_AXI_AWBURST                  => OPEN,
    M_AXI_AWLOCK                   => OPEN,
    M_AXI_AWCACHE                  => OPEN,
    M_AXI_AWPROT                   => OPEN,
    M_AXI_AWQOS                    => OPEN,
    M_AXI_AWREGION                 => OPEN,
    M_AXI_AWUSER                   => OPEN,
    M_AXI_AWVALID                  => OPEN,
    M_AXI_AWREADY                  => '0',
    M_AXI_WID                      => OPEN,
    M_AXI_WDATA                    => OPEN,
    M_AXI_WSTRB                    => OPEN,
    M_AXI_WLAST                    => OPEN,
    M_AXI_WUSER                    => OPEN,
    M_AXI_WVALID                   => OPEN,
    M_AXI_WREADY                   => '0',
    M_AXI_BID                      => (others => '0'),
    M_AXI_BRESP                    => (others => '0'),
    M_AXI_BUSER                    => (others => '0'),
    M_AXI_BVALID                   => '0',
    M_AXI_BREADY                   => OPEN,
    S_AXI_ARID                     => (others => '0'),
    S_AXI_ARADDR                   => (others => '0'),
    S_AXI_ARLEN                    => (others => '0'),
    S_AXI_ARSIZE                   => (others => '0'),
    S_AXI_ARBURST                  => (others => '0'),
    S_AXI_ARLOCK                   => (others => '0'),
    S_AXI_ARCACHE                  => (others => '0'),
    S_AXI_ARPROT                   => (others => '0'),
    S_AXI_ARQOS                    => (others => '0'),
    S_AXI_ARREGION                 => (others => '0'),
    S_AXI_ARUSER                   => (others => '0'),
    S_AXI_ARVALID                  => '0',
    S_AXI_ARREADY                  => OPEN,
    S_AXI_RID                      => OPEN,
    S_AXI_RDATA                    => OPEN,
    S_AXI_RRESP                    => OPEN,
    S_AXI_RLAST                    => OPEN,
    S_AXI_RUSER                    => OPEN,
    S_AXI_RVALID                   => OPEN,
    S_AXI_RREADY                   => '0',
    M_AXI_ARID                     => OPEN,
    M_AXI_ARADDR                   => OPEN,
    M_AXI_ARLEN                    => OPEN,
    M_AXI_ARSIZE                   => OPEN,
    M_AXI_ARBURST                  => OPEN,
    M_AXI_ARLOCK                   => OPEN,
    M_AXI_ARCACHE                  => OPEN,
    M_AXI_ARPROT                   => OPEN,
    M_AXI_ARQOS                    => OPEN,
    M_AXI_ARREGION                 => OPEN,
    M_AXI_ARUSER                   => OPEN,
    M_AXI_ARVALID                  => OPEN,
    M_AXI_ARREADY                  => '0',
    M_AXI_RID                      => (others => '0'),
    M_AXI_RDATA                    => (others => '0'),
    M_AXI_RRESP                    => (others => '0'),
    M_AXI_RLAST                    => '0',
    M_AXI_RUSER                    => (others => '0'),
    M_AXI_RVALID                   => '0',
    M_AXI_RREADY                   => OPEN,
    S_AXIS_TVALID                  => '0',
    S_AXIS_TREADY                  => OPEN,
    S_AXIS_TDATA                   => (others => '0'),
    S_AXIS_TSTRB                   => (others => '0'),
    S_AXIS_TKEEP                   => (others => '0'),
    S_AXIS_TLAST                   => '0',
    S_AXIS_TID                     => (others => '0'),
    S_AXIS_TDEST                   => (others => '0'),
    S_AXIS_TUSER                   => (others => '0'),
    M_AXIS_TVALID                  => OPEN,
    M_AXIS_TREADY                  => '0',
    M_AXIS_TDATA                   => OPEN,
    M_AXIS_TSTRB                   => OPEN,
    M_AXIS_TKEEP                   => OPEN,
    M_AXIS_TLAST                   => OPEN,
    M_AXIS_TID                     => OPEN,
    M_AXIS_TDEST                   => OPEN,
    M_AXIS_TUSER                   => OPEN,
    AXI_AW_INJECTSBITERR           => '0',
    AXI_AW_INJECTDBITERR           => '0',
    AXI_AW_PROG_FULL_THRESH        => (others => '0'),
    AXI_AW_PROG_EMPTY_THRESH       => (others => '0'),
    AXI_AW_DATA_COUNT              => OPEN,
    AXI_AW_WR_DATA_COUNT           => OPEN,
    AXI_AW_RD_DATA_COUNT           => OPEN,
    AXI_AW_SBITERR                 => OPEN,
    AXI_AW_DBITERR                 => OPEN,
    AXI_AW_OVERFLOW                => OPEN,
    AXI_AW_UNDERFLOW               => OPEN,
    AXI_AW_PROG_FULL               => OPEN,
    AXI_AW_PROG_EMPTY              => OPEN,
    AXI_W_INJECTSBITERR            => '0',
    AXI_W_INJECTDBITERR            => '0',
    AXI_W_PROG_FULL_THRESH         => (others => '0'),
    AXI_W_PROG_EMPTY_THRESH        => (others => '0'),
    AXI_W_DATA_COUNT               => OPEN,
    AXI_W_WR_DATA_COUNT            => OPEN,
    AXI_W_RD_DATA_COUNT            => OPEN,
    AXI_W_SBITERR                  => OPEN,
    AXI_W_DBITERR                  => OPEN,
    AXI_W_OVERFLOW                 => OPEN,
    AXI_W_UNDERFLOW                => OPEN,
    AXI_W_PROG_FULL                => OPEN,
    AXI_W_PROG_EMPTY               => OPEN,
    AXI_B_INJECTSBITERR            => '0',
    AXI_B_INJECTDBITERR            => '0',
    AXI_B_PROG_FULL_THRESH         => (others => '0'),
    AXI_B_PROG_EMPTY_THRESH        => (others => '0'),
    AXI_B_DATA_COUNT               => OPEN,
    AXI_B_WR_DATA_COUNT            => OPEN,
    AXI_B_RD_DATA_COUNT            => OPEN,
    AXI_B_SBITERR                  => OPEN,
    AXI_B_DBITERR                  => OPEN,
    AXI_B_OVERFLOW                 => OPEN,
    AXI_B_UNDERFLOW                => OPEN,
    AXI_B_PROG_FULL                => OPEN,
    AXI_B_PROG_EMPTY               => OPEN,
    AXI_AR_INJECTSBITERR           => '0',
    AXI_AR_INJECTDBITERR           => '0',
    AXI_AR_PROG_FULL_THRESH        => (others => '0'),
    AXI_AR_PROG_EMPTY_THRESH       => (others => '0'),
    AXI_AR_DATA_COUNT              => OPEN,
    AXI_AR_WR_DATA_COUNT           => OPEN,
    AXI_AR_RD_DATA_COUNT           => OPEN,
    AXI_AR_SBITERR                 => OPEN,
    AXI_AR_DBITERR                 => OPEN,
    AXI_AR_OVERFLOW                => OPEN,
    AXI_AR_UNDERFLOW               => OPEN,
    AXI_AR_PROG_FULL               => OPEN,
    AXI_AR_PROG_EMPTY              => OPEN,
    AXI_R_INJECTSBITERR            => '0',
    AXI_R_INJECTDBITERR            => '0',
    AXI_R_PROG_FULL_THRESH         => (others => '0'),
    AXI_R_PROG_EMPTY_THRESH        => (others => '0'),
    AXI_R_DATA_COUNT               => OPEN,
    AXI_R_WR_DATA_COUNT            => OPEN,
    AXI_R_RD_DATA_COUNT            => OPEN,
    AXI_R_SBITERR                  => OPEN,
    AXI_R_DBITERR                  => OPEN,
    AXI_R_OVERFLOW                 => OPEN,
    AXI_R_UNDERFLOW                => OPEN,
    AXI_R_PROG_FULL                => OPEN,
    AXI_R_PROG_EMPTY               => OPEN,
    AXIS_INJECTSBITERR             => '0',
    AXIS_INJECTDBITERR             => '0',
    AXIS_PROG_FULL_THRESH          => (others => '0'),
    AXIS_PROG_EMPTY_THRESH         => (others => '0'),
    AXIS_DATA_COUNT                => OPEN,
    AXIS_WR_DATA_COUNT             => OPEN,
    AXIS_RD_DATA_COUNT             => OPEN,
    AXIS_SBITERR                   => OPEN,
    AXIS_DBITERR                   => OPEN,
    AXIS_OVERFLOW                  => OPEN,
    AXIS_UNDERFLOW                 => OPEN,
    AXIS_PROG_FULL                 => OPEN,
    AXIS_PROG_EMPTY                => OPEN
  );

END axis_async_fifo_a;
