------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- map.ads -- Map representation for K-colorability problem
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Numerics.Float_Random;
with Ada.Float_Text_IO;

package Map is
   
   subtype Bit_Type is Natural range 0 .. 1;
   
   type Neuron_Type is
      record
         V : Bit_Type := 0;     -- Output
         U : Float    := 0.0;   -- Input
      end record;
   
   -- Number of colors. Up to 255 colors
   subtype Nr_Colors_Type is Natural range 1 .. 2**8 - 1;
   
   -- Number of regions to be colored. Up to 65535 regions
   subtype Nr_Regions_Type is Natural range 1 .. 2**16 - 1;
   
   type Region_Type is array ( Nr_Colors_Type range <> ) of Neuron_Type;
   
   -- I need a dynamic array of dynamic arrays. Using Ada's implicit
   --   dereferencing of access types.
   type Map_Type is array ( Nr_Regions_Type range <> ) of access
     Region_Type;
   
   -- 0 for not adjacent, 1 for adjacent. Not using Boolean type because
   --   the adjacency value is used in the calculations for the motion
   --   equation
   type Adjacency_Matrix_Type is array ( Nr_Regions_Type range <>,
					 Nr_Regions_Type range <> )
     of Bit_Type
       with Default_Component_Value => 0;
   
   procedure Read_Map ( File_Name : in String );
   
   procedure Print_Adjacency_Matrix;
   
   procedure Initialize;
   
   procedure Print_NN_Map ( Map : access Map_Type );
   
   procedure Motion ( Region : in Nr_Regions_Type;
                      Color : in Nr_Colors_Type );
 
   procedure Color;
   
end Map;
