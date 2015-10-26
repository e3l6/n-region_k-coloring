------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- color.adb -- Colorize n-map with k colors.
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_Io;
with Map;

procedure Color is
   
begin
   
   if Ada.Command_Line.Argument_Count /= 1 then
     Ada.Text_Io.Put_Line ( Ada.Command_Line.Command_Name &
			      " usage: " & Ada.Command_Line.Command_Name &
			      " <map file name>" );
   else   
      Map.Read_Map ( Ada.Command_Line.Argument ( 1 ) );
   end if;
   
end Color;
