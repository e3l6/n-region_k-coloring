--------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- color.adb -- Colorize n-region map with k colors.
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
--------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with Map;

procedure Color is
   
begin
   
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ( Ada.Command_Line.Command_Name &
                             " usage: " & Ada.Command_Line.Command_Name &
                             " <map file name>" );
   else   
      -- Read the map and start everything
      Map.Read_Map ( Ada.Command_Line.Argument ( 1 ) );
      
      Map.Map_Initialize.Start;
   end if;
   
   Ada.Text_IO.Put_Line ( "Ended main" );
   
end Color;
