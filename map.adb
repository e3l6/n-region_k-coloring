------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- map.adb -- Map representation body for K-colorability problem
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
-------------------------------------------------------------------------------

package body Map is
   
   Nr_Colors : Nr_Colors_Type;
   Nr_Regions : Nr_Regions_Type;
   Adjacency_Matrix : access Adjacency_Matrix_Type;
   
   -- Read_Map - open the map file and build the adjacency matrix
   -- Input:   File_Name : String
   -- Output:  stdio
   -- Changes: Nr_Colors, Nr_Regions, Map
   
   procedure Read_Map ( File_Name : in String ) is
      
      Map_File   : Ada.Text_Io.File_Type;
      Edge_Count : Natural := 0;
      Tmp_X, Tmp_Y : Natural; -- Next values for adjacencies
      Tmp_Char : Character;  -- Throw away for the ',' in adjacency pairs
      
   begin
      
      Ada.Text_Io.Put_Line ( "Opening """ & File_Name & """" );
      
      Ada.Text_Io.Open ( File => Map_File,
			 Mode => Ada.Text_IO.In_File,
			 Name => File_Name );
      
      Ada.Integer_Text_Io.Get ( File => Map_File,
				Item => Nr_Colors );
      
      Ada.Integer_Text_Io.Get ( File => Map_File,
				Item => Nr_Regions );
      
      Ada.Integer_Text_Io.Put ( Nr_Colors, 0 );
      Ada.Text_Io.Put ( " colors for " );
      Ada.Integer_Text_Io.Put ( Nr_Regions, 0 );
      Ada.Text_Io.Put_Line ( " regions" );
      
      Adjacency_Matrix := new Adjacency_Matrix_Type ( 1 .. Nr_Regions,
						      1 .. Nr_Regions );
      
      while not Ada.Text_Io.End_Of_File ( Map_File ) loop
	 Ada.Integer_Text_Io.Get ( File => Map_File,
				   Item => Tmp_X );
	 Ada.Text_Io.Get ( File => Map_File,
			   Item => Tmp_Char );
	 Ada.Integer_Text_Io.Get ( File => Map_File,
				   Item => Tmp_Y );
	 
	 if ( Tmp_X > Nr_Regions ) or ( Tmp_Y > Nr_Regions ) then
	    Ada.Text_Io.Put ( "Invalid region pair: " );
	    Ada.Integer_Text_Io.Put ( Tmp_X, 0 );
	    Ada.Text_Io.Put ( "," );
	    Ada.Integer_Text_Io.Put ( Tmp_Y, 0 );
	    Ada.Text_Io.Put_Line ( ". Ignoring pair" );
	 else
	    Ada.Text_Io.Put ( "Region " );
	    Ada.Integer_Text_Io.Put ( Tmp_X, 0 );
	    Ada.Text_Io.Put ( " adjacent to region " );
	    Ada.Integer_Text_Io.Put ( Tmp_Y, 0 );
	    Ada.Text_Io.New_LIne;
	    
	    Adjacency_Matrix ( Tmp_X, Tmp_Y ) := 1;
	    Adjacency_Matrix ( Tmp_Y, Tmp_X ) := 1;
	 end if;
      end loop;
      
      Ada.Text_Io.Put_Line ( "Closing """ & File_Name & """" );
      Ada.Text_Io.Close ( Map_File );
      
      Ada.Text_Io.New_Line ( 2 );
      
      Print_Adjacency_Matrix;
   end Read_Map;

   
   
   -- Print_Adjacency_Matrix - display the adjacency matrix for this coloring
   --    problem
   --    This is all kinds of fugly because of type conversions and that I
   --    am not "use"-ing any of my packages
   -- Input:   none
   -- Output:  stdio
   -- Changes: none
   
   procedure Print_Adjacency_Matrix is
      
      Table_Col_Width : Natural;
      
   begin
      
      Table_Col_Width := Ada.Strings.Unbounded.Length ( Ada.Strings.Unbounded.To_Unbounded_String ( Integer'Image ( Nr_Regions ) ) );
      
      Ada.Text_Io.Set_Col ( Ada.Text_Io.Count ( Table_Col_Width + 1 ) );
      
      for I in 1 .. Nr_Regions loop
	Ada.Integer_Text_Io.Put ( I, Table_Col_Width );
      end loop;
      
      Ada.Text_Io.New_Line;
      
      for I in 1 .. Nr_Regions loop
	 Ada.Integer_Text_Io.Put ( I, Table_Col_Width );
	 
	 for J in 1 .. Nr_Regions loop
	    if Adjacency_Matrix ( I, J ) = 1 then
	       Ada.Integer_Text_Io.Put ( Adjacency_Matrix ( I, J ),
					 Table_Col_Width );
	    else
	       Ada.Text_Io.Set_Col ( Ada.Text_Io.Count ( Integer ( Ada.Text_Io.Col ) + Table_Col_Width ) );
	    end if;
	 end loop;
	 
	 Ada.Text_Io.New_Line;
      end loop;
      
   end Print_Adjacency_Matrix;
   
end Map;
