--------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- map.adb -- Map representation body for K-colorability problem
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
--------------------------------------------------------------------------------

package body Map is
   
   Nr_Colors          : Nr_Colors_Type;
   Nr_Regions         : Nr_Regions_Type;
   Adjacency_Matrix   : access Adjacency_Matrix_Type;
   NN_Map, NN_Map_Old : access Map_Type;
   Table_Col_Width    : Natural;
   Dirty              : Boolean := True;  -- True if Motion changed a neuron
                                          -- Set to false at the beginning of every iteration
   Iterations : Integer := 0;
      
   
   
   -- Read_Map - open the map file and build the adjacency matrix
   -- Input:   File_Name : String
   -- Output:  stdio
   -- Changes: Nr_Colors, Nr_Regions, Map
   
   procedure Read_Map ( File_Name : in String ) is
      
      Map_File     : Ada.Text_IO.File_Type;
      Edge_Count   : Natural := 0;
      Tmp_X, Tmp_Y : Natural;    -- Next values for adjacencies
      Tmp_Char     : Character;  -- Throw away for the ',' in adjacency pairs
      
   begin
      
      Ada.Text_IO.Put_Line ( "Opening """ & File_Name & """" );
      
      Ada.Text_IO.Open ( File => Map_File,
			 Mode => Ada.Text_IO.In_File,
			 Name => File_Name );
      
      Ada.Integer_Text_IO.Get ( File => Map_File,
				Item => Nr_Colors );
      
      Ada.Integer_Text_IO.Get ( File => Map_File,
				Item => Nr_Regions );
      
      Ada.Integer_Text_IO.Put ( Nr_Colors, 0 );
      Ada.Text_IO.Put ( " colors for " );
      Ada.Integer_Text_IO.Put ( Nr_Regions, 0 );
      Ada.Text_IO.Put_Line ( " regions" );
      
      Adjacency_Matrix := new Adjacency_Matrix_Type ( 1 .. Nr_Regions,
						      1 .. Nr_Regions );
      
      while not Ada.Text_IO.End_Of_File ( Map_File ) loop
	 Ada.Integer_Text_IO.Get ( File => Map_File,
				   Item => Tmp_X );
	 Ada.Text_IO.Get ( File => Map_File,
			   Item => Tmp_Char );
	 Ada.Integer_Text_IO.Get ( File => Map_File,
				   Item => Tmp_Y );
	 
	 if ( Tmp_X > Nr_Regions ) or ( Tmp_Y > Nr_Regions ) then
	    Ada.Text_IO.Put ( "Invalid region pair: " );
	    Ada.Integer_Text_IO.Put ( Tmp_X, 0 );
	    Ada.Text_IO.Put ( "," );
	    Ada.Integer_Text_IO.Put ( Tmp_Y, 0 );
	    Ada.Text_IO.Put_Line ( ". Ignoring pair" );
	 elsif ( Tmp_X = Tmp_Y ) then
	    Ada.Text_IO.Put ( "Invalid region pair: " );
	    Ada.Integer_Text_IO.Put ( Tmp_X, 0 );
	    Ada.Text_IO.Put ( "," );
	    Ada.Integer_Text_IO.Put ( Tmp_Y, 0 );
	    Ada.Text_IO.Put_Line ( ". That's just silly. Ignoring pair" );
	 else
	    Ada.Text_IO.Put ( "Region " );
	    Ada.Integer_Text_IO.Put ( Tmp_X, 0 );
	    Ada.Text_IO.Put ( " adjacent to region " );
	    Ada.Integer_Text_IO.Put ( Tmp_Y, 0 );
	    Ada.Text_IO.New_LIne;
	    
	    Adjacency_Matrix ( Tmp_X, Tmp_Y ) := 1;
	    Adjacency_Matrix ( Tmp_Y, Tmp_X ) := 1;
	    
	 end if;
      end loop;
      
      Ada.Text_IO.Put_Line ( "Closing """ & File_Name & """" );
      Ada.Text_IO.Close ( Map_File );
      
      Ada.Text_IO.New_Line ( 2 );
      
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
      
   begin
      
      Table_Col_Width := Ada.Strings.Unbounded.Length ( Ada.Strings.Unbounded.To_Unbounded_String ( Integer'Image ( Nr_Regions ) ) );
      
      Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Table_Col_Width + 1 ) );
      
      for I in 1 .. Nr_Regions loop
         Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
      end loop;
      
      Ada.Text_IO.New_Line;
      
      for I in 1 .. Nr_Regions loop
	 Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
	 
	 for J in 1 .. Nr_Regions loop
	    if Adjacency_Matrix ( I, J ) = 1 then
	       Ada.Integer_Text_IO.Put ( Adjacency_Matrix ( I, J ),
					 Table_Col_Width );
	    else
	       Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) +
                                                         Table_Col_Width ) );
	    end if;
	 end loop;
	 
	 Ada.Text_IO.New_Line;
      end loop;
      
      Ada.Text_IO.New_Line;
      
   end Print_Adjacency_Matrix;
   
   
   
   -- Initialize - Initialize the NN map. It's a N-row dynamic array of access types
   --              to a K-element dynamic array of bits (N region, K color).
   -- Input:   none
   -- Output:  stdio
   -- Changes: NN_Map
   
   procedure Initialize is
      
      Seed : Ada.Numerics.Float_Random.Generator;
      
   begin
      
      Ada.Numerics.Float_Random.Reset ( Seed );
      
      NN_Map     := new Map_Type ( 1 .. Nr_Regions );
      NN_Map_Old := new Map_Type ( 1 .. Nr_Regions );
      
      for I in 1 .. Nr_Regions loop
	 NN_Map ( I )     := new Region_Type ( 1 .. Nr_Colors );
	 NN_Map_Old ( I ) := new Region_Type ( 1 .. Nr_Colors );
	 
	 for J in 1 .. Nr_Colors loop
	    NN_Map ( I ) ( J ).U := Float'Rounding ( ( Ada.Numerics.Float_Random.Random ( Seed ) - 
                                                       0.5 ) * 10.0 );
            if NN_Map ( I ) ( J ).U >= 0.0 then
               NN_Map ( I ) ( J ).V := 1;
            end if;
	 end loop;
      end loop;
      
      for I in 1 .. Nr_Regions loop
	 for J in 1 .. Nr_Colors loop
	    NN_Map_Old ( I ) ( J ) := NN_Map ( I ) ( J );
	 end loop;
      end loop;
      
      Ada.Text_IO.Put_Line ( "NN_Map:" );
      Print_NN_Map ( NN_Map );
      Ada.Text_IO.New_Line;
      
   end Initialize;
   
   
   
   -- Print_NN_Map - Display the current iteration of NN_Map
   -- Input:   Map_Type
   -- Output:  stdio
   -- Changes: none
   
   procedure Print_NN_Map ( Map : access Map_Type ) is
      
   begin
      
      Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Table_Col_Width + 1 ) );
      
      for I in 1 .. Nr_Regions loop
         Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
      end loop;
      
      Ada.Text_IO.New_Line;
      
      for J in 1 .. Nr_Colors loop
	 Ada.Integer_Text_IO.Put ( J, Table_Col_Width );
	 
	 for I in 1 .. Nr_Regions loop
	    if Map ( I ) ( J ).V = 1 then
	       Ada.Integer_Text_IO.Put ( 1, Table_Col_Width );
	    else
	       Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) +
                                                         Table_Col_Width ) );
	    end if;
	 end loop;
	 
	 Ada.Text_IO.New_Line;
      end loop;
      
   end Print_NN_Map;
   
   
   
   -- Motion - Calculate and apply motion, update output for a given neuron
   -- Input:   Nr_Regions_Type, Nr_Colors_Type
   -- Output:  none
   -- Changes: NN_Map ( Region ) ( Color ), Dirty
   
   procedure Motion ( Region : in Nr_Regions_Type;
                      Color  : in Nr_Colors_Type ) is
      
      -- Region = X, Color = i in the published equation
      
      dUXi : Float := 0.0;              -- Calculated energy change for a neuron
      A, B, C, C2 : Float := 1.0;   -- Coefficients
      C1 : Float := 1.0;
      A_Term, B_Term, B2_Term, H, H_Term, C_Term, C1_Term,
        C2_Term, C2B_Term, C2C_Term : Integer := 0;   -- Different components of the motion
                                                      -- equation
   begin
      
      -- Compute the A term
      for J in 1 .. Nr_Colors loop
         A_Term := A_Term + ( Integer ( NN_Map_Old ( Region ) ( J ).V ) - 1 );
      end loop;
      
      -- Compute the B term
      for Y in 1 .. Nr_Regions loop
         B2_Term := 0;         -- Clear the sum of nr of neighbors to this neighbor
         if Y /= Region then   -- Don't check a region against itself
            for K in 1 .. Nr_Regions loop
               B2_Term := B2_Term + Integer ( Adjacency_Matrix ( Y, K ) );
            end loop;
            
            B_Term := B_Term + ( ( Integer ( Adjacency_Matrix ( Region, Y ) ) *
                                   Integer ( NN_Map_Old ( Y ) ( Color ).V ) ) * B2_Term );
         end if;
      end loop;
      
      -- Compute the H term
      for J in 1 .. Nr_Colors loop
         H_Term := H_Term + Integer ( NN_Map_Old ( Region ) ( J ).V );
      end loop;
      
      if H_Term = 0 then   -- This is h(x). h(x) = 1 if no colors are active for this region
         H := 1;
      else
         H := 0;
      end if;
      
      -- Compute the C1 term
      for K in 1 .. Nr_Regions loop
         C1_Term := C1_Term + Integer ( Adjacency_Matrix ( Region, K ) );
      end loop;
      
      -- Compute the C2 term
      for K in 1 .. Nr_Regions loop
         for Y in 1 .. Nr_Regions loop
            C2_Term := C2_Term + ( Integer ( Adjacency_Matrix ( Region, Y ) ) *
                                   Integer ( Adjacency_Matrix ( Y, K ) ) );
         end loop;
      end loop;
      
      for K in 1 .. Nr_Regions loop
         C2B_Term := C2B_Term + Integer ( Adjacency_Matrix ( Region, K ) );
      end loop;
      
      --  if ( Iterations > 60 ) and ( Iterations mod 5 = 0 ) then
      --     C := 5.0;
      --  else
      --     C := 1.0;
      --  end if;
      
      -- Add it all up for dUXi
      dUXi := ( -1.0 * A * Float ( A_Term ) ) - ( B * Float ( B_Term ) ) + 
        ( C * Float ( H ) * ( ( C1 * Float ( C1_Term ) ) + 
                              ( C2 * Float ( C2_Term ) / Float ( C2B_Term ) ) ) );
      
      --  Ada.Text_IO.Put ( "Region" & Nr_Regions_Type'Image ( Region ) & " Color" & 
      --                    Nr_Colors_Type'Image ( Color ) );
      --  Ada.Text_IO.Put ( " A_Term:" );
      --  Ada.Integer_Text_IO.Put ( A_Term, 5 );
      --  Ada.Text_IO.Put ( " B_Term:" );
      --  Ada.Integer_Text_IO.Put ( B_Term, 5 );
      --  Ada.Text_IO.Put ( " H_Term:" );
      --  Ada.Integer_Text_IO.Put ( H_Term, 5 );
      --  Ada.Text_IO.Put ( " H:" );
      --  Ada.Integer_Text_IO.Put ( H, 5 );
      --  Ada.Text_IO.Put ( " C1_Term:" );
      --  Ada.Integer_Text_IO.Put ( C1_Term, 5 );
      --  Ada.Text_IO.Put ( " C2_Term:" );
      --  Ada.Integer_Text_IO.Put ( C2_Term, 5 );
      --  Ada.Text_IO.Put ( " C2B_Term:" );
      --  Ada.Integer_Text_IO.Put ( C2B_Term, 5 );
      --  Ada.Text_IO.Put ( " dUXi:" );
      --  Ada.Float_Text_IO.Put ( DUXi, 3, 0, 0 );
      --  Ada.Text_IO.New_Line;
      
      -- Apply dUXi and update output (if U = 0.0 then V := 1)
      
      NN_Map ( Region ) ( Color ).U := NN_Map ( Region ) ( Color ).U + dUXi;
      
      --  Ada.Text_IO.Put ( "Region" & Nr_Regions_Type'Image ( Region ) & " Color" & 
      --                    Nr_Colors_Type'Image ( Color ) );
      --  Ada.Text_IO.Put ( " U " );
      --  Ada.Float_Text_IO.Put ( NN_Map_Old ( Region ) ( Color ).U, 5, 0, 0 );
      --  Ada.Text_IO.Put ( " + dUXi " );
      --  Ada.Float_Text_IO.Put ( dUxi, 5, 0, 0 );
      --  Ada.Text_IO.Put ( " = U " );
      --  Ada.Float_Text_IO.Put ( NN_Map ( Region ) ( Color ).U, 5, 0, 0 );
      --  Ada.Text_IO.New_Line;
      
      --  if NN_Map ( Region ) ( Color ).U > 0.0 then
      --     NN_Map ( Region ) ( Color ).V := 1;
      --     --  if NN_Map ( Region ) ( Color ).V = 0 then
      --     --     NN_Map ( Region ) ( Color ).V := 1;
      --     --     Dirty := True;
      --     --  end if;   
      --  else
      --     NN_Map ( Region ) ( Color ).V := 0;
      --     --  if NN_Map ( Region ) ( Color ).V = 1 then
      --     --     NN_Map ( Region ) ( Color ).V := 0;
      --     --     Dirty := True;
      --     --  end if;
      --  end if;
      
      --  if dUXi /= 0.0 then
      --     Dirty := True;
      --  end if;
      
   end Motion;
   
   
   
   -- Color - Do the colory bits
   -- Input:   none
   -- Output:  stdio
   -- Changes: NN_Map_Old
   
   procedure Color is
      
      Max_U : Float := 0.0;
      Ganglion_Fired : Boolean := False;
      
   begin
      
      while Dirty loop
         Dirty := False;
         
         for Region in 1 .. Nr_Regions loop
            for Color in 1 .. Nr_Colors loop
               Motion ( Region, Color );
            end loop;
         end loop;
	 
         -- Winner take all in each region group...neuron with highest
	 --    activation energy wins. If two have same, then the lowest
	 --    numbered color wins.
	 
	 for Region in 1 .. Nr_Regions loop
	    Max_U := 0.0;
	    Ganglion_Fired := False;
	    
	    for Color in 1 .. Nr_Colors loop
	       if Max_U < NN_Map ( Region ) ( Color ).U then
		  Max_U := NN_Map ( Region ) ( Color ).U;
	       end if;
	    end loop;
	    
	    for Color in 1 .. Nr_Colors loop
	       if ( NN_Map ( Region ) ( Color ).U > 0.0 ) and
		 ( NN_Map ( Region ) ( Color ).U = Max_U ) and
		 ( not Ganglion_Fired ) then
		  
		  if ( NN_Map ( Region ) ( Color ).V = 0 ) then
		    Dirty := True;
		  end if;
		  
		  NN_Map ( Region ) ( Color ).V := 1;
		  Ganglion_Fired := True;
	       else
		  
		  if ( NN_Map ( Region ) ( Color ).V = 1 ) then
		    Dirty := True;
		  end if;
		  
		  NN_Map ( Region ) ( Color ).V := 0;
		  
	       end if;
	    end loop;
	 end loop;
	 
	 Ada.Text_IO.New_Line;
	 
         for I in 1 .. Nr_Regions loop
            for J in 1 .. Nr_Colors loop
               NN_Map_Old ( I ) ( J ) := NN_Map ( I ) ( J );
            end loop;
         end loop;
         
         Iterations := Iterations + 1;
         
         Ada.Text_IO.Put ( "Iteration: " );
         Ada.Integer_Text_IO.Put ( Iterations, 0 );
         Ada.Text_IO.New_Line;
         Print_NN_Map ( NN_Map );   
         Ada.Text_IO.New_Line;
      end loop;
      
      Ada.Text_IO.Put ( "Map colored in " );
      Ada.Integer_Text_IO.Put ( Iterations, 0 );
      Ada.Text_IO.Put_Line ( " iterations" );
      
   end Color;
   
end Map;
