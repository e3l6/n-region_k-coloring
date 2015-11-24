--------------------------------------------------------------------------------
--
-- Eric Laursen, 25 October 2015, CS 441-001 Term Project
--
-- map.adb -- Map representation body for K-colorability problem
--
-- Implementing Y. Takefuji, et al., solution from Neural Network Parallel
--    Computing, chapter 3
--
-- 2015-11-11 - Added "ganglia" for each region to implement winner-takes-all
--              strategy to ensure only one neuron in the region is fired.
--
-- 2015-11-23 - Refactored a bunch of this to parallelize it
--
--------------------------------------------------------------------------------

package body Map is

   Nr_Colors          : Nr_Colors_Type;
   Nr_Regions         : Nr_Regions_Type;
   Adjacency_Matrix   : access Adjacency_Matrix_Type;
   NN_Map             : access Map_Type;
   Table_Col_Width    : Natural;
   Dirty              : Boolean := True;  -- True if Motion changed a neuron
                                          -- Set to false at the beginning of every iteration
                                          -- Starts as True, because the map is new (it rhymes)
   Iterations         : Natural := 0;
   RNG                : Ada.Numerics.Float_Random.Generator;
   
   
   
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
            --  Ada.Text_IO.Put ( "Region " );
            --  Ada.Integer_Text_IO.Put ( Tmp_X, 0 );
            --  Ada.Text_IO.Put ( " adjacent to region " );
            --  Ada.Integer_Text_IO.Put ( Tmp_Y, 0 );
            --  Ada.Text_IO.New_LIne;
            
            Adjacency_Matrix ( Tmp_X, Tmp_Y ) := 1;
            Adjacency_Matrix ( Tmp_Y, Tmp_X ) := 1;
            
         end if;
      end loop;
      
      Ada.Text_IO.Put_Line ( "Closing """ & File_Name & """" );
      Ada.Text_IO.Close ( Map_File );
      
      Ada.Text_IO.New_Line;
      
      Print_Adjacency_Matrix;
      
      Ada.Text_IO.Put_Line ( "Creating neural map" );
      
      NN_Map := new Map_Type ( 1 .. Nr_Regions );
      
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
      
      --  Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Table_Col_Width + 1 ) );
      
      --  for I in 1 .. Nr_Regions loop
      --     Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
      --  end loop;
      
      Ada.Text_IO.New_Line;
      
      for I in 1 .. Nr_Regions loop
         --  Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
         
         --  for J in 1 .. Nr_Regions loop
         --     if Adjacency_Matrix ( I, J ) = 1 then
         --        Ada.Integer_Text_IO.Put ( Adjacency_Matrix ( I, J ),
         --                                  Table_Col_Width );
         --     else
         --        Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) +
         --                                                  Table_Col_Width ) );
         --     end if;
         for J in 1 .. Nr_Regions loop
            if Adjacency_Matrix ( I, J ) = 1 then
               Ada.Integer_Text_IO.Put ( Adjacency_Matrix ( I, J ), 0 );
            else
               Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) + 1 ) );
            end if;
         end loop;
         
         Ada.Text_IO.New_Line;
      end loop;
      
      Ada.Text_IO.New_Line;
      
   end Print_Adjacency_Matrix;
   
   
   
   -- Map_Initialize - Initialize the NN map. It's a N-row dynamic array of access types
   --                  to a K-element array of neurons (N region, K color).
   -- Input:   none
   -- Output:  stdio
   -- Changes: NN_Map
   -- Accepts: Start, Report
   -- Issues:  Color.Start
   
   task body Map_Initialize is
      
      Reports : Natural := 0;
      
   begin
      
      accept Start;
      
      Ada.Numerics.Float_Random.Reset ( RNG );
      
      Ada.Text_IO.Put_Line ( "RNG Seeded" );
      
      Ada.Text_IO.Put_Line ( "Initializing Map" );
      
      for I in 1 .. Nr_Regions loop
         NN_Map ( I ) := new Region_Type ( Nr_Colors );
         --  Ada.Text_IO.Put ( "Created region " );
         --  Ada.Integer_Text_IO.Put ( I, 0 );
         --  Ada.Text_IO.New_Line;
      end loop;
      
      for I in 1 .. Nr_Regions loop
         --  Ada.Text_IO.Put ( "Activating region " );
         --  Ada.Integer_Text_IO.Put ( I, 0 );
         --  Ada.Text_IO.New_Line;
         NN_Map ( I ).Activate.Initialize ( I );
      end loop;
      
      while Reports < Nr_Regions loop
         accept Report do
            Reports := Reports + 1;
         end Report;
      end loop;
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ( "NN_Map:" );
      Print_NN_Map;
      Ada.Text_IO.New_Line;
      
      Color_Task.Start;
      
   end Map_Initialize;
   
   
   
   -- Print_NN_Map - Display the current iteration of NN_Map
   -- Input:   Map_Type
   -- Output:  stdio
   -- Changes: none
   
   procedure Print_NN_Map is
      
   begin
      
      --  Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Table_Col_Width + 1 ) );
      
      for I in 1 .. Nr_Regions loop
         --  Ada.Integer_Text_IO.Put ( I, Table_Col_Width );
         if ( I mod 10 ) = 0 then
            Ada.Text_IO.Put ( "-" );
         else
            Ada.Text_IO.Put ( " " );
         end if;
      end loop;
      
      Ada.Text_IO.New_Line;
      
      for J in 1 .. Nr_Colors loop
         --  Ada.Integer_Text_IO.Put ( J, Table_Col_Width );
         
         --  for I in 1 .. Nr_Regions loop
         --     if NN_Map ( I ).Neurons ( J ).V = 1 then
         --        Ada.Integer_Text_IO.Put ( 1, Table_Col_Width );
         --     else
         --        Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) +
         --                                                  Table_Col_Width ) );
         --     end if;
         --  end loop;         
         for I in 1 .. Nr_Regions loop
            if NN_Map ( I ).Neurons ( J ).V = 1 then
               Ada.Integer_Text_IO.Put ( 1, 0 );
            else
               Ada.Text_IO.Set_Col ( Ada.Text_IO.Count ( Integer ( Ada.Text_IO.Col ) + 1 ) );
            end if;
         end loop;
         
         Ada.Text_IO.New_Line;
      end loop;
      
   end Print_NN_Map;
   
   
   
   -- Motion - Calculate and apply motion
   -- Input:   Nr_Regions_Type, Nr_Colors_Type
   -- Output:  none
   -- Changes: NN_Map ( Region ).Neurons ( Color ).U, NN_Map ( Region ).Neurons ( Color ).V
   -- Accepts: Initialize, Pulse
   -- Issues:  Activate_Task.Report
   
   task body Motion_Task is

      My_Region : Nr_Regions_Type;
      My_Color  : Nr_Colors_Type;
      
      dUXi : Float := 0.0;          -- Calculated energy change for a neuron
      A, B, C, C2 : Float := 1.0;   -- Coefficients
      C1 : Float := 1.0;
      A_Term, B_Term, B2_Term, H, H_Term, C_Term, C1_Term,
        C2_Term, C2B_Term, C2C_Term : Integer := 0;   -- Different components of the motion
                                                      -- equation
   begin
      accept Initialize ( Region : Nr_Regions_Type;
                          Color  : Nr_Colors_Type  ) do
         
         My_Region := Region;
         My_Color  := Color;
         
      end Initialize;
      
      NN_Map ( My_Region ).Neurons ( My_Color ).U := Float'Rounding ( ( Ada.Numerics.Float_Random.Random ( RNG ) - 0.5 ) * 10.0 );
      
      NN_Map ( My_Region ).Activate.Report;  -- Task is ready to begin
      
      -- Task just loops, waiting for the pulse notification, does the computation, and reports
      --    back its region task.
      loop
         select
            accept Calculate;
            
            dUXi     := 0.0;
            A_Term   := 0;
            B_Term   := 0;
            B2_Term  := 0;
            H        := 0;
            H_Term   := 0;
            C_Term   := 0;
            C1_Term  := 0;
            C2_Term  := 0;
            C2B_Term := 0;
            C2C_Term := 0;
            
            -- Compute the A term
            for J in 1 .. Nr_Colors loop
               A_Term := A_Term + ( Integer ( NN_Map ( My_Region ).Neurons ( J ).V ) - 1 );
            end loop;
            
            -- Compute the B term
            for Y in 1 .. Nr_Regions loop
               B2_Term := 0;         -- Clear the sum of nr of neighbors to this neighbor
               --  if Y /= My_Region then   -- Don't check a region against itself
                  for K in 1 .. Nr_Regions loop
                     B2_Term := B2_Term + Integer ( Adjacency_Matrix ( Y, K ) );
                  end loop;
                  
                  B_Term := B_Term + ( ( Integer ( Adjacency_Matrix ( My_Region, Y ) ) *
                                         Integer ( NN_Map ( Y ).Neurons ( My_Color ).V ) ) * B2_Term );
               --  end if;
            end loop;
            
            -- Compute the H term
            for J in 1 .. Nr_Colors loop
               H_Term := H_Term + Integer ( NN_Map ( My_Region ).Neurons ( J ).V );
            end loop;
            
            if H_Term = 0 then   -- This is h(x). h(x) = 1 if no colors are active for this region
               H := 1;
            else
               H := 0;
            end if;
            
            -- Compute the C1 term
            for K in 1 .. Nr_Regions loop
               C1_Term := C1_Term + Integer ( Adjacency_Matrix ( My_Region, K ) );
            end loop;
            
            -- Compute the C2 term
            for K in 1 .. Nr_Regions loop
               for Y in 1 .. Nr_Regions loop
                  C2_Term := C2_Term + ( Integer ( Adjacency_Matrix ( My_Region, Y ) ) *
                                         Integer ( Adjacency_Matrix ( Y, K ) ) );
               end loop;
            end loop;
            
            for K in 1 .. Nr_Regions loop
               C2B_Term := C2B_Term + Integer ( Adjacency_Matrix ( My_Region, K ) );
            end loop;
            
            -- Add it all up for dUXi
            dUXi := ( -1.0 * A * Float ( A_Term ) ) - ( B * Float ( B_Term ) ) + 
              ( C * Float ( H ) * ( ( C1 * Float ( C1_Term ) ) + 
                                    ( C2 * Float ( C2_Term ) / Float ( C2B_Term ) ) ) );
            
            -- Apply dUXi
            
            NN_Map ( My_Region ).Neurons ( My_Color ).U := NN_Map ( My_Region ).Neurons ( My_Color ).U + dUXi;
            
            NN_Map ( My_Region ).Activate.Report;
         or
            terminate;
         end select;
      end loop;
      
   end Motion_Task;
   
   
   
   -- Activate_task - Determine the neuron in the region with the highest input and activate it,
   --                 deactivate the others
   -- Input:   none
   -- Output:  stdio
   -- Changes: NN_Map
   -- Accepts: Initialize, Report, Pulse
   -- Issues:  Color.Report
   
   task body Activate_Task is
      
      My_Region      : Nr_Regions_Type;
      Max_U          : Float := 0.0;
      Ganglion_Fired : Boolean := False;
      Reports        : Natural := 0;
      
   begin
      
      accept Initialize ( Region : Nr_Regions_Type ) do
         My_Region := Region;
      end initialize;
      
      for Color in 1 .. Nr_Colors loop
         NN_Map ( My_Region ).Neurons ( Color ).Motion.Initialize ( My_Region, Color );
      end loop;
      
      while Reports < Nr_Colors loop
         accept Report do
            Reports := Reports + 1;
         end Report;
      end loop;
      
      Max_U := 0.0;
      Ganglion_Fired := False;
      
      for Color in 1 .. Nr_Colors loop
         if Max_U < NN_Map ( My_Region ).Neurons ( Color ).U then
            Max_U := NN_Map ( My_Region ).Neurons ( Color ).U;
         end if;
      end loop;
      
      for Color in 1 .. Nr_Colors loop
         if ( not Ganglion_Fired ) and 
           ( NN_Map ( My_Region ).Neurons ( Color ).U > 0.0 ) and
           ( NN_Map ( My_Region ).Neurons ( Color ).U = Max_U ) then
            
            if ( NN_Map ( My_Region ).Neurons ( Color ).V = 0 ) then
               Dirty := True;
            end if;
            
            NN_Map ( My_Region ).Neurons ( Color ).V := 1;
            Ganglion_Fired := True;
         else
            
            if ( NN_Map ( My_Region ).Neurons ( Color ).V = 1 ) then
               Dirty := True;
            end if;
            
            NN_Map ( My_Region ).Neurons ( Color ).V := 0;
            
         end if;
      end loop;
      
      Map_initialize.Report;
      
      loop
         select
            accept Calculate;

            for Color in 1 .. Nr_Colors loop
               NN_Map ( My_Region ).Neurons ( Color ).Motion.Calculate;
            end loop;
            
            Reports := 0;
            
            while Reports < Nr_Colors loop
               accept Report;
               Reports := Reports + 1;
            end loop;
            
            Color_Task.Report;
            
            -- Block and wait for task Color to signal an update
            accept Update do
               null;
            end Update;
            
            Max_U := 0.0;
            Ganglion_Fired := False;
            
            for Color in 1 .. Nr_Colors loop
               if Max_U < NN_Map ( My_Region ).Neurons ( Color ).U then
                  Max_U := NN_Map ( My_Region ).Neurons ( Color ).U;
               end if;
            end loop;
            
            for Color in 1 .. Nr_Colors loop
               if ( not Ganglion_Fired ) and 
                 ( NN_Map ( My_Region ).Neurons ( Color ).U > 0.0 ) and
                 ( NN_Map ( My_Region ).Neurons ( Color ).U = Max_U ) then
                  
                  if ( NN_Map ( My_Region ).Neurons ( Color ).V = 0 ) then
                     Dirty := True;
                  end if;
                  
                  NN_Map ( My_Region ).Neurons ( Color ).V := 1;
                  Ganglion_Fired := True;
               else
                  if ( NN_Map ( My_Region ).Neurons ( Color ).V = 1 ) then
                     Dirty := True;
                  end if;
                  
                  NN_Map ( My_Region ).Neurons ( Color ).V := 0;
                  
               end if;
            end loop;
            
            Color_Task.Report;
         or
            terminate;
         end select;
      end loop;
      
   end Activate_Task;
   
   
   -- Color - Do the colory bits
   -- Input:   none
   -- Output:  stdio
   -- Changes: NN_Map_Old
   -- Accepts: Start, Report
   -- Issues:  Neuron_Type.Motion.Initialize, Neuron_Type.Motion.Pulse
   --  procedure Color is
   
   task body Color_Task is
      
      Reports : Natural := 0;
      
   begin
      
      accept Start do
         Ada.Text_IO.Put_Line ( "Coloring map..." );
      end Start;
      
      while Dirty loop
         Dirty := False;
         
         for Region in 1 .. Nr_Regions loop
            NN_Map ( Region ).Activate.Calculate;
         end loop;
         
         Reports := 0;
         
         while Reports < Nr_Regions loop
            accept Report do
               Reports := Reports + 1;
            end Report;
         end loop;
         
         -- Winner take all in each region group...neuron with highest
         --    activation energy wins. If two have same, then the lowest
         --    numbered color wins.
         
         for Region in 1 .. Nr_Regions loop
            NN_Map ( Region ).Activate.Update;
         end loop;
         
         Reports := 0;
         
         while Reports < Nr_Regions loop
            accept Report do
               Reports := Reports + 1;
            end Report;
         end loop;
         
         Ada.Text_IO.New_Line;
         
         if Dirty then
            Iterations := Iterations + 1;
            
            Ada.Text_IO.Put ( "Iteration: " );
            Ada.Integer_Text_IO.Put ( Iterations, 0 );
            Ada.Text_IO.New_Line;
            Print_NN_Map;
            Ada.Text_IO.New_Line;
         end if;
      end loop;
      
      Ada.Text_IO.Put ( "Map colored in " );
      Ada.Integer_Text_IO.Put ( Iterations, 0 );
      Ada.Text_IO.Put_Line ( " iterations" );
      
   end Color_Task;
   
end Map;
