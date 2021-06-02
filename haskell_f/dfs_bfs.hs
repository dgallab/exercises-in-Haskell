-- Daniel Gallab
-- project 5
-- depth first search 

import Data.List
import Data.Set (Set)

-- game details
start = [[2,8,3],[1,6,4],[7,0,5]]
depth = 5
goal= [[1,2,3],[8,0,4],[7,6,5]]

-- generates the coordinates of elements adjacent with the 0 element but subtracts moves that would imply a larger board state
move_set (r,c) = [(r-1,c),(r,c+1),(r+1,c),(r,c-1)]  \\  [(0,c),(4,c),(r,0),(r,4)]

-- the 0th element is replaced by an adjacent element
swap (r,c)(a,b)(y:ys)(ol) 
       |r == 1 = (shift c (find_elem (a,b) ol) y):ys
       |otherwise = y:swap(r-1,c)(a,b)(ys)(ol)

-- the aforementioned adjacent element's location is taken by the 0
swip (a,b)(y:ys)
       |a == 1 = (shift b 0 y):ys
       |otherwise = y: swip (a-1,b) ys

-- locate the column, after the row has been determined
shift c r (x:xs)
     |c == 1 = (r:xs) 
     |otherwise = x:shift (c-1) r xs

-- returns value at certain location
find_elem (r,c) (y:ys)
         |r == 1 = find_elem_c (c) (y)
         |otherwise = find_elem (r-1,c) (ys)

-- locates column after row has been determined
find_elem_c c (x:xs)
        |c == 1 = x
        |otherwise = find_elem_c (c-1) xs

-- finds the row with 0
find_0_r (y:ys)
     |elem 0 y = 1 
     |otherwise = 1 + find_0_r ys

-- returns the row with 0
find_0_r2 (y:ys)
     |elem 0 y = y 
     |otherwise = find_0_r2 ys

-- returns column with 0
find_0_c (x:xs)
    |x == 0 = 1
    |otherwise = 1 + find_0_c xs

-- returns location of 0 in coordinates
find_0 ys = (find_0_r ys, find_0_c (find_0_r2 ys))

-- t is where the 0 element is located. f is where it is being swapped. ys is the preceding board state
gen_board f ys = swip f (swap (find_0 ys) f ys ys)

-- takes the move set and initial state to generate a new board state for each possible move
gen_board_set_helper [] _ = []
gen_board_set_helper (f:ms) ys = (gen_board f ys): gen_board_set_helper ms ys            

-- takes initial board state and generates all possible states that can result from a single move
gen_board_set [] = []
gen_board_set ys = gen_board_set_helper(move_set (find_0(ys))) (ys)

-- Breadth First Search

gen_children_b [] = []
gen_children_b (x:xs) = gen_board_set x ++ gen_children_b xs

-- generates the ith set of children. i is the chosen depth
gen_children_b_set ys i
                 | i == 0 || elem goal ys = ys 
                 |otherwise = gen_children_b_set (gen_children_b ys) (i-1)

-- generates tree breadth-wise at height i
gen_tree_b 0 = [start]
gen_tree_b i = addUnique (gen_tree_b (i-1)) (gen_children_b_set [start] i)




-- Depth First Search 


gen_children_d xs i v
                    |i == 0 = [xs] ++ drop v (gen_board_set xs)
                    |otherwise = xs :(gen_children_d (gen_board_set xs !! 0) (i-1) 0) ++ (gen_children_d (gen_board_set xs !! (1)) (i-1) 0)


-- generates tree depth-wise at height i 				
gen_tree_d 0 = [start]
gen_tree_d i = gen_children_d start (i-1) 0





-- miscellaneous functions not specific to the program
makeUnique xs = addUnique [] xs

addUnique sum [] = sum
addUnique sum (n:new)
           |elem n sum = addUnique sum new 
           |otherwise = addUnique (sum ++ [n]) new

