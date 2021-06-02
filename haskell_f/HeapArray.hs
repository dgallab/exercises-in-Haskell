-- Daniel Gallab
-- Design a HeapArray data structure which can call
-- multiple functions on itself, and utilize the structure
-- to implement a sorting method

data Heap a = Node a (Heap a) (Heap a)
             |Nil
             deriving (Show,Eq)

-- test trees			 
tree1 = Node 1 (Node 5 Nil Nil) (Node 6 Nil Nil) -- balanced tree
tree2 = Node 1 Nil Nil -- root 
tree3 = Node 1 (Node 4 Nil Nil) Nil -- node with only a leftChild

-- leftmost branch will have the larger/equal height
height (Node _ l _) = 1 + height l 
height Nil = 0 

-- checks to see if tree is balanced
isFull (Node y l r) 
              |l /= Nil && r == Nil = False
              |l == Nil && r == Nil = True
              |otherwise = isFull l && isFull r

-- a leaf will have 0 children
isLeaf (Node y l r)
           |l== Nil && r == Nil = True
           |otherwise = False

-- insert must take into account several factors, such as which children if any has a node 
-- insert also "trickles up" as it descends the tree (it checks to see if the inserted node
-- is smaller than than value it is at in the tree, and if it is, it switches it and inserts the new node
insert x Nil = Node x Nil Nil
insert x (Node y l r)
        | l == Nil && r == Nil = Node (min x y) (Node (max x y) Nil Nil) Nil
        | l /= Nil && r == Nil = Node (min x y) l (Node (max x y) Nil Nil)
        | (isLeaf l || isFull l == False) || (isFull r && isLeaf r == False) = Node (min x y) (insert (max x y) l) r
        | otherwise = Node (min x y) l (insert (max x y) r)

-- min will always be the root of the tree
getMin Nil = error "empty heap"
getMin (Node x l r) = x

-- last child is the rightmost leaf at max height 
lastChild (Node x l r)
       | l == Nil && r == Nil = x
       | height l > height r = lastChild l
       | otherwise = lastChild r
 
-- to keep the tree balanced, we must delete the last child
deleteLast (Node y l r)
            | l == Nil && r == Nil = Nil
            | height l > height r = Node y (deleteLast l) r
            | otherwise = Node y l (deleteLast r)

-- deleteMin replaces the root with the last child, deletes the last child, 
-- then trickles down the new root to the correct location in the tree
deleteMin Nil = error "empty heap"
deleteMin (Node y Nil Nil) = Nil
deleteMin (Node y l r) = trickleDown(deleteLast(Node (lastChild (Node y l r)) l r))

-- switches the value of the node with another
changevalue x (Node y l r) = Node x l r

getValue (Node x l r) = x

-- trickleDown takes a tree and descends the node to the correct location
-- note that after deleting a node, the next smallest will be located in the second level of the
-- tree (if it exists). Changevalue is used to change the value of the node which we are descending
-- with the node we are at in the tree (similar to insert)
trickleDown (Node y l r)
           | l==Nil && r==Nil = Node y l r
           | l/=Nil && r==Nil && getValue(l) < y = Node (getValue l) (Node y Nil Nil) Nil
           | l/=Nil && r==Nil && getValue(l) >= y = Node y l r
           | y < getValue l && y < getValue r = Node y l r 
           | getValue l > getValue r = Node (getValue r) l (trickleDown (changevalue y r))
           | otherwise  = Node (getValue l) (trickleDown (changevalue y l)) r

-- builds a heap given a list by calling insert on head of the list repeatedly
buildHeap [] heap = heap
buildHeap (x:xs) heap = buildHeap xs (insert x heap)

-- takes a list and sorts it using heapToList
-- needs a helper function because heapsort cannot take a new tree as 
-- a parameter
heapSort [] = []
heapSort xs = heapToList (buildHeap xs Nil)

-- gets the min of the tree, then shrinks it while adding the min to a new list
heapToList Nil =[]
heapToList t= getMin(t): heapToList(deleteMin t)
                                                                                