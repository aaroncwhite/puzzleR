# puzzleR
I like puzzles.  My friend gave me one a while back, and I solved it in about five minutes.  Not one to give up quickly, 
my friend decided to go and find the hardest puzzle he could find. He got me this one:  https://www.gaya-game.com/products/17-pieces-packing-problem?variant=9237856963

Several hours in, I realized I was always trying the same possibilities and decided to approach the solution more systematically.  

This program does the following:
* generates the 17 pieces and represents each as 3d array. Each piece is then rotated to find all of the possible orientations that it could have in a 3d space on x,y,z axes. 
* represents the full puzzle as an empty 5x5x5 array. Each piece at each potential orientation is then placed in the array recursively finding each potential solution.
* a tree_length can be defined to determine how deep the recursion goes before returning back to evaluate current solutions and trim any duplicates.
* if there are still pieces remaining, the process will continue until it finds a solution, or all possible solution branches end in failure. 
* the filling and trimming steps run in parallel on the host machine to take advantage multiple cores due of the high amounts of processor power needed to calculate solutions. 

final result.R has the steps laid out to recalculate the final solution in a reasonable amount of time and vizualize the result using OpenGL.  


I wasn't happy when I found this video after working on all of this: https://www.youtube.com/watch?v=S-IgYLym10o
