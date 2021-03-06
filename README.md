![compatibility](https://img.shields.io/badge/Mathematica-8.x_9.x_10.x_11.x-brightgreen.svg)

# EntropyGO
We use the concept of entropy maximisation to calculate the influence on a GO board. We provide a Mathematica code and a 
[document](doc/EntropyGO.pdf) explaining the code. We are able to arrive at a simple formula, which can be used to write efficient algorithms. To calculate the influence of, say, the white stones on an empty space V, we calculate how many, how long, and how strong are the paths that link V to the other white stones. See [doc/README.md](doc/README.md) for some illustrating videos. 

It has been suggested that [maximising entropy](https://journals.aps.org/prl/abstract/10.1103/PhysRevLett.110.168702)
is an "intelligent" move in any game. See [Link between Intelligence and Entropy](https://physics.aps.org/articles/v6/46)
for a light discussion on the topic. 


## Simple example
```
Needs["EntropyGO`", NotebookDirectory[] <> "src/EntropyGO.m"]

Board = {{0, 0, 0, 0, 0, 0}, 
         {0, 0, 1, 0, 0, 0}, 
         {0, 0, 0, 0, 0, 0},
         {0, 0, 0, 1, 0, 0},
         {0, 0,-1, 0, 0, 0},
         {0, 0, 0, 0, 0, 0}};
         
LoadBoard[board] (*loads global varibles such as BoardInfluence*)
Draw[]
```
![example board](images/board_example.svg)

To draw the influence of the paths from the top white piece:
```
options = {"boardPaths" -> True};
{boardInf, boardPaths} = BoardInfluenceFromGroup[Groups[1][[1]], 1, options];
(*use Group[-1] to access black groups*)

influenceSize = 3
DrawTentacles[boardInf, influenceSize] (*influenceSize inversely specifies how big influence balls should be. *)
```
![influence from one group](images/group_tentacles.svg)

To draw the balance of influence between white and black 
```
DrawTentacles[BoardInfluence, influenceSize]
```
![influence over whole board](images/tentacles.svg)

The size of the red (blue) balls indicate the influence of the white (black) stone. 

## Big random example
```
GenerateRandomBoard[19]
Draw[]
DrawTentacles[BoardInfluence, 3]
```
![big random board](images/big_board.svg)
![big random board influence](images/big_influence.svg)
