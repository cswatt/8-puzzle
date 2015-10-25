originally written a long long time ago. 2011? maybe.

### TO RUN
```
(8puzzle n)
```
* does n random permutations on a solved puzzle, then solves that puzzle
* prints your random initial state, then a sequence of successors, and the number of expanded nodes

	OR

```
(8puzzle n initial-state)
```
* in this case the n doesn't matter
* prints the initial state you inputted, then a sequence of successors, and the number of expanded nodes

### examples
```
>> (8puzzle 10)
(1 2 0 3 4 5 6 7 8)
((1 0 2 3 4 5 6 7 8) (0 1 2 3 4 5 6 7 8))
5
```

```
>> (8puzzle 1 '(3 2 0 6 1 5 7 4 8))
(3 2 0 6 1 5 7 4 8)
((3 0 2 6 1 5 7 4 8) (3 1 2 6 0 5 7 4 8) (3 1 2 6 4 5 7 0 8) (3 1 2 6 4 5 0 7 8) (3 1 2 0 4 5 6 7 8) (0 1 2 3 4 5 6 7 8))
17
	-- note that this would be the same for any input for n
```


By default, this program uses Manhattan Distance. To change over to Misplaced Tiles, on lines 91 and 134 replace "manhattan-distance" with "misplaced-tile." (TODO: make this not so clunky)