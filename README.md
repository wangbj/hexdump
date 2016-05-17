# hexdump

hexdump files which is equivalent to unix tool ``hexdump -C``.

wrote this tool to explorer how to write performant haskell code, as well as different I/O model.

This version (intial) mixed Lazy/Strict I/O, consumes constant space, while out perform the standard unix ``hexdump -C`` tool (for a larger file).

threading is not used, run with ``+RTS -Nx`` where ``x > 1`` causes serious performance degration due to parallel GC.

```
$ ~/stack/hexdump $ time cat linux-4.5.tar.xz | stack exec hexdump-exe -- +RTS -N1 -RTS   > /tmp/o2.txt

real    0m38.812s
user    0m27.430s
sys     0m4.206s
$ ~/stack/hexdump $ time cat linux-4.5.tar.xz | hexdump -C > /tmp/o1.txt

real    0m49.765s
user    0m33.708s
sys     0m4.734s

$ diff -ubp /tmp/o1.txt /tmp/o2.txt
$

$ time cat linux-4.5.tar.xz | hexdump -C > /dev/null

real    0m42.766s
user    0m41.863s
sys     0m2.215s

$ time cat linux-4.5.tar.xz | stack exec hexdump-exe -- +RTS -N1 -RTS   > /dev/null

real    0m36.590s
user    0m33.534s
sys     0m3.407s

```

Another run with ``-s``:
```
  25,882,976,616 bytes allocated in the heap
  12,938,854,736 bytes copied during GC
       1,337,296 bytes maximum residency (4880 sample(s))
         116,152 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     44467 colls,     0 par   11.996s  13.663s     0.0003s    0.0213s
  Gen  1      4880 colls,     0 par    6.309s   7.269s     0.0015s    0.0232s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.003s  (  0.022s elapsed)
  MUT     time   14.223s  ( 18.126s elapsed)
  GC      time   18.305s  ( 20.931s elapsed)
  EXIT    time    0.001s  (  0.001s elapsed)
  Total   time   32.977s  ( 39.080s elapsed)

  Alloc rate    1,819,762,933 bytes per MUT second

  Productivity  44.5% of total user, 37.5% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

real    0m40.583s
user    0m27.950s
sys     0m5.692s
```
