gen_interval
============

A genereal behaviour for task that needs to be repeated at an interval


Interface
============

The interval/0 callback should return an integer that (in milliseoncds) defines the interval.

handle_interval/0 is called once an interval has passed.
