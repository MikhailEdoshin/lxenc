``lxenc``
=========

``lxenc`` is a module to handle encodings.

Currently ``lxenc`` provides code to read, encode, and decode UTF-8 and
UTF-16, BE or LE. The code to read and encode is basic. The code to decode 
uses a stream-based decoder that is fed a byte a time with extensive error
handling.

An example of intended use is in ``src/lxtool.c``, function
``LxTestCmd_EncConv``.

Design
------

The module presumes there will be trusted and non-trusted text objects. A 
trusted text object contains data that is known to be correctly encoded. A 
non-trusted text object contains data that whose status is unknown. All but 
one text operations work with trusted strings. The only one operation that 
works with non-trusted strings is decoding. Once a string is successfully 
decoded it becomes trusted.