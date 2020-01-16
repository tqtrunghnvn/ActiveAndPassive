# AandP: Utilizing Prolog for converting between active sentence and passive sentence with three-steps conversion

* `convertible.pl`: implementing DCG rules for 1st and 3rd steps in the three-steps conversion, as well as other rules including lexicon.
* `convert.pl`: implementing the three-steps conversion and its 2nd step.
* `testSuite.pl`: providing commands for user interaction. Users do not need to type the input sentence as a list (like `[the, man, buys, an, apple]`) but can type the sentence in the common way (directly type: `the man buys an apple`) by using two commands: `active` and `passive`. Moreover, users can easily check the correctness of the program by using two test suite commands: `activeTestSuite` and `passiveTestSuite`.
