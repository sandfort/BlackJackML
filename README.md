# BlackjackML #

Blackjack ML (or bjml) was my project for CSC 466 - Artificial Intelligence II
in my senior year at SUNY Oswego.  It is a blackjack machine learning system.
The main focus of this project was the learning-player class and its methods.
However, it is also possible for a human to play a game, and there are also
machine players using random choices or a predefined set of heuristics.

The learning-player must first learn to play by running a number of games
using random choices.  This is done by setting the "mode" slot to nil.
Then, setting the "mode" slot to t will tell it to begin using its data to
improve its choices.  In practice, the performance increase was about 20%.