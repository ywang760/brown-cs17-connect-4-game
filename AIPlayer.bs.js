// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Connect4$Game = require("./Connect4.bs.js");

function AIPlayer(MyGame) {
  var nextMove = function (s) {
    var nextStateHelper = function (s) {
      var nextStateHelper2 = function (s, mov) {
        if (!mov) {
          return /* [] */0;
        }
        var movtl = mov.tl;
        var mova = mov.hd;
        if (movtl) {
          return {
                  hd: Curry._1(MyGame.estimateValue, Curry._2(MyGame.nextState, s, mova)),
                  tl: nextStateHelper2(s, movtl)
                };
        } else {
          return {
                  hd: Curry._1(MyGame.estimateValue, Curry._2(MyGame.nextState, s, mova)),
                  tl: /* [] */0
                };
        }
      };
      return nextStateHelper2(s, Curry._1(MyGame.legalMoves, s));
    };
    var netStateH = function (s) {
      var netStateHe = function (s, mov) {
        if (!mov) {
          return /* [] */0;
        }
        var movtl = mov.tl;
        var mova = mov.hd;
        if (movtl) {
          return {
                  hd: Curry._2(MyGame.nextState, s, mova),
                  tl: netStateHe(s, movtl)
                };
        } else {
          return {
                  hd: Curry._2(MyGame.nextState, s, mova),
                  tl: /* [] */0
                };
        }
      };
      return netStateHe(s, Curry._1(MyGame.legalMoves, s));
    };
    var getMax = function (_alof) {
      while(true) {
        var alof = _alof;
        if (!alof) {
          return Pervasives.failwith("");
        }
        var tl = alof.tl;
        var a = alof.hd;
        if (!tl) {
          return a;
        }
        if (a >= getMax(tl)) {
          return a;
        }
        _alof = tl;
        continue ;
      };
    };
    var getMin = function (_alof) {
      while(true) {
        var alof = _alof;
        if (!alof) {
          return Pervasives.failwith("");
        }
        var tl = alof.tl;
        var a = alof.hd;
        if (!tl) {
          return a;
        }
        if (a <= getMin(tl)) {
          return a;
        }
        _alof = tl;
        continue ;
      };
    };
    var getMax2 = function (_alof) {
      while(true) {
        var alof = _alof;
        if (!alof) {
          return Pervasives.failwith("");
        }
        var tl = alof.tl;
        var a = alof.hd;
        if (!tl) {
          return a;
        }
        if (Curry._1(MyGame.estimateValue, a) >= Curry._1(MyGame.estimateValue, getMax2(tl))) {
          return a;
        }
        _alof = tl;
        continue ;
      };
    };
    var getMin2 = function (_alof) {
      while(true) {
        var alof = _alof;
        if (!alof) {
          return Pervasives.failwith("");
        }
        var tl = alof.tl;
        var a = alof.hd;
        if (!tl) {
          return a;
        }
        if (Curry._1(MyGame.estimateValue, a) <= Curry._1(MyGame.estimateValue, getMin2(tl))) {
          return a;
        }
        _alof = tl;
        continue ;
      };
    };
    var minimax = function (_s, _d) {
      while(true) {
        var d = _d;
        var s = _s;
        var match = Curry._1(MyGame.gameStatus, s);
        if (typeof match === "number") {
          return 0;
        }
        if (match.TAG === /* Win */0) {
          if (match._0) {
            return -100000000000000000000000000;
          } else {
            return 100000000000000000000000000;
          }
        }
        if (match._0) {
          if (d === 0) {
            return getMin(nextStateHelper(s));
          }
          _d = d - 1 | 0;
          _s = getMin2(netStateH(s));
          continue ;
        }
        if (d === 0) {
          return getMax(nextStateHelper(s));
        }
        _d = d - 1 | 0;
        _s = getMax2(netStateH(s));
        continue ;
      };
    };
    minimax(s, 4);
    return Pervasives.failwith("we dont have time");
  };
  return {
          PlayerGame: MyGame,
          nextMove: nextMove,
          playerName: "Spike the smart squirrel"
        };
}

var MyGame_stringOfPlayer = Connect4$Game.Connect4.stringOfPlayer;

var MyGame_stringOfState = Connect4$Game.Connect4.stringOfState;

var MyGame_stringOfMove = Connect4$Game.Connect4.stringOfMove;

var MyGame_initialState = Connect4$Game.Connect4.initialState;

var MyGame_legalMoves = Connect4$Game.Connect4.legalMoves;

var MyGame_gameStatus = Connect4$Game.Connect4.gameStatus;

var MyGame_nextState = Connect4$Game.Connect4.nextState;

var MyGame_moveOfString = Connect4$Game.Connect4.moveOfString;

var MyGame_estimateValue = Connect4$Game.Connect4.estimateValue;

var MyGame = {
  stringOfPlayer: MyGame_stringOfPlayer,
  stringOfState: MyGame_stringOfState,
  stringOfMove: MyGame_stringOfMove,
  initialState: MyGame_initialState,
  legalMoves: MyGame_legalMoves,
  gameStatus: MyGame_gameStatus,
  nextState: MyGame_nextState,
  moveOfString: MyGame_moveOfString,
  estimateValue: MyGame_estimateValue
};

function nextMove(s) {
  var nextStateHelper = function (s) {
    var nextStateHelper2 = function (s, mov) {
      if (!mov) {
        return /* [] */0;
      }
      var movtl = mov.tl;
      var mova = mov.hd;
      if (movtl) {
        return {
                hd: Curry._1(Connect4$Game.Connect4.estimateValue, Curry._2(Connect4$Game.Connect4.nextState, s, mova)),
                tl: nextStateHelper2(s, movtl)
              };
      } else {
        return {
                hd: Curry._1(Connect4$Game.Connect4.estimateValue, Curry._2(Connect4$Game.Connect4.nextState, s, mova)),
                tl: /* [] */0
              };
      }
    };
    return nextStateHelper2(s, Curry._1(Connect4$Game.Connect4.legalMoves, s));
  };
  var netStateH = function (s) {
    var netStateHe = function (s, mov) {
      if (!mov) {
        return /* [] */0;
      }
      var movtl = mov.tl;
      var mova = mov.hd;
      if (movtl) {
        return {
                hd: Curry._2(Connect4$Game.Connect4.nextState, s, mova),
                tl: netStateHe(s, movtl)
              };
      } else {
        return {
                hd: Curry._2(Connect4$Game.Connect4.nextState, s, mova),
                tl: /* [] */0
              };
      }
    };
    return netStateHe(s, Curry._1(Connect4$Game.Connect4.legalMoves, s));
  };
  var getMax = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("");
      }
      var tl = alof.tl;
      var a = alof.hd;
      if (!tl) {
        return a;
      }
      if (a >= getMax(tl)) {
        return a;
      }
      _alof = tl;
      continue ;
    };
  };
  var getMin = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("");
      }
      var tl = alof.tl;
      var a = alof.hd;
      if (!tl) {
        return a;
      }
      if (a <= getMin(tl)) {
        return a;
      }
      _alof = tl;
      continue ;
    };
  };
  var getMax2 = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("");
      }
      var tl = alof.tl;
      var a = alof.hd;
      if (!tl) {
        return a;
      }
      if (Curry._1(Connect4$Game.Connect4.estimateValue, a) >= Curry._1(Connect4$Game.Connect4.estimateValue, getMax2(tl))) {
        return a;
      }
      _alof = tl;
      continue ;
    };
  };
  var getMin2 = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("");
      }
      var tl = alof.tl;
      var a = alof.hd;
      if (!tl) {
        return a;
      }
      if (Curry._1(Connect4$Game.Connect4.estimateValue, a) <= Curry._1(Connect4$Game.Connect4.estimateValue, getMin2(tl))) {
        return a;
      }
      _alof = tl;
      continue ;
    };
  };
  var minimax = function (_s, _d) {
    while(true) {
      var d = _d;
      var s = _s;
      var match = Curry._1(Connect4$Game.Connect4.gameStatus, s);
      if (typeof match === "number") {
        return 0;
      }
      if (match.TAG === /* Win */0) {
        if (match._0) {
          return -100000000000000000000000000;
        } else {
          return 100000000000000000000000000;
        }
      }
      if (match._0) {
        if (d === 0) {
          return getMin(nextStateHelper(s));
        }
        _d = d - 1 | 0;
        _s = getMin2(netStateH(s));
        continue ;
      }
      if (d === 0) {
        return getMax(nextStateHelper(s));
      }
      _d = d - 1 | 0;
      _s = getMax2(netStateH(s));
      continue ;
    };
  };
  minimax(s, 4);
  return Pervasives.failwith("we dont have time");
}

var TestAIPlayer = {
  PlayerGame: MyGame,
  nextMove: nextMove,
  playerName: "Spike the smart squirrel"
};

var TestGame;

var MyAIPlayer = TestAIPlayer;

exports.AIPlayer = AIPlayer;
exports.TestGame = TestGame;
exports.TestAIPlayer = TestAIPlayer;
exports.MyAIPlayer = MyAIPlayer;
/* Connect4-Game Not a pure module */
