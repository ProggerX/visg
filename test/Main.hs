{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Visg.Parser (Action (..), parseGCode)

main =
  defaultMain $
    testGroup
      "parser tests"
      [ testCase
          "basic"
          ( parseGCode "T1M6"
              @?= Right [Action "T1" [('M', 6.0)]]
          )
      , testCase
          "g0"
          ( parseGCode "G0X6Y7"
              @?= Right [Action "G0" [('X', 6.0), ('Y', 7.0)]]
          )
      , testCase
          "g0 with spaces"
          ( parseGCode "G0  X6   Y 7"
              @?= Right [Action "G0" [('X', 6.0), ('Y', 7.0)]]
          )
      , testCase
          "m30"
          ( parseGCode "M30"
              @?= Right [Action "M30" []]
          )
      , testCase
          "complex"
          ( parseGCode
              "T1M6\n\
              \G17\n\
              \G0Z10\n\
              \G0X0Y0\n\
              \G0X5Y50\n\
              \G0X10Y10\n\
              \G1Z-6.5F400\n\
              \G1X5Y10F400\n\
              \G1X25Y10F400\n\
              \G1X25Y60F400\n\
              \G1X45Y60F400\n\
              \G1X45Y45F400\n\
              \G0Z10\n\
              \G0X0Y0\n\
              \M30"
              @?= Right
                [ Action{code = "T1", assocs = [('M', 6.0)]}
                , Action{code = "G17", assocs = []}
                , Action{code = "G0", assocs = [('Z', 10.0)]}
                , Action
                    { code = "G0"
                    , assocs = [('X', 0.0), ('Y', 0.0)]
                    }
                , Action{code = "G0", assocs = [('X', 5.0), ('Y', 50.0)]}
                , Action{code = "G0", assocs = [('X', 10.0), ('Y', 10.0)]}
                , Action
                    { code =
                        "G1"
                    , assocs = [('Z', -6.5), ('F', 400.0)]
                    }
                , Action{code = "G1", assocs = [('X', 5.0), ('Y', 10.0), ('F', 400.0)]}
                , Action
                    { code = "G1"
                    , assocs =
                        [ ('X', 25.0)
                        ,
                          ( 'Y'
                          , 10.0
                          )
                        , ('F', 400.0)
                        ]
                    }
                , Action{code = "G1", assocs = [('X', 25.0), ('Y', 60.0), ('F', 400.0)]}
                , Action
                    { code = "G1"
                    , assocs =
                        [ ('X', 45.0)
                        , ('Y', 60.0)
                        ,
                          ( 'F'
                          , 400.0
                          )
                        ]
                    }
                , Action{code = "G1", assocs = [('X', 45.0), ('Y', 45.0), ('F', 400.0)]}
                , Action{code = "G0", assocs = [('Z', 10.0)]}
                , Action
                    { code = "G0"
                    , assocs =
                        [
                          ( 'X'
                          , 0.0
                          )
                        , ('Y', 0.0)
                        ]
                    }
                , Action{code = "M30", assocs = []}
                ]
          )
      ]
