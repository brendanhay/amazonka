-- |
-- Module      : Test.Amazonka.Query.List
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Data.Query (tests) where

import Amazonka.Data
import Amazonka.Prelude
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "query"
    [ testGroup
        "fromString"
        [ testCase "key" $
            parseQueryString "foo"
              @?= QList
                [ QPair "foo" (QValue Nothing)
                ],
          testCase "key=" $
            parseQueryString "foo="
              @?= QList
                [ QPair "foo" (QValue Nothing)
                ],
          testCase "key=value" $
            parseQueryString "foo=bar"
              @?= QList
                [ QPair "foo" (QValue (Just "bar"))
                ],
          testCase "key&.." $
            parseQueryString "foo&bar&baz"
              @?= QList
                [ QPair "foo" (QValue Nothing),
                  QPair "bar" (QValue Nothing),
                  QPair "baz" (QValue Nothing)
                ],
          testCase "key=value&.." $
            parseQueryString "foo=1&bar=2&baz&qux=3"
              @?= QList
                [ QPair "foo" (QValue (Just "1")),
                  QPair "bar" (QValue (Just "2")),
                  QPair "baz" (QValue Nothing),
                  QPair "qux" (QValue (Just "3"))
                ]
        ],
      testGroup
        "toBS"
        [ testCase "SQS.DeleteMessageBatchRequestEntries" $
            toBS
              ( QPair
                  "DeleteMessageBatchRequestEntry"
                  ( QList
                      [ QPair
                          "1"
                          ( QList
                              [ QPair "Id" (QValue (Just "someId")),
                                QPair "ReceiptHandle" (QValue (Just "someReceiptMessageHandle"))
                              ]
                          ),
                        QPair
                          "2"
                          ( QList
                              [ QPair "Id" (QValue (Just "anotherId")),
                                QPair "ReceiptHandle" (QValue (Just "anotherReceiptMessageHandle"))
                              ]
                          )
                      ]
                  )
              )
              @?= "DeleteMessageBatchRequestEntry.1.Id=someId&\
                  \DeleteMessageBatchRequestEntry.1.ReceiptHandle=someReceiptMessageHandle&\
                  \DeleteMessageBatchRequestEntry.2.Id=anotherId&\
                  \DeleteMessageBatchRequestEntry.2.ReceiptHandle=anotherReceiptMessageHandle"
        ]
    ]
