{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.AWS.Data.Time
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Test.AWS.Data.Time (tests) where

import Network.AWS.Prelude
import Network.AWS.Data.Time
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time.POSIX
import Test.AWS.Util
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "time"
    [ testGroup
        "text"
        [ testGroup
            "deserialise"
            [ testFromText
                "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT"
                (Text.pack (formatDateTime rfc822Format time)),
              testFromText
                "iso8601"
                "2014-11-07T04:42:13Z"
                (time :: DateTime),
              testFromText
                "aws"
                "20141107T044213Z"
                (Text.pack (formatDateTime awsFormat time))
            ],
          testGroup
            "serialise"
            [ testToText
                "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT"
                (Text.pack (formatDateTime rfc822Format time)),
              testToText
                "iso8601"
                "2014-11-07T04:42:13Z"
                (time :: DateTime),
              testToText
                "aws"
                "20141107T044213Z"
                (Text.pack (formatDateTime awsFormat time))
            ]
        ],
      testGroup
        "query"
        [ testGroup
            "serialise"
            [ testToQuery
                "rfc822"
                "x=Fri%2C%2007%20Nov%202014%2004%3A42%3A13%20GMT"
                (Text.pack (formatDateTime rfc822Format time)),
              testToQuery
                "iso8601"
                "x=2014-11-07T04%3A42%3A13Z"
                (time :: DateTime),
              testToQuery
                "aws"
                "x=20141107T044213Z"
                (Text.pack (formatDateTime awsFormat time))
            ]
        ],
      testGroup
        "xml"
        [ testGroup
            "deserialise"
            [ testFromXML
                "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT"
                (Text.pack (formatDateTime rfc822Format time)),
              testFromXML
                "iso8601"
                "2014-11-07T04:42:13Z"
                (time :: DateTime),
              testFromXML
                "aws"
                "20141107T044213Z"
                (Text.pack (formatDateTime awsFormat time))
            ],
          testGroup
            "serialise"
            [ testToXML
                "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT"
                (Text.pack (formatDateTime rfc822Format time)),
              testToXML
                "iso8601"
                "2014-11-07T04:42:13Z"
                (time :: DateTime),
              testToXML
                "aws"
                "20141107T044213Z"
                (Text.pack (formatDateTime awsFormat time))
            ]
        ],
      testGroup
        "json"
        [ testGroup
            "deserialise"
            [ testFromJSON
                "rfc822"
                (str "Fri, 07 Nov 2014 04:42:13 GMT")
                (Text.pack (formatDateTime rfc822Format time)),
              testFromJSON
                "iso8601"
                (str "2014-11-07T04:42:13Z")
                time,
              testFromJSON
                "aws"
                (str "20141107T044213Z")
                (Text.pack (formatDateTime awsFormat time)),
              testGroup
                "posix"
                [ testFromJSON
                    "integer"
                    "1415335333"
                    timestamp,
                  testFromJSON
                    "double"
                    "1415335333.000"
                    timestamp,
                  testFromJSON
                    "scientific"
                    "1.415335333E9"
                    timestamp
                ]
            ],
          testGroup
            "serialise"
            [ testToJSON
                "rfc822"
                (str "Fri, 07 Nov 2014 04:42:13 GMT")
                (Text.pack (formatDateTime rfc822Format time)),
              testToJSON
                "iso8601"
                (str "2014-11-07T04:42:13Z")
                (time :: DateTime),
              testToJSON
                "aws"
                (str "20141107T044213Z")
                (Text.pack (formatDateTime awsFormat time)),
              testToJSON
                "posix"
                "1415335333"
                timestamp
            ]
        ]
    ]

timestamp :: Timestamp
timestamp = Time $ Time.POSIX.utcTimeToPOSIXSeconds $ fromTime time

time :: DateTime
time = either error id (parseDateTime iso8601Format "2014-11-07T04:42:13Z")
