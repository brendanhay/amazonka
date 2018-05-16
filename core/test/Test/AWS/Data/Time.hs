{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Test.AWS.Data.Time
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.Time (tests) where

import           Network.AWS.Compat.Locale
import           Network.AWS.Compat.Time
import           Network.AWS.Prelude
import           Test.AWS.Util
import           Test.Tasty

tests :: TestTree
tests = testGroup "time"
    [ testGroup "text"
        [ testGroup "deserialise"
            [ testFromText "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT" (time :: RFC822)

            , testFromText "iso8601"
                "2014-11-07T04:42:13.000Z" (time :: ISO8601)

            , testFromText "aws"
                "20141107T044213Z" (time :: AWSTime)
            ]

        , testGroup "serialise"
            [ testToText "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT" (time :: RFC822)

            , testToText "iso8601"
                "2014-11-07T04:42:13Z" (time :: ISO8601)

            , testToText "aws"
                "20141107T044213Z" (time :: AWSTime)
            ]
        ]

    , testGroup "query"
        [ testGroup "serialise"
            [ testToQuery "rfc822"
                "x=Fri%2C%2007%20Nov%202014%2004%3A42%3A13%20GMT" (time :: RFC822)

            , testToQuery "iso8601"
                "x=2014-11-07T04%3A42%3A13Z" (time :: ISO8601)

            , testToQuery "aws"
                "x=20141107T044213Z" (time :: AWSTime)
            ]
        ]

    , testGroup "xml"
        [ testGroup "deserialise"
            [ testFromXML "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT" (time :: RFC822)

            , testFromXML "iso8601"
                "2014-11-07T04:42:13.000Z" (time :: ISO8601)

            , testFromXML "aws"
                "20141107T044213Z" (time :: AWSTime)
            ]

        , testGroup "serialise"
            [ testToXML "rfc822"
                "Fri, 07 Nov 2014 04:42:13 GMT" (time :: RFC822)

            , testToXML "iso8601"
                "2014-11-07T04:42:13Z" (time :: ISO8601)

            , testToXML "aws"
                "20141107T044213Z" (time :: AWSTime)
            ]
        ]

    , testGroup "json"
        [ testGroup "deserialise"
            [ testFromJSON "rfc822"
                (str "Fri, 07 Nov 2014 04:42:13 GMT") (time :: RFC822)

            , testFromJSON "iso8601"
                (str "2014-11-07T04:42:13.000Z") (time :: ISO8601)

            , testFromJSON "aws"
                (str "20141107T044213Z") (time :: AWSTime)

            , testGroup "posix"
                [ testFromJSON "integer"
                    "1415335333" (time :: POSIX)

                , testFromJSON "double"
                    "1415335333.000" (time :: POSIX)

                , testFromJSON "scientific"
                    "1.415335333E9" (time :: POSIX)
                ]
            ]

        , testGroup "serialise"
            [ testToJSON "rfc822"
                (str "Fri, 07 Nov 2014 04:42:13 GMT") (time :: RFC822)

            , testToJSON "iso8601"
                (str "2014-11-07T04:42:13Z") (time :: ISO8601)

            , testToJSON "aws"
                (str "20141107T044213Z") (time :: AWSTime)

            , testToJSON "posix"
                "1415335333" (time :: POSIX)
            ]
        ]
    ]

time :: Time a
time = Time . fromMaybe (error msg) $ parseTime defaultTimeLocale fmt ts
  where
    msg = "Unable to parse time: " ++ ts
    fmt = (iso8601DateFormat (Just "%H:%M:%S"))
    ts  = "2014-11-07T04:42:13"
