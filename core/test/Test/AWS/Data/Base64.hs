{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.Data.Base64
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Data.Base64 (tests) where

import           Data.Monoid
import           Data.String
import           Network.AWS.Prelude
import           Network.HTTP.Types
import           Test.AWS.Util
import           Test.Tasty

tests :: TestTree
tests = testGroup "base64"
    [ testGroup "text"
        [ testFromText "deserialise" encoded decoded
        , testToText   "serialise"   encoded decoded
        ]

    , testGroup "query"
        [ testToQuery "serialise" ("x=" <> urlEncode True encoded) decoded
        ]

    , testGroup "xml"
        [ testFromXML "deserialise" encoded decoded
        , testToXML   "serialise"   encoded decoded
        ]

    , testGroup "json"
        [ testFromJSON "deserialise" (str encoded) decoded
        , testToJSON   "serialise"   (str encoded) decoded
        ]
    ]

encoded :: IsString a => a
encoded = "U2VkIHV0IHBlcnNwaWNpYXRpcyB1bmRlIG9tbmlzIGlzdGUgbmF0dXMgZXJyb3Igc2l0IHZvbHVwdGF0ZW0="

decoded :: Base64
decoded = Base64 "Sed ut perspiciatis unde omnis iste natus error sit voluptatem"
