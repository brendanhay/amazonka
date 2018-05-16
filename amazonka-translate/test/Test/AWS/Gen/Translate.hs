{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Translate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Translate where

import Data.Proxy
import Network.AWS.Translate
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Translate.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestTranslateText $
--             translateText
--
--           ]

--     , testGroup "response"
--         [ responseTranslateText $
--             translateTextResponse
--
--           ]
--     ]

-- Requests

requestTranslateText :: TranslateText -> TestTree
requestTranslateText = req
    "TranslateText"
    "fixture/TranslateText.yaml"

-- Responses

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText = res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    translate
    (Proxy :: Proxy TranslateText)
