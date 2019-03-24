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
--         [ requestListTerminologies $
--             listTerminologies
--
--         , requestGetTerminology $
--             getTerminology
--
--         , requestTranslateText $
--             translateText
--
--         , requestImportTerminology $
--             importTerminology
--
--         , requestDeleteTerminology $
--             deleteTerminology
--
--           ]

--     , testGroup "response"
--         [ responseListTerminologies $
--             listTerminologiesResponse
--
--         , responseGetTerminology $
--             getTerminologyResponse
--
--         , responseTranslateText $
--             translateTextResponse
--
--         , responseImportTerminology $
--             importTerminologyResponse
--
--         , responseDeleteTerminology $
--             deleteTerminologyResponse
--
--           ]
--     ]

-- Requests

requestListTerminologies :: ListTerminologies -> TestTree
requestListTerminologies = req
    "ListTerminologies"
    "fixture/ListTerminologies.yaml"

requestGetTerminology :: GetTerminology -> TestTree
requestGetTerminology = req
    "GetTerminology"
    "fixture/GetTerminology.yaml"

requestTranslateText :: TranslateText -> TestTree
requestTranslateText = req
    "TranslateText"
    "fixture/TranslateText.yaml"

requestImportTerminology :: ImportTerminology -> TestTree
requestImportTerminology = req
    "ImportTerminology"
    "fixture/ImportTerminology.yaml"

requestDeleteTerminology :: DeleteTerminology -> TestTree
requestDeleteTerminology = req
    "DeleteTerminology"
    "fixture/DeleteTerminology.yaml"

-- Responses

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies = res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    translate
    (Proxy :: Proxy ListTerminologies)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology = res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    translate
    (Proxy :: Proxy GetTerminology)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText = res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    translate
    (Proxy :: Proxy TranslateText)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology = res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    translate
    (Proxy :: Proxy ImportTerminology)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology = res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    translate
    (Proxy :: Proxy DeleteTerminology)
