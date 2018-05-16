{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Connect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Connect where

import Data.Proxy
import Network.AWS.Connect
import Test.AWS.Connect.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartOutboundVoiceContact $
--             startOutboundVoiceContact
--
--         , requestStopContact $
--             stopContact
--
--           ]

--     , testGroup "response"
--         [ responseStartOutboundVoiceContact $
--             startOutboundVoiceContactResponse
--
--         , responseStopContact $
--             stopContactResponse
--
--           ]
--     ]

-- Requests

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact = req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact = req
    "StopContact"
    "fixture/StopContact.yaml"

-- Responses

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact = res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    connect
    (Proxy :: Proxy StartOutboundVoiceContact)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact = res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    connect
    (Proxy :: Proxy StopContact)
