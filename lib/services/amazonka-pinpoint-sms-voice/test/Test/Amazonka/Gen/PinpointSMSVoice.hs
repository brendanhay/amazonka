{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PinpointSMSVoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PinpointSMSVoice where

import Amazonka.PinpointSMSVoice
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PinpointSMSVoice.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinations
--
--         , requestSendVoiceMessage $
--             newSendVoiceMessage
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--           ]

--     , testGroup "response"
--         [ responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinationsResponse
--
--         , responseSendVoiceMessage $
--             newSendVoiceMessageResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--           ]
--     ]

-- Requests

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinations -> TestTree
requestGetConfigurationSetEventDestinations =
  req
    "GetConfigurationSetEventDestinations"
    "fixture/GetConfigurationSetEventDestinations.yaml"

requestSendVoiceMessage :: SendVoiceMessage -> TestTree
requestSendVoiceMessage =
  req
    "SendVoiceMessage"
    "fixture/SendVoiceMessage.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

-- Responses

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSet)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSetEventDestination)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSet)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSetEventDestination)

responseGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinationsResponse -> TestTree
responseGetConfigurationSetEventDestinations =
  res
    "GetConfigurationSetEventDestinationsResponse"
    "fixture/GetConfigurationSetEventDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfigurationSetEventDestinations)

responseSendVoiceMessage :: SendVoiceMessageResponse -> TestTree
responseSendVoiceMessage =
  res
    "SendVoiceMessageResponse"
    "fixture/SendVoiceMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendVoiceMessage)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetEventDestination)
