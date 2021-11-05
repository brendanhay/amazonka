{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisVideoSignaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KinesisVideoSignaling where

import Amazonka.KinesisVideoSignaling
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KinesisVideoSignaling.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSendAlexaOfferToMaster $
--             newSendAlexaOfferToMaster
--
--         , requestGetIceServerConfig $
--             newGetIceServerConfig
--
--           ]

--     , testGroup "response"
--         [ responseSendAlexaOfferToMaster $
--             newSendAlexaOfferToMasterResponse
--
--         , responseGetIceServerConfig $
--             newGetIceServerConfigResponse
--
--           ]
--     ]

-- Requests

requestSendAlexaOfferToMaster :: SendAlexaOfferToMaster -> TestTree
requestSendAlexaOfferToMaster =
  req
    "SendAlexaOfferToMaster"
    "fixture/SendAlexaOfferToMaster.yaml"

requestGetIceServerConfig :: GetIceServerConfig -> TestTree
requestGetIceServerConfig =
  req
    "GetIceServerConfig"
    "fixture/GetIceServerConfig.yaml"

-- Responses

responseSendAlexaOfferToMaster :: SendAlexaOfferToMasterResponse -> TestTree
responseSendAlexaOfferToMaster =
  res
    "SendAlexaOfferToMasterResponse"
    "fixture/SendAlexaOfferToMasterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendAlexaOfferToMaster)

responseGetIceServerConfig :: GetIceServerConfigResponse -> TestTree
responseGetIceServerConfig =
  res
    "GetIceServerConfigResponse"
    "fixture/GetIceServerConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIceServerConfig)
