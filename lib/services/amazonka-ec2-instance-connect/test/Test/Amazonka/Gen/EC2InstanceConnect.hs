{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EC2InstanceConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EC2InstanceConnect where

import Amazonka.EC2InstanceConnect
import qualified Data.Proxy as Proxy
import Test.Amazonka.EC2InstanceConnect.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSendSSHPublicKey $
--             newSendSSHPublicKey
--
--         , requestSendSerialConsoleSSHPublicKey $
--             newSendSerialConsoleSSHPublicKey
--
--           ]

--     , testGroup "response"
--         [ responseSendSSHPublicKey $
--             newSendSSHPublicKeyResponse
--
--         , responseSendSerialConsoleSSHPublicKey $
--             newSendSerialConsoleSSHPublicKeyResponse
--
--           ]
--     ]

-- Requests

requestSendSSHPublicKey :: SendSSHPublicKey -> TestTree
requestSendSSHPublicKey =
  req
    "SendSSHPublicKey"
    "fixture/SendSSHPublicKey.yaml"

requestSendSerialConsoleSSHPublicKey :: SendSerialConsoleSSHPublicKey -> TestTree
requestSendSerialConsoleSSHPublicKey =
  req
    "SendSerialConsoleSSHPublicKey"
    "fixture/SendSerialConsoleSSHPublicKey.yaml"

-- Responses

responseSendSSHPublicKey :: SendSSHPublicKeyResponse -> TestTree
responseSendSSHPublicKey =
  res
    "SendSSHPublicKeyResponse"
    "fixture/SendSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendSSHPublicKey)

responseSendSerialConsoleSSHPublicKey :: SendSerialConsoleSSHPublicKeyResponse -> TestTree
responseSendSerialConsoleSSHPublicKey =
  res
    "SendSerialConsoleSSHPublicKeyResponse"
    "fixture/SendSerialConsoleSSHPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendSerialConsoleSSHPublicKey)
