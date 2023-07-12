{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.QLDBSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.QLDBSession where

import Amazonka.QLDBSession
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.QLDBSession.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSendCommand $
--             newSendCommand
--
--           ]

--     , testGroup "response"
--         [ responseSendCommand $
--             newSendCommandResponse
--
--           ]
--     ]

-- Requests

requestSendCommand :: SendCommand -> TestTree
requestSendCommand =
  req
    "SendCommand"
    "fixture/SendCommand.yaml"

-- Responses

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendCommand)
