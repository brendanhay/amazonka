{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisVideoWebRtcStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KinesisVideoWebRtcStorage where

import Amazonka.KinesisVideoWebRtcStorage
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KinesisVideoWebRtcStorage.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestJoinStorageSession $
--             newJoinStorageSession
--
--           ]

--     , testGroup "response"
--         [ responseJoinStorageSession $
--             newJoinStorageSessionResponse
--
--           ]
--     ]

-- Requests

requestJoinStorageSession :: JoinStorageSession -> TestTree
requestJoinStorageSession =
  req
    "JoinStorageSession"
    "fixture/JoinStorageSession.yaml"

-- Responses

responseJoinStorageSession :: JoinStorageSessionResponse -> TestTree
responseJoinStorageSession =
  res
    "JoinStorageSessionResponse"
    "fixture/JoinStorageSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy JoinStorageSession)
