{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AlexaBusiness
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AlexaBusiness where

import Amazonka.AlexaBusiness
import qualified Data.Proxy as Proxy
import Test.Amazonka.AlexaBusiness.Internal
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
--         [ requestSendAnnouncement $
--             newSendAnnouncement
--
--           ]

--     , testGroup "response"
--         [ responseSendAnnouncement $
--             newSendAnnouncementResponse
--
--           ]
--     ]

-- Requests

requestSendAnnouncement :: SendAnnouncement -> TestTree
requestSendAnnouncement =
  req
    "SendAnnouncement"
    "fixture/SendAnnouncement.yaml"

-- Responses

responseSendAnnouncement :: SendAnnouncementResponse -> TestTree
responseSendAnnouncement =
  res
    "SendAnnouncementResponse"
    "fixture/SendAnnouncementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendAnnouncement)
