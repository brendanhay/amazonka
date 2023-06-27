{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudTrailData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudTrailData where

import Amazonka.CloudTrailData
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudTrailData.Internal
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
--         [ requestPutAuditEvents $
--             newPutAuditEvents
--
--           ]

--     , testGroup "response"
--         [ responsePutAuditEvents $
--             newPutAuditEventsResponse
--
--           ]
--     ]

-- Requests

requestPutAuditEvents :: PutAuditEvents -> TestTree
requestPutAuditEvents =
  req
    "PutAuditEvents"
    "fixture/PutAuditEvents.yaml"

-- Responses

responsePutAuditEvents :: PutAuditEventsResponse -> TestTree
responsePutAuditEvents =
  res
    "PutAuditEventsResponse"
    "fixture/PutAuditEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAuditEvents)
