{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ConnectContactLens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ConnectContactLens where

import Amazonka.ConnectContactLens
import qualified Data.Proxy as Proxy
import Test.Amazonka.ConnectContactLens.Internal
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
--         [ requestListRealtimeContactAnalysisSegments $
--             newListRealtimeContactAnalysisSegments
--
--           ]

--     , testGroup "response"
--         [ responseListRealtimeContactAnalysisSegments $
--             newListRealtimeContactAnalysisSegmentsResponse
--
--           ]
--     ]

-- Requests

requestListRealtimeContactAnalysisSegments :: ListRealtimeContactAnalysisSegments -> TestTree
requestListRealtimeContactAnalysisSegments =
  req
    "ListRealtimeContactAnalysisSegments"
    "fixture/ListRealtimeContactAnalysisSegments.yaml"

-- Responses

responseListRealtimeContactAnalysisSegments :: ListRealtimeContactAnalysisSegmentsResponse -> TestTree
responseListRealtimeContactAnalysisSegments =
  res
    "ListRealtimeContactAnalysisSegmentsResponse"
    "fixture/ListRealtimeContactAnalysisSegmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRealtimeContactAnalysisSegments)
