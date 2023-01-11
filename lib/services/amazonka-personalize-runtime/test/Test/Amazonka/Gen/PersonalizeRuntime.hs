{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PersonalizeRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PersonalizeRuntime where

import Amazonka.PersonalizeRuntime
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PersonalizeRuntime.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetPersonalizedRanking $
--             newGetPersonalizedRanking
--
--         , requestGetRecommendations $
--             newGetRecommendations
--
--           ]

--     , testGroup "response"
--         [ responseGetPersonalizedRanking $
--             newGetPersonalizedRankingResponse
--
--         , responseGetRecommendations $
--             newGetRecommendationsResponse
--
--           ]
--     ]

-- Requests

requestGetPersonalizedRanking :: GetPersonalizedRanking -> TestTree
requestGetPersonalizedRanking =
  req
    "GetPersonalizedRanking"
    "fixture/GetPersonalizedRanking.yaml"

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

-- Responses

responseGetPersonalizedRanking :: GetPersonalizedRankingResponse -> TestTree
responseGetPersonalizedRanking =
  res
    "GetPersonalizedRankingResponse"
    "fixture/GetPersonalizedRankingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPersonalizedRanking)

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendations)
