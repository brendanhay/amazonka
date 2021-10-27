{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.PersonalizeRuntime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.PersonalizeRuntime where

import Data.Proxy
import Network.AWS.PersonalizeRuntime
import Test.AWS.Fixture
import Test.AWS.PersonalizeRuntime.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetRecommendations $
--             newGetRecommendations
--
--         , requestGetPersonalizedRanking $
--             newGetPersonalizedRanking
--
--           ]

--     , testGroup "response"
--         [ responseGetRecommendations $
--             newGetRecommendationsResponse
--
--         , responseGetPersonalizedRanking $
--             newGetPersonalizedRankingResponse
--
--           ]
--     ]

-- Requests

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

requestGetPersonalizedRanking :: GetPersonalizedRanking -> TestTree
requestGetPersonalizedRanking =
  req
    "GetPersonalizedRanking"
    "fixture/GetPersonalizedRanking.yaml"

-- Responses

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecommendations)

responseGetPersonalizedRanking :: GetPersonalizedRankingResponse -> TestTree
responseGetPersonalizedRanking =
  res
    "GetPersonalizedRankingResponse"
    "fixture/GetPersonalizedRankingResponse.proto"
    defaultService
    (Proxy :: Proxy GetPersonalizedRanking)
