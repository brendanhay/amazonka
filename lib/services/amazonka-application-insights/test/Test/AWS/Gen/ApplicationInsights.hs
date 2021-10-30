{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApplicationInsights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ApplicationInsights where

import qualified Data.Proxy as Proxy
import Network.AWS.ApplicationInsights
import Test.AWS.ApplicationInsights.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDescribeComponent $
--             newDescribeComponent
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDescribeComponentConfigurationRecommendation $
--             newDescribeComponentConfigurationRecommendation
--
--         , requestDescribeProblem $
--             newDescribeProblem
--
--         , requestUpdateComponentConfiguration $
--             newUpdateComponentConfiguration
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestDescribeProblemObservations $
--             newDescribeProblemObservations
--
--         , requestDescribeObservation $
--             newDescribeObservation
--
--         , requestListLogPatternSets $
--             newListLogPatternSets
--
--         , requestDescribeComponentConfiguration $
--             newDescribeComponentConfiguration
--
--         , requestListProblems $
--             newListProblems
--
--         , requestListLogPatterns $
--             newListLogPatterns
--
--         , requestDeleteLogPattern $
--             newDeleteLogPattern
--
--         , requestUpdateLogPattern $
--             newUpdateLogPattern
--
--         , requestCreateLogPattern $
--             newCreateLogPattern
--
--         , requestListConfigurationHistory $
--             newListConfigurationHistory
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateComponent $
--             newCreateComponent
--
--         , requestListComponents $
--             newListComponents
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestUpdateComponent $
--             newUpdateComponent
--
--         , requestDescribeLogPattern $
--             newDescribeLogPattern
--
--           ]

--     , testGroup "response"
--         [ responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDescribeComponent $
--             newDescribeComponentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseDescribeComponentConfigurationRecommendation $
--             newDescribeComponentConfigurationRecommendationResponse
--
--         , responseDescribeProblem $
--             newDescribeProblemResponse
--
--         , responseUpdateComponentConfiguration $
--             newUpdateComponentConfigurationResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseDescribeProblemObservations $
--             newDescribeProblemObservationsResponse
--
--         , responseDescribeObservation $
--             newDescribeObservationResponse
--
--         , responseListLogPatternSets $
--             newListLogPatternSetsResponse
--
--         , responseDescribeComponentConfiguration $
--             newDescribeComponentConfigurationResponse
--
--         , responseListProblems $
--             newListProblemsResponse
--
--         , responseListLogPatterns $
--             newListLogPatternsResponse
--
--         , responseDeleteLogPattern $
--             newDeleteLogPatternResponse
--
--         , responseUpdateLogPattern $
--             newUpdateLogPatternResponse
--
--         , responseCreateLogPattern $
--             newCreateLogPatternResponse
--
--         , responseListConfigurationHistory $
--             newListConfigurationHistoryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseUpdateComponent $
--             newUpdateComponentResponse
--
--         , responseDescribeLogPattern $
--             newDescribeLogPatternResponse
--
--           ]
--     ]

-- Requests

requestDescribeApplication :: DescribeApplication -> TestTree
requestDescribeApplication =
  req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

requestDescribeComponent :: DescribeComponent -> TestTree
requestDescribeComponent =
  req
    "DescribeComponent"
    "fixture/DescribeComponent.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribeComponentConfigurationRecommendation :: DescribeComponentConfigurationRecommendation -> TestTree
requestDescribeComponentConfigurationRecommendation =
  req
    "DescribeComponentConfigurationRecommendation"
    "fixture/DescribeComponentConfigurationRecommendation.yaml"

requestDescribeProblem :: DescribeProblem -> TestTree
requestDescribeProblem =
  req
    "DescribeProblem"
    "fixture/DescribeProblem.yaml"

requestUpdateComponentConfiguration :: UpdateComponentConfiguration -> TestTree
requestUpdateComponentConfiguration =
  req
    "UpdateComponentConfiguration"
    "fixture/UpdateComponentConfiguration.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestDescribeProblemObservations :: DescribeProblemObservations -> TestTree
requestDescribeProblemObservations =
  req
    "DescribeProblemObservations"
    "fixture/DescribeProblemObservations.yaml"

requestDescribeObservation :: DescribeObservation -> TestTree
requestDescribeObservation =
  req
    "DescribeObservation"
    "fixture/DescribeObservation.yaml"

requestListLogPatternSets :: ListLogPatternSets -> TestTree
requestListLogPatternSets =
  req
    "ListLogPatternSets"
    "fixture/ListLogPatternSets.yaml"

requestDescribeComponentConfiguration :: DescribeComponentConfiguration -> TestTree
requestDescribeComponentConfiguration =
  req
    "DescribeComponentConfiguration"
    "fixture/DescribeComponentConfiguration.yaml"

requestListProblems :: ListProblems -> TestTree
requestListProblems =
  req
    "ListProblems"
    "fixture/ListProblems.yaml"

requestListLogPatterns :: ListLogPatterns -> TestTree
requestListLogPatterns =
  req
    "ListLogPatterns"
    "fixture/ListLogPatterns.yaml"

requestDeleteLogPattern :: DeleteLogPattern -> TestTree
requestDeleteLogPattern =
  req
    "DeleteLogPattern"
    "fixture/DeleteLogPattern.yaml"

requestUpdateLogPattern :: UpdateLogPattern -> TestTree
requestUpdateLogPattern =
  req
    "UpdateLogPattern"
    "fixture/UpdateLogPattern.yaml"

requestCreateLogPattern :: CreateLogPattern -> TestTree
requestCreateLogPattern =
  req
    "CreateLogPattern"
    "fixture/CreateLogPattern.yaml"

requestListConfigurationHistory :: ListConfigurationHistory -> TestTree
requestListConfigurationHistory =
  req
    "ListConfigurationHistory"
    "fixture/ListConfigurationHistory.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestUpdateComponent :: UpdateComponent -> TestTree
requestUpdateComponent =
  req
    "UpdateComponent"
    "fixture/UpdateComponent.yaml"

requestDescribeLogPattern :: DescribeLogPattern -> TestTree
requestDescribeLogPattern =
  req
    "DescribeLogPattern"
    "fixture/DescribeLogPattern.yaml"

-- Responses

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication =
  res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplication)

responseDescribeComponent :: DescribeComponentResponse -> TestTree
responseDescribeComponent =
  res
    "DescribeComponentResponse"
    "fixture/DescribeComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponent)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseDescribeComponentConfigurationRecommendation :: DescribeComponentConfigurationRecommendationResponse -> TestTree
responseDescribeComponentConfigurationRecommendation =
  res
    "DescribeComponentConfigurationRecommendationResponse"
    "fixture/DescribeComponentConfigurationRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponentConfigurationRecommendation)

responseDescribeProblem :: DescribeProblemResponse -> TestTree
responseDescribeProblem =
  res
    "DescribeProblemResponse"
    "fixture/DescribeProblemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProblem)

responseUpdateComponentConfiguration :: UpdateComponentConfigurationResponse -> TestTree
responseUpdateComponentConfiguration =
  res
    "UpdateComponentConfigurationResponse"
    "fixture/UpdateComponentConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponentConfiguration)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseDescribeProblemObservations :: DescribeProblemObservationsResponse -> TestTree
responseDescribeProblemObservations =
  res
    "DescribeProblemObservationsResponse"
    "fixture/DescribeProblemObservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProblemObservations)

responseDescribeObservation :: DescribeObservationResponse -> TestTree
responseDescribeObservation =
  res
    "DescribeObservationResponse"
    "fixture/DescribeObservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeObservation)

responseListLogPatternSets :: ListLogPatternSetsResponse -> TestTree
responseListLogPatternSets =
  res
    "ListLogPatternSetsResponse"
    "fixture/ListLogPatternSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogPatternSets)

responseDescribeComponentConfiguration :: DescribeComponentConfigurationResponse -> TestTree
responseDescribeComponentConfiguration =
  res
    "DescribeComponentConfigurationResponse"
    "fixture/DescribeComponentConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponentConfiguration)

responseListProblems :: ListProblemsResponse -> TestTree
responseListProblems =
  res
    "ListProblemsResponse"
    "fixture/ListProblemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProblems)

responseListLogPatterns :: ListLogPatternsResponse -> TestTree
responseListLogPatterns =
  res
    "ListLogPatternsResponse"
    "fixture/ListLogPatternsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogPatterns)

responseDeleteLogPattern :: DeleteLogPatternResponse -> TestTree
responseDeleteLogPattern =
  res
    "DeleteLogPatternResponse"
    "fixture/DeleteLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogPattern)

responseUpdateLogPattern :: UpdateLogPatternResponse -> TestTree
responseUpdateLogPattern =
  res
    "UpdateLogPatternResponse"
    "fixture/UpdateLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLogPattern)

responseCreateLogPattern :: CreateLogPatternResponse -> TestTree
responseCreateLogPattern =
  res
    "CreateLogPatternResponse"
    "fixture/CreateLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogPattern)

responseListConfigurationHistory :: ListConfigurationHistoryResponse -> TestTree
responseListConfigurationHistory =
  res
    "ListConfigurationHistoryResponse"
    "fixture/ListConfigurationHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationHistory)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponent)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseUpdateComponent :: UpdateComponentResponse -> TestTree
responseUpdateComponent =
  res
    "UpdateComponentResponse"
    "fixture/UpdateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponent)

responseDescribeLogPattern :: DescribeLogPatternResponse -> TestTree
responseDescribeLogPattern =
  res
    "DescribeLogPatternResponse"
    "fixture/DescribeLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogPattern)
