{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ApplicationInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ApplicationInsights where

import Amazonka.ApplicationInsights
import qualified Data.Proxy as Proxy
import Test.Amazonka.ApplicationInsights.Internal
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
--         [ requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateComponent $
--             newCreateComponent
--
--         , requestCreateLogPattern $
--             newCreateLogPattern
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestDeleteLogPattern $
--             newDeleteLogPattern
--
--         , requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDescribeComponent $
--             newDescribeComponent
--
--         , requestDescribeComponentConfiguration $
--             newDescribeComponentConfiguration
--
--         , requestDescribeComponentConfigurationRecommendation $
--             newDescribeComponentConfigurationRecommendation
--
--         , requestDescribeLogPattern $
--             newDescribeLogPattern
--
--         , requestDescribeObservation $
--             newDescribeObservation
--
--         , requestDescribeProblem $
--             newDescribeProblem
--
--         , requestDescribeProblemObservations $
--             newDescribeProblemObservations
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListConfigurationHistory $
--             newListConfigurationHistory
--
--         , requestListLogPatternSets $
--             newListLogPatternSets
--
--         , requestListLogPatterns $
--             newListLogPatterns
--
--         , requestListProblems $
--             newListProblems
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateComponent $
--             newUpdateComponent
--
--         , requestUpdateComponentConfiguration $
--             newUpdateComponentConfiguration
--
--         , requestUpdateLogPattern $
--             newUpdateLogPattern
--
--           ]

--     , testGroup "response"
--         [ responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseCreateLogPattern $
--             newCreateLogPatternResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseDeleteLogPattern $
--             newDeleteLogPatternResponse
--
--         , responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDescribeComponent $
--             newDescribeComponentResponse
--
--         , responseDescribeComponentConfiguration $
--             newDescribeComponentConfigurationResponse
--
--         , responseDescribeComponentConfigurationRecommendation $
--             newDescribeComponentConfigurationRecommendationResponse
--
--         , responseDescribeLogPattern $
--             newDescribeLogPatternResponse
--
--         , responseDescribeObservation $
--             newDescribeObservationResponse
--
--         , responseDescribeProblem $
--             newDescribeProblemResponse
--
--         , responseDescribeProblemObservations $
--             newDescribeProblemObservationsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListConfigurationHistory $
--             newListConfigurationHistoryResponse
--
--         , responseListLogPatternSets $
--             newListLogPatternSetsResponse
--
--         , responseListLogPatterns $
--             newListLogPatternsResponse
--
--         , responseListProblems $
--             newListProblemsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseUpdateComponent $
--             newUpdateComponentResponse
--
--         , responseUpdateComponentConfiguration $
--             newUpdateComponentConfigurationResponse
--
--         , responseUpdateLogPattern $
--             newUpdateLogPatternResponse
--
--           ]
--     ]

-- Requests

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestCreateLogPattern :: CreateLogPattern -> TestTree
requestCreateLogPattern =
  req
    "CreateLogPattern"
    "fixture/CreateLogPattern.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestDeleteLogPattern :: DeleteLogPattern -> TestTree
requestDeleteLogPattern =
  req
    "DeleteLogPattern"
    "fixture/DeleteLogPattern.yaml"

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

requestDescribeComponentConfiguration :: DescribeComponentConfiguration -> TestTree
requestDescribeComponentConfiguration =
  req
    "DescribeComponentConfiguration"
    "fixture/DescribeComponentConfiguration.yaml"

requestDescribeComponentConfigurationRecommendation :: DescribeComponentConfigurationRecommendation -> TestTree
requestDescribeComponentConfigurationRecommendation =
  req
    "DescribeComponentConfigurationRecommendation"
    "fixture/DescribeComponentConfigurationRecommendation.yaml"

requestDescribeLogPattern :: DescribeLogPattern -> TestTree
requestDescribeLogPattern =
  req
    "DescribeLogPattern"
    "fixture/DescribeLogPattern.yaml"

requestDescribeObservation :: DescribeObservation -> TestTree
requestDescribeObservation =
  req
    "DescribeObservation"
    "fixture/DescribeObservation.yaml"

requestDescribeProblem :: DescribeProblem -> TestTree
requestDescribeProblem =
  req
    "DescribeProblem"
    "fixture/DescribeProblem.yaml"

requestDescribeProblemObservations :: DescribeProblemObservations -> TestTree
requestDescribeProblemObservations =
  req
    "DescribeProblemObservations"
    "fixture/DescribeProblemObservations.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListConfigurationHistory :: ListConfigurationHistory -> TestTree
requestListConfigurationHistory =
  req
    "ListConfigurationHistory"
    "fixture/ListConfigurationHistory.yaml"

requestListLogPatternSets :: ListLogPatternSets -> TestTree
requestListLogPatternSets =
  req
    "ListLogPatternSets"
    "fixture/ListLogPatternSets.yaml"

requestListLogPatterns :: ListLogPatterns -> TestTree
requestListLogPatterns =
  req
    "ListLogPatterns"
    "fixture/ListLogPatterns.yaml"

requestListProblems :: ListProblems -> TestTree
requestListProblems =
  req
    "ListProblems"
    "fixture/ListProblems.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateComponent :: UpdateComponent -> TestTree
requestUpdateComponent =
  req
    "UpdateComponent"
    "fixture/UpdateComponent.yaml"

requestUpdateComponentConfiguration :: UpdateComponentConfiguration -> TestTree
requestUpdateComponentConfiguration =
  req
    "UpdateComponentConfiguration"
    "fixture/UpdateComponentConfiguration.yaml"

requestUpdateLogPattern :: UpdateLogPattern -> TestTree
requestUpdateLogPattern =
  req
    "UpdateLogPattern"
    "fixture/UpdateLogPattern.yaml"

-- Responses

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponent)

responseCreateLogPattern :: CreateLogPatternResponse -> TestTree
responseCreateLogPattern =
  res
    "CreateLogPatternResponse"
    "fixture/CreateLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogPattern)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseDeleteLogPattern :: DeleteLogPatternResponse -> TestTree
responseDeleteLogPattern =
  res
    "DeleteLogPatternResponse"
    "fixture/DeleteLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogPattern)

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

responseDescribeComponentConfiguration :: DescribeComponentConfigurationResponse -> TestTree
responseDescribeComponentConfiguration =
  res
    "DescribeComponentConfigurationResponse"
    "fixture/DescribeComponentConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponentConfiguration)

responseDescribeComponentConfigurationRecommendation :: DescribeComponentConfigurationRecommendationResponse -> TestTree
responseDescribeComponentConfigurationRecommendation =
  res
    "DescribeComponentConfigurationRecommendationResponse"
    "fixture/DescribeComponentConfigurationRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponentConfigurationRecommendation)

responseDescribeLogPattern :: DescribeLogPatternResponse -> TestTree
responseDescribeLogPattern =
  res
    "DescribeLogPatternResponse"
    "fixture/DescribeLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogPattern)

responseDescribeObservation :: DescribeObservationResponse -> TestTree
responseDescribeObservation =
  res
    "DescribeObservationResponse"
    "fixture/DescribeObservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeObservation)

responseDescribeProblem :: DescribeProblemResponse -> TestTree
responseDescribeProblem =
  res
    "DescribeProblemResponse"
    "fixture/DescribeProblemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProblem)

responseDescribeProblemObservations :: DescribeProblemObservationsResponse -> TestTree
responseDescribeProblemObservations =
  res
    "DescribeProblemObservationsResponse"
    "fixture/DescribeProblemObservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProblemObservations)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListConfigurationHistory :: ListConfigurationHistoryResponse -> TestTree
responseListConfigurationHistory =
  res
    "ListConfigurationHistoryResponse"
    "fixture/ListConfigurationHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationHistory)

responseListLogPatternSets :: ListLogPatternSetsResponse -> TestTree
responseListLogPatternSets =
  res
    "ListLogPatternSetsResponse"
    "fixture/ListLogPatternSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogPatternSets)

responseListLogPatterns :: ListLogPatternsResponse -> TestTree
responseListLogPatterns =
  res
    "ListLogPatternsResponse"
    "fixture/ListLogPatternsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLogPatterns)

responseListProblems :: ListProblemsResponse -> TestTree
responseListProblems =
  res
    "ListProblemsResponse"
    "fixture/ListProblemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProblems)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateComponent :: UpdateComponentResponse -> TestTree
responseUpdateComponent =
  res
    "UpdateComponentResponse"
    "fixture/UpdateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponent)

responseUpdateComponentConfiguration :: UpdateComponentConfigurationResponse -> TestTree
responseUpdateComponentConfiguration =
  res
    "UpdateComponentConfigurationResponse"
    "fixture/UpdateComponentConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponentConfiguration)

responseUpdateLogPattern :: UpdateLogPatternResponse -> TestTree
responseUpdateLogPattern =
  res
    "UpdateLogPatternResponse"
    "fixture/UpdateLogPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLogPattern)
