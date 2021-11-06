{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DevOpsGuru
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DevOpsGuru where

import Amazonka.DevOpsGuru
import qualified Data.Proxy as Proxy
import Test.Amazonka.DevOpsGuru.Internal
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
--         [ requestDescribeAnomaly $
--             newDescribeAnomaly
--
--         , requestDescribeFeedback $
--             newDescribeFeedback
--
--         , requestListInsights $
--             newListInsights
--
--         , requestAddNotificationChannel $
--             newAddNotificationChannel
--
--         , requestListNotificationChannels $
--             newListNotificationChannels
--
--         , requestDescribeAccountOverview $
--             newDescribeAccountOverview
--
--         , requestDescribeResourceCollectionHealth $
--             newDescribeResourceCollectionHealth
--
--         , requestRemoveNotificationChannel $
--             newRemoveNotificationChannel
--
--         , requestListAnomaliesForInsight $
--             newListAnomaliesForInsight
--
--         , requestPutFeedback $
--             newPutFeedback
--
--         , requestSearchInsights $
--             newSearchInsights
--
--         , requestDescribeServiceIntegration $
--             newDescribeServiceIntegration
--
--         , requestUpdateServiceIntegration $
--             newUpdateServiceIntegration
--
--         , requestGetResourceCollection $
--             newGetResourceCollection
--
--         , requestListEvents $
--             newListEvents
--
--         , requestUpdateResourceCollection $
--             newUpdateResourceCollection
--
--         , requestStartCostEstimation $
--             newStartCostEstimation
--
--         , requestListRecommendations $
--             newListRecommendations
--
--         , requestDescribeAccountHealth $
--             newDescribeAccountHealth
--
--         , requestDescribeInsight $
--             newDescribeInsight
--
--         , requestGetCostEstimation $
--             newGetCostEstimation
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAnomaly $
--             newDescribeAnomalyResponse
--
--         , responseDescribeFeedback $
--             newDescribeFeedbackResponse
--
--         , responseListInsights $
--             newListInsightsResponse
--
--         , responseAddNotificationChannel $
--             newAddNotificationChannelResponse
--
--         , responseListNotificationChannels $
--             newListNotificationChannelsResponse
--
--         , responseDescribeAccountOverview $
--             newDescribeAccountOverviewResponse
--
--         , responseDescribeResourceCollectionHealth $
--             newDescribeResourceCollectionHealthResponse
--
--         , responseRemoveNotificationChannel $
--             newRemoveNotificationChannelResponse
--
--         , responseListAnomaliesForInsight $
--             newListAnomaliesForInsightResponse
--
--         , responsePutFeedback $
--             newPutFeedbackResponse
--
--         , responseSearchInsights $
--             newSearchInsightsResponse
--
--         , responseDescribeServiceIntegration $
--             newDescribeServiceIntegrationResponse
--
--         , responseUpdateServiceIntegration $
--             newUpdateServiceIntegrationResponse
--
--         , responseGetResourceCollection $
--             newGetResourceCollectionResponse
--
--         , responseListEvents $
--             newListEventsResponse
--
--         , responseUpdateResourceCollection $
--             newUpdateResourceCollectionResponse
--
--         , responseStartCostEstimation $
--             newStartCostEstimationResponse
--
--         , responseListRecommendations $
--             newListRecommendationsResponse
--
--         , responseDescribeAccountHealth $
--             newDescribeAccountHealthResponse
--
--         , responseDescribeInsight $
--             newDescribeInsightResponse
--
--         , responseGetCostEstimation $
--             newGetCostEstimationResponse
--
--           ]
--     ]

-- Requests

requestDescribeAnomaly :: DescribeAnomaly -> TestTree
requestDescribeAnomaly =
  req
    "DescribeAnomaly"
    "fixture/DescribeAnomaly.yaml"

requestDescribeFeedback :: DescribeFeedback -> TestTree
requestDescribeFeedback =
  req
    "DescribeFeedback"
    "fixture/DescribeFeedback.yaml"

requestListInsights :: ListInsights -> TestTree
requestListInsights =
  req
    "ListInsights"
    "fixture/ListInsights.yaml"

requestAddNotificationChannel :: AddNotificationChannel -> TestTree
requestAddNotificationChannel =
  req
    "AddNotificationChannel"
    "fixture/AddNotificationChannel.yaml"

requestListNotificationChannels :: ListNotificationChannels -> TestTree
requestListNotificationChannels =
  req
    "ListNotificationChannels"
    "fixture/ListNotificationChannels.yaml"

requestDescribeAccountOverview :: DescribeAccountOverview -> TestTree
requestDescribeAccountOverview =
  req
    "DescribeAccountOverview"
    "fixture/DescribeAccountOverview.yaml"

requestDescribeResourceCollectionHealth :: DescribeResourceCollectionHealth -> TestTree
requestDescribeResourceCollectionHealth =
  req
    "DescribeResourceCollectionHealth"
    "fixture/DescribeResourceCollectionHealth.yaml"

requestRemoveNotificationChannel :: RemoveNotificationChannel -> TestTree
requestRemoveNotificationChannel =
  req
    "RemoveNotificationChannel"
    "fixture/RemoveNotificationChannel.yaml"

requestListAnomaliesForInsight :: ListAnomaliesForInsight -> TestTree
requestListAnomaliesForInsight =
  req
    "ListAnomaliesForInsight"
    "fixture/ListAnomaliesForInsight.yaml"

requestPutFeedback :: PutFeedback -> TestTree
requestPutFeedback =
  req
    "PutFeedback"
    "fixture/PutFeedback.yaml"

requestSearchInsights :: SearchInsights -> TestTree
requestSearchInsights =
  req
    "SearchInsights"
    "fixture/SearchInsights.yaml"

requestDescribeServiceIntegration :: DescribeServiceIntegration -> TestTree
requestDescribeServiceIntegration =
  req
    "DescribeServiceIntegration"
    "fixture/DescribeServiceIntegration.yaml"

requestUpdateServiceIntegration :: UpdateServiceIntegration -> TestTree
requestUpdateServiceIntegration =
  req
    "UpdateServiceIntegration"
    "fixture/UpdateServiceIntegration.yaml"

requestGetResourceCollection :: GetResourceCollection -> TestTree
requestGetResourceCollection =
  req
    "GetResourceCollection"
    "fixture/GetResourceCollection.yaml"

requestListEvents :: ListEvents -> TestTree
requestListEvents =
  req
    "ListEvents"
    "fixture/ListEvents.yaml"

requestUpdateResourceCollection :: UpdateResourceCollection -> TestTree
requestUpdateResourceCollection =
  req
    "UpdateResourceCollection"
    "fixture/UpdateResourceCollection.yaml"

requestStartCostEstimation :: StartCostEstimation -> TestTree
requestStartCostEstimation =
  req
    "StartCostEstimation"
    "fixture/StartCostEstimation.yaml"

requestListRecommendations :: ListRecommendations -> TestTree
requestListRecommendations =
  req
    "ListRecommendations"
    "fixture/ListRecommendations.yaml"

requestDescribeAccountHealth :: DescribeAccountHealth -> TestTree
requestDescribeAccountHealth =
  req
    "DescribeAccountHealth"
    "fixture/DescribeAccountHealth.yaml"

requestDescribeInsight :: DescribeInsight -> TestTree
requestDescribeInsight =
  req
    "DescribeInsight"
    "fixture/DescribeInsight.yaml"

requestGetCostEstimation :: GetCostEstimation -> TestTree
requestGetCostEstimation =
  req
    "GetCostEstimation"
    "fixture/GetCostEstimation.yaml"

-- Responses

responseDescribeAnomaly :: DescribeAnomalyResponse -> TestTree
responseDescribeAnomaly =
  res
    "DescribeAnomalyResponse"
    "fixture/DescribeAnomalyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomaly)

responseDescribeFeedback :: DescribeFeedbackResponse -> TestTree
responseDescribeFeedback =
  res
    "DescribeFeedbackResponse"
    "fixture/DescribeFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeedback)

responseListInsights :: ListInsightsResponse -> TestTree
responseListInsights =
  res
    "ListInsightsResponse"
    "fixture/ListInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInsights)

responseAddNotificationChannel :: AddNotificationChannelResponse -> TestTree
responseAddNotificationChannel =
  res
    "AddNotificationChannelResponse"
    "fixture/AddNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddNotificationChannel)

responseListNotificationChannels :: ListNotificationChannelsResponse -> TestTree
responseListNotificationChannels =
  res
    "ListNotificationChannelsResponse"
    "fixture/ListNotificationChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotificationChannels)

responseDescribeAccountOverview :: DescribeAccountOverviewResponse -> TestTree
responseDescribeAccountOverview =
  res
    "DescribeAccountOverviewResponse"
    "fixture/DescribeAccountOverviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountOverview)

responseDescribeResourceCollectionHealth :: DescribeResourceCollectionHealthResponse -> TestTree
responseDescribeResourceCollectionHealth =
  res
    "DescribeResourceCollectionHealthResponse"
    "fixture/DescribeResourceCollectionHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourceCollectionHealth)

responseRemoveNotificationChannel :: RemoveNotificationChannelResponse -> TestTree
responseRemoveNotificationChannel =
  res
    "RemoveNotificationChannelResponse"
    "fixture/RemoveNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveNotificationChannel)

responseListAnomaliesForInsight :: ListAnomaliesForInsightResponse -> TestTree
responseListAnomaliesForInsight =
  res
    "ListAnomaliesForInsightResponse"
    "fixture/ListAnomaliesForInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomaliesForInsight)

responsePutFeedback :: PutFeedbackResponse -> TestTree
responsePutFeedback =
  res
    "PutFeedbackResponse"
    "fixture/PutFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFeedback)

responseSearchInsights :: SearchInsightsResponse -> TestTree
responseSearchInsights =
  res
    "SearchInsightsResponse"
    "fixture/SearchInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchInsights)

responseDescribeServiceIntegration :: DescribeServiceIntegrationResponse -> TestTree
responseDescribeServiceIntegration =
  res
    "DescribeServiceIntegrationResponse"
    "fixture/DescribeServiceIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceIntegration)

responseUpdateServiceIntegration :: UpdateServiceIntegrationResponse -> TestTree
responseUpdateServiceIntegration =
  res
    "UpdateServiceIntegrationResponse"
    "fixture/UpdateServiceIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceIntegration)

responseGetResourceCollection :: GetResourceCollectionResponse -> TestTree
responseGetResourceCollection =
  res
    "GetResourceCollectionResponse"
    "fixture/GetResourceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceCollection)

responseListEvents :: ListEventsResponse -> TestTree
responseListEvents =
  res
    "ListEventsResponse"
    "fixture/ListEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEvents)

responseUpdateResourceCollection :: UpdateResourceCollectionResponse -> TestTree
responseUpdateResourceCollection =
  res
    "UpdateResourceCollectionResponse"
    "fixture/UpdateResourceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceCollection)

responseStartCostEstimation :: StartCostEstimationResponse -> TestTree
responseStartCostEstimation =
  res
    "StartCostEstimationResponse"
    "fixture/StartCostEstimationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCostEstimation)

responseListRecommendations :: ListRecommendationsResponse -> TestTree
responseListRecommendations =
  res
    "ListRecommendationsResponse"
    "fixture/ListRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendations)

responseDescribeAccountHealth :: DescribeAccountHealthResponse -> TestTree
responseDescribeAccountHealth =
  res
    "DescribeAccountHealthResponse"
    "fixture/DescribeAccountHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountHealth)

responseDescribeInsight :: DescribeInsightResponse -> TestTree
responseDescribeInsight =
  res
    "DescribeInsightResponse"
    "fixture/DescribeInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInsight)

responseGetCostEstimation :: GetCostEstimationResponse -> TestTree
responseGetCostEstimation =
  res
    "GetCostEstimationResponse"
    "fixture/GetCostEstimationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostEstimation)
