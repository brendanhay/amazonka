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
--         [ requestAddNotificationChannel $
--             newAddNotificationChannel
--
--         , requestDescribeAccountHealth $
--             newDescribeAccountHealth
--
--         , requestDescribeAccountOverview $
--             newDescribeAccountOverview
--
--         , requestDescribeAnomaly $
--             newDescribeAnomaly
--
--         , requestDescribeFeedback $
--             newDescribeFeedback
--
--         , requestDescribeInsight $
--             newDescribeInsight
--
--         , requestDescribeResourceCollectionHealth $
--             newDescribeResourceCollectionHealth
--
--         , requestDescribeServiceIntegration $
--             newDescribeServiceIntegration
--
--         , requestGetCostEstimation $
--             newGetCostEstimation
--
--         , requestGetResourceCollection $
--             newGetResourceCollection
--
--         , requestListAnomaliesForInsight $
--             newListAnomaliesForInsight
--
--         , requestListEvents $
--             newListEvents
--
--         , requestListInsights $
--             newListInsights
--
--         , requestListNotificationChannels $
--             newListNotificationChannels
--
--         , requestListRecommendations $
--             newListRecommendations
--
--         , requestPutFeedback $
--             newPutFeedback
--
--         , requestRemoveNotificationChannel $
--             newRemoveNotificationChannel
--
--         , requestSearchInsights $
--             newSearchInsights
--
--         , requestStartCostEstimation $
--             newStartCostEstimation
--
--         , requestUpdateResourceCollection $
--             newUpdateResourceCollection
--
--         , requestUpdateServiceIntegration $
--             newUpdateServiceIntegration
--
--           ]

--     , testGroup "response"
--         [ responseAddNotificationChannel $
--             newAddNotificationChannelResponse
--
--         , responseDescribeAccountHealth $
--             newDescribeAccountHealthResponse
--
--         , responseDescribeAccountOverview $
--             newDescribeAccountOverviewResponse
--
--         , responseDescribeAnomaly $
--             newDescribeAnomalyResponse
--
--         , responseDescribeFeedback $
--             newDescribeFeedbackResponse
--
--         , responseDescribeInsight $
--             newDescribeInsightResponse
--
--         , responseDescribeResourceCollectionHealth $
--             newDescribeResourceCollectionHealthResponse
--
--         , responseDescribeServiceIntegration $
--             newDescribeServiceIntegrationResponse
--
--         , responseGetCostEstimation $
--             newGetCostEstimationResponse
--
--         , responseGetResourceCollection $
--             newGetResourceCollectionResponse
--
--         , responseListAnomaliesForInsight $
--             newListAnomaliesForInsightResponse
--
--         , responseListEvents $
--             newListEventsResponse
--
--         , responseListInsights $
--             newListInsightsResponse
--
--         , responseListNotificationChannels $
--             newListNotificationChannelsResponse
--
--         , responseListRecommendations $
--             newListRecommendationsResponse
--
--         , responsePutFeedback $
--             newPutFeedbackResponse
--
--         , responseRemoveNotificationChannel $
--             newRemoveNotificationChannelResponse
--
--         , responseSearchInsights $
--             newSearchInsightsResponse
--
--         , responseStartCostEstimation $
--             newStartCostEstimationResponse
--
--         , responseUpdateResourceCollection $
--             newUpdateResourceCollectionResponse
--
--         , responseUpdateServiceIntegration $
--             newUpdateServiceIntegrationResponse
--
--           ]
--     ]

-- Requests

requestAddNotificationChannel :: AddNotificationChannel -> TestTree
requestAddNotificationChannel =
  req
    "AddNotificationChannel"
    "fixture/AddNotificationChannel.yaml"

requestDescribeAccountHealth :: DescribeAccountHealth -> TestTree
requestDescribeAccountHealth =
  req
    "DescribeAccountHealth"
    "fixture/DescribeAccountHealth.yaml"

requestDescribeAccountOverview :: DescribeAccountOverview -> TestTree
requestDescribeAccountOverview =
  req
    "DescribeAccountOverview"
    "fixture/DescribeAccountOverview.yaml"

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

requestDescribeInsight :: DescribeInsight -> TestTree
requestDescribeInsight =
  req
    "DescribeInsight"
    "fixture/DescribeInsight.yaml"

requestDescribeResourceCollectionHealth :: DescribeResourceCollectionHealth -> TestTree
requestDescribeResourceCollectionHealth =
  req
    "DescribeResourceCollectionHealth"
    "fixture/DescribeResourceCollectionHealth.yaml"

requestDescribeServiceIntegration :: DescribeServiceIntegration -> TestTree
requestDescribeServiceIntegration =
  req
    "DescribeServiceIntegration"
    "fixture/DescribeServiceIntegration.yaml"

requestGetCostEstimation :: GetCostEstimation -> TestTree
requestGetCostEstimation =
  req
    "GetCostEstimation"
    "fixture/GetCostEstimation.yaml"

requestGetResourceCollection :: GetResourceCollection -> TestTree
requestGetResourceCollection =
  req
    "GetResourceCollection"
    "fixture/GetResourceCollection.yaml"

requestListAnomaliesForInsight :: ListAnomaliesForInsight -> TestTree
requestListAnomaliesForInsight =
  req
    "ListAnomaliesForInsight"
    "fixture/ListAnomaliesForInsight.yaml"

requestListEvents :: ListEvents -> TestTree
requestListEvents =
  req
    "ListEvents"
    "fixture/ListEvents.yaml"

requestListInsights :: ListInsights -> TestTree
requestListInsights =
  req
    "ListInsights"
    "fixture/ListInsights.yaml"

requestListNotificationChannels :: ListNotificationChannels -> TestTree
requestListNotificationChannels =
  req
    "ListNotificationChannels"
    "fixture/ListNotificationChannels.yaml"

requestListRecommendations :: ListRecommendations -> TestTree
requestListRecommendations =
  req
    "ListRecommendations"
    "fixture/ListRecommendations.yaml"

requestPutFeedback :: PutFeedback -> TestTree
requestPutFeedback =
  req
    "PutFeedback"
    "fixture/PutFeedback.yaml"

requestRemoveNotificationChannel :: RemoveNotificationChannel -> TestTree
requestRemoveNotificationChannel =
  req
    "RemoveNotificationChannel"
    "fixture/RemoveNotificationChannel.yaml"

requestSearchInsights :: SearchInsights -> TestTree
requestSearchInsights =
  req
    "SearchInsights"
    "fixture/SearchInsights.yaml"

requestStartCostEstimation :: StartCostEstimation -> TestTree
requestStartCostEstimation =
  req
    "StartCostEstimation"
    "fixture/StartCostEstimation.yaml"

requestUpdateResourceCollection :: UpdateResourceCollection -> TestTree
requestUpdateResourceCollection =
  req
    "UpdateResourceCollection"
    "fixture/UpdateResourceCollection.yaml"

requestUpdateServiceIntegration :: UpdateServiceIntegration -> TestTree
requestUpdateServiceIntegration =
  req
    "UpdateServiceIntegration"
    "fixture/UpdateServiceIntegration.yaml"

-- Responses

responseAddNotificationChannel :: AddNotificationChannelResponse -> TestTree
responseAddNotificationChannel =
  res
    "AddNotificationChannelResponse"
    "fixture/AddNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddNotificationChannel)

responseDescribeAccountHealth :: DescribeAccountHealthResponse -> TestTree
responseDescribeAccountHealth =
  res
    "DescribeAccountHealthResponse"
    "fixture/DescribeAccountHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountHealth)

responseDescribeAccountOverview :: DescribeAccountOverviewResponse -> TestTree
responseDescribeAccountOverview =
  res
    "DescribeAccountOverviewResponse"
    "fixture/DescribeAccountOverviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountOverview)

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

responseDescribeInsight :: DescribeInsightResponse -> TestTree
responseDescribeInsight =
  res
    "DescribeInsightResponse"
    "fixture/DescribeInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInsight)

responseDescribeResourceCollectionHealth :: DescribeResourceCollectionHealthResponse -> TestTree
responseDescribeResourceCollectionHealth =
  res
    "DescribeResourceCollectionHealthResponse"
    "fixture/DescribeResourceCollectionHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourceCollectionHealth)

responseDescribeServiceIntegration :: DescribeServiceIntegrationResponse -> TestTree
responseDescribeServiceIntegration =
  res
    "DescribeServiceIntegrationResponse"
    "fixture/DescribeServiceIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceIntegration)

responseGetCostEstimation :: GetCostEstimationResponse -> TestTree
responseGetCostEstimation =
  res
    "GetCostEstimationResponse"
    "fixture/GetCostEstimationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostEstimation)

responseGetResourceCollection :: GetResourceCollectionResponse -> TestTree
responseGetResourceCollection =
  res
    "GetResourceCollectionResponse"
    "fixture/GetResourceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceCollection)

responseListAnomaliesForInsight :: ListAnomaliesForInsightResponse -> TestTree
responseListAnomaliesForInsight =
  res
    "ListAnomaliesForInsightResponse"
    "fixture/ListAnomaliesForInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomaliesForInsight)

responseListEvents :: ListEventsResponse -> TestTree
responseListEvents =
  res
    "ListEventsResponse"
    "fixture/ListEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEvents)

responseListInsights :: ListInsightsResponse -> TestTree
responseListInsights =
  res
    "ListInsightsResponse"
    "fixture/ListInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInsights)

responseListNotificationChannels :: ListNotificationChannelsResponse -> TestTree
responseListNotificationChannels =
  res
    "ListNotificationChannelsResponse"
    "fixture/ListNotificationChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotificationChannels)

responseListRecommendations :: ListRecommendationsResponse -> TestTree
responseListRecommendations =
  res
    "ListRecommendationsResponse"
    "fixture/ListRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendations)

responsePutFeedback :: PutFeedbackResponse -> TestTree
responsePutFeedback =
  res
    "PutFeedbackResponse"
    "fixture/PutFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFeedback)

responseRemoveNotificationChannel :: RemoveNotificationChannelResponse -> TestTree
responseRemoveNotificationChannel =
  res
    "RemoveNotificationChannelResponse"
    "fixture/RemoveNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveNotificationChannel)

responseSearchInsights :: SearchInsightsResponse -> TestTree
responseSearchInsights =
  res
    "SearchInsightsResponse"
    "fixture/SearchInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchInsights)

responseStartCostEstimation :: StartCostEstimationResponse -> TestTree
responseStartCostEstimation =
  res
    "StartCostEstimationResponse"
    "fixture/StartCostEstimationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCostEstimation)

responseUpdateResourceCollection :: UpdateResourceCollectionResponse -> TestTree
responseUpdateResourceCollection =
  res
    "UpdateResourceCollectionResponse"
    "fixture/UpdateResourceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceCollection)

responseUpdateServiceIntegration :: UpdateServiceIntegrationResponse -> TestTree
responseUpdateServiceIntegration =
  res
    "UpdateServiceIntegrationResponse"
    "fixture/UpdateServiceIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceIntegration)
