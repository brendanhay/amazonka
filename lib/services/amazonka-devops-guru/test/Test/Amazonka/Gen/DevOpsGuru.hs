{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DevOpsGuru
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         , requestDeleteInsight $
--             newDeleteInsight
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
--         , requestDescribeEventSourcesConfig $
--             newDescribeEventSourcesConfig
--
--         , requestDescribeFeedback $
--             newDescribeFeedback
--
--         , requestDescribeInsight $
--             newDescribeInsight
--
--         , requestDescribeOrganizationHealth $
--             newDescribeOrganizationHealth
--
--         , requestDescribeOrganizationOverview $
--             newDescribeOrganizationOverview
--
--         , requestDescribeOrganizationResourceCollectionHealth $
--             newDescribeOrganizationResourceCollectionHealth
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
--         , requestListAnomalousLogGroups $
--             newListAnomalousLogGroups
--
--         , requestListEvents $
--             newListEvents
--
--         , requestListInsights $
--             newListInsights
--
--         , requestListMonitoredResources $
--             newListMonitoredResources
--
--         , requestListNotificationChannels $
--             newListNotificationChannels
--
--         , requestListOrganizationInsights $
--             newListOrganizationInsights
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
--         , requestSearchOrganizationInsights $
--             newSearchOrganizationInsights
--
--         , requestStartCostEstimation $
--             newStartCostEstimation
--
--         , requestUpdateEventSourcesConfig $
--             newUpdateEventSourcesConfig
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
--         , responseDeleteInsight $
--             newDeleteInsightResponse
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
--         , responseDescribeEventSourcesConfig $
--             newDescribeEventSourcesConfigResponse
--
--         , responseDescribeFeedback $
--             newDescribeFeedbackResponse
--
--         , responseDescribeInsight $
--             newDescribeInsightResponse
--
--         , responseDescribeOrganizationHealth $
--             newDescribeOrganizationHealthResponse
--
--         , responseDescribeOrganizationOverview $
--             newDescribeOrganizationOverviewResponse
--
--         , responseDescribeOrganizationResourceCollectionHealth $
--             newDescribeOrganizationResourceCollectionHealthResponse
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
--         , responseListAnomalousLogGroups $
--             newListAnomalousLogGroupsResponse
--
--         , responseListEvents $
--             newListEventsResponse
--
--         , responseListInsights $
--             newListInsightsResponse
--
--         , responseListMonitoredResources $
--             newListMonitoredResourcesResponse
--
--         , responseListNotificationChannels $
--             newListNotificationChannelsResponse
--
--         , responseListOrganizationInsights $
--             newListOrganizationInsightsResponse
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
--         , responseSearchOrganizationInsights $
--             newSearchOrganizationInsightsResponse
--
--         , responseStartCostEstimation $
--             newStartCostEstimationResponse
--
--         , responseUpdateEventSourcesConfig $
--             newUpdateEventSourcesConfigResponse
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

requestDeleteInsight :: DeleteInsight -> TestTree
requestDeleteInsight =
  req
    "DeleteInsight"
    "fixture/DeleteInsight.yaml"

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

requestDescribeEventSourcesConfig :: DescribeEventSourcesConfig -> TestTree
requestDescribeEventSourcesConfig =
  req
    "DescribeEventSourcesConfig"
    "fixture/DescribeEventSourcesConfig.yaml"

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

requestDescribeOrganizationHealth :: DescribeOrganizationHealth -> TestTree
requestDescribeOrganizationHealth =
  req
    "DescribeOrganizationHealth"
    "fixture/DescribeOrganizationHealth.yaml"

requestDescribeOrganizationOverview :: DescribeOrganizationOverview -> TestTree
requestDescribeOrganizationOverview =
  req
    "DescribeOrganizationOverview"
    "fixture/DescribeOrganizationOverview.yaml"

requestDescribeOrganizationResourceCollectionHealth :: DescribeOrganizationResourceCollectionHealth -> TestTree
requestDescribeOrganizationResourceCollectionHealth =
  req
    "DescribeOrganizationResourceCollectionHealth"
    "fixture/DescribeOrganizationResourceCollectionHealth.yaml"

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

requestListAnomalousLogGroups :: ListAnomalousLogGroups -> TestTree
requestListAnomalousLogGroups =
  req
    "ListAnomalousLogGroups"
    "fixture/ListAnomalousLogGroups.yaml"

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

requestListMonitoredResources :: ListMonitoredResources -> TestTree
requestListMonitoredResources =
  req
    "ListMonitoredResources"
    "fixture/ListMonitoredResources.yaml"

requestListNotificationChannels :: ListNotificationChannels -> TestTree
requestListNotificationChannels =
  req
    "ListNotificationChannels"
    "fixture/ListNotificationChannels.yaml"

requestListOrganizationInsights :: ListOrganizationInsights -> TestTree
requestListOrganizationInsights =
  req
    "ListOrganizationInsights"
    "fixture/ListOrganizationInsights.yaml"

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

requestSearchOrganizationInsights :: SearchOrganizationInsights -> TestTree
requestSearchOrganizationInsights =
  req
    "SearchOrganizationInsights"
    "fixture/SearchOrganizationInsights.yaml"

requestStartCostEstimation :: StartCostEstimation -> TestTree
requestStartCostEstimation =
  req
    "StartCostEstimation"
    "fixture/StartCostEstimation.yaml"

requestUpdateEventSourcesConfig :: UpdateEventSourcesConfig -> TestTree
requestUpdateEventSourcesConfig =
  req
    "UpdateEventSourcesConfig"
    "fixture/UpdateEventSourcesConfig.yaml"

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

responseDeleteInsight :: DeleteInsightResponse -> TestTree
responseDeleteInsight =
  res
    "DeleteInsightResponse"
    "fixture/DeleteInsightResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInsight)

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

responseDescribeEventSourcesConfig :: DescribeEventSourcesConfigResponse -> TestTree
responseDescribeEventSourcesConfig =
  res
    "DescribeEventSourcesConfigResponse"
    "fixture/DescribeEventSourcesConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSourcesConfig)

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

responseDescribeOrganizationHealth :: DescribeOrganizationHealthResponse -> TestTree
responseDescribeOrganizationHealth =
  res
    "DescribeOrganizationHealthResponse"
    "fixture/DescribeOrganizationHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationHealth)

responseDescribeOrganizationOverview :: DescribeOrganizationOverviewResponse -> TestTree
responseDescribeOrganizationOverview =
  res
    "DescribeOrganizationOverviewResponse"
    "fixture/DescribeOrganizationOverviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationOverview)

responseDescribeOrganizationResourceCollectionHealth :: DescribeOrganizationResourceCollectionHealthResponse -> TestTree
responseDescribeOrganizationResourceCollectionHealth =
  res
    "DescribeOrganizationResourceCollectionHealthResponse"
    "fixture/DescribeOrganizationResourceCollectionHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationResourceCollectionHealth)

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

responseListAnomalousLogGroups :: ListAnomalousLogGroupsResponse -> TestTree
responseListAnomalousLogGroups =
  res
    "ListAnomalousLogGroupsResponse"
    "fixture/ListAnomalousLogGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalousLogGroups)

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

responseListMonitoredResources :: ListMonitoredResourcesResponse -> TestTree
responseListMonitoredResources =
  res
    "ListMonitoredResourcesResponse"
    "fixture/ListMonitoredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoredResources)

responseListNotificationChannels :: ListNotificationChannelsResponse -> TestTree
responseListNotificationChannels =
  res
    "ListNotificationChannelsResponse"
    "fixture/ListNotificationChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotificationChannels)

responseListOrganizationInsights :: ListOrganizationInsightsResponse -> TestTree
responseListOrganizationInsights =
  res
    "ListOrganizationInsightsResponse"
    "fixture/ListOrganizationInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationInsights)

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

responseSearchOrganizationInsights :: SearchOrganizationInsightsResponse -> TestTree
responseSearchOrganizationInsights =
  res
    "SearchOrganizationInsightsResponse"
    "fixture/SearchOrganizationInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchOrganizationInsights)

responseStartCostEstimation :: StartCostEstimationResponse -> TestTree
responseStartCostEstimation =
  res
    "StartCostEstimationResponse"
    "fixture/StartCostEstimationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCostEstimation)

responseUpdateEventSourcesConfig :: UpdateEventSourcesConfigResponse -> TestTree
responseUpdateEventSourcesConfig =
  res
    "UpdateEventSourcesConfigResponse"
    "fixture/UpdateEventSourcesConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventSourcesConfig)

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
