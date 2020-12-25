{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostExplorer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CostExplorer where

import Data.Proxy
import Network.AWS.CostExplorer
import Test.AWS.CostExplorer.Internal
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
--         [ requestGetReservationUtilization $
--             mkGetReservationUtilization
--
--         , requestGetSavingsPlansCoverage $
--             mkGetSavingsPlansCoverage
--
--         , requestGetTags $
--             mkGetTags
--
--         , requestGetRightsizingRecommendation $
--             mkGetRightsizingRecommendation
--
--         , requestGetCostAndUsageWithResources $
--             mkGetCostAndUsageWithResources
--
--         , requestGetUsageForecast $
--             mkGetUsageForecast
--
--         , requestGetReservationCoverage $
--             mkGetReservationCoverage
--
--         , requestGetCostForecast $
--             mkGetCostForecast
--
--         , requestGetDimensionValues $
--             mkGetDimensionValues
--
--         , requestGetAnomalies $
--             mkGetAnomalies
--
--         , requestGetReservationPurchaseRecommendation $
--             mkGetReservationPurchaseRecommendation
--
--         , requestDeleteAnomalyMonitor $
--             mkDeleteAnomalyMonitor
--
--         , requestUpdateAnomalyMonitor $
--             mkUpdateAnomalyMonitor
--
--         , requestListCostCategoryDefinitions $
--             mkListCostCategoryDefinitions
--
--         , requestUpdateCostCategoryDefinition $
--             mkUpdateCostCategoryDefinition
--
--         , requestDeleteCostCategoryDefinition $
--             mkDeleteCostCategoryDefinition
--
--         , requestGetAnomalySubscriptions $
--             mkGetAnomalySubscriptions
--
--         , requestCreateCostCategoryDefinition $
--             mkCreateCostCategoryDefinition
--
--         , requestGetAnomalyMonitors $
--             mkGetAnomalyMonitors
--
--         , requestDeleteAnomalySubscription $
--             mkDeleteAnomalySubscription
--
--         , requestUpdateAnomalySubscription $
--             mkUpdateAnomalySubscription
--
--         , requestGetCostAndUsage $
--             mkGetCostAndUsage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             mkGetSavingsPlansPurchaseRecommendation
--
--         , requestProvideAnomalyFeedback $
--             mkProvideAnomalyFeedback
--
--         , requestGetSavingsPlansUtilization $
--             mkGetSavingsPlansUtilization
--
--         , requestDescribeCostCategoryDefinition $
--             mkDescribeCostCategoryDefinition
--
--         , requestCreateAnomalySubscription $
--             mkCreateAnomalySubscription
--
--         , requestCreateAnomalyMonitor $
--             mkCreateAnomalyMonitor
--
--         , requestGetSavingsPlansUtilizationDetails $
--             mkGetSavingsPlansUtilizationDetails
--
--           ]

--     , testGroup "response"
--         [ responseGetReservationUtilization $
--             mkGetReservationUtilizationResponse
--
--         , responseGetSavingsPlansCoverage $
--             mkGetSavingsPlansCoverageResponse
--
--         , responseGetTags $
--             mkGetTagsResponse
--
--         , responseGetRightsizingRecommendation $
--             mkGetRightsizingRecommendationResponse
--
--         , responseGetCostAndUsageWithResources $
--             mkGetCostAndUsageWithResourcesResponse
--
--         , responseGetUsageForecast $
--             mkGetUsageForecastResponse
--
--         , responseGetReservationCoverage $
--             mkGetReservationCoverageResponse
--
--         , responseGetCostForecast $
--             mkGetCostForecastResponse
--
--         , responseGetDimensionValues $
--             mkGetDimensionValuesResponse
--
--         , responseGetAnomalies $
--             mkGetAnomaliesResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             mkGetReservationPurchaseRecommendationResponse
--
--         , responseDeleteAnomalyMonitor $
--             mkDeleteAnomalyMonitorResponse
--
--         , responseUpdateAnomalyMonitor $
--             mkUpdateAnomalyMonitorResponse
--
--         , responseListCostCategoryDefinitions $
--             mkListCostCategoryDefinitionsResponse
--
--         , responseUpdateCostCategoryDefinition $
--             mkUpdateCostCategoryDefinitionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             mkDeleteCostCategoryDefinitionResponse
--
--         , responseGetAnomalySubscriptions $
--             mkGetAnomalySubscriptionsResponse
--
--         , responseCreateCostCategoryDefinition $
--             mkCreateCostCategoryDefinitionResponse
--
--         , responseGetAnomalyMonitors $
--             mkGetAnomalyMonitorsResponse
--
--         , responseDeleteAnomalySubscription $
--             mkDeleteAnomalySubscriptionResponse
--
--         , responseUpdateAnomalySubscription $
--             mkUpdateAnomalySubscriptionResponse
--
--         , responseGetCostAndUsage $
--             mkGetCostAndUsageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             mkGetSavingsPlansPurchaseRecommendationResponse
--
--         , responseProvideAnomalyFeedback $
--             mkProvideAnomalyFeedbackResponse
--
--         , responseGetSavingsPlansUtilization $
--             mkGetSavingsPlansUtilizationResponse
--
--         , responseDescribeCostCategoryDefinition $
--             mkDescribeCostCategoryDefinitionResponse
--
--         , responseCreateAnomalySubscription $
--             mkCreateAnomalySubscriptionResponse
--
--         , responseCreateAnomalyMonitor $
--             mkCreateAnomalyMonitorResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             mkGetSavingsPlansUtilizationDetailsResponse
--
--           ]
--     ]

-- Requests

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization =
  req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

requestGetSavingsPlansCoverage :: GetSavingsPlansCoverage -> TestTree
requestGetSavingsPlansCoverage =
  req
    "GetSavingsPlansCoverage"
    "fixture/GetSavingsPlansCoverage.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetRightsizingRecommendation :: GetRightsizingRecommendation -> TestTree
requestGetRightsizingRecommendation =
  req
    "GetRightsizingRecommendation"
    "fixture/GetRightsizingRecommendation.yaml"

requestGetCostAndUsageWithResources :: GetCostAndUsageWithResources -> TestTree
requestGetCostAndUsageWithResources =
  req
    "GetCostAndUsageWithResources"
    "fixture/GetCostAndUsageWithResources.yaml"

requestGetUsageForecast :: GetUsageForecast -> TestTree
requestGetUsageForecast =
  req
    "GetUsageForecast"
    "fixture/GetUsageForecast.yaml"

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage =
  req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

requestGetCostForecast :: GetCostForecast -> TestTree
requestGetCostForecast =
  req
    "GetCostForecast"
    "fixture/GetCostForecast.yaml"

requestGetDimensionValues :: GetDimensionValues -> TestTree
requestGetDimensionValues =
  req
    "GetDimensionValues"
    "fixture/GetDimensionValues.yaml"

requestGetAnomalies :: GetAnomalies -> TestTree
requestGetAnomalies =
  req
    "GetAnomalies"
    "fixture/GetAnomalies.yaml"

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation =
  req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestDeleteAnomalyMonitor :: DeleteAnomalyMonitor -> TestTree
requestDeleteAnomalyMonitor =
  req
    "DeleteAnomalyMonitor"
    "fixture/DeleteAnomalyMonitor.yaml"

requestUpdateAnomalyMonitor :: UpdateAnomalyMonitor -> TestTree
requestUpdateAnomalyMonitor =
  req
    "UpdateAnomalyMonitor"
    "fixture/UpdateAnomalyMonitor.yaml"

requestListCostCategoryDefinitions :: ListCostCategoryDefinitions -> TestTree
requestListCostCategoryDefinitions =
  req
    "ListCostCategoryDefinitions"
    "fixture/ListCostCategoryDefinitions.yaml"

requestUpdateCostCategoryDefinition :: UpdateCostCategoryDefinition -> TestTree
requestUpdateCostCategoryDefinition =
  req
    "UpdateCostCategoryDefinition"
    "fixture/UpdateCostCategoryDefinition.yaml"

requestDeleteCostCategoryDefinition :: DeleteCostCategoryDefinition -> TestTree
requestDeleteCostCategoryDefinition =
  req
    "DeleteCostCategoryDefinition"
    "fixture/DeleteCostCategoryDefinition.yaml"

requestGetAnomalySubscriptions :: GetAnomalySubscriptions -> TestTree
requestGetAnomalySubscriptions =
  req
    "GetAnomalySubscriptions"
    "fixture/GetAnomalySubscriptions.yaml"

requestCreateCostCategoryDefinition :: CreateCostCategoryDefinition -> TestTree
requestCreateCostCategoryDefinition =
  req
    "CreateCostCategoryDefinition"
    "fixture/CreateCostCategoryDefinition.yaml"

requestGetAnomalyMonitors :: GetAnomalyMonitors -> TestTree
requestGetAnomalyMonitors =
  req
    "GetAnomalyMonitors"
    "fixture/GetAnomalyMonitors.yaml"

requestDeleteAnomalySubscription :: DeleteAnomalySubscription -> TestTree
requestDeleteAnomalySubscription =
  req
    "DeleteAnomalySubscription"
    "fixture/DeleteAnomalySubscription.yaml"

requestUpdateAnomalySubscription :: UpdateAnomalySubscription -> TestTree
requestUpdateAnomalySubscription =
  req
    "UpdateAnomalySubscription"
    "fixture/UpdateAnomalySubscription.yaml"

requestGetCostAndUsage :: GetCostAndUsage -> TestTree
requestGetCostAndUsage =
  req
    "GetCostAndUsage"
    "fixture/GetCostAndUsage.yaml"

requestGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendation -> TestTree
requestGetSavingsPlansPurchaseRecommendation =
  req
    "GetSavingsPlansPurchaseRecommendation"
    "fixture/GetSavingsPlansPurchaseRecommendation.yaml"

requestProvideAnomalyFeedback :: ProvideAnomalyFeedback -> TestTree
requestProvideAnomalyFeedback =
  req
    "ProvideAnomalyFeedback"
    "fixture/ProvideAnomalyFeedback.yaml"

requestGetSavingsPlansUtilization :: GetSavingsPlansUtilization -> TestTree
requestGetSavingsPlansUtilization =
  req
    "GetSavingsPlansUtilization"
    "fixture/GetSavingsPlansUtilization.yaml"

requestDescribeCostCategoryDefinition :: DescribeCostCategoryDefinition -> TestTree
requestDescribeCostCategoryDefinition =
  req
    "DescribeCostCategoryDefinition"
    "fixture/DescribeCostCategoryDefinition.yaml"

requestCreateAnomalySubscription :: CreateAnomalySubscription -> TestTree
requestCreateAnomalySubscription =
  req
    "CreateAnomalySubscription"
    "fixture/CreateAnomalySubscription.yaml"

requestCreateAnomalyMonitor :: CreateAnomalyMonitor -> TestTree
requestCreateAnomalyMonitor =
  req
    "CreateAnomalyMonitor"
    "fixture/CreateAnomalyMonitor.yaml"

requestGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetails -> TestTree
requestGetSavingsPlansUtilizationDetails =
  req
    "GetSavingsPlansUtilizationDetails"
    "fixture/GetSavingsPlansUtilizationDetails.yaml"

-- Responses

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization =
  res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReservationUtilization)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSavingsPlansCoverage)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTags)

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation =
  res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRightsizingRecommendation)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources =
  res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCostAndUsageWithResources)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast =
  res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsageForecast)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReservationCoverage)

responseGetCostForecast :: GetCostForecastResponse -> TestTree
responseGetCostForecast =
  res
    "GetCostForecastResponse"
    "fixture/GetCostForecastResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCostForecast)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues =
  res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDimensionValues)

responseGetAnomalies :: GetAnomaliesResponse -> TestTree
responseGetAnomalies =
  res
    "GetAnomaliesResponse"
    "fixture/GetAnomaliesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAnomalies)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAnomalyMonitor)

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAnomalyMonitor)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCostCategoryDefinitions)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCostCategoryDefinition)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCostCategoryDefinition)

responseGetAnomalySubscriptions :: GetAnomalySubscriptionsResponse -> TestTree
responseGetAnomalySubscriptions =
  res
    "GetAnomalySubscriptionsResponse"
    "fixture/GetAnomalySubscriptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAnomalySubscriptions)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition =
  res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCostCategoryDefinition)

responseGetAnomalyMonitors :: GetAnomalyMonitorsResponse -> TestTree
responseGetAnomalyMonitors =
  res
    "GetAnomalyMonitorsResponse"
    "fixture/GetAnomalyMonitorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAnomalyMonitors)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAnomalySubscription)

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAnomalySubscription)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage =
  res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCostAndUsage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation =
  res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSavingsPlansPurchaseRecommendation)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ProvideAnomalyFeedback)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSavingsPlansUtilization)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCostCategoryDefinition)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAnomalySubscription)

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAnomalyMonitor)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails =
  res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSavingsPlansUtilizationDetails)
