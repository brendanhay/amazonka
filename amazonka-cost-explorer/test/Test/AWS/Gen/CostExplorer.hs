{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostExplorer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetRightsizingRecommendation $
--             newGetRightsizingRecommendation
--
--         , requestGetAnomalySubscriptions $
--             newGetAnomalySubscriptions
--
--         , requestListCostCategoryDefinitions $
--             newListCostCategoryDefinitions
--
--         , requestGetAnomalies $
--             newGetAnomalies
--
--         , requestGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetails
--
--         , requestGetCostForecast $
--             newGetCostForecast
--
--         , requestDeleteAnomalySubscription $
--             newDeleteAnomalySubscription
--
--         , requestUpdateAnomalySubscription $
--             newUpdateAnomalySubscription
--
--         , requestGetCostAndUsage $
--             newGetCostAndUsage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendation
--
--         , requestGetUsageForecast $
--             newGetUsageForecast
--
--         , requestGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResources
--
--         , requestGetReservationCoverage $
--             newGetReservationCoverage
--
--         , requestGetTags $
--             newGetTags
--
--         , requestUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinition
--
--         , requestDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinition
--
--         , requestGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverage
--
--         , requestGetReservationUtilization $
--             newGetReservationUtilization
--
--         , requestDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitor
--
--         , requestUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitor
--
--         , requestGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendation
--
--         , requestCreateAnomalyMonitor $
--             newCreateAnomalyMonitor
--
--         , requestCreateAnomalySubscription $
--             newCreateAnomalySubscription
--
--         , requestGetDimensionValues $
--             newGetDimensionValues
--
--         , requestGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilization
--
--         , requestDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinition
--
--         , requestProvideAnomalyFeedback $
--             newProvideAnomalyFeedback
--
--         , requestGetCostCategories $
--             newGetCostCategories
--
--         , requestGetAnomalyMonitors $
--             newGetAnomalyMonitors
--
--         , requestCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinition
--
--           ]

--     , testGroup "response"
--         [ responseGetRightsizingRecommendation $
--             newGetRightsizingRecommendationResponse
--
--         , responseGetAnomalySubscriptions $
--             newGetAnomalySubscriptionsResponse
--
--         , responseListCostCategoryDefinitions $
--             newListCostCategoryDefinitionsResponse
--
--         , responseGetAnomalies $
--             newGetAnomaliesResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetailsResponse
--
--         , responseGetCostForecast $
--             newGetCostForecastResponse
--
--         , responseDeleteAnomalySubscription $
--             newDeleteAnomalySubscriptionResponse
--
--         , responseUpdateAnomalySubscription $
--             newUpdateAnomalySubscriptionResponse
--
--         , responseGetCostAndUsage $
--             newGetCostAndUsageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendationResponse
--
--         , responseGetUsageForecast $
--             newGetUsageForecastResponse
--
--         , responseGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResourcesResponse
--
--         , responseGetReservationCoverage $
--             newGetReservationCoverageResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinitionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinitionResponse
--
--         , responseGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverageResponse
--
--         , responseGetReservationUtilization $
--             newGetReservationUtilizationResponse
--
--         , responseDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitorResponse
--
--         , responseUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitorResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendationResponse
--
--         , responseCreateAnomalyMonitor $
--             newCreateAnomalyMonitorResponse
--
--         , responseCreateAnomalySubscription $
--             newCreateAnomalySubscriptionResponse
--
--         , responseGetDimensionValues $
--             newGetDimensionValuesResponse
--
--         , responseGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilizationResponse
--
--         , responseDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinitionResponse
--
--         , responseProvideAnomalyFeedback $
--             newProvideAnomalyFeedbackResponse
--
--         , responseGetCostCategories $
--             newGetCostCategoriesResponse
--
--         , responseGetAnomalyMonitors $
--             newGetAnomalyMonitorsResponse
--
--         , responseCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinitionResponse
--
--           ]
--     ]

-- Requests

requestGetRightsizingRecommendation :: GetRightsizingRecommendation -> TestTree
requestGetRightsizingRecommendation =
  req
    "GetRightsizingRecommendation"
    "fixture/GetRightsizingRecommendation.yaml"

requestGetAnomalySubscriptions :: GetAnomalySubscriptions -> TestTree
requestGetAnomalySubscriptions =
  req
    "GetAnomalySubscriptions"
    "fixture/GetAnomalySubscriptions.yaml"

requestListCostCategoryDefinitions :: ListCostCategoryDefinitions -> TestTree
requestListCostCategoryDefinitions =
  req
    "ListCostCategoryDefinitions"
    "fixture/ListCostCategoryDefinitions.yaml"

requestGetAnomalies :: GetAnomalies -> TestTree
requestGetAnomalies =
  req
    "GetAnomalies"
    "fixture/GetAnomalies.yaml"

requestGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetails -> TestTree
requestGetSavingsPlansUtilizationDetails =
  req
    "GetSavingsPlansUtilizationDetails"
    "fixture/GetSavingsPlansUtilizationDetails.yaml"

requestGetCostForecast :: GetCostForecast -> TestTree
requestGetCostForecast =
  req
    "GetCostForecast"
    "fixture/GetCostForecast.yaml"

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

requestGetUsageForecast :: GetUsageForecast -> TestTree
requestGetUsageForecast =
  req
    "GetUsageForecast"
    "fixture/GetUsageForecast.yaml"

requestGetCostAndUsageWithResources :: GetCostAndUsageWithResources -> TestTree
requestGetCostAndUsageWithResources =
  req
    "GetCostAndUsageWithResources"
    "fixture/GetCostAndUsageWithResources.yaml"

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage =
  req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

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

requestGetSavingsPlansCoverage :: GetSavingsPlansCoverage -> TestTree
requestGetSavingsPlansCoverage =
  req
    "GetSavingsPlansCoverage"
    "fixture/GetSavingsPlansCoverage.yaml"

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization =
  req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

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

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation =
  req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestCreateAnomalyMonitor :: CreateAnomalyMonitor -> TestTree
requestCreateAnomalyMonitor =
  req
    "CreateAnomalyMonitor"
    "fixture/CreateAnomalyMonitor.yaml"

requestCreateAnomalySubscription :: CreateAnomalySubscription -> TestTree
requestCreateAnomalySubscription =
  req
    "CreateAnomalySubscription"
    "fixture/CreateAnomalySubscription.yaml"

requestGetDimensionValues :: GetDimensionValues -> TestTree
requestGetDimensionValues =
  req
    "GetDimensionValues"
    "fixture/GetDimensionValues.yaml"

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

requestProvideAnomalyFeedback :: ProvideAnomalyFeedback -> TestTree
requestProvideAnomalyFeedback =
  req
    "ProvideAnomalyFeedback"
    "fixture/ProvideAnomalyFeedback.yaml"

requestGetCostCategories :: GetCostCategories -> TestTree
requestGetCostCategories =
  req
    "GetCostCategories"
    "fixture/GetCostCategories.yaml"

requestGetAnomalyMonitors :: GetAnomalyMonitors -> TestTree
requestGetAnomalyMonitors =
  req
    "GetAnomalyMonitors"
    "fixture/GetAnomalyMonitors.yaml"

requestCreateCostCategoryDefinition :: CreateCostCategoryDefinition -> TestTree
requestCreateCostCategoryDefinition =
  req
    "CreateCostCategoryDefinition"
    "fixture/CreateCostCategoryDefinition.yaml"

-- Responses

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation =
  res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    defaultService
    (Proxy :: Proxy GetRightsizingRecommendation)

responseGetAnomalySubscriptions :: GetAnomalySubscriptionsResponse -> TestTree
responseGetAnomalySubscriptions =
  res
    "GetAnomalySubscriptionsResponse"
    "fixture/GetAnomalySubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnomalySubscriptions)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCostCategoryDefinitions)

responseGetAnomalies :: GetAnomaliesResponse -> TestTree
responseGetAnomalies =
  res
    "GetAnomaliesResponse"
    "fixture/GetAnomaliesResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnomalies)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails =
  res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansUtilizationDetails)

responseGetCostForecast :: GetCostForecastResponse -> TestTree
responseGetCostForecast =
  res
    "GetCostForecastResponse"
    "fixture/GetCostForecastResponse.proto"
    defaultService
    (Proxy :: Proxy GetCostForecast)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalySubscription)

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAnomalySubscription)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage =
  res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetCostAndUsage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation =
  res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansPurchaseRecommendation)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast =
  res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsageForecast)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources =
  res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCostAndUsageWithResources)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationCoverage)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCostCategoryDefinition)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCostCategoryDefinition)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansCoverage)

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization =
  res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationUtilization)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalyMonitor)

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAnomalyMonitor)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAnomalyMonitor)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAnomalySubscription)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues =
  res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDimensionValues)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansUtilization)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCostCategoryDefinition)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy ProvideAnomalyFeedback)

responseGetCostCategories :: GetCostCategoriesResponse -> TestTree
responseGetCostCategories =
  res
    "GetCostCategoriesResponse"
    "fixture/GetCostCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCostCategories)

responseGetAnomalyMonitors :: GetAnomalyMonitorsResponse -> TestTree
responseGetAnomalyMonitors =
  res
    "GetAnomalyMonitorsResponse"
    "fixture/GetAnomalyMonitorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnomalyMonitors)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition =
  res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCostCategoryDefinition)
