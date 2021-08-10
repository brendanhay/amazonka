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
--         [ requestListCostCategoryDefinitions $
--             newListCostCategoryDefinitions
--
--         , requestGetRightsizingRecommendation $
--             newGetRightsizingRecommendation
--
--         , requestGetAnomalySubscriptions $
--             newGetAnomalySubscriptions
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
--         , requestGetCostAndUsage $
--             newGetCostAndUsage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendation
--
--         , requestUpdateAnomalySubscription $
--             newUpdateAnomalySubscription
--
--         , requestDeleteAnomalySubscription $
--             newDeleteAnomalySubscription
--
--         , requestGetReservationCoverage $
--             newGetReservationCoverage
--
--         , requestGetUsageForecast $
--             newGetUsageForecast
--
--         , requestGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResources
--
--         , requestGetTags $
--             newGetTags
--
--         , requestDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinition
--
--         , requestUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinition
--
--         , requestGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverage
--
--         , requestDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitor
--
--         , requestGetReservationUtilization $
--             newGetReservationUtilization
--
--         , requestGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendation
--
--         , requestUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitor
--
--         , requestCreateAnomalyMonitor $
--             newCreateAnomalyMonitor
--
--         , requestGetDimensionValues $
--             newGetDimensionValues
--
--         , requestCreateAnomalySubscription $
--             newCreateAnomalySubscription
--
--         , requestDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinition
--
--         , requestGetCostCategories $
--             newGetCostCategories
--
--         , requestGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilization
--
--         , requestProvideAnomalyFeedback $
--             newProvideAnomalyFeedback
--
--         , requestGetAnomalyMonitors $
--             newGetAnomalyMonitors
--
--         , requestCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinition
--
--           ]

--     , testGroup "response"
--         [ responseListCostCategoryDefinitions $
--             newListCostCategoryDefinitionsResponse
--
--         , responseGetRightsizingRecommendation $
--             newGetRightsizingRecommendationResponse
--
--         , responseGetAnomalySubscriptions $
--             newGetAnomalySubscriptionsResponse
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
--         , responseGetCostAndUsage $
--             newGetCostAndUsageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendationResponse
--
--         , responseUpdateAnomalySubscription $
--             newUpdateAnomalySubscriptionResponse
--
--         , responseDeleteAnomalySubscription $
--             newDeleteAnomalySubscriptionResponse
--
--         , responseGetReservationCoverage $
--             newGetReservationCoverageResponse
--
--         , responseGetUsageForecast $
--             newGetUsageForecastResponse
--
--         , responseGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResourcesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinitionResponse
--
--         , responseUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinitionResponse
--
--         , responseGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverageResponse
--
--         , responseDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitorResponse
--
--         , responseGetReservationUtilization $
--             newGetReservationUtilizationResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendationResponse
--
--         , responseUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitorResponse
--
--         , responseCreateAnomalyMonitor $
--             newCreateAnomalyMonitorResponse
--
--         , responseGetDimensionValues $
--             newGetDimensionValuesResponse
--
--         , responseCreateAnomalySubscription $
--             newCreateAnomalySubscriptionResponse
--
--         , responseDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinitionResponse
--
--         , responseGetCostCategories $
--             newGetCostCategoriesResponse
--
--         , responseGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilizationResponse
--
--         , responseProvideAnomalyFeedback $
--             newProvideAnomalyFeedbackResponse
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

requestListCostCategoryDefinitions :: ListCostCategoryDefinitions -> TestTree
requestListCostCategoryDefinitions =
  req
    "ListCostCategoryDefinitions"
    "fixture/ListCostCategoryDefinitions.yaml"

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

requestUpdateAnomalySubscription :: UpdateAnomalySubscription -> TestTree
requestUpdateAnomalySubscription =
  req
    "UpdateAnomalySubscription"
    "fixture/UpdateAnomalySubscription.yaml"

requestDeleteAnomalySubscription :: DeleteAnomalySubscription -> TestTree
requestDeleteAnomalySubscription =
  req
    "DeleteAnomalySubscription"
    "fixture/DeleteAnomalySubscription.yaml"

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage =
  req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

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

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestDeleteCostCategoryDefinition :: DeleteCostCategoryDefinition -> TestTree
requestDeleteCostCategoryDefinition =
  req
    "DeleteCostCategoryDefinition"
    "fixture/DeleteCostCategoryDefinition.yaml"

requestUpdateCostCategoryDefinition :: UpdateCostCategoryDefinition -> TestTree
requestUpdateCostCategoryDefinition =
  req
    "UpdateCostCategoryDefinition"
    "fixture/UpdateCostCategoryDefinition.yaml"

requestGetSavingsPlansCoverage :: GetSavingsPlansCoverage -> TestTree
requestGetSavingsPlansCoverage =
  req
    "GetSavingsPlansCoverage"
    "fixture/GetSavingsPlansCoverage.yaml"

requestDeleteAnomalyMonitor :: DeleteAnomalyMonitor -> TestTree
requestDeleteAnomalyMonitor =
  req
    "DeleteAnomalyMonitor"
    "fixture/DeleteAnomalyMonitor.yaml"

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization =
  req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation =
  req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestUpdateAnomalyMonitor :: UpdateAnomalyMonitor -> TestTree
requestUpdateAnomalyMonitor =
  req
    "UpdateAnomalyMonitor"
    "fixture/UpdateAnomalyMonitor.yaml"

requestCreateAnomalyMonitor :: CreateAnomalyMonitor -> TestTree
requestCreateAnomalyMonitor =
  req
    "CreateAnomalyMonitor"
    "fixture/CreateAnomalyMonitor.yaml"

requestGetDimensionValues :: GetDimensionValues -> TestTree
requestGetDimensionValues =
  req
    "GetDimensionValues"
    "fixture/GetDimensionValues.yaml"

requestCreateAnomalySubscription :: CreateAnomalySubscription -> TestTree
requestCreateAnomalySubscription =
  req
    "CreateAnomalySubscription"
    "fixture/CreateAnomalySubscription.yaml"

requestDescribeCostCategoryDefinition :: DescribeCostCategoryDefinition -> TestTree
requestDescribeCostCategoryDefinition =
  req
    "DescribeCostCategoryDefinition"
    "fixture/DescribeCostCategoryDefinition.yaml"

requestGetCostCategories :: GetCostCategories -> TestTree
requestGetCostCategories =
  req
    "GetCostCategories"
    "fixture/GetCostCategories.yaml"

requestGetSavingsPlansUtilization :: GetSavingsPlansUtilization -> TestTree
requestGetSavingsPlansUtilization =
  req
    "GetSavingsPlansUtilization"
    "fixture/GetSavingsPlansUtilization.yaml"

requestProvideAnomalyFeedback :: ProvideAnomalyFeedback -> TestTree
requestProvideAnomalyFeedback =
  req
    "ProvideAnomalyFeedback"
    "fixture/ProvideAnomalyFeedback.yaml"

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

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCostCategoryDefinitions)

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

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAnomalySubscription)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalySubscription)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationCoverage)

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

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCostCategoryDefinition)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCostCategoryDefinition)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansCoverage)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalyMonitor)

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization =
  res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationUtilization)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAnomalyMonitor)

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAnomalyMonitor)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues =
  res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDimensionValues)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAnomalySubscription)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCostCategoryDefinition)

responseGetCostCategories :: GetCostCategoriesResponse -> TestTree
responseGetCostCategories =
  res
    "GetCostCategoriesResponse"
    "fixture/GetCostCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCostCategories)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSavingsPlansUtilization)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy ProvideAnomalyFeedback)

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
