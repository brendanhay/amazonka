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

import Amazonka.CostExplorer
import qualified Data.Proxy as Proxy
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
--             newGetReservationUtilization
--
--         , requestGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverage
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetRightsizingRecommendation $
--             newGetRightsizingRecommendation
--
--         , requestGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResources
--
--         , requestGetUsageForecast $
--             newGetUsageForecast
--
--         , requestGetReservationCoverage $
--             newGetReservationCoverage
--
--         , requestGetCostCategories $
--             newGetCostCategories
--
--         , requestGetCostForecast $
--             newGetCostForecast
--
--         , requestGetDimensionValues $
--             newGetDimensionValues
--
--         , requestGetAnomalies $
--             newGetAnomalies
--
--         , requestGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendation
--
--         , requestDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitor
--
--         , requestUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitor
--
--         , requestListCostCategoryDefinitions $
--             newListCostCategoryDefinitions
--
--         , requestUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinition
--
--         , requestDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinition
--
--         , requestGetAnomalySubscriptions $
--             newGetAnomalySubscriptions
--
--         , requestCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinition
--
--         , requestGetAnomalyMonitors $
--             newGetAnomalyMonitors
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
--         , requestProvideAnomalyFeedback $
--             newProvideAnomalyFeedback
--
--         , requestGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilization
--
--         , requestDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinition
--
--         , requestCreateAnomalySubscription $
--             newCreateAnomalySubscription
--
--         , requestCreateAnomalyMonitor $
--             newCreateAnomalyMonitor
--
--         , requestGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetails
--
--           ]

--     , testGroup "response"
--         [ responseGetReservationUtilization $
--             newGetReservationUtilizationResponse
--
--         , responseGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverageResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetRightsizingRecommendation $
--             newGetRightsizingRecommendationResponse
--
--         , responseGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResourcesResponse
--
--         , responseGetUsageForecast $
--             newGetUsageForecastResponse
--
--         , responseGetReservationCoverage $
--             newGetReservationCoverageResponse
--
--         , responseGetCostCategories $
--             newGetCostCategoriesResponse
--
--         , responseGetCostForecast $
--             newGetCostForecastResponse
--
--         , responseGetDimensionValues $
--             newGetDimensionValuesResponse
--
--         , responseGetAnomalies $
--             newGetAnomaliesResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendationResponse
--
--         , responseDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitorResponse
--
--         , responseUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitorResponse
--
--         , responseListCostCategoryDefinitions $
--             newListCostCategoryDefinitionsResponse
--
--         , responseUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinitionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinitionResponse
--
--         , responseGetAnomalySubscriptions $
--             newGetAnomalySubscriptionsResponse
--
--         , responseCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinitionResponse
--
--         , responseGetAnomalyMonitors $
--             newGetAnomalyMonitorsResponse
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
--         , responseProvideAnomalyFeedback $
--             newProvideAnomalyFeedbackResponse
--
--         , responseGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilizationResponse
--
--         , responseDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinitionResponse
--
--         , responseCreateAnomalySubscription $
--             newCreateAnomalySubscriptionResponse
--
--         , responseCreateAnomalyMonitor $
--             newCreateAnomalyMonitorResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetailsResponse
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

requestGetCostCategories :: GetCostCategories -> TestTree
requestGetCostCategories =
  req
    "GetCostCategories"
    "fixture/GetCostCategories.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationUtilization)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansCoverage)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation =
  res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRightsizingRecommendation)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources =
  res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostAndUsageWithResources)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast =
  res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageForecast)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationCoverage)

responseGetCostCategories :: GetCostCategoriesResponse -> TestTree
responseGetCostCategories =
  res
    "GetCostCategoriesResponse"
    "fixture/GetCostCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostCategories)

responseGetCostForecast :: GetCostForecastResponse -> TestTree
responseGetCostForecast =
  res
    "GetCostForecastResponse"
    "fixture/GetCostForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostForecast)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues =
  res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDimensionValues)

responseGetAnomalies :: GetAnomaliesResponse -> TestTree
responseGetAnomalies =
  res
    "GetAnomaliesResponse"
    "fixture/GetAnomaliesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalies)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationPurchaseRecommendation)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalyMonitor)

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalyMonitor)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCostCategoryDefinitions)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCostCategoryDefinition)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCostCategoryDefinition)

responseGetAnomalySubscriptions :: GetAnomalySubscriptionsResponse -> TestTree
responseGetAnomalySubscriptions =
  res
    "GetAnomalySubscriptionsResponse"
    "fixture/GetAnomalySubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalySubscriptions)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition =
  res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCostCategoryDefinition)

responseGetAnomalyMonitors :: GetAnomalyMonitorsResponse -> TestTree
responseGetAnomalyMonitors =
  res
    "GetAnomalyMonitorsResponse"
    "fixture/GetAnomalyMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalyMonitors)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalySubscription)

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalySubscription)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage =
  res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostAndUsage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation =
  res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansPurchaseRecommendation)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvideAnomalyFeedback)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansUtilization)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCostCategoryDefinition)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalySubscription)

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalyMonitor)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails =
  res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansUtilizationDetails)
