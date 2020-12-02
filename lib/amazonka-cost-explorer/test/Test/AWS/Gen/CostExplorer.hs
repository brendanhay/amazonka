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
--             getReservationUtilization
--
--         , requestGetSavingsPlansCoverage $
--             getSavingsPlansCoverage
--
--         , requestGetTags $
--             getTags
--
--         , requestGetRightsizingRecommendation $
--             getRightsizingRecommendation
--
--         , requestGetCostAndUsageWithResources $
--             getCostAndUsageWithResources
--
--         , requestGetUsageForecast $
--             getUsageForecast
--
--         , requestGetReservationCoverage $
--             getReservationCoverage
--
--         , requestGetCostForecast $
--             getCostForecast
--
--         , requestGetDimensionValues $
--             getDimensionValues
--
--         , requestGetAnomalies $
--             getAnomalies
--
--         , requestGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendation
--
--         , requestDeleteAnomalyMonitor $
--             deleteAnomalyMonitor
--
--         , requestUpdateAnomalyMonitor $
--             updateAnomalyMonitor
--
--         , requestListCostCategoryDefinitions $
--             listCostCategoryDefinitions
--
--         , requestUpdateCostCategoryDefinition $
--             updateCostCategoryDefinition
--
--         , requestDeleteCostCategoryDefinition $
--             deleteCostCategoryDefinition
--
--         , requestGetAnomalySubscriptions $
--             getAnomalySubscriptions
--
--         , requestCreateCostCategoryDefinition $
--             createCostCategoryDefinition
--
--         , requestGetAnomalyMonitors $
--             getAnomalyMonitors
--
--         , requestDeleteAnomalySubscription $
--             deleteAnomalySubscription
--
--         , requestUpdateAnomalySubscription $
--             updateAnomalySubscription
--
--         , requestGetCostAndUsage $
--             getCostAndUsage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             getSavingsPlansPurchaseRecommendation
--
--         , requestProvideAnomalyFeedback $
--             provideAnomalyFeedback
--
--         , requestGetSavingsPlansUtilization $
--             getSavingsPlansUtilization
--
--         , requestDescribeCostCategoryDefinition $
--             describeCostCategoryDefinition
--
--         , requestCreateAnomalySubscription $
--             createAnomalySubscription
--
--         , requestCreateAnomalyMonitor $
--             createAnomalyMonitor
--
--         , requestGetSavingsPlansUtilizationDetails $
--             getSavingsPlansUtilizationDetails
--
--           ]

--     , testGroup "response"
--         [ responseGetReservationUtilization $
--             getReservationUtilizationResponse
--
--         , responseGetSavingsPlansCoverage $
--             getSavingsPlansCoverageResponse
--
--         , responseGetTags $
--             getTagsResponse
--
--         , responseGetRightsizingRecommendation $
--             getRightsizingRecommendationResponse
--
--         , responseGetCostAndUsageWithResources $
--             getCostAndUsageWithResourcesResponse
--
--         , responseGetUsageForecast $
--             getUsageForecastResponse
--
--         , responseGetReservationCoverage $
--             getReservationCoverageResponse
--
--         , responseGetCostForecast $
--             getCostForecastResponse
--
--         , responseGetDimensionValues $
--             getDimensionValuesResponse
--
--         , responseGetAnomalies $
--             getAnomaliesResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendationResponse
--
--         , responseDeleteAnomalyMonitor $
--             deleteAnomalyMonitorResponse
--
--         , responseUpdateAnomalyMonitor $
--             updateAnomalyMonitorResponse
--
--         , responseListCostCategoryDefinitions $
--             listCostCategoryDefinitionsResponse
--
--         , responseUpdateCostCategoryDefinition $
--             updateCostCategoryDefinitionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             deleteCostCategoryDefinitionResponse
--
--         , responseGetAnomalySubscriptions $
--             getAnomalySubscriptionsResponse
--
--         , responseCreateCostCategoryDefinition $
--             createCostCategoryDefinitionResponse
--
--         , responseGetAnomalyMonitors $
--             getAnomalyMonitorsResponse
--
--         , responseDeleteAnomalySubscription $
--             deleteAnomalySubscriptionResponse
--
--         , responseUpdateAnomalySubscription $
--             updateAnomalySubscriptionResponse
--
--         , responseGetCostAndUsage $
--             getCostAndUsageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             getSavingsPlansPurchaseRecommendationResponse
--
--         , responseProvideAnomalyFeedback $
--             provideAnomalyFeedbackResponse
--
--         , responseGetSavingsPlansUtilization $
--             getSavingsPlansUtilizationResponse
--
--         , responseDescribeCostCategoryDefinition $
--             describeCostCategoryDefinitionResponse
--
--         , responseCreateAnomalySubscription $
--             createAnomalySubscriptionResponse
--
--         , responseCreateAnomalyMonitor $
--             createAnomalyMonitorResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             getSavingsPlansUtilizationDetailsResponse
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
    costExplorer
    (Proxy :: Proxy GetReservationUtilization)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansCoverage)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetTags)

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation =
  res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetRightsizingRecommendation)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources =
  res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostAndUsageWithResources)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast =
  res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    costExplorer
    (Proxy :: Proxy GetUsageForecast)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationCoverage)

responseGetCostForecast :: GetCostForecastResponse -> TestTree
responseGetCostForecast =
  res
    "GetCostForecastResponse"
    "fixture/GetCostForecastResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostForecast)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues =
  res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetDimensionValues)

responseGetAnomalies :: GetAnomaliesResponse -> TestTree
responseGetAnomalies =
  res
    "GetAnomaliesResponse"
    "fixture/GetAnomaliesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetAnomalies)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    costExplorer
    (Proxy :: Proxy DeleteAnomalyMonitor)

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    costExplorer
    (Proxy :: Proxy UpdateAnomalyMonitor)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    costExplorer
    (Proxy :: Proxy ListCostCategoryDefinitions)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy UpdateCostCategoryDefinition)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy DeleteCostCategoryDefinition)

responseGetAnomalySubscriptions :: GetAnomalySubscriptionsResponse -> TestTree
responseGetAnomalySubscriptions =
  res
    "GetAnomalySubscriptionsResponse"
    "fixture/GetAnomalySubscriptionsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetAnomalySubscriptions)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition =
  res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy CreateCostCategoryDefinition)

responseGetAnomalyMonitors :: GetAnomalyMonitorsResponse -> TestTree
responseGetAnomalyMonitors =
  res
    "GetAnomalyMonitorsResponse"
    "fixture/GetAnomalyMonitorsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetAnomalyMonitors)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    costExplorer
    (Proxy :: Proxy DeleteAnomalySubscription)

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    costExplorer
    (Proxy :: Proxy UpdateAnomalySubscription)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage =
  res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostAndUsage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation =
  res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansPurchaseRecommendation)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    costExplorer
    (Proxy :: Proxy ProvideAnomalyFeedback)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansUtilization)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy DescribeCostCategoryDefinition)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    costExplorer
    (Proxy :: Proxy CreateAnomalySubscription)

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    costExplorer
    (Proxy :: Proxy CreateAnomalyMonitor)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails =
  res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansUtilizationDetails)
