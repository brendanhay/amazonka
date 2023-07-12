{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CostExplorer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CostExplorer where

import Amazonka.CostExplorer
import qualified Data.Proxy as Proxy
import Test.Amazonka.CostExplorer.Internal
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
--         [ requestCreateAnomalyMonitor $
--             newCreateAnomalyMonitor
--
--         , requestCreateAnomalySubscription $
--             newCreateAnomalySubscription
--
--         , requestCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinition
--
--         , requestDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitor
--
--         , requestDeleteAnomalySubscription $
--             newDeleteAnomalySubscription
--
--         , requestDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinition
--
--         , requestDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinition
--
--         , requestGetAnomalies $
--             newGetAnomalies
--
--         , requestGetAnomalyMonitors $
--             newGetAnomalyMonitors
--
--         , requestGetAnomalySubscriptions $
--             newGetAnomalySubscriptions
--
--         , requestGetCostAndUsage $
--             newGetCostAndUsage
--
--         , requestGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResources
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
--         , requestGetReservationCoverage $
--             newGetReservationCoverage
--
--         , requestGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendation
--
--         , requestGetReservationUtilization $
--             newGetReservationUtilization
--
--         , requestGetRightsizingRecommendation $
--             newGetRightsizingRecommendation
--
--         , requestGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendation
--
--         , requestGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilization
--
--         , requestGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetails
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetUsageForecast $
--             newGetUsageForecast
--
--         , requestListCostAllocationTags $
--             newListCostAllocationTags
--
--         , requestListCostCategoryDefinitions $
--             newListCostCategoryDefinitions
--
--         , requestListSavingsPlansPurchaseRecommendationGeneration $
--             newListSavingsPlansPurchaseRecommendationGeneration
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestProvideAnomalyFeedback $
--             newProvideAnomalyFeedback
--
--         , requestStartSavingsPlansPurchaseRecommendationGeneration $
--             newStartSavingsPlansPurchaseRecommendationGeneration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitor
--
--         , requestUpdateAnomalySubscription $
--             newUpdateAnomalySubscription
--
--         , requestUpdateCostAllocationTagsStatus $
--             newUpdateCostAllocationTagsStatus
--
--         , requestUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinition
--
--           ]

--     , testGroup "response"
--         [ responseCreateAnomalyMonitor $
--             newCreateAnomalyMonitorResponse
--
--         , responseCreateAnomalySubscription $
--             newCreateAnomalySubscriptionResponse
--
--         , responseCreateCostCategoryDefinition $
--             newCreateCostCategoryDefinitionResponse
--
--         , responseDeleteAnomalyMonitor $
--             newDeleteAnomalyMonitorResponse
--
--         , responseDeleteAnomalySubscription $
--             newDeleteAnomalySubscriptionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             newDeleteCostCategoryDefinitionResponse
--
--         , responseDescribeCostCategoryDefinition $
--             newDescribeCostCategoryDefinitionResponse
--
--         , responseGetAnomalies $
--             newGetAnomaliesResponse
--
--         , responseGetAnomalyMonitors $
--             newGetAnomalyMonitorsResponse
--
--         , responseGetAnomalySubscriptions $
--             newGetAnomalySubscriptionsResponse
--
--         , responseGetCostAndUsage $
--             newGetCostAndUsageResponse
--
--         , responseGetCostAndUsageWithResources $
--             newGetCostAndUsageWithResourcesResponse
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
--         , responseGetReservationCoverage $
--             newGetReservationCoverageResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             newGetReservationPurchaseRecommendationResponse
--
--         , responseGetReservationUtilization $
--             newGetReservationUtilizationResponse
--
--         , responseGetRightsizingRecommendation $
--             newGetRightsizingRecommendationResponse
--
--         , responseGetSavingsPlansCoverage $
--             newGetSavingsPlansCoverageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             newGetSavingsPlansPurchaseRecommendationResponse
--
--         , responseGetSavingsPlansUtilization $
--             newGetSavingsPlansUtilizationResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             newGetSavingsPlansUtilizationDetailsResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetUsageForecast $
--             newGetUsageForecastResponse
--
--         , responseListCostAllocationTags $
--             newListCostAllocationTagsResponse
--
--         , responseListCostCategoryDefinitions $
--             newListCostCategoryDefinitionsResponse
--
--         , responseListSavingsPlansPurchaseRecommendationGeneration $
--             newListSavingsPlansPurchaseRecommendationGenerationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseProvideAnomalyFeedback $
--             newProvideAnomalyFeedbackResponse
--
--         , responseStartSavingsPlansPurchaseRecommendationGeneration $
--             newStartSavingsPlansPurchaseRecommendationGenerationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAnomalyMonitor $
--             newUpdateAnomalyMonitorResponse
--
--         , responseUpdateAnomalySubscription $
--             newUpdateAnomalySubscriptionResponse
--
--         , responseUpdateCostAllocationTagsStatus $
--             newUpdateCostAllocationTagsStatusResponse
--
--         , responseUpdateCostCategoryDefinition $
--             newUpdateCostCategoryDefinitionResponse
--
--           ]
--     ]

-- Requests

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

requestCreateCostCategoryDefinition :: CreateCostCategoryDefinition -> TestTree
requestCreateCostCategoryDefinition =
  req
    "CreateCostCategoryDefinition"
    "fixture/CreateCostCategoryDefinition.yaml"

requestDeleteAnomalyMonitor :: DeleteAnomalyMonitor -> TestTree
requestDeleteAnomalyMonitor =
  req
    "DeleteAnomalyMonitor"
    "fixture/DeleteAnomalyMonitor.yaml"

requestDeleteAnomalySubscription :: DeleteAnomalySubscription -> TestTree
requestDeleteAnomalySubscription =
  req
    "DeleteAnomalySubscription"
    "fixture/DeleteAnomalySubscription.yaml"

requestDeleteCostCategoryDefinition :: DeleteCostCategoryDefinition -> TestTree
requestDeleteCostCategoryDefinition =
  req
    "DeleteCostCategoryDefinition"
    "fixture/DeleteCostCategoryDefinition.yaml"

requestDescribeCostCategoryDefinition :: DescribeCostCategoryDefinition -> TestTree
requestDescribeCostCategoryDefinition =
  req
    "DescribeCostCategoryDefinition"
    "fixture/DescribeCostCategoryDefinition.yaml"

requestGetAnomalies :: GetAnomalies -> TestTree
requestGetAnomalies =
  req
    "GetAnomalies"
    "fixture/GetAnomalies.yaml"

requestGetAnomalyMonitors :: GetAnomalyMonitors -> TestTree
requestGetAnomalyMonitors =
  req
    "GetAnomalyMonitors"
    "fixture/GetAnomalyMonitors.yaml"

requestGetAnomalySubscriptions :: GetAnomalySubscriptions -> TestTree
requestGetAnomalySubscriptions =
  req
    "GetAnomalySubscriptions"
    "fixture/GetAnomalySubscriptions.yaml"

requestGetCostAndUsage :: GetCostAndUsage -> TestTree
requestGetCostAndUsage =
  req
    "GetCostAndUsage"
    "fixture/GetCostAndUsage.yaml"

requestGetCostAndUsageWithResources :: GetCostAndUsageWithResources -> TestTree
requestGetCostAndUsageWithResources =
  req
    "GetCostAndUsageWithResources"
    "fixture/GetCostAndUsageWithResources.yaml"

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

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage =
  req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation =
  req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization =
  req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

requestGetRightsizingRecommendation :: GetRightsizingRecommendation -> TestTree
requestGetRightsizingRecommendation =
  req
    "GetRightsizingRecommendation"
    "fixture/GetRightsizingRecommendation.yaml"

requestGetSavingsPlansCoverage :: GetSavingsPlansCoverage -> TestTree
requestGetSavingsPlansCoverage =
  req
    "GetSavingsPlansCoverage"
    "fixture/GetSavingsPlansCoverage.yaml"

requestGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendation -> TestTree
requestGetSavingsPlansPurchaseRecommendation =
  req
    "GetSavingsPlansPurchaseRecommendation"
    "fixture/GetSavingsPlansPurchaseRecommendation.yaml"

requestGetSavingsPlansUtilization :: GetSavingsPlansUtilization -> TestTree
requestGetSavingsPlansUtilization =
  req
    "GetSavingsPlansUtilization"
    "fixture/GetSavingsPlansUtilization.yaml"

requestGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetails -> TestTree
requestGetSavingsPlansUtilizationDetails =
  req
    "GetSavingsPlansUtilizationDetails"
    "fixture/GetSavingsPlansUtilizationDetails.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetUsageForecast :: GetUsageForecast -> TestTree
requestGetUsageForecast =
  req
    "GetUsageForecast"
    "fixture/GetUsageForecast.yaml"

requestListCostAllocationTags :: ListCostAllocationTags -> TestTree
requestListCostAllocationTags =
  req
    "ListCostAllocationTags"
    "fixture/ListCostAllocationTags.yaml"

requestListCostCategoryDefinitions :: ListCostCategoryDefinitions -> TestTree
requestListCostCategoryDefinitions =
  req
    "ListCostCategoryDefinitions"
    "fixture/ListCostCategoryDefinitions.yaml"

requestListSavingsPlansPurchaseRecommendationGeneration :: ListSavingsPlansPurchaseRecommendationGeneration -> TestTree
requestListSavingsPlansPurchaseRecommendationGeneration =
  req
    "ListSavingsPlansPurchaseRecommendationGeneration"
    "fixture/ListSavingsPlansPurchaseRecommendationGeneration.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestProvideAnomalyFeedback :: ProvideAnomalyFeedback -> TestTree
requestProvideAnomalyFeedback =
  req
    "ProvideAnomalyFeedback"
    "fixture/ProvideAnomalyFeedback.yaml"

requestStartSavingsPlansPurchaseRecommendationGeneration :: StartSavingsPlansPurchaseRecommendationGeneration -> TestTree
requestStartSavingsPlansPurchaseRecommendationGeneration =
  req
    "StartSavingsPlansPurchaseRecommendationGeneration"
    "fixture/StartSavingsPlansPurchaseRecommendationGeneration.yaml"

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

requestUpdateAnomalyMonitor :: UpdateAnomalyMonitor -> TestTree
requestUpdateAnomalyMonitor =
  req
    "UpdateAnomalyMonitor"
    "fixture/UpdateAnomalyMonitor.yaml"

requestUpdateAnomalySubscription :: UpdateAnomalySubscription -> TestTree
requestUpdateAnomalySubscription =
  req
    "UpdateAnomalySubscription"
    "fixture/UpdateAnomalySubscription.yaml"

requestUpdateCostAllocationTagsStatus :: UpdateCostAllocationTagsStatus -> TestTree
requestUpdateCostAllocationTagsStatus =
  req
    "UpdateCostAllocationTagsStatus"
    "fixture/UpdateCostAllocationTagsStatus.yaml"

requestUpdateCostCategoryDefinition :: UpdateCostCategoryDefinition -> TestTree
requestUpdateCostCategoryDefinition =
  req
    "UpdateCostCategoryDefinition"
    "fixture/UpdateCostCategoryDefinition.yaml"

-- Responses

responseCreateAnomalyMonitor :: CreateAnomalyMonitorResponse -> TestTree
responseCreateAnomalyMonitor =
  res
    "CreateAnomalyMonitorResponse"
    "fixture/CreateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalyMonitor)

responseCreateAnomalySubscription :: CreateAnomalySubscriptionResponse -> TestTree
responseCreateAnomalySubscription =
  res
    "CreateAnomalySubscriptionResponse"
    "fixture/CreateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalySubscription)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition =
  res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCostCategoryDefinition)

responseDeleteAnomalyMonitor :: DeleteAnomalyMonitorResponse -> TestTree
responseDeleteAnomalyMonitor =
  res
    "DeleteAnomalyMonitorResponse"
    "fixture/DeleteAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalyMonitor)

responseDeleteAnomalySubscription :: DeleteAnomalySubscriptionResponse -> TestTree
responseDeleteAnomalySubscription =
  res
    "DeleteAnomalySubscriptionResponse"
    "fixture/DeleteAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalySubscription)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition =
  res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCostCategoryDefinition)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition =
  res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCostCategoryDefinition)

responseGetAnomalies :: GetAnomaliesResponse -> TestTree
responseGetAnomalies =
  res
    "GetAnomaliesResponse"
    "fixture/GetAnomaliesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalies)

responseGetAnomalyMonitors :: GetAnomalyMonitorsResponse -> TestTree
responseGetAnomalyMonitors =
  res
    "GetAnomalyMonitorsResponse"
    "fixture/GetAnomalyMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalyMonitors)

responseGetAnomalySubscriptions :: GetAnomalySubscriptionsResponse -> TestTree
responseGetAnomalySubscriptions =
  res
    "GetAnomalySubscriptionsResponse"
    "fixture/GetAnomalySubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalySubscriptions)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage =
  res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostAndUsage)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources =
  res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCostAndUsageWithResources)

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

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage =
  res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationCoverage)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation =
  res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationPurchaseRecommendation)

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization =
  res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservationUtilization)

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation =
  res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRightsizingRecommendation)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage =
  res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansCoverage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation =
  res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansPurchaseRecommendation)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization =
  res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansUtilization)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails =
  res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSavingsPlansUtilizationDetails)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast =
  res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsageForecast)

responseListCostAllocationTags :: ListCostAllocationTagsResponse -> TestTree
responseListCostAllocationTags =
  res
    "ListCostAllocationTagsResponse"
    "fixture/ListCostAllocationTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCostAllocationTags)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions =
  res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCostCategoryDefinitions)

responseListSavingsPlansPurchaseRecommendationGeneration :: ListSavingsPlansPurchaseRecommendationGenerationResponse -> TestTree
responseListSavingsPlansPurchaseRecommendationGeneration =
  res
    "ListSavingsPlansPurchaseRecommendationGenerationResponse"
    "fixture/ListSavingsPlansPurchaseRecommendationGenerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSavingsPlansPurchaseRecommendationGeneration)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseProvideAnomalyFeedback :: ProvideAnomalyFeedbackResponse -> TestTree
responseProvideAnomalyFeedback =
  res
    "ProvideAnomalyFeedbackResponse"
    "fixture/ProvideAnomalyFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvideAnomalyFeedback)

responseStartSavingsPlansPurchaseRecommendationGeneration :: StartSavingsPlansPurchaseRecommendationGenerationResponse -> TestTree
responseStartSavingsPlansPurchaseRecommendationGeneration =
  res
    "StartSavingsPlansPurchaseRecommendationGenerationResponse"
    "fixture/StartSavingsPlansPurchaseRecommendationGenerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSavingsPlansPurchaseRecommendationGeneration)

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

responseUpdateAnomalyMonitor :: UpdateAnomalyMonitorResponse -> TestTree
responseUpdateAnomalyMonitor =
  res
    "UpdateAnomalyMonitorResponse"
    "fixture/UpdateAnomalyMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalyMonitor)

responseUpdateAnomalySubscription :: UpdateAnomalySubscriptionResponse -> TestTree
responseUpdateAnomalySubscription =
  res
    "UpdateAnomalySubscriptionResponse"
    "fixture/UpdateAnomalySubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalySubscription)

responseUpdateCostAllocationTagsStatus :: UpdateCostAllocationTagsStatusResponse -> TestTree
responseUpdateCostAllocationTagsStatus =
  res
    "UpdateCostAllocationTagsStatusResponse"
    "fixture/UpdateCostAllocationTagsStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCostAllocationTagsStatus)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition =
  res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCostCategoryDefinition)
