{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Personalize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Personalize where

import Amazonka.Personalize
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Personalize.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateBatchInferenceJob $
--             newCreateBatchInferenceJob
--
--         , requestCreateBatchSegmentJob $
--             newCreateBatchSegmentJob
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateDatasetExportJob $
--             newCreateDatasetExportJob
--
--         , requestCreateDatasetGroup $
--             newCreateDatasetGroup
--
--         , requestCreateDatasetImportJob $
--             newCreateDatasetImportJob
--
--         , requestCreateEventTracker $
--             newCreateEventTracker
--
--         , requestCreateFilter $
--             newCreateFilter
--
--         , requestCreateMetricAttribution $
--             newCreateMetricAttribution
--
--         , requestCreateRecommender $
--             newCreateRecommender
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestCreateSolution $
--             newCreateSolution
--
--         , requestCreateSolutionVersion $
--             newCreateSolutionVersion
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteDatasetGroup $
--             newDeleteDatasetGroup
--
--         , requestDeleteEventTracker $
--             newDeleteEventTracker
--
--         , requestDeleteFilter $
--             newDeleteFilter
--
--         , requestDeleteMetricAttribution $
--             newDeleteMetricAttribution
--
--         , requestDeleteRecommender $
--             newDeleteRecommender
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestDeleteSolution $
--             newDeleteSolution
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestDescribeBatchInferenceJob $
--             newDescribeBatchInferenceJob
--
--         , requestDescribeBatchSegmentJob $
--             newDescribeBatchSegmentJob
--
--         , requestDescribeCampaign $
--             newDescribeCampaign
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeDatasetExportJob $
--             newDescribeDatasetExportJob
--
--         , requestDescribeDatasetGroup $
--             newDescribeDatasetGroup
--
--         , requestDescribeDatasetImportJob $
--             newDescribeDatasetImportJob
--
--         , requestDescribeEventTracker $
--             newDescribeEventTracker
--
--         , requestDescribeFeatureTransformation $
--             newDescribeFeatureTransformation
--
--         , requestDescribeFilter $
--             newDescribeFilter
--
--         , requestDescribeMetricAttribution $
--             newDescribeMetricAttribution
--
--         , requestDescribeRecipe $
--             newDescribeRecipe
--
--         , requestDescribeRecommender $
--             newDescribeRecommender
--
--         , requestDescribeSchema $
--             newDescribeSchema
--
--         , requestDescribeSolution $
--             newDescribeSolution
--
--         , requestDescribeSolutionVersion $
--             newDescribeSolutionVersion
--
--         , requestGetSolutionMetrics $
--             newGetSolutionMetrics
--
--         , requestListBatchInferenceJobs $
--             newListBatchInferenceJobs
--
--         , requestListBatchSegmentJobs $
--             newListBatchSegmentJobs
--
--         , requestListCampaigns $
--             newListCampaigns
--
--         , requestListDatasetExportJobs $
--             newListDatasetExportJobs
--
--         , requestListDatasetGroups $
--             newListDatasetGroups
--
--         , requestListDatasetImportJobs $
--             newListDatasetImportJobs
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListEventTrackers $
--             newListEventTrackers
--
--         , requestListFilters $
--             newListFilters
--
--         , requestListMetricAttributionMetrics $
--             newListMetricAttributionMetrics
--
--         , requestListMetricAttributions $
--             newListMetricAttributions
--
--         , requestListRecipes $
--             newListRecipes
--
--         , requestListRecommenders $
--             newListRecommenders
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestListSolutionVersions $
--             newListSolutionVersions
--
--         , requestListSolutions $
--             newListSolutions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartRecommender $
--             newStartRecommender
--
--         , requestStopRecommender $
--             newStopRecommender
--
--         , requestStopSolutionVersionCreation $
--             newStopSolutionVersionCreation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestUpdateMetricAttribution $
--             newUpdateMetricAttribution
--
--         , requestUpdateRecommender $
--             newUpdateRecommender
--
--           ]

--     , testGroup "response"
--         [ responseCreateBatchInferenceJob $
--             newCreateBatchInferenceJobResponse
--
--         , responseCreateBatchSegmentJob $
--             newCreateBatchSegmentJobResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateDatasetExportJob $
--             newCreateDatasetExportJobResponse
--
--         , responseCreateDatasetGroup $
--             newCreateDatasetGroupResponse
--
--         , responseCreateDatasetImportJob $
--             newCreateDatasetImportJobResponse
--
--         , responseCreateEventTracker $
--             newCreateEventTrackerResponse
--
--         , responseCreateFilter $
--             newCreateFilterResponse
--
--         , responseCreateMetricAttribution $
--             newCreateMetricAttributionResponse
--
--         , responseCreateRecommender $
--             newCreateRecommenderResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseCreateSolution $
--             newCreateSolutionResponse
--
--         , responseCreateSolutionVersion $
--             newCreateSolutionVersionResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteDatasetGroup $
--             newDeleteDatasetGroupResponse
--
--         , responseDeleteEventTracker $
--             newDeleteEventTrackerResponse
--
--         , responseDeleteFilter $
--             newDeleteFilterResponse
--
--         , responseDeleteMetricAttribution $
--             newDeleteMetricAttributionResponse
--
--         , responseDeleteRecommender $
--             newDeleteRecommenderResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseDeleteSolution $
--             newDeleteSolutionResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseDescribeBatchInferenceJob $
--             newDescribeBatchInferenceJobResponse
--
--         , responseDescribeBatchSegmentJob $
--             newDescribeBatchSegmentJobResponse
--
--         , responseDescribeCampaign $
--             newDescribeCampaignResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeDatasetExportJob $
--             newDescribeDatasetExportJobResponse
--
--         , responseDescribeDatasetGroup $
--             newDescribeDatasetGroupResponse
--
--         , responseDescribeDatasetImportJob $
--             newDescribeDatasetImportJobResponse
--
--         , responseDescribeEventTracker $
--             newDescribeEventTrackerResponse
--
--         , responseDescribeFeatureTransformation $
--             newDescribeFeatureTransformationResponse
--
--         , responseDescribeFilter $
--             newDescribeFilterResponse
--
--         , responseDescribeMetricAttribution $
--             newDescribeMetricAttributionResponse
--
--         , responseDescribeRecipe $
--             newDescribeRecipeResponse
--
--         , responseDescribeRecommender $
--             newDescribeRecommenderResponse
--
--         , responseDescribeSchema $
--             newDescribeSchemaResponse
--
--         , responseDescribeSolution $
--             newDescribeSolutionResponse
--
--         , responseDescribeSolutionVersion $
--             newDescribeSolutionVersionResponse
--
--         , responseGetSolutionMetrics $
--             newGetSolutionMetricsResponse
--
--         , responseListBatchInferenceJobs $
--             newListBatchInferenceJobsResponse
--
--         , responseListBatchSegmentJobs $
--             newListBatchSegmentJobsResponse
--
--         , responseListCampaigns $
--             newListCampaignsResponse
--
--         , responseListDatasetExportJobs $
--             newListDatasetExportJobsResponse
--
--         , responseListDatasetGroups $
--             newListDatasetGroupsResponse
--
--         , responseListDatasetImportJobs $
--             newListDatasetImportJobsResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListEventTrackers $
--             newListEventTrackersResponse
--
--         , responseListFilters $
--             newListFiltersResponse
--
--         , responseListMetricAttributionMetrics $
--             newListMetricAttributionMetricsResponse
--
--         , responseListMetricAttributions $
--             newListMetricAttributionsResponse
--
--         , responseListRecipes $
--             newListRecipesResponse
--
--         , responseListRecommenders $
--             newListRecommendersResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseListSolutionVersions $
--             newListSolutionVersionsResponse
--
--         , responseListSolutions $
--             newListSolutionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartRecommender $
--             newStartRecommenderResponse
--
--         , responseStopRecommender $
--             newStopRecommenderResponse
--
--         , responseStopSolutionVersionCreation $
--             newStopSolutionVersionCreationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseUpdateMetricAttribution $
--             newUpdateMetricAttributionResponse
--
--         , responseUpdateRecommender $
--             newUpdateRecommenderResponse
--
--           ]
--     ]

-- Requests

requestCreateBatchInferenceJob :: CreateBatchInferenceJob -> TestTree
requestCreateBatchInferenceJob =
  req
    "CreateBatchInferenceJob"
    "fixture/CreateBatchInferenceJob.yaml"

requestCreateBatchSegmentJob :: CreateBatchSegmentJob -> TestTree
requestCreateBatchSegmentJob =
  req
    "CreateBatchSegmentJob"
    "fixture/CreateBatchSegmentJob.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateDatasetExportJob :: CreateDatasetExportJob -> TestTree
requestCreateDatasetExportJob =
  req
    "CreateDatasetExportJob"
    "fixture/CreateDatasetExportJob.yaml"

requestCreateDatasetGroup :: CreateDatasetGroup -> TestTree
requestCreateDatasetGroup =
  req
    "CreateDatasetGroup"
    "fixture/CreateDatasetGroup.yaml"

requestCreateDatasetImportJob :: CreateDatasetImportJob -> TestTree
requestCreateDatasetImportJob =
  req
    "CreateDatasetImportJob"
    "fixture/CreateDatasetImportJob.yaml"

requestCreateEventTracker :: CreateEventTracker -> TestTree
requestCreateEventTracker =
  req
    "CreateEventTracker"
    "fixture/CreateEventTracker.yaml"

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter =
  req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestCreateMetricAttribution :: CreateMetricAttribution -> TestTree
requestCreateMetricAttribution =
  req
    "CreateMetricAttribution"
    "fixture/CreateMetricAttribution.yaml"

requestCreateRecommender :: CreateRecommender -> TestTree
requestCreateRecommender =
  req
    "CreateRecommender"
    "fixture/CreateRecommender.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestCreateSolution :: CreateSolution -> TestTree
requestCreateSolution =
  req
    "CreateSolution"
    "fixture/CreateSolution.yaml"

requestCreateSolutionVersion :: CreateSolutionVersion -> TestTree
requestCreateSolutionVersion =
  req
    "CreateSolutionVersion"
    "fixture/CreateSolutionVersion.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeleteDatasetGroup :: DeleteDatasetGroup -> TestTree
requestDeleteDatasetGroup =
  req
    "DeleteDatasetGroup"
    "fixture/DeleteDatasetGroup.yaml"

requestDeleteEventTracker :: DeleteEventTracker -> TestTree
requestDeleteEventTracker =
  req
    "DeleteEventTracker"
    "fixture/DeleteEventTracker.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter =
  req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestDeleteMetricAttribution :: DeleteMetricAttribution -> TestTree
requestDeleteMetricAttribution =
  req
    "DeleteMetricAttribution"
    "fixture/DeleteMetricAttribution.yaml"

requestDeleteRecommender :: DeleteRecommender -> TestTree
requestDeleteRecommender =
  req
    "DeleteRecommender"
    "fixture/DeleteRecommender.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDeleteSolution :: DeleteSolution -> TestTree
requestDeleteSolution =
  req
    "DeleteSolution"
    "fixture/DeleteSolution.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestDescribeBatchInferenceJob :: DescribeBatchInferenceJob -> TestTree
requestDescribeBatchInferenceJob =
  req
    "DescribeBatchInferenceJob"
    "fixture/DescribeBatchInferenceJob.yaml"

requestDescribeBatchSegmentJob :: DescribeBatchSegmentJob -> TestTree
requestDescribeBatchSegmentJob =
  req
    "DescribeBatchSegmentJob"
    "fixture/DescribeBatchSegmentJob.yaml"

requestDescribeCampaign :: DescribeCampaign -> TestTree
requestDescribeCampaign =
  req
    "DescribeCampaign"
    "fixture/DescribeCampaign.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeDatasetExportJob :: DescribeDatasetExportJob -> TestTree
requestDescribeDatasetExportJob =
  req
    "DescribeDatasetExportJob"
    "fixture/DescribeDatasetExportJob.yaml"

requestDescribeDatasetGroup :: DescribeDatasetGroup -> TestTree
requestDescribeDatasetGroup =
  req
    "DescribeDatasetGroup"
    "fixture/DescribeDatasetGroup.yaml"

requestDescribeDatasetImportJob :: DescribeDatasetImportJob -> TestTree
requestDescribeDatasetImportJob =
  req
    "DescribeDatasetImportJob"
    "fixture/DescribeDatasetImportJob.yaml"

requestDescribeEventTracker :: DescribeEventTracker -> TestTree
requestDescribeEventTracker =
  req
    "DescribeEventTracker"
    "fixture/DescribeEventTracker.yaml"

requestDescribeFeatureTransformation :: DescribeFeatureTransformation -> TestTree
requestDescribeFeatureTransformation =
  req
    "DescribeFeatureTransformation"
    "fixture/DescribeFeatureTransformation.yaml"

requestDescribeFilter :: DescribeFilter -> TestTree
requestDescribeFilter =
  req
    "DescribeFilter"
    "fixture/DescribeFilter.yaml"

requestDescribeMetricAttribution :: DescribeMetricAttribution -> TestTree
requestDescribeMetricAttribution =
  req
    "DescribeMetricAttribution"
    "fixture/DescribeMetricAttribution.yaml"

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe =
  req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

requestDescribeRecommender :: DescribeRecommender -> TestTree
requestDescribeRecommender =
  req
    "DescribeRecommender"
    "fixture/DescribeRecommender.yaml"

requestDescribeSchema :: DescribeSchema -> TestTree
requestDescribeSchema =
  req
    "DescribeSchema"
    "fixture/DescribeSchema.yaml"

requestDescribeSolution :: DescribeSolution -> TestTree
requestDescribeSolution =
  req
    "DescribeSolution"
    "fixture/DescribeSolution.yaml"

requestDescribeSolutionVersion :: DescribeSolutionVersion -> TestTree
requestDescribeSolutionVersion =
  req
    "DescribeSolutionVersion"
    "fixture/DescribeSolutionVersion.yaml"

requestGetSolutionMetrics :: GetSolutionMetrics -> TestTree
requestGetSolutionMetrics =
  req
    "GetSolutionMetrics"
    "fixture/GetSolutionMetrics.yaml"

requestListBatchInferenceJobs :: ListBatchInferenceJobs -> TestTree
requestListBatchInferenceJobs =
  req
    "ListBatchInferenceJobs"
    "fixture/ListBatchInferenceJobs.yaml"

requestListBatchSegmentJobs :: ListBatchSegmentJobs -> TestTree
requestListBatchSegmentJobs =
  req
    "ListBatchSegmentJobs"
    "fixture/ListBatchSegmentJobs.yaml"

requestListCampaigns :: ListCampaigns -> TestTree
requestListCampaigns =
  req
    "ListCampaigns"
    "fixture/ListCampaigns.yaml"

requestListDatasetExportJobs :: ListDatasetExportJobs -> TestTree
requestListDatasetExportJobs =
  req
    "ListDatasetExportJobs"
    "fixture/ListDatasetExportJobs.yaml"

requestListDatasetGroups :: ListDatasetGroups -> TestTree
requestListDatasetGroups =
  req
    "ListDatasetGroups"
    "fixture/ListDatasetGroups.yaml"

requestListDatasetImportJobs :: ListDatasetImportJobs -> TestTree
requestListDatasetImportJobs =
  req
    "ListDatasetImportJobs"
    "fixture/ListDatasetImportJobs.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListEventTrackers :: ListEventTrackers -> TestTree
requestListEventTrackers =
  req
    "ListEventTrackers"
    "fixture/ListEventTrackers.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters =
  req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestListMetricAttributionMetrics :: ListMetricAttributionMetrics -> TestTree
requestListMetricAttributionMetrics =
  req
    "ListMetricAttributionMetrics"
    "fixture/ListMetricAttributionMetrics.yaml"

requestListMetricAttributions :: ListMetricAttributions -> TestTree
requestListMetricAttributions =
  req
    "ListMetricAttributions"
    "fixture/ListMetricAttributions.yaml"

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes =
  req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

requestListRecommenders :: ListRecommenders -> TestTree
requestListRecommenders =
  req
    "ListRecommenders"
    "fixture/ListRecommenders.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestListSolutionVersions :: ListSolutionVersions -> TestTree
requestListSolutionVersions =
  req
    "ListSolutionVersions"
    "fixture/ListSolutionVersions.yaml"

requestListSolutions :: ListSolutions -> TestTree
requestListSolutions =
  req
    "ListSolutions"
    "fixture/ListSolutions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartRecommender :: StartRecommender -> TestTree
requestStartRecommender =
  req
    "StartRecommender"
    "fixture/StartRecommender.yaml"

requestStopRecommender :: StopRecommender -> TestTree
requestStopRecommender =
  req
    "StopRecommender"
    "fixture/StopRecommender.yaml"

requestStopSolutionVersionCreation :: StopSolutionVersionCreation -> TestTree
requestStopSolutionVersionCreation =
  req
    "StopSolutionVersionCreation"
    "fixture/StopSolutionVersionCreation.yaml"

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

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestUpdateMetricAttribution :: UpdateMetricAttribution -> TestTree
requestUpdateMetricAttribution =
  req
    "UpdateMetricAttribution"
    "fixture/UpdateMetricAttribution.yaml"

requestUpdateRecommender :: UpdateRecommender -> TestTree
requestUpdateRecommender =
  req
    "UpdateRecommender"
    "fixture/UpdateRecommender.yaml"

-- Responses

responseCreateBatchInferenceJob :: CreateBatchInferenceJobResponse -> TestTree
responseCreateBatchInferenceJob =
  res
    "CreateBatchInferenceJobResponse"
    "fixture/CreateBatchInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchInferenceJob)

responseCreateBatchSegmentJob :: CreateBatchSegmentJobResponse -> TestTree
responseCreateBatchSegmentJob =
  res
    "CreateBatchSegmentJobResponse"
    "fixture/CreateBatchSegmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchSegmentJob)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateDatasetExportJob :: CreateDatasetExportJobResponse -> TestTree
responseCreateDatasetExportJob =
  res
    "CreateDatasetExportJobResponse"
    "fixture/CreateDatasetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetExportJob)

responseCreateDatasetGroup :: CreateDatasetGroupResponse -> TestTree
responseCreateDatasetGroup =
  res
    "CreateDatasetGroupResponse"
    "fixture/CreateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetGroup)

responseCreateDatasetImportJob :: CreateDatasetImportJobResponse -> TestTree
responseCreateDatasetImportJob =
  res
    "CreateDatasetImportJobResponse"
    "fixture/CreateDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetImportJob)

responseCreateEventTracker :: CreateEventTrackerResponse -> TestTree
responseCreateEventTracker =
  res
    "CreateEventTrackerResponse"
    "fixture/CreateEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventTracker)

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter =
  res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFilter)

responseCreateMetricAttribution :: CreateMetricAttributionResponse -> TestTree
responseCreateMetricAttribution =
  res
    "CreateMetricAttributionResponse"
    "fixture/CreateMetricAttributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMetricAttribution)

responseCreateRecommender :: CreateRecommenderResponse -> TestTree
responseCreateRecommender =
  res
    "CreateRecommenderResponse"
    "fixture/CreateRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecommender)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseCreateSolution :: CreateSolutionResponse -> TestTree
responseCreateSolution =
  res
    "CreateSolutionResponse"
    "fixture/CreateSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolution)

responseCreateSolutionVersion :: CreateSolutionVersionResponse -> TestTree
responseCreateSolutionVersion =
  res
    "CreateSolutionVersionResponse"
    "fixture/CreateSolutionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolutionVersion)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeleteDatasetGroup :: DeleteDatasetGroupResponse -> TestTree
responseDeleteDatasetGroup =
  res
    "DeleteDatasetGroupResponse"
    "fixture/DeleteDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetGroup)

responseDeleteEventTracker :: DeleteEventTrackerResponse -> TestTree
responseDeleteEventTracker =
  res
    "DeleteEventTrackerResponse"
    "fixture/DeleteEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventTracker)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter =
  res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFilter)

responseDeleteMetricAttribution :: DeleteMetricAttributionResponse -> TestTree
responseDeleteMetricAttribution =
  res
    "DeleteMetricAttributionResponse"
    "fixture/DeleteMetricAttributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMetricAttribution)

responseDeleteRecommender :: DeleteRecommenderResponse -> TestTree
responseDeleteRecommender =
  res
    "DeleteRecommenderResponse"
    "fixture/DeleteRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecommender)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseDeleteSolution :: DeleteSolutionResponse -> TestTree
responseDeleteSolution =
  res
    "DeleteSolutionResponse"
    "fixture/DeleteSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSolution)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlgorithm)

responseDescribeBatchInferenceJob :: DescribeBatchInferenceJobResponse -> TestTree
responseDescribeBatchInferenceJob =
  res
    "DescribeBatchInferenceJobResponse"
    "fixture/DescribeBatchInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBatchInferenceJob)

responseDescribeBatchSegmentJob :: DescribeBatchSegmentJobResponse -> TestTree
responseDescribeBatchSegmentJob =
  res
    "DescribeBatchSegmentJobResponse"
    "fixture/DescribeBatchSegmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBatchSegmentJob)

responseDescribeCampaign :: DescribeCampaignResponse -> TestTree
responseDescribeCampaign =
  res
    "DescribeCampaignResponse"
    "fixture/DescribeCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCampaign)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeDatasetExportJob :: DescribeDatasetExportJobResponse -> TestTree
responseDescribeDatasetExportJob =
  res
    "DescribeDatasetExportJobResponse"
    "fixture/DescribeDatasetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetExportJob)

responseDescribeDatasetGroup :: DescribeDatasetGroupResponse -> TestTree
responseDescribeDatasetGroup =
  res
    "DescribeDatasetGroupResponse"
    "fixture/DescribeDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetGroup)

responseDescribeDatasetImportJob :: DescribeDatasetImportJobResponse -> TestTree
responseDescribeDatasetImportJob =
  res
    "DescribeDatasetImportJobResponse"
    "fixture/DescribeDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetImportJob)

responseDescribeEventTracker :: DescribeEventTrackerResponse -> TestTree
responseDescribeEventTracker =
  res
    "DescribeEventTrackerResponse"
    "fixture/DescribeEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventTracker)

responseDescribeFeatureTransformation :: DescribeFeatureTransformationResponse -> TestTree
responseDescribeFeatureTransformation =
  res
    "DescribeFeatureTransformationResponse"
    "fixture/DescribeFeatureTransformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeatureTransformation)

responseDescribeFilter :: DescribeFilterResponse -> TestTree
responseDescribeFilter =
  res
    "DescribeFilterResponse"
    "fixture/DescribeFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFilter)

responseDescribeMetricAttribution :: DescribeMetricAttributionResponse -> TestTree
responseDescribeMetricAttribution =
  res
    "DescribeMetricAttributionResponse"
    "fixture/DescribeMetricAttributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricAttribution)

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe =
  res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecipe)

responseDescribeRecommender :: DescribeRecommenderResponse -> TestTree
responseDescribeRecommender =
  res
    "DescribeRecommenderResponse"
    "fixture/DescribeRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecommender)

responseDescribeSchema :: DescribeSchemaResponse -> TestTree
responseDescribeSchema =
  res
    "DescribeSchemaResponse"
    "fixture/DescribeSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchema)

responseDescribeSolution :: DescribeSolutionResponse -> TestTree
responseDescribeSolution =
  res
    "DescribeSolutionResponse"
    "fixture/DescribeSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSolution)

responseDescribeSolutionVersion :: DescribeSolutionVersionResponse -> TestTree
responseDescribeSolutionVersion =
  res
    "DescribeSolutionVersionResponse"
    "fixture/DescribeSolutionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSolutionVersion)

responseGetSolutionMetrics :: GetSolutionMetricsResponse -> TestTree
responseGetSolutionMetrics =
  res
    "GetSolutionMetricsResponse"
    "fixture/GetSolutionMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolutionMetrics)

responseListBatchInferenceJobs :: ListBatchInferenceJobsResponse -> TestTree
responseListBatchInferenceJobs =
  res
    "ListBatchInferenceJobsResponse"
    "fixture/ListBatchInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBatchInferenceJobs)

responseListBatchSegmentJobs :: ListBatchSegmentJobsResponse -> TestTree
responseListBatchSegmentJobs =
  res
    "ListBatchSegmentJobsResponse"
    "fixture/ListBatchSegmentJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBatchSegmentJobs)

responseListCampaigns :: ListCampaignsResponse -> TestTree
responseListCampaigns =
  res
    "ListCampaignsResponse"
    "fixture/ListCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCampaigns)

responseListDatasetExportJobs :: ListDatasetExportJobsResponse -> TestTree
responseListDatasetExportJobs =
  res
    "ListDatasetExportJobsResponse"
    "fixture/ListDatasetExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetExportJobs)

responseListDatasetGroups :: ListDatasetGroupsResponse -> TestTree
responseListDatasetGroups =
  res
    "ListDatasetGroupsResponse"
    "fixture/ListDatasetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetGroups)

responseListDatasetImportJobs :: ListDatasetImportJobsResponse -> TestTree
responseListDatasetImportJobs =
  res
    "ListDatasetImportJobsResponse"
    "fixture/ListDatasetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetImportJobs)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListEventTrackers :: ListEventTrackersResponse -> TestTree
responseListEventTrackers =
  res
    "ListEventTrackersResponse"
    "fixture/ListEventTrackersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventTrackers)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters =
  res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFilters)

responseListMetricAttributionMetrics :: ListMetricAttributionMetricsResponse -> TestTree
responseListMetricAttributionMetrics =
  res
    "ListMetricAttributionMetricsResponse"
    "fixture/ListMetricAttributionMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricAttributionMetrics)

responseListMetricAttributions :: ListMetricAttributionsResponse -> TestTree
responseListMetricAttributions =
  res
    "ListMetricAttributionsResponse"
    "fixture/ListMetricAttributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricAttributions)

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes =
  res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipes)

responseListRecommenders :: ListRecommendersResponse -> TestTree
responseListRecommenders =
  res
    "ListRecommendersResponse"
    "fixture/ListRecommendersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommenders)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseListSolutionVersions :: ListSolutionVersionsResponse -> TestTree
responseListSolutionVersions =
  res
    "ListSolutionVersionsResponse"
    "fixture/ListSolutionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolutionVersions)

responseListSolutions :: ListSolutionsResponse -> TestTree
responseListSolutions =
  res
    "ListSolutionsResponse"
    "fixture/ListSolutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolutions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartRecommender :: StartRecommenderResponse -> TestTree
responseStartRecommender =
  res
    "StartRecommenderResponse"
    "fixture/StartRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRecommender)

responseStopRecommender :: StopRecommenderResponse -> TestTree
responseStopRecommender =
  res
    "StopRecommenderResponse"
    "fixture/StopRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRecommender)

responseStopSolutionVersionCreation :: StopSolutionVersionCreationResponse -> TestTree
responseStopSolutionVersionCreation =
  res
    "StopSolutionVersionCreationResponse"
    "fixture/StopSolutionVersionCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSolutionVersionCreation)

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

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)

responseUpdateMetricAttribution :: UpdateMetricAttributionResponse -> TestTree
responseUpdateMetricAttribution =
  res
    "UpdateMetricAttributionResponse"
    "fixture/UpdateMetricAttributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMetricAttribution)

responseUpdateRecommender :: UpdateRecommenderResponse -> TestTree
responseUpdateRecommender =
  res
    "UpdateRecommenderResponse"
    "fixture/UpdateRecommenderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecommender)
