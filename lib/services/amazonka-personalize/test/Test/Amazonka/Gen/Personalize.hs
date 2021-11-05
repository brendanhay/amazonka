{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Personalize
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListDatasetGroups $
--             newListDatasetGroups
--
--         , requestCreateBatchInferenceJob $
--             newCreateBatchInferenceJob
--
--         , requestCreateFilter $
--             newCreateFilter
--
--         , requestCreateDatasetImportJob $
--             newCreateDatasetImportJob
--
--         , requestDescribeSolution $
--             newDescribeSolution
--
--         , requestDescribeDatasetExportJob $
--             newDescribeDatasetExportJob
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestListCampaigns $
--             newListCampaigns
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestCreateSolutionVersion $
--             newCreateSolutionVersion
--
--         , requestStopSolutionVersionCreation $
--             newStopSolutionVersionCreation
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestDescribeFilter $
--             newDescribeFilter
--
--         , requestListEventTrackers $
--             newListEventTrackers
--
--         , requestCreateDatasetExportJob $
--             newCreateDatasetExportJob
--
--         , requestCreateSolution $
--             newCreateSolution
--
--         , requestDeleteEventTracker $
--             newDeleteEventTracker
--
--         , requestDescribeDatasetImportJob $
--             newDescribeDatasetImportJob
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestCreateEventTracker $
--             newCreateEventTracker
--
--         , requestDeleteSolution $
--             newDeleteSolution
--
--         , requestDescribeCampaign $
--             newDescribeCampaign
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestDescribeSolutionVersion $
--             newDescribeSolutionVersion
--
--         , requestDescribeEventTracker $
--             newDescribeEventTracker
--
--         , requestListDatasetImportJobs $
--             newListDatasetImportJobs
--
--         , requestDeleteFilter $
--             newDeleteFilter
--
--         , requestListBatchInferenceJobs $
--             newListBatchInferenceJobs
--
--         , requestListFilters $
--             newListFilters
--
--         , requestDeleteDatasetGroup $
--             newDeleteDatasetGroup
--
--         , requestDescribeSchema $
--             newDescribeSchema
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestListSolutionVersions $
--             newListSolutionVersions
--
--         , requestDescribeBatchInferenceJob $
--             newDescribeBatchInferenceJob
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestDescribeRecipe $
--             newDescribeRecipe
--
--         , requestListSolutions $
--             newListSolutions
--
--         , requestListDatasetExportJobs $
--             newListDatasetExportJobs
--
--         , requestDescribeDatasetGroup $
--             newDescribeDatasetGroup
--
--         , requestDescribeFeatureTransformation $
--             newDescribeFeatureTransformation
--
--         , requestGetSolutionMetrics $
--             newGetSolutionMetrics
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestCreateDatasetGroup $
--             newCreateDatasetGroup
--
--         , requestListRecipes $
--             newListRecipes
--
--           ]

--     , testGroup "response"
--         [ responseListDatasetGroups $
--             newListDatasetGroupsResponse
--
--         , responseCreateBatchInferenceJob $
--             newCreateBatchInferenceJobResponse
--
--         , responseCreateFilter $
--             newCreateFilterResponse
--
--         , responseCreateDatasetImportJob $
--             newCreateDatasetImportJobResponse
--
--         , responseDescribeSolution $
--             newDescribeSolutionResponse
--
--         , responseDescribeDatasetExportJob $
--             newDescribeDatasetExportJobResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseListCampaigns $
--             newListCampaignsResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseCreateSolutionVersion $
--             newCreateSolutionVersionResponse
--
--         , responseStopSolutionVersionCreation $
--             newStopSolutionVersionCreationResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseDescribeFilter $
--             newDescribeFilterResponse
--
--         , responseListEventTrackers $
--             newListEventTrackersResponse
--
--         , responseCreateDatasetExportJob $
--             newCreateDatasetExportJobResponse
--
--         , responseCreateSolution $
--             newCreateSolutionResponse
--
--         , responseDeleteEventTracker $
--             newDeleteEventTrackerResponse
--
--         , responseDescribeDatasetImportJob $
--             newDescribeDatasetImportJobResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseCreateEventTracker $
--             newCreateEventTrackerResponse
--
--         , responseDeleteSolution $
--             newDeleteSolutionResponse
--
--         , responseDescribeCampaign $
--             newDescribeCampaignResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseDescribeSolutionVersion $
--             newDescribeSolutionVersionResponse
--
--         , responseDescribeEventTracker $
--             newDescribeEventTrackerResponse
--
--         , responseListDatasetImportJobs $
--             newListDatasetImportJobsResponse
--
--         , responseDeleteFilter $
--             newDeleteFilterResponse
--
--         , responseListBatchInferenceJobs $
--             newListBatchInferenceJobsResponse
--
--         , responseListFilters $
--             newListFiltersResponse
--
--         , responseDeleteDatasetGroup $
--             newDeleteDatasetGroupResponse
--
--         , responseDescribeSchema $
--             newDescribeSchemaResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseListSolutionVersions $
--             newListSolutionVersionsResponse
--
--         , responseDescribeBatchInferenceJob $
--             newDescribeBatchInferenceJobResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseDescribeRecipe $
--             newDescribeRecipeResponse
--
--         , responseListSolutions $
--             newListSolutionsResponse
--
--         , responseListDatasetExportJobs $
--             newListDatasetExportJobsResponse
--
--         , responseDescribeDatasetGroup $
--             newDescribeDatasetGroupResponse
--
--         , responseDescribeFeatureTransformation $
--             newDescribeFeatureTransformationResponse
--
--         , responseGetSolutionMetrics $
--             newGetSolutionMetricsResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseCreateDatasetGroup $
--             newCreateDatasetGroupResponse
--
--         , responseListRecipes $
--             newListRecipesResponse
--
--           ]
--     ]

-- Requests

requestListDatasetGroups :: ListDatasetGroups -> TestTree
requestListDatasetGroups =
  req
    "ListDatasetGroups"
    "fixture/ListDatasetGroups.yaml"

requestCreateBatchInferenceJob :: CreateBatchInferenceJob -> TestTree
requestCreateBatchInferenceJob =
  req
    "CreateBatchInferenceJob"
    "fixture/CreateBatchInferenceJob.yaml"

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter =
  req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestCreateDatasetImportJob :: CreateDatasetImportJob -> TestTree
requestCreateDatasetImportJob =
  req
    "CreateDatasetImportJob"
    "fixture/CreateDatasetImportJob.yaml"

requestDescribeSolution :: DescribeSolution -> TestTree
requestDescribeSolution =
  req
    "DescribeSolution"
    "fixture/DescribeSolution.yaml"

requestDescribeDatasetExportJob :: DescribeDatasetExportJob -> TestTree
requestDescribeDatasetExportJob =
  req
    "DescribeDatasetExportJob"
    "fixture/DescribeDatasetExportJob.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestListCampaigns :: ListCampaigns -> TestTree
requestListCampaigns =
  req
    "ListCampaigns"
    "fixture/ListCampaigns.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestCreateSolutionVersion :: CreateSolutionVersion -> TestTree
requestCreateSolutionVersion =
  req
    "CreateSolutionVersion"
    "fixture/CreateSolutionVersion.yaml"

requestStopSolutionVersionCreation :: StopSolutionVersionCreation -> TestTree
requestStopSolutionVersionCreation =
  req
    "StopSolutionVersionCreation"
    "fixture/StopSolutionVersionCreation.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestDescribeFilter :: DescribeFilter -> TestTree
requestDescribeFilter =
  req
    "DescribeFilter"
    "fixture/DescribeFilter.yaml"

requestListEventTrackers :: ListEventTrackers -> TestTree
requestListEventTrackers =
  req
    "ListEventTrackers"
    "fixture/ListEventTrackers.yaml"

requestCreateDatasetExportJob :: CreateDatasetExportJob -> TestTree
requestCreateDatasetExportJob =
  req
    "CreateDatasetExportJob"
    "fixture/CreateDatasetExportJob.yaml"

requestCreateSolution :: CreateSolution -> TestTree
requestCreateSolution =
  req
    "CreateSolution"
    "fixture/CreateSolution.yaml"

requestDeleteEventTracker :: DeleteEventTracker -> TestTree
requestDeleteEventTracker =
  req
    "DeleteEventTracker"
    "fixture/DeleteEventTracker.yaml"

requestDescribeDatasetImportJob :: DescribeDatasetImportJob -> TestTree
requestDescribeDatasetImportJob =
  req
    "DescribeDatasetImportJob"
    "fixture/DescribeDatasetImportJob.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestCreateEventTracker :: CreateEventTracker -> TestTree
requestCreateEventTracker =
  req
    "CreateEventTracker"
    "fixture/CreateEventTracker.yaml"

requestDeleteSolution :: DeleteSolution -> TestTree
requestDeleteSolution =
  req
    "DeleteSolution"
    "fixture/DeleteSolution.yaml"

requestDescribeCampaign :: DescribeCampaign -> TestTree
requestDescribeCampaign =
  req
    "DescribeCampaign"
    "fixture/DescribeCampaign.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestDescribeSolutionVersion :: DescribeSolutionVersion -> TestTree
requestDescribeSolutionVersion =
  req
    "DescribeSolutionVersion"
    "fixture/DescribeSolutionVersion.yaml"

requestDescribeEventTracker :: DescribeEventTracker -> TestTree
requestDescribeEventTracker =
  req
    "DescribeEventTracker"
    "fixture/DescribeEventTracker.yaml"

requestListDatasetImportJobs :: ListDatasetImportJobs -> TestTree
requestListDatasetImportJobs =
  req
    "ListDatasetImportJobs"
    "fixture/ListDatasetImportJobs.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter =
  req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestListBatchInferenceJobs :: ListBatchInferenceJobs -> TestTree
requestListBatchInferenceJobs =
  req
    "ListBatchInferenceJobs"
    "fixture/ListBatchInferenceJobs.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters =
  req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestDeleteDatasetGroup :: DeleteDatasetGroup -> TestTree
requestDeleteDatasetGroup =
  req
    "DeleteDatasetGroup"
    "fixture/DeleteDatasetGroup.yaml"

requestDescribeSchema :: DescribeSchema -> TestTree
requestDescribeSchema =
  req
    "DescribeSchema"
    "fixture/DescribeSchema.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestListSolutionVersions :: ListSolutionVersions -> TestTree
requestListSolutionVersions =
  req
    "ListSolutionVersions"
    "fixture/ListSolutionVersions.yaml"

requestDescribeBatchInferenceJob :: DescribeBatchInferenceJob -> TestTree
requestDescribeBatchInferenceJob =
  req
    "DescribeBatchInferenceJob"
    "fixture/DescribeBatchInferenceJob.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe =
  req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

requestListSolutions :: ListSolutions -> TestTree
requestListSolutions =
  req
    "ListSolutions"
    "fixture/ListSolutions.yaml"

requestListDatasetExportJobs :: ListDatasetExportJobs -> TestTree
requestListDatasetExportJobs =
  req
    "ListDatasetExportJobs"
    "fixture/ListDatasetExportJobs.yaml"

requestDescribeDatasetGroup :: DescribeDatasetGroup -> TestTree
requestDescribeDatasetGroup =
  req
    "DescribeDatasetGroup"
    "fixture/DescribeDatasetGroup.yaml"

requestDescribeFeatureTransformation :: DescribeFeatureTransformation -> TestTree
requestDescribeFeatureTransformation =
  req
    "DescribeFeatureTransformation"
    "fixture/DescribeFeatureTransformation.yaml"

requestGetSolutionMetrics :: GetSolutionMetrics -> TestTree
requestGetSolutionMetrics =
  req
    "GetSolutionMetrics"
    "fixture/GetSolutionMetrics.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestCreateDatasetGroup :: CreateDatasetGroup -> TestTree
requestCreateDatasetGroup =
  req
    "CreateDatasetGroup"
    "fixture/CreateDatasetGroup.yaml"

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes =
  req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

-- Responses

responseListDatasetGroups :: ListDatasetGroupsResponse -> TestTree
responseListDatasetGroups =
  res
    "ListDatasetGroupsResponse"
    "fixture/ListDatasetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetGroups)

responseCreateBatchInferenceJob :: CreateBatchInferenceJobResponse -> TestTree
responseCreateBatchInferenceJob =
  res
    "CreateBatchInferenceJobResponse"
    "fixture/CreateBatchInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchInferenceJob)

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter =
  res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFilter)

responseCreateDatasetImportJob :: CreateDatasetImportJobResponse -> TestTree
responseCreateDatasetImportJob =
  res
    "CreateDatasetImportJobResponse"
    "fixture/CreateDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetImportJob)

responseDescribeSolution :: DescribeSolutionResponse -> TestTree
responseDescribeSolution =
  res
    "DescribeSolutionResponse"
    "fixture/DescribeSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSolution)

responseDescribeDatasetExportJob :: DescribeDatasetExportJobResponse -> TestTree
responseDescribeDatasetExportJob =
  res
    "DescribeDatasetExportJobResponse"
    "fixture/DescribeDatasetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetExportJob)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)

responseListCampaigns :: ListCampaignsResponse -> TestTree
responseListCampaigns =
  res
    "ListCampaignsResponse"
    "fixture/ListCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCampaigns)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseCreateSolutionVersion :: CreateSolutionVersionResponse -> TestTree
responseCreateSolutionVersion =
  res
    "CreateSolutionVersionResponse"
    "fixture/CreateSolutionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolutionVersion)

responseStopSolutionVersionCreation :: StopSolutionVersionCreationResponse -> TestTree
responseStopSolutionVersionCreation =
  res
    "StopSolutionVersionCreationResponse"
    "fixture/StopSolutionVersionCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSolutionVersionCreation)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseDescribeFilter :: DescribeFilterResponse -> TestTree
responseDescribeFilter =
  res
    "DescribeFilterResponse"
    "fixture/DescribeFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFilter)

responseListEventTrackers :: ListEventTrackersResponse -> TestTree
responseListEventTrackers =
  res
    "ListEventTrackersResponse"
    "fixture/ListEventTrackersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventTrackers)

responseCreateDatasetExportJob :: CreateDatasetExportJobResponse -> TestTree
responseCreateDatasetExportJob =
  res
    "CreateDatasetExportJobResponse"
    "fixture/CreateDatasetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetExportJob)

responseCreateSolution :: CreateSolutionResponse -> TestTree
responseCreateSolution =
  res
    "CreateSolutionResponse"
    "fixture/CreateSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolution)

responseDeleteEventTracker :: DeleteEventTrackerResponse -> TestTree
responseDeleteEventTracker =
  res
    "DeleteEventTrackerResponse"
    "fixture/DeleteEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventTracker)

responseDescribeDatasetImportJob :: DescribeDatasetImportJobResponse -> TestTree
responseDescribeDatasetImportJob =
  res
    "DescribeDatasetImportJobResponse"
    "fixture/DescribeDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetImportJob)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseCreateEventTracker :: CreateEventTrackerResponse -> TestTree
responseCreateEventTracker =
  res
    "CreateEventTrackerResponse"
    "fixture/CreateEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventTracker)

responseDeleteSolution :: DeleteSolutionResponse -> TestTree
responseDeleteSolution =
  res
    "DeleteSolutionResponse"
    "fixture/DeleteSolutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSolution)

responseDescribeCampaign :: DescribeCampaignResponse -> TestTree
responseDescribeCampaign =
  res
    "DescribeCampaignResponse"
    "fixture/DescribeCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCampaign)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseDescribeSolutionVersion :: DescribeSolutionVersionResponse -> TestTree
responseDescribeSolutionVersion =
  res
    "DescribeSolutionVersionResponse"
    "fixture/DescribeSolutionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSolutionVersion)

responseDescribeEventTracker :: DescribeEventTrackerResponse -> TestTree
responseDescribeEventTracker =
  res
    "DescribeEventTrackerResponse"
    "fixture/DescribeEventTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventTracker)

responseListDatasetImportJobs :: ListDatasetImportJobsResponse -> TestTree
responseListDatasetImportJobs =
  res
    "ListDatasetImportJobsResponse"
    "fixture/ListDatasetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetImportJobs)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter =
  res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFilter)

responseListBatchInferenceJobs :: ListBatchInferenceJobsResponse -> TestTree
responseListBatchInferenceJobs =
  res
    "ListBatchInferenceJobsResponse"
    "fixture/ListBatchInferenceJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBatchInferenceJobs)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters =
  res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFilters)

responseDeleteDatasetGroup :: DeleteDatasetGroupResponse -> TestTree
responseDeleteDatasetGroup =
  res
    "DeleteDatasetGroupResponse"
    "fixture/DeleteDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetGroup)

responseDescribeSchema :: DescribeSchemaResponse -> TestTree
responseDescribeSchema =
  res
    "DescribeSchemaResponse"
    "fixture/DescribeSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchema)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlgorithm)

responseListSolutionVersions :: ListSolutionVersionsResponse -> TestTree
responseListSolutionVersions =
  res
    "ListSolutionVersionsResponse"
    "fixture/ListSolutionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolutionVersions)

responseDescribeBatchInferenceJob :: DescribeBatchInferenceJobResponse -> TestTree
responseDescribeBatchInferenceJob =
  res
    "DescribeBatchInferenceJobResponse"
    "fixture/DescribeBatchInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBatchInferenceJob)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe =
  res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecipe)

responseListSolutions :: ListSolutionsResponse -> TestTree
responseListSolutions =
  res
    "ListSolutionsResponse"
    "fixture/ListSolutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolutions)

responseListDatasetExportJobs :: ListDatasetExportJobsResponse -> TestTree
responseListDatasetExportJobs =
  res
    "ListDatasetExportJobsResponse"
    "fixture/ListDatasetExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetExportJobs)

responseDescribeDatasetGroup :: DescribeDatasetGroupResponse -> TestTree
responseDescribeDatasetGroup =
  res
    "DescribeDatasetGroupResponse"
    "fixture/DescribeDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetGroup)

responseDescribeFeatureTransformation :: DescribeFeatureTransformationResponse -> TestTree
responseDescribeFeatureTransformation =
  res
    "DescribeFeatureTransformationResponse"
    "fixture/DescribeFeatureTransformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeatureTransformation)

responseGetSolutionMetrics :: GetSolutionMetricsResponse -> TestTree
responseGetSolutionMetrics =
  res
    "GetSolutionMetricsResponse"
    "fixture/GetSolutionMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolutionMetrics)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseCreateDatasetGroup :: CreateDatasetGroupResponse -> TestTree
responseCreateDatasetGroup =
  res
    "CreateDatasetGroupResponse"
    "fixture/CreateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetGroup)

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes =
  res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipes)
