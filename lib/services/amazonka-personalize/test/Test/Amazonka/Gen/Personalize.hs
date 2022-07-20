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
--         [ requestCreateBatchInferenceJob $
--             newCreateBatchInferenceJob
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
--         , requestDescribeRecipe $
--             newDescribeRecipe
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
--         , requestListRecipes $
--             newListRecipes
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
--         , requestStopSolutionVersionCreation $
--             newStopSolutionVersionCreation
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--           ]

--     , testGroup "response"
--         [ responseCreateBatchInferenceJob $
--             newCreateBatchInferenceJobResponse
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
--         , responseDescribeRecipe $
--             newDescribeRecipeResponse
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
--         , responseListRecipes $
--             newListRecipesResponse
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
--         , responseStopSolutionVersionCreation $
--             newStopSolutionVersionCreationResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--           ]
--     ]

-- Requests

requestCreateBatchInferenceJob :: CreateBatchInferenceJob -> TestTree
requestCreateBatchInferenceJob =
  req
    "CreateBatchInferenceJob"
    "fixture/CreateBatchInferenceJob.yaml"

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

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe =
  req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

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

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes =
  req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

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

requestStopSolutionVersionCreation :: StopSolutionVersionCreation -> TestTree
requestStopSolutionVersionCreation =
  req
    "StopSolutionVersionCreation"
    "fixture/StopSolutionVersionCreation.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

-- Responses

responseCreateBatchInferenceJob :: CreateBatchInferenceJobResponse -> TestTree
responseCreateBatchInferenceJob =
  res
    "CreateBatchInferenceJobResponse"
    "fixture/CreateBatchInferenceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchInferenceJob)

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

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe =
  res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecipe)

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

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes =
  res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipes)

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

responseStopSolutionVersionCreation :: StopSolutionVersionCreationResponse -> TestTree
responseStopSolutionVersionCreation =
  res
    "StopSolutionVersionCreationResponse"
    "fixture/StopSolutionVersionCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSolutionVersionCreation)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)
