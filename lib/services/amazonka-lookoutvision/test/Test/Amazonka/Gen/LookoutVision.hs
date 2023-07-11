{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LookoutVision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LookoutVision where

import Amazonka.LookoutVision
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LookoutVision.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestDescribeModelPackagingJob $
--             newDescribeModelPackagingJob
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDetectAnomalies $
--             newDetectAnomalies
--
--         , requestListDatasetEntries $
--             newListDatasetEntries
--
--         , requestListModelPackagingJobs $
--             newListModelPackagingJobs
--
--         , requestListModels $
--             newListModels
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartModel $
--             newStartModel
--
--         , requestStartModelPackagingJob $
--             newStartModelPackagingJob
--
--         , requestStopModel $
--             newStopModel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDatasetEntries $
--             newUpdateDatasetEntries
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseDescribeModelPackagingJob $
--             newDescribeModelPackagingJobResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDetectAnomalies $
--             newDetectAnomaliesResponse
--
--         , responseListDatasetEntries $
--             newListDatasetEntriesResponse
--
--         , responseListModelPackagingJobs $
--             newListModelPackagingJobsResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartModel $
--             newStartModelResponse
--
--         , responseStartModelPackagingJob $
--             newStartModelPackagingJobResponse
--
--         , responseStopModel $
--             newStopModelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDatasetEntries $
--             newUpdateDatasetEntriesResponse
--
--           ]
--     ]

-- Requests

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestDescribeModelPackagingJob :: DescribeModelPackagingJob -> TestTree
requestDescribeModelPackagingJob =
  req
    "DescribeModelPackagingJob"
    "fixture/DescribeModelPackagingJob.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestListDatasetEntries :: ListDatasetEntries -> TestTree
requestListDatasetEntries =
  req
    "ListDatasetEntries"
    "fixture/ListDatasetEntries.yaml"

requestListModelPackagingJobs :: ListModelPackagingJobs -> TestTree
requestListModelPackagingJobs =
  req
    "ListModelPackagingJobs"
    "fixture/ListModelPackagingJobs.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartModel :: StartModel -> TestTree
requestStartModel =
  req
    "StartModel"
    "fixture/StartModel.yaml"

requestStartModelPackagingJob :: StartModelPackagingJob -> TestTree
requestStartModelPackagingJob =
  req
    "StartModelPackagingJob"
    "fixture/StartModelPackagingJob.yaml"

requestStopModel :: StopModel -> TestTree
requestStopModel =
  req
    "StopModel"
    "fixture/StopModel.yaml"

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

requestUpdateDatasetEntries :: UpdateDatasetEntries -> TestTree
requestUpdateDatasetEntries =
  req
    "UpdateDatasetEntries"
    "fixture/UpdateDatasetEntries.yaml"

-- Responses

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModel)

responseDescribeModelPackagingJob :: DescribeModelPackagingJobResponse -> TestTree
responseDescribeModelPackagingJob =
  res
    "DescribeModelPackagingJobResponse"
    "fixture/DescribeModelPackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelPackagingJob)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseDetectAnomalies :: DetectAnomaliesResponse -> TestTree
responseDetectAnomalies =
  res
    "DetectAnomaliesResponse"
    "fixture/DetectAnomaliesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectAnomalies)

responseListDatasetEntries :: ListDatasetEntriesResponse -> TestTree
responseListDatasetEntries =
  res
    "ListDatasetEntriesResponse"
    "fixture/ListDatasetEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetEntries)

responseListModelPackagingJobs :: ListModelPackagingJobsResponse -> TestTree
responseListModelPackagingJobs =
  res
    "ListModelPackagingJobsResponse"
    "fixture/ListModelPackagingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelPackagingJobs)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartModel :: StartModelResponse -> TestTree
responseStartModel =
  res
    "StartModelResponse"
    "fixture/StartModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartModel)

responseStartModelPackagingJob :: StartModelPackagingJobResponse -> TestTree
responseStartModelPackagingJob =
  res
    "StartModelPackagingJobResponse"
    "fixture/StartModelPackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartModelPackagingJob)

responseStopModel :: StopModelResponse -> TestTree
responseStopModel =
  res
    "StopModelResponse"
    "fixture/StopModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopModel)

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

responseUpdateDatasetEntries :: UpdateDatasetEntriesResponse -> TestTree
responseUpdateDatasetEntries =
  res
    "UpdateDatasetEntriesResponse"
    "fixture/UpdateDatasetEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatasetEntries)
