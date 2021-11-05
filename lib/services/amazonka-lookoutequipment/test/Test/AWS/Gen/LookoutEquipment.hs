{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LookoutEquipment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LookoutEquipment where

import qualified Data.Proxy as Proxy
import Network.AWS.LookoutEquipment
import Test.AWS.Fixture
import Test.AWS.LookoutEquipment.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartInferenceScheduler $
--             newStartInferenceScheduler
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeDataIngestionJob $
--             newDescribeDataIngestionJob
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestListModels $
--             newListModels
--
--         , requestStopInferenceScheduler $
--             newStopInferenceScheduler
--
--         , requestListDataIngestionJobs $
--             newListDataIngestionJobs
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestStartDataIngestionJob $
--             newStartDataIngestionJob
--
--         , requestListInferenceSchedulers $
--             newListInferenceSchedulers
--
--         , requestUpdateInferenceScheduler $
--             newUpdateInferenceScheduler
--
--         , requestDeleteInferenceScheduler $
--             newDeleteInferenceScheduler
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListInferenceExecutions $
--             newListInferenceExecutions
--
--         , requestCreateInferenceScheduler $
--             newCreateInferenceScheduler
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeInferenceScheduler $
--             newDescribeInferenceScheduler
--
--           ]

--     , testGroup "response"
--         [ responseStartInferenceScheduler $
--             newStartInferenceSchedulerResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeDataIngestionJob $
--             newDescribeDataIngestionJobResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseStopInferenceScheduler $
--             newStopInferenceSchedulerResponse
--
--         , responseListDataIngestionJobs $
--             newListDataIngestionJobsResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseStartDataIngestionJob $
--             newStartDataIngestionJobResponse
--
--         , responseListInferenceSchedulers $
--             newListInferenceSchedulersResponse
--
--         , responseUpdateInferenceScheduler $
--             newUpdateInferenceSchedulerResponse
--
--         , responseDeleteInferenceScheduler $
--             newDeleteInferenceSchedulerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListInferenceExecutions $
--             newListInferenceExecutionsResponse
--
--         , responseCreateInferenceScheduler $
--             newCreateInferenceSchedulerResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeInferenceScheduler $
--             newDescribeInferenceSchedulerResponse
--
--           ]
--     ]

-- Requests

requestStartInferenceScheduler :: StartInferenceScheduler -> TestTree
requestStartInferenceScheduler =
  req
    "StartInferenceScheduler"
    "fixture/StartInferenceScheduler.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDataIngestionJob :: DescribeDataIngestionJob -> TestTree
requestDescribeDataIngestionJob =
  req
    "DescribeDataIngestionJob"
    "fixture/DescribeDataIngestionJob.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

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

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestStopInferenceScheduler :: StopInferenceScheduler -> TestTree
requestStopInferenceScheduler =
  req
    "StopInferenceScheduler"
    "fixture/StopInferenceScheduler.yaml"

requestListDataIngestionJobs :: ListDataIngestionJobs -> TestTree
requestListDataIngestionJobs =
  req
    "ListDataIngestionJobs"
    "fixture/ListDataIngestionJobs.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestStartDataIngestionJob :: StartDataIngestionJob -> TestTree
requestStartDataIngestionJob =
  req
    "StartDataIngestionJob"
    "fixture/StartDataIngestionJob.yaml"

requestListInferenceSchedulers :: ListInferenceSchedulers -> TestTree
requestListInferenceSchedulers =
  req
    "ListInferenceSchedulers"
    "fixture/ListInferenceSchedulers.yaml"

requestUpdateInferenceScheduler :: UpdateInferenceScheduler -> TestTree
requestUpdateInferenceScheduler =
  req
    "UpdateInferenceScheduler"
    "fixture/UpdateInferenceScheduler.yaml"

requestDeleteInferenceScheduler :: DeleteInferenceScheduler -> TestTree
requestDeleteInferenceScheduler =
  req
    "DeleteInferenceScheduler"
    "fixture/DeleteInferenceScheduler.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListInferenceExecutions :: ListInferenceExecutions -> TestTree
requestListInferenceExecutions =
  req
    "ListInferenceExecutions"
    "fixture/ListInferenceExecutions.yaml"

requestCreateInferenceScheduler :: CreateInferenceScheduler -> TestTree
requestCreateInferenceScheduler =
  req
    "CreateInferenceScheduler"
    "fixture/CreateInferenceScheduler.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeInferenceScheduler :: DescribeInferenceScheduler -> TestTree
requestDescribeInferenceScheduler =
  req
    "DescribeInferenceScheduler"
    "fixture/DescribeInferenceScheduler.yaml"

-- Responses

responseStartInferenceScheduler :: StartInferenceSchedulerResponse -> TestTree
responseStartInferenceScheduler =
  res
    "StartInferenceSchedulerResponse"
    "fixture/StartInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInferenceScheduler)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeDataIngestionJob :: DescribeDataIngestionJobResponse -> TestTree
responseDescribeDataIngestionJob =
  res
    "DescribeDataIngestionJobResponse"
    "fixture/DescribeDataIngestionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataIngestionJob)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

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

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseStopInferenceScheduler :: StopInferenceSchedulerResponse -> TestTree
responseStopInferenceScheduler =
  res
    "StopInferenceSchedulerResponse"
    "fixture/StopInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInferenceScheduler)

responseListDataIngestionJobs :: ListDataIngestionJobsResponse -> TestTree
responseListDataIngestionJobs =
  res
    "ListDataIngestionJobsResponse"
    "fixture/ListDataIngestionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIngestionJobs)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModel)

responseStartDataIngestionJob :: StartDataIngestionJobResponse -> TestTree
responseStartDataIngestionJob =
  res
    "StartDataIngestionJobResponse"
    "fixture/StartDataIngestionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataIngestionJob)

responseListInferenceSchedulers :: ListInferenceSchedulersResponse -> TestTree
responseListInferenceSchedulers =
  res
    "ListInferenceSchedulersResponse"
    "fixture/ListInferenceSchedulersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceSchedulers)

responseUpdateInferenceScheduler :: UpdateInferenceSchedulerResponse -> TestTree
responseUpdateInferenceScheduler =
  res
    "UpdateInferenceSchedulerResponse"
    "fixture/UpdateInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInferenceScheduler)

responseDeleteInferenceScheduler :: DeleteInferenceSchedulerResponse -> TestTree
responseDeleteInferenceScheduler =
  res
    "DeleteInferenceSchedulerResponse"
    "fixture/DeleteInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInferenceScheduler)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListInferenceExecutions :: ListInferenceExecutionsResponse -> TestTree
responseListInferenceExecutions =
  res
    "ListInferenceExecutionsResponse"
    "fixture/ListInferenceExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceExecutions)

responseCreateInferenceScheduler :: CreateInferenceSchedulerResponse -> TestTree
responseCreateInferenceScheduler =
  res
    "CreateInferenceSchedulerResponse"
    "fixture/CreateInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInferenceScheduler)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeInferenceScheduler :: DescribeInferenceSchedulerResponse -> TestTree
responseDescribeInferenceScheduler =
  res
    "DescribeInferenceSchedulerResponse"
    "fixture/DescribeInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInferenceScheduler)
