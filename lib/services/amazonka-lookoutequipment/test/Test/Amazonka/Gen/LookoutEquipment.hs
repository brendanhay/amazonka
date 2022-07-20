{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LookoutEquipment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LookoutEquipment where

import Amazonka.LookoutEquipment
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LookoutEquipment.Internal
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
--         , requestCreateInferenceScheduler $
--             newCreateInferenceScheduler
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteInferenceScheduler $
--             newDeleteInferenceScheduler
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDescribeDataIngestionJob $
--             newDescribeDataIngestionJob
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeInferenceScheduler $
--             newDescribeInferenceScheduler
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestListDataIngestionJobs $
--             newListDataIngestionJobs
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListInferenceExecutions $
--             newListInferenceExecutions
--
--         , requestListInferenceSchedulers $
--             newListInferenceSchedulers
--
--         , requestListModels $
--             newListModels
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartDataIngestionJob $
--             newStartDataIngestionJob
--
--         , requestStartInferenceScheduler $
--             newStartInferenceScheduler
--
--         , requestStopInferenceScheduler $
--             newStopInferenceScheduler
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateInferenceScheduler $
--             newUpdateInferenceScheduler
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateInferenceScheduler $
--             newCreateInferenceSchedulerResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteInferenceScheduler $
--             newDeleteInferenceSchedulerResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDescribeDataIngestionJob $
--             newDescribeDataIngestionJobResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeInferenceScheduler $
--             newDescribeInferenceSchedulerResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseListDataIngestionJobs $
--             newListDataIngestionJobsResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListInferenceExecutions $
--             newListInferenceExecutionsResponse
--
--         , responseListInferenceSchedulers $
--             newListInferenceSchedulersResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartDataIngestionJob $
--             newStartDataIngestionJobResponse
--
--         , responseStartInferenceScheduler $
--             newStartInferenceSchedulerResponse
--
--         , responseStopInferenceScheduler $
--             newStopInferenceSchedulerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateInferenceScheduler $
--             newUpdateInferenceSchedulerResponse
--
--           ]
--     ]

-- Requests

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateInferenceScheduler :: CreateInferenceScheduler -> TestTree
requestCreateInferenceScheduler =
  req
    "CreateInferenceScheduler"
    "fixture/CreateInferenceScheduler.yaml"

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

requestDeleteInferenceScheduler :: DeleteInferenceScheduler -> TestTree
requestDeleteInferenceScheduler =
  req
    "DeleteInferenceScheduler"
    "fixture/DeleteInferenceScheduler.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDescribeDataIngestionJob :: DescribeDataIngestionJob -> TestTree
requestDescribeDataIngestionJob =
  req
    "DescribeDataIngestionJob"
    "fixture/DescribeDataIngestionJob.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeInferenceScheduler :: DescribeInferenceScheduler -> TestTree
requestDescribeInferenceScheduler =
  req
    "DescribeInferenceScheduler"
    "fixture/DescribeInferenceScheduler.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestListDataIngestionJobs :: ListDataIngestionJobs -> TestTree
requestListDataIngestionJobs =
  req
    "ListDataIngestionJobs"
    "fixture/ListDataIngestionJobs.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListInferenceExecutions :: ListInferenceExecutions -> TestTree
requestListInferenceExecutions =
  req
    "ListInferenceExecutions"
    "fixture/ListInferenceExecutions.yaml"

requestListInferenceSchedulers :: ListInferenceSchedulers -> TestTree
requestListInferenceSchedulers =
  req
    "ListInferenceSchedulers"
    "fixture/ListInferenceSchedulers.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartDataIngestionJob :: StartDataIngestionJob -> TestTree
requestStartDataIngestionJob =
  req
    "StartDataIngestionJob"
    "fixture/StartDataIngestionJob.yaml"

requestStartInferenceScheduler :: StartInferenceScheduler -> TestTree
requestStartInferenceScheduler =
  req
    "StartInferenceScheduler"
    "fixture/StartInferenceScheduler.yaml"

requestStopInferenceScheduler :: StopInferenceScheduler -> TestTree
requestStopInferenceScheduler =
  req
    "StopInferenceScheduler"
    "fixture/StopInferenceScheduler.yaml"

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

requestUpdateInferenceScheduler :: UpdateInferenceScheduler -> TestTree
requestUpdateInferenceScheduler =
  req
    "UpdateInferenceScheduler"
    "fixture/UpdateInferenceScheduler.yaml"

-- Responses

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateInferenceScheduler :: CreateInferenceSchedulerResponse -> TestTree
responseCreateInferenceScheduler =
  res
    "CreateInferenceSchedulerResponse"
    "fixture/CreateInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInferenceScheduler)

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

responseDeleteInferenceScheduler :: DeleteInferenceSchedulerResponse -> TestTree
responseDeleteInferenceScheduler =
  res
    "DeleteInferenceSchedulerResponse"
    "fixture/DeleteInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInferenceScheduler)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDescribeDataIngestionJob :: DescribeDataIngestionJobResponse -> TestTree
responseDescribeDataIngestionJob =
  res
    "DescribeDataIngestionJobResponse"
    "fixture/DescribeDataIngestionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataIngestionJob)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeInferenceScheduler :: DescribeInferenceSchedulerResponse -> TestTree
responseDescribeInferenceScheduler =
  res
    "DescribeInferenceSchedulerResponse"
    "fixture/DescribeInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInferenceScheduler)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModel)

responseListDataIngestionJobs :: ListDataIngestionJobsResponse -> TestTree
responseListDataIngestionJobs =
  res
    "ListDataIngestionJobsResponse"
    "fixture/ListDataIngestionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIngestionJobs)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListInferenceExecutions :: ListInferenceExecutionsResponse -> TestTree
responseListInferenceExecutions =
  res
    "ListInferenceExecutionsResponse"
    "fixture/ListInferenceExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceExecutions)

responseListInferenceSchedulers :: ListInferenceSchedulersResponse -> TestTree
responseListInferenceSchedulers =
  res
    "ListInferenceSchedulersResponse"
    "fixture/ListInferenceSchedulersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceSchedulers)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartDataIngestionJob :: StartDataIngestionJobResponse -> TestTree
responseStartDataIngestionJob =
  res
    "StartDataIngestionJobResponse"
    "fixture/StartDataIngestionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataIngestionJob)

responseStartInferenceScheduler :: StartInferenceSchedulerResponse -> TestTree
responseStartInferenceScheduler =
  res
    "StartInferenceSchedulerResponse"
    "fixture/StartInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInferenceScheduler)

responseStopInferenceScheduler :: StopInferenceSchedulerResponse -> TestTree
responseStopInferenceScheduler =
  res
    "StopInferenceSchedulerResponse"
    "fixture/StopInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInferenceScheduler)

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

responseUpdateInferenceScheduler :: UpdateInferenceSchedulerResponse -> TestTree
responseUpdateInferenceScheduler =
  res
    "UpdateInferenceSchedulerResponse"
    "fixture/UpdateInferenceSchedulerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInferenceScheduler)
