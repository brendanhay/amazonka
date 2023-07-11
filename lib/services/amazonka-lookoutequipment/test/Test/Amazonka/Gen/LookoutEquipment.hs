{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LookoutEquipment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         , requestCreateLabel $
--             newCreateLabel
--
--         , requestCreateLabelGroup $
--             newCreateLabelGroup
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
--         , requestDeleteLabel $
--             newDeleteLabel
--
--         , requestDeleteLabelGroup $
--             newDeleteLabelGroup
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
--         , requestDescribeLabel $
--             newDescribeLabel
--
--         , requestDescribeLabelGroup $
--             newDescribeLabelGroup
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
--         , requestListInferenceEvents $
--             newListInferenceEvents
--
--         , requestListInferenceExecutions $
--             newListInferenceExecutions
--
--         , requestListInferenceSchedulers $
--             newListInferenceSchedulers
--
--         , requestListLabelGroups $
--             newListLabelGroups
--
--         , requestListLabels $
--             newListLabels
--
--         , requestListModels $
--             newListModels
--
--         , requestListSensorStatistics $
--             newListSensorStatistics
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
--         , requestUpdateLabelGroup $
--             newUpdateLabelGroup
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateInferenceScheduler $
--             newCreateInferenceSchedulerResponse
--
--         , responseCreateLabel $
--             newCreateLabelResponse
--
--         , responseCreateLabelGroup $
--             newCreateLabelGroupResponse
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
--         , responseDeleteLabel $
--             newDeleteLabelResponse
--
--         , responseDeleteLabelGroup $
--             newDeleteLabelGroupResponse
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
--         , responseDescribeLabel $
--             newDescribeLabelResponse
--
--         , responseDescribeLabelGroup $
--             newDescribeLabelGroupResponse
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
--         , responseListInferenceEvents $
--             newListInferenceEventsResponse
--
--         , responseListInferenceExecutions $
--             newListInferenceExecutionsResponse
--
--         , responseListInferenceSchedulers $
--             newListInferenceSchedulersResponse
--
--         , responseListLabelGroups $
--             newListLabelGroupsResponse
--
--         , responseListLabels $
--             newListLabelsResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseListSensorStatistics $
--             newListSensorStatisticsResponse
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
--         , responseUpdateLabelGroup $
--             newUpdateLabelGroupResponse
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

requestCreateLabel :: CreateLabel -> TestTree
requestCreateLabel =
  req
    "CreateLabel"
    "fixture/CreateLabel.yaml"

requestCreateLabelGroup :: CreateLabelGroup -> TestTree
requestCreateLabelGroup =
  req
    "CreateLabelGroup"
    "fixture/CreateLabelGroup.yaml"

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

requestDeleteLabel :: DeleteLabel -> TestTree
requestDeleteLabel =
  req
    "DeleteLabel"
    "fixture/DeleteLabel.yaml"

requestDeleteLabelGroup :: DeleteLabelGroup -> TestTree
requestDeleteLabelGroup =
  req
    "DeleteLabelGroup"
    "fixture/DeleteLabelGroup.yaml"

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

requestDescribeLabel :: DescribeLabel -> TestTree
requestDescribeLabel =
  req
    "DescribeLabel"
    "fixture/DescribeLabel.yaml"

requestDescribeLabelGroup :: DescribeLabelGroup -> TestTree
requestDescribeLabelGroup =
  req
    "DescribeLabelGroup"
    "fixture/DescribeLabelGroup.yaml"

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

requestListInferenceEvents :: ListInferenceEvents -> TestTree
requestListInferenceEvents =
  req
    "ListInferenceEvents"
    "fixture/ListInferenceEvents.yaml"

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

requestListLabelGroups :: ListLabelGroups -> TestTree
requestListLabelGroups =
  req
    "ListLabelGroups"
    "fixture/ListLabelGroups.yaml"

requestListLabels :: ListLabels -> TestTree
requestListLabels =
  req
    "ListLabels"
    "fixture/ListLabels.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestListSensorStatistics :: ListSensorStatistics -> TestTree
requestListSensorStatistics =
  req
    "ListSensorStatistics"
    "fixture/ListSensorStatistics.yaml"

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

requestUpdateLabelGroup :: UpdateLabelGroup -> TestTree
requestUpdateLabelGroup =
  req
    "UpdateLabelGroup"
    "fixture/UpdateLabelGroup.yaml"

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

responseCreateLabel :: CreateLabelResponse -> TestTree
responseCreateLabel =
  res
    "CreateLabelResponse"
    "fixture/CreateLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabel)

responseCreateLabelGroup :: CreateLabelGroupResponse -> TestTree
responseCreateLabelGroup =
  res
    "CreateLabelGroupResponse"
    "fixture/CreateLabelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabelGroup)

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

responseDeleteLabel :: DeleteLabelResponse -> TestTree
responseDeleteLabel =
  res
    "DeleteLabelResponse"
    "fixture/DeleteLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabel)

responseDeleteLabelGroup :: DeleteLabelGroupResponse -> TestTree
responseDeleteLabelGroup =
  res
    "DeleteLabelGroupResponse"
    "fixture/DeleteLabelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLabelGroup)

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

responseDescribeLabel :: DescribeLabelResponse -> TestTree
responseDescribeLabel =
  res
    "DescribeLabelResponse"
    "fixture/DescribeLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLabel)

responseDescribeLabelGroup :: DescribeLabelGroupResponse -> TestTree
responseDescribeLabelGroup =
  res
    "DescribeLabelGroupResponse"
    "fixture/DescribeLabelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLabelGroup)

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

responseListInferenceEvents :: ListInferenceEventsResponse -> TestTree
responseListInferenceEvents =
  res
    "ListInferenceEventsResponse"
    "fixture/ListInferenceEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceEvents)

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

responseListLabelGroups :: ListLabelGroupsResponse -> TestTree
responseListLabelGroups =
  res
    "ListLabelGroupsResponse"
    "fixture/ListLabelGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabelGroups)

responseListLabels :: ListLabelsResponse -> TestTree
responseListLabels =
  res
    "ListLabelsResponse"
    "fixture/ListLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabels)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseListSensorStatistics :: ListSensorStatisticsResponse -> TestTree
responseListSensorStatistics =
  res
    "ListSensorStatisticsResponse"
    "fixture/ListSensorStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSensorStatistics)

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

responseUpdateLabelGroup :: UpdateLabelGroupResponse -> TestTree
responseUpdateLabelGroup =
  res
    "UpdateLabelGroupResponse"
    "fixture/UpdateLabelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLabelGroup)
