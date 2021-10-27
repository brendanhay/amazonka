{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LookoutVision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LookoutVision where

import Data.Proxy
import Network.AWS.LookoutVision
import Test.AWS.Fixture
import Test.AWS.LookoutVision.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestStopModel $
--             newStopModel
--
--         , requestListDatasetEntries $
--             newListDatasetEntries
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeProject $
--             newDescribeProject
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
--         , requestStartModel $
--             newStartModel
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestDetectAnomalies $
--             newDetectAnomalies
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestUpdateDatasetEntries $
--             newUpdateDatasetEntries
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseStopModel $
--             newStopModelResponse
--
--         , responseListDatasetEntries $
--             newListDatasetEntriesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
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
--         , responseStartModel $
--             newStartModelResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseDetectAnomalies $
--             newDetectAnomaliesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseUpdateDatasetEntries $
--             newUpdateDatasetEntriesResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

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

requestStopModel :: StopModel -> TestTree
requestStopModel =
  req
    "StopModel"
    "fixture/StopModel.yaml"

requestListDatasetEntries :: ListDatasetEntries -> TestTree
requestListDatasetEntries =
  req
    "ListDatasetEntries"
    "fixture/ListDatasetEntries.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

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

requestStartModel :: StartModel -> TestTree
requestStartModel =
  req
    "StartModel"
    "fixture/StartModel.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

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

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestUpdateDatasetEntries :: UpdateDatasetEntries -> TestTree
requestUpdateDatasetEntries =
  req
    "UpdateDatasetEntries"
    "fixture/UpdateDatasetEntries.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataset)

responseStopModel :: StopModelResponse -> TestTree
responseStopModel =
  res
    "StopModelResponse"
    "fixture/StopModelResponse.proto"
    defaultService
    (Proxy :: Proxy StopModel)

responseListDatasetEntries :: ListDatasetEntriesResponse -> TestTree
responseListDatasetEntries =
  res
    "ListDatasetEntriesResponse"
    "fixture/ListDatasetEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatasetEntries)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModel)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataset)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataset)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModel)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModels)

responseStartModel :: StartModelResponse -> TestTree
responseStartModel =
  res
    "StartModelResponse"
    "fixture/StartModelResponse.proto"
    defaultService
    (Proxy :: Proxy StartModel)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModel)

responseDetectAnomalies :: DetectAnomaliesResponse -> TestTree
responseDetectAnomalies =
  res
    "DetectAnomaliesResponse"
    "fixture/DetectAnomaliesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectAnomalies)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseUpdateDatasetEntries :: UpdateDatasetEntriesResponse -> TestTree
responseUpdateDatasetEntries =
  res
    "UpdateDatasetEntriesResponse"
    "fixture/UpdateDatasetEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDatasetEntries)
