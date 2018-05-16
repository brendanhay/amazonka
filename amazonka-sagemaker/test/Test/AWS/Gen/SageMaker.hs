{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMaker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SageMaker where

import Data.Proxy
import Network.AWS.SageMaker
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SageMaker.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateNotebookInstance $
--             createNotebookInstance
--
--         , requestDescribeEndpointConfig $
--             describeEndpointConfig
--
--         , requestCreateEndpoint $
--             createEndpoint
--
--         , requestDescribeTrainingJob $
--             describeTrainingJob
--
--         , requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestUpdateEndpoint $
--             updateEndpoint
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfig
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfig
--
--         , requestDescribeNotebookInstance $
--             describeNotebookInstance
--
--         , requestCreateEndpointConfig $
--             createEndpointConfig
--
--         , requestStopNotebookInstance $
--             stopNotebookInstance
--
--         , requestUpdateEndpointWeightsAndCapacities $
--             updateEndpointWeightsAndCapacities
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDeleteEndpointConfig $
--             deleteEndpointConfig
--
--         , requestCreateModel $
--             createModel
--
--         , requestDeleteModel $
--             deleteModel
--
--         , requestListModels $
--             listModels
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfig
--
--         , requestListNotebookInstances $
--             listNotebookInstances
--
--         , requestDeleteNotebookInstance $
--             deleteNotebookInstance
--
--         , requestUpdateNotebookInstance $
--             updateNotebookInstance
--
--         , requestStopTrainingJob $
--             stopTrainingJob
--
--         , requestDescribeModel $
--             describeModel
--
--         , requestListEndpoints $
--             listEndpoints
--
--         , requestCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURL
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigs
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             createNotebookInstanceLifecycleConfig
--
--         , requestStartNotebookInstance $
--             startNotebookInstance
--
--         , requestAddTags $
--             addTags
--
--         , requestListEndpointConfigs $
--             listEndpointConfigs
--
--         , requestListTags $
--             listTags
--
--         , requestCreateTrainingJob $
--             createTrainingJob
--
--         , requestDescribeEndpoint $
--             describeEndpoint
--
--         , requestListTrainingJobs $
--             listTrainingJobs
--
--           ]

--     , testGroup "response"
--         [ responseCreateNotebookInstance $
--             createNotebookInstanceResponse
--
--         , responseDescribeEndpointConfig $
--             describeEndpointConfigResponse
--
--         , responseCreateEndpoint $
--             createEndpointResponse
--
--         , responseDescribeTrainingJob $
--             describeTrainingJobResponse
--
--         , responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             updateEndpointResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfigResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribeNotebookInstance $
--             describeNotebookInstanceResponse
--
--         , responseCreateEndpointConfig $
--             createEndpointConfigResponse
--
--         , responseStopNotebookInstance $
--             stopNotebookInstanceResponse
--
--         , responseUpdateEndpointWeightsAndCapacities $
--             updateEndpointWeightsAndCapacitiesResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDeleteEndpointConfig $
--             deleteEndpointConfigResponse
--
--         , responseCreateModel $
--             createModelResponse
--
--         , responseDeleteModel $
--             deleteModelResponse
--
--         , responseListModels $
--             listModelsResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfigResponse
--
--         , responseListNotebookInstances $
--             listNotebookInstancesResponse
--
--         , responseDeleteNotebookInstance $
--             deleteNotebookInstanceResponse
--
--         , responseUpdateNotebookInstance $
--             updateNotebookInstanceResponse
--
--         , responseStopTrainingJob $
--             stopTrainingJobResponse
--
--         , responseDescribeModel $
--             describeModelResponse
--
--         , responseListEndpoints $
--             listEndpointsResponse
--
--         , responseCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURLResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigsResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             createNotebookInstanceLifecycleConfigResponse
--
--         , responseStartNotebookInstance $
--             startNotebookInstanceResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListEndpointConfigs $
--             listEndpointConfigsResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseCreateTrainingJob $
--             createTrainingJobResponse
--
--         , responseDescribeEndpoint $
--             describeEndpointResponse
--
--         , responseListTrainingJobs $
--             listTrainingJobsResponse
--
--           ]
--     ]

-- Requests

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance = req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig = req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint = req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob = req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint = req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig = req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig = req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestDescribeNotebookInstance :: DescribeNotebookInstance -> TestTree
requestDescribeNotebookInstance = req
    "DescribeNotebookInstance"
    "fixture/DescribeNotebookInstance.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig = req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance = req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

requestUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> TestTree
requestUpdateEndpointWeightsAndCapacities = req
    "UpdateEndpointWeightsAndCapacities"
    "fixture/UpdateEndpointWeightsAndCapacities.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig = req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel = req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestListModels :: ListModels -> TestTree
requestListModels = req
    "ListModels"
    "fixture/ListModels.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig = req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances = req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance = req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance = req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob = req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel = req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints = req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURL -> TestTree
requestCreatePresignedNotebookInstanceURL = req
    "CreatePresignedNotebookInstanceURL"
    "fixture/CreatePresignedNotebookInstanceURL.yaml"

requestListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigs -> TestTree
requestListNotebookInstanceLifecycleConfigs = req
    "ListNotebookInstanceLifecycleConfigs"
    "fixture/ListNotebookInstanceLifecycleConfigs.yaml"

requestCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfig -> TestTree
requestCreateNotebookInstanceLifecycleConfig = req
    "CreateNotebookInstanceLifecycleConfig"
    "fixture/CreateNotebookInstanceLifecycleConfig.yaml"

requestStartNotebookInstance :: StartNotebookInstance -> TestTree
requestStartNotebookInstance = req
    "StartNotebookInstance"
    "fixture/StartNotebookInstance.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs = req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestCreateTrainingJob :: CreateTrainingJob -> TestTree
requestCreateTrainingJob = req
    "CreateTrainingJob"
    "fixture/CreateTrainingJob.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint = req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs = req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

-- Responses

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance = res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstance)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig = res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpointConfig)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint = res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpoint)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob = res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTrainingJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint = res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpoint)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig = res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstanceLifecycleConfig)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig = res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstanceLifecycleConfig)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance = res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstance)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig = res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpointConfig)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance = res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StopNotebookInstance)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities = res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpointWeightsAndCapacities)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteTags)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig = res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpointConfig)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateModel)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteModel)

responseListModels :: ListModelsResponse -> TestTree
responseListModels = res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListModels)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig = res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstanceLifecycleConfig)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances = res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstances)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance = res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstance)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance = res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstance)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob = res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopTrainingJob)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel = res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeModel)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints = res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpoints)

responseCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURLResponse -> TestTree
responseCreatePresignedNotebookInstanceURL = res
    "CreatePresignedNotebookInstanceURLResponse"
    "fixture/CreatePresignedNotebookInstanceURLResponse.proto"
    sageMaker
    (Proxy :: Proxy CreatePresignedNotebookInstanceURL)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs = res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstanceLifecycleConfigs)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig = res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstanceLifecycleConfig)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance = res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StartNotebookInstance)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy AddTags)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs = res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpointConfigs)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTags)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob = res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTrainingJob)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint = res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpoint)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs = res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrainingJobs)
