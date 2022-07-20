{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RobOMaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RobOMaker where

import Amazonka.RobOMaker
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RobOMaker.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDeleteWorlds $
--             newBatchDeleteWorlds
--
--         , requestBatchDescribeSimulationJob $
--             newBatchDescribeSimulationJob
--
--         , requestCancelDeploymentJob $
--             newCancelDeploymentJob
--
--         , requestCancelSimulationJob $
--             newCancelSimulationJob
--
--         , requestCancelSimulationJobBatch $
--             newCancelSimulationJobBatch
--
--         , requestCancelWorldExportJob $
--             newCancelWorldExportJob
--
--         , requestCancelWorldGenerationJob $
--             newCancelWorldGenerationJob
--
--         , requestCreateDeploymentJob $
--             newCreateDeploymentJob
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateRobot $
--             newCreateRobot
--
--         , requestCreateRobotApplication $
--             newCreateRobotApplication
--
--         , requestCreateRobotApplicationVersion $
--             newCreateRobotApplicationVersion
--
--         , requestCreateSimulationApplication $
--             newCreateSimulationApplication
--
--         , requestCreateSimulationApplicationVersion $
--             newCreateSimulationApplicationVersion
--
--         , requestCreateSimulationJob $
--             newCreateSimulationJob
--
--         , requestCreateWorldExportJob $
--             newCreateWorldExportJob
--
--         , requestCreateWorldGenerationJob $
--             newCreateWorldGenerationJob
--
--         , requestCreateWorldTemplate $
--             newCreateWorldTemplate
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDeleteRobot $
--             newDeleteRobot
--
--         , requestDeleteRobotApplication $
--             newDeleteRobotApplication
--
--         , requestDeleteSimulationApplication $
--             newDeleteSimulationApplication
--
--         , requestDeleteWorldTemplate $
--             newDeleteWorldTemplate
--
--         , requestDeregisterRobot $
--             newDeregisterRobot
--
--         , requestDescribeDeploymentJob $
--             newDescribeDeploymentJob
--
--         , requestDescribeFleet $
--             newDescribeFleet
--
--         , requestDescribeRobot $
--             newDescribeRobot
--
--         , requestDescribeRobotApplication $
--             newDescribeRobotApplication
--
--         , requestDescribeSimulationApplication $
--             newDescribeSimulationApplication
--
--         , requestDescribeSimulationJob $
--             newDescribeSimulationJob
--
--         , requestDescribeSimulationJobBatch $
--             newDescribeSimulationJobBatch
--
--         , requestDescribeWorld $
--             newDescribeWorld
--
--         , requestDescribeWorldExportJob $
--             newDescribeWorldExportJob
--
--         , requestDescribeWorldGenerationJob $
--             newDescribeWorldGenerationJob
--
--         , requestDescribeWorldTemplate $
--             newDescribeWorldTemplate
--
--         , requestGetWorldTemplateBody $
--             newGetWorldTemplateBody
--
--         , requestListDeploymentJobs $
--             newListDeploymentJobs
--
--         , requestListFleets $
--             newListFleets
--
--         , requestListRobotApplications $
--             newListRobotApplications
--
--         , requestListRobots $
--             newListRobots
--
--         , requestListSimulationApplications $
--             newListSimulationApplications
--
--         , requestListSimulationJobBatches $
--             newListSimulationJobBatches
--
--         , requestListSimulationJobs $
--             newListSimulationJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorldExportJobs $
--             newListWorldExportJobs
--
--         , requestListWorldGenerationJobs $
--             newListWorldGenerationJobs
--
--         , requestListWorldTemplates $
--             newListWorldTemplates
--
--         , requestListWorlds $
--             newListWorlds
--
--         , requestRegisterRobot $
--             newRegisterRobot
--
--         , requestRestartSimulationJob $
--             newRestartSimulationJob
--
--         , requestStartSimulationJobBatch $
--             newStartSimulationJobBatch
--
--         , requestSyncDeploymentJob $
--             newSyncDeploymentJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateRobotApplication $
--             newUpdateRobotApplication
--
--         , requestUpdateSimulationApplication $
--             newUpdateSimulationApplication
--
--         , requestUpdateWorldTemplate $
--             newUpdateWorldTemplate
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteWorlds $
--             newBatchDeleteWorldsResponse
--
--         , responseBatchDescribeSimulationJob $
--             newBatchDescribeSimulationJobResponse
--
--         , responseCancelDeploymentJob $
--             newCancelDeploymentJobResponse
--
--         , responseCancelSimulationJob $
--             newCancelSimulationJobResponse
--
--         , responseCancelSimulationJobBatch $
--             newCancelSimulationJobBatchResponse
--
--         , responseCancelWorldExportJob $
--             newCancelWorldExportJobResponse
--
--         , responseCancelWorldGenerationJob $
--             newCancelWorldGenerationJobResponse
--
--         , responseCreateDeploymentJob $
--             newCreateDeploymentJobResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateRobot $
--             newCreateRobotResponse
--
--         , responseCreateRobotApplication $
--             newCreateRobotApplicationResponse
--
--         , responseCreateRobotApplicationVersion $
--             newCreateRobotApplicationVersionResponse
--
--         , responseCreateSimulationApplication $
--             newCreateSimulationApplicationResponse
--
--         , responseCreateSimulationApplicationVersion $
--             newCreateSimulationApplicationVersionResponse
--
--         , responseCreateSimulationJob $
--             newCreateSimulationJobResponse
--
--         , responseCreateWorldExportJob $
--             newCreateWorldExportJobResponse
--
--         , responseCreateWorldGenerationJob $
--             newCreateWorldGenerationJobResponse
--
--         , responseCreateWorldTemplate $
--             newCreateWorldTemplateResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDeleteRobot $
--             newDeleteRobotResponse
--
--         , responseDeleteRobotApplication $
--             newDeleteRobotApplicationResponse
--
--         , responseDeleteSimulationApplication $
--             newDeleteSimulationApplicationResponse
--
--         , responseDeleteWorldTemplate $
--             newDeleteWorldTemplateResponse
--
--         , responseDeregisterRobot $
--             newDeregisterRobotResponse
--
--         , responseDescribeDeploymentJob $
--             newDescribeDeploymentJobResponse
--
--         , responseDescribeFleet $
--             newDescribeFleetResponse
--
--         , responseDescribeRobot $
--             newDescribeRobotResponse
--
--         , responseDescribeRobotApplication $
--             newDescribeRobotApplicationResponse
--
--         , responseDescribeSimulationApplication $
--             newDescribeSimulationApplicationResponse
--
--         , responseDescribeSimulationJob $
--             newDescribeSimulationJobResponse
--
--         , responseDescribeSimulationJobBatch $
--             newDescribeSimulationJobBatchResponse
--
--         , responseDescribeWorld $
--             newDescribeWorldResponse
--
--         , responseDescribeWorldExportJob $
--             newDescribeWorldExportJobResponse
--
--         , responseDescribeWorldGenerationJob $
--             newDescribeWorldGenerationJobResponse
--
--         , responseDescribeWorldTemplate $
--             newDescribeWorldTemplateResponse
--
--         , responseGetWorldTemplateBody $
--             newGetWorldTemplateBodyResponse
--
--         , responseListDeploymentJobs $
--             newListDeploymentJobsResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseListRobotApplications $
--             newListRobotApplicationsResponse
--
--         , responseListRobots $
--             newListRobotsResponse
--
--         , responseListSimulationApplications $
--             newListSimulationApplicationsResponse
--
--         , responseListSimulationJobBatches $
--             newListSimulationJobBatchesResponse
--
--         , responseListSimulationJobs $
--             newListSimulationJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorldExportJobs $
--             newListWorldExportJobsResponse
--
--         , responseListWorldGenerationJobs $
--             newListWorldGenerationJobsResponse
--
--         , responseListWorldTemplates $
--             newListWorldTemplatesResponse
--
--         , responseListWorlds $
--             newListWorldsResponse
--
--         , responseRegisterRobot $
--             newRegisterRobotResponse
--
--         , responseRestartSimulationJob $
--             newRestartSimulationJobResponse
--
--         , responseStartSimulationJobBatch $
--             newStartSimulationJobBatchResponse
--
--         , responseSyncDeploymentJob $
--             newSyncDeploymentJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateRobotApplication $
--             newUpdateRobotApplicationResponse
--
--         , responseUpdateSimulationApplication $
--             newUpdateSimulationApplicationResponse
--
--         , responseUpdateWorldTemplate $
--             newUpdateWorldTemplateResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteWorlds :: BatchDeleteWorlds -> TestTree
requestBatchDeleteWorlds =
  req
    "BatchDeleteWorlds"
    "fixture/BatchDeleteWorlds.yaml"

requestBatchDescribeSimulationJob :: BatchDescribeSimulationJob -> TestTree
requestBatchDescribeSimulationJob =
  req
    "BatchDescribeSimulationJob"
    "fixture/BatchDescribeSimulationJob.yaml"

requestCancelDeploymentJob :: CancelDeploymentJob -> TestTree
requestCancelDeploymentJob =
  req
    "CancelDeploymentJob"
    "fixture/CancelDeploymentJob.yaml"

requestCancelSimulationJob :: CancelSimulationJob -> TestTree
requestCancelSimulationJob =
  req
    "CancelSimulationJob"
    "fixture/CancelSimulationJob.yaml"

requestCancelSimulationJobBatch :: CancelSimulationJobBatch -> TestTree
requestCancelSimulationJobBatch =
  req
    "CancelSimulationJobBatch"
    "fixture/CancelSimulationJobBatch.yaml"

requestCancelWorldExportJob :: CancelWorldExportJob -> TestTree
requestCancelWorldExportJob =
  req
    "CancelWorldExportJob"
    "fixture/CancelWorldExportJob.yaml"

requestCancelWorldGenerationJob :: CancelWorldGenerationJob -> TestTree
requestCancelWorldGenerationJob =
  req
    "CancelWorldGenerationJob"
    "fixture/CancelWorldGenerationJob.yaml"

requestCreateDeploymentJob :: CreateDeploymentJob -> TestTree
requestCreateDeploymentJob =
  req
    "CreateDeploymentJob"
    "fixture/CreateDeploymentJob.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateRobot :: CreateRobot -> TestTree
requestCreateRobot =
  req
    "CreateRobot"
    "fixture/CreateRobot.yaml"

requestCreateRobotApplication :: CreateRobotApplication -> TestTree
requestCreateRobotApplication =
  req
    "CreateRobotApplication"
    "fixture/CreateRobotApplication.yaml"

requestCreateRobotApplicationVersion :: CreateRobotApplicationVersion -> TestTree
requestCreateRobotApplicationVersion =
  req
    "CreateRobotApplicationVersion"
    "fixture/CreateRobotApplicationVersion.yaml"

requestCreateSimulationApplication :: CreateSimulationApplication -> TestTree
requestCreateSimulationApplication =
  req
    "CreateSimulationApplication"
    "fixture/CreateSimulationApplication.yaml"

requestCreateSimulationApplicationVersion :: CreateSimulationApplicationVersion -> TestTree
requestCreateSimulationApplicationVersion =
  req
    "CreateSimulationApplicationVersion"
    "fixture/CreateSimulationApplicationVersion.yaml"

requestCreateSimulationJob :: CreateSimulationJob -> TestTree
requestCreateSimulationJob =
  req
    "CreateSimulationJob"
    "fixture/CreateSimulationJob.yaml"

requestCreateWorldExportJob :: CreateWorldExportJob -> TestTree
requestCreateWorldExportJob =
  req
    "CreateWorldExportJob"
    "fixture/CreateWorldExportJob.yaml"

requestCreateWorldGenerationJob :: CreateWorldGenerationJob -> TestTree
requestCreateWorldGenerationJob =
  req
    "CreateWorldGenerationJob"
    "fixture/CreateWorldGenerationJob.yaml"

requestCreateWorldTemplate :: CreateWorldTemplate -> TestTree
requestCreateWorldTemplate =
  req
    "CreateWorldTemplate"
    "fixture/CreateWorldTemplate.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDeleteRobot :: DeleteRobot -> TestTree
requestDeleteRobot =
  req
    "DeleteRobot"
    "fixture/DeleteRobot.yaml"

requestDeleteRobotApplication :: DeleteRobotApplication -> TestTree
requestDeleteRobotApplication =
  req
    "DeleteRobotApplication"
    "fixture/DeleteRobotApplication.yaml"

requestDeleteSimulationApplication :: DeleteSimulationApplication -> TestTree
requestDeleteSimulationApplication =
  req
    "DeleteSimulationApplication"
    "fixture/DeleteSimulationApplication.yaml"

requestDeleteWorldTemplate :: DeleteWorldTemplate -> TestTree
requestDeleteWorldTemplate =
  req
    "DeleteWorldTemplate"
    "fixture/DeleteWorldTemplate.yaml"

requestDeregisterRobot :: DeregisterRobot -> TestTree
requestDeregisterRobot =
  req
    "DeregisterRobot"
    "fixture/DeregisterRobot.yaml"

requestDescribeDeploymentJob :: DescribeDeploymentJob -> TestTree
requestDescribeDeploymentJob =
  req
    "DescribeDeploymentJob"
    "fixture/DescribeDeploymentJob.yaml"

requestDescribeFleet :: DescribeFleet -> TestTree
requestDescribeFleet =
  req
    "DescribeFleet"
    "fixture/DescribeFleet.yaml"

requestDescribeRobot :: DescribeRobot -> TestTree
requestDescribeRobot =
  req
    "DescribeRobot"
    "fixture/DescribeRobot.yaml"

requestDescribeRobotApplication :: DescribeRobotApplication -> TestTree
requestDescribeRobotApplication =
  req
    "DescribeRobotApplication"
    "fixture/DescribeRobotApplication.yaml"

requestDescribeSimulationApplication :: DescribeSimulationApplication -> TestTree
requestDescribeSimulationApplication =
  req
    "DescribeSimulationApplication"
    "fixture/DescribeSimulationApplication.yaml"

requestDescribeSimulationJob :: DescribeSimulationJob -> TestTree
requestDescribeSimulationJob =
  req
    "DescribeSimulationJob"
    "fixture/DescribeSimulationJob.yaml"

requestDescribeSimulationJobBatch :: DescribeSimulationJobBatch -> TestTree
requestDescribeSimulationJobBatch =
  req
    "DescribeSimulationJobBatch"
    "fixture/DescribeSimulationJobBatch.yaml"

requestDescribeWorld :: DescribeWorld -> TestTree
requestDescribeWorld =
  req
    "DescribeWorld"
    "fixture/DescribeWorld.yaml"

requestDescribeWorldExportJob :: DescribeWorldExportJob -> TestTree
requestDescribeWorldExportJob =
  req
    "DescribeWorldExportJob"
    "fixture/DescribeWorldExportJob.yaml"

requestDescribeWorldGenerationJob :: DescribeWorldGenerationJob -> TestTree
requestDescribeWorldGenerationJob =
  req
    "DescribeWorldGenerationJob"
    "fixture/DescribeWorldGenerationJob.yaml"

requestDescribeWorldTemplate :: DescribeWorldTemplate -> TestTree
requestDescribeWorldTemplate =
  req
    "DescribeWorldTemplate"
    "fixture/DescribeWorldTemplate.yaml"

requestGetWorldTemplateBody :: GetWorldTemplateBody -> TestTree
requestGetWorldTemplateBody =
  req
    "GetWorldTemplateBody"
    "fixture/GetWorldTemplateBody.yaml"

requestListDeploymentJobs :: ListDeploymentJobs -> TestTree
requestListDeploymentJobs =
  req
    "ListDeploymentJobs"
    "fixture/ListDeploymentJobs.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestListRobotApplications :: ListRobotApplications -> TestTree
requestListRobotApplications =
  req
    "ListRobotApplications"
    "fixture/ListRobotApplications.yaml"

requestListRobots :: ListRobots -> TestTree
requestListRobots =
  req
    "ListRobots"
    "fixture/ListRobots.yaml"

requestListSimulationApplications :: ListSimulationApplications -> TestTree
requestListSimulationApplications =
  req
    "ListSimulationApplications"
    "fixture/ListSimulationApplications.yaml"

requestListSimulationJobBatches :: ListSimulationJobBatches -> TestTree
requestListSimulationJobBatches =
  req
    "ListSimulationJobBatches"
    "fixture/ListSimulationJobBatches.yaml"

requestListSimulationJobs :: ListSimulationJobs -> TestTree
requestListSimulationJobs =
  req
    "ListSimulationJobs"
    "fixture/ListSimulationJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorldExportJobs :: ListWorldExportJobs -> TestTree
requestListWorldExportJobs =
  req
    "ListWorldExportJobs"
    "fixture/ListWorldExportJobs.yaml"

requestListWorldGenerationJobs :: ListWorldGenerationJobs -> TestTree
requestListWorldGenerationJobs =
  req
    "ListWorldGenerationJobs"
    "fixture/ListWorldGenerationJobs.yaml"

requestListWorldTemplates :: ListWorldTemplates -> TestTree
requestListWorldTemplates =
  req
    "ListWorldTemplates"
    "fixture/ListWorldTemplates.yaml"

requestListWorlds :: ListWorlds -> TestTree
requestListWorlds =
  req
    "ListWorlds"
    "fixture/ListWorlds.yaml"

requestRegisterRobot :: RegisterRobot -> TestTree
requestRegisterRobot =
  req
    "RegisterRobot"
    "fixture/RegisterRobot.yaml"

requestRestartSimulationJob :: RestartSimulationJob -> TestTree
requestRestartSimulationJob =
  req
    "RestartSimulationJob"
    "fixture/RestartSimulationJob.yaml"

requestStartSimulationJobBatch :: StartSimulationJobBatch -> TestTree
requestStartSimulationJobBatch =
  req
    "StartSimulationJobBatch"
    "fixture/StartSimulationJobBatch.yaml"

requestSyncDeploymentJob :: SyncDeploymentJob -> TestTree
requestSyncDeploymentJob =
  req
    "SyncDeploymentJob"
    "fixture/SyncDeploymentJob.yaml"

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

requestUpdateRobotApplication :: UpdateRobotApplication -> TestTree
requestUpdateRobotApplication =
  req
    "UpdateRobotApplication"
    "fixture/UpdateRobotApplication.yaml"

requestUpdateSimulationApplication :: UpdateSimulationApplication -> TestTree
requestUpdateSimulationApplication =
  req
    "UpdateSimulationApplication"
    "fixture/UpdateSimulationApplication.yaml"

requestUpdateWorldTemplate :: UpdateWorldTemplate -> TestTree
requestUpdateWorldTemplate =
  req
    "UpdateWorldTemplate"
    "fixture/UpdateWorldTemplate.yaml"

-- Responses

responseBatchDeleteWorlds :: BatchDeleteWorldsResponse -> TestTree
responseBatchDeleteWorlds =
  res
    "BatchDeleteWorldsResponse"
    "fixture/BatchDeleteWorldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteWorlds)

responseBatchDescribeSimulationJob :: BatchDescribeSimulationJobResponse -> TestTree
responseBatchDescribeSimulationJob =
  res
    "BatchDescribeSimulationJobResponse"
    "fixture/BatchDescribeSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeSimulationJob)

responseCancelDeploymentJob :: CancelDeploymentJobResponse -> TestTree
responseCancelDeploymentJob =
  res
    "CancelDeploymentJobResponse"
    "fixture/CancelDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDeploymentJob)

responseCancelSimulationJob :: CancelSimulationJobResponse -> TestTree
responseCancelSimulationJob =
  res
    "CancelSimulationJobResponse"
    "fixture/CancelSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSimulationJob)

responseCancelSimulationJobBatch :: CancelSimulationJobBatchResponse -> TestTree
responseCancelSimulationJobBatch =
  res
    "CancelSimulationJobBatchResponse"
    "fixture/CancelSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSimulationJobBatch)

responseCancelWorldExportJob :: CancelWorldExportJobResponse -> TestTree
responseCancelWorldExportJob =
  res
    "CancelWorldExportJobResponse"
    "fixture/CancelWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelWorldExportJob)

responseCancelWorldGenerationJob :: CancelWorldGenerationJobResponse -> TestTree
responseCancelWorldGenerationJob =
  res
    "CancelWorldGenerationJobResponse"
    "fixture/CancelWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelWorldGenerationJob)

responseCreateDeploymentJob :: CreateDeploymentJobResponse -> TestTree
responseCreateDeploymentJob =
  res
    "CreateDeploymentJobResponse"
    "fixture/CreateDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentJob)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCreateRobot :: CreateRobotResponse -> TestTree
responseCreateRobot =
  res
    "CreateRobotResponse"
    "fixture/CreateRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobot)

responseCreateRobotApplication :: CreateRobotApplicationResponse -> TestTree
responseCreateRobotApplication =
  res
    "CreateRobotApplicationResponse"
    "fixture/CreateRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobotApplication)

responseCreateRobotApplicationVersion :: CreateRobotApplicationVersionResponse -> TestTree
responseCreateRobotApplicationVersion =
  res
    "CreateRobotApplicationVersionResponse"
    "fixture/CreateRobotApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobotApplicationVersion)

responseCreateSimulationApplication :: CreateSimulationApplicationResponse -> TestTree
responseCreateSimulationApplication =
  res
    "CreateSimulationApplicationResponse"
    "fixture/CreateSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationApplication)

responseCreateSimulationApplicationVersion :: CreateSimulationApplicationVersionResponse -> TestTree
responseCreateSimulationApplicationVersion =
  res
    "CreateSimulationApplicationVersionResponse"
    "fixture/CreateSimulationApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationApplicationVersion)

responseCreateSimulationJob :: CreateSimulationJobResponse -> TestTree
responseCreateSimulationJob =
  res
    "CreateSimulationJobResponse"
    "fixture/CreateSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationJob)

responseCreateWorldExportJob :: CreateWorldExportJobResponse -> TestTree
responseCreateWorldExportJob =
  res
    "CreateWorldExportJobResponse"
    "fixture/CreateWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldExportJob)

responseCreateWorldGenerationJob :: CreateWorldGenerationJobResponse -> TestTree
responseCreateWorldGenerationJob =
  res
    "CreateWorldGenerationJobResponse"
    "fixture/CreateWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldGenerationJob)

responseCreateWorldTemplate :: CreateWorldTemplateResponse -> TestTree
responseCreateWorldTemplate =
  res
    "CreateWorldTemplateResponse"
    "fixture/CreateWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldTemplate)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDeleteRobot :: DeleteRobotResponse -> TestTree
responseDeleteRobot =
  res
    "DeleteRobotResponse"
    "fixture/DeleteRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRobot)

responseDeleteRobotApplication :: DeleteRobotApplicationResponse -> TestTree
responseDeleteRobotApplication =
  res
    "DeleteRobotApplicationResponse"
    "fixture/DeleteRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRobotApplication)

responseDeleteSimulationApplication :: DeleteSimulationApplicationResponse -> TestTree
responseDeleteSimulationApplication =
  res
    "DeleteSimulationApplicationResponse"
    "fixture/DeleteSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSimulationApplication)

responseDeleteWorldTemplate :: DeleteWorldTemplateResponse -> TestTree
responseDeleteWorldTemplate =
  res
    "DeleteWorldTemplateResponse"
    "fixture/DeleteWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorldTemplate)

responseDeregisterRobot :: DeregisterRobotResponse -> TestTree
responseDeregisterRobot =
  res
    "DeregisterRobotResponse"
    "fixture/DeregisterRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterRobot)

responseDescribeDeploymentJob :: DescribeDeploymentJobResponse -> TestTree
responseDescribeDeploymentJob =
  res
    "DescribeDeploymentJobResponse"
    "fixture/DescribeDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeploymentJob)

responseDescribeFleet :: DescribeFleetResponse -> TestTree
responseDescribeFleet =
  res
    "DescribeFleetResponse"
    "fixture/DescribeFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleet)

responseDescribeRobot :: DescribeRobotResponse -> TestTree
responseDescribeRobot =
  res
    "DescribeRobotResponse"
    "fixture/DescribeRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRobot)

responseDescribeRobotApplication :: DescribeRobotApplicationResponse -> TestTree
responseDescribeRobotApplication =
  res
    "DescribeRobotApplicationResponse"
    "fixture/DescribeRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRobotApplication)

responseDescribeSimulationApplication :: DescribeSimulationApplicationResponse -> TestTree
responseDescribeSimulationApplication =
  res
    "DescribeSimulationApplicationResponse"
    "fixture/DescribeSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationApplication)

responseDescribeSimulationJob :: DescribeSimulationJobResponse -> TestTree
responseDescribeSimulationJob =
  res
    "DescribeSimulationJobResponse"
    "fixture/DescribeSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationJob)

responseDescribeSimulationJobBatch :: DescribeSimulationJobBatchResponse -> TestTree
responseDescribeSimulationJobBatch =
  res
    "DescribeSimulationJobBatchResponse"
    "fixture/DescribeSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationJobBatch)

responseDescribeWorld :: DescribeWorldResponse -> TestTree
responseDescribeWorld =
  res
    "DescribeWorldResponse"
    "fixture/DescribeWorldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorld)

responseDescribeWorldExportJob :: DescribeWorldExportJobResponse -> TestTree
responseDescribeWorldExportJob =
  res
    "DescribeWorldExportJobResponse"
    "fixture/DescribeWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldExportJob)

responseDescribeWorldGenerationJob :: DescribeWorldGenerationJobResponse -> TestTree
responseDescribeWorldGenerationJob =
  res
    "DescribeWorldGenerationJobResponse"
    "fixture/DescribeWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldGenerationJob)

responseDescribeWorldTemplate :: DescribeWorldTemplateResponse -> TestTree
responseDescribeWorldTemplate =
  res
    "DescribeWorldTemplateResponse"
    "fixture/DescribeWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldTemplate)

responseGetWorldTemplateBody :: GetWorldTemplateBodyResponse -> TestTree
responseGetWorldTemplateBody =
  res
    "GetWorldTemplateBodyResponse"
    "fixture/GetWorldTemplateBodyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorldTemplateBody)

responseListDeploymentJobs :: ListDeploymentJobsResponse -> TestTree
responseListDeploymentJobs =
  res
    "ListDeploymentJobsResponse"
    "fixture/ListDeploymentJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentJobs)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseListRobotApplications :: ListRobotApplicationsResponse -> TestTree
responseListRobotApplications =
  res
    "ListRobotApplicationsResponse"
    "fixture/ListRobotApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRobotApplications)

responseListRobots :: ListRobotsResponse -> TestTree
responseListRobots =
  res
    "ListRobotsResponse"
    "fixture/ListRobotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRobots)

responseListSimulationApplications :: ListSimulationApplicationsResponse -> TestTree
responseListSimulationApplications =
  res
    "ListSimulationApplicationsResponse"
    "fixture/ListSimulationApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationApplications)

responseListSimulationJobBatches :: ListSimulationJobBatchesResponse -> TestTree
responseListSimulationJobBatches =
  res
    "ListSimulationJobBatchesResponse"
    "fixture/ListSimulationJobBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationJobBatches)

responseListSimulationJobs :: ListSimulationJobsResponse -> TestTree
responseListSimulationJobs =
  res
    "ListSimulationJobsResponse"
    "fixture/ListSimulationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorldExportJobs :: ListWorldExportJobsResponse -> TestTree
responseListWorldExportJobs =
  res
    "ListWorldExportJobsResponse"
    "fixture/ListWorldExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldExportJobs)

responseListWorldGenerationJobs :: ListWorldGenerationJobsResponse -> TestTree
responseListWorldGenerationJobs =
  res
    "ListWorldGenerationJobsResponse"
    "fixture/ListWorldGenerationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldGenerationJobs)

responseListWorldTemplates :: ListWorldTemplatesResponse -> TestTree
responseListWorldTemplates =
  res
    "ListWorldTemplatesResponse"
    "fixture/ListWorldTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldTemplates)

responseListWorlds :: ListWorldsResponse -> TestTree
responseListWorlds =
  res
    "ListWorldsResponse"
    "fixture/ListWorldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorlds)

responseRegisterRobot :: RegisterRobotResponse -> TestTree
responseRegisterRobot =
  res
    "RegisterRobotResponse"
    "fixture/RegisterRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterRobot)

responseRestartSimulationJob :: RestartSimulationJobResponse -> TestTree
responseRestartSimulationJob =
  res
    "RestartSimulationJobResponse"
    "fixture/RestartSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestartSimulationJob)

responseStartSimulationJobBatch :: StartSimulationJobBatchResponse -> TestTree
responseStartSimulationJobBatch =
  res
    "StartSimulationJobBatchResponse"
    "fixture/StartSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSimulationJobBatch)

responseSyncDeploymentJob :: SyncDeploymentJobResponse -> TestTree
responseSyncDeploymentJob =
  res
    "SyncDeploymentJobResponse"
    "fixture/SyncDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SyncDeploymentJob)

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

responseUpdateRobotApplication :: UpdateRobotApplicationResponse -> TestTree
responseUpdateRobotApplication =
  res
    "UpdateRobotApplicationResponse"
    "fixture/UpdateRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRobotApplication)

responseUpdateSimulationApplication :: UpdateSimulationApplicationResponse -> TestTree
responseUpdateSimulationApplication =
  res
    "UpdateSimulationApplicationResponse"
    "fixture/UpdateSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSimulationApplication)

responseUpdateWorldTemplate :: UpdateWorldTemplateResponse -> TestTree
responseUpdateWorldTemplate =
  res
    "UpdateWorldTemplateResponse"
    "fixture/UpdateWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorldTemplate)
