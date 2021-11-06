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
--         [ requestDescribeWorldExportJob $
--             newDescribeWorldExportJob
--
--         , requestBatchDeleteWorlds $
--             newBatchDeleteWorlds
--
--         , requestGetWorldTemplateBody $
--             newGetWorldTemplateBody
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestCreateWorldGenerationJob $
--             newCreateWorldGenerationJob
--
--         , requestListRobotApplications $
--             newListRobotApplications
--
--         , requestUpdateRobotApplication $
--             newUpdateRobotApplication
--
--         , requestDeleteRobotApplication $
--             newDeleteRobotApplication
--
--         , requestCreateSimulationApplicationVersion $
--             newCreateSimulationApplicationVersion
--
--         , requestListDeploymentJobs $
--             newListDeploymentJobs
--
--         , requestDescribeWorld $
--             newDescribeWorld
--
--         , requestCancelSimulationJob $
--             newCancelSimulationJob
--
--         , requestCreateRobotApplication $
--             newCreateRobotApplication
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateDeploymentJob $
--             newCreateDeploymentJob
--
--         , requestRegisterRobot $
--             newRegisterRobot
--
--         , requestListRobots $
--             newListRobots
--
--         , requestCreateWorldExportJob $
--             newCreateWorldExportJob
--
--         , requestBatchDescribeSimulationJob $
--             newBatchDescribeSimulationJob
--
--         , requestCreateSimulationApplication $
--             newCreateSimulationApplication
--
--         , requestStartSimulationJobBatch $
--             newStartSimulationJobBatch
--
--         , requestCreateRobot $
--             newCreateRobot
--
--         , requestDescribeFleet $
--             newDescribeFleet
--
--         , requestListWorldTemplates $
--             newListWorldTemplates
--
--         , requestDescribeRobotApplication $
--             newDescribeRobotApplication
--
--         , requestRestartSimulationJob $
--             newRestartSimulationJob
--
--         , requestDescribeSimulationJob $
--             newDescribeSimulationJob
--
--         , requestDeregisterRobot $
--             newDeregisterRobot
--
--         , requestDescribeSimulationApplication $
--             newDescribeSimulationApplication
--
--         , requestListSimulationJobBatches $
--             newListSimulationJobBatches
--
--         , requestListFleets $
--             newListFleets
--
--         , requestDescribeWorldTemplate $
--             newDescribeWorldTemplate
--
--         , requestCancelWorldExportJob $
--             newCancelWorldExportJob
--
--         , requestListWorldGenerationJobs $
--             newListWorldGenerationJobs
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCancelWorldGenerationJob $
--             newCancelWorldGenerationJob
--
--         , requestDescribeSimulationJobBatch $
--             newDescribeSimulationJobBatch
--
--         , requestListSimulationJobs $
--             newListSimulationJobs
--
--         , requestDeleteRobot $
--             newDeleteRobot
--
--         , requestDeleteSimulationApplication $
--             newDeleteSimulationApplication
--
--         , requestUpdateSimulationApplication $
--             newUpdateSimulationApplication
--
--         , requestCreateSimulationJob $
--             newCreateSimulationJob
--
--         , requestListWorldExportJobs $
--             newListWorldExportJobs
--
--         , requestListSimulationApplications $
--             newListSimulationApplications
--
--         , requestCreateRobotApplicationVersion $
--             newCreateRobotApplicationVersion
--
--         , requestCancelDeploymentJob $
--             newCancelDeploymentJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListWorlds $
--             newListWorlds
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeDeploymentJob $
--             newDescribeDeploymentJob
--
--         , requestDeleteWorldTemplate $
--             newDeleteWorldTemplate
--
--         , requestUpdateWorldTemplate $
--             newUpdateWorldTemplate
--
--         , requestDescribeWorldGenerationJob $
--             newDescribeWorldGenerationJob
--
--         , requestCreateWorldTemplate $
--             newCreateWorldTemplate
--
--         , requestCancelSimulationJobBatch $
--             newCancelSimulationJobBatch
--
--         , requestDescribeRobot $
--             newDescribeRobot
--
--         , requestSyncDeploymentJob $
--             newSyncDeploymentJob
--
--           ]

--     , testGroup "response"
--         [ responseDescribeWorldExportJob $
--             newDescribeWorldExportJobResponse
--
--         , responseBatchDeleteWorlds $
--             newBatchDeleteWorldsResponse
--
--         , responseGetWorldTemplateBody $
--             newGetWorldTemplateBodyResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseCreateWorldGenerationJob $
--             newCreateWorldGenerationJobResponse
--
--         , responseListRobotApplications $
--             newListRobotApplicationsResponse
--
--         , responseUpdateRobotApplication $
--             newUpdateRobotApplicationResponse
--
--         , responseDeleteRobotApplication $
--             newDeleteRobotApplicationResponse
--
--         , responseCreateSimulationApplicationVersion $
--             newCreateSimulationApplicationVersionResponse
--
--         , responseListDeploymentJobs $
--             newListDeploymentJobsResponse
--
--         , responseDescribeWorld $
--             newDescribeWorldResponse
--
--         , responseCancelSimulationJob $
--             newCancelSimulationJobResponse
--
--         , responseCreateRobotApplication $
--             newCreateRobotApplicationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateDeploymentJob $
--             newCreateDeploymentJobResponse
--
--         , responseRegisterRobot $
--             newRegisterRobotResponse
--
--         , responseListRobots $
--             newListRobotsResponse
--
--         , responseCreateWorldExportJob $
--             newCreateWorldExportJobResponse
--
--         , responseBatchDescribeSimulationJob $
--             newBatchDescribeSimulationJobResponse
--
--         , responseCreateSimulationApplication $
--             newCreateSimulationApplicationResponse
--
--         , responseStartSimulationJobBatch $
--             newStartSimulationJobBatchResponse
--
--         , responseCreateRobot $
--             newCreateRobotResponse
--
--         , responseDescribeFleet $
--             newDescribeFleetResponse
--
--         , responseListWorldTemplates $
--             newListWorldTemplatesResponse
--
--         , responseDescribeRobotApplication $
--             newDescribeRobotApplicationResponse
--
--         , responseRestartSimulationJob $
--             newRestartSimulationJobResponse
--
--         , responseDescribeSimulationJob $
--             newDescribeSimulationJobResponse
--
--         , responseDeregisterRobot $
--             newDeregisterRobotResponse
--
--         , responseDescribeSimulationApplication $
--             newDescribeSimulationApplicationResponse
--
--         , responseListSimulationJobBatches $
--             newListSimulationJobBatchesResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseDescribeWorldTemplate $
--             newDescribeWorldTemplateResponse
--
--         , responseCancelWorldExportJob $
--             newCancelWorldExportJobResponse
--
--         , responseListWorldGenerationJobs $
--             newListWorldGenerationJobsResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCancelWorldGenerationJob $
--             newCancelWorldGenerationJobResponse
--
--         , responseDescribeSimulationJobBatch $
--             newDescribeSimulationJobBatchResponse
--
--         , responseListSimulationJobs $
--             newListSimulationJobsResponse
--
--         , responseDeleteRobot $
--             newDeleteRobotResponse
--
--         , responseDeleteSimulationApplication $
--             newDeleteSimulationApplicationResponse
--
--         , responseUpdateSimulationApplication $
--             newUpdateSimulationApplicationResponse
--
--         , responseCreateSimulationJob $
--             newCreateSimulationJobResponse
--
--         , responseListWorldExportJobs $
--             newListWorldExportJobsResponse
--
--         , responseListSimulationApplications $
--             newListSimulationApplicationsResponse
--
--         , responseCreateRobotApplicationVersion $
--             newCreateRobotApplicationVersionResponse
--
--         , responseCancelDeploymentJob $
--             newCancelDeploymentJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListWorlds $
--             newListWorldsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeDeploymentJob $
--             newDescribeDeploymentJobResponse
--
--         , responseDeleteWorldTemplate $
--             newDeleteWorldTemplateResponse
--
--         , responseUpdateWorldTemplate $
--             newUpdateWorldTemplateResponse
--
--         , responseDescribeWorldGenerationJob $
--             newDescribeWorldGenerationJobResponse
--
--         , responseCreateWorldTemplate $
--             newCreateWorldTemplateResponse
--
--         , responseCancelSimulationJobBatch $
--             newCancelSimulationJobBatchResponse
--
--         , responseDescribeRobot $
--             newDescribeRobotResponse
--
--         , responseSyncDeploymentJob $
--             newSyncDeploymentJobResponse
--
--           ]
--     ]

-- Requests

requestDescribeWorldExportJob :: DescribeWorldExportJob -> TestTree
requestDescribeWorldExportJob =
  req
    "DescribeWorldExportJob"
    "fixture/DescribeWorldExportJob.yaml"

requestBatchDeleteWorlds :: BatchDeleteWorlds -> TestTree
requestBatchDeleteWorlds =
  req
    "BatchDeleteWorlds"
    "fixture/BatchDeleteWorlds.yaml"

requestGetWorldTemplateBody :: GetWorldTemplateBody -> TestTree
requestGetWorldTemplateBody =
  req
    "GetWorldTemplateBody"
    "fixture/GetWorldTemplateBody.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestCreateWorldGenerationJob :: CreateWorldGenerationJob -> TestTree
requestCreateWorldGenerationJob =
  req
    "CreateWorldGenerationJob"
    "fixture/CreateWorldGenerationJob.yaml"

requestListRobotApplications :: ListRobotApplications -> TestTree
requestListRobotApplications =
  req
    "ListRobotApplications"
    "fixture/ListRobotApplications.yaml"

requestUpdateRobotApplication :: UpdateRobotApplication -> TestTree
requestUpdateRobotApplication =
  req
    "UpdateRobotApplication"
    "fixture/UpdateRobotApplication.yaml"

requestDeleteRobotApplication :: DeleteRobotApplication -> TestTree
requestDeleteRobotApplication =
  req
    "DeleteRobotApplication"
    "fixture/DeleteRobotApplication.yaml"

requestCreateSimulationApplicationVersion :: CreateSimulationApplicationVersion -> TestTree
requestCreateSimulationApplicationVersion =
  req
    "CreateSimulationApplicationVersion"
    "fixture/CreateSimulationApplicationVersion.yaml"

requestListDeploymentJobs :: ListDeploymentJobs -> TestTree
requestListDeploymentJobs =
  req
    "ListDeploymentJobs"
    "fixture/ListDeploymentJobs.yaml"

requestDescribeWorld :: DescribeWorld -> TestTree
requestDescribeWorld =
  req
    "DescribeWorld"
    "fixture/DescribeWorld.yaml"

requestCancelSimulationJob :: CancelSimulationJob -> TestTree
requestCancelSimulationJob =
  req
    "CancelSimulationJob"
    "fixture/CancelSimulationJob.yaml"

requestCreateRobotApplication :: CreateRobotApplication -> TestTree
requestCreateRobotApplication =
  req
    "CreateRobotApplication"
    "fixture/CreateRobotApplication.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateDeploymentJob :: CreateDeploymentJob -> TestTree
requestCreateDeploymentJob =
  req
    "CreateDeploymentJob"
    "fixture/CreateDeploymentJob.yaml"

requestRegisterRobot :: RegisterRobot -> TestTree
requestRegisterRobot =
  req
    "RegisterRobot"
    "fixture/RegisterRobot.yaml"

requestListRobots :: ListRobots -> TestTree
requestListRobots =
  req
    "ListRobots"
    "fixture/ListRobots.yaml"

requestCreateWorldExportJob :: CreateWorldExportJob -> TestTree
requestCreateWorldExportJob =
  req
    "CreateWorldExportJob"
    "fixture/CreateWorldExportJob.yaml"

requestBatchDescribeSimulationJob :: BatchDescribeSimulationJob -> TestTree
requestBatchDescribeSimulationJob =
  req
    "BatchDescribeSimulationJob"
    "fixture/BatchDescribeSimulationJob.yaml"

requestCreateSimulationApplication :: CreateSimulationApplication -> TestTree
requestCreateSimulationApplication =
  req
    "CreateSimulationApplication"
    "fixture/CreateSimulationApplication.yaml"

requestStartSimulationJobBatch :: StartSimulationJobBatch -> TestTree
requestStartSimulationJobBatch =
  req
    "StartSimulationJobBatch"
    "fixture/StartSimulationJobBatch.yaml"

requestCreateRobot :: CreateRobot -> TestTree
requestCreateRobot =
  req
    "CreateRobot"
    "fixture/CreateRobot.yaml"

requestDescribeFleet :: DescribeFleet -> TestTree
requestDescribeFleet =
  req
    "DescribeFleet"
    "fixture/DescribeFleet.yaml"

requestListWorldTemplates :: ListWorldTemplates -> TestTree
requestListWorldTemplates =
  req
    "ListWorldTemplates"
    "fixture/ListWorldTemplates.yaml"

requestDescribeRobotApplication :: DescribeRobotApplication -> TestTree
requestDescribeRobotApplication =
  req
    "DescribeRobotApplication"
    "fixture/DescribeRobotApplication.yaml"

requestRestartSimulationJob :: RestartSimulationJob -> TestTree
requestRestartSimulationJob =
  req
    "RestartSimulationJob"
    "fixture/RestartSimulationJob.yaml"

requestDescribeSimulationJob :: DescribeSimulationJob -> TestTree
requestDescribeSimulationJob =
  req
    "DescribeSimulationJob"
    "fixture/DescribeSimulationJob.yaml"

requestDeregisterRobot :: DeregisterRobot -> TestTree
requestDeregisterRobot =
  req
    "DeregisterRobot"
    "fixture/DeregisterRobot.yaml"

requestDescribeSimulationApplication :: DescribeSimulationApplication -> TestTree
requestDescribeSimulationApplication =
  req
    "DescribeSimulationApplication"
    "fixture/DescribeSimulationApplication.yaml"

requestListSimulationJobBatches :: ListSimulationJobBatches -> TestTree
requestListSimulationJobBatches =
  req
    "ListSimulationJobBatches"
    "fixture/ListSimulationJobBatches.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestDescribeWorldTemplate :: DescribeWorldTemplate -> TestTree
requestDescribeWorldTemplate =
  req
    "DescribeWorldTemplate"
    "fixture/DescribeWorldTemplate.yaml"

requestCancelWorldExportJob :: CancelWorldExportJob -> TestTree
requestCancelWorldExportJob =
  req
    "CancelWorldExportJob"
    "fixture/CancelWorldExportJob.yaml"

requestListWorldGenerationJobs :: ListWorldGenerationJobs -> TestTree
requestListWorldGenerationJobs =
  req
    "ListWorldGenerationJobs"
    "fixture/ListWorldGenerationJobs.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCancelWorldGenerationJob :: CancelWorldGenerationJob -> TestTree
requestCancelWorldGenerationJob =
  req
    "CancelWorldGenerationJob"
    "fixture/CancelWorldGenerationJob.yaml"

requestDescribeSimulationJobBatch :: DescribeSimulationJobBatch -> TestTree
requestDescribeSimulationJobBatch =
  req
    "DescribeSimulationJobBatch"
    "fixture/DescribeSimulationJobBatch.yaml"

requestListSimulationJobs :: ListSimulationJobs -> TestTree
requestListSimulationJobs =
  req
    "ListSimulationJobs"
    "fixture/ListSimulationJobs.yaml"

requestDeleteRobot :: DeleteRobot -> TestTree
requestDeleteRobot =
  req
    "DeleteRobot"
    "fixture/DeleteRobot.yaml"

requestDeleteSimulationApplication :: DeleteSimulationApplication -> TestTree
requestDeleteSimulationApplication =
  req
    "DeleteSimulationApplication"
    "fixture/DeleteSimulationApplication.yaml"

requestUpdateSimulationApplication :: UpdateSimulationApplication -> TestTree
requestUpdateSimulationApplication =
  req
    "UpdateSimulationApplication"
    "fixture/UpdateSimulationApplication.yaml"

requestCreateSimulationJob :: CreateSimulationJob -> TestTree
requestCreateSimulationJob =
  req
    "CreateSimulationJob"
    "fixture/CreateSimulationJob.yaml"

requestListWorldExportJobs :: ListWorldExportJobs -> TestTree
requestListWorldExportJobs =
  req
    "ListWorldExportJobs"
    "fixture/ListWorldExportJobs.yaml"

requestListSimulationApplications :: ListSimulationApplications -> TestTree
requestListSimulationApplications =
  req
    "ListSimulationApplications"
    "fixture/ListSimulationApplications.yaml"

requestCreateRobotApplicationVersion :: CreateRobotApplicationVersion -> TestTree
requestCreateRobotApplicationVersion =
  req
    "CreateRobotApplicationVersion"
    "fixture/CreateRobotApplicationVersion.yaml"

requestCancelDeploymentJob :: CancelDeploymentJob -> TestTree
requestCancelDeploymentJob =
  req
    "CancelDeploymentJob"
    "fixture/CancelDeploymentJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListWorlds :: ListWorlds -> TestTree
requestListWorlds =
  req
    "ListWorlds"
    "fixture/ListWorlds.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeDeploymentJob :: DescribeDeploymentJob -> TestTree
requestDescribeDeploymentJob =
  req
    "DescribeDeploymentJob"
    "fixture/DescribeDeploymentJob.yaml"

requestDeleteWorldTemplate :: DeleteWorldTemplate -> TestTree
requestDeleteWorldTemplate =
  req
    "DeleteWorldTemplate"
    "fixture/DeleteWorldTemplate.yaml"

requestUpdateWorldTemplate :: UpdateWorldTemplate -> TestTree
requestUpdateWorldTemplate =
  req
    "UpdateWorldTemplate"
    "fixture/UpdateWorldTemplate.yaml"

requestDescribeWorldGenerationJob :: DescribeWorldGenerationJob -> TestTree
requestDescribeWorldGenerationJob =
  req
    "DescribeWorldGenerationJob"
    "fixture/DescribeWorldGenerationJob.yaml"

requestCreateWorldTemplate :: CreateWorldTemplate -> TestTree
requestCreateWorldTemplate =
  req
    "CreateWorldTemplate"
    "fixture/CreateWorldTemplate.yaml"

requestCancelSimulationJobBatch :: CancelSimulationJobBatch -> TestTree
requestCancelSimulationJobBatch =
  req
    "CancelSimulationJobBatch"
    "fixture/CancelSimulationJobBatch.yaml"

requestDescribeRobot :: DescribeRobot -> TestTree
requestDescribeRobot =
  req
    "DescribeRobot"
    "fixture/DescribeRobot.yaml"

requestSyncDeploymentJob :: SyncDeploymentJob -> TestTree
requestSyncDeploymentJob =
  req
    "SyncDeploymentJob"
    "fixture/SyncDeploymentJob.yaml"

-- Responses

responseDescribeWorldExportJob :: DescribeWorldExportJobResponse -> TestTree
responseDescribeWorldExportJob =
  res
    "DescribeWorldExportJobResponse"
    "fixture/DescribeWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldExportJob)

responseBatchDeleteWorlds :: BatchDeleteWorldsResponse -> TestTree
responseBatchDeleteWorlds =
  res
    "BatchDeleteWorldsResponse"
    "fixture/BatchDeleteWorldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteWorlds)

responseGetWorldTemplateBody :: GetWorldTemplateBodyResponse -> TestTree
responseGetWorldTemplateBody =
  res
    "GetWorldTemplateBodyResponse"
    "fixture/GetWorldTemplateBodyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorldTemplateBody)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseCreateWorldGenerationJob :: CreateWorldGenerationJobResponse -> TestTree
responseCreateWorldGenerationJob =
  res
    "CreateWorldGenerationJobResponse"
    "fixture/CreateWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldGenerationJob)

responseListRobotApplications :: ListRobotApplicationsResponse -> TestTree
responseListRobotApplications =
  res
    "ListRobotApplicationsResponse"
    "fixture/ListRobotApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRobotApplications)

responseUpdateRobotApplication :: UpdateRobotApplicationResponse -> TestTree
responseUpdateRobotApplication =
  res
    "UpdateRobotApplicationResponse"
    "fixture/UpdateRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRobotApplication)

responseDeleteRobotApplication :: DeleteRobotApplicationResponse -> TestTree
responseDeleteRobotApplication =
  res
    "DeleteRobotApplicationResponse"
    "fixture/DeleteRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRobotApplication)

responseCreateSimulationApplicationVersion :: CreateSimulationApplicationVersionResponse -> TestTree
responseCreateSimulationApplicationVersion =
  res
    "CreateSimulationApplicationVersionResponse"
    "fixture/CreateSimulationApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationApplicationVersion)

responseListDeploymentJobs :: ListDeploymentJobsResponse -> TestTree
responseListDeploymentJobs =
  res
    "ListDeploymentJobsResponse"
    "fixture/ListDeploymentJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentJobs)

responseDescribeWorld :: DescribeWorldResponse -> TestTree
responseDescribeWorld =
  res
    "DescribeWorldResponse"
    "fixture/DescribeWorldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorld)

responseCancelSimulationJob :: CancelSimulationJobResponse -> TestTree
responseCancelSimulationJob =
  res
    "CancelSimulationJobResponse"
    "fixture/CancelSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSimulationJob)

responseCreateRobotApplication :: CreateRobotApplicationResponse -> TestTree
responseCreateRobotApplication =
  res
    "CreateRobotApplicationResponse"
    "fixture/CreateRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobotApplication)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateDeploymentJob :: CreateDeploymentJobResponse -> TestTree
responseCreateDeploymentJob =
  res
    "CreateDeploymentJobResponse"
    "fixture/CreateDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentJob)

responseRegisterRobot :: RegisterRobotResponse -> TestTree
responseRegisterRobot =
  res
    "RegisterRobotResponse"
    "fixture/RegisterRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterRobot)

responseListRobots :: ListRobotsResponse -> TestTree
responseListRobots =
  res
    "ListRobotsResponse"
    "fixture/ListRobotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRobots)

responseCreateWorldExportJob :: CreateWorldExportJobResponse -> TestTree
responseCreateWorldExportJob =
  res
    "CreateWorldExportJobResponse"
    "fixture/CreateWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldExportJob)

responseBatchDescribeSimulationJob :: BatchDescribeSimulationJobResponse -> TestTree
responseBatchDescribeSimulationJob =
  res
    "BatchDescribeSimulationJobResponse"
    "fixture/BatchDescribeSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeSimulationJob)

responseCreateSimulationApplication :: CreateSimulationApplicationResponse -> TestTree
responseCreateSimulationApplication =
  res
    "CreateSimulationApplicationResponse"
    "fixture/CreateSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationApplication)

responseStartSimulationJobBatch :: StartSimulationJobBatchResponse -> TestTree
responseStartSimulationJobBatch =
  res
    "StartSimulationJobBatchResponse"
    "fixture/StartSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSimulationJobBatch)

responseCreateRobot :: CreateRobotResponse -> TestTree
responseCreateRobot =
  res
    "CreateRobotResponse"
    "fixture/CreateRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobot)

responseDescribeFleet :: DescribeFleetResponse -> TestTree
responseDescribeFleet =
  res
    "DescribeFleetResponse"
    "fixture/DescribeFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleet)

responseListWorldTemplates :: ListWorldTemplatesResponse -> TestTree
responseListWorldTemplates =
  res
    "ListWorldTemplatesResponse"
    "fixture/ListWorldTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldTemplates)

responseDescribeRobotApplication :: DescribeRobotApplicationResponse -> TestTree
responseDescribeRobotApplication =
  res
    "DescribeRobotApplicationResponse"
    "fixture/DescribeRobotApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRobotApplication)

responseRestartSimulationJob :: RestartSimulationJobResponse -> TestTree
responseRestartSimulationJob =
  res
    "RestartSimulationJobResponse"
    "fixture/RestartSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestartSimulationJob)

responseDescribeSimulationJob :: DescribeSimulationJobResponse -> TestTree
responseDescribeSimulationJob =
  res
    "DescribeSimulationJobResponse"
    "fixture/DescribeSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationJob)

responseDeregisterRobot :: DeregisterRobotResponse -> TestTree
responseDeregisterRobot =
  res
    "DeregisterRobotResponse"
    "fixture/DeregisterRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterRobot)

responseDescribeSimulationApplication :: DescribeSimulationApplicationResponse -> TestTree
responseDescribeSimulationApplication =
  res
    "DescribeSimulationApplicationResponse"
    "fixture/DescribeSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationApplication)

responseListSimulationJobBatches :: ListSimulationJobBatchesResponse -> TestTree
responseListSimulationJobBatches =
  res
    "ListSimulationJobBatchesResponse"
    "fixture/ListSimulationJobBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationJobBatches)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseDescribeWorldTemplate :: DescribeWorldTemplateResponse -> TestTree
responseDescribeWorldTemplate =
  res
    "DescribeWorldTemplateResponse"
    "fixture/DescribeWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldTemplate)

responseCancelWorldExportJob :: CancelWorldExportJobResponse -> TestTree
responseCancelWorldExportJob =
  res
    "CancelWorldExportJobResponse"
    "fixture/CancelWorldExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelWorldExportJob)

responseListWorldGenerationJobs :: ListWorldGenerationJobsResponse -> TestTree
responseListWorldGenerationJobs =
  res
    "ListWorldGenerationJobsResponse"
    "fixture/ListWorldGenerationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldGenerationJobs)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCancelWorldGenerationJob :: CancelWorldGenerationJobResponse -> TestTree
responseCancelWorldGenerationJob =
  res
    "CancelWorldGenerationJobResponse"
    "fixture/CancelWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelWorldGenerationJob)

responseDescribeSimulationJobBatch :: DescribeSimulationJobBatchResponse -> TestTree
responseDescribeSimulationJobBatch =
  res
    "DescribeSimulationJobBatchResponse"
    "fixture/DescribeSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulationJobBatch)

responseListSimulationJobs :: ListSimulationJobsResponse -> TestTree
responseListSimulationJobs =
  res
    "ListSimulationJobsResponse"
    "fixture/ListSimulationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationJobs)

responseDeleteRobot :: DeleteRobotResponse -> TestTree
responseDeleteRobot =
  res
    "DeleteRobotResponse"
    "fixture/DeleteRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRobot)

responseDeleteSimulationApplication :: DeleteSimulationApplicationResponse -> TestTree
responseDeleteSimulationApplication =
  res
    "DeleteSimulationApplicationResponse"
    "fixture/DeleteSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSimulationApplication)

responseUpdateSimulationApplication :: UpdateSimulationApplicationResponse -> TestTree
responseUpdateSimulationApplication =
  res
    "UpdateSimulationApplicationResponse"
    "fixture/UpdateSimulationApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSimulationApplication)

responseCreateSimulationJob :: CreateSimulationJobResponse -> TestTree
responseCreateSimulationJob =
  res
    "CreateSimulationJobResponse"
    "fixture/CreateSimulationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSimulationJob)

responseListWorldExportJobs :: ListWorldExportJobsResponse -> TestTree
responseListWorldExportJobs =
  res
    "ListWorldExportJobsResponse"
    "fixture/ListWorldExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorldExportJobs)

responseListSimulationApplications :: ListSimulationApplicationsResponse -> TestTree
responseListSimulationApplications =
  res
    "ListSimulationApplicationsResponse"
    "fixture/ListSimulationApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulationApplications)

responseCreateRobotApplicationVersion :: CreateRobotApplicationVersionResponse -> TestTree
responseCreateRobotApplicationVersion =
  res
    "CreateRobotApplicationVersionResponse"
    "fixture/CreateRobotApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRobotApplicationVersion)

responseCancelDeploymentJob :: CancelDeploymentJobResponse -> TestTree
responseCancelDeploymentJob =
  res
    "CancelDeploymentJobResponse"
    "fixture/CancelDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDeploymentJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListWorlds :: ListWorldsResponse -> TestTree
responseListWorlds =
  res
    "ListWorldsResponse"
    "fixture/ListWorldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorlds)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeDeploymentJob :: DescribeDeploymentJobResponse -> TestTree
responseDescribeDeploymentJob =
  res
    "DescribeDeploymentJobResponse"
    "fixture/DescribeDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeploymentJob)

responseDeleteWorldTemplate :: DeleteWorldTemplateResponse -> TestTree
responseDeleteWorldTemplate =
  res
    "DeleteWorldTemplateResponse"
    "fixture/DeleteWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorldTemplate)

responseUpdateWorldTemplate :: UpdateWorldTemplateResponse -> TestTree
responseUpdateWorldTemplate =
  res
    "UpdateWorldTemplateResponse"
    "fixture/UpdateWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorldTemplate)

responseDescribeWorldGenerationJob :: DescribeWorldGenerationJobResponse -> TestTree
responseDescribeWorldGenerationJob =
  res
    "DescribeWorldGenerationJobResponse"
    "fixture/DescribeWorldGenerationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorldGenerationJob)

responseCreateWorldTemplate :: CreateWorldTemplateResponse -> TestTree
responseCreateWorldTemplate =
  res
    "CreateWorldTemplateResponse"
    "fixture/CreateWorldTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorldTemplate)

responseCancelSimulationJobBatch :: CancelSimulationJobBatchResponse -> TestTree
responseCancelSimulationJobBatch =
  res
    "CancelSimulationJobBatchResponse"
    "fixture/CancelSimulationJobBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSimulationJobBatch)

responseDescribeRobot :: DescribeRobotResponse -> TestTree
responseDescribeRobot =
  res
    "DescribeRobotResponse"
    "fixture/DescribeRobotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRobot)

responseSyncDeploymentJob :: SyncDeploymentJobResponse -> TestTree
responseSyncDeploymentJob =
  res
    "SyncDeploymentJobResponse"
    "fixture/SyncDeploymentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SyncDeploymentJob)
