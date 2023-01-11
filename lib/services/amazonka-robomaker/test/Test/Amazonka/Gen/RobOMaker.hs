{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RobOMaker
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestDeleteRobotApplication $
--             newDeleteRobotApplication
--
--         , requestDeleteSimulationApplication $
--             newDeleteSimulationApplication
--
--         , requestDeleteWorldTemplate $
--             newDeleteWorldTemplate
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
--         , requestListRobotApplications $
--             newListRobotApplications
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
--         , requestRestartSimulationJob $
--             newRestartSimulationJob
--
--         , requestStartSimulationJobBatch $
--             newStartSimulationJobBatch
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
--         , responseDeleteRobotApplication $
--             newDeleteRobotApplicationResponse
--
--         , responseDeleteSimulationApplication $
--             newDeleteSimulationApplicationResponse
--
--         , responseDeleteWorldTemplate $
--             newDeleteWorldTemplateResponse
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
--         , responseListRobotApplications $
--             newListRobotApplicationsResponse
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
--         , responseRestartSimulationJob $
--             newRestartSimulationJobResponse
--
--         , responseStartSimulationJobBatch $
--             newStartSimulationJobBatchResponse
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

requestListRobotApplications :: ListRobotApplications -> TestTree
requestListRobotApplications =
  req
    "ListRobotApplications"
    "fixture/ListRobotApplications.yaml"

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

responseListRobotApplications :: ListRobotApplicationsResponse -> TestTree
responseListRobotApplications =
  res
    "ListRobotApplicationsResponse"
    "fixture/ListRobotApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRobotApplications)

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
