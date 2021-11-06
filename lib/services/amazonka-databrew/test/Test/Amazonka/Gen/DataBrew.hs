{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataBrew
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DataBrew where

import Amazonka.DataBrew
import qualified Data.Proxy as Proxy
import Test.Amazonka.DataBrew.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
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
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestListSchedules $
--             newListSchedules
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestCreateRecipeJob $
--             newCreateRecipeJob
--
--         , requestListRecipeVersions $
--             newListRecipeVersions
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestUpdateDataset $
--             newUpdateDataset
--
--         , requestStopJobRun $
--             newStopJobRun
--
--         , requestUpdateRecipeJob $
--             newUpdateRecipeJob
--
--         , requestListJobs $
--             newListJobs
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestCreateRecipe $
--             newCreateRecipe
--
--         , requestUpdateSchedule $
--             newUpdateSchedule
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestBatchDeleteRecipeVersion $
--             newBatchDeleteRecipeVersion
--
--         , requestListJobRuns $
--             newListJobRuns
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestUpdateProfileJob $
--             newUpdateProfileJob
--
--         , requestDescribeRecipe $
--             newDescribeRecipe
--
--         , requestCreateProfileJob $
--             newCreateProfileJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestDescribeJobRun $
--             newDescribeJobRun
--
--         , requestStartProjectSession $
--             newStartProjectSession
--
--         , requestDeleteRecipeVersion $
--             newDeleteRecipeVersion
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestUpdateRecipe $
--             newUpdateRecipe
--
--         , requestCreateSchedule $
--             newCreateSchedule
--
--         , requestListRecipes $
--             newListRecipes
--
--         , requestPublishRecipe $
--             newPublishRecipe
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestSendProjectSessionAction $
--             newSendProjectSessionAction
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseListSchedules $
--             newListSchedulesResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseCreateRecipeJob $
--             newCreateRecipeJobResponse
--
--         , responseListRecipeVersions $
--             newListRecipeVersionsResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseUpdateDataset $
--             newUpdateDatasetResponse
--
--         , responseStopJobRun $
--             newStopJobRunResponse
--
--         , responseUpdateRecipeJob $
--             newUpdateRecipeJobResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseCreateRecipe $
--             newCreateRecipeResponse
--
--         , responseUpdateSchedule $
--             newUpdateScheduleResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseBatchDeleteRecipeVersion $
--             newBatchDeleteRecipeVersionResponse
--
--         , responseListJobRuns $
--             newListJobRunsResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseUpdateProfileJob $
--             newUpdateProfileJobResponse
--
--         , responseDescribeRecipe $
--             newDescribeRecipeResponse
--
--         , responseCreateProfileJob $
--             newCreateProfileJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseDescribeJobRun $
--             newDescribeJobRunResponse
--
--         , responseStartProjectSession $
--             newStartProjectSessionResponse
--
--         , responseDeleteRecipeVersion $
--             newDeleteRecipeVersionResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseUpdateRecipe $
--             newUpdateRecipeResponse
--
--         , responseCreateSchedule $
--             newCreateScheduleResponse
--
--         , responseListRecipes $
--             newListRecipesResponse
--
--         , responsePublishRecipe $
--             newPublishRecipeResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseSendProjectSessionAction $
--             newSendProjectSessionActionResponse
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

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestListSchedules :: ListSchedules -> TestTree
requestListSchedules =
  req
    "ListSchedules"
    "fixture/ListSchedules.yaml"

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

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestCreateRecipeJob :: CreateRecipeJob -> TestTree
requestCreateRecipeJob =
  req
    "CreateRecipeJob"
    "fixture/CreateRecipeJob.yaml"

requestListRecipeVersions :: ListRecipeVersions -> TestTree
requestListRecipeVersions =
  req
    "ListRecipeVersions"
    "fixture/ListRecipeVersions.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset =
  req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestStopJobRun :: StopJobRun -> TestTree
requestStopJobRun =
  req
    "StopJobRun"
    "fixture/StopJobRun.yaml"

requestUpdateRecipeJob :: UpdateRecipeJob -> TestTree
requestUpdateRecipeJob =
  req
    "UpdateRecipeJob"
    "fixture/UpdateRecipeJob.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestCreateRecipe :: CreateRecipe -> TestTree
requestCreateRecipe =
  req
    "CreateRecipe"
    "fixture/CreateRecipe.yaml"

requestUpdateSchedule :: UpdateSchedule -> TestTree
requestUpdateSchedule =
  req
    "UpdateSchedule"
    "fixture/UpdateSchedule.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestBatchDeleteRecipeVersion :: BatchDeleteRecipeVersion -> TestTree
requestBatchDeleteRecipeVersion =
  req
    "BatchDeleteRecipeVersion"
    "fixture/BatchDeleteRecipeVersion.yaml"

requestListJobRuns :: ListJobRuns -> TestTree
requestListJobRuns =
  req
    "ListJobRuns"
    "fixture/ListJobRuns.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestUpdateProfileJob :: UpdateProfileJob -> TestTree
requestUpdateProfileJob =
  req
    "UpdateProfileJob"
    "fixture/UpdateProfileJob.yaml"

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe =
  req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

requestCreateProfileJob :: CreateProfileJob -> TestTree
requestCreateProfileJob =
  req
    "CreateProfileJob"
    "fixture/CreateProfileJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestDescribeJobRun :: DescribeJobRun -> TestTree
requestDescribeJobRun =
  req
    "DescribeJobRun"
    "fixture/DescribeJobRun.yaml"

requestStartProjectSession :: StartProjectSession -> TestTree
requestStartProjectSession =
  req
    "StartProjectSession"
    "fixture/StartProjectSession.yaml"

requestDeleteRecipeVersion :: DeleteRecipeVersion -> TestTree
requestDeleteRecipeVersion =
  req
    "DeleteRecipeVersion"
    "fixture/DeleteRecipeVersion.yaml"

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

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestUpdateRecipe :: UpdateRecipe -> TestTree
requestUpdateRecipe =
  req
    "UpdateRecipe"
    "fixture/UpdateRecipe.yaml"

requestCreateSchedule :: CreateSchedule -> TestTree
requestCreateSchedule =
  req
    "CreateSchedule"
    "fixture/CreateSchedule.yaml"

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes =
  req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

requestPublishRecipe :: PublishRecipe -> TestTree
requestPublishRecipe =
  req
    "PublishRecipe"
    "fixture/PublishRecipe.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestSendProjectSessionAction :: SendProjectSessionAction -> TestTree
requestSendProjectSessionAction =
  req
    "SendProjectSessionAction"
    "fixture/SendProjectSessionAction.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseListSchedules :: ListSchedulesResponse -> TestTree
responseListSchedules =
  res
    "ListSchedulesResponse"
    "fixture/ListSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchedules)

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

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseCreateRecipeJob :: CreateRecipeJobResponse -> TestTree
responseCreateRecipeJob =
  res
    "CreateRecipeJobResponse"
    "fixture/CreateRecipeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecipeJob)

responseListRecipeVersions :: ListRecipeVersionsResponse -> TestTree
responseListRecipeVersions =
  res
    "ListRecipeVersionsResponse"
    "fixture/ListRecipeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipeVersions)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataset)

responseStopJobRun :: StopJobRunResponse -> TestTree
responseStopJobRun =
  res
    "StopJobRunResponse"
    "fixture/StopJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJobRun)

responseUpdateRecipeJob :: UpdateRecipeJobResponse -> TestTree
responseUpdateRecipeJob =
  res
    "UpdateRecipeJobResponse"
    "fixture/UpdateRecipeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecipeJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseCreateRecipe :: CreateRecipeResponse -> TestTree
responseCreateRecipe =
  res
    "CreateRecipeResponse"
    "fixture/CreateRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecipe)

responseUpdateSchedule :: UpdateScheduleResponse -> TestTree
responseUpdateSchedule =
  res
    "UpdateScheduleResponse"
    "fixture/UpdateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchedule)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedule)

responseBatchDeleteRecipeVersion :: BatchDeleteRecipeVersionResponse -> TestTree
responseBatchDeleteRecipeVersion =
  res
    "BatchDeleteRecipeVersionResponse"
    "fixture/BatchDeleteRecipeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteRecipeVersion)

responseListJobRuns :: ListJobRunsResponse -> TestTree
responseListJobRuns =
  res
    "ListJobRunsResponse"
    "fixture/ListJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobRuns)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseUpdateProfileJob :: UpdateProfileJobResponse -> TestTree
responseUpdateProfileJob =
  res
    "UpdateProfileJobResponse"
    "fixture/UpdateProfileJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfileJob)

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe =
  res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecipe)

responseCreateProfileJob :: CreateProfileJobResponse -> TestTree
responseCreateProfileJob =
  res
    "CreateProfileJobResponse"
    "fixture/CreateProfileJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfileJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchedule)

responseDescribeJobRun :: DescribeJobRunResponse -> TestTree
responseDescribeJobRun =
  res
    "DescribeJobRunResponse"
    "fixture/DescribeJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobRun)

responseStartProjectSession :: StartProjectSessionResponse -> TestTree
responseStartProjectSession =
  res
    "StartProjectSessionResponse"
    "fixture/StartProjectSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProjectSession)

responseDeleteRecipeVersion :: DeleteRecipeVersionResponse -> TestTree
responseDeleteRecipeVersion =
  res
    "DeleteRecipeVersionResponse"
    "fixture/DeleteRecipeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecipeVersion)

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

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

responseUpdateRecipe :: UpdateRecipeResponse -> TestTree
responseUpdateRecipe =
  res
    "UpdateRecipeResponse"
    "fixture/UpdateRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecipe)

responseCreateSchedule :: CreateScheduleResponse -> TestTree
responseCreateSchedule =
  res
    "CreateScheduleResponse"
    "fixture/CreateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchedule)

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes =
  res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipes)

responsePublishRecipe :: PublishRecipeResponse -> TestTree
responsePublishRecipe =
  res
    "PublishRecipeResponse"
    "fixture/PublishRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishRecipe)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseSendProjectSessionAction :: SendProjectSessionActionResponse -> TestTree
responseSendProjectSessionAction =
  res
    "SendProjectSessionActionResponse"
    "fixture/SendProjectSessionActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendProjectSessionAction)
