{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataBrew
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestBatchDeleteRecipeVersion $
--             newBatchDeleteRecipeVersion
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateProfileJob $
--             newCreateProfileJob
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateRecipe $
--             newCreateRecipe
--
--         , requestCreateRecipeJob $
--             newCreateRecipeJob
--
--         , requestCreateRuleset $
--             newCreateRuleset
--
--         , requestCreateSchedule $
--             newCreateSchedule
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteRecipeVersion $
--             newDeleteRecipeVersion
--
--         , requestDeleteRuleset $
--             newDeleteRuleset
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeJob $
--             newDescribeJob
--
--         , requestDescribeJobRun $
--             newDescribeJobRun
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDescribeRecipe $
--             newDescribeRecipe
--
--         , requestDescribeRuleset $
--             newDescribeRuleset
--
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListJobRuns $
--             newListJobRuns
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListRecipeVersions $
--             newListRecipeVersions
--
--         , requestListRecipes $
--             newListRecipes
--
--         , requestListRulesets $
--             newListRulesets
--
--         , requestListSchedules $
--             newListSchedules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPublishRecipe $
--             newPublishRecipe
--
--         , requestSendProjectSessionAction $
--             newSendProjectSessionAction
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestStartProjectSession $
--             newStartProjectSession
--
--         , requestStopJobRun $
--             newStopJobRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDataset $
--             newUpdateDataset
--
--         , requestUpdateProfileJob $
--             newUpdateProfileJob
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateRecipe $
--             newUpdateRecipe
--
--         , requestUpdateRecipeJob $
--             newUpdateRecipeJob
--
--         , requestUpdateRuleset $
--             newUpdateRuleset
--
--         , requestUpdateSchedule $
--             newUpdateSchedule
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteRecipeVersion $
--             newBatchDeleteRecipeVersionResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateProfileJob $
--             newCreateProfileJobResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateRecipe $
--             newCreateRecipeResponse
--
--         , responseCreateRecipeJob $
--             newCreateRecipeJobResponse
--
--         , responseCreateRuleset $
--             newCreateRulesetResponse
--
--         , responseCreateSchedule $
--             newCreateScheduleResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteRecipeVersion $
--             newDeleteRecipeVersionResponse
--
--         , responseDeleteRuleset $
--             newDeleteRulesetResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeJob $
--             newDescribeJobResponse
--
--         , responseDescribeJobRun $
--             newDescribeJobRunResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDescribeRecipe $
--             newDescribeRecipeResponse
--
--         , responseDescribeRuleset $
--             newDescribeRulesetResponse
--
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListJobRuns $
--             newListJobRunsResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListRecipeVersions $
--             newListRecipeVersionsResponse
--
--         , responseListRecipes $
--             newListRecipesResponse
--
--         , responseListRulesets $
--             newListRulesetsResponse
--
--         , responseListSchedules $
--             newListSchedulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePublishRecipe $
--             newPublishRecipeResponse
--
--         , responseSendProjectSessionAction $
--             newSendProjectSessionActionResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseStartProjectSession $
--             newStartProjectSessionResponse
--
--         , responseStopJobRun $
--             newStopJobRunResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDataset $
--             newUpdateDatasetResponse
--
--         , responseUpdateProfileJob $
--             newUpdateProfileJobResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateRecipe $
--             newUpdateRecipeResponse
--
--         , responseUpdateRecipeJob $
--             newUpdateRecipeJobResponse
--
--         , responseUpdateRuleset $
--             newUpdateRulesetResponse
--
--         , responseUpdateSchedule $
--             newUpdateScheduleResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteRecipeVersion :: BatchDeleteRecipeVersion -> TestTree
requestBatchDeleteRecipeVersion =
  req
    "BatchDeleteRecipeVersion"
    "fixture/BatchDeleteRecipeVersion.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateProfileJob :: CreateProfileJob -> TestTree
requestCreateProfileJob =
  req
    "CreateProfileJob"
    "fixture/CreateProfileJob.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateRecipe :: CreateRecipe -> TestTree
requestCreateRecipe =
  req
    "CreateRecipe"
    "fixture/CreateRecipe.yaml"

requestCreateRecipeJob :: CreateRecipeJob -> TestTree
requestCreateRecipeJob =
  req
    "CreateRecipeJob"
    "fixture/CreateRecipeJob.yaml"

requestCreateRuleset :: CreateRuleset -> TestTree
requestCreateRuleset =
  req
    "CreateRuleset"
    "fixture/CreateRuleset.yaml"

requestCreateSchedule :: CreateSchedule -> TestTree
requestCreateSchedule =
  req
    "CreateSchedule"
    "fixture/CreateSchedule.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteRecipeVersion :: DeleteRecipeVersion -> TestTree
requestDeleteRecipeVersion =
  req
    "DeleteRecipeVersion"
    "fixture/DeleteRecipeVersion.yaml"

requestDeleteRuleset :: DeleteRuleset -> TestTree
requestDeleteRuleset =
  req
    "DeleteRuleset"
    "fixture/DeleteRuleset.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeJob :: DescribeJob -> TestTree
requestDescribeJob =
  req
    "DescribeJob"
    "fixture/DescribeJob.yaml"

requestDescribeJobRun :: DescribeJobRun -> TestTree
requestDescribeJobRun =
  req
    "DescribeJobRun"
    "fixture/DescribeJobRun.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe =
  req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

requestDescribeRuleset :: DescribeRuleset -> TestTree
requestDescribeRuleset =
  req
    "DescribeRuleset"
    "fixture/DescribeRuleset.yaml"

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListJobRuns :: ListJobRuns -> TestTree
requestListJobRuns =
  req
    "ListJobRuns"
    "fixture/ListJobRuns.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListRecipeVersions :: ListRecipeVersions -> TestTree
requestListRecipeVersions =
  req
    "ListRecipeVersions"
    "fixture/ListRecipeVersions.yaml"

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes =
  req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

requestListRulesets :: ListRulesets -> TestTree
requestListRulesets =
  req
    "ListRulesets"
    "fixture/ListRulesets.yaml"

requestListSchedules :: ListSchedules -> TestTree
requestListSchedules =
  req
    "ListSchedules"
    "fixture/ListSchedules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPublishRecipe :: PublishRecipe -> TestTree
requestPublishRecipe =
  req
    "PublishRecipe"
    "fixture/PublishRecipe.yaml"

requestSendProjectSessionAction :: SendProjectSessionAction -> TestTree
requestSendProjectSessionAction =
  req
    "SendProjectSessionAction"
    "fixture/SendProjectSessionAction.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestStartProjectSession :: StartProjectSession -> TestTree
requestStartProjectSession =
  req
    "StartProjectSession"
    "fixture/StartProjectSession.yaml"

requestStopJobRun :: StopJobRun -> TestTree
requestStopJobRun =
  req
    "StopJobRun"
    "fixture/StopJobRun.yaml"

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

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset =
  req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestUpdateProfileJob :: UpdateProfileJob -> TestTree
requestUpdateProfileJob =
  req
    "UpdateProfileJob"
    "fixture/UpdateProfileJob.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateRecipe :: UpdateRecipe -> TestTree
requestUpdateRecipe =
  req
    "UpdateRecipe"
    "fixture/UpdateRecipe.yaml"

requestUpdateRecipeJob :: UpdateRecipeJob -> TestTree
requestUpdateRecipeJob =
  req
    "UpdateRecipeJob"
    "fixture/UpdateRecipeJob.yaml"

requestUpdateRuleset :: UpdateRuleset -> TestTree
requestUpdateRuleset =
  req
    "UpdateRuleset"
    "fixture/UpdateRuleset.yaml"

requestUpdateSchedule :: UpdateSchedule -> TestTree
requestUpdateSchedule =
  req
    "UpdateSchedule"
    "fixture/UpdateSchedule.yaml"

-- Responses

responseBatchDeleteRecipeVersion :: BatchDeleteRecipeVersionResponse -> TestTree
responseBatchDeleteRecipeVersion =
  res
    "BatchDeleteRecipeVersionResponse"
    "fixture/BatchDeleteRecipeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteRecipeVersion)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateProfileJob :: CreateProfileJobResponse -> TestTree
responseCreateProfileJob =
  res
    "CreateProfileJobResponse"
    "fixture/CreateProfileJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfileJob)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateRecipe :: CreateRecipeResponse -> TestTree
responseCreateRecipe =
  res
    "CreateRecipeResponse"
    "fixture/CreateRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecipe)

responseCreateRecipeJob :: CreateRecipeJobResponse -> TestTree
responseCreateRecipeJob =
  res
    "CreateRecipeJobResponse"
    "fixture/CreateRecipeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecipeJob)

responseCreateRuleset :: CreateRulesetResponse -> TestTree
responseCreateRuleset =
  res
    "CreateRulesetResponse"
    "fixture/CreateRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleset)

responseCreateSchedule :: CreateScheduleResponse -> TestTree
responseCreateSchedule =
  res
    "CreateScheduleResponse"
    "fixture/CreateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchedule)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteRecipeVersion :: DeleteRecipeVersionResponse -> TestTree
responseDeleteRecipeVersion =
  res
    "DeleteRecipeVersionResponse"
    "fixture/DeleteRecipeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecipeVersion)

responseDeleteRuleset :: DeleteRulesetResponse -> TestTree
responseDeleteRuleset =
  res
    "DeleteRulesetResponse"
    "fixture/DeleteRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleset)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedule)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeJob :: DescribeJobResponse -> TestTree
responseDescribeJob =
  res
    "DescribeJobResponse"
    "fixture/DescribeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJob)

responseDescribeJobRun :: DescribeJobRunResponse -> TestTree
responseDescribeJobRun =
  res
    "DescribeJobRunResponse"
    "fixture/DescribeJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJobRun)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe =
  res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecipe)

responseDescribeRuleset :: DescribeRulesetResponse -> TestTree
responseDescribeRuleset =
  res
    "DescribeRulesetResponse"
    "fixture/DescribeRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleset)

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchedule)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListJobRuns :: ListJobRunsResponse -> TestTree
responseListJobRuns =
  res
    "ListJobRunsResponse"
    "fixture/ListJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobRuns)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListRecipeVersions :: ListRecipeVersionsResponse -> TestTree
responseListRecipeVersions =
  res
    "ListRecipeVersionsResponse"
    "fixture/ListRecipeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipeVersions)

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes =
  res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecipes)

responseListRulesets :: ListRulesetsResponse -> TestTree
responseListRulesets =
  res
    "ListRulesetsResponse"
    "fixture/ListRulesetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRulesets)

responseListSchedules :: ListSchedulesResponse -> TestTree
responseListSchedules =
  res
    "ListSchedulesResponse"
    "fixture/ListSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchedules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePublishRecipe :: PublishRecipeResponse -> TestTree
responsePublishRecipe =
  res
    "PublishRecipeResponse"
    "fixture/PublishRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishRecipe)

responseSendProjectSessionAction :: SendProjectSessionActionResponse -> TestTree
responseSendProjectSessionAction =
  res
    "SendProjectSessionActionResponse"
    "fixture/SendProjectSessionActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendProjectSessionAction)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

responseStartProjectSession :: StartProjectSessionResponse -> TestTree
responseStartProjectSession =
  res
    "StartProjectSessionResponse"
    "fixture/StartProjectSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProjectSession)

responseStopJobRun :: StopJobRunResponse -> TestTree
responseStopJobRun =
  res
    "StopJobRunResponse"
    "fixture/StopJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJobRun)

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

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataset)

responseUpdateProfileJob :: UpdateProfileJobResponse -> TestTree
responseUpdateProfileJob =
  res
    "UpdateProfileJobResponse"
    "fixture/UpdateProfileJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfileJob)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateRecipe :: UpdateRecipeResponse -> TestTree
responseUpdateRecipe =
  res
    "UpdateRecipeResponse"
    "fixture/UpdateRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecipe)

responseUpdateRecipeJob :: UpdateRecipeJobResponse -> TestTree
responseUpdateRecipeJob =
  res
    "UpdateRecipeJobResponse"
    "fixture/UpdateRecipeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecipeJob)

responseUpdateRuleset :: UpdateRulesetResponse -> TestTree
responseUpdateRuleset =
  res
    "UpdateRulesetResponse"
    "fixture/UpdateRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleset)

responseUpdateSchedule :: UpdateScheduleResponse -> TestTree
responseUpdateSchedule =
  res
    "UpdateScheduleResponse"
    "fixture/UpdateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchedule)
