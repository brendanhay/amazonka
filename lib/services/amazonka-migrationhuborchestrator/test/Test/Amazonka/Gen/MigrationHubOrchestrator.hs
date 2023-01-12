{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHubOrchestrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MigrationHubOrchestrator where

import Amazonka.MigrationHubOrchestrator
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MigrationHubOrchestrator.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestCreateWorkflowStep $
--             newCreateWorkflowStep
--
--         , requestCreateWorkflowStepGroup $
--             newCreateWorkflowStepGroup
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestDeleteWorkflowStep $
--             newDeleteWorkflowStep
--
--         , requestDeleteWorkflowStepGroup $
--             newDeleteWorkflowStepGroup
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestGetTemplateStep $
--             newGetTemplateStep
--
--         , requestGetTemplateStepGroup $
--             newGetTemplateStepGroup
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestGetWorkflowStep $
--             newGetWorkflowStep
--
--         , requestGetWorkflowStepGroup $
--             newGetWorkflowStepGroup
--
--         , requestListPlugins $
--             newListPlugins
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTemplateStepGroups $
--             newListTemplateStepGroups
--
--         , requestListTemplateSteps $
--             newListTemplateSteps
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestListWorkflowStepGroups $
--             newListWorkflowStepGroups
--
--         , requestListWorkflowSteps $
--             newListWorkflowSteps
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestRetryWorkflowStep $
--             newRetryWorkflowStep
--
--         , requestStartWorkflow $
--             newStartWorkflow
--
--         , requestStopWorkflow $
--             newStopWorkflow
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--         , requestUpdateWorkflowStep $
--             newUpdateWorkflowStep
--
--         , requestUpdateWorkflowStepGroup $
--             newUpdateWorkflowStepGroup
--
--           ]

--     , testGroup "response"
--         [ responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseCreateWorkflowStep $
--             newCreateWorkflowStepResponse
--
--         , responseCreateWorkflowStepGroup $
--             newCreateWorkflowStepGroupResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseDeleteWorkflowStep $
--             newDeleteWorkflowStepResponse
--
--         , responseDeleteWorkflowStepGroup $
--             newDeleteWorkflowStepGroupResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseGetTemplateStep $
--             newGetTemplateStepResponse
--
--         , responseGetTemplateStepGroup $
--             newGetTemplateStepGroupResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseGetWorkflowStep $
--             newGetWorkflowStepResponse
--
--         , responseGetWorkflowStepGroup $
--             newGetWorkflowStepGroupResponse
--
--         , responseListPlugins $
--             newListPluginsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTemplateStepGroups $
--             newListTemplateStepGroupsResponse
--
--         , responseListTemplateSteps $
--             newListTemplateStepsResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseListWorkflowStepGroups $
--             newListWorkflowStepGroupsResponse
--
--         , responseListWorkflowSteps $
--             newListWorkflowStepsResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseRetryWorkflowStep $
--             newRetryWorkflowStepResponse
--
--         , responseStartWorkflow $
--             newStartWorkflowResponse
--
--         , responseStopWorkflow $
--             newStopWorkflowResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--         , responseUpdateWorkflowStep $
--             newUpdateWorkflowStepResponse
--
--         , responseUpdateWorkflowStepGroup $
--             newUpdateWorkflowStepGroupResponse
--
--           ]
--     ]

-- Requests

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestCreateWorkflowStep :: CreateWorkflowStep -> TestTree
requestCreateWorkflowStep =
  req
    "CreateWorkflowStep"
    "fixture/CreateWorkflowStep.yaml"

requestCreateWorkflowStepGroup :: CreateWorkflowStepGroup -> TestTree
requestCreateWorkflowStepGroup =
  req
    "CreateWorkflowStepGroup"
    "fixture/CreateWorkflowStepGroup.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestDeleteWorkflowStep :: DeleteWorkflowStep -> TestTree
requestDeleteWorkflowStep =
  req
    "DeleteWorkflowStep"
    "fixture/DeleteWorkflowStep.yaml"

requestDeleteWorkflowStepGroup :: DeleteWorkflowStepGroup -> TestTree
requestDeleteWorkflowStepGroup =
  req
    "DeleteWorkflowStepGroup"
    "fixture/DeleteWorkflowStepGroup.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestGetTemplateStep :: GetTemplateStep -> TestTree
requestGetTemplateStep =
  req
    "GetTemplateStep"
    "fixture/GetTemplateStep.yaml"

requestGetTemplateStepGroup :: GetTemplateStepGroup -> TestTree
requestGetTemplateStepGroup =
  req
    "GetTemplateStepGroup"
    "fixture/GetTemplateStepGroup.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestGetWorkflowStep :: GetWorkflowStep -> TestTree
requestGetWorkflowStep =
  req
    "GetWorkflowStep"
    "fixture/GetWorkflowStep.yaml"

requestGetWorkflowStepGroup :: GetWorkflowStepGroup -> TestTree
requestGetWorkflowStepGroup =
  req
    "GetWorkflowStepGroup"
    "fixture/GetWorkflowStepGroup.yaml"

requestListPlugins :: ListPlugins -> TestTree
requestListPlugins =
  req
    "ListPlugins"
    "fixture/ListPlugins.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTemplateStepGroups :: ListTemplateStepGroups -> TestTree
requestListTemplateStepGroups =
  req
    "ListTemplateStepGroups"
    "fixture/ListTemplateStepGroups.yaml"

requestListTemplateSteps :: ListTemplateSteps -> TestTree
requestListTemplateSteps =
  req
    "ListTemplateSteps"
    "fixture/ListTemplateSteps.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestListWorkflowStepGroups :: ListWorkflowStepGroups -> TestTree
requestListWorkflowStepGroups =
  req
    "ListWorkflowStepGroups"
    "fixture/ListWorkflowStepGroups.yaml"

requestListWorkflowSteps :: ListWorkflowSteps -> TestTree
requestListWorkflowSteps =
  req
    "ListWorkflowSteps"
    "fixture/ListWorkflowSteps.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestRetryWorkflowStep :: RetryWorkflowStep -> TestTree
requestRetryWorkflowStep =
  req
    "RetryWorkflowStep"
    "fixture/RetryWorkflowStep.yaml"

requestStartWorkflow :: StartWorkflow -> TestTree
requestStartWorkflow =
  req
    "StartWorkflow"
    "fixture/StartWorkflow.yaml"

requestStopWorkflow :: StopWorkflow -> TestTree
requestStopWorkflow =
  req
    "StopWorkflow"
    "fixture/StopWorkflow.yaml"

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

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

requestUpdateWorkflowStep :: UpdateWorkflowStep -> TestTree
requestUpdateWorkflowStep =
  req
    "UpdateWorkflowStep"
    "fixture/UpdateWorkflowStep.yaml"

requestUpdateWorkflowStepGroup :: UpdateWorkflowStepGroup -> TestTree
requestUpdateWorkflowStepGroup =
  req
    "UpdateWorkflowStepGroup"
    "fixture/UpdateWorkflowStepGroup.yaml"

-- Responses

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseCreateWorkflowStep :: CreateWorkflowStepResponse -> TestTree
responseCreateWorkflowStep =
  res
    "CreateWorkflowStepResponse"
    "fixture/CreateWorkflowStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflowStep)

responseCreateWorkflowStepGroup :: CreateWorkflowStepGroupResponse -> TestTree
responseCreateWorkflowStepGroup =
  res
    "CreateWorkflowStepGroupResponse"
    "fixture/CreateWorkflowStepGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflowStepGroup)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseDeleteWorkflowStep :: DeleteWorkflowStepResponse -> TestTree
responseDeleteWorkflowStep =
  res
    "DeleteWorkflowStepResponse"
    "fixture/DeleteWorkflowStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflowStep)

responseDeleteWorkflowStepGroup :: DeleteWorkflowStepGroupResponse -> TestTree
responseDeleteWorkflowStepGroup =
  res
    "DeleteWorkflowStepGroupResponse"
    "fixture/DeleteWorkflowStepGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflowStepGroup)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplate)

responseGetTemplateStep :: GetTemplateStepResponse -> TestTree
responseGetTemplateStep =
  res
    "GetTemplateStepResponse"
    "fixture/GetTemplateStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateStep)

responseGetTemplateStepGroup :: GetTemplateStepGroupResponse -> TestTree
responseGetTemplateStepGroup =
  res
    "GetTemplateStepGroupResponse"
    "fixture/GetTemplateStepGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateStepGroup)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflow)

responseGetWorkflowStep :: GetWorkflowStepResponse -> TestTree
responseGetWorkflowStep =
  res
    "GetWorkflowStepResponse"
    "fixture/GetWorkflowStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowStep)

responseGetWorkflowStepGroup :: GetWorkflowStepGroupResponse -> TestTree
responseGetWorkflowStepGroup =
  res
    "GetWorkflowStepGroupResponse"
    "fixture/GetWorkflowStepGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowStepGroup)

responseListPlugins :: ListPluginsResponse -> TestTree
responseListPlugins =
  res
    "ListPluginsResponse"
    "fixture/ListPluginsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlugins)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTemplateStepGroups :: ListTemplateStepGroupsResponse -> TestTree
responseListTemplateStepGroups =
  res
    "ListTemplateStepGroupsResponse"
    "fixture/ListTemplateStepGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateStepGroups)

responseListTemplateSteps :: ListTemplateStepsResponse -> TestTree
responseListTemplateSteps =
  res
    "ListTemplateStepsResponse"
    "fixture/ListTemplateStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateSteps)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responseListWorkflowStepGroups :: ListWorkflowStepGroupsResponse -> TestTree
responseListWorkflowStepGroups =
  res
    "ListWorkflowStepGroupsResponse"
    "fixture/ListWorkflowStepGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowStepGroups)

responseListWorkflowSteps :: ListWorkflowStepsResponse -> TestTree
responseListWorkflowSteps =
  res
    "ListWorkflowStepsResponse"
    "fixture/ListWorkflowStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowSteps)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responseRetryWorkflowStep :: RetryWorkflowStepResponse -> TestTree
responseRetryWorkflowStep =
  res
    "RetryWorkflowStepResponse"
    "fixture/RetryWorkflowStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryWorkflowStep)

responseStartWorkflow :: StartWorkflowResponse -> TestTree
responseStartWorkflow =
  res
    "StartWorkflowResponse"
    "fixture/StartWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkflow)

responseStopWorkflow :: StopWorkflowResponse -> TestTree
responseStopWorkflow =
  res
    "StopWorkflowResponse"
    "fixture/StopWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopWorkflow)

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

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflow)

responseUpdateWorkflowStep :: UpdateWorkflowStepResponse -> TestTree
responseUpdateWorkflowStep =
  res
    "UpdateWorkflowStepResponse"
    "fixture/UpdateWorkflowStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflowStep)

responseUpdateWorkflowStepGroup :: UpdateWorkflowStepGroupResponse -> TestTree
responseUpdateWorkflowStepGroup =
  res
    "UpdateWorkflowStepGroupResponse"
    "fixture/UpdateWorkflowStepGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflowStepGroup)
