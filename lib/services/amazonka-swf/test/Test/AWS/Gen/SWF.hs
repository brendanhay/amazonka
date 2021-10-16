{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SWF where

import Data.Proxy
import Network.AWS.SWF
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SWF.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSignalWorkflowExecution $
--             newSignalWorkflowExecution
--
--         , requestDescribeWorkflowExecution $
--             newDescribeWorkflowExecution
--
--         , requestRegisterActivityType $
--             newRegisterActivityType
--
--         , requestPollForActivityTask $
--             newPollForActivityTask
--
--         , requestListOpenWorkflowExecutions $
--             newListOpenWorkflowExecutions
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompleted
--
--         , requestPollForDecisionTask $
--             newPollForDecisionTask
--
--         , requestListDomains $
--             newListDomains
--
--         , requestUndeprecateDomain $
--             newUndeprecateDomain
--
--         , requestDeprecateWorkflowType $
--             newDeprecateWorkflowType
--
--         , requestCountClosedWorkflowExecutions $
--             newCountClosedWorkflowExecutions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUndeprecateActivityType $
--             newUndeprecateActivityType
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeprecateActivityType $
--             newDeprecateActivityType
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestCountOpenWorkflowExecutions $
--             newCountOpenWorkflowExecutions
--
--         , requestCountPendingDecisionTasks $
--             newCountPendingDecisionTasks
--
--         , requestRegisterWorkflowType $
--             newRegisterWorkflowType
--
--         , requestCountPendingActivityTasks $
--             newCountPendingActivityTasks
--
--         , requestListActivityTypes $
--             newListActivityTypes
--
--         , requestRespondActivityTaskCompleted $
--             newRespondActivityTaskCompleted
--
--         , requestStartWorkflowExecution $
--             newStartWorkflowExecution
--
--         , requestRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceled
--
--         , requestRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecution
--
--         , requestDescribeWorkflowType $
--             newDescribeWorkflowType
--
--         , requestTerminateWorkflowExecution $
--             newTerminateWorkflowExecution
--
--         , requestDescribeActivityType $
--             newDescribeActivityType
--
--         , requestUndeprecateWorkflowType $
--             newUndeprecateWorkflowType
--
--         , requestDeprecateDomain $
--             newDeprecateDomain
--
--         , requestGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistory
--
--         , requestRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeat
--
--         , requestRespondActivityTaskFailed $
--             newRespondActivityTaskFailed
--
--         , requestListClosedWorkflowExecutions $
--             newListClosedWorkflowExecutions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkflowTypes $
--             newListWorkflowTypes
--
--           ]

--     , testGroup "response"
--         [ responseSignalWorkflowExecution $
--             newSignalWorkflowExecutionResponse
--
--         , responseDescribeWorkflowExecution $
--             newDescribeWorkflowExecutionResponse
--
--         , responseRegisterActivityType $
--             newRegisterActivityTypeResponse
--
--         , responsePollForActivityTask $
--             newPollForActivityTaskResponse
--
--         , responseListOpenWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompletedResponse
--
--         , responsePollForDecisionTask $
--             newPollForDecisionTaskResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseUndeprecateDomain $
--             newUndeprecateDomainResponse
--
--         , responseDeprecateWorkflowType $
--             newDeprecateWorkflowTypeResponse
--
--         , responseCountClosedWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUndeprecateActivityType $
--             newUndeprecateActivityTypeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeprecateActivityType $
--             newDeprecateActivityTypeResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseCountOpenWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseCountPendingDecisionTasks $
--             newPendingTaskCount
--
--         , responseRegisterWorkflowType $
--             newRegisterWorkflowTypeResponse
--
--         , responseCountPendingActivityTasks $
--             newPendingTaskCount
--
--         , responseListActivityTypes $
--             newListActivityTypesResponse
--
--         , responseRespondActivityTaskCompleted $
--             newRespondActivityTaskCompletedResponse
--
--         , responseStartWorkflowExecution $
--             newStartWorkflowExecutionResponse
--
--         , responseRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceledResponse
--
--         , responseRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecutionResponse
--
--         , responseDescribeWorkflowType $
--             newDescribeWorkflowTypeResponse
--
--         , responseTerminateWorkflowExecution $
--             newTerminateWorkflowExecutionResponse
--
--         , responseDescribeActivityType $
--             newDescribeActivityTypeResponse
--
--         , responseUndeprecateWorkflowType $
--             newUndeprecateWorkflowTypeResponse
--
--         , responseDeprecateDomain $
--             newDeprecateDomainResponse
--
--         , responseGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistoryResponse
--
--         , responseRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeatResponse
--
--         , responseRespondActivityTaskFailed $
--             newRespondActivityTaskFailedResponse
--
--         , responseListClosedWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkflowTypes $
--             newListWorkflowTypesResponse
--
--           ]
--     ]

-- Requests

requestSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
requestSignalWorkflowExecution =
  req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution.yaml"

requestDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
requestDescribeWorkflowExecution =
  req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution.yaml"

requestRegisterActivityType :: RegisterActivityType -> TestTree
requestRegisterActivityType =
  req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

requestPollForActivityTask :: PollForActivityTask -> TestTree
requestPollForActivityTask =
  req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

requestListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
requestListOpenWorkflowExecutions =
  req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions.yaml"

requestRegisterDomain :: RegisterDomain -> TestTree
requestRegisterDomain =
  req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

requestRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
requestRespondDecisionTaskCompleted =
  req
    "RespondDecisionTaskCompleted"
    "fixture/RespondDecisionTaskCompleted.yaml"

requestPollForDecisionTask :: PollForDecisionTask -> TestTree
requestPollForDecisionTask =
  req
    "PollForDecisionTask"
    "fixture/PollForDecisionTask.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestUndeprecateDomain :: UndeprecateDomain -> TestTree
requestUndeprecateDomain =
  req
    "UndeprecateDomain"
    "fixture/UndeprecateDomain.yaml"

requestDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
requestDeprecateWorkflowType =
  req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

requestCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
requestCountClosedWorkflowExecutions =
  req
    "CountClosedWorkflowExecutions"
    "fixture/CountClosedWorkflowExecutions.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUndeprecateActivityType :: UndeprecateActivityType -> TestTree
requestUndeprecateActivityType =
  req
    "UndeprecateActivityType"
    "fixture/UndeprecateActivityType.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeprecateActivityType :: DeprecateActivityType -> TestTree
requestDeprecateActivityType =
  req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
requestCountOpenWorkflowExecutions =
  req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions.yaml"

requestCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
requestCountPendingDecisionTasks =
  req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks.yaml"

requestRegisterWorkflowType :: RegisterWorkflowType -> TestTree
requestRegisterWorkflowType =
  req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType.yaml"

requestCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
requestCountPendingActivityTasks =
  req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks.yaml"

requestListActivityTypes :: ListActivityTypes -> TestTree
requestListActivityTypes =
  req
    "ListActivityTypes"
    "fixture/ListActivityTypes.yaml"

requestRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
requestRespondActivityTaskCompleted =
  req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

requestStartWorkflowExecution :: StartWorkflowExecution -> TestTree
requestStartWorkflowExecution =
  req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

requestRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
requestRespondActivityTaskCanceled =
  req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled.yaml"

requestRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
requestRequestCancelWorkflowExecution =
  req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution.yaml"

requestDescribeWorkflowType :: DescribeWorkflowType -> TestTree
requestDescribeWorkflowType =
  req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

requestTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
requestTerminateWorkflowExecution =
  req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution.yaml"

requestDescribeActivityType :: DescribeActivityType -> TestTree
requestDescribeActivityType =
  req
    "DescribeActivityType"
    "fixture/DescribeActivityType.yaml"

requestUndeprecateWorkflowType :: UndeprecateWorkflowType -> TestTree
requestUndeprecateWorkflowType =
  req
    "UndeprecateWorkflowType"
    "fixture/UndeprecateWorkflowType.yaml"

requestDeprecateDomain :: DeprecateDomain -> TestTree
requestDeprecateDomain =
  req
    "DeprecateDomain"
    "fixture/DeprecateDomain.yaml"

requestGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
requestGetWorkflowExecutionHistory =
  req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory.yaml"

requestRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
requestRecordActivityTaskHeartbeat =
  req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

requestRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
requestRespondActivityTaskFailed =
  req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

requestListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
requestListClosedWorkflowExecutions =
  req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkflowTypes :: ListWorkflowTypes -> TestTree
requestListWorkflowTypes =
  req
    "ListWorkflowTypes"
    "fixture/ListWorkflowTypes.yaml"

-- Responses

responseSignalWorkflowExecution :: SignalWorkflowExecutionResponse -> TestTree
responseSignalWorkflowExecution =
  res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy SignalWorkflowExecution)

responseDescribeWorkflowExecution :: DescribeWorkflowExecutionResponse -> TestTree
responseDescribeWorkflowExecution =
  res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkflowExecution)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType =
  res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterActivityType)

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask =
  res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    defaultService
    (Proxy :: Proxy PollForActivityTask)

responseListOpenWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListOpenWorkflowExecutions =
  res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpenWorkflowExecutions)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDomain)

responseRespondDecisionTaskCompleted :: RespondDecisionTaskCompletedResponse -> TestTree
responseRespondDecisionTaskCompleted =
  res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    defaultService
    (Proxy :: Proxy RespondDecisionTaskCompleted)

responsePollForDecisionTask :: PollForDecisionTaskResponse -> TestTree
responsePollForDecisionTask =
  res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    defaultService
    (Proxy :: Proxy PollForDecisionTask)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseUndeprecateDomain :: UndeprecateDomainResponse -> TestTree
responseUndeprecateDomain =
  res
    "UndeprecateDomainResponse"
    "fixture/UndeprecateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateDomain)

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType =
  res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateWorkflowType)

responseCountClosedWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountClosedWorkflowExecutions =
  res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy CountClosedWorkflowExecutions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUndeprecateActivityType :: UndeprecateActivityTypeResponse -> TestTree
responseUndeprecateActivityType =
  res
    "UndeprecateActivityTypeResponse"
    "fixture/UndeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateActivityType)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType =
  res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateActivityType)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomain)

responseCountOpenWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountOpenWorkflowExecutions =
  res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy CountOpenWorkflowExecutions)

responseCountPendingDecisionTasks :: PendingTaskCount -> TestTree
responseCountPendingDecisionTasks =
  res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    defaultService
    (Proxy :: Proxy CountPendingDecisionTasks)

responseRegisterWorkflowType :: RegisterWorkflowTypeResponse -> TestTree
responseRegisterWorkflowType =
  res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWorkflowType)

responseCountPendingActivityTasks :: PendingTaskCount -> TestTree
responseCountPendingActivityTasks =
  res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    defaultService
    (Proxy :: Proxy CountPendingActivityTasks)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes =
  res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListActivityTypes)

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted =
  res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    defaultService
    (Proxy :: Proxy RespondActivityTaskCompleted)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution =
  res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkflowExecution)

responseRespondActivityTaskCanceled :: RespondActivityTaskCanceledResponse -> TestTree
responseRespondActivityTaskCanceled =
  res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    defaultService
    (Proxy :: Proxy RespondActivityTaskCanceled)

responseRequestCancelWorkflowExecution :: RequestCancelWorkflowExecutionResponse -> TestTree
responseRequestCancelWorkflowExecution =
  res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy RequestCancelWorkflowExecution)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType =
  res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkflowType)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution =
  res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateWorkflowExecution)

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType =
  res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivityType)

responseUndeprecateWorkflowType :: UndeprecateWorkflowTypeResponse -> TestTree
responseUndeprecateWorkflowType =
  res
    "UndeprecateWorkflowTypeResponse"
    "fixture/UndeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateWorkflowType)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain =
  res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateDomain)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory =
  res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowExecutionHistory)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat =
  res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed =
  res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    defaultService
    (Proxy :: Proxy RespondActivityTaskFailed)

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions =
  res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListClosedWorkflowExecutions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListWorkflowTypes :: ListWorkflowTypesResponse -> TestTree
responseListWorkflowTypes =
  res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkflowTypes)
