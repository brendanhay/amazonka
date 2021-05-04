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
--         , requestPollForActivityTask $
--             newPollForActivityTask
--
--         , requestRegisterActivityType $
--             newRegisterActivityType
--
--         , requestListOpenWorkflowExecutions $
--             newListOpenWorkflowExecutions
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestListDomains $
--             newListDomains
--
--         , requestRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompleted
--
--         , requestPollForDecisionTask $
--             newPollForDecisionTask
--
--         , requestDeprecateWorkflowType $
--             newDeprecateWorkflowType
--
--         , requestUndeprecateDomain $
--             newUndeprecateDomain
--
--         , requestUndeprecateActivityType $
--             newUndeprecateActivityType
--
--         , requestCountClosedWorkflowExecutions $
--             newCountClosedWorkflowExecutions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeprecateActivityType $
--             newDeprecateActivityType
--
--         , requestTagResource $
--             newTagResource
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
--         , requestRespondActivityTaskCompleted $
--             newRespondActivityTaskCompleted
--
--         , requestListActivityTypes $
--             newListActivityTypes
--
--         , requestStartWorkflowExecution $
--             newStartWorkflowExecution
--
--         , requestDescribeWorkflowType $
--             newDescribeWorkflowType
--
--         , requestRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceled
--
--         , requestRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecution
--
--         , requestDescribeActivityType $
--             newDescribeActivityType
--
--         , requestTerminateWorkflowExecution $
--             newTerminateWorkflowExecution
--
--         , requestRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeat
--
--         , requestGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistory
--
--         , requestDeprecateDomain $
--             newDeprecateDomain
--
--         , requestUndeprecateWorkflowType $
--             newUndeprecateWorkflowType
--
--         , requestRespondActivityTaskFailed $
--             newRespondActivityTaskFailed
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkflowTypes $
--             newListWorkflowTypes
--
--         , requestListClosedWorkflowExecutions $
--             newListClosedWorkflowExecutions
--
--           ]

--     , testGroup "response"
--         [ responseSignalWorkflowExecution $
--             newSignalWorkflowExecutionResponse
--
--         , responseDescribeWorkflowExecution $
--             newDescribeWorkflowExecutionResponse
--
--         , responsePollForActivityTask $
--             newPollForActivityTaskResponse
--
--         , responseRegisterActivityType $
--             newRegisterActivityTypeResponse
--
--         , responseListOpenWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompletedResponse
--
--         , responsePollForDecisionTask $
--             newPollForDecisionTaskResponse
--
--         , responseDeprecateWorkflowType $
--             newDeprecateWorkflowTypeResponse
--
--         , responseUndeprecateDomain $
--             newUndeprecateDomainResponse
--
--         , responseUndeprecateActivityType $
--             newUndeprecateActivityTypeResponse
--
--         , responseCountClosedWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeprecateActivityType $
--             newDeprecateActivityTypeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
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
--         , responseRespondActivityTaskCompleted $
--             newRespondActivityTaskCompletedResponse
--
--         , responseListActivityTypes $
--             newListActivityTypesResponse
--
--         , responseStartWorkflowExecution $
--             newStartWorkflowExecutionResponse
--
--         , responseDescribeWorkflowType $
--             newDescribeWorkflowTypeResponse
--
--         , responseRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceledResponse
--
--         , responseRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecutionResponse
--
--         , responseDescribeActivityType $
--             newDescribeActivityTypeResponse
--
--         , responseTerminateWorkflowExecution $
--             newTerminateWorkflowExecutionResponse
--
--         , responseRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeatResponse
--
--         , responseGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistoryResponse
--
--         , responseDeprecateDomain $
--             newDeprecateDomainResponse
--
--         , responseUndeprecateWorkflowType $
--             newUndeprecateWorkflowTypeResponse
--
--         , responseRespondActivityTaskFailed $
--             newRespondActivityTaskFailedResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkflowTypes $
--             newListWorkflowTypesResponse
--
--         , responseListClosedWorkflowExecutions $
--             newWorkflowExecutionInfos
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

requestPollForActivityTask :: PollForActivityTask -> TestTree
requestPollForActivityTask =
  req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

requestRegisterActivityType :: RegisterActivityType -> TestTree
requestRegisterActivityType =
  req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

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

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

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

requestDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
requestDeprecateWorkflowType =
  req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

requestUndeprecateDomain :: UndeprecateDomain -> TestTree
requestUndeprecateDomain =
  req
    "UndeprecateDomain"
    "fixture/UndeprecateDomain.yaml"

requestUndeprecateActivityType :: UndeprecateActivityType -> TestTree
requestUndeprecateActivityType =
  req
    "UndeprecateActivityType"
    "fixture/UndeprecateActivityType.yaml"

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

requestDeprecateActivityType :: DeprecateActivityType -> TestTree
requestDeprecateActivityType =
  req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
requestRespondActivityTaskCompleted =
  req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

requestListActivityTypes :: ListActivityTypes -> TestTree
requestListActivityTypes =
  req
    "ListActivityTypes"
    "fixture/ListActivityTypes.yaml"

requestStartWorkflowExecution :: StartWorkflowExecution -> TestTree
requestStartWorkflowExecution =
  req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

requestDescribeWorkflowType :: DescribeWorkflowType -> TestTree
requestDescribeWorkflowType =
  req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

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

requestDescribeActivityType :: DescribeActivityType -> TestTree
requestDescribeActivityType =
  req
    "DescribeActivityType"
    "fixture/DescribeActivityType.yaml"

requestTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
requestTerminateWorkflowExecution =
  req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution.yaml"

requestRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
requestRecordActivityTaskHeartbeat =
  req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

requestGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
requestGetWorkflowExecutionHistory =
  req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory.yaml"

requestDeprecateDomain :: DeprecateDomain -> TestTree
requestDeprecateDomain =
  req
    "DeprecateDomain"
    "fixture/DeprecateDomain.yaml"

requestUndeprecateWorkflowType :: UndeprecateWorkflowType -> TestTree
requestUndeprecateWorkflowType =
  req
    "UndeprecateWorkflowType"
    "fixture/UndeprecateWorkflowType.yaml"

requestRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
requestRespondActivityTaskFailed =
  req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

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

requestListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
requestListClosedWorkflowExecutions =
  req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

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

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask =
  res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    defaultService
    (Proxy :: Proxy PollForActivityTask)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType =
  res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterActivityType)

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

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

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

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType =
  res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateWorkflowType)

responseUndeprecateDomain :: UndeprecateDomainResponse -> TestTree
responseUndeprecateDomain =
  res
    "UndeprecateDomainResponse"
    "fixture/UndeprecateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateDomain)

responseUndeprecateActivityType :: UndeprecateActivityTypeResponse -> TestTree
responseUndeprecateActivityType =
  res
    "UndeprecateActivityTypeResponse"
    "fixture/UndeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateActivityType)

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

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType =
  res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateActivityType)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

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

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted =
  res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    defaultService
    (Proxy :: Proxy RespondActivityTaskCompleted)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes =
  res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListActivityTypes)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution =
  res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkflowExecution)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType =
  res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkflowType)

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

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType =
  res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivityType)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution =
  res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateWorkflowExecution)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat =
  res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory =
  res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowExecutionHistory)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain =
  res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeprecateDomain)

responseUndeprecateWorkflowType :: UndeprecateWorkflowTypeResponse -> TestTree
responseUndeprecateWorkflowType =
  res
    "UndeprecateWorkflowTypeResponse"
    "fixture/UndeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UndeprecateWorkflowType)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed =
  res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    defaultService
    (Proxy :: Proxy RespondActivityTaskFailed)

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

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions =
  res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListClosedWorkflowExecutions)
