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

import Amazonka.SWF
import qualified Data.Proxy as Proxy
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
--         [ requestListOpenWorkflowExecutions $
--             newListOpenWorkflowExecutions
--
--         , requestRegisterActivityType $
--             newRegisterActivityType
--
--         , requestListActivityTypes $
--             newListActivityTypes
--
--         , requestCountPendingActivityTasks $
--             newCountPendingActivityTasks
--
--         , requestRegisterWorkflowType $
--             newRegisterWorkflowType
--
--         , requestListWorkflowTypes $
--             newListWorkflowTypes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRespondActivityTaskFailed $
--             newRespondActivityTaskFailed
--
--         , requestCountOpenWorkflowExecutions $
--             newCountOpenWorkflowExecutions
--
--         , requestUndeprecateDomain $
--             newUndeprecateDomain
--
--         , requestDescribeWorkflowType $
--             newDescribeWorkflowType
--
--         , requestDeprecateWorkflowType $
--             newDeprecateWorkflowType
--
--         , requestRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecution
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompleted
--
--         , requestPollForActivityTask $
--             newPollForActivityTask
--
--         , requestRespondActivityTaskCompleted $
--             newRespondActivityTaskCompleted
--
--         , requestDescribeWorkflowExecution $
--             newDescribeWorkflowExecution
--
--         , requestSignalWorkflowExecution $
--             newSignalWorkflowExecution
--
--         , requestCountPendingDecisionTasks $
--             newCountPendingDecisionTasks
--
--         , requestListClosedWorkflowExecutions $
--             newListClosedWorkflowExecutions
--
--         , requestRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeat
--
--         , requestDescribeDomain $
--             newDescribeDomain
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
--         , requestTerminateWorkflowExecution $
--             newTerminateWorkflowExecution
--
--         , requestDescribeActivityType $
--             newDescribeActivityType
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeprecateActivityType $
--             newDeprecateActivityType
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
--         , requestRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceled
--
--         , requestStartWorkflowExecution $
--             newStartWorkflowExecution
--
--         , requestPollForDecisionTask $
--             newPollForDecisionTask
--
--         , requestListDomains $
--             newListDomains
--
--           ]

--     , testGroup "response"
--         [ responseListOpenWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseRegisterActivityType $
--             newRegisterActivityTypeResponse
--
--         , responseListActivityTypes $
--             newListActivityTypesResponse
--
--         , responseCountPendingActivityTasks $
--             newPendingTaskCount
--
--         , responseRegisterWorkflowType $
--             newRegisterWorkflowTypeResponse
--
--         , responseListWorkflowTypes $
--             newListWorkflowTypesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRespondActivityTaskFailed $
--             newRespondActivityTaskFailedResponse
--
--         , responseCountOpenWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseUndeprecateDomain $
--             newUndeprecateDomainResponse
--
--         , responseDescribeWorkflowType $
--             newDescribeWorkflowTypeResponse
--
--         , responseDeprecateWorkflowType $
--             newDeprecateWorkflowTypeResponse
--
--         , responseRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecutionResponse
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompletedResponse
--
--         , responsePollForActivityTask $
--             newPollForActivityTaskResponse
--
--         , responseRespondActivityTaskCompleted $
--             newRespondActivityTaskCompletedResponse
--
--         , responseDescribeWorkflowExecution $
--             newDescribeWorkflowExecutionResponse
--
--         , responseSignalWorkflowExecution $
--             newSignalWorkflowExecutionResponse
--
--         , responseCountPendingDecisionTasks $
--             newPendingTaskCount
--
--         , responseListClosedWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeatResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
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
--         , responseTerminateWorkflowExecution $
--             newTerminateWorkflowExecutionResponse
--
--         , responseDescribeActivityType $
--             newDescribeActivityTypeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeprecateActivityType $
--             newDeprecateActivityTypeResponse
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
--         , responseRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceledResponse
--
--         , responseStartWorkflowExecution $
--             newStartWorkflowExecutionResponse
--
--         , responsePollForDecisionTask $
--             newPollForDecisionTaskResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--           ]
--     ]

-- Requests

requestListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
requestListOpenWorkflowExecutions =
  req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions.yaml"

requestRegisterActivityType :: RegisterActivityType -> TestTree
requestRegisterActivityType =
  req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

requestListActivityTypes :: ListActivityTypes -> TestTree
requestListActivityTypes =
  req
    "ListActivityTypes"
    "fixture/ListActivityTypes.yaml"

requestCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
requestCountPendingActivityTasks =
  req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks.yaml"

requestRegisterWorkflowType :: RegisterWorkflowType -> TestTree
requestRegisterWorkflowType =
  req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType.yaml"

requestListWorkflowTypes :: ListWorkflowTypes -> TestTree
requestListWorkflowTypes =
  req
    "ListWorkflowTypes"
    "fixture/ListWorkflowTypes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
requestRespondActivityTaskFailed =
  req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

requestCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
requestCountOpenWorkflowExecutions =
  req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions.yaml"

requestUndeprecateDomain :: UndeprecateDomain -> TestTree
requestUndeprecateDomain =
  req
    "UndeprecateDomain"
    "fixture/UndeprecateDomain.yaml"

requestDescribeWorkflowType :: DescribeWorkflowType -> TestTree
requestDescribeWorkflowType =
  req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

requestDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
requestDeprecateWorkflowType =
  req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

requestRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
requestRequestCancelWorkflowExecution =
  req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution.yaml"

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

requestPollForActivityTask :: PollForActivityTask -> TestTree
requestPollForActivityTask =
  req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

requestRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
requestRespondActivityTaskCompleted =
  req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

requestDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
requestDescribeWorkflowExecution =
  req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution.yaml"

requestSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
requestSignalWorkflowExecution =
  req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution.yaml"

requestCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
requestCountPendingDecisionTasks =
  req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks.yaml"

requestListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
requestListClosedWorkflowExecutions =
  req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

requestRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
requestRecordActivityTaskHeartbeat =
  req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

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

requestRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
requestRespondActivityTaskCanceled =
  req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled.yaml"

requestStartWorkflowExecution :: StartWorkflowExecution -> TestTree
requestStartWorkflowExecution =
  req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

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

-- Responses

responseListOpenWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListOpenWorkflowExecutions =
  res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpenWorkflowExecutions)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType =
  res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterActivityType)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes =
  res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActivityTypes)

responseCountPendingActivityTasks :: PendingTaskCount -> TestTree
responseCountPendingActivityTasks =
  res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountPendingActivityTasks)

responseRegisterWorkflowType :: RegisterWorkflowTypeResponse -> TestTree
responseRegisterWorkflowType =
  res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterWorkflowType)

responseListWorkflowTypes :: ListWorkflowTypesResponse -> TestTree
responseListWorkflowTypes =
  res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowTypes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed =
  res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskFailed)

responseCountOpenWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountOpenWorkflowExecutions =
  res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountOpenWorkflowExecutions)

responseUndeprecateDomain :: UndeprecateDomainResponse -> TestTree
responseUndeprecateDomain =
  res
    "UndeprecateDomainResponse"
    "fixture/UndeprecateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateDomain)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType =
  res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflowType)

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType =
  res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateWorkflowType)

responseRequestCancelWorkflowExecution :: RequestCancelWorkflowExecutionResponse -> TestTree
responseRequestCancelWorkflowExecution =
  res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestCancelWorkflowExecution)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDomain)

responseRespondDecisionTaskCompleted :: RespondDecisionTaskCompletedResponse -> TestTree
responseRespondDecisionTaskCompleted =
  res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondDecisionTaskCompleted)

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask =
  res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForActivityTask)

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted =
  res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskCompleted)

responseDescribeWorkflowExecution :: DescribeWorkflowExecutionResponse -> TestTree
responseDescribeWorkflowExecution =
  res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflowExecution)

responseSignalWorkflowExecution :: SignalWorkflowExecutionResponse -> TestTree
responseSignalWorkflowExecution =
  res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignalWorkflowExecution)

responseCountPendingDecisionTasks :: PendingTaskCount -> TestTree
responseCountPendingDecisionTasks =
  res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountPendingDecisionTasks)

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions =
  res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClosedWorkflowExecutions)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat =
  res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordActivityTaskHeartbeat)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory =
  res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowExecutionHistory)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain =
  res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateDomain)

responseUndeprecateWorkflowType :: UndeprecateWorkflowTypeResponse -> TestTree
responseUndeprecateWorkflowType =
  res
    "UndeprecateWorkflowTypeResponse"
    "fixture/UndeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateWorkflowType)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution =
  res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateWorkflowExecution)

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType =
  res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivityType)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType =
  res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateActivityType)

responseUndeprecateActivityType :: UndeprecateActivityTypeResponse -> TestTree
responseUndeprecateActivityType =
  res
    "UndeprecateActivityTypeResponse"
    "fixture/UndeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateActivityType)

responseCountClosedWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountClosedWorkflowExecutions =
  res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountClosedWorkflowExecutions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseRespondActivityTaskCanceled :: RespondActivityTaskCanceledResponse -> TestTree
responseRespondActivityTaskCanceled =
  res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskCanceled)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution =
  res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkflowExecution)

responsePollForDecisionTask :: PollForDecisionTaskResponse -> TestTree
responsePollForDecisionTask =
  res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForDecisionTask)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)
