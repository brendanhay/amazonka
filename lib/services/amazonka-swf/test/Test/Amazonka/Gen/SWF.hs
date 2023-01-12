{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SWF
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SWF where

import Amazonka.SWF
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SWF.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCountClosedWorkflowExecutions $
--             newCountClosedWorkflowExecutions
--
--         , requestCountOpenWorkflowExecutions $
--             newCountOpenWorkflowExecutions
--
--         , requestCountPendingActivityTasks $
--             newCountPendingActivityTasks
--
--         , requestCountPendingDecisionTasks $
--             newCountPendingDecisionTasks
--
--         , requestDeprecateActivityType $
--             newDeprecateActivityType
--
--         , requestDeprecateDomain $
--             newDeprecateDomain
--
--         , requestDeprecateWorkflowType $
--             newDeprecateWorkflowType
--
--         , requestDescribeActivityType $
--             newDescribeActivityType
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribeWorkflowExecution $
--             newDescribeWorkflowExecution
--
--         , requestDescribeWorkflowType $
--             newDescribeWorkflowType
--
--         , requestGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistory
--
--         , requestListActivityTypes $
--             newListActivityTypes
--
--         , requestListClosedWorkflowExecutions $
--             newListClosedWorkflowExecutions
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListOpenWorkflowExecutions $
--             newListOpenWorkflowExecutions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkflowTypes $
--             newListWorkflowTypes
--
--         , requestPollForActivityTask $
--             newPollForActivityTask
--
--         , requestPollForDecisionTask $
--             newPollForDecisionTask
--
--         , requestRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeat
--
--         , requestRegisterActivityType $
--             newRegisterActivityType
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestRegisterWorkflowType $
--             newRegisterWorkflowType
--
--         , requestRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecution
--
--         , requestRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceled
--
--         , requestRespondActivityTaskCompleted $
--             newRespondActivityTaskCompleted
--
--         , requestRespondActivityTaskFailed $
--             newRespondActivityTaskFailed
--
--         , requestRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompleted
--
--         , requestSignalWorkflowExecution $
--             newSignalWorkflowExecution
--
--         , requestStartWorkflowExecution $
--             newStartWorkflowExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateWorkflowExecution $
--             newTerminateWorkflowExecution
--
--         , requestUndeprecateActivityType $
--             newUndeprecateActivityType
--
--         , requestUndeprecateDomain $
--             newUndeprecateDomain
--
--         , requestUndeprecateWorkflowType $
--             newUndeprecateWorkflowType
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCountClosedWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseCountOpenWorkflowExecutions $
--             newWorkflowExecutionCount
--
--         , responseCountPendingActivityTasks $
--             newPendingTaskCount
--
--         , responseCountPendingDecisionTasks $
--             newPendingTaskCount
--
--         , responseDeprecateActivityType $
--             newDeprecateActivityTypeResponse
--
--         , responseDeprecateDomain $
--             newDeprecateDomainResponse
--
--         , responseDeprecateWorkflowType $
--             newDeprecateWorkflowTypeResponse
--
--         , responseDescribeActivityType $
--             newDescribeActivityTypeResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribeWorkflowExecution $
--             newDescribeWorkflowExecutionResponse
--
--         , responseDescribeWorkflowType $
--             newDescribeWorkflowTypeResponse
--
--         , responseGetWorkflowExecutionHistory $
--             newGetWorkflowExecutionHistoryResponse
--
--         , responseListActivityTypes $
--             newListActivityTypesResponse
--
--         , responseListClosedWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListOpenWorkflowExecutions $
--             newWorkflowExecutionInfos
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkflowTypes $
--             newListWorkflowTypesResponse
--
--         , responsePollForActivityTask $
--             newPollForActivityTaskResponse
--
--         , responsePollForDecisionTask $
--             newPollForDecisionTaskResponse
--
--         , responseRecordActivityTaskHeartbeat $
--             newRecordActivityTaskHeartbeatResponse
--
--         , responseRegisterActivityType $
--             newRegisterActivityTypeResponse
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseRegisterWorkflowType $
--             newRegisterWorkflowTypeResponse
--
--         , responseRequestCancelWorkflowExecution $
--             newRequestCancelWorkflowExecutionResponse
--
--         , responseRespondActivityTaskCanceled $
--             newRespondActivityTaskCanceledResponse
--
--         , responseRespondActivityTaskCompleted $
--             newRespondActivityTaskCompletedResponse
--
--         , responseRespondActivityTaskFailed $
--             newRespondActivityTaskFailedResponse
--
--         , responseRespondDecisionTaskCompleted $
--             newRespondDecisionTaskCompletedResponse
--
--         , responseSignalWorkflowExecution $
--             newSignalWorkflowExecutionResponse
--
--         , responseStartWorkflowExecution $
--             newStartWorkflowExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateWorkflowExecution $
--             newTerminateWorkflowExecutionResponse
--
--         , responseUndeprecateActivityType $
--             newUndeprecateActivityTypeResponse
--
--         , responseUndeprecateDomain $
--             newUndeprecateDomainResponse
--
--         , responseUndeprecateWorkflowType $
--             newUndeprecateWorkflowTypeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
requestCountClosedWorkflowExecutions =
  req
    "CountClosedWorkflowExecutions"
    "fixture/CountClosedWorkflowExecutions.yaml"

requestCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
requestCountOpenWorkflowExecutions =
  req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions.yaml"

requestCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
requestCountPendingActivityTasks =
  req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks.yaml"

requestCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
requestCountPendingDecisionTasks =
  req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks.yaml"

requestDeprecateActivityType :: DeprecateActivityType -> TestTree
requestDeprecateActivityType =
  req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType.yaml"

requestDeprecateDomain :: DeprecateDomain -> TestTree
requestDeprecateDomain =
  req
    "DeprecateDomain"
    "fixture/DeprecateDomain.yaml"

requestDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
requestDeprecateWorkflowType =
  req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

requestDescribeActivityType :: DescribeActivityType -> TestTree
requestDescribeActivityType =
  req
    "DescribeActivityType"
    "fixture/DescribeActivityType.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
requestDescribeWorkflowExecution =
  req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution.yaml"

requestDescribeWorkflowType :: DescribeWorkflowType -> TestTree
requestDescribeWorkflowType =
  req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

requestGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
requestGetWorkflowExecutionHistory =
  req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory.yaml"

requestListActivityTypes :: ListActivityTypes -> TestTree
requestListActivityTypes =
  req
    "ListActivityTypes"
    "fixture/ListActivityTypes.yaml"

requestListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
requestListClosedWorkflowExecutions =
  req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
requestListOpenWorkflowExecutions =
  req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions.yaml"

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

requestPollForActivityTask :: PollForActivityTask -> TestTree
requestPollForActivityTask =
  req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

requestPollForDecisionTask :: PollForDecisionTask -> TestTree
requestPollForDecisionTask =
  req
    "PollForDecisionTask"
    "fixture/PollForDecisionTask.yaml"

requestRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
requestRecordActivityTaskHeartbeat =
  req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

requestRegisterActivityType :: RegisterActivityType -> TestTree
requestRegisterActivityType =
  req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

requestRegisterDomain :: RegisterDomain -> TestTree
requestRegisterDomain =
  req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

requestRegisterWorkflowType :: RegisterWorkflowType -> TestTree
requestRegisterWorkflowType =
  req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType.yaml"

requestRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
requestRequestCancelWorkflowExecution =
  req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution.yaml"

requestRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
requestRespondActivityTaskCanceled =
  req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled.yaml"

requestRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
requestRespondActivityTaskCompleted =
  req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

requestRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
requestRespondActivityTaskFailed =
  req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

requestRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
requestRespondDecisionTaskCompleted =
  req
    "RespondDecisionTaskCompleted"
    "fixture/RespondDecisionTaskCompleted.yaml"

requestSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
requestSignalWorkflowExecution =
  req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution.yaml"

requestStartWorkflowExecution :: StartWorkflowExecution -> TestTree
requestStartWorkflowExecution =
  req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
requestTerminateWorkflowExecution =
  req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution.yaml"

requestUndeprecateActivityType :: UndeprecateActivityType -> TestTree
requestUndeprecateActivityType =
  req
    "UndeprecateActivityType"
    "fixture/UndeprecateActivityType.yaml"

requestUndeprecateDomain :: UndeprecateDomain -> TestTree
requestUndeprecateDomain =
  req
    "UndeprecateDomain"
    "fixture/UndeprecateDomain.yaml"

requestUndeprecateWorkflowType :: UndeprecateWorkflowType -> TestTree
requestUndeprecateWorkflowType =
  req
    "UndeprecateWorkflowType"
    "fixture/UndeprecateWorkflowType.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseCountClosedWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountClosedWorkflowExecutions =
  res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountClosedWorkflowExecutions)

responseCountOpenWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountOpenWorkflowExecutions =
  res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountOpenWorkflowExecutions)

responseCountPendingActivityTasks :: PendingTaskCount -> TestTree
responseCountPendingActivityTasks =
  res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountPendingActivityTasks)

responseCountPendingDecisionTasks :: PendingTaskCount -> TestTree
responseCountPendingDecisionTasks =
  res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CountPendingDecisionTasks)

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType =
  res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateActivityType)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain =
  res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateDomain)

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType =
  res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateWorkflowType)

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType =
  res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivityType)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribeWorkflowExecution :: DescribeWorkflowExecutionResponse -> TestTree
responseDescribeWorkflowExecution =
  res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflowExecution)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType =
  res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkflowType)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory =
  res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowExecutionHistory)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes =
  res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActivityTypes)

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions =
  res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClosedWorkflowExecutions)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListOpenWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListOpenWorkflowExecutions =
  res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpenWorkflowExecutions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkflowTypes :: ListWorkflowTypesResponse -> TestTree
responseListWorkflowTypes =
  res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowTypes)

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask =
  res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForActivityTask)

responsePollForDecisionTask :: PollForDecisionTaskResponse -> TestTree
responsePollForDecisionTask =
  res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForDecisionTask)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat =
  res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordActivityTaskHeartbeat)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType =
  res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterActivityType)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDomain)

responseRegisterWorkflowType :: RegisterWorkflowTypeResponse -> TestTree
responseRegisterWorkflowType =
  res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterWorkflowType)

responseRequestCancelWorkflowExecution :: RequestCancelWorkflowExecutionResponse -> TestTree
responseRequestCancelWorkflowExecution =
  res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestCancelWorkflowExecution)

responseRespondActivityTaskCanceled :: RespondActivityTaskCanceledResponse -> TestTree
responseRespondActivityTaskCanceled =
  res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskCanceled)

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted =
  res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskCompleted)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed =
  res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondActivityTaskFailed)

responseRespondDecisionTaskCompleted :: RespondDecisionTaskCompletedResponse -> TestTree
responseRespondDecisionTaskCompleted =
  res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondDecisionTaskCompleted)

responseSignalWorkflowExecution :: SignalWorkflowExecutionResponse -> TestTree
responseSignalWorkflowExecution =
  res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignalWorkflowExecution)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution =
  res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkflowExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution =
  res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateWorkflowExecution)

responseUndeprecateActivityType :: UndeprecateActivityTypeResponse -> TestTree
responseUndeprecateActivityType =
  res
    "UndeprecateActivityTypeResponse"
    "fixture/UndeprecateActivityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateActivityType)

responseUndeprecateDomain :: UndeprecateDomainResponse -> TestTree
responseUndeprecateDomain =
  res
    "UndeprecateDomainResponse"
    "fixture/UndeprecateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateDomain)

responseUndeprecateWorkflowType :: UndeprecateWorkflowTypeResponse -> TestTree
responseUndeprecateWorkflowType =
  res
    "UndeprecateWorkflowTypeResponse"
    "fixture/UndeprecateWorkflowTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeprecateWorkflowType)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
