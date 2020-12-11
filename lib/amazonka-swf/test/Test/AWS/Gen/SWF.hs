{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestListOpenWorkflowExecutions $
--             mkListOpenWorkflowExecutions
--
--         , requestRegisterActivityType $
--             mkRegisterActivityType
--
--         , requestListActivityTypes $
--             mkListActivityTypes
--
--         , requestCountPendingActivityTasks $
--             mkCountPendingActivityTasks
--
--         , requestRegisterWorkflowType $
--             mkRegisterWorkflowType
--
--         , requestListWorkflowTypes $
--             mkListWorkflowTypes
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRespondActivityTaskFailed $
--             mkRespondActivityTaskFailed
--
--         , requestCountOpenWorkflowExecutions $
--             mkCountOpenWorkflowExecutions
--
--         , requestUndeprecateDomain $
--             mkUndeprecateDomain
--
--         , requestDescribeWorkflowType $
--             mkDescribeWorkflowType
--
--         , requestDeprecateWorkflowType $
--             mkDeprecateWorkflowType
--
--         , requestRequestCancelWorkflowExecution $
--             mkRequestCancelWorkflowExecution
--
--         , requestRegisterDomain $
--             mkRegisterDomain
--
--         , requestRespondDecisionTaskCompleted $
--             mkRespondDecisionTaskCompleted
--
--         , requestPollForActivityTask $
--             mkPollForActivityTask
--
--         , requestRespondActivityTaskCompleted $
--             mkRespondActivityTaskCompleted
--
--         , requestDescribeWorkflowExecution $
--             mkDescribeWorkflowExecution
--
--         , requestSignalWorkflowExecution $
--             mkSignalWorkflowExecution
--
--         , requestCountPendingDecisionTasks $
--             mkCountPendingDecisionTasks
--
--         , requestListClosedWorkflowExecutions $
--             mkListClosedWorkflowExecutions
--
--         , requestRecordActivityTaskHeartbeat $
--             mkRecordActivityTaskHeartbeat
--
--         , requestDescribeDomain $
--             mkDescribeDomain
--
--         , requestGetWorkflowExecutionHistory $
--             mkGetWorkflowExecutionHistory
--
--         , requestDeprecateDomain $
--             mkDeprecateDomain
--
--         , requestUndeprecateWorkflowType $
--             mkUndeprecateWorkflowType
--
--         , requestTerminateWorkflowExecution $
--             mkTerminateWorkflowExecution
--
--         , requestDescribeActivityType $
--             mkDescribeActivityType
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDeprecateActivityType $
--             mkDeprecateActivityType
--
--         , requestUndeprecateActivityType $
--             mkUndeprecateActivityType
--
--         , requestCountClosedWorkflowExecutions $
--             mkCountClosedWorkflowExecutions
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestRespondActivityTaskCanceled $
--             mkRespondActivityTaskCanceled
--
--         , requestStartWorkflowExecution $
--             mkStartWorkflowExecution
--
--         , requestPollForDecisionTask $
--             mkPollForDecisionTask
--
--         , requestListDomains $
--             mkListDomains
--
--           ]

--     , testGroup "response"
--         [ responseListOpenWorkflowExecutions $
--             mkWorkflowExecutionInfos
--
--         , responseRegisterActivityType $
--             mkRegisterActivityTypeResponse
--
--         , responseListActivityTypes $
--             mkListActivityTypesResponse
--
--         , responseCountPendingActivityTasks $
--             mkPendingTaskCount
--
--         , responseRegisterWorkflowType $
--             mkRegisterWorkflowTypeResponse
--
--         , responseListWorkflowTypes $
--             mkListWorkflowTypesResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRespondActivityTaskFailed $
--             mkRespondActivityTaskFailedResponse
--
--         , responseCountOpenWorkflowExecutions $
--             mkWorkflowExecutionCount
--
--         , responseUndeprecateDomain $
--             mkUndeprecateDomainResponse
--
--         , responseDescribeWorkflowType $
--             mkDescribeWorkflowTypeResponse
--
--         , responseDeprecateWorkflowType $
--             mkDeprecateWorkflowTypeResponse
--
--         , responseRequestCancelWorkflowExecution $
--             mkRequestCancelWorkflowExecutionResponse
--
--         , responseRegisterDomain $
--             mkRegisterDomainResponse
--
--         , responseRespondDecisionTaskCompleted $
--             mkRespondDecisionTaskCompletedResponse
--
--         , responsePollForActivityTask $
--             mkPollForActivityTaskResponse
--
--         , responseRespondActivityTaskCompleted $
--             mkRespondActivityTaskCompletedResponse
--
--         , responseDescribeWorkflowExecution $
--             mkDescribeWorkflowExecutionResponse
--
--         , responseSignalWorkflowExecution $
--             mkSignalWorkflowExecutionResponse
--
--         , responseCountPendingDecisionTasks $
--             mkPendingTaskCount
--
--         , responseListClosedWorkflowExecutions $
--             mkWorkflowExecutionInfos
--
--         , responseRecordActivityTaskHeartbeat $
--             mkRecordActivityTaskHeartbeatResponse
--
--         , responseDescribeDomain $
--             mkDescribeDomainResponse
--
--         , responseGetWorkflowExecutionHistory $
--             mkGetWorkflowExecutionHistoryResponse
--
--         , responseDeprecateDomain $
--             mkDeprecateDomainResponse
--
--         , responseUndeprecateWorkflowType $
--             mkUndeprecateWorkflowTypeResponse
--
--         , responseTerminateWorkflowExecution $
--             mkTerminateWorkflowExecutionResponse
--
--         , responseDescribeActivityType $
--             mkDescribeActivityTypeResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDeprecateActivityType $
--             mkDeprecateActivityTypeResponse
--
--         , responseUndeprecateActivityType $
--             mkUndeprecateActivityTypeResponse
--
--         , responseCountClosedWorkflowExecutions $
--             mkWorkflowExecutionCount
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseRespondActivityTaskCanceled $
--             mkRespondActivityTaskCanceledResponse
--
--         , responseStartWorkflowExecution $
--             mkStartWorkflowExecutionResponse
--
--         , responsePollForDecisionTask $
--             mkPollForDecisionTaskResponse
--
--         , responseListDomains $
--             mkListDomainsResponse
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
    swfService
    (Proxy :: Proxy ListOpenWorkflowExecutions)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType =
  res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    swfService
    (Proxy :: Proxy RegisterActivityType)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes =
  res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    swfService
    (Proxy :: Proxy ListActivityTypes)

responseCountPendingActivityTasks :: PendingTaskCount -> TestTree
responseCountPendingActivityTasks =
  res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    swfService
    (Proxy :: Proxy CountPendingActivityTasks)

responseRegisterWorkflowType :: RegisterWorkflowTypeResponse -> TestTree
responseRegisterWorkflowType =
  res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    swfService
    (Proxy :: Proxy RegisterWorkflowType)

responseListWorkflowTypes :: ListWorkflowTypesResponse -> TestTree
responseListWorkflowTypes =
  res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    swfService
    (Proxy :: Proxy ListWorkflowTypes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    swfService
    (Proxy :: Proxy ListTagsForResource)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed =
  res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    swfService
    (Proxy :: Proxy RespondActivityTaskFailed)

responseCountOpenWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountOpenWorkflowExecutions =
  res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    swfService
    (Proxy :: Proxy CountOpenWorkflowExecutions)

responseUndeprecateDomain :: UndeprecateDomainResponse -> TestTree
responseUndeprecateDomain =
  res
    "UndeprecateDomainResponse"
    "fixture/UndeprecateDomainResponse.proto"
    swfService
    (Proxy :: Proxy UndeprecateDomain)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType =
  res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    swfService
    (Proxy :: Proxy DescribeWorkflowType)

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType =
  res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    swfService
    (Proxy :: Proxy DeprecateWorkflowType)

responseRequestCancelWorkflowExecution :: RequestCancelWorkflowExecutionResponse -> TestTree
responseRequestCancelWorkflowExecution =
  res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    swfService
    (Proxy :: Proxy RequestCancelWorkflowExecution)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    swfService
    (Proxy :: Proxy RegisterDomain)

responseRespondDecisionTaskCompleted :: RespondDecisionTaskCompletedResponse -> TestTree
responseRespondDecisionTaskCompleted =
  res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    swfService
    (Proxy :: Proxy RespondDecisionTaskCompleted)

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask =
  res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    swfService
    (Proxy :: Proxy PollForActivityTask)

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted =
  res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    swfService
    (Proxy :: Proxy RespondActivityTaskCompleted)

responseDescribeWorkflowExecution :: DescribeWorkflowExecutionResponse -> TestTree
responseDescribeWorkflowExecution =
  res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    swfService
    (Proxy :: Proxy DescribeWorkflowExecution)

responseSignalWorkflowExecution :: SignalWorkflowExecutionResponse -> TestTree
responseSignalWorkflowExecution =
  res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    swfService
    (Proxy :: Proxy SignalWorkflowExecution)

responseCountPendingDecisionTasks :: PendingTaskCount -> TestTree
responseCountPendingDecisionTasks =
  res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    swfService
    (Proxy :: Proxy CountPendingDecisionTasks)

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions =
  res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    swfService
    (Proxy :: Proxy ListClosedWorkflowExecutions)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat =
  res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    swfService
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    swfService
    (Proxy :: Proxy DescribeDomain)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory =
  res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    swfService
    (Proxy :: Proxy GetWorkflowExecutionHistory)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain =
  res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    swfService
    (Proxy :: Proxy DeprecateDomain)

responseUndeprecateWorkflowType :: UndeprecateWorkflowTypeResponse -> TestTree
responseUndeprecateWorkflowType =
  res
    "UndeprecateWorkflowTypeResponse"
    "fixture/UndeprecateWorkflowTypeResponse.proto"
    swfService
    (Proxy :: Proxy UndeprecateWorkflowType)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution =
  res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    swfService
    (Proxy :: Proxy TerminateWorkflowExecution)

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType =
  res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    swfService
    (Proxy :: Proxy DescribeActivityType)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    swfService
    (Proxy :: Proxy TagResource)

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType =
  res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    swfService
    (Proxy :: Proxy DeprecateActivityType)

responseUndeprecateActivityType :: UndeprecateActivityTypeResponse -> TestTree
responseUndeprecateActivityType =
  res
    "UndeprecateActivityTypeResponse"
    "fixture/UndeprecateActivityTypeResponse.proto"
    swfService
    (Proxy :: Proxy UndeprecateActivityType)

responseCountClosedWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountClosedWorkflowExecutions =
  res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    swfService
    (Proxy :: Proxy CountClosedWorkflowExecutions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    swfService
    (Proxy :: Proxy UntagResource)

responseRespondActivityTaskCanceled :: RespondActivityTaskCanceledResponse -> TestTree
responseRespondActivityTaskCanceled =
  res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    swfService
    (Proxy :: Proxy RespondActivityTaskCanceled)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution =
  res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    swfService
    (Proxy :: Proxy StartWorkflowExecution)

responsePollForDecisionTask :: PollForDecisionTaskResponse -> TestTree
responsePollForDecisionTask =
  res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    swfService
    (Proxy :: Proxy PollForDecisionTask)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    swfService
    (Proxy :: Proxy ListDomains)
