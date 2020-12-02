{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             listOpenWorkflowExecutions
--
--         , requestRegisterActivityType $
--             registerActivityType
--
--         , requestListActivityTypes $
--             listActivityTypes
--
--         , requestCountPendingActivityTasks $
--             countPendingActivityTasks
--
--         , requestRegisterWorkflowType $
--             registerWorkflowType
--
--         , requestListWorkflowTypes $
--             listWorkflowTypes
--
--         , requestRespondActivityTaskFailed $
--             respondActivityTaskFailed
--
--         , requestCountOpenWorkflowExecutions $
--             countOpenWorkflowExecutions
--
--         , requestDescribeWorkflowType $
--             describeWorkflowType
--
--         , requestDeprecateWorkflowType $
--             deprecateWorkflowType
--
--         , requestRequestCancelWorkflowExecution $
--             requestCancelWorkflowExecution
--
--         , requestRegisterDomain $
--             registerDomain
--
--         , requestRespondDecisionTaskCompleted $
--             respondDecisionTaskCompleted
--
--         , requestPollForActivityTask $
--             pollForActivityTask
--
--         , requestRespondActivityTaskCompleted $
--             respondActivityTaskCompleted
--
--         , requestDescribeWorkflowExecution $
--             describeWorkflowExecution
--
--         , requestSignalWorkflowExecution $
--             signalWorkflowExecution
--
--         , requestCountPendingDecisionTasks $
--             countPendingDecisionTasks
--
--         , requestListClosedWorkflowExecutions $
--             listClosedWorkflowExecutions
--
--         , requestRecordActivityTaskHeartbeat $
--             recordActivityTaskHeartbeat
--
--         , requestDescribeDomain $
--             describeDomain
--
--         , requestGetWorkflowExecutionHistory $
--             getWorkflowExecutionHistory
--
--         , requestDeprecateDomain $
--             deprecateDomain
--
--         , requestTerminateWorkflowExecution $
--             terminateWorkflowExecution
--
--         , requestDescribeActivityType $
--             describeActivityType
--
--         , requestDeprecateActivityType $
--             deprecateActivityType
--
--         , requestCountClosedWorkflowExecutions $
--             countClosedWorkflowExecutions
--
--         , requestRespondActivityTaskCanceled $
--             respondActivityTaskCanceled
--
--         , requestStartWorkflowExecution $
--             startWorkflowExecution
--
--         , requestPollForDecisionTask $
--             pollForDecisionTask
--
--         , requestListDomains $
--             listDomains
--
--           ]

--     , testGroup "response"
--         [ responseListOpenWorkflowExecutions $
--             workflowExecutionInfos
--
--         , responseRegisterActivityType $
--             registerActivityTypeResponse
--
--         , responseListActivityTypes $
--             listActivityTypesResponse
--
--         , responseCountPendingActivityTasks $
--             pendingTaskCount
--
--         , responseRegisterWorkflowType $
--             registerWorkflowTypeResponse
--
--         , responseListWorkflowTypes $
--             listWorkflowTypesResponse
--
--         , responseRespondActivityTaskFailed $
--             respondActivityTaskFailedResponse
--
--         , responseCountOpenWorkflowExecutions $
--             workflowExecutionCount
--
--         , responseDescribeWorkflowType $
--             describeWorkflowTypeResponse
--
--         , responseDeprecateWorkflowType $
--             deprecateWorkflowTypeResponse
--
--         , responseRequestCancelWorkflowExecution $
--             requestCancelWorkflowExecutionResponse
--
--         , responseRegisterDomain $
--             registerDomainResponse
--
--         , responseRespondDecisionTaskCompleted $
--             respondDecisionTaskCompletedResponse
--
--         , responsePollForActivityTask $
--             pollForActivityTaskResponse
--
--         , responseRespondActivityTaskCompleted $
--             respondActivityTaskCompletedResponse
--
--         , responseDescribeWorkflowExecution $
--             describeWorkflowExecutionResponse
--
--         , responseSignalWorkflowExecution $
--             signalWorkflowExecutionResponse
--
--         , responseCountPendingDecisionTasks $
--             pendingTaskCount
--
--         , responseListClosedWorkflowExecutions $
--             workflowExecutionInfos
--
--         , responseRecordActivityTaskHeartbeat $
--             recordActivityTaskHeartbeatResponse
--
--         , responseDescribeDomain $
--             describeDomainResponse
--
--         , responseGetWorkflowExecutionHistory $
--             getWorkflowExecutionHistoryResponse
--
--         , responseDeprecateDomain $
--             deprecateDomainResponse
--
--         , responseTerminateWorkflowExecution $
--             terminateWorkflowExecutionResponse
--
--         , responseDescribeActivityType $
--             describeActivityTypeResponse
--
--         , responseDeprecateActivityType $
--             deprecateActivityTypeResponse
--
--         , responseCountClosedWorkflowExecutions $
--             workflowExecutionCount
--
--         , responseRespondActivityTaskCanceled $
--             respondActivityTaskCanceledResponse
--
--         , responseStartWorkflowExecution $
--             startWorkflowExecutionResponse
--
--         , responsePollForDecisionTask $
--             pollForDecisionTaskResponse
--
--         , responseListDomains $
--             listDomainsResponse
--
--           ]
--     ]

-- Requests

requestListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
requestListOpenWorkflowExecutions = req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions.yaml"

requestRegisterActivityType :: RegisterActivityType -> TestTree
requestRegisterActivityType = req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

requestListActivityTypes :: ListActivityTypes -> TestTree
requestListActivityTypes = req
    "ListActivityTypes"
    "fixture/ListActivityTypes.yaml"

requestCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
requestCountPendingActivityTasks = req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks.yaml"

requestRegisterWorkflowType :: RegisterWorkflowType -> TestTree
requestRegisterWorkflowType = req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType.yaml"

requestListWorkflowTypes :: ListWorkflowTypes -> TestTree
requestListWorkflowTypes = req
    "ListWorkflowTypes"
    "fixture/ListWorkflowTypes.yaml"

requestRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
requestRespondActivityTaskFailed = req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

requestCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
requestCountOpenWorkflowExecutions = req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions.yaml"

requestDescribeWorkflowType :: DescribeWorkflowType -> TestTree
requestDescribeWorkflowType = req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

requestDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
requestDeprecateWorkflowType = req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

requestRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
requestRequestCancelWorkflowExecution = req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution.yaml"

requestRegisterDomain :: RegisterDomain -> TestTree
requestRegisterDomain = req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

requestRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
requestRespondDecisionTaskCompleted = req
    "RespondDecisionTaskCompleted"
    "fixture/RespondDecisionTaskCompleted.yaml"

requestPollForActivityTask :: PollForActivityTask -> TestTree
requestPollForActivityTask = req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

requestRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
requestRespondActivityTaskCompleted = req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

requestDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
requestDescribeWorkflowExecution = req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution.yaml"

requestSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
requestSignalWorkflowExecution = req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution.yaml"

requestCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
requestCountPendingDecisionTasks = req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks.yaml"

requestListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
requestListClosedWorkflowExecutions = req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

requestRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
requestRecordActivityTaskHeartbeat = req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain = req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
requestGetWorkflowExecutionHistory = req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory.yaml"

requestDeprecateDomain :: DeprecateDomain -> TestTree
requestDeprecateDomain = req
    "DeprecateDomain"
    "fixture/DeprecateDomain.yaml"

requestTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
requestTerminateWorkflowExecution = req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution.yaml"

requestDescribeActivityType :: DescribeActivityType -> TestTree
requestDescribeActivityType = req
    "DescribeActivityType"
    "fixture/DescribeActivityType.yaml"

requestDeprecateActivityType :: DeprecateActivityType -> TestTree
requestDeprecateActivityType = req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType.yaml"

requestCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
requestCountClosedWorkflowExecutions = req
    "CountClosedWorkflowExecutions"
    "fixture/CountClosedWorkflowExecutions.yaml"

requestRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
requestRespondActivityTaskCanceled = req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled.yaml"

requestStartWorkflowExecution :: StartWorkflowExecution -> TestTree
requestStartWorkflowExecution = req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

requestPollForDecisionTask :: PollForDecisionTask -> TestTree
requestPollForDecisionTask = req
    "PollForDecisionTask"
    "fixture/PollForDecisionTask.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

responseListOpenWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListOpenWorkflowExecutions = res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse.proto"
    swf
    (Proxy :: Proxy ListOpenWorkflowExecutions)

responseRegisterActivityType :: RegisterActivityTypeResponse -> TestTree
responseRegisterActivityType = res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    swf
    (Proxy :: Proxy RegisterActivityType)

responseListActivityTypes :: ListActivityTypesResponse -> TestTree
responseListActivityTypes = res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    swf
    (Proxy :: Proxy ListActivityTypes)

responseCountPendingActivityTasks :: PendingTaskCount -> TestTree
responseCountPendingActivityTasks = res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    swf
    (Proxy :: Proxy CountPendingActivityTasks)

responseRegisterWorkflowType :: RegisterWorkflowTypeResponse -> TestTree
responseRegisterWorkflowType = res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    swf
    (Proxy :: Proxy RegisterWorkflowType)

responseListWorkflowTypes :: ListWorkflowTypesResponse -> TestTree
responseListWorkflowTypes = res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    swf
    (Proxy :: Proxy ListWorkflowTypes)

responseRespondActivityTaskFailed :: RespondActivityTaskFailedResponse -> TestTree
responseRespondActivityTaskFailed = res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    swf
    (Proxy :: Proxy RespondActivityTaskFailed)

responseCountOpenWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountOpenWorkflowExecutions = res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    swf
    (Proxy :: Proxy CountOpenWorkflowExecutions)

responseDescribeWorkflowType :: DescribeWorkflowTypeResponse -> TestTree
responseDescribeWorkflowType = res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    swf
    (Proxy :: Proxy DescribeWorkflowType)

responseDeprecateWorkflowType :: DeprecateWorkflowTypeResponse -> TestTree
responseDeprecateWorkflowType = res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    swf
    (Proxy :: Proxy DeprecateWorkflowType)

responseRequestCancelWorkflowExecution :: RequestCancelWorkflowExecutionResponse -> TestTree
responseRequestCancelWorkflowExecution = res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    swf
    (Proxy :: Proxy RequestCancelWorkflowExecution)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    swf
    (Proxy :: Proxy RegisterDomain)

responseRespondDecisionTaskCompleted :: RespondDecisionTaskCompletedResponse -> TestTree
responseRespondDecisionTaskCompleted = res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    swf
    (Proxy :: Proxy RespondDecisionTaskCompleted)

responsePollForActivityTask :: PollForActivityTaskResponse -> TestTree
responsePollForActivityTask = res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    swf
    (Proxy :: Proxy PollForActivityTask)

responseRespondActivityTaskCompleted :: RespondActivityTaskCompletedResponse -> TestTree
responseRespondActivityTaskCompleted = res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    swf
    (Proxy :: Proxy RespondActivityTaskCompleted)

responseDescribeWorkflowExecution :: DescribeWorkflowExecutionResponse -> TestTree
responseDescribeWorkflowExecution = res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    swf
    (Proxy :: Proxy DescribeWorkflowExecution)

responseSignalWorkflowExecution :: SignalWorkflowExecutionResponse -> TestTree
responseSignalWorkflowExecution = res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    swf
    (Proxy :: Proxy SignalWorkflowExecution)

responseCountPendingDecisionTasks :: PendingTaskCount -> TestTree
responseCountPendingDecisionTasks = res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    swf
    (Proxy :: Proxy CountPendingDecisionTasks)

responseListClosedWorkflowExecutions :: WorkflowExecutionInfos -> TestTree
responseListClosedWorkflowExecutions = res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    swf
    (Proxy :: Proxy ListClosedWorkflowExecutions)

responseRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeatResponse -> TestTree
responseRecordActivityTaskHeartbeat = res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    swf
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain = res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    swf
    (Proxy :: Proxy DescribeDomain)

responseGetWorkflowExecutionHistory :: GetWorkflowExecutionHistoryResponse -> TestTree
responseGetWorkflowExecutionHistory = res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    swf
    (Proxy :: Proxy GetWorkflowExecutionHistory)

responseDeprecateDomain :: DeprecateDomainResponse -> TestTree
responseDeprecateDomain = res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    swf
    (Proxy :: Proxy DeprecateDomain)

responseTerminateWorkflowExecution :: TerminateWorkflowExecutionResponse -> TestTree
responseTerminateWorkflowExecution = res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    swf
    (Proxy :: Proxy TerminateWorkflowExecution)

responseDescribeActivityType :: DescribeActivityTypeResponse -> TestTree
responseDescribeActivityType = res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    swf
    (Proxy :: Proxy DescribeActivityType)

responseDeprecateActivityType :: DeprecateActivityTypeResponse -> TestTree
responseDeprecateActivityType = res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    swf
    (Proxy :: Proxy DeprecateActivityType)

responseCountClosedWorkflowExecutions :: WorkflowExecutionCount -> TestTree
responseCountClosedWorkflowExecutions = res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    swf
    (Proxy :: Proxy CountClosedWorkflowExecutions)

responseRespondActivityTaskCanceled :: RespondActivityTaskCanceledResponse -> TestTree
responseRespondActivityTaskCanceled = res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    swf
    (Proxy :: Proxy RespondActivityTaskCanceled)

responseStartWorkflowExecution :: StartWorkflowExecutionResponse -> TestTree
responseStartWorkflowExecution = res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    swf
    (Proxy :: Proxy StartWorkflowExecution)

responsePollForDecisionTask :: PollForDecisionTaskResponse -> TestTree
responsePollForDecisionTask = res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    swf
    (Proxy :: Proxy PollForDecisionTask)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    swf
    (Proxy :: Proxy ListDomains)
