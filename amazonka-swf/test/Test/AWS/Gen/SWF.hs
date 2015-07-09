{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.SWF where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SWF

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testListActivityTypes $
--             listActivityTypes
--
--         , testListOpenWorkflowExecutions $
--             listOpenWorkflowExecutions
--
--         , testRegisterActivityType $
--             registerActivityType
--
--         , testCountPendingActivityTasks $
--             countPendingActivityTasks
--
--         , testRegisterWorkflowType $
--             registerWorkflowType
--
--         , testRespondActivityTaskFailed $
--             respondActivityTaskFailed
--
--         , testListWorkflowTypes $
--             listWorkflowTypes
--
--         , testCountOpenWorkflowExecutions $
--             countOpenWorkflowExecutions
--
--         , testDescribeWorkflowType $
--             describeWorkflowType
--
--         , testRequestCancelWorkflowExecution $
--             requestCancelWorkflowExecution
--
--         , testDeprecateWorkflowType $
--             deprecateWorkflowType
--
--         , testRespondDecisionTaskCompleted $
--             respondDecisionTaskCompleted
--
--         , testRegisterDomain $
--             registerDomain
--
--         , testDescribeWorkflowExecution $
--             describeWorkflowExecution
--
--         , testPollForActivityTask $
--             pollForActivityTask
--
--         , testRespondActivityTaskCompleted $
--             respondActivityTaskCompleted
--
--         , testSignalWorkflowExecution $
--             signalWorkflowExecution
--
--         , testCountPendingDecisionTasks $
--             countPendingDecisionTasks
--
--         , testListClosedWorkflowExecutions $
--             listClosedWorkflowExecutions
--
--         , testRecordActivityTaskHeartbeat $
--             recordActivityTaskHeartbeat
--
--         , testDescribeDomain $
--             describeDomain
--
--         , testDeprecateDomain $
--             deprecateDomain
--
--         , testGetWorkflowExecutionHistory $
--             getWorkflowExecutionHistory
--
--         , testDescribeActivityType $
--             describeActivityType
--
--         , testDeprecateActivityType $
--             deprecateActivityType
--
--         , testTerminateWorkflowExecution $
--             terminateWorkflowExecution
--
--         , testCountClosedWorkflowExecutions $
--             countClosedWorkflowExecutions
--
--         , testRespondActivityTaskCanceled $
--             respondActivityTaskCanceled
--
--         , testListDomains $
--             listDomains
--
--         , testStartWorkflowExecution $
--             startWorkflowExecution
--
--         , testPollForDecisionTask $
--             pollForDecisionTask
--
--           ]

--     , testGroup "response"
--         [ testListActivityTypesResponse $
--             listActivityTypesResponse
--
--         , testListOpenWorkflowExecutionsResponse $
--             workflowExecutionInfos
--
--         , testRegisterActivityTypeResponse $
--             registerActivityTypeResponse
--
--         , testCountPendingActivityTasksResponse $
--             pendingTaskCount
--
--         , testRegisterWorkflowTypeResponse $
--             registerWorkflowTypeResponse
--
--         , testRespondActivityTaskFailedResponse $
--             respondActivityTaskFailedResponse
--
--         , testListWorkflowTypesResponse $
--             listWorkflowTypesResponse
--
--         , testCountOpenWorkflowExecutionsResponse $
--             workflowExecutionCount
--
--         , testDescribeWorkflowTypeResponse $
--             describeWorkflowTypeResponse
--
--         , testRequestCancelWorkflowExecutionResponse $
--             requestCancelWorkflowExecutionResponse
--
--         , testDeprecateWorkflowTypeResponse $
--             deprecateWorkflowTypeResponse
--
--         , testRespondDecisionTaskCompletedResponse $
--             respondDecisionTaskCompletedResponse
--
--         , testRegisterDomainResponse $
--             registerDomainResponse
--
--         , testDescribeWorkflowExecutionResponse $
--             describeWorkflowExecutionResponse
--
--         , testPollForActivityTaskResponse $
--             pollForActivityTaskResponse
--
--         , testRespondActivityTaskCompletedResponse $
--             respondActivityTaskCompletedResponse
--
--         , testSignalWorkflowExecutionResponse $
--             signalWorkflowExecutionResponse
--
--         , testCountPendingDecisionTasksResponse $
--             pendingTaskCount
--
--         , testListClosedWorkflowExecutionsResponse $
--             workflowExecutionInfos
--
--         , testRecordActivityTaskHeartbeatResponse $
--             recordActivityTaskHeartbeatResponse
--
--         , testDescribeDomainResponse $
--             describeDomainResponse
--
--         , testDeprecateDomainResponse $
--             deprecateDomainResponse
--
--         , testGetWorkflowExecutionHistoryResponse $
--             getWorkflowExecutionHistoryResponse
--
--         , testDescribeActivityTypeResponse $
--             describeActivityTypeResponse
--
--         , testDeprecateActivityTypeResponse $
--             deprecateActivityTypeResponse
--
--         , testTerminateWorkflowExecutionResponse $
--             terminateWorkflowExecutionResponse
--
--         , testCountClosedWorkflowExecutionsResponse $
--             workflowExecutionCount
--
--         , testRespondActivityTaskCanceledResponse $
--             respondActivityTaskCanceledResponse
--
--         , testListDomainsResponse $
--             listDomainsResponse
--
--         , testStartWorkflowExecutionResponse $
--             startWorkflowExecutionResponse
--
--         , testPollForDecisionTaskResponse $
--             pollForDecisionTaskResponse
--
--           ]
--     ]

-- Requests

testListActivityTypes :: ListActivityTypes -> TestTree
testListActivityTypes = undefined

testListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
testListOpenWorkflowExecutions = undefined

testRegisterActivityType :: RegisterActivityType -> TestTree
testRegisterActivityType = undefined

testCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
testCountPendingActivityTasks = undefined

testRegisterWorkflowType :: RegisterWorkflowType -> TestTree
testRegisterWorkflowType = undefined

testRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
testRespondActivityTaskFailed = undefined

testListWorkflowTypes :: ListWorkflowTypes -> TestTree
testListWorkflowTypes = undefined

testCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
testCountOpenWorkflowExecutions = undefined

testDescribeWorkflowType :: DescribeWorkflowType -> TestTree
testDescribeWorkflowType = undefined

testRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
testRequestCancelWorkflowExecution = undefined

testDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
testDeprecateWorkflowType = undefined

testRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
testRespondDecisionTaskCompleted = undefined

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = undefined

testDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
testDescribeWorkflowExecution = undefined

testPollForActivityTask :: PollForActivityTask -> TestTree
testPollForActivityTask = undefined

testRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
testRespondActivityTaskCompleted = undefined

testSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
testSignalWorkflowExecution = undefined

testCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
testCountPendingDecisionTasks = undefined

testListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
testListClosedWorkflowExecutions = undefined

testRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
testRecordActivityTaskHeartbeat = undefined

testDescribeDomain :: DescribeDomain -> TestTree
testDescribeDomain = undefined

testDeprecateDomain :: DeprecateDomain -> TestTree
testDeprecateDomain = undefined

testGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
testGetWorkflowExecutionHistory = undefined

testDescribeActivityType :: DescribeActivityType -> TestTree
testDescribeActivityType = undefined

testDeprecateActivityType :: DeprecateActivityType -> TestTree
testDeprecateActivityType = undefined

testTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
testTerminateWorkflowExecution = undefined

testCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
testCountClosedWorkflowExecutions = undefined

testRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
testRespondActivityTaskCanceled = undefined

testListDomains :: ListDomains -> TestTree
testListDomains = undefined

testStartWorkflowExecution :: StartWorkflowExecution -> TestTree
testStartWorkflowExecution = undefined

testPollForDecisionTask :: PollForDecisionTask -> TestTree
testPollForDecisionTask = undefined

-- Responses

testListActivityTypesResponse :: ListActivityTypesResponse -> TestTree
testListActivityTypesResponse = resp
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse"
    (Proxy :: Proxy ListActivityTypes)

testListOpenWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListOpenWorkflowExecutionsResponse = resp
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse"
    (Proxy :: Proxy ListOpenWorkflowExecutions)

testRegisterActivityTypeResponse :: RegisterActivityTypeResponse -> TestTree
testRegisterActivityTypeResponse = resp
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse"
    (Proxy :: Proxy RegisterActivityType)

testCountPendingActivityTasksResponse :: PendingTaskCount -> TestTree
testCountPendingActivityTasksResponse = resp
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse"
    (Proxy :: Proxy CountPendingActivityTasks)

testRegisterWorkflowTypeResponse :: RegisterWorkflowTypeResponse -> TestTree
testRegisterWorkflowTypeResponse = resp
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse"
    (Proxy :: Proxy RegisterWorkflowType)

testRespondActivityTaskFailedResponse :: RespondActivityTaskFailedResponse -> TestTree
testRespondActivityTaskFailedResponse = resp
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse"
    (Proxy :: Proxy RespondActivityTaskFailed)

testListWorkflowTypesResponse :: ListWorkflowTypesResponse -> TestTree
testListWorkflowTypesResponse = resp
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse"
    (Proxy :: Proxy ListWorkflowTypes)

testCountOpenWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountOpenWorkflowExecutionsResponse = resp
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse"
    (Proxy :: Proxy CountOpenWorkflowExecutions)

testDescribeWorkflowTypeResponse :: DescribeWorkflowTypeResponse -> TestTree
testDescribeWorkflowTypeResponse = resp
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse"
    (Proxy :: Proxy DescribeWorkflowType)

testRequestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse -> TestTree
testRequestCancelWorkflowExecutionResponse = resp
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse"
    (Proxy :: Proxy RequestCancelWorkflowExecution)

testDeprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse -> TestTree
testDeprecateWorkflowTypeResponse = resp
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse"
    (Proxy :: Proxy DeprecateWorkflowType)

testRespondDecisionTaskCompletedResponse :: RespondDecisionTaskCompletedResponse -> TestTree
testRespondDecisionTaskCompletedResponse = resp
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse"
    (Proxy :: Proxy RespondDecisionTaskCompleted)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = resp
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

testDescribeWorkflowExecutionResponse :: DescribeWorkflowExecutionResponse -> TestTree
testDescribeWorkflowExecutionResponse = resp
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse"
    (Proxy :: Proxy DescribeWorkflowExecution)

testPollForActivityTaskResponse :: PollForActivityTaskResponse -> TestTree
testPollForActivityTaskResponse = resp
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse"
    (Proxy :: Proxy PollForActivityTask)

testRespondActivityTaskCompletedResponse :: RespondActivityTaskCompletedResponse -> TestTree
testRespondActivityTaskCompletedResponse = resp
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse"
    (Proxy :: Proxy RespondActivityTaskCompleted)

testSignalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse -> TestTree
testSignalWorkflowExecutionResponse = resp
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse"
    (Proxy :: Proxy SignalWorkflowExecution)

testCountPendingDecisionTasksResponse :: PendingTaskCount -> TestTree
testCountPendingDecisionTasksResponse = resp
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse"
    (Proxy :: Proxy CountPendingDecisionTasks)

testListClosedWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListClosedWorkflowExecutionsResponse = resp
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse"
    (Proxy :: Proxy ListClosedWorkflowExecutions)

testRecordActivityTaskHeartbeatResponse :: RecordActivityTaskHeartbeatResponse -> TestTree
testRecordActivityTaskHeartbeatResponse = resp
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse"
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

testDescribeDomainResponse :: DescribeDomainResponse -> TestTree
testDescribeDomainResponse = resp
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse"
    (Proxy :: Proxy DescribeDomain)

testDeprecateDomainResponse :: DeprecateDomainResponse -> TestTree
testDeprecateDomainResponse = resp
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse"
    (Proxy :: Proxy DeprecateDomain)

testGetWorkflowExecutionHistoryResponse :: GetWorkflowExecutionHistoryResponse -> TestTree
testGetWorkflowExecutionHistoryResponse = resp
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse"
    (Proxy :: Proxy GetWorkflowExecutionHistory)

testDescribeActivityTypeResponse :: DescribeActivityTypeResponse -> TestTree
testDescribeActivityTypeResponse = resp
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse"
    (Proxy :: Proxy DescribeActivityType)

testDeprecateActivityTypeResponse :: DeprecateActivityTypeResponse -> TestTree
testDeprecateActivityTypeResponse = resp
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse"
    (Proxy :: Proxy DeprecateActivityType)

testTerminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse -> TestTree
testTerminateWorkflowExecutionResponse = resp
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse"
    (Proxy :: Proxy TerminateWorkflowExecution)

testCountClosedWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountClosedWorkflowExecutionsResponse = resp
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse"
    (Proxy :: Proxy CountClosedWorkflowExecutions)

testRespondActivityTaskCanceledResponse :: RespondActivityTaskCanceledResponse -> TestTree
testRespondActivityTaskCanceledResponse = resp
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse"
    (Proxy :: Proxy RespondActivityTaskCanceled)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = resp
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

testStartWorkflowExecutionResponse :: StartWorkflowExecutionResponse -> TestTree
testStartWorkflowExecutionResponse = resp
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse"
    (Proxy :: Proxy StartWorkflowExecution)

testPollForDecisionTaskResponse :: PollForDecisionTaskResponse -> TestTree
testPollForDecisionTaskResponse = resp
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse"
    (Proxy :: Proxy PollForDecisionTask)

instance Out ActivityTaskCancelRequestedEventAttributes
instance Out ActivityTaskCanceledEventAttributes
instance Out ActivityTaskCompletedEventAttributes
instance Out ActivityTaskFailedEventAttributes
instance Out ActivityTaskScheduledEventAttributes
instance Out ActivityTaskStartedEventAttributes
instance Out ActivityTaskTimedOutEventAttributes
instance Out ActivityTaskTimeoutType
instance Out ActivityType
instance Out ActivityTypeConfiguration
instance Out ActivityTypeInfo
instance Out CancelTimerDecisionAttributes
instance Out CancelTimerFailedCause
instance Out CancelTimerFailedEventAttributes
instance Out CancelWorkflowExecutionDecisionAttributes
instance Out CancelWorkflowExecutionFailedCause
instance Out CancelWorkflowExecutionFailedEventAttributes
instance Out ChildPolicy
instance Out ChildWorkflowExecutionCanceledEventAttributes
instance Out ChildWorkflowExecutionCompletedEventAttributes
instance Out ChildWorkflowExecutionFailedEventAttributes
instance Out ChildWorkflowExecutionStartedEventAttributes
instance Out ChildWorkflowExecutionTerminatedEventAttributes
instance Out ChildWorkflowExecutionTimedOutEventAttributes
instance Out CloseStatus
instance Out CloseStatusFilter
instance Out CompleteWorkflowExecutionDecisionAttributes
instance Out CompleteWorkflowExecutionFailedCause
instance Out CompleteWorkflowExecutionFailedEventAttributes
instance Out ContinueAsNewWorkflowExecutionDecisionAttributes
instance Out ContinueAsNewWorkflowExecutionFailedCause
instance Out ContinueAsNewWorkflowExecutionFailedEventAttributes
instance Out CountClosedWorkflowExecutions
instance Out CountOpenWorkflowExecutions
instance Out CountPendingActivityTasks
instance Out CountPendingDecisionTasks
instance Out Decision
instance Out DecisionTaskCompletedEventAttributes
instance Out DecisionTaskScheduledEventAttributes
instance Out DecisionTaskStartedEventAttributes
instance Out DecisionTaskTimedOutEventAttributes
instance Out DecisionTaskTimeoutType
instance Out DecisionType
instance Out DeprecateActivityType
instance Out DeprecateActivityTypeResponse
instance Out DeprecateDomain
instance Out DeprecateDomainResponse
instance Out DeprecateWorkflowType
instance Out DeprecateWorkflowTypeResponse
instance Out DescribeActivityType
instance Out DescribeActivityTypeResponse
instance Out DescribeDomain
instance Out DescribeDomainResponse
instance Out DescribeWorkflowExecution
instance Out DescribeWorkflowExecutionResponse
instance Out DescribeWorkflowType
instance Out DescribeWorkflowTypeResponse
instance Out DomainConfiguration
instance Out DomainInfo
instance Out EventType
instance Out ExecutionStatus
instance Out ExecutionTimeFilter
instance Out ExternalWorkflowExecutionCancelRequestedEventAttributes
instance Out ExternalWorkflowExecutionSignaledEventAttributes
instance Out FailWorkflowExecutionDecisionAttributes
instance Out FailWorkflowExecutionFailedCause
instance Out FailWorkflowExecutionFailedEventAttributes
instance Out GetWorkflowExecutionHistory
instance Out GetWorkflowExecutionHistoryResponse
instance Out HistoryEvent
instance Out ListActivityTypes
instance Out ListActivityTypesResponse
instance Out ListClosedWorkflowExecutions
instance Out ListDomains
instance Out ListDomainsResponse
instance Out ListOpenWorkflowExecutions
instance Out ListWorkflowTypes
instance Out ListWorkflowTypesResponse
instance Out MarkerRecordedEventAttributes
instance Out PendingTaskCount
instance Out PollForActivityTask
instance Out PollForActivityTaskResponse
instance Out PollForDecisionTask
instance Out PollForDecisionTaskResponse
instance Out RecordActivityTaskHeartbeat
instance Out RecordActivityTaskHeartbeatResponse
instance Out RecordMarkerDecisionAttributes
instance Out RecordMarkerFailedCause
instance Out RecordMarkerFailedEventAttributes
instance Out RegisterActivityType
instance Out RegisterActivityTypeResponse
instance Out RegisterDomain
instance Out RegisterDomainResponse
instance Out RegisterWorkflowType
instance Out RegisterWorkflowTypeResponse
instance Out RegistrationStatus
instance Out RequestCancelActivityTaskDecisionAttributes
instance Out RequestCancelActivityTaskFailedCause
instance Out RequestCancelActivityTaskFailedEventAttributes
instance Out RequestCancelExternalWorkflowExecutionDecisionAttributes
instance Out RequestCancelExternalWorkflowExecutionFailedCause
instance Out RequestCancelExternalWorkflowExecutionFailedEventAttributes
instance Out RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
instance Out RequestCancelWorkflowExecution
instance Out RequestCancelWorkflowExecutionResponse
instance Out RespondActivityTaskCanceled
instance Out RespondActivityTaskCanceledResponse
instance Out RespondActivityTaskCompleted
instance Out RespondActivityTaskCompletedResponse
instance Out RespondActivityTaskFailed
instance Out RespondActivityTaskFailedResponse
instance Out RespondDecisionTaskCompleted
instance Out RespondDecisionTaskCompletedResponse
instance Out ScheduleActivityTaskDecisionAttributes
instance Out ScheduleActivityTaskFailedCause
instance Out ScheduleActivityTaskFailedEventAttributes
instance Out SignalExternalWorkflowExecutionDecisionAttributes
instance Out SignalExternalWorkflowExecutionFailedCause
instance Out SignalExternalWorkflowExecutionFailedEventAttributes
instance Out SignalExternalWorkflowExecutionInitiatedEventAttributes
instance Out SignalWorkflowExecution
instance Out SignalWorkflowExecutionResponse
instance Out StartChildWorkflowExecutionDecisionAttributes
instance Out StartChildWorkflowExecutionFailedCause
instance Out StartChildWorkflowExecutionFailedEventAttributes
instance Out StartChildWorkflowExecutionInitiatedEventAttributes
instance Out StartTimerDecisionAttributes
instance Out StartTimerFailedCause
instance Out StartTimerFailedEventAttributes
instance Out StartWorkflowExecution
instance Out StartWorkflowExecutionResponse
instance Out TagFilter
instance Out TaskList
instance Out TerminateWorkflowExecution
instance Out TerminateWorkflowExecutionResponse
instance Out TimerCanceledEventAttributes
instance Out TimerFiredEventAttributes
instance Out TimerStartedEventAttributes
instance Out WorkflowExecution
instance Out WorkflowExecutionCancelRequestedCause
instance Out WorkflowExecutionCancelRequestedEventAttributes
instance Out WorkflowExecutionCanceledEventAttributes
instance Out WorkflowExecutionCompletedEventAttributes
instance Out WorkflowExecutionConfiguration
instance Out WorkflowExecutionContinuedAsNewEventAttributes
instance Out WorkflowExecutionCount
instance Out WorkflowExecutionFailedEventAttributes
instance Out WorkflowExecutionFilter
instance Out WorkflowExecutionInfo
instance Out WorkflowExecutionInfos
instance Out WorkflowExecutionOpenCounts
instance Out WorkflowExecutionSignaledEventAttributes
instance Out WorkflowExecutionStartedEventAttributes
instance Out WorkflowExecutionTerminatedCause
instance Out WorkflowExecutionTerminatedEventAttributes
instance Out WorkflowExecutionTimedOutEventAttributes
instance Out WorkflowExecutionTimeoutType
instance Out WorkflowType
instance Out WorkflowTypeConfiguration
instance Out WorkflowTypeFilter
instance Out WorkflowTypeInfo
