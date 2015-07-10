{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SWF where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SWF
import Test.AWS.SWF.Internal

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
testListActivityTypes = req
    "ListActivityTypes"
    "fixture/ListActivityTypes"

testListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
testListOpenWorkflowExecutions = req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions"

testRegisterActivityType :: RegisterActivityType -> TestTree
testRegisterActivityType = req
    "RegisterActivityType"
    "fixture/RegisterActivityType"

testCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
testCountPendingActivityTasks = req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks"

testRegisterWorkflowType :: RegisterWorkflowType -> TestTree
testRegisterWorkflowType = req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType"

testRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
testRespondActivityTaskFailed = req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed"

testListWorkflowTypes :: ListWorkflowTypes -> TestTree
testListWorkflowTypes = req
    "ListWorkflowTypes"
    "fixture/ListWorkflowTypes"

testCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
testCountOpenWorkflowExecutions = req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions"

testDescribeWorkflowType :: DescribeWorkflowType -> TestTree
testDescribeWorkflowType = req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType"

testRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
testRequestCancelWorkflowExecution = req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution"

testDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
testDeprecateWorkflowType = req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType"

testRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
testRespondDecisionTaskCompleted = req
    "RespondDecisionTaskCompleted"
    "fixture/RespondDecisionTaskCompleted"

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = req
    "RegisterDomain"
    "fixture/RegisterDomain"

testDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
testDescribeWorkflowExecution = req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution"

testPollForActivityTask :: PollForActivityTask -> TestTree
testPollForActivityTask = req
    "PollForActivityTask"
    "fixture/PollForActivityTask"

testRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
testRespondActivityTaskCompleted = req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted"

testSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
testSignalWorkflowExecution = req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution"

testCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
testCountPendingDecisionTasks = req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks"

testListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
testListClosedWorkflowExecutions = req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions"

testRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
testRecordActivityTaskHeartbeat = req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat"

testDescribeDomain :: DescribeDomain -> TestTree
testDescribeDomain = req
    "DescribeDomain"
    "fixture/DescribeDomain"

testDeprecateDomain :: DeprecateDomain -> TestTree
testDeprecateDomain = req
    "DeprecateDomain"
    "fixture/DeprecateDomain"

testGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
testGetWorkflowExecutionHistory = req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory"

testDescribeActivityType :: DescribeActivityType -> TestTree
testDescribeActivityType = req
    "DescribeActivityType"
    "fixture/DescribeActivityType"

testDeprecateActivityType :: DeprecateActivityType -> TestTree
testDeprecateActivityType = req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType"

testTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
testTerminateWorkflowExecution = req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution"

testCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
testCountClosedWorkflowExecutions = req
    "CountClosedWorkflowExecutions"
    "fixture/CountClosedWorkflowExecutions"

testRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
testRespondActivityTaskCanceled = req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains"

testStartWorkflowExecution :: StartWorkflowExecution -> TestTree
testStartWorkflowExecution = req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution"

testPollForDecisionTask :: PollForDecisionTask -> TestTree
testPollForDecisionTask = req
    "PollForDecisionTask"
    "fixture/PollForDecisionTask"

-- Responses

testListActivityTypesResponse :: ListActivityTypesResponse -> TestTree
testListActivityTypesResponse = res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse"
    (Proxy :: Proxy ListActivityTypes)

testListOpenWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListOpenWorkflowExecutionsResponse = res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse"
    (Proxy :: Proxy ListOpenWorkflowExecutions)

testRegisterActivityTypeResponse :: RegisterActivityTypeResponse -> TestTree
testRegisterActivityTypeResponse = res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse"
    (Proxy :: Proxy RegisterActivityType)

testCountPendingActivityTasksResponse :: PendingTaskCount -> TestTree
testCountPendingActivityTasksResponse = res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse"
    (Proxy :: Proxy CountPendingActivityTasks)

testRegisterWorkflowTypeResponse :: RegisterWorkflowTypeResponse -> TestTree
testRegisterWorkflowTypeResponse = res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse"
    (Proxy :: Proxy RegisterWorkflowType)

testRespondActivityTaskFailedResponse :: RespondActivityTaskFailedResponse -> TestTree
testRespondActivityTaskFailedResponse = res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse"
    (Proxy :: Proxy RespondActivityTaskFailed)

testListWorkflowTypesResponse :: ListWorkflowTypesResponse -> TestTree
testListWorkflowTypesResponse = res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse"
    (Proxy :: Proxy ListWorkflowTypes)

testCountOpenWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountOpenWorkflowExecutionsResponse = res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse"
    (Proxy :: Proxy CountOpenWorkflowExecutions)

testDescribeWorkflowTypeResponse :: DescribeWorkflowTypeResponse -> TestTree
testDescribeWorkflowTypeResponse = res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse"
    (Proxy :: Proxy DescribeWorkflowType)

testRequestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse -> TestTree
testRequestCancelWorkflowExecutionResponse = res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse"
    (Proxy :: Proxy RequestCancelWorkflowExecution)

testDeprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse -> TestTree
testDeprecateWorkflowTypeResponse = res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse"
    (Proxy :: Proxy DeprecateWorkflowType)

testRespondDecisionTaskCompletedResponse :: RespondDecisionTaskCompletedResponse -> TestTree
testRespondDecisionTaskCompletedResponse = res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse"
    (Proxy :: Proxy RespondDecisionTaskCompleted)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

testDescribeWorkflowExecutionResponse :: DescribeWorkflowExecutionResponse -> TestTree
testDescribeWorkflowExecutionResponse = res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse"
    (Proxy :: Proxy DescribeWorkflowExecution)

testPollForActivityTaskResponse :: PollForActivityTaskResponse -> TestTree
testPollForActivityTaskResponse = res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse"
    (Proxy :: Proxy PollForActivityTask)

testRespondActivityTaskCompletedResponse :: RespondActivityTaskCompletedResponse -> TestTree
testRespondActivityTaskCompletedResponse = res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse"
    (Proxy :: Proxy RespondActivityTaskCompleted)

testSignalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse -> TestTree
testSignalWorkflowExecutionResponse = res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse"
    (Proxy :: Proxy SignalWorkflowExecution)

testCountPendingDecisionTasksResponse :: PendingTaskCount -> TestTree
testCountPendingDecisionTasksResponse = res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse"
    (Proxy :: Proxy CountPendingDecisionTasks)

testListClosedWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListClosedWorkflowExecutionsResponse = res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse"
    (Proxy :: Proxy ListClosedWorkflowExecutions)

testRecordActivityTaskHeartbeatResponse :: RecordActivityTaskHeartbeatResponse -> TestTree
testRecordActivityTaskHeartbeatResponse = res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse"
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

testDescribeDomainResponse :: DescribeDomainResponse -> TestTree
testDescribeDomainResponse = res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse"
    (Proxy :: Proxy DescribeDomain)

testDeprecateDomainResponse :: DeprecateDomainResponse -> TestTree
testDeprecateDomainResponse = res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse"
    (Proxy :: Proxy DeprecateDomain)

testGetWorkflowExecutionHistoryResponse :: GetWorkflowExecutionHistoryResponse -> TestTree
testGetWorkflowExecutionHistoryResponse = res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse"
    (Proxy :: Proxy GetWorkflowExecutionHistory)

testDescribeActivityTypeResponse :: DescribeActivityTypeResponse -> TestTree
testDescribeActivityTypeResponse = res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse"
    (Proxy :: Proxy DescribeActivityType)

testDeprecateActivityTypeResponse :: DeprecateActivityTypeResponse -> TestTree
testDeprecateActivityTypeResponse = res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse"
    (Proxy :: Proxy DeprecateActivityType)

testTerminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse -> TestTree
testTerminateWorkflowExecutionResponse = res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse"
    (Proxy :: Proxy TerminateWorkflowExecution)

testCountClosedWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountClosedWorkflowExecutionsResponse = res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse"
    (Proxy :: Proxy CountClosedWorkflowExecutions)

testRespondActivityTaskCanceledResponse :: RespondActivityTaskCanceledResponse -> TestTree
testRespondActivityTaskCanceledResponse = res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse"
    (Proxy :: Proxy RespondActivityTaskCanceled)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

testStartWorkflowExecutionResponse :: StartWorkflowExecutionResponse -> TestTree
testStartWorkflowExecutionResponse = res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse"
    (Proxy :: Proxy StartWorkflowExecution)

testPollForDecisionTaskResponse :: PollForDecisionTaskResponse -> TestTree
testPollForDecisionTaskResponse = res
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
