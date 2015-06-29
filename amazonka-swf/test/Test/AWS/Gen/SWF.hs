-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
