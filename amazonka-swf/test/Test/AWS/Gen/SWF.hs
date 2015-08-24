{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SWF
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    sWF
    (Proxy :: Proxy ListActivityTypes)

testListOpenWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListOpenWorkflowExecutionsResponse = res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse"
    sWF
    (Proxy :: Proxy ListOpenWorkflowExecutions)

testRegisterActivityTypeResponse :: RegisterActivityTypeResponse -> TestTree
testRegisterActivityTypeResponse = res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse"
    sWF
    (Proxy :: Proxy RegisterActivityType)

testCountPendingActivityTasksResponse :: PendingTaskCount -> TestTree
testCountPendingActivityTasksResponse = res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse"
    sWF
    (Proxy :: Proxy CountPendingActivityTasks)

testRegisterWorkflowTypeResponse :: RegisterWorkflowTypeResponse -> TestTree
testRegisterWorkflowTypeResponse = res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse"
    sWF
    (Proxy :: Proxy RegisterWorkflowType)

testRespondActivityTaskFailedResponse :: RespondActivityTaskFailedResponse -> TestTree
testRespondActivityTaskFailedResponse = res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse"
    sWF
    (Proxy :: Proxy RespondActivityTaskFailed)

testListWorkflowTypesResponse :: ListWorkflowTypesResponse -> TestTree
testListWorkflowTypesResponse = res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse"
    sWF
    (Proxy :: Proxy ListWorkflowTypes)

testCountOpenWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountOpenWorkflowExecutionsResponse = res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse"
    sWF
    (Proxy :: Proxy CountOpenWorkflowExecutions)

testDescribeWorkflowTypeResponse :: DescribeWorkflowTypeResponse -> TestTree
testDescribeWorkflowTypeResponse = res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse"
    sWF
    (Proxy :: Proxy DescribeWorkflowType)

testRequestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse -> TestTree
testRequestCancelWorkflowExecutionResponse = res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse"
    sWF
    (Proxy :: Proxy RequestCancelWorkflowExecution)

testDeprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse -> TestTree
testDeprecateWorkflowTypeResponse = res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse"
    sWF
    (Proxy :: Proxy DeprecateWorkflowType)

testRespondDecisionTaskCompletedResponse :: RespondDecisionTaskCompletedResponse -> TestTree
testRespondDecisionTaskCompletedResponse = res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse"
    sWF
    (Proxy :: Proxy RespondDecisionTaskCompleted)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse"
    sWF
    (Proxy :: Proxy RegisterDomain)

testDescribeWorkflowExecutionResponse :: DescribeWorkflowExecutionResponse -> TestTree
testDescribeWorkflowExecutionResponse = res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse"
    sWF
    (Proxy :: Proxy DescribeWorkflowExecution)

testPollForActivityTaskResponse :: PollForActivityTaskResponse -> TestTree
testPollForActivityTaskResponse = res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse"
    sWF
    (Proxy :: Proxy PollForActivityTask)

testRespondActivityTaskCompletedResponse :: RespondActivityTaskCompletedResponse -> TestTree
testRespondActivityTaskCompletedResponse = res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse"
    sWF
    (Proxy :: Proxy RespondActivityTaskCompleted)

testSignalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse -> TestTree
testSignalWorkflowExecutionResponse = res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse"
    sWF
    (Proxy :: Proxy SignalWorkflowExecution)

testCountPendingDecisionTasksResponse :: PendingTaskCount -> TestTree
testCountPendingDecisionTasksResponse = res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse"
    sWF
    (Proxy :: Proxy CountPendingDecisionTasks)

testListClosedWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListClosedWorkflowExecutionsResponse = res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse"
    sWF
    (Proxy :: Proxy ListClosedWorkflowExecutions)

testRecordActivityTaskHeartbeatResponse :: RecordActivityTaskHeartbeatResponse -> TestTree
testRecordActivityTaskHeartbeatResponse = res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse"
    sWF
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

testDescribeDomainResponse :: DescribeDomainResponse -> TestTree
testDescribeDomainResponse = res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse"
    sWF
    (Proxy :: Proxy DescribeDomain)

testDeprecateDomainResponse :: DeprecateDomainResponse -> TestTree
testDeprecateDomainResponse = res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse"
    sWF
    (Proxy :: Proxy DeprecateDomain)

testGetWorkflowExecutionHistoryResponse :: GetWorkflowExecutionHistoryResponse -> TestTree
testGetWorkflowExecutionHistoryResponse = res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse"
    sWF
    (Proxy :: Proxy GetWorkflowExecutionHistory)

testDescribeActivityTypeResponse :: DescribeActivityTypeResponse -> TestTree
testDescribeActivityTypeResponse = res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse"
    sWF
    (Proxy :: Proxy DescribeActivityType)

testDeprecateActivityTypeResponse :: DeprecateActivityTypeResponse -> TestTree
testDeprecateActivityTypeResponse = res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse"
    sWF
    (Proxy :: Proxy DeprecateActivityType)

testTerminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse -> TestTree
testTerminateWorkflowExecutionResponse = res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse"
    sWF
    (Proxy :: Proxy TerminateWorkflowExecution)

testCountClosedWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountClosedWorkflowExecutionsResponse = res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse"
    sWF
    (Proxy :: Proxy CountClosedWorkflowExecutions)

testRespondActivityTaskCanceledResponse :: RespondActivityTaskCanceledResponse -> TestTree
testRespondActivityTaskCanceledResponse = res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse"
    sWF
    (Proxy :: Proxy RespondActivityTaskCanceled)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    sWF
    (Proxy :: Proxy ListDomains)

testStartWorkflowExecutionResponse :: StartWorkflowExecutionResponse -> TestTree
testStartWorkflowExecutionResponse = res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse"
    sWF
    (Proxy :: Proxy StartWorkflowExecution)

testPollForDecisionTaskResponse :: PollForDecisionTaskResponse -> TestTree
testPollForDecisionTaskResponse = res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse"
    sWF
    (Proxy :: Proxy PollForDecisionTask)
