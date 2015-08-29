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
    "fixture/ListActivityTypes.yaml"

testListOpenWorkflowExecutions :: ListOpenWorkflowExecutions -> TestTree
testListOpenWorkflowExecutions = req
    "ListOpenWorkflowExecutions"
    "fixture/ListOpenWorkflowExecutions.yaml"

testRegisterActivityType :: RegisterActivityType -> TestTree
testRegisterActivityType = req
    "RegisterActivityType"
    "fixture/RegisterActivityType.yaml"

testCountPendingActivityTasks :: CountPendingActivityTasks -> TestTree
testCountPendingActivityTasks = req
    "CountPendingActivityTasks"
    "fixture/CountPendingActivityTasks.yaml"

testRegisterWorkflowType :: RegisterWorkflowType -> TestTree
testRegisterWorkflowType = req
    "RegisterWorkflowType"
    "fixture/RegisterWorkflowType.yaml"

testRespondActivityTaskFailed :: RespondActivityTaskFailed -> TestTree
testRespondActivityTaskFailed = req
    "RespondActivityTaskFailed"
    "fixture/RespondActivityTaskFailed.yaml"

testListWorkflowTypes :: ListWorkflowTypes -> TestTree
testListWorkflowTypes = req
    "ListWorkflowTypes"
    "fixture/ListWorkflowTypes.yaml"

testCountOpenWorkflowExecutions :: CountOpenWorkflowExecutions -> TestTree
testCountOpenWorkflowExecutions = req
    "CountOpenWorkflowExecutions"
    "fixture/CountOpenWorkflowExecutions.yaml"

testDescribeWorkflowType :: DescribeWorkflowType -> TestTree
testDescribeWorkflowType = req
    "DescribeWorkflowType"
    "fixture/DescribeWorkflowType.yaml"

testRequestCancelWorkflowExecution :: RequestCancelWorkflowExecution -> TestTree
testRequestCancelWorkflowExecution = req
    "RequestCancelWorkflowExecution"
    "fixture/RequestCancelWorkflowExecution.yaml"

testDeprecateWorkflowType :: DeprecateWorkflowType -> TestTree
testDeprecateWorkflowType = req
    "DeprecateWorkflowType"
    "fixture/DeprecateWorkflowType.yaml"

testRespondDecisionTaskCompleted :: RespondDecisionTaskCompleted -> TestTree
testRespondDecisionTaskCompleted = req
    "RespondDecisionTaskCompleted"
    "fixture/RespondDecisionTaskCompleted.yaml"

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

testDescribeWorkflowExecution :: DescribeWorkflowExecution -> TestTree
testDescribeWorkflowExecution = req
    "DescribeWorkflowExecution"
    "fixture/DescribeWorkflowExecution.yaml"

testPollForActivityTask :: PollForActivityTask -> TestTree
testPollForActivityTask = req
    "PollForActivityTask"
    "fixture/PollForActivityTask.yaml"

testRespondActivityTaskCompleted :: RespondActivityTaskCompleted -> TestTree
testRespondActivityTaskCompleted = req
    "RespondActivityTaskCompleted"
    "fixture/RespondActivityTaskCompleted.yaml"

testSignalWorkflowExecution :: SignalWorkflowExecution -> TestTree
testSignalWorkflowExecution = req
    "SignalWorkflowExecution"
    "fixture/SignalWorkflowExecution.yaml"

testCountPendingDecisionTasks :: CountPendingDecisionTasks -> TestTree
testCountPendingDecisionTasks = req
    "CountPendingDecisionTasks"
    "fixture/CountPendingDecisionTasks.yaml"

testListClosedWorkflowExecutions :: ListClosedWorkflowExecutions -> TestTree
testListClosedWorkflowExecutions = req
    "ListClosedWorkflowExecutions"
    "fixture/ListClosedWorkflowExecutions.yaml"

testRecordActivityTaskHeartbeat :: RecordActivityTaskHeartbeat -> TestTree
testRecordActivityTaskHeartbeat = req
    "RecordActivityTaskHeartbeat"
    "fixture/RecordActivityTaskHeartbeat.yaml"

testDescribeDomain :: DescribeDomain -> TestTree
testDescribeDomain = req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

testDeprecateDomain :: DeprecateDomain -> TestTree
testDeprecateDomain = req
    "DeprecateDomain"
    "fixture/DeprecateDomain.yaml"

testGetWorkflowExecutionHistory :: GetWorkflowExecutionHistory -> TestTree
testGetWorkflowExecutionHistory = req
    "GetWorkflowExecutionHistory"
    "fixture/GetWorkflowExecutionHistory.yaml"

testDescribeActivityType :: DescribeActivityType -> TestTree
testDescribeActivityType = req
    "DescribeActivityType"
    "fixture/DescribeActivityType.yaml"

testDeprecateActivityType :: DeprecateActivityType -> TestTree
testDeprecateActivityType = req
    "DeprecateActivityType"
    "fixture/DeprecateActivityType.yaml"

testTerminateWorkflowExecution :: TerminateWorkflowExecution -> TestTree
testTerminateWorkflowExecution = req
    "TerminateWorkflowExecution"
    "fixture/TerminateWorkflowExecution.yaml"

testCountClosedWorkflowExecutions :: CountClosedWorkflowExecutions -> TestTree
testCountClosedWorkflowExecutions = req
    "CountClosedWorkflowExecutions"
    "fixture/CountClosedWorkflowExecutions.yaml"

testRespondActivityTaskCanceled :: RespondActivityTaskCanceled -> TestTree
testRespondActivityTaskCanceled = req
    "RespondActivityTaskCanceled"
    "fixture/RespondActivityTaskCanceled.yaml"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

testStartWorkflowExecution :: StartWorkflowExecution -> TestTree
testStartWorkflowExecution = req
    "StartWorkflowExecution"
    "fixture/StartWorkflowExecution.yaml"

testPollForDecisionTask :: PollForDecisionTask -> TestTree
testPollForDecisionTask = req
    "PollForDecisionTask"
    "fixture/PollForDecisionTask.yaml"

-- Responses

testListActivityTypesResponse :: ListActivityTypesResponse -> TestTree
testListActivityTypesResponse = res
    "ListActivityTypesResponse"
    "fixture/ListActivityTypesResponse.proto"
    sWF
    (Proxy :: Proxy ListActivityTypes)

testListOpenWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListOpenWorkflowExecutionsResponse = res
    "ListOpenWorkflowExecutionsResponse"
    "fixture/ListOpenWorkflowExecutionsResponse.proto"
    sWF
    (Proxy :: Proxy ListOpenWorkflowExecutions)

testRegisterActivityTypeResponse :: RegisterActivityTypeResponse -> TestTree
testRegisterActivityTypeResponse = res
    "RegisterActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse.proto"
    sWF
    (Proxy :: Proxy RegisterActivityType)

testCountPendingActivityTasksResponse :: PendingTaskCount -> TestTree
testCountPendingActivityTasksResponse = res
    "CountPendingActivityTasksResponse"
    "fixture/CountPendingActivityTasksResponse.proto"
    sWF
    (Proxy :: Proxy CountPendingActivityTasks)

testRegisterWorkflowTypeResponse :: RegisterWorkflowTypeResponse -> TestTree
testRegisterWorkflowTypeResponse = res
    "RegisterWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse.proto"
    sWF
    (Proxy :: Proxy RegisterWorkflowType)

testRespondActivityTaskFailedResponse :: RespondActivityTaskFailedResponse -> TestTree
testRespondActivityTaskFailedResponse = res
    "RespondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse.proto"
    sWF
    (Proxy :: Proxy RespondActivityTaskFailed)

testListWorkflowTypesResponse :: ListWorkflowTypesResponse -> TestTree
testListWorkflowTypesResponse = res
    "ListWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse.proto"
    sWF
    (Proxy :: Proxy ListWorkflowTypes)

testCountOpenWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountOpenWorkflowExecutionsResponse = res
    "CountOpenWorkflowExecutionsResponse"
    "fixture/CountOpenWorkflowExecutionsResponse.proto"
    sWF
    (Proxy :: Proxy CountOpenWorkflowExecutions)

testDescribeWorkflowTypeResponse :: DescribeWorkflowTypeResponse -> TestTree
testDescribeWorkflowTypeResponse = res
    "DescribeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse.proto"
    sWF
    (Proxy :: Proxy DescribeWorkflowType)

testRequestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse -> TestTree
testRequestCancelWorkflowExecutionResponse = res
    "RequestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse.proto"
    sWF
    (Proxy :: Proxy RequestCancelWorkflowExecution)

testDeprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse -> TestTree
testDeprecateWorkflowTypeResponse = res
    "DeprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse.proto"
    sWF
    (Proxy :: Proxy DeprecateWorkflowType)

testRespondDecisionTaskCompletedResponse :: RespondDecisionTaskCompletedResponse -> TestTree
testRespondDecisionTaskCompletedResponse = res
    "RespondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse.proto"
    sWF
    (Proxy :: Proxy RespondDecisionTaskCompleted)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    sWF
    (Proxy :: Proxy RegisterDomain)

testDescribeWorkflowExecutionResponse :: DescribeWorkflowExecutionResponse -> TestTree
testDescribeWorkflowExecutionResponse = res
    "DescribeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse.proto"
    sWF
    (Proxy :: Proxy DescribeWorkflowExecution)

testPollForActivityTaskResponse :: PollForActivityTaskResponse -> TestTree
testPollForActivityTaskResponse = res
    "PollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse.proto"
    sWF
    (Proxy :: Proxy PollForActivityTask)

testRespondActivityTaskCompletedResponse :: RespondActivityTaskCompletedResponse -> TestTree
testRespondActivityTaskCompletedResponse = res
    "RespondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse.proto"
    sWF
    (Proxy :: Proxy RespondActivityTaskCompleted)

testSignalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse -> TestTree
testSignalWorkflowExecutionResponse = res
    "SignalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse.proto"
    sWF
    (Proxy :: Proxy SignalWorkflowExecution)

testCountPendingDecisionTasksResponse :: PendingTaskCount -> TestTree
testCountPendingDecisionTasksResponse = res
    "CountPendingDecisionTasksResponse"
    "fixture/CountPendingDecisionTasksResponse.proto"
    sWF
    (Proxy :: Proxy CountPendingDecisionTasks)

testListClosedWorkflowExecutionsResponse :: WorkflowExecutionInfos -> TestTree
testListClosedWorkflowExecutionsResponse = res
    "ListClosedWorkflowExecutionsResponse"
    "fixture/ListClosedWorkflowExecutionsResponse.proto"
    sWF
    (Proxy :: Proxy ListClosedWorkflowExecutions)

testRecordActivityTaskHeartbeatResponse :: RecordActivityTaskHeartbeatResponse -> TestTree
testRecordActivityTaskHeartbeatResponse = res
    "RecordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse.proto"
    sWF
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

testDescribeDomainResponse :: DescribeDomainResponse -> TestTree
testDescribeDomainResponse = res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    sWF
    (Proxy :: Proxy DescribeDomain)

testDeprecateDomainResponse :: DeprecateDomainResponse -> TestTree
testDeprecateDomainResponse = res
    "DeprecateDomainResponse"
    "fixture/DeprecateDomainResponse.proto"
    sWF
    (Proxy :: Proxy DeprecateDomain)

testGetWorkflowExecutionHistoryResponse :: GetWorkflowExecutionHistoryResponse -> TestTree
testGetWorkflowExecutionHistoryResponse = res
    "GetWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse.proto"
    sWF
    (Proxy :: Proxy GetWorkflowExecutionHistory)

testDescribeActivityTypeResponse :: DescribeActivityTypeResponse -> TestTree
testDescribeActivityTypeResponse = res
    "DescribeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse.proto"
    sWF
    (Proxy :: Proxy DescribeActivityType)

testDeprecateActivityTypeResponse :: DeprecateActivityTypeResponse -> TestTree
testDeprecateActivityTypeResponse = res
    "DeprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse.proto"
    sWF
    (Proxy :: Proxy DeprecateActivityType)

testTerminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse -> TestTree
testTerminateWorkflowExecutionResponse = res
    "TerminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse.proto"
    sWF
    (Proxy :: Proxy TerminateWorkflowExecution)

testCountClosedWorkflowExecutionsResponse :: WorkflowExecutionCount -> TestTree
testCountClosedWorkflowExecutionsResponse = res
    "CountClosedWorkflowExecutionsResponse"
    "fixture/CountClosedWorkflowExecutionsResponse.proto"
    sWF
    (Proxy :: Proxy CountClosedWorkflowExecutions)

testRespondActivityTaskCanceledResponse :: RespondActivityTaskCanceledResponse -> TestTree
testRespondActivityTaskCanceledResponse = res
    "RespondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse.proto"
    sWF
    (Proxy :: Proxy RespondActivityTaskCanceled)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sWF
    (Proxy :: Proxy ListDomains)

testStartWorkflowExecutionResponse :: StartWorkflowExecutionResponse -> TestTree
testStartWorkflowExecutionResponse = res
    "StartWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse.proto"
    sWF
    (Proxy :: Proxy StartWorkflowExecution)

testPollForDecisionTaskResponse :: PollForDecisionTaskResponse -> TestTree
testPollForDecisionTaskResponse = res
    "PollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse.proto"
    sWF
    (Proxy :: Proxy PollForDecisionTask)
