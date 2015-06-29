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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ countClosedWorkflowExecutionsTest $
--             countClosedWorkflowExecutions
--
--         , countOpenWorkflowExecutionsTest $
--             countOpenWorkflowExecutions
--
--         , countPendingActivityTasksTest $
--             countPendingActivityTasks
--
--         , countPendingDecisionTasksTest $
--             countPendingDecisionTasks
--
--         , deprecateActivityTypeTest $
--             deprecateActivityType
--
--         , deprecateDomainTest $
--             deprecateDomain
--
--         , deprecateWorkflowTypeTest $
--             deprecateWorkflowType
--
--         , describeActivityTypeTest $
--             describeActivityType
--
--         , describeDomainTest $
--             describeDomain
--
--         , describeWorkflowExecutionTest $
--             describeWorkflowExecution
--
--         , describeWorkflowTypeTest $
--             describeWorkflowType
--
--         , getWorkflowExecutionHistoryTest $
--             getWorkflowExecutionHistory
--
--         , listActivityTypesTest $
--             listActivityTypes
--
--         , listClosedWorkflowExecutionsTest $
--             listClosedWorkflowExecutions
--
--         , listDomainsTest $
--             listDomains
--
--         , listOpenWorkflowExecutionsTest $
--             listOpenWorkflowExecutions
--
--         , listWorkflowTypesTest $
--             listWorkflowTypes
--
--         , pollForActivityTaskTest $
--             pollForActivityTask
--
--         , pollForDecisionTaskTest $
--             pollForDecisionTask
--
--         , recordActivityTaskHeartbeatTest $
--             recordActivityTaskHeartbeat
--
--         , registerActivityTypeTest $
--             registerActivityType
--
--         , registerDomainTest $
--             registerDomain
--
--         , registerWorkflowTypeTest $
--             registerWorkflowType
--
--         , requestCancelWorkflowExecutionTest $
--             requestCancelWorkflowExecution
--
--         , respondActivityTaskCanceledTest $
--             respondActivityTaskCanceled
--
--         , respondActivityTaskCompletedTest $
--             respondActivityTaskCompleted
--
--         , respondActivityTaskFailedTest $
--             respondActivityTaskFailed
--
--         , respondDecisionTaskCompletedTest $
--             respondDecisionTaskCompleted
--
--         , signalWorkflowExecutionTest $
--             signalWorkflowExecution
--
--         , startWorkflowExecutionTest $
--             startWorkflowExecution
--
--         , terminateWorkflowExecutionTest $
--             terminateWorkflowExecution
--
--           ]

--     , testGroup "response"
--         [ countClosedWorkflowExecutionsResponseTest $
--             workflowExecutionCount
--
--         , countOpenWorkflowExecutionsResponseTest $
--             workflowExecutionCount
--
--         , countPendingActivityTasksResponseTest $
--             pendingTaskCount
--
--         , countPendingDecisionTasksResponseTest $
--             pendingTaskCount
--
--         , deprecateActivityTypeResponseTest $
--             deprecateActivityTypeResponse
--
--         , deprecateDomainResponseTest $
--             deprecateDomainResponse
--
--         , deprecateWorkflowTypeResponseTest $
--             deprecateWorkflowTypeResponse
--
--         , describeActivityTypeResponseTest $
--             describeActivityTypeResponse
--
--         , describeDomainResponseTest $
--             describeDomainResponse
--
--         , describeWorkflowExecutionResponseTest $
--             describeWorkflowExecutionResponse
--
--         , describeWorkflowTypeResponseTest $
--             describeWorkflowTypeResponse
--
--         , getWorkflowExecutionHistoryResponseTest $
--             getWorkflowExecutionHistoryResponse
--
--         , listActivityTypesResponseTest $
--             listActivityTypesResponse
--
--         , listClosedWorkflowExecutionsResponseTest $
--             workflowExecutionInfos
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--         , listOpenWorkflowExecutionsResponseTest $
--             workflowExecutionInfos
--
--         , listWorkflowTypesResponseTest $
--             listWorkflowTypesResponse
--
--         , pollForActivityTaskResponseTest $
--             pollForActivityTaskResponse
--
--         , pollForDecisionTaskResponseTest $
--             pollForDecisionTaskResponse
--
--         , recordActivityTaskHeartbeatResponseTest $
--             recordActivityTaskHeartbeatResponse
--
--         , registerActivityTypeResponseTest $
--             registerActivityTypeResponse
--
--         , registerDomainResponseTest $
--             registerDomainResponse
--
--         , registerWorkflowTypeResponseTest $
--             registerWorkflowTypeResponse
--
--         , requestCancelWorkflowExecutionResponseTest $
--             requestCancelWorkflowExecutionResponse
--
--         , respondActivityTaskCanceledResponseTest $
--             respondActivityTaskCanceledResponse
--
--         , respondActivityTaskCompletedResponseTest $
--             respondActivityTaskCompletedResponse
--
--         , respondActivityTaskFailedResponseTest $
--             respondActivityTaskFailedResponse
--
--         , respondDecisionTaskCompletedResponseTest $
--             respondDecisionTaskCompletedResponse
--
--         , signalWorkflowExecutionResponseTest $
--             signalWorkflowExecutionResponse
--
--         , startWorkflowExecutionResponseTest $
--             startWorkflowExecutionResponse
--
--         , terminateWorkflowExecutionResponseTest $
--             terminateWorkflowExecutionResponse
--
--           ]
--     ]

-- Requests

countClosedWorkflowExecutionsTest :: CountClosedWorkflowExecutions -> TestTree
countClosedWorkflowExecutionsTest = undefined

countOpenWorkflowExecutionsTest :: CountOpenWorkflowExecutions -> TestTree
countOpenWorkflowExecutionsTest = undefined

countPendingActivityTasksTest :: CountPendingActivityTasks -> TestTree
countPendingActivityTasksTest = undefined

countPendingDecisionTasksTest :: CountPendingDecisionTasks -> TestTree
countPendingDecisionTasksTest = undefined

deprecateActivityTypeTest :: DeprecateActivityType -> TestTree
deprecateActivityTypeTest = undefined

deprecateDomainTest :: DeprecateDomain -> TestTree
deprecateDomainTest = undefined

deprecateWorkflowTypeTest :: DeprecateWorkflowType -> TestTree
deprecateWorkflowTypeTest = undefined

describeActivityTypeTest :: DescribeActivityType -> TestTree
describeActivityTypeTest = undefined

describeDomainTest :: DescribeDomain -> TestTree
describeDomainTest = undefined

describeWorkflowExecutionTest :: DescribeWorkflowExecution -> TestTree
describeWorkflowExecutionTest = undefined

describeWorkflowTypeTest :: DescribeWorkflowType -> TestTree
describeWorkflowTypeTest = undefined

getWorkflowExecutionHistoryTest :: GetWorkflowExecutionHistory -> TestTree
getWorkflowExecutionHistoryTest = undefined

listActivityTypesTest :: ListActivityTypes -> TestTree
listActivityTypesTest = undefined

listClosedWorkflowExecutionsTest :: ListClosedWorkflowExecutions -> TestTree
listClosedWorkflowExecutionsTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

listOpenWorkflowExecutionsTest :: ListOpenWorkflowExecutions -> TestTree
listOpenWorkflowExecutionsTest = undefined

listWorkflowTypesTest :: ListWorkflowTypes -> TestTree
listWorkflowTypesTest = undefined

pollForActivityTaskTest :: PollForActivityTask -> TestTree
pollForActivityTaskTest = undefined

pollForDecisionTaskTest :: PollForDecisionTask -> TestTree
pollForDecisionTaskTest = undefined

recordActivityTaskHeartbeatTest :: RecordActivityTaskHeartbeat -> TestTree
recordActivityTaskHeartbeatTest = undefined

registerActivityTypeTest :: RegisterActivityType -> TestTree
registerActivityTypeTest = undefined

registerDomainTest :: RegisterDomain -> TestTree
registerDomainTest = undefined

registerWorkflowTypeTest :: RegisterWorkflowType -> TestTree
registerWorkflowTypeTest = undefined

requestCancelWorkflowExecutionTest :: RequestCancelWorkflowExecution -> TestTree
requestCancelWorkflowExecutionTest = undefined

respondActivityTaskCanceledTest :: RespondActivityTaskCanceled -> TestTree
respondActivityTaskCanceledTest = undefined

respondActivityTaskCompletedTest :: RespondActivityTaskCompleted -> TestTree
respondActivityTaskCompletedTest = undefined

respondActivityTaskFailedTest :: RespondActivityTaskFailed -> TestTree
respondActivityTaskFailedTest = undefined

respondDecisionTaskCompletedTest :: RespondDecisionTaskCompleted -> TestTree
respondDecisionTaskCompletedTest = undefined

signalWorkflowExecutionTest :: SignalWorkflowExecution -> TestTree
signalWorkflowExecutionTest = undefined

startWorkflowExecutionTest :: StartWorkflowExecution -> TestTree
startWorkflowExecutionTest = undefined

terminateWorkflowExecutionTest :: TerminateWorkflowExecution -> TestTree
terminateWorkflowExecutionTest = undefined

-- Responses

countClosedWorkflowExecutionsResponseTest :: WorkflowExecutionCount -> TestTree
countClosedWorkflowExecutionsResponseTest = resp
    "countClosedWorkflowExecutionsResponse"
    "fixture/WorkflowExecutionCount"
    (Proxy :: Proxy CountClosedWorkflowExecutions)

countOpenWorkflowExecutionsResponseTest :: WorkflowExecutionCount -> TestTree
countOpenWorkflowExecutionsResponseTest = resp
    "countOpenWorkflowExecutionsResponse"
    "fixture/WorkflowExecutionCount"
    (Proxy :: Proxy CountOpenWorkflowExecutions)

countPendingActivityTasksResponseTest :: PendingTaskCount -> TestTree
countPendingActivityTasksResponseTest = resp
    "countPendingActivityTasksResponse"
    "fixture/PendingTaskCount"
    (Proxy :: Proxy CountPendingActivityTasks)

countPendingDecisionTasksResponseTest :: PendingTaskCount -> TestTree
countPendingDecisionTasksResponseTest = resp
    "countPendingDecisionTasksResponse"
    "fixture/PendingTaskCount"
    (Proxy :: Proxy CountPendingDecisionTasks)

deprecateActivityTypeResponseTest :: DeprecateActivityTypeResponse -> TestTree
deprecateActivityTypeResponseTest = resp
    "deprecateActivityTypeResponse"
    "fixture/DeprecateActivityTypeResponse"
    (Proxy :: Proxy DeprecateActivityType)

deprecateDomainResponseTest :: DeprecateDomainResponse -> TestTree
deprecateDomainResponseTest = resp
    "deprecateDomainResponse"
    "fixture/DeprecateDomainResponse"
    (Proxy :: Proxy DeprecateDomain)

deprecateWorkflowTypeResponseTest :: DeprecateWorkflowTypeResponse -> TestTree
deprecateWorkflowTypeResponseTest = resp
    "deprecateWorkflowTypeResponse"
    "fixture/DeprecateWorkflowTypeResponse"
    (Proxy :: Proxy DeprecateWorkflowType)

describeActivityTypeResponseTest :: DescribeActivityTypeResponse -> TestTree
describeActivityTypeResponseTest = resp
    "describeActivityTypeResponse"
    "fixture/DescribeActivityTypeResponse"
    (Proxy :: Proxy DescribeActivityType)

describeDomainResponseTest :: DescribeDomainResponse -> TestTree
describeDomainResponseTest = resp
    "describeDomainResponse"
    "fixture/DescribeDomainResponse"
    (Proxy :: Proxy DescribeDomain)

describeWorkflowExecutionResponseTest :: DescribeWorkflowExecutionResponse -> TestTree
describeWorkflowExecutionResponseTest = resp
    "describeWorkflowExecutionResponse"
    "fixture/DescribeWorkflowExecutionResponse"
    (Proxy :: Proxy DescribeWorkflowExecution)

describeWorkflowTypeResponseTest :: DescribeWorkflowTypeResponse -> TestTree
describeWorkflowTypeResponseTest = resp
    "describeWorkflowTypeResponse"
    "fixture/DescribeWorkflowTypeResponse"
    (Proxy :: Proxy DescribeWorkflowType)

getWorkflowExecutionHistoryResponseTest :: GetWorkflowExecutionHistoryResponse -> TestTree
getWorkflowExecutionHistoryResponseTest = resp
    "getWorkflowExecutionHistoryResponse"
    "fixture/GetWorkflowExecutionHistoryResponse"
    (Proxy :: Proxy GetWorkflowExecutionHistory)

listActivityTypesResponseTest :: ListActivityTypesResponse -> TestTree
listActivityTypesResponseTest = resp
    "listActivityTypesResponse"
    "fixture/ListActivityTypesResponse"
    (Proxy :: Proxy ListActivityTypes)

listClosedWorkflowExecutionsResponseTest :: WorkflowExecutionInfos -> TestTree
listClosedWorkflowExecutionsResponseTest = resp
    "listClosedWorkflowExecutionsResponse"
    "fixture/WorkflowExecutionInfos"
    (Proxy :: Proxy ListClosedWorkflowExecutions)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "listDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

listOpenWorkflowExecutionsResponseTest :: WorkflowExecutionInfos -> TestTree
listOpenWorkflowExecutionsResponseTest = resp
    "listOpenWorkflowExecutionsResponse"
    "fixture/WorkflowExecutionInfos"
    (Proxy :: Proxy ListOpenWorkflowExecutions)

listWorkflowTypesResponseTest :: ListWorkflowTypesResponse -> TestTree
listWorkflowTypesResponseTest = resp
    "listWorkflowTypesResponse"
    "fixture/ListWorkflowTypesResponse"
    (Proxy :: Proxy ListWorkflowTypes)

pollForActivityTaskResponseTest :: PollForActivityTaskResponse -> TestTree
pollForActivityTaskResponseTest = resp
    "pollForActivityTaskResponse"
    "fixture/PollForActivityTaskResponse"
    (Proxy :: Proxy PollForActivityTask)

pollForDecisionTaskResponseTest :: PollForDecisionTaskResponse -> TestTree
pollForDecisionTaskResponseTest = resp
    "pollForDecisionTaskResponse"
    "fixture/PollForDecisionTaskResponse"
    (Proxy :: Proxy PollForDecisionTask)

recordActivityTaskHeartbeatResponseTest :: RecordActivityTaskHeartbeatResponse -> TestTree
recordActivityTaskHeartbeatResponseTest = resp
    "recordActivityTaskHeartbeatResponse"
    "fixture/RecordActivityTaskHeartbeatResponse"
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

registerActivityTypeResponseTest :: RegisterActivityTypeResponse -> TestTree
registerActivityTypeResponseTest = resp
    "registerActivityTypeResponse"
    "fixture/RegisterActivityTypeResponse"
    (Proxy :: Proxy RegisterActivityType)

registerDomainResponseTest :: RegisterDomainResponse -> TestTree
registerDomainResponseTest = resp
    "registerDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

registerWorkflowTypeResponseTest :: RegisterWorkflowTypeResponse -> TestTree
registerWorkflowTypeResponseTest = resp
    "registerWorkflowTypeResponse"
    "fixture/RegisterWorkflowTypeResponse"
    (Proxy :: Proxy RegisterWorkflowType)

requestCancelWorkflowExecutionResponseTest :: RequestCancelWorkflowExecutionResponse -> TestTree
requestCancelWorkflowExecutionResponseTest = resp
    "requestCancelWorkflowExecutionResponse"
    "fixture/RequestCancelWorkflowExecutionResponse"
    (Proxy :: Proxy RequestCancelWorkflowExecution)

respondActivityTaskCanceledResponseTest :: RespondActivityTaskCanceledResponse -> TestTree
respondActivityTaskCanceledResponseTest = resp
    "respondActivityTaskCanceledResponse"
    "fixture/RespondActivityTaskCanceledResponse"
    (Proxy :: Proxy RespondActivityTaskCanceled)

respondActivityTaskCompletedResponseTest :: RespondActivityTaskCompletedResponse -> TestTree
respondActivityTaskCompletedResponseTest = resp
    "respondActivityTaskCompletedResponse"
    "fixture/RespondActivityTaskCompletedResponse"
    (Proxy :: Proxy RespondActivityTaskCompleted)

respondActivityTaskFailedResponseTest :: RespondActivityTaskFailedResponse -> TestTree
respondActivityTaskFailedResponseTest = resp
    "respondActivityTaskFailedResponse"
    "fixture/RespondActivityTaskFailedResponse"
    (Proxy :: Proxy RespondActivityTaskFailed)

respondDecisionTaskCompletedResponseTest :: RespondDecisionTaskCompletedResponse -> TestTree
respondDecisionTaskCompletedResponseTest = resp
    "respondDecisionTaskCompletedResponse"
    "fixture/RespondDecisionTaskCompletedResponse"
    (Proxy :: Proxy RespondDecisionTaskCompleted)

signalWorkflowExecutionResponseTest :: SignalWorkflowExecutionResponse -> TestTree
signalWorkflowExecutionResponseTest = resp
    "signalWorkflowExecutionResponse"
    "fixture/SignalWorkflowExecutionResponse"
    (Proxy :: Proxy SignalWorkflowExecution)

startWorkflowExecutionResponseTest :: StartWorkflowExecutionResponse -> TestTree
startWorkflowExecutionResponseTest = resp
    "startWorkflowExecutionResponse"
    "fixture/StartWorkflowExecutionResponse"
    (Proxy :: Proxy StartWorkflowExecution)

terminateWorkflowExecutionResponseTest :: TerminateWorkflowExecutionResponse -> TestTree
terminateWorkflowExecutionResponseTest = resp
    "terminateWorkflowExecutionResponse"
    "fixture/TerminateWorkflowExecutionResponse"
    (Proxy :: Proxy TerminateWorkflowExecution)
