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

import           Data.Proxy
import           Network.AWS.SWF
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ listActivityTypesTest $
--             listActivityTypes
--
--         , listOpenWorkflowExecutionsTest $
--             listOpenWorkflowExecutions
--
--         , registerActivityTypeTest $
--             registerActivityType
--
--         , countPendingActivityTasksTest $
--             countPendingActivityTasks
--
--         , registerWorkflowTypeTest $
--             registerWorkflowType
--
--         , respondActivityTaskFailedTest $
--             respondActivityTaskFailed
--
--         , listWorkflowTypesTest $
--             listWorkflowTypes
--
--         , countOpenWorkflowExecutionsTest $
--             countOpenWorkflowExecutions
--
--         , describeWorkflowTypeTest $
--             describeWorkflowType
--
--         , requestCancelWorkflowExecutionTest $
--             requestCancelWorkflowExecution
--
--         , deprecateWorkflowTypeTest $
--             deprecateWorkflowType
--
--         , respondDecisionTaskCompletedTest $
--             respondDecisionTaskCompleted
--
--         , registerDomainTest $
--             registerDomain
--
--         , describeWorkflowExecutionTest $
--             describeWorkflowExecution
--
--         , pollForActivityTaskTest $
--             pollForActivityTask
--
--         , respondActivityTaskCompletedTest $
--             respondActivityTaskCompleted
--
--         , signalWorkflowExecutionTest $
--             signalWorkflowExecution
--
--         , countPendingDecisionTasksTest $
--             countPendingDecisionTasks
--
--         , listClosedWorkflowExecutionsTest $
--             listClosedWorkflowExecutions
--
--         , recordActivityTaskHeartbeatTest $
--             recordActivityTaskHeartbeat
--
--         , describeDomainTest $
--             describeDomain
--
--         , deprecateDomainTest $
--             deprecateDomain
--
--         , getWorkflowExecutionHistoryTest $
--             getWorkflowExecutionHistory
--
--         , describeActivityTypeTest $
--             describeActivityType
--
--         , deprecateActivityTypeTest $
--             deprecateActivityType
--
--         , terminateWorkflowExecutionTest $
--             terminateWorkflowExecution
--
--         , countClosedWorkflowExecutionsTest $
--             countClosedWorkflowExecutions
--
--         , respondActivityTaskCanceledTest $
--             respondActivityTaskCanceled
--
--         , listDomainsTest $
--             listDomains
--
--         , startWorkflowExecutionTest $
--             startWorkflowExecution
--
--         , pollForDecisionTaskTest $
--             pollForDecisionTask
--
--           ]

--     , testGroup "response"
--         [ listActivityTypesResponseTest $
--             listActivityTypesResponse
--
--         , workflowExecutionInfosTest $
--             workflowExecutionInfos
--
--         , registerActivityTypeResponseTest $
--             registerActivityTypeResponse
--
--         , pendingTaskCountTest $
--             pendingTaskCount
--
--         , registerWorkflowTypeResponseTest $
--             registerWorkflowTypeResponse
--
--         , respondActivityTaskFailedResponseTest $
--             respondActivityTaskFailedResponse
--
--         , listWorkflowTypesResponseTest $
--             listWorkflowTypesResponse
--
--         , workflowExecutionCountTest $
--             workflowExecutionCount
--
--         , describeWorkflowTypeResponseTest $
--             describeWorkflowTypeResponse
--
--         , requestCancelWorkflowExecutionResponseTest $
--             requestCancelWorkflowExecutionResponse
--
--         , deprecateWorkflowTypeResponseTest $
--             deprecateWorkflowTypeResponse
--
--         , respondDecisionTaskCompletedResponseTest $
--             respondDecisionTaskCompletedResponse
--
--         , registerDomainResponseTest $
--             registerDomainResponse
--
--         , describeWorkflowExecutionResponseTest $
--             describeWorkflowExecutionResponse
--
--         , pollForActivityTaskResponseTest $
--             pollForActivityTaskResponse
--
--         , respondActivityTaskCompletedResponseTest $
--             respondActivityTaskCompletedResponse
--
--         , signalWorkflowExecutionResponseTest $
--             signalWorkflowExecutionResponse
--
--         , pendingTaskCountTest $
--             pendingTaskCount
--
--         , workflowExecutionInfosTest $
--             workflowExecutionInfos
--
--         , recordActivityTaskHeartbeatResponseTest $
--             recordActivityTaskHeartbeatResponse
--
--         , describeDomainResponseTest $
--             describeDomainResponse
--
--         , deprecateDomainResponseTest $
--             deprecateDomainResponse
--
--         , getWorkflowExecutionHistoryResponseTest $
--             getWorkflowExecutionHistoryResponse
--
--         , describeActivityTypeResponseTest $
--             describeActivityTypeResponse
--
--         , deprecateActivityTypeResponseTest $
--             deprecateActivityTypeResponse
--
--         , terminateWorkflowExecutionResponseTest $
--             terminateWorkflowExecutionResponse
--
--         , workflowExecutionCountTest $
--             workflowExecutionCount
--
--         , respondActivityTaskCanceledResponseTest $
--             respondActivityTaskCanceledResponse
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--         , startWorkflowExecutionResponseTest $
--             startWorkflowExecutionResponse
--
--         , pollForDecisionTaskResponseTest $
--             pollForDecisionTaskResponse
--
--           ]
--     ]

-- Requests

listActivityTypesTest :: ListActivityTypes -> TestTree
listActivityTypesTest = undefined

listOpenWorkflowExecutionsTest :: ListOpenWorkflowExecutions -> TestTree
listOpenWorkflowExecutionsTest = undefined

registerActivityTypeTest :: RegisterActivityType -> TestTree
registerActivityTypeTest = undefined

countPendingActivityTasksTest :: CountPendingActivityTasks -> TestTree
countPendingActivityTasksTest = undefined

registerWorkflowTypeTest :: RegisterWorkflowType -> TestTree
registerWorkflowTypeTest = undefined

respondActivityTaskFailedTest :: RespondActivityTaskFailed -> TestTree
respondActivityTaskFailedTest = undefined

listWorkflowTypesTest :: ListWorkflowTypes -> TestTree
listWorkflowTypesTest = undefined

countOpenWorkflowExecutionsTest :: CountOpenWorkflowExecutions -> TestTree
countOpenWorkflowExecutionsTest = undefined

describeWorkflowTypeTest :: DescribeWorkflowType -> TestTree
describeWorkflowTypeTest = undefined

requestCancelWorkflowExecutionTest :: RequestCancelWorkflowExecution -> TestTree
requestCancelWorkflowExecutionTest = undefined

deprecateWorkflowTypeTest :: DeprecateWorkflowType -> TestTree
deprecateWorkflowTypeTest = undefined

respondDecisionTaskCompletedTest :: RespondDecisionTaskCompleted -> TestTree
respondDecisionTaskCompletedTest = undefined

registerDomainTest :: RegisterDomain -> TestTree
registerDomainTest = undefined

describeWorkflowExecutionTest :: DescribeWorkflowExecution -> TestTree
describeWorkflowExecutionTest = undefined

pollForActivityTaskTest :: PollForActivityTask -> TestTree
pollForActivityTaskTest = undefined

respondActivityTaskCompletedTest :: RespondActivityTaskCompleted -> TestTree
respondActivityTaskCompletedTest = undefined

signalWorkflowExecutionTest :: SignalWorkflowExecution -> TestTree
signalWorkflowExecutionTest = undefined

countPendingDecisionTasksTest :: CountPendingDecisionTasks -> TestTree
countPendingDecisionTasksTest = undefined

listClosedWorkflowExecutionsTest :: ListClosedWorkflowExecutions -> TestTree
listClosedWorkflowExecutionsTest = undefined

recordActivityTaskHeartbeatTest :: RecordActivityTaskHeartbeat -> TestTree
recordActivityTaskHeartbeatTest = undefined

describeDomainTest :: DescribeDomain -> TestTree
describeDomainTest = undefined

deprecateDomainTest :: DeprecateDomain -> TestTree
deprecateDomainTest = undefined

getWorkflowExecutionHistoryTest :: GetWorkflowExecutionHistory -> TestTree
getWorkflowExecutionHistoryTest = undefined

describeActivityTypeTest :: DescribeActivityType -> TestTree
describeActivityTypeTest = undefined

deprecateActivityTypeTest :: DeprecateActivityType -> TestTree
deprecateActivityTypeTest = undefined

terminateWorkflowExecutionTest :: TerminateWorkflowExecution -> TestTree
terminateWorkflowExecutionTest = undefined

countClosedWorkflowExecutionsTest :: CountClosedWorkflowExecutions -> TestTree
countClosedWorkflowExecutionsTest = undefined

respondActivityTaskCanceledTest :: RespondActivityTaskCanceled -> TestTree
respondActivityTaskCanceledTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

startWorkflowExecutionTest :: StartWorkflowExecution -> TestTree
startWorkflowExecutionTest = undefined

pollForDecisionTaskTest :: PollForDecisionTask -> TestTree
pollForDecisionTaskTest = undefined

-- Responses

listActivityTypesResponseTest :: ListActivityTypesResponse -> TestTree
listActivityTypesResponseTest = resp
    "ListActivityTypes"
    "fixture/SWF/ListActivityTypesResponse"
    (Proxy :: Proxy ListActivityTypes)

workflowExecutionInfosTest :: WorkflowExecutionInfos -> TestTree
workflowExecutionInfosTest = resp
    "ListOpenWorkflowExecutions"
    "fixture/SWF/WorkflowExecutionInfos"
    (Proxy :: Proxy ListOpenWorkflowExecutions)

registerActivityTypeResponseTest :: RegisterActivityTypeResponse -> TestTree
registerActivityTypeResponseTest = resp
    "RegisterActivityType"
    "fixture/SWF/RegisterActivityTypeResponse"
    (Proxy :: Proxy RegisterActivityType)

pendingTaskCountTest :: PendingTaskCount -> TestTree
pendingTaskCountTest = resp
    "CountPendingActivityTasks"
    "fixture/SWF/PendingTaskCount"
    (Proxy :: Proxy CountPendingActivityTasks)

registerWorkflowTypeResponseTest :: RegisterWorkflowTypeResponse -> TestTree
registerWorkflowTypeResponseTest = resp
    "RegisterWorkflowType"
    "fixture/SWF/RegisterWorkflowTypeResponse"
    (Proxy :: Proxy RegisterWorkflowType)

respondActivityTaskFailedResponseTest :: RespondActivityTaskFailedResponse -> TestTree
respondActivityTaskFailedResponseTest = resp
    "RespondActivityTaskFailed"
    "fixture/SWF/RespondActivityTaskFailedResponse"
    (Proxy :: Proxy RespondActivityTaskFailed)

listWorkflowTypesResponseTest :: ListWorkflowTypesResponse -> TestTree
listWorkflowTypesResponseTest = resp
    "ListWorkflowTypes"
    "fixture/SWF/ListWorkflowTypesResponse"
    (Proxy :: Proxy ListWorkflowTypes)

workflowExecutionCountTest :: WorkflowExecutionCount -> TestTree
workflowExecutionCountTest = resp
    "CountOpenWorkflowExecutions"
    "fixture/SWF/WorkflowExecutionCount"
    (Proxy :: Proxy CountOpenWorkflowExecutions)

describeWorkflowTypeResponseTest :: DescribeWorkflowTypeResponse -> TestTree
describeWorkflowTypeResponseTest = resp
    "DescribeWorkflowType"
    "fixture/SWF/DescribeWorkflowTypeResponse"
    (Proxy :: Proxy DescribeWorkflowType)

requestCancelWorkflowExecutionResponseTest :: RequestCancelWorkflowExecutionResponse -> TestTree
requestCancelWorkflowExecutionResponseTest = resp
    "RequestCancelWorkflowExecution"
    "fixture/SWF/RequestCancelWorkflowExecutionResponse"
    (Proxy :: Proxy RequestCancelWorkflowExecution)

deprecateWorkflowTypeResponseTest :: DeprecateWorkflowTypeResponse -> TestTree
deprecateWorkflowTypeResponseTest = resp
    "DeprecateWorkflowType"
    "fixture/SWF/DeprecateWorkflowTypeResponse"
    (Proxy :: Proxy DeprecateWorkflowType)

respondDecisionTaskCompletedResponseTest :: RespondDecisionTaskCompletedResponse -> TestTree
respondDecisionTaskCompletedResponseTest = resp
    "RespondDecisionTaskCompleted"
    "fixture/SWF/RespondDecisionTaskCompletedResponse"
    (Proxy :: Proxy RespondDecisionTaskCompleted)

registerDomainResponseTest :: RegisterDomainResponse -> TestTree
registerDomainResponseTest = resp
    "RegisterDomain"
    "fixture/SWF/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

describeWorkflowExecutionResponseTest :: DescribeWorkflowExecutionResponse -> TestTree
describeWorkflowExecutionResponseTest = resp
    "DescribeWorkflowExecution"
    "fixture/SWF/DescribeWorkflowExecutionResponse"
    (Proxy :: Proxy DescribeWorkflowExecution)

pollForActivityTaskResponseTest :: PollForActivityTaskResponse -> TestTree
pollForActivityTaskResponseTest = resp
    "PollForActivityTask"
    "fixture/SWF/PollForActivityTaskResponse"
    (Proxy :: Proxy PollForActivityTask)

respondActivityTaskCompletedResponseTest :: RespondActivityTaskCompletedResponse -> TestTree
respondActivityTaskCompletedResponseTest = resp
    "RespondActivityTaskCompleted"
    "fixture/SWF/RespondActivityTaskCompletedResponse"
    (Proxy :: Proxy RespondActivityTaskCompleted)

signalWorkflowExecutionResponseTest :: SignalWorkflowExecutionResponse -> TestTree
signalWorkflowExecutionResponseTest = resp
    "SignalWorkflowExecution"
    "fixture/SWF/SignalWorkflowExecutionResponse"
    (Proxy :: Proxy SignalWorkflowExecution)

pendingTaskCountTest :: PendingTaskCount -> TestTree
pendingTaskCountTest = resp
    "CountPendingDecisionTasks"
    "fixture/SWF/PendingTaskCount"
    (Proxy :: Proxy CountPendingDecisionTasks)

workflowExecutionInfosTest :: WorkflowExecutionInfos -> TestTree
workflowExecutionInfosTest = resp
    "ListClosedWorkflowExecutions"
    "fixture/SWF/WorkflowExecutionInfos"
    (Proxy :: Proxy ListClosedWorkflowExecutions)

recordActivityTaskHeartbeatResponseTest :: RecordActivityTaskHeartbeatResponse -> TestTree
recordActivityTaskHeartbeatResponseTest = resp
    "RecordActivityTaskHeartbeat"
    "fixture/SWF/RecordActivityTaskHeartbeatResponse"
    (Proxy :: Proxy RecordActivityTaskHeartbeat)

describeDomainResponseTest :: DescribeDomainResponse -> TestTree
describeDomainResponseTest = resp
    "DescribeDomain"
    "fixture/SWF/DescribeDomainResponse"
    (Proxy :: Proxy DescribeDomain)

deprecateDomainResponseTest :: DeprecateDomainResponse -> TestTree
deprecateDomainResponseTest = resp
    "DeprecateDomain"
    "fixture/SWF/DeprecateDomainResponse"
    (Proxy :: Proxy DeprecateDomain)

getWorkflowExecutionHistoryResponseTest :: GetWorkflowExecutionHistoryResponse -> TestTree
getWorkflowExecutionHistoryResponseTest = resp
    "GetWorkflowExecutionHistory"
    "fixture/SWF/GetWorkflowExecutionHistoryResponse"
    (Proxy :: Proxy GetWorkflowExecutionHistory)

describeActivityTypeResponseTest :: DescribeActivityTypeResponse -> TestTree
describeActivityTypeResponseTest = resp
    "DescribeActivityType"
    "fixture/SWF/DescribeActivityTypeResponse"
    (Proxy :: Proxy DescribeActivityType)

deprecateActivityTypeResponseTest :: DeprecateActivityTypeResponse -> TestTree
deprecateActivityTypeResponseTest = resp
    "DeprecateActivityType"
    "fixture/SWF/DeprecateActivityTypeResponse"
    (Proxy :: Proxy DeprecateActivityType)

terminateWorkflowExecutionResponseTest :: TerminateWorkflowExecutionResponse -> TestTree
terminateWorkflowExecutionResponseTest = resp
    "TerminateWorkflowExecution"
    "fixture/SWF/TerminateWorkflowExecutionResponse"
    (Proxy :: Proxy TerminateWorkflowExecution)

workflowExecutionCountTest :: WorkflowExecutionCount -> TestTree
workflowExecutionCountTest = resp
    "CountClosedWorkflowExecutions"
    "fixture/SWF/WorkflowExecutionCount"
    (Proxy :: Proxy CountClosedWorkflowExecutions)

respondActivityTaskCanceledResponseTest :: RespondActivityTaskCanceledResponse -> TestTree
respondActivityTaskCanceledResponseTest = resp
    "RespondActivityTaskCanceled"
    "fixture/SWF/RespondActivityTaskCanceledResponse"
    (Proxy :: Proxy RespondActivityTaskCanceled)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "ListDomains"
    "fixture/SWF/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

startWorkflowExecutionResponseTest :: StartWorkflowExecutionResponse -> TestTree
startWorkflowExecutionResponseTest = resp
    "StartWorkflowExecution"
    "fixture/SWF/StartWorkflowExecutionResponse"
    (Proxy :: Proxy StartWorkflowExecution)

pollForDecisionTaskResponseTest :: PollForDecisionTaskResponse -> TestTree
pollForDecisionTaskResponseTest = resp
    "PollForDecisionTask"
    "fixture/SWF/PollForDecisionTaskResponse"
    (Proxy :: Proxy PollForDecisionTask)
