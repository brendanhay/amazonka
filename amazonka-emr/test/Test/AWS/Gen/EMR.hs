-- Module      : Test.AWS.Gen.EMR
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

module Test.AWS.Gen.EMR where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.EMR

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addInstanceGroupsTest $
--             addInstanceGroups
--
--         , addJobFlowStepsTest $
--             addJobFlowSteps
--
--         , addTagsTest $
--             addTags
--
--         , describeClusterTest $
--             describeCluster
--
--         , describeStepTest $
--             describeStep
--
--         , listBootstrapActionsTest $
--             listBootstrapActions
--
--         , listClustersTest $
--             listClusters
--
--         , listInstanceGroupsTest $
--             listInstanceGroups
--
--         , listInstancesTest $
--             listInstances
--
--         , listStepsTest $
--             listSteps
--
--         , modifyInstanceGroupsTest $
--             modifyInstanceGroups
--
--         , removeTagsTest $
--             removeTags
--
--         , runJobFlowTest $
--             runJobFlow
--
--         , setTerminationProtectionTest $
--             setTerminationProtection
--
--         , setVisibleToAllUsersTest $
--             setVisibleToAllUsers
--
--         , terminateJobFlowsTest $
--             terminateJobFlows
--
--           ]

--     , testGroup "response"
--         [ addInstanceGroupsResponseTest $
--             addInstanceGroupsResponse
--
--         , addJobFlowStepsResponseTest $
--             addJobFlowStepsResponse
--
--         , addTagsResponseTest $
--             addTagsResponse
--
--         , describeClusterResponseTest $
--             describeClusterResponse
--
--         , describeStepResponseTest $
--             describeStepResponse
--
--         , listBootstrapActionsResponseTest $
--             listBootstrapActionsResponse
--
--         , listClustersResponseTest $
--             listClustersResponse
--
--         , listInstanceGroupsResponseTest $
--             listInstanceGroupsResponse
--
--         , listInstancesResponseTest $
--             listInstancesResponse
--
--         , listStepsResponseTest $
--             listStepsResponse
--
--         , modifyInstanceGroupsResponseTest $
--             modifyInstanceGroupsResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , runJobFlowResponseTest $
--             runJobFlowResponse
--
--         , setTerminationProtectionResponseTest $
--             setTerminationProtectionResponse
--
--         , setVisibleToAllUsersResponseTest $
--             setVisibleToAllUsersResponse
--
--         , terminateJobFlowsResponseTest $
--             terminateJobFlowsResponse
--
--           ]
--     ]

-- Requests

addInstanceGroupsTest :: AddInstanceGroups -> TestTree
addInstanceGroupsTest = undefined

addJobFlowStepsTest :: AddJobFlowSteps -> TestTree
addJobFlowStepsTest = undefined

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

describeClusterTest :: DescribeCluster -> TestTree
describeClusterTest = undefined

describeStepTest :: DescribeStep -> TestTree
describeStepTest = undefined

listBootstrapActionsTest :: ListBootstrapActions -> TestTree
listBootstrapActionsTest = undefined

listClustersTest :: ListClusters -> TestTree
listClustersTest = undefined

listInstanceGroupsTest :: ListInstanceGroups -> TestTree
listInstanceGroupsTest = undefined

listInstancesTest :: ListInstances -> TestTree
listInstancesTest = undefined

listStepsTest :: ListSteps -> TestTree
listStepsTest = undefined

modifyInstanceGroupsTest :: ModifyInstanceGroups -> TestTree
modifyInstanceGroupsTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

runJobFlowTest :: RunJobFlow -> TestTree
runJobFlowTest = undefined

setTerminationProtectionTest :: SetTerminationProtection -> TestTree
setTerminationProtectionTest = undefined

setVisibleToAllUsersTest :: SetVisibleToAllUsers -> TestTree
setVisibleToAllUsersTest = undefined

terminateJobFlowsTest :: TerminateJobFlows -> TestTree
terminateJobFlowsTest = undefined

-- Responses

addInstanceGroupsResponseTest :: AddInstanceGroupsResponse -> TestTree
addInstanceGroupsResponseTest = resp
    "addInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse"
    (Proxy :: Proxy AddInstanceGroups)

addJobFlowStepsResponseTest :: AddJobFlowStepsResponse -> TestTree
addJobFlowStepsResponseTest = resp
    "addJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse"
    (Proxy :: Proxy AddJobFlowSteps)

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "addTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

describeClusterResponseTest :: DescribeClusterResponse -> TestTree
describeClusterResponseTest = resp
    "describeClusterResponse"
    "fixture/DescribeClusterResponse"
    (Proxy :: Proxy DescribeCluster)

describeStepResponseTest :: DescribeStepResponse -> TestTree
describeStepResponseTest = resp
    "describeStepResponse"
    "fixture/DescribeStepResponse"
    (Proxy :: Proxy DescribeStep)

listBootstrapActionsResponseTest :: ListBootstrapActionsResponse -> TestTree
listBootstrapActionsResponseTest = resp
    "listBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse"
    (Proxy :: Proxy ListBootstrapActions)

listClustersResponseTest :: ListClustersResponse -> TestTree
listClustersResponseTest = resp
    "listClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)

listInstanceGroupsResponseTest :: ListInstanceGroupsResponse -> TestTree
listInstanceGroupsResponseTest = resp
    "listInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse"
    (Proxy :: Proxy ListInstanceGroups)

listInstancesResponseTest :: ListInstancesResponse -> TestTree
listInstancesResponseTest = resp
    "listInstancesResponse"
    "fixture/ListInstancesResponse"
    (Proxy :: Proxy ListInstances)

listStepsResponseTest :: ListStepsResponse -> TestTree
listStepsResponseTest = resp
    "listStepsResponse"
    "fixture/ListStepsResponse"
    (Proxy :: Proxy ListSteps)

modifyInstanceGroupsResponseTest :: ModifyInstanceGroupsResponse -> TestTree
modifyInstanceGroupsResponseTest = resp
    "modifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse"
    (Proxy :: Proxy ModifyInstanceGroups)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "removeTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

runJobFlowResponseTest :: RunJobFlowResponse -> TestTree
runJobFlowResponseTest = resp
    "runJobFlowResponse"
    "fixture/RunJobFlowResponse"
    (Proxy :: Proxy RunJobFlow)

setTerminationProtectionResponseTest :: SetTerminationProtectionResponse -> TestTree
setTerminationProtectionResponseTest = resp
    "setTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse"
    (Proxy :: Proxy SetTerminationProtection)

setVisibleToAllUsersResponseTest :: SetVisibleToAllUsersResponse -> TestTree
setVisibleToAllUsersResponseTest = resp
    "setVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse"
    (Proxy :: Proxy SetVisibleToAllUsers)

terminateJobFlowsResponseTest :: TerminateJobFlowsResponse -> TestTree
terminateJobFlowsResponseTest = resp
    "terminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse"
    (Proxy :: Proxy TerminateJobFlows)
