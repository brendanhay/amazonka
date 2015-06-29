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
-- fixtures =
--     [ testGroup "request"
--         [ testRunJobFlow $
--             runJobFlow
--
--         , testSetVisibleToAllUsers $
--             setVisibleToAllUsers
--
--         , testTerminateJobFlows $
--             terminateJobFlows
--
--         , testRemoveTags $
--             removeTags
--
--         , testDescribeStep $
--             describeStep
--
--         , testDescribeCluster $
--             describeCluster
--
--         , testModifyInstanceGroups $
--             modifyInstanceGroups
--
--         , testAddJobFlowSteps $
--             addJobFlowSteps
--
--         , testSetTerminationProtection $
--             setTerminationProtection
--
--         , testListSteps $
--             listSteps
--
--         , testAddInstanceGroups $
--             addInstanceGroups
--
--         , testListInstanceGroups $
--             listInstanceGroups
--
--         , testListBootstrapActions $
--             listBootstrapActions
--
--         , testAddTags $
--             addTags
--
--         , testListInstances $
--             listInstances
--
--         , testListClusters $
--             listClusters
--
--           ]

--     , testGroup "response"
--         [ testRunJobFlowResponse $
--             runJobFlowResponse
--
--         , testSetVisibleToAllUsersResponse $
--             setVisibleToAllUsersResponse
--
--         , testTerminateJobFlowsResponse $
--             terminateJobFlowsResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
--
--         , testDescribeStepResponse $
--             describeStepResponse
--
--         , testDescribeClusterResponse $
--             describeClusterResponse
--
--         , testModifyInstanceGroupsResponse $
--             modifyInstanceGroupsResponse
--
--         , testAddJobFlowStepsResponse $
--             addJobFlowStepsResponse
--
--         , testSetTerminationProtectionResponse $
--             setTerminationProtectionResponse
--
--         , testListStepsResponse $
--             listStepsResponse
--
--         , testAddInstanceGroupsResponse $
--             addInstanceGroupsResponse
--
--         , testListInstanceGroupsResponse $
--             listInstanceGroupsResponse
--
--         , testListBootstrapActionsResponse $
--             listBootstrapActionsResponse
--
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testListInstancesResponse $
--             listInstancesResponse
--
--         , testListClustersResponse $
--             listClustersResponse
--
--           ]
--     ]

-- Requests

testRunJobFlow :: RunJobFlow -> TestTree
testRunJobFlow = undefined

testSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
testSetVisibleToAllUsers = undefined

testTerminateJobFlows :: TerminateJobFlows -> TestTree
testTerminateJobFlows = undefined

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = undefined

testDescribeStep :: DescribeStep -> TestTree
testDescribeStep = undefined

testDescribeCluster :: DescribeCluster -> TestTree
testDescribeCluster = undefined

testModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
testModifyInstanceGroups = undefined

testAddJobFlowSteps :: AddJobFlowSteps -> TestTree
testAddJobFlowSteps = undefined

testSetTerminationProtection :: SetTerminationProtection -> TestTree
testSetTerminationProtection = undefined

testListSteps :: ListSteps -> TestTree
testListSteps = undefined

testAddInstanceGroups :: AddInstanceGroups -> TestTree
testAddInstanceGroups = undefined

testListInstanceGroups :: ListInstanceGroups -> TestTree
testListInstanceGroups = undefined

testListBootstrapActions :: ListBootstrapActions -> TestTree
testListBootstrapActions = undefined

testAddTags :: AddTags -> TestTree
testAddTags = undefined

testListInstances :: ListInstances -> TestTree
testListInstances = undefined

testListClusters :: ListClusters -> TestTree
testListClusters = undefined

-- Responses

testRunJobFlowResponse :: RunJobFlowResponse -> TestTree
testRunJobFlowResponse = resp
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse"
    (Proxy :: Proxy RunJobFlow)

testSetVisibleToAllUsersResponse :: SetVisibleToAllUsersResponse -> TestTree
testSetVisibleToAllUsersResponse = resp
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse"
    (Proxy :: Proxy SetVisibleToAllUsers)

testTerminateJobFlowsResponse :: TerminateJobFlowsResponse -> TestTree
testTerminateJobFlowsResponse = resp
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse"
    (Proxy :: Proxy TerminateJobFlows)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = resp
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testDescribeStepResponse :: DescribeStepResponse -> TestTree
testDescribeStepResponse = resp
    "DescribeStepResponse"
    "fixture/DescribeStepResponse"
    (Proxy :: Proxy DescribeStep)

testDescribeClusterResponse :: DescribeClusterResponse -> TestTree
testDescribeClusterResponse = resp
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse"
    (Proxy :: Proxy DescribeCluster)

testModifyInstanceGroupsResponse :: ModifyInstanceGroupsResponse -> TestTree
testModifyInstanceGroupsResponse = resp
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse"
    (Proxy :: Proxy ModifyInstanceGroups)

testAddJobFlowStepsResponse :: AddJobFlowStepsResponse -> TestTree
testAddJobFlowStepsResponse = resp
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse"
    (Proxy :: Proxy AddJobFlowSteps)

testSetTerminationProtectionResponse :: SetTerminationProtectionResponse -> TestTree
testSetTerminationProtectionResponse = resp
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse"
    (Proxy :: Proxy SetTerminationProtection)

testListStepsResponse :: ListStepsResponse -> TestTree
testListStepsResponse = resp
    "ListStepsResponse"
    "fixture/ListStepsResponse"
    (Proxy :: Proxy ListSteps)

testAddInstanceGroupsResponse :: AddInstanceGroupsResponse -> TestTree
testAddInstanceGroupsResponse = resp
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse"
    (Proxy :: Proxy AddInstanceGroups)

testListInstanceGroupsResponse :: ListInstanceGroupsResponse -> TestTree
testListInstanceGroupsResponse = resp
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse"
    (Proxy :: Proxy ListInstanceGroups)

testListBootstrapActionsResponse :: ListBootstrapActionsResponse -> TestTree
testListBootstrapActionsResponse = resp
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse"
    (Proxy :: Proxy ListBootstrapActions)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = resp
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testListInstancesResponse :: ListInstancesResponse -> TestTree
testListInstancesResponse = resp
    "ListInstancesResponse"
    "fixture/ListInstancesResponse"
    (Proxy :: Proxy ListInstances)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = resp
    "ListClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)
