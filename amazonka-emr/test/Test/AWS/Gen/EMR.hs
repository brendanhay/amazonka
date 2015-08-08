{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EMR where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.EMR
import Test.AWS.EMR.Internal

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
testRunJobFlow = req
    "RunJobFlow"
    "fixture/RunJobFlow"

testSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
testSetVisibleToAllUsers = req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers"

testTerminateJobFlows :: TerminateJobFlows -> TestTree
testTerminateJobFlows = req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags"

testDescribeStep :: DescribeStep -> TestTree
testDescribeStep = req
    "DescribeStep"
    "fixture/DescribeStep"

testDescribeCluster :: DescribeCluster -> TestTree
testDescribeCluster = req
    "DescribeCluster"
    "fixture/DescribeCluster"

testModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
testModifyInstanceGroups = req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups"

testAddJobFlowSteps :: AddJobFlowSteps -> TestTree
testAddJobFlowSteps = req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps"

testSetTerminationProtection :: SetTerminationProtection -> TestTree
testSetTerminationProtection = req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection"

testListSteps :: ListSteps -> TestTree
testListSteps = req
    "ListSteps"
    "fixture/ListSteps"

testAddInstanceGroups :: AddInstanceGroups -> TestTree
testAddInstanceGroups = req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups"

testListInstanceGroups :: ListInstanceGroups -> TestTree
testListInstanceGroups = req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups"

testListBootstrapActions :: ListBootstrapActions -> TestTree
testListBootstrapActions = req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags"

testListInstances :: ListInstances -> TestTree
testListInstances = req
    "ListInstances"
    "fixture/ListInstances"

testListClusters :: ListClusters -> TestTree
testListClusters = req
    "ListClusters"
    "fixture/ListClusters"

-- Responses

testRunJobFlowResponse :: RunJobFlowResponse -> TestTree
testRunJobFlowResponse = res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse"
    (Proxy :: Proxy RunJobFlow)

testSetVisibleToAllUsersResponse :: SetVisibleToAllUsersResponse -> TestTree
testSetVisibleToAllUsersResponse = res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse"
    (Proxy :: Proxy SetVisibleToAllUsers)

testTerminateJobFlowsResponse :: TerminateJobFlowsResponse -> TestTree
testTerminateJobFlowsResponse = res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse"
    (Proxy :: Proxy TerminateJobFlows)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testDescribeStepResponse :: DescribeStepResponse -> TestTree
testDescribeStepResponse = res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse"
    (Proxy :: Proxy DescribeStep)

testDescribeClusterResponse :: DescribeClusterResponse -> TestTree
testDescribeClusterResponse = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse"
    (Proxy :: Proxy DescribeCluster)

testModifyInstanceGroupsResponse :: ModifyInstanceGroupsResponse -> TestTree
testModifyInstanceGroupsResponse = res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse"
    (Proxy :: Proxy ModifyInstanceGroups)

testAddJobFlowStepsResponse :: AddJobFlowStepsResponse -> TestTree
testAddJobFlowStepsResponse = res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse"
    (Proxy :: Proxy AddJobFlowSteps)

testSetTerminationProtectionResponse :: SetTerminationProtectionResponse -> TestTree
testSetTerminationProtectionResponse = res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse"
    (Proxy :: Proxy SetTerminationProtection)

testListStepsResponse :: ListStepsResponse -> TestTree
testListStepsResponse = res
    "ListStepsResponse"
    "fixture/ListStepsResponse"
    (Proxy :: Proxy ListSteps)

testAddInstanceGroupsResponse :: AddInstanceGroupsResponse -> TestTree
testAddInstanceGroupsResponse = res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse"
    (Proxy :: Proxy AddInstanceGroups)

testListInstanceGroupsResponse :: ListInstanceGroupsResponse -> TestTree
testListInstanceGroupsResponse = res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse"
    (Proxy :: Proxy ListInstanceGroups)

testListBootstrapActionsResponse :: ListBootstrapActionsResponse -> TestTree
testListBootstrapActionsResponse = res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse"
    (Proxy :: Proxy ListBootstrapActions)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testListInstancesResponse :: ListInstancesResponse -> TestTree
testListInstancesResponse = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse"
    (Proxy :: Proxy ListInstances)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = res
    "ListClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)
