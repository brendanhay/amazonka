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
--         , testDescribeStep $
--             describeStep
--
--         , testRemoveTags $
--             removeTags
--
--         , testDescribeCluster $
--             describeCluster
--
--         , testSetTerminationProtection $
--             setTerminationProtection
--
--         , testAddJobFlowSteps $
--             addJobFlowSteps
--
--         , testModifyInstanceGroups $
--             modifyInstanceGroups
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
--         , testDescribeStepResponse $
--             describeStepResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
--
--         , testDescribeClusterResponse $
--             describeClusterResponse
--
--         , testSetTerminationProtectionResponse $
--             setTerminationProtectionResponse
--
--         , testAddJobFlowStepsResponse $
--             addJobFlowStepsResponse
--
--         , testModifyInstanceGroupsResponse $
--             modifyInstanceGroupsResponse
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
    "fixture/RunJobFlow.yaml"

testSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
testSetVisibleToAllUsers = req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

testTerminateJobFlows :: TerminateJobFlows -> TestTree
testTerminateJobFlows = req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

testDescribeStep :: DescribeStep -> TestTree
testDescribeStep = req
    "DescribeStep"
    "fixture/DescribeStep.yaml"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

testDescribeCluster :: DescribeCluster -> TestTree
testDescribeCluster = req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

testSetTerminationProtection :: SetTerminationProtection -> TestTree
testSetTerminationProtection = req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

testAddJobFlowSteps :: AddJobFlowSteps -> TestTree
testAddJobFlowSteps = req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

testModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
testModifyInstanceGroups = req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

testListSteps :: ListSteps -> TestTree
testListSteps = req
    "ListSteps"
    "fixture/ListSteps.yaml"

testAddInstanceGroups :: AddInstanceGroups -> TestTree
testAddInstanceGroups = req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

testListInstanceGroups :: ListInstanceGroups -> TestTree
testListInstanceGroups = req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

testListBootstrapActions :: ListBootstrapActions -> TestTree
testListBootstrapActions = req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testListInstances :: ListInstances -> TestTree
testListInstances = req
    "ListInstances"
    "fixture/ListInstances.yaml"

testListClusters :: ListClusters -> TestTree
testListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

-- Responses

testRunJobFlowResponse :: RunJobFlowResponse -> TestTree
testRunJobFlowResponse = res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    eMR
    (Proxy :: Proxy RunJobFlow)

testSetVisibleToAllUsersResponse :: SetVisibleToAllUsersResponse -> TestTree
testSetVisibleToAllUsersResponse = res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    eMR
    (Proxy :: Proxy SetVisibleToAllUsers)

testTerminateJobFlowsResponse :: TerminateJobFlowsResponse -> TestTree
testTerminateJobFlowsResponse = res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    eMR
    (Proxy :: Proxy TerminateJobFlows)

testDescribeStepResponse :: DescribeStepResponse -> TestTree
testDescribeStepResponse = res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    eMR
    (Proxy :: Proxy DescribeStep)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    eMR
    (Proxy :: Proxy RemoveTags)

testDescribeClusterResponse :: DescribeClusterResponse -> TestTree
testDescribeClusterResponse = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    eMR
    (Proxy :: Proxy DescribeCluster)

testSetTerminationProtectionResponse :: SetTerminationProtectionResponse -> TestTree
testSetTerminationProtectionResponse = res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    eMR
    (Proxy :: Proxy SetTerminationProtection)

testAddJobFlowStepsResponse :: AddJobFlowStepsResponse -> TestTree
testAddJobFlowStepsResponse = res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    eMR
    (Proxy :: Proxy AddJobFlowSteps)

testModifyInstanceGroupsResponse :: ModifyInstanceGroupsResponse -> TestTree
testModifyInstanceGroupsResponse = res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    eMR
    (Proxy :: Proxy ModifyInstanceGroups)

testListStepsResponse :: ListStepsResponse -> TestTree
testListStepsResponse = res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    eMR
    (Proxy :: Proxy ListSteps)

testAddInstanceGroupsResponse :: AddInstanceGroupsResponse -> TestTree
testAddInstanceGroupsResponse = res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    eMR
    (Proxy :: Proxy AddInstanceGroups)

testListInstanceGroupsResponse :: ListInstanceGroupsResponse -> TestTree
testListInstanceGroupsResponse = res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    eMR
    (Proxy :: Proxy ListInstanceGroups)

testListBootstrapActionsResponse :: ListBootstrapActionsResponse -> TestTree
testListBootstrapActionsResponse = res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    eMR
    (Proxy :: Proxy ListBootstrapActions)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    eMR
    (Proxy :: Proxy AddTags)

testListInstancesResponse :: ListInstancesResponse -> TestTree
testListInstancesResponse = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    eMR
    (Proxy :: Proxy ListInstances)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    eMR
    (Proxy :: Proxy ListClusters)
