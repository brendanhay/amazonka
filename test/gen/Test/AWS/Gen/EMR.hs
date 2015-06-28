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

import           Data.Proxy
import           Network.AWS.EMR
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ runJobFlowTest $
--             runJobFlow
--
--         , setVisibleToAllUsersTest $
--             setVisibleToAllUsers
--
--         , terminateJobFlowsTest $
--             terminateJobFlows
--
--         , removeTagsTest $
--             removeTags
--
--         , describeStepTest $
--             describeStep
--
--         , describeClusterTest $
--             describeCluster
--
--         , modifyInstanceGroupsTest $
--             modifyInstanceGroups
--
--         , addJobFlowStepsTest $
--             addJobFlowSteps
--
--         , setTerminationProtectionTest $
--             setTerminationProtection
--
--         , listStepsTest $
--             listSteps
--
--         , addInstanceGroupsTest $
--             addInstanceGroups
--
--         , listInstanceGroupsTest $
--             listInstanceGroups
--
--         , listBootstrapActionsTest $
--             listBootstrapActions
--
--         , addTagsTest $
--             addTags
--
--         , listInstancesTest $
--             listInstances
--
--         , listClustersTest $
--             listClusters
--
--           ]

--     , testGroup "response"
--         [ runJobFlowResponseTest $
--             runJobFlowResponse
--
--         , setVisibleToAllUsersResponseTest $
--             setVisibleToAllUsersResponse
--
--         , terminateJobFlowsResponseTest $
--             terminateJobFlowsResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , describeStepResponseTest $
--             describeStepResponse
--
--         , describeClusterResponseTest $
--             describeClusterResponse
--
--         , modifyInstanceGroupsResponseTest $
--             modifyInstanceGroupsResponse
--
--         , addJobFlowStepsResponseTest $
--             addJobFlowStepsResponse
--
--         , setTerminationProtectionResponseTest $
--             setTerminationProtectionResponse
--
--         , listStepsResponseTest $
--             listStepsResponse
--
--         , addInstanceGroupsResponseTest $
--             addInstanceGroupsResponse
--
--         , listInstanceGroupsResponseTest $
--             listInstanceGroupsResponse
--
--         , listBootstrapActionsResponseTest $
--             listBootstrapActionsResponse
--
--         , addTagsResponseTest $
--             addTagsResponse
--
--         , listInstancesResponseTest $
--             listInstancesResponse
--
--         , listClustersResponseTest $
--             listClustersResponse
--
--           ]
--     ]

-- Requests

runJobFlowTest :: RunJobFlow -> TestTree
runJobFlowTest = undefined

setVisibleToAllUsersTest :: SetVisibleToAllUsers -> TestTree
setVisibleToAllUsersTest = undefined

terminateJobFlowsTest :: TerminateJobFlows -> TestTree
terminateJobFlowsTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

describeStepTest :: DescribeStep -> TestTree
describeStepTest = undefined

describeClusterTest :: DescribeCluster -> TestTree
describeClusterTest = undefined

modifyInstanceGroupsTest :: ModifyInstanceGroups -> TestTree
modifyInstanceGroupsTest = undefined

addJobFlowStepsTest :: AddJobFlowSteps -> TestTree
addJobFlowStepsTest = undefined

setTerminationProtectionTest :: SetTerminationProtection -> TestTree
setTerminationProtectionTest = undefined

listStepsTest :: ListSteps -> TestTree
listStepsTest = undefined

addInstanceGroupsTest :: AddInstanceGroups -> TestTree
addInstanceGroupsTest = undefined

listInstanceGroupsTest :: ListInstanceGroups -> TestTree
listInstanceGroupsTest = undefined

listBootstrapActionsTest :: ListBootstrapActions -> TestTree
listBootstrapActionsTest = undefined

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

listInstancesTest :: ListInstances -> TestTree
listInstancesTest = undefined

listClustersTest :: ListClusters -> TestTree
listClustersTest = undefined

-- Responses

runJobFlowResponseTest :: RunJobFlowResponse -> TestTree
runJobFlowResponseTest = resp
    "RunJobFlow"
    "fixture/EMR/RunJobFlowResponse"
    (Proxy :: Proxy RunJobFlow)

setVisibleToAllUsersResponseTest :: SetVisibleToAllUsersResponse -> TestTree
setVisibleToAllUsersResponseTest = resp
    "SetVisibleToAllUsers"
    "fixture/EMR/SetVisibleToAllUsersResponse"
    (Proxy :: Proxy SetVisibleToAllUsers)

terminateJobFlowsResponseTest :: TerminateJobFlowsResponse -> TestTree
terminateJobFlowsResponseTest = resp
    "TerminateJobFlows"
    "fixture/EMR/TerminateJobFlowsResponse"
    (Proxy :: Proxy TerminateJobFlows)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "RemoveTags"
    "fixture/EMR/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

describeStepResponseTest :: DescribeStepResponse -> TestTree
describeStepResponseTest = resp
    "DescribeStep"
    "fixture/EMR/DescribeStepResponse"
    (Proxy :: Proxy DescribeStep)

describeClusterResponseTest :: DescribeClusterResponse -> TestTree
describeClusterResponseTest = resp
    "DescribeCluster"
    "fixture/EMR/DescribeClusterResponse"
    (Proxy :: Proxy DescribeCluster)

modifyInstanceGroupsResponseTest :: ModifyInstanceGroupsResponse -> TestTree
modifyInstanceGroupsResponseTest = resp
    "ModifyInstanceGroups"
    "fixture/EMR/ModifyInstanceGroupsResponse"
    (Proxy :: Proxy ModifyInstanceGroups)

addJobFlowStepsResponseTest :: AddJobFlowStepsResponse -> TestTree
addJobFlowStepsResponseTest = resp
    "AddJobFlowSteps"
    "fixture/EMR/AddJobFlowStepsResponse"
    (Proxy :: Proxy AddJobFlowSteps)

setTerminationProtectionResponseTest :: SetTerminationProtectionResponse -> TestTree
setTerminationProtectionResponseTest = resp
    "SetTerminationProtection"
    "fixture/EMR/SetTerminationProtectionResponse"
    (Proxy :: Proxy SetTerminationProtection)

listStepsResponseTest :: ListStepsResponse -> TestTree
listStepsResponseTest = resp
    "ListSteps"
    "fixture/EMR/ListStepsResponse"
    (Proxy :: Proxy ListSteps)

addInstanceGroupsResponseTest :: AddInstanceGroupsResponse -> TestTree
addInstanceGroupsResponseTest = resp
    "AddInstanceGroups"
    "fixture/EMR/AddInstanceGroupsResponse"
    (Proxy :: Proxy AddInstanceGroups)

listInstanceGroupsResponseTest :: ListInstanceGroupsResponse -> TestTree
listInstanceGroupsResponseTest = resp
    "ListInstanceGroups"
    "fixture/EMR/ListInstanceGroupsResponse"
    (Proxy :: Proxy ListInstanceGroups)

listBootstrapActionsResponseTest :: ListBootstrapActionsResponse -> TestTree
listBootstrapActionsResponseTest = resp
    "ListBootstrapActions"
    "fixture/EMR/ListBootstrapActionsResponse"
    (Proxy :: Proxy ListBootstrapActions)

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "AddTags"
    "fixture/EMR/AddTagsResponse"
    (Proxy :: Proxy AddTags)

listInstancesResponseTest :: ListInstancesResponse -> TestTree
listInstancesResponseTest = resp
    "ListInstances"
    "fixture/EMR/ListInstancesResponse"
    (Proxy :: Proxy ListInstances)

listClustersResponseTest :: ListClustersResponse -> TestTree
listClustersResponseTest = resp
    "ListClusters"
    "fixture/EMR/ListClustersResponse"
    (Proxy :: Proxy ListClusters)
