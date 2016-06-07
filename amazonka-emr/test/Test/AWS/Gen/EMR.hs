{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         [ requestRunJobFlow $
--             runJobFlow
--
--         , requestSetVisibleToAllUsers $
--             setVisibleToAllUsers
--
--         , requestTerminateJobFlows $
--             terminateJobFlows
--
--         , requestDescribeStep $
--             describeStep
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestDescribeCluster $
--             describeCluster
--
--         , requestSetTerminationProtection $
--             setTerminationProtection
--
--         , requestAddJobFlowSteps $
--             addJobFlowSteps
--
--         , requestModifyInstanceGroups $
--             modifyInstanceGroups
--
--         , requestListSteps $
--             listSteps
--
--         , requestAddInstanceGroups $
--             addInstanceGroups
--
--         , requestListInstanceGroups $
--             listInstanceGroups
--
--         , requestListBootstrapActions $
--             listBootstrapActions
--
--         , requestAddTags $
--             addTags
--
--         , requestListInstances $
--             listInstances
--
--         , requestListClusters $
--             listClusters
--
--           ]

--     , testGroup "response"
--         [ responseRunJobFlow $
--             runJobFlowResponse
--
--         , responseSetVisibleToAllUsers $
--             setVisibleToAllUsersResponse
--
--         , responseTerminateJobFlows $
--             terminateJobFlowsResponse
--
--         , responseDescribeStep $
--             describeStepResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseDescribeCluster $
--             describeClusterResponse
--
--         , responseSetTerminationProtection $
--             setTerminationProtectionResponse
--
--         , responseAddJobFlowSteps $
--             addJobFlowStepsResponse
--
--         , responseModifyInstanceGroups $
--             modifyInstanceGroupsResponse
--
--         , responseListSteps $
--             listStepsResponse
--
--         , responseAddInstanceGroups $
--             addInstanceGroupsResponse
--
--         , responseListInstanceGroups $
--             listInstanceGroupsResponse
--
--         , responseListBootstrapActions $
--             listBootstrapActionsResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListInstances $
--             listInstancesResponse
--
--         , responseListClusters $
--             listClustersResponse
--
--           ]
--     ]

-- Requests

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow = req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
requestSetVisibleToAllUsers = req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

requestTerminateJobFlows :: TerminateJobFlows -> TestTree
requestTerminateJobFlows = req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

requestDescribeStep :: DescribeStep -> TestTree
requestDescribeStep = req
    "DescribeStep"
    "fixture/DescribeStep.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster = req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection = req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps = req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
requestModifyInstanceGroups = req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps = req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups = req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups = req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestListBootstrapActions :: ListBootstrapActions -> TestTree
requestListBootstrapActions = req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances = req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

-- Responses

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow = res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    emr
    (Proxy :: Proxy RunJobFlow)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers = res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    emr
    (Proxy :: Proxy SetVisibleToAllUsers)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows = res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    emr
    (Proxy :: Proxy TerminateJobFlows)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep = res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    emr
    (Proxy :: Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    emr
    (Proxy :: Proxy RemoveTags)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    emr
    (Proxy :: Proxy DescribeCluster)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection = res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    emr
    (Proxy :: Proxy SetTerminationProtection)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps = res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    emr
    (Proxy :: Proxy AddJobFlowSteps)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups = res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ModifyInstanceGroups)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps = res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    emr
    (Proxy :: Proxy ListSteps)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups = res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy AddInstanceGroups)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups = res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ListInstanceGroups)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions = res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    emr
    (Proxy :: Proxy ListBootstrapActions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    emr
    (Proxy :: Proxy AddTags)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    emr
    (Proxy :: Proxy ListInstances)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    emr
    (Proxy :: Proxy ListClusters)
