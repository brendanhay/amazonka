{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.EMR where

import Data.Proxy
import Network.AWS.EMR
import Test.AWS.EMR.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

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
--         , requestRemoveAutoScalingPolicy $
--             removeAutoScalingPolicy
--
--         , requestCreateStudio $
--             createStudio
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
--         , requestListSecurityConfigurations $
--             listSecurityConfigurations
--
--         , requestCancelSteps $
--             cancelSteps
--
--         , requestListNotebookExecutions $
--             listNotebookExecutions
--
--         , requestCreateSecurityConfiguration $
--             createSecurityConfiguration
--
--         , requestSetTerminationProtection $
--             setTerminationProtection
--
--         , requestAddJobFlowSteps $
--             addJobFlowSteps
--
--         , requestDescribeStudio $
--             describeStudio
--
--         , requestModifyInstanceGroups $
--             modifyInstanceGroups
--
--         , requestStartNotebookExecution $
--             startNotebookExecution
--
--         , requestListSteps $
--             listSteps
--
--         , requestCreateStudioSessionMapping $
--             createStudioSessionMapping
--
--         , requestAddInstanceFleet $
--             addInstanceFleet
--
--         , requestDeleteStudio $
--             deleteStudio
--
--         , requestListStudios $
--             listStudios
--
--         , requestPutManagedScalingPolicy $
--             putManagedScalingPolicy
--
--         , requestAddInstanceGroups $
--             addInstanceGroups
--
--         , requestGetStudioSessionMapping $
--             getStudioSessionMapping
--
--         , requestDeleteSecurityConfiguration $
--             deleteSecurityConfiguration
--
--         , requestModifyInstanceFleet $
--             modifyInstanceFleet
--
--         , requestListInstanceGroups $
--             listInstanceGroups
--
--         , requestGetBlockPublicAccessConfiguration $
--             getBlockPublicAccessConfiguration
--
--         , requestModifyCluster $
--             modifyCluster
--
--         , requestPutBlockPublicAccessConfiguration $
--             putBlockPublicAccessConfiguration
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
--         , requestPutAutoScalingPolicy $
--             putAutoScalingPolicy
--
--         , requestDeleteStudioSessionMapping $
--             deleteStudioSessionMapping
--
--         , requestUpdateStudioSessionMapping $
--             updateStudioSessionMapping
--
--         , requestListClusters $
--             listClusters
--
--         , requestDescribeSecurityConfiguration $
--             describeSecurityConfiguration
--
--         , requestStopNotebookExecution $
--             stopNotebookExecution
--
--         , requestListStudioSessionMappings $
--             listStudioSessionMappings
--
--         , requestGetManagedScalingPolicy $
--             getManagedScalingPolicy
--
--         , requestListInstanceFleets $
--             listInstanceFleets
--
--         , requestRemoveManagedScalingPolicy $
--             removeManagedScalingPolicy
--
--         , requestDescribeNotebookExecution $
--             describeNotebookExecution
--
--           ]

--     , testGroup "response"
--         [ responseRunJobFlow $
--             runJobFlowResponse
--
--         , responseRemoveAutoScalingPolicy $
--             removeAutoScalingPolicyResponse
--
--         , responseCreateStudio $
--             createStudioResponse
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
--         , responseListSecurityConfigurations $
--             listSecurityConfigurationsResponse
--
--         , responseCancelSteps $
--             cancelStepsResponse
--
--         , responseListNotebookExecutions $
--             listNotebookExecutionsResponse
--
--         , responseCreateSecurityConfiguration $
--             createSecurityConfigurationResponse
--
--         , responseSetTerminationProtection $
--             setTerminationProtectionResponse
--
--         , responseAddJobFlowSteps $
--             addJobFlowStepsResponse
--
--         , responseDescribeStudio $
--             describeStudioResponse
--
--         , responseModifyInstanceGroups $
--             modifyInstanceGroupsResponse
--
--         , responseStartNotebookExecution $
--             startNotebookExecutionResponse
--
--         , responseListSteps $
--             listStepsResponse
--
--         , responseCreateStudioSessionMapping $
--             createStudioSessionMappingResponse
--
--         , responseAddInstanceFleet $
--             addInstanceFleetResponse
--
--         , responseDeleteStudio $
--             deleteStudioResponse
--
--         , responseListStudios $
--             listStudiosResponse
--
--         , responsePutManagedScalingPolicy $
--             putManagedScalingPolicyResponse
--
--         , responseAddInstanceGroups $
--             addInstanceGroupsResponse
--
--         , responseGetStudioSessionMapping $
--             getStudioSessionMappingResponse
--
--         , responseDeleteSecurityConfiguration $
--             deleteSecurityConfigurationResponse
--
--         , responseModifyInstanceFleet $
--             modifyInstanceFleetResponse
--
--         , responseListInstanceGroups $
--             listInstanceGroupsResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             getBlockPublicAccessConfigurationResponse
--
--         , responseModifyCluster $
--             modifyClusterResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             putBlockPublicAccessConfigurationResponse
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
--         , responsePutAutoScalingPolicy $
--             putAutoScalingPolicyResponse
--
--         , responseDeleteStudioSessionMapping $
--             deleteStudioSessionMappingResponse
--
--         , responseUpdateStudioSessionMapping $
--             updateStudioSessionMappingResponse
--
--         , responseListClusters $
--             listClustersResponse
--
--         , responseDescribeSecurityConfiguration $
--             describeSecurityConfigurationResponse
--
--         , responseStopNotebookExecution $
--             stopNotebookExecutionResponse
--
--         , responseListStudioSessionMappings $
--             listStudioSessionMappingsResponse
--
--         , responseGetManagedScalingPolicy $
--             getManagedScalingPolicyResponse
--
--         , responseListInstanceFleets $
--             listInstanceFleetsResponse
--
--         , responseRemoveManagedScalingPolicy $
--             removeManagedScalingPolicyResponse
--
--         , responseDescribeNotebookExecution $
--             describeNotebookExecutionResponse
--
--           ]
--     ]

-- Requests

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow =
  req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestRemoveAutoScalingPolicy :: RemoveAutoScalingPolicy -> TestTree
requestRemoveAutoScalingPolicy =
  req
    "RemoveAutoScalingPolicy"
    "fixture/RemoveAutoScalingPolicy.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio =
  req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

requestSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
requestSetVisibleToAllUsers =
  req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

requestTerminateJobFlows :: TerminateJobFlows -> TestTree
requestTerminateJobFlows =
  req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

requestDescribeStep :: DescribeStep -> TestTree
requestDescribeStep =
  req
    "DescribeStep"
    "fixture/DescribeStep.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestListSecurityConfigurations :: ListSecurityConfigurations -> TestTree
requestListSecurityConfigurations =
  req
    "ListSecurityConfigurations"
    "fixture/ListSecurityConfigurations.yaml"

requestCancelSteps :: CancelSteps -> TestTree
requestCancelSteps =
  req
    "CancelSteps"
    "fixture/CancelSteps.yaml"

requestListNotebookExecutions :: ListNotebookExecutions -> TestTree
requestListNotebookExecutions =
  req
    "ListNotebookExecutions"
    "fixture/ListNotebookExecutions.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection =
  req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps =
  req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestDescribeStudio :: DescribeStudio -> TestTree
requestDescribeStudio =
  req
    "DescribeStudio"
    "fixture/DescribeStudio.yaml"

requestModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
requestModifyInstanceGroups =
  req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

requestStartNotebookExecution :: StartNotebookExecution -> TestTree
requestStartNotebookExecution =
  req
    "StartNotebookExecution"
    "fixture/StartNotebookExecution.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps =
  req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestCreateStudioSessionMapping :: CreateStudioSessionMapping -> TestTree
requestCreateStudioSessionMapping =
  req
    "CreateStudioSessionMapping"
    "fixture/CreateStudioSessionMapping.yaml"

requestAddInstanceFleet :: AddInstanceFleet -> TestTree
requestAddInstanceFleet =
  req
    "AddInstanceFleet"
    "fixture/AddInstanceFleet.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio =
  req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios =
  req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestPutManagedScalingPolicy :: PutManagedScalingPolicy -> TestTree
requestPutManagedScalingPolicy =
  req
    "PutManagedScalingPolicy"
    "fixture/PutManagedScalingPolicy.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups =
  req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestGetStudioSessionMapping :: GetStudioSessionMapping -> TestTree
requestGetStudioSessionMapping =
  req
    "GetStudioSessionMapping"
    "fixture/GetStudioSessionMapping.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestModifyInstanceFleet :: ModifyInstanceFleet -> TestTree
requestModifyInstanceFleet =
  req
    "ModifyInstanceFleet"
    "fixture/ModifyInstanceFleet.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups =
  req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfiguration -> TestTree
requestGetBlockPublicAccessConfiguration =
  req
    "GetBlockPublicAccessConfiguration"
    "fixture/GetBlockPublicAccessConfiguration.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestPutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> TestTree
requestPutBlockPublicAccessConfiguration =
  req
    "PutBlockPublicAccessConfiguration"
    "fixture/PutBlockPublicAccessConfiguration.yaml"

requestListBootstrapActions :: ListBootstrapActions -> TestTree
requestListBootstrapActions =
  req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestPutAutoScalingPolicy :: PutAutoScalingPolicy -> TestTree
requestPutAutoScalingPolicy =
  req
    "PutAutoScalingPolicy"
    "fixture/PutAutoScalingPolicy.yaml"

requestDeleteStudioSessionMapping :: DeleteStudioSessionMapping -> TestTree
requestDeleteStudioSessionMapping =
  req
    "DeleteStudioSessionMapping"
    "fixture/DeleteStudioSessionMapping.yaml"

requestUpdateStudioSessionMapping :: UpdateStudioSessionMapping -> TestTree
requestUpdateStudioSessionMapping =
  req
    "UpdateStudioSessionMapping"
    "fixture/UpdateStudioSessionMapping.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeSecurityConfiguration :: DescribeSecurityConfiguration -> TestTree
requestDescribeSecurityConfiguration =
  req
    "DescribeSecurityConfiguration"
    "fixture/DescribeSecurityConfiguration.yaml"

requestStopNotebookExecution :: StopNotebookExecution -> TestTree
requestStopNotebookExecution =
  req
    "StopNotebookExecution"
    "fixture/StopNotebookExecution.yaml"

requestListStudioSessionMappings :: ListStudioSessionMappings -> TestTree
requestListStudioSessionMappings =
  req
    "ListStudioSessionMappings"
    "fixture/ListStudioSessionMappings.yaml"

requestGetManagedScalingPolicy :: GetManagedScalingPolicy -> TestTree
requestGetManagedScalingPolicy =
  req
    "GetManagedScalingPolicy"
    "fixture/GetManagedScalingPolicy.yaml"

requestListInstanceFleets :: ListInstanceFleets -> TestTree
requestListInstanceFleets =
  req
    "ListInstanceFleets"
    "fixture/ListInstanceFleets.yaml"

requestRemoveManagedScalingPolicy :: RemoveManagedScalingPolicy -> TestTree
requestRemoveManagedScalingPolicy =
  req
    "RemoveManagedScalingPolicy"
    "fixture/RemoveManagedScalingPolicy.yaml"

requestDescribeNotebookExecution :: DescribeNotebookExecution -> TestTree
requestDescribeNotebookExecution =
  req
    "DescribeNotebookExecution"
    "fixture/DescribeNotebookExecution.yaml"

-- Responses

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow =
  res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    emr
    (Proxy :: Proxy RunJobFlow)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy =
  res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy RemoveAutoScalingPolicy)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    emr
    (Proxy :: Proxy CreateStudio)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers =
  res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    emr
    (Proxy :: Proxy SetVisibleToAllUsers)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows =
  res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    emr
    (Proxy :: Proxy TerminateJobFlows)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep =
  res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    emr
    (Proxy :: Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    emr
    (Proxy :: Proxy RemoveTags)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    emr
    (Proxy :: Proxy DescribeCluster)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations =
  res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    emr
    (Proxy :: Proxy ListSecurityConfigurations)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps =
  res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    emr
    (Proxy :: Proxy CancelSteps)

responseListNotebookExecutions :: ListNotebookExecutionsResponse -> TestTree
responseListNotebookExecutions =
  res
    "ListNotebookExecutionsResponse"
    "fixture/ListNotebookExecutionsResponse.proto"
    emr
    (Proxy :: Proxy ListNotebookExecutions)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy CreateSecurityConfiguration)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection =
  res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    emr
    (Proxy :: Proxy SetTerminationProtection)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps =
  res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    emr
    (Proxy :: Proxy AddJobFlowSteps)

responseDescribeStudio :: DescribeStudioResponse -> TestTree
responseDescribeStudio =
  res
    "DescribeStudioResponse"
    "fixture/DescribeStudioResponse.proto"
    emr
    (Proxy :: Proxy DescribeStudio)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups =
  res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ModifyInstanceGroups)

responseStartNotebookExecution :: StartNotebookExecutionResponse -> TestTree
responseStartNotebookExecution =
  res
    "StartNotebookExecutionResponse"
    "fixture/StartNotebookExecutionResponse.proto"
    emr
    (Proxy :: Proxy StartNotebookExecution)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps =
  res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    emr
    (Proxy :: Proxy ListSteps)

responseCreateStudioSessionMapping :: CreateStudioSessionMappingResponse -> TestTree
responseCreateStudioSessionMapping =
  res
    "CreateStudioSessionMappingResponse"
    "fixture/CreateStudioSessionMappingResponse.proto"
    emr
    (Proxy :: Proxy CreateStudioSessionMapping)

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet =
  res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    emr
    (Proxy :: Proxy AddInstanceFleet)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    emr
    (Proxy :: Proxy DeleteStudio)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    emr
    (Proxy :: Proxy ListStudios)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy =
  res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy PutManagedScalingPolicy)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups =
  res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy AddInstanceGroups)

responseGetStudioSessionMapping :: GetStudioSessionMappingResponse -> TestTree
responseGetStudioSessionMapping =
  res
    "GetStudioSessionMappingResponse"
    "fixture/GetStudioSessionMappingResponse.proto"
    emr
    (Proxy :: Proxy GetStudioSessionMapping)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet =
  res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    emr
    (Proxy :: Proxy ModifyInstanceFleet)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups =
  res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ListInstanceGroups)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration =
  res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    emr
    (Proxy :: Proxy GetBlockPublicAccessConfiguration)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    emr
    (Proxy :: Proxy ModifyCluster)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration =
  res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    emr
    (Proxy :: Proxy PutBlockPublicAccessConfiguration)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions =
  res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    emr
    (Proxy :: Proxy ListBootstrapActions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    emr
    (Proxy :: Proxy AddTags)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    emr
    (Proxy :: Proxy ListInstances)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy =
  res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy PutAutoScalingPolicy)

responseDeleteStudioSessionMapping :: DeleteStudioSessionMappingResponse -> TestTree
responseDeleteStudioSessionMapping =
  res
    "DeleteStudioSessionMappingResponse"
    "fixture/DeleteStudioSessionMappingResponse.proto"
    emr
    (Proxy :: Proxy DeleteStudioSessionMapping)

responseUpdateStudioSessionMapping :: UpdateStudioSessionMappingResponse -> TestTree
responseUpdateStudioSessionMapping =
  res
    "UpdateStudioSessionMappingResponse"
    "fixture/UpdateStudioSessionMappingResponse.proto"
    emr
    (Proxy :: Proxy UpdateStudioSessionMapping)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    emr
    (Proxy :: Proxy ListClusters)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration =
  res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy DescribeSecurityConfiguration)

responseStopNotebookExecution :: StopNotebookExecutionResponse -> TestTree
responseStopNotebookExecution =
  res
    "StopNotebookExecutionResponse"
    "fixture/StopNotebookExecutionResponse.proto"
    emr
    (Proxy :: Proxy StopNotebookExecution)

responseListStudioSessionMappings :: ListStudioSessionMappingsResponse -> TestTree
responseListStudioSessionMappings =
  res
    "ListStudioSessionMappingsResponse"
    "fixture/ListStudioSessionMappingsResponse.proto"
    emr
    (Proxy :: Proxy ListStudioSessionMappings)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy =
  res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy GetManagedScalingPolicy)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets =
  res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    emr
    (Proxy :: Proxy ListInstanceFleets)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy =
  res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy RemoveManagedScalingPolicy)

responseDescribeNotebookExecution :: DescribeNotebookExecutionResponse -> TestTree
responseDescribeNotebookExecution =
  res
    "DescribeNotebookExecutionResponse"
    "fixture/DescribeNotebookExecutionResponse.proto"
    emr
    (Proxy :: Proxy DescribeNotebookExecution)
