{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeStep $
--             newDescribeStep
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestListSecurityConfigurations $
--             newListSecurityConfigurations
--
--         , requestModifyInstanceFleet $
--             newModifyInstanceFleet
--
--         , requestRunJobFlow $
--             newRunJobFlow
--
--         , requestGetStudioSessionMapping $
--             newGetStudioSessionMapping
--
--         , requestSetVisibleToAllUsers $
--             newSetVisibleToAllUsers
--
--         , requestAddInstanceGroups $
--             newAddInstanceGroups
--
--         , requestCreateStudio $
--             newCreateStudio
--
--         , requestDeleteStudio $
--             newDeleteStudio
--
--         , requestUpdateStudio $
--             newUpdateStudio
--
--         , requestListInstanceFleets $
--             newListInstanceFleets
--
--         , requestRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicy
--
--         , requestDescribeSecurityConfiguration $
--             newDescribeSecurityConfiguration
--
--         , requestStartNotebookExecution $
--             newStartNotebookExecution
--
--         , requestListStudioSessionMappings $
--             newListStudioSessionMappings
--
--         , requestStopNotebookExecution $
--             newStopNotebookExecution
--
--         , requestListInstances $
--             newListInstances
--
--         , requestAddTags $
--             newAddTags
--
--         , requestAddJobFlowSteps $
--             newAddJobFlowSteps
--
--         , requestListBootstrapActions $
--             newListBootstrapActions
--
--         , requestListNotebookExecutions $
--             newListNotebookExecutions
--
--         , requestGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfiguration
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestTerminateJobFlows $
--             newTerminateJobFlows
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestCancelSteps $
--             newCancelSteps
--
--         , requestListInstanceGroups $
--             newListInstanceGroups
--
--         , requestRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicy
--
--         , requestPutManagedScalingPolicy $
--             newPutManagedScalingPolicy
--
--         , requestListStudios $
--             newListStudios
--
--         , requestAddInstanceFleet $
--             newAddInstanceFleet
--
--         , requestCreateStudioSessionMapping $
--             newCreateStudioSessionMapping
--
--         , requestGetManagedScalingPolicy $
--             newGetManagedScalingPolicy
--
--         , requestDescribeNotebookExecution $
--             newDescribeNotebookExecution
--
--         , requestUpdateStudioSessionMapping $
--             newUpdateStudioSessionMapping
--
--         , requestDeleteStudioSessionMapping $
--             newDeleteStudioSessionMapping
--
--         , requestListSteps $
--             newListSteps
--
--         , requestListClusters $
--             newListClusters
--
--         , requestPutAutoScalingPolicy $
--             newPutAutoScalingPolicy
--
--         , requestSetTerminationProtection $
--             newSetTerminationProtection
--
--         , requestPutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfiguration
--
--         , requestDescribeStudio $
--             newDescribeStudio
--
--         , requestModifyInstanceGroups $
--             newModifyInstanceGroups
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseDescribeStep $
--             newDescribeStepResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseListSecurityConfigurations $
--             newListSecurityConfigurationsResponse
--
--         , responseModifyInstanceFleet $
--             newModifyInstanceFleetResponse
--
--         , responseRunJobFlow $
--             newRunJobFlowResponse
--
--         , responseGetStudioSessionMapping $
--             newGetStudioSessionMappingResponse
--
--         , responseSetVisibleToAllUsers $
--             newSetVisibleToAllUsersResponse
--
--         , responseAddInstanceGroups $
--             newAddInstanceGroupsResponse
--
--         , responseCreateStudio $
--             newCreateStudioResponse
--
--         , responseDeleteStudio $
--             newDeleteStudioResponse
--
--         , responseUpdateStudio $
--             newUpdateStudioResponse
--
--         , responseListInstanceFleets $
--             newListInstanceFleetsResponse
--
--         , responseRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicyResponse
--
--         , responseDescribeSecurityConfiguration $
--             newDescribeSecurityConfigurationResponse
--
--         , responseStartNotebookExecution $
--             newStartNotebookExecutionResponse
--
--         , responseListStudioSessionMappings $
--             newListStudioSessionMappingsResponse
--
--         , responseStopNotebookExecution $
--             newStopNotebookExecutionResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseAddJobFlowSteps $
--             newAddJobFlowStepsResponse
--
--         , responseListBootstrapActions $
--             newListBootstrapActionsResponse
--
--         , responseListNotebookExecutions $
--             newListNotebookExecutionsResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfigurationResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseTerminateJobFlows $
--             newTerminateJobFlowsResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseCancelSteps $
--             newCancelStepsResponse
--
--         , responseListInstanceGroups $
--             newListInstanceGroupsResponse
--
--         , responseRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicyResponse
--
--         , responsePutManagedScalingPolicy $
--             newPutManagedScalingPolicyResponse
--
--         , responseListStudios $
--             newListStudiosResponse
--
--         , responseAddInstanceFleet $
--             newAddInstanceFleetResponse
--
--         , responseCreateStudioSessionMapping $
--             newCreateStudioSessionMappingResponse
--
--         , responseGetManagedScalingPolicy $
--             newGetManagedScalingPolicyResponse
--
--         , responseDescribeNotebookExecution $
--             newDescribeNotebookExecutionResponse
--
--         , responseUpdateStudioSessionMapping $
--             newUpdateStudioSessionMappingResponse
--
--         , responseDeleteStudioSessionMapping $
--             newDeleteStudioSessionMappingResponse
--
--         , responseListSteps $
--             newListStepsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responsePutAutoScalingPolicy $
--             newPutAutoScalingPolicyResponse
--
--         , responseSetTerminationProtection $
--             newSetTerminationProtectionResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfigurationResponse
--
--         , responseDescribeStudio $
--             newDescribeStudioResponse
--
--         , responseModifyInstanceGroups $
--             newModifyInstanceGroupsResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--           ]
--     ]

-- Requests

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

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestListSecurityConfigurations :: ListSecurityConfigurations -> TestTree
requestListSecurityConfigurations =
  req
    "ListSecurityConfigurations"
    "fixture/ListSecurityConfigurations.yaml"

requestModifyInstanceFleet :: ModifyInstanceFleet -> TestTree
requestModifyInstanceFleet =
  req
    "ModifyInstanceFleet"
    "fixture/ModifyInstanceFleet.yaml"

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow =
  req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestGetStudioSessionMapping :: GetStudioSessionMapping -> TestTree
requestGetStudioSessionMapping =
  req
    "GetStudioSessionMapping"
    "fixture/GetStudioSessionMapping.yaml"

requestSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
requestSetVisibleToAllUsers =
  req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups =
  req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio =
  req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio =
  req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestUpdateStudio :: UpdateStudio -> TestTree
requestUpdateStudio =
  req
    "UpdateStudio"
    "fixture/UpdateStudio.yaml"

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

requestDescribeSecurityConfiguration :: DescribeSecurityConfiguration -> TestTree
requestDescribeSecurityConfiguration =
  req
    "DescribeSecurityConfiguration"
    "fixture/DescribeSecurityConfiguration.yaml"

requestStartNotebookExecution :: StartNotebookExecution -> TestTree
requestStartNotebookExecution =
  req
    "StartNotebookExecution"
    "fixture/StartNotebookExecution.yaml"

requestListStudioSessionMappings :: ListStudioSessionMappings -> TestTree
requestListStudioSessionMappings =
  req
    "ListStudioSessionMappings"
    "fixture/ListStudioSessionMappings.yaml"

requestStopNotebookExecution :: StopNotebookExecution -> TestTree
requestStopNotebookExecution =
  req
    "StopNotebookExecution"
    "fixture/StopNotebookExecution.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps =
  req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestListBootstrapActions :: ListBootstrapActions -> TestTree
requestListBootstrapActions =
  req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

requestListNotebookExecutions :: ListNotebookExecutions -> TestTree
requestListNotebookExecutions =
  req
    "ListNotebookExecutions"
    "fixture/ListNotebookExecutions.yaml"

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

requestTerminateJobFlows :: TerminateJobFlows -> TestTree
requestTerminateJobFlows =
  req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestCancelSteps :: CancelSteps -> TestTree
requestCancelSteps =
  req
    "CancelSteps"
    "fixture/CancelSteps.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups =
  req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestRemoveAutoScalingPolicy :: RemoveAutoScalingPolicy -> TestTree
requestRemoveAutoScalingPolicy =
  req
    "RemoveAutoScalingPolicy"
    "fixture/RemoveAutoScalingPolicy.yaml"

requestPutManagedScalingPolicy :: PutManagedScalingPolicy -> TestTree
requestPutManagedScalingPolicy =
  req
    "PutManagedScalingPolicy"
    "fixture/PutManagedScalingPolicy.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios =
  req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestAddInstanceFleet :: AddInstanceFleet -> TestTree
requestAddInstanceFleet =
  req
    "AddInstanceFleet"
    "fixture/AddInstanceFleet.yaml"

requestCreateStudioSessionMapping :: CreateStudioSessionMapping -> TestTree
requestCreateStudioSessionMapping =
  req
    "CreateStudioSessionMapping"
    "fixture/CreateStudioSessionMapping.yaml"

requestGetManagedScalingPolicy :: GetManagedScalingPolicy -> TestTree
requestGetManagedScalingPolicy =
  req
    "GetManagedScalingPolicy"
    "fixture/GetManagedScalingPolicy.yaml"

requestDescribeNotebookExecution :: DescribeNotebookExecution -> TestTree
requestDescribeNotebookExecution =
  req
    "DescribeNotebookExecution"
    "fixture/DescribeNotebookExecution.yaml"

requestUpdateStudioSessionMapping :: UpdateStudioSessionMapping -> TestTree
requestUpdateStudioSessionMapping =
  req
    "UpdateStudioSessionMapping"
    "fixture/UpdateStudioSessionMapping.yaml"

requestDeleteStudioSessionMapping :: DeleteStudioSessionMapping -> TestTree
requestDeleteStudioSessionMapping =
  req
    "DeleteStudioSessionMapping"
    "fixture/DeleteStudioSessionMapping.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps =
  req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestPutAutoScalingPolicy :: PutAutoScalingPolicy -> TestTree
requestPutAutoScalingPolicy =
  req
    "PutAutoScalingPolicy"
    "fixture/PutAutoScalingPolicy.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection =
  req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestPutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> TestTree
requestPutBlockPublicAccessConfiguration =
  req
    "PutBlockPublicAccessConfiguration"
    "fixture/PutBlockPublicAccessConfiguration.yaml"

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

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

-- Responses

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep =
  res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations =
  res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityConfigurations)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet =
  res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceFleet)

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow =
  res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    defaultService
    (Proxy :: Proxy RunJobFlow)

responseGetStudioSessionMapping :: GetStudioSessionMappingResponse -> TestTree
responseGetStudioSessionMapping =
  res
    "GetStudioSessionMappingResponse"
    "fixture/GetStudioSessionMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetStudioSessionMapping)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers =
  res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    defaultService
    (Proxy :: Proxy SetVisibleToAllUsers)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups =
  res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy AddInstanceGroups)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStudio)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStudio)

responseUpdateStudio :: UpdateStudioResponse -> TestTree
responseUpdateStudio =
  res
    "UpdateStudioResponse"
    "fixture/UpdateStudioResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStudio)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets =
  res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceFleets)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy =
  res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveManagedScalingPolicy)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration =
  res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityConfiguration)

responseStartNotebookExecution :: StartNotebookExecutionResponse -> TestTree
responseStartNotebookExecution =
  res
    "StartNotebookExecutionResponse"
    "fixture/StartNotebookExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartNotebookExecution)

responseListStudioSessionMappings :: ListStudioSessionMappingsResponse -> TestTree
responseListStudioSessionMappings =
  res
    "ListStudioSessionMappingsResponse"
    "fixture/ListStudioSessionMappingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStudioSessionMappings)

responseStopNotebookExecution :: StopNotebookExecutionResponse -> TestTree
responseStopNotebookExecution =
  res
    "StopNotebookExecutionResponse"
    "fixture/StopNotebookExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopNotebookExecution)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstances)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps =
  res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    defaultService
    (Proxy :: Proxy AddJobFlowSteps)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions =
  res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBootstrapActions)

responseListNotebookExecutions :: ListNotebookExecutionsResponse -> TestTree
responseListNotebookExecutions =
  res
    "ListNotebookExecutionsResponse"
    "fixture/ListNotebookExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotebookExecutions)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration =
  res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlockPublicAccessConfiguration)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCluster)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows =
  res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateJobFlows)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCluster)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps =
  res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSteps)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups =
  res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceGroups)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy =
  res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAutoScalingPolicy)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy =
  res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutManagedScalingPolicy)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    defaultService
    (Proxy :: Proxy ListStudios)

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet =
  res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy AddInstanceFleet)

responseCreateStudioSessionMapping :: CreateStudioSessionMappingResponse -> TestTree
responseCreateStudioSessionMapping =
  res
    "CreateStudioSessionMappingResponse"
    "fixture/CreateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStudioSessionMapping)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy =
  res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetManagedScalingPolicy)

responseDescribeNotebookExecution :: DescribeNotebookExecutionResponse -> TestTree
responseDescribeNotebookExecution =
  res
    "DescribeNotebookExecutionResponse"
    "fixture/DescribeNotebookExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotebookExecution)

responseUpdateStudioSessionMapping :: UpdateStudioSessionMappingResponse -> TestTree
responseUpdateStudioSessionMapping =
  res
    "UpdateStudioSessionMappingResponse"
    "fixture/UpdateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStudioSessionMapping)

responseDeleteStudioSessionMapping :: DeleteStudioSessionMappingResponse -> TestTree
responseDeleteStudioSessionMapping =
  res
    "DeleteStudioSessionMappingResponse"
    "fixture/DeleteStudioSessionMappingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStudioSessionMapping)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps =
  res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSteps)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy =
  res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutAutoScalingPolicy)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection =
  res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy SetTerminationProtection)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration =
  res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBlockPublicAccessConfiguration)

responseDescribeStudio :: DescribeStudioResponse -> TestTree
responseDescribeStudio =
  res
    "DescribeStudioResponse"
    "fixture/DescribeStudioResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStudio)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups =
  res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceGroups)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityConfiguration)
