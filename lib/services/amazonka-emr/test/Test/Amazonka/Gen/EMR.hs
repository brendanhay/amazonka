{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EMR
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EMR where

import Amazonka.EMR
import qualified Data.Proxy as Proxy
import Test.Amazonka.EMR.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRunJobFlow $
--             newRunJobFlow
--
--         , requestRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicy
--
--         , requestCreateStudio $
--             newCreateStudio
--
--         , requestSetVisibleToAllUsers $
--             newSetVisibleToAllUsers
--
--         , requestTerminateJobFlows $
--             newTerminateJobFlows
--
--         , requestDescribeStep $
--             newDescribeStep
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestListSecurityConfigurations $
--             newListSecurityConfigurations
--
--         , requestCancelSteps $
--             newCancelSteps
--
--         , requestListNotebookExecutions $
--             newListNotebookExecutions
--
--         , requestPutAutoTerminationPolicy $
--             newPutAutoTerminationPolicy
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--         , requestDescribeReleaseLabel $
--             newDescribeReleaseLabel
--
--         , requestSetTerminationProtection $
--             newSetTerminationProtection
--
--         , requestAddJobFlowSteps $
--             newAddJobFlowSteps
--
--         , requestDescribeStudio $
--             newDescribeStudio
--
--         , requestModifyInstanceGroups $
--             newModifyInstanceGroups
--
--         , requestStartNotebookExecution $
--             newStartNotebookExecution
--
--         , requestListSteps $
--             newListSteps
--
--         , requestListReleaseLabels $
--             newListReleaseLabels
--
--         , requestCreateStudioSessionMapping $
--             newCreateStudioSessionMapping
--
--         , requestAddInstanceFleet $
--             newAddInstanceFleet
--
--         , requestDeleteStudio $
--             newDeleteStudio
--
--         , requestUpdateStudio $
--             newUpdateStudio
--
--         , requestListStudios $
--             newListStudios
--
--         , requestPutManagedScalingPolicy $
--             newPutManagedScalingPolicy
--
--         , requestAddInstanceGroups $
--             newAddInstanceGroups
--
--         , requestGetStudioSessionMapping $
--             newGetStudioSessionMapping
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestModifyInstanceFleet $
--             newModifyInstanceFleet
--
--         , requestListInstanceGroups $
--             newListInstanceGroups
--
--         , requestGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfiguration
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestGetAutoTerminationPolicy $
--             newGetAutoTerminationPolicy
--
--         , requestPutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfiguration
--
--         , requestListBootstrapActions $
--             newListBootstrapActions
--
--         , requestRemoveAutoTerminationPolicy $
--             newRemoveAutoTerminationPolicy
--
--         , requestAddTags $
--             newAddTags
--
--         , requestListInstances $
--             newListInstances
--
--         , requestPutAutoScalingPolicy $
--             newPutAutoScalingPolicy
--
--         , requestDeleteStudioSessionMapping $
--             newDeleteStudioSessionMapping
--
--         , requestUpdateStudioSessionMapping $
--             newUpdateStudioSessionMapping
--
--         , requestListClusters $
--             newListClusters
--
--         , requestDescribeSecurityConfiguration $
--             newDescribeSecurityConfiguration
--
--         , requestStopNotebookExecution $
--             newStopNotebookExecution
--
--         , requestListStudioSessionMappings $
--             newListStudioSessionMappings
--
--         , requestGetManagedScalingPolicy $
--             newGetManagedScalingPolicy
--
--         , requestListInstanceFleets $
--             newListInstanceFleets
--
--         , requestRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicy
--
--         , requestDescribeNotebookExecution $
--             newDescribeNotebookExecution
--
--           ]

--     , testGroup "response"
--         [ responseRunJobFlow $
--             newRunJobFlowResponse
--
--         , responseRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicyResponse
--
--         , responseCreateStudio $
--             newCreateStudioResponse
--
--         , responseSetVisibleToAllUsers $
--             newSetVisibleToAllUsersResponse
--
--         , responseTerminateJobFlows $
--             newTerminateJobFlowsResponse
--
--         , responseDescribeStep $
--             newDescribeStepResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseListSecurityConfigurations $
--             newListSecurityConfigurationsResponse
--
--         , responseCancelSteps $
--             newCancelStepsResponse
--
--         , responseListNotebookExecutions $
--             newListNotebookExecutionsResponse
--
--         , responsePutAutoTerminationPolicy $
--             newPutAutoTerminationPolicyResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--         , responseDescribeReleaseLabel $
--             newDescribeReleaseLabelResponse
--
--         , responseSetTerminationProtection $
--             newSetTerminationProtectionResponse
--
--         , responseAddJobFlowSteps $
--             newAddJobFlowStepsResponse
--
--         , responseDescribeStudio $
--             newDescribeStudioResponse
--
--         , responseModifyInstanceGroups $
--             newModifyInstanceGroupsResponse
--
--         , responseStartNotebookExecution $
--             newStartNotebookExecutionResponse
--
--         , responseListSteps $
--             newListStepsResponse
--
--         , responseListReleaseLabels $
--             newListReleaseLabelsResponse
--
--         , responseCreateStudioSessionMapping $
--             newCreateStudioSessionMappingResponse
--
--         , responseAddInstanceFleet $
--             newAddInstanceFleetResponse
--
--         , responseDeleteStudio $
--             newDeleteStudioResponse
--
--         , responseUpdateStudio $
--             newUpdateStudioResponse
--
--         , responseListStudios $
--             newListStudiosResponse
--
--         , responsePutManagedScalingPolicy $
--             newPutManagedScalingPolicyResponse
--
--         , responseAddInstanceGroups $
--             newAddInstanceGroupsResponse
--
--         , responseGetStudioSessionMapping $
--             newGetStudioSessionMappingResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseModifyInstanceFleet $
--             newModifyInstanceFleetResponse
--
--         , responseListInstanceGroups $
--             newListInstanceGroupsResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfigurationResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseGetAutoTerminationPolicy $
--             newGetAutoTerminationPolicyResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfigurationResponse
--
--         , responseListBootstrapActions $
--             newListBootstrapActionsResponse
--
--         , responseRemoveAutoTerminationPolicy $
--             newRemoveAutoTerminationPolicyResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responsePutAutoScalingPolicy $
--             newPutAutoScalingPolicyResponse
--
--         , responseDeleteStudioSessionMapping $
--             newDeleteStudioSessionMappingResponse
--
--         , responseUpdateStudioSessionMapping $
--             newUpdateStudioSessionMappingResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseDescribeSecurityConfiguration $
--             newDescribeSecurityConfigurationResponse
--
--         , responseStopNotebookExecution $
--             newStopNotebookExecutionResponse
--
--         , responseListStudioSessionMappings $
--             newListStudioSessionMappingsResponse
--
--         , responseGetManagedScalingPolicy $
--             newGetManagedScalingPolicyResponse
--
--         , responseListInstanceFleets $
--             newListInstanceFleetsResponse
--
--         , responseRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicyResponse
--
--         , responseDescribeNotebookExecution $
--             newDescribeNotebookExecutionResponse
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

requestPutAutoTerminationPolicy :: PutAutoTerminationPolicy -> TestTree
requestPutAutoTerminationPolicy =
  req
    "PutAutoTerminationPolicy"
    "fixture/PutAutoTerminationPolicy.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestDescribeReleaseLabel :: DescribeReleaseLabel -> TestTree
requestDescribeReleaseLabel =
  req
    "DescribeReleaseLabel"
    "fixture/DescribeReleaseLabel.yaml"

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

requestListReleaseLabels :: ListReleaseLabels -> TestTree
requestListReleaseLabels =
  req
    "ListReleaseLabels"
    "fixture/ListReleaseLabels.yaml"

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

requestUpdateStudio :: UpdateStudio -> TestTree
requestUpdateStudio =
  req
    "UpdateStudio"
    "fixture/UpdateStudio.yaml"

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

requestGetAutoTerminationPolicy :: GetAutoTerminationPolicy -> TestTree
requestGetAutoTerminationPolicy =
  req
    "GetAutoTerminationPolicy"
    "fixture/GetAutoTerminationPolicy.yaml"

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

requestRemoveAutoTerminationPolicy :: RemoveAutoTerminationPolicy -> TestTree
requestRemoveAutoTerminationPolicy =
  req
    "RemoveAutoTerminationPolicy"
    "fixture/RemoveAutoTerminationPolicy.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunJobFlow)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy =
  res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAutoScalingPolicy)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudio)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers =
  res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVisibleToAllUsers)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows =
  res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateJobFlows)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep =
  res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations =
  res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityConfigurations)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps =
  res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSteps)

responseListNotebookExecutions :: ListNotebookExecutionsResponse -> TestTree
responseListNotebookExecutions =
  res
    "ListNotebookExecutionsResponse"
    "fixture/ListNotebookExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookExecutions)

responsePutAutoTerminationPolicy :: PutAutoTerminationPolicyResponse -> TestTree
responsePutAutoTerminationPolicy =
  res
    "PutAutoTerminationPolicyResponse"
    "fixture/PutAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAutoTerminationPolicy)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityConfiguration)

responseDescribeReleaseLabel :: DescribeReleaseLabelResponse -> TestTree
responseDescribeReleaseLabel =
  res
    "DescribeReleaseLabelResponse"
    "fixture/DescribeReleaseLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReleaseLabel)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection =
  res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTerminationProtection)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps =
  res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddJobFlowSteps)

responseDescribeStudio :: DescribeStudioResponse -> TestTree
responseDescribeStudio =
  res
    "DescribeStudioResponse"
    "fixture/DescribeStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStudio)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups =
  res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceGroups)

responseStartNotebookExecution :: StartNotebookExecutionResponse -> TestTree
responseStartNotebookExecution =
  res
    "StartNotebookExecutionResponse"
    "fixture/StartNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNotebookExecution)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps =
  res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSteps)

responseListReleaseLabels :: ListReleaseLabelsResponse -> TestTree
responseListReleaseLabels =
  res
    "ListReleaseLabelsResponse"
    "fixture/ListReleaseLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReleaseLabels)

responseCreateStudioSessionMapping :: CreateStudioSessionMappingResponse -> TestTree
responseCreateStudioSessionMapping =
  res
    "CreateStudioSessionMappingResponse"
    "fixture/CreateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioSessionMapping)

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet =
  res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddInstanceFleet)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudio)

responseUpdateStudio :: UpdateStudioResponse -> TestTree
responseUpdateStudio =
  res
    "UpdateStudioResponse"
    "fixture/UpdateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudio)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudios)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy =
  res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutManagedScalingPolicy)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups =
  res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddInstanceGroups)

responseGetStudioSessionMapping :: GetStudioSessionMappingResponse -> TestTree
responseGetStudioSessionMapping =
  res
    "GetStudioSessionMappingResponse"
    "fixture/GetStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioSessionMapping)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityConfiguration)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet =
  res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceFleet)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups =
  res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceGroups)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration =
  res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlockPublicAccessConfiguration)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseGetAutoTerminationPolicy :: GetAutoTerminationPolicyResponse -> TestTree
responseGetAutoTerminationPolicy =
  res
    "GetAutoTerminationPolicyResponse"
    "fixture/GetAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoTerminationPolicy)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration =
  res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBlockPublicAccessConfiguration)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions =
  res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBootstrapActions)

responseRemoveAutoTerminationPolicy :: RemoveAutoTerminationPolicyResponse -> TestTree
responseRemoveAutoTerminationPolicy =
  res
    "RemoveAutoTerminationPolicyResponse"
    "fixture/RemoveAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAutoTerminationPolicy)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy =
  res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAutoScalingPolicy)

responseDeleteStudioSessionMapping :: DeleteStudioSessionMappingResponse -> TestTree
responseDeleteStudioSessionMapping =
  res
    "DeleteStudioSessionMappingResponse"
    "fixture/DeleteStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioSessionMapping)

responseUpdateStudioSessionMapping :: UpdateStudioSessionMappingResponse -> TestTree
responseUpdateStudioSessionMapping =
  res
    "UpdateStudioSessionMappingResponse"
    "fixture/UpdateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudioSessionMapping)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration =
  res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityConfiguration)

responseStopNotebookExecution :: StopNotebookExecutionResponse -> TestTree
responseStopNotebookExecution =
  res
    "StopNotebookExecutionResponse"
    "fixture/StopNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopNotebookExecution)

responseListStudioSessionMappings :: ListStudioSessionMappingsResponse -> TestTree
responseListStudioSessionMappings =
  res
    "ListStudioSessionMappingsResponse"
    "fixture/ListStudioSessionMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioSessionMappings)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy =
  res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedScalingPolicy)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets =
  res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceFleets)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy =
  res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveManagedScalingPolicy)

responseDescribeNotebookExecution :: DescribeNotebookExecutionResponse -> TestTree
responseDescribeNotebookExecution =
  res
    "DescribeNotebookExecutionResponse"
    "fixture/DescribeNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookExecution)
