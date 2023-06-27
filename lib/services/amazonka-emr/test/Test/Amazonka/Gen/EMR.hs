{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EMR
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAddInstanceFleet $
--             newAddInstanceFleet
--
--         , requestAddInstanceGroups $
--             newAddInstanceGroups
--
--         , requestAddJobFlowSteps $
--             newAddJobFlowSteps
--
--         , requestAddTags $
--             newAddTags
--
--         , requestCancelSteps $
--             newCancelSteps
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--         , requestCreateStudio $
--             newCreateStudio
--
--         , requestCreateStudioSessionMapping $
--             newCreateStudioSessionMapping
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestDeleteStudio $
--             newDeleteStudio
--
--         , requestDeleteStudioSessionMapping $
--             newDeleteStudioSessionMapping
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDescribeNotebookExecution $
--             newDescribeNotebookExecution
--
--         , requestDescribeReleaseLabel $
--             newDescribeReleaseLabel
--
--         , requestDescribeSecurityConfiguration $
--             newDescribeSecurityConfiguration
--
--         , requestDescribeStep $
--             newDescribeStep
--
--         , requestDescribeStudio $
--             newDescribeStudio
--
--         , requestGetAutoTerminationPolicy $
--             newGetAutoTerminationPolicy
--
--         , requestGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfiguration
--
--         , requestGetClusterSessionCredentials $
--             newGetClusterSessionCredentials
--
--         , requestGetManagedScalingPolicy $
--             newGetManagedScalingPolicy
--
--         , requestGetStudioSessionMapping $
--             newGetStudioSessionMapping
--
--         , requestListBootstrapActions $
--             newListBootstrapActions
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListInstanceFleets $
--             newListInstanceFleets
--
--         , requestListInstanceGroups $
--             newListInstanceGroups
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListNotebookExecutions $
--             newListNotebookExecutions
--
--         , requestListReleaseLabels $
--             newListReleaseLabels
--
--         , requestListSecurityConfigurations $
--             newListSecurityConfigurations
--
--         , requestListSteps $
--             newListSteps
--
--         , requestListStudioSessionMappings $
--             newListStudioSessionMappings
--
--         , requestListStudios $
--             newListStudios
--
--         , requestListSupportedInstanceTypes $
--             newListSupportedInstanceTypes
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestModifyInstanceFleet $
--             newModifyInstanceFleet
--
--         , requestModifyInstanceGroups $
--             newModifyInstanceGroups
--
--         , requestPutAutoScalingPolicy $
--             newPutAutoScalingPolicy
--
--         , requestPutAutoTerminationPolicy $
--             newPutAutoTerminationPolicy
--
--         , requestPutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfiguration
--
--         , requestPutManagedScalingPolicy $
--             newPutManagedScalingPolicy
--
--         , requestRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicy
--
--         , requestRemoveAutoTerminationPolicy $
--             newRemoveAutoTerminationPolicy
--
--         , requestRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicy
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestRunJobFlow $
--             newRunJobFlow
--
--         , requestSetTerminationProtection $
--             newSetTerminationProtection
--
--         , requestSetVisibleToAllUsers $
--             newSetVisibleToAllUsers
--
--         , requestStartNotebookExecution $
--             newStartNotebookExecution
--
--         , requestStopNotebookExecution $
--             newStopNotebookExecution
--
--         , requestTerminateJobFlows $
--             newTerminateJobFlows
--
--         , requestUpdateStudio $
--             newUpdateStudio
--
--         , requestUpdateStudioSessionMapping $
--             newUpdateStudioSessionMapping
--
--           ]

--     , testGroup "response"
--         [ responseAddInstanceFleet $
--             newAddInstanceFleetResponse
--
--         , responseAddInstanceGroups $
--             newAddInstanceGroupsResponse
--
--         , responseAddJobFlowSteps $
--             newAddJobFlowStepsResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseCancelSteps $
--             newCancelStepsResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--         , responseCreateStudio $
--             newCreateStudioResponse
--
--         , responseCreateStudioSessionMapping $
--             newCreateStudioSessionMappingResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseDeleteStudio $
--             newDeleteStudioResponse
--
--         , responseDeleteStudioSessionMapping $
--             newDeleteStudioSessionMappingResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDescribeNotebookExecution $
--             newDescribeNotebookExecutionResponse
--
--         , responseDescribeReleaseLabel $
--             newDescribeReleaseLabelResponse
--
--         , responseDescribeSecurityConfiguration $
--             newDescribeSecurityConfigurationResponse
--
--         , responseDescribeStep $
--             newDescribeStepResponse
--
--         , responseDescribeStudio $
--             newDescribeStudioResponse
--
--         , responseGetAutoTerminationPolicy $
--             newGetAutoTerminationPolicyResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             newGetBlockPublicAccessConfigurationResponse
--
--         , responseGetClusterSessionCredentials $
--             newGetClusterSessionCredentialsResponse
--
--         , responseGetManagedScalingPolicy $
--             newGetManagedScalingPolicyResponse
--
--         , responseGetStudioSessionMapping $
--             newGetStudioSessionMappingResponse
--
--         , responseListBootstrapActions $
--             newListBootstrapActionsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListInstanceFleets $
--             newListInstanceFleetsResponse
--
--         , responseListInstanceGroups $
--             newListInstanceGroupsResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListNotebookExecutions $
--             newListNotebookExecutionsResponse
--
--         , responseListReleaseLabels $
--             newListReleaseLabelsResponse
--
--         , responseListSecurityConfigurations $
--             newListSecurityConfigurationsResponse
--
--         , responseListSteps $
--             newListStepsResponse
--
--         , responseListStudioSessionMappings $
--             newListStudioSessionMappingsResponse
--
--         , responseListStudios $
--             newListStudiosResponse
--
--         , responseListSupportedInstanceTypes $
--             newListSupportedInstanceTypesResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseModifyInstanceFleet $
--             newModifyInstanceFleetResponse
--
--         , responseModifyInstanceGroups $
--             newModifyInstanceGroupsResponse
--
--         , responsePutAutoScalingPolicy $
--             newPutAutoScalingPolicyResponse
--
--         , responsePutAutoTerminationPolicy $
--             newPutAutoTerminationPolicyResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             newPutBlockPublicAccessConfigurationResponse
--
--         , responsePutManagedScalingPolicy $
--             newPutManagedScalingPolicyResponse
--
--         , responseRemoveAutoScalingPolicy $
--             newRemoveAutoScalingPolicyResponse
--
--         , responseRemoveAutoTerminationPolicy $
--             newRemoveAutoTerminationPolicyResponse
--
--         , responseRemoveManagedScalingPolicy $
--             newRemoveManagedScalingPolicyResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseRunJobFlow $
--             newRunJobFlowResponse
--
--         , responseSetTerminationProtection $
--             newSetTerminationProtectionResponse
--
--         , responseSetVisibleToAllUsers $
--             newSetVisibleToAllUsersResponse
--
--         , responseStartNotebookExecution $
--             newStartNotebookExecutionResponse
--
--         , responseStopNotebookExecution $
--             newStopNotebookExecutionResponse
--
--         , responseTerminateJobFlows $
--             newTerminateJobFlowsResponse
--
--         , responseUpdateStudio $
--             newUpdateStudioResponse
--
--         , responseUpdateStudioSessionMapping $
--             newUpdateStudioSessionMappingResponse
--
--           ]
--     ]

-- Requests

requestAddInstanceFleet :: AddInstanceFleet -> TestTree
requestAddInstanceFleet =
  req
    "AddInstanceFleet"
    "fixture/AddInstanceFleet.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups =
  req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps =
  req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestCancelSteps :: CancelSteps -> TestTree
requestCancelSteps =
  req
    "CancelSteps"
    "fixture/CancelSteps.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio =
  req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

requestCreateStudioSessionMapping :: CreateStudioSessionMapping -> TestTree
requestCreateStudioSessionMapping =
  req
    "CreateStudioSessionMapping"
    "fixture/CreateStudioSessionMapping.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio =
  req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestDeleteStudioSessionMapping :: DeleteStudioSessionMapping -> TestTree
requestDeleteStudioSessionMapping =
  req
    "DeleteStudioSessionMapping"
    "fixture/DeleteStudioSessionMapping.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDescribeNotebookExecution :: DescribeNotebookExecution -> TestTree
requestDescribeNotebookExecution =
  req
    "DescribeNotebookExecution"
    "fixture/DescribeNotebookExecution.yaml"

requestDescribeReleaseLabel :: DescribeReleaseLabel -> TestTree
requestDescribeReleaseLabel =
  req
    "DescribeReleaseLabel"
    "fixture/DescribeReleaseLabel.yaml"

requestDescribeSecurityConfiguration :: DescribeSecurityConfiguration -> TestTree
requestDescribeSecurityConfiguration =
  req
    "DescribeSecurityConfiguration"
    "fixture/DescribeSecurityConfiguration.yaml"

requestDescribeStep :: DescribeStep -> TestTree
requestDescribeStep =
  req
    "DescribeStep"
    "fixture/DescribeStep.yaml"

requestDescribeStudio :: DescribeStudio -> TestTree
requestDescribeStudio =
  req
    "DescribeStudio"
    "fixture/DescribeStudio.yaml"

requestGetAutoTerminationPolicy :: GetAutoTerminationPolicy -> TestTree
requestGetAutoTerminationPolicy =
  req
    "GetAutoTerminationPolicy"
    "fixture/GetAutoTerminationPolicy.yaml"

requestGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfiguration -> TestTree
requestGetBlockPublicAccessConfiguration =
  req
    "GetBlockPublicAccessConfiguration"
    "fixture/GetBlockPublicAccessConfiguration.yaml"

requestGetClusterSessionCredentials :: GetClusterSessionCredentials -> TestTree
requestGetClusterSessionCredentials =
  req
    "GetClusterSessionCredentials"
    "fixture/GetClusterSessionCredentials.yaml"

requestGetManagedScalingPolicy :: GetManagedScalingPolicy -> TestTree
requestGetManagedScalingPolicy =
  req
    "GetManagedScalingPolicy"
    "fixture/GetManagedScalingPolicy.yaml"

requestGetStudioSessionMapping :: GetStudioSessionMapping -> TestTree
requestGetStudioSessionMapping =
  req
    "GetStudioSessionMapping"
    "fixture/GetStudioSessionMapping.yaml"

requestListBootstrapActions :: ListBootstrapActions -> TestTree
requestListBootstrapActions =
  req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListInstanceFleets :: ListInstanceFleets -> TestTree
requestListInstanceFleets =
  req
    "ListInstanceFleets"
    "fixture/ListInstanceFleets.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups =
  req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListNotebookExecutions :: ListNotebookExecutions -> TestTree
requestListNotebookExecutions =
  req
    "ListNotebookExecutions"
    "fixture/ListNotebookExecutions.yaml"

requestListReleaseLabels :: ListReleaseLabels -> TestTree
requestListReleaseLabels =
  req
    "ListReleaseLabels"
    "fixture/ListReleaseLabels.yaml"

requestListSecurityConfigurations :: ListSecurityConfigurations -> TestTree
requestListSecurityConfigurations =
  req
    "ListSecurityConfigurations"
    "fixture/ListSecurityConfigurations.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps =
  req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestListStudioSessionMappings :: ListStudioSessionMappings -> TestTree
requestListStudioSessionMappings =
  req
    "ListStudioSessionMappings"
    "fixture/ListStudioSessionMappings.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios =
  req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestListSupportedInstanceTypes :: ListSupportedInstanceTypes -> TestTree
requestListSupportedInstanceTypes =
  req
    "ListSupportedInstanceTypes"
    "fixture/ListSupportedInstanceTypes.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestModifyInstanceFleet :: ModifyInstanceFleet -> TestTree
requestModifyInstanceFleet =
  req
    "ModifyInstanceFleet"
    "fixture/ModifyInstanceFleet.yaml"

requestModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
requestModifyInstanceGroups =
  req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

requestPutAutoScalingPolicy :: PutAutoScalingPolicy -> TestTree
requestPutAutoScalingPolicy =
  req
    "PutAutoScalingPolicy"
    "fixture/PutAutoScalingPolicy.yaml"

requestPutAutoTerminationPolicy :: PutAutoTerminationPolicy -> TestTree
requestPutAutoTerminationPolicy =
  req
    "PutAutoTerminationPolicy"
    "fixture/PutAutoTerminationPolicy.yaml"

requestPutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> TestTree
requestPutBlockPublicAccessConfiguration =
  req
    "PutBlockPublicAccessConfiguration"
    "fixture/PutBlockPublicAccessConfiguration.yaml"

requestPutManagedScalingPolicy :: PutManagedScalingPolicy -> TestTree
requestPutManagedScalingPolicy =
  req
    "PutManagedScalingPolicy"
    "fixture/PutManagedScalingPolicy.yaml"

requestRemoveAutoScalingPolicy :: RemoveAutoScalingPolicy -> TestTree
requestRemoveAutoScalingPolicy =
  req
    "RemoveAutoScalingPolicy"
    "fixture/RemoveAutoScalingPolicy.yaml"

requestRemoveAutoTerminationPolicy :: RemoveAutoTerminationPolicy -> TestTree
requestRemoveAutoTerminationPolicy =
  req
    "RemoveAutoTerminationPolicy"
    "fixture/RemoveAutoTerminationPolicy.yaml"

requestRemoveManagedScalingPolicy :: RemoveManagedScalingPolicy -> TestTree
requestRemoveManagedScalingPolicy =
  req
    "RemoveManagedScalingPolicy"
    "fixture/RemoveManagedScalingPolicy.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow =
  req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection =
  req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
requestSetVisibleToAllUsers =
  req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

requestStartNotebookExecution :: StartNotebookExecution -> TestTree
requestStartNotebookExecution =
  req
    "StartNotebookExecution"
    "fixture/StartNotebookExecution.yaml"

requestStopNotebookExecution :: StopNotebookExecution -> TestTree
requestStopNotebookExecution =
  req
    "StopNotebookExecution"
    "fixture/StopNotebookExecution.yaml"

requestTerminateJobFlows :: TerminateJobFlows -> TestTree
requestTerminateJobFlows =
  req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

requestUpdateStudio :: UpdateStudio -> TestTree
requestUpdateStudio =
  req
    "UpdateStudio"
    "fixture/UpdateStudio.yaml"

requestUpdateStudioSessionMapping :: UpdateStudioSessionMapping -> TestTree
requestUpdateStudioSessionMapping =
  req
    "UpdateStudioSessionMapping"
    "fixture/UpdateStudioSessionMapping.yaml"

-- Responses

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet =
  res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddInstanceFleet)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups =
  res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddInstanceGroups)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps =
  res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddJobFlowSteps)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps =
  res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSteps)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityConfiguration)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudio)

responseCreateStudioSessionMapping :: CreateStudioSessionMappingResponse -> TestTree
responseCreateStudioSessionMapping =
  res
    "CreateStudioSessionMappingResponse"
    "fixture/CreateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioSessionMapping)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityConfiguration)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudio)

responseDeleteStudioSessionMapping :: DeleteStudioSessionMappingResponse -> TestTree
responseDeleteStudioSessionMapping =
  res
    "DeleteStudioSessionMappingResponse"
    "fixture/DeleteStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioSessionMapping)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDescribeNotebookExecution :: DescribeNotebookExecutionResponse -> TestTree
responseDescribeNotebookExecution =
  res
    "DescribeNotebookExecutionResponse"
    "fixture/DescribeNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookExecution)

responseDescribeReleaseLabel :: DescribeReleaseLabelResponse -> TestTree
responseDescribeReleaseLabel =
  res
    "DescribeReleaseLabelResponse"
    "fixture/DescribeReleaseLabelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReleaseLabel)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration =
  res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityConfiguration)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep =
  res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStep)

responseDescribeStudio :: DescribeStudioResponse -> TestTree
responseDescribeStudio =
  res
    "DescribeStudioResponse"
    "fixture/DescribeStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStudio)

responseGetAutoTerminationPolicy :: GetAutoTerminationPolicyResponse -> TestTree
responseGetAutoTerminationPolicy =
  res
    "GetAutoTerminationPolicyResponse"
    "fixture/GetAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoTerminationPolicy)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration =
  res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlockPublicAccessConfiguration)

responseGetClusterSessionCredentials :: GetClusterSessionCredentialsResponse -> TestTree
responseGetClusterSessionCredentials =
  res
    "GetClusterSessionCredentialsResponse"
    "fixture/GetClusterSessionCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClusterSessionCredentials)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy =
  res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedScalingPolicy)

responseGetStudioSessionMapping :: GetStudioSessionMappingResponse -> TestTree
responseGetStudioSessionMapping =
  res
    "GetStudioSessionMappingResponse"
    "fixture/GetStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioSessionMapping)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions =
  res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBootstrapActions)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets =
  res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceFleets)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups =
  res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceGroups)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseListNotebookExecutions :: ListNotebookExecutionsResponse -> TestTree
responseListNotebookExecutions =
  res
    "ListNotebookExecutionsResponse"
    "fixture/ListNotebookExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookExecutions)

responseListReleaseLabels :: ListReleaseLabelsResponse -> TestTree
responseListReleaseLabels =
  res
    "ListReleaseLabelsResponse"
    "fixture/ListReleaseLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReleaseLabels)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations =
  res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityConfigurations)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps =
  res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSteps)

responseListStudioSessionMappings :: ListStudioSessionMappingsResponse -> TestTree
responseListStudioSessionMappings =
  res
    "ListStudioSessionMappingsResponse"
    "fixture/ListStudioSessionMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioSessionMappings)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudios)

responseListSupportedInstanceTypes :: ListSupportedInstanceTypesResponse -> TestTree
responseListSupportedInstanceTypes =
  res
    "ListSupportedInstanceTypesResponse"
    "fixture/ListSupportedInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSupportedInstanceTypes)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet =
  res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceFleet)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups =
  res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyInstanceGroups)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy =
  res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAutoScalingPolicy)

responsePutAutoTerminationPolicy :: PutAutoTerminationPolicyResponse -> TestTree
responsePutAutoTerminationPolicy =
  res
    "PutAutoTerminationPolicyResponse"
    "fixture/PutAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAutoTerminationPolicy)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration =
  res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBlockPublicAccessConfiguration)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy =
  res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutManagedScalingPolicy)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy =
  res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAutoScalingPolicy)

responseRemoveAutoTerminationPolicy :: RemoveAutoTerminationPolicyResponse -> TestTree
responseRemoveAutoTerminationPolicy =
  res
    "RemoveAutoTerminationPolicyResponse"
    "fixture/RemoveAutoTerminationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAutoTerminationPolicy)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy =
  res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveManagedScalingPolicy)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow =
  res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunJobFlow)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection =
  res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTerminationProtection)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers =
  res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVisibleToAllUsers)

responseStartNotebookExecution :: StartNotebookExecutionResponse -> TestTree
responseStartNotebookExecution =
  res
    "StartNotebookExecutionResponse"
    "fixture/StartNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNotebookExecution)

responseStopNotebookExecution :: StopNotebookExecutionResponse -> TestTree
responseStopNotebookExecution =
  res
    "StopNotebookExecutionResponse"
    "fixture/StopNotebookExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopNotebookExecution)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows =
  res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateJobFlows)

responseUpdateStudio :: UpdateStudioResponse -> TestTree
responseUpdateStudio =
  res
    "UpdateStudioResponse"
    "fixture/UpdateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudio)

responseUpdateStudioSessionMapping :: UpdateStudioSessionMappingResponse -> TestTree
responseUpdateStudioSessionMapping =
  res
    "UpdateStudioSessionMappingResponse"
    "fixture/UpdateStudioSessionMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudioSessionMapping)
