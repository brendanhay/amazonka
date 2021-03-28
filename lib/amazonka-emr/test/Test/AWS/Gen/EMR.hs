{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--             mkRunJobFlow
--
--         , requestRemoveAutoScalingPolicy $
--             mkRemoveAutoScalingPolicy
--
--         , requestCreateStudio $
--             mkCreateStudio
--
--         , requestSetVisibleToAllUsers $
--             mkSetVisibleToAllUsers
--
--         , requestTerminateJobFlows $
--             mkTerminateJobFlows
--
--         , requestDescribeStep $
--             mkDescribeStep
--
--         , requestRemoveTags $
--             mkRemoveTags
--
--         , requestDescribeCluster $
--             mkDescribeCluster
--
--         , requestListSecurityConfigurations $
--             mkListSecurityConfigurations
--
--         , requestCancelSteps $
--             mkCancelSteps
--
--         , requestListNotebookExecutions $
--             mkListNotebookExecutions
--
--         , requestCreateSecurityConfiguration $
--             mkCreateSecurityConfiguration
--
--         , requestSetTerminationProtection $
--             mkSetTerminationProtection
--
--         , requestAddJobFlowSteps $
--             mkAddJobFlowSteps
--
--         , requestDescribeStudio $
--             mkDescribeStudio
--
--         , requestModifyInstanceGroups $
--             mkModifyInstanceGroups
--
--         , requestStartNotebookExecution $
--             mkStartNotebookExecution
--
--         , requestListSteps $
--             mkListSteps
--
--         , requestCreateStudioSessionMapping $
--             mkCreateStudioSessionMapping
--
--         , requestAddInstanceFleet $
--             mkAddInstanceFleet
--
--         , requestDeleteStudio $
--             mkDeleteStudio
--
--         , requestListStudios $
--             mkListStudios
--
--         , requestPutManagedScalingPolicy $
--             mkPutManagedScalingPolicy
--
--         , requestAddInstanceGroups $
--             mkAddInstanceGroups
--
--         , requestGetStudioSessionMapping $
--             mkGetStudioSessionMapping
--
--         , requestDeleteSecurityConfiguration $
--             mkDeleteSecurityConfiguration
--
--         , requestModifyInstanceFleet $
--             mkModifyInstanceFleet
--
--         , requestListInstanceGroups $
--             mkListInstanceGroups
--
--         , requestGetBlockPublicAccessConfiguration $
--             mkGetBlockPublicAccessConfiguration
--
--         , requestModifyCluster $
--             mkModifyCluster
--
--         , requestPutBlockPublicAccessConfiguration $
--             mkPutBlockPublicAccessConfiguration
--
--         , requestListBootstrapActions $
--             mkListBootstrapActions
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestListInstances $
--             mkListInstances
--
--         , requestPutAutoScalingPolicy $
--             mkPutAutoScalingPolicy
--
--         , requestDeleteStudioSessionMapping $
--             mkDeleteStudioSessionMapping
--
--         , requestUpdateStudioSessionMapping $
--             mkUpdateStudioSessionMapping
--
--         , requestListClusters $
--             mkListClusters
--
--         , requestDescribeSecurityConfiguration $
--             mkDescribeSecurityConfiguration
--
--         , requestStopNotebookExecution $
--             mkStopNotebookExecution
--
--         , requestListStudioSessionMappings $
--             mkListStudioSessionMappings
--
--         , requestGetManagedScalingPolicy $
--             mkGetManagedScalingPolicy
--
--         , requestListInstanceFleets $
--             mkListInstanceFleets
--
--         , requestRemoveManagedScalingPolicy $
--             mkRemoveManagedScalingPolicy
--
--         , requestDescribeNotebookExecution $
--             mkDescribeNotebookExecution
--
--           ]

--     , testGroup "response"
--         [ responseRunJobFlow $
--             mkRunJobFlowResponse
--
--         , responseRemoveAutoScalingPolicy $
--             mkRemoveAutoScalingPolicyResponse
--
--         , responseCreateStudio $
--             mkCreateStudioResponse
--
--         , responseSetVisibleToAllUsers $
--             mkSetVisibleToAllUsersResponse
--
--         , responseTerminateJobFlows $
--             mkTerminateJobFlowsResponse
--
--         , responseDescribeStep $
--             mkDescribeStepResponse
--
--         , responseRemoveTags $
--             mkRemoveTagsResponse
--
--         , responseDescribeCluster $
--             mkDescribeClusterResponse
--
--         , responseListSecurityConfigurations $
--             mkListSecurityConfigurationsResponse
--
--         , responseCancelSteps $
--             mkCancelStepsResponse
--
--         , responseListNotebookExecutions $
--             mkListNotebookExecutionsResponse
--
--         , responseCreateSecurityConfiguration $
--             mkCreateSecurityConfigurationResponse
--
--         , responseSetTerminationProtection $
--             mkSetTerminationProtectionResponse
--
--         , responseAddJobFlowSteps $
--             mkAddJobFlowStepsResponse
--
--         , responseDescribeStudio $
--             mkDescribeStudioResponse
--
--         , responseModifyInstanceGroups $
--             mkModifyInstanceGroupsResponse
--
--         , responseStartNotebookExecution $
--             mkStartNotebookExecutionResponse
--
--         , responseListSteps $
--             mkListStepsResponse
--
--         , responseCreateStudioSessionMapping $
--             mkCreateStudioSessionMappingResponse
--
--         , responseAddInstanceFleet $
--             mkAddInstanceFleetResponse
--
--         , responseDeleteStudio $
--             mkDeleteStudioResponse
--
--         , responseListStudios $
--             mkListStudiosResponse
--
--         , responsePutManagedScalingPolicy $
--             mkPutManagedScalingPolicyResponse
--
--         , responseAddInstanceGroups $
--             mkAddInstanceGroupsResponse
--
--         , responseGetStudioSessionMapping $
--             mkGetStudioSessionMappingResponse
--
--         , responseDeleteSecurityConfiguration $
--             mkDeleteSecurityConfigurationResponse
--
--         , responseModifyInstanceFleet $
--             mkModifyInstanceFleetResponse
--
--         , responseListInstanceGroups $
--             mkListInstanceGroupsResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             mkGetBlockPublicAccessConfigurationResponse
--
--         , responseModifyCluster $
--             mkModifyClusterResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             mkPutBlockPublicAccessConfigurationResponse
--
--         , responseListBootstrapActions $
--             mkListBootstrapActionsResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseListInstances $
--             mkListInstancesResponse
--
--         , responsePutAutoScalingPolicy $
--             mkPutAutoScalingPolicyResponse
--
--         , responseDeleteStudioSessionMapping $
--             mkDeleteStudioSessionMappingResponse
--
--         , responseUpdateStudioSessionMapping $
--             mkUpdateStudioSessionMappingResponse
--
--         , responseListClusters $
--             mkListClustersResponse
--
--         , responseDescribeSecurityConfiguration $
--             mkDescribeSecurityConfigurationResponse
--
--         , responseStopNotebookExecution $
--             mkStopNotebookExecutionResponse
--
--         , responseListStudioSessionMappings $
--             mkListStudioSessionMappingsResponse
--
--         , responseGetManagedScalingPolicy $
--             mkGetManagedScalingPolicyResponse
--
--         , responseListInstanceFleets $
--             mkListInstanceFleetsResponse
--
--         , responseRemoveManagedScalingPolicy $
--             mkRemoveManagedScalingPolicyResponse
--
--         , responseDescribeNotebookExecution $
--             mkDescribeNotebookExecutionResponse
--
--           ]
--     ]

-- Requests

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow = req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestRemoveAutoScalingPolicy :: RemoveAutoScalingPolicy -> TestTree
requestRemoveAutoScalingPolicy = req
    "RemoveAutoScalingPolicy"
    "fixture/RemoveAutoScalingPolicy.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio = req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

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

requestListSecurityConfigurations :: ListSecurityConfigurations -> TestTree
requestListSecurityConfigurations = req
    "ListSecurityConfigurations"
    "fixture/ListSecurityConfigurations.yaml"

requestCancelSteps :: CancelSteps -> TestTree
requestCancelSteps = req
    "CancelSteps"
    "fixture/CancelSteps.yaml"

requestListNotebookExecutions :: ListNotebookExecutions -> TestTree
requestListNotebookExecutions = req
    "ListNotebookExecutions"
    "fixture/ListNotebookExecutions.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration = req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection = req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps = req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestDescribeStudio :: DescribeStudio -> TestTree
requestDescribeStudio = req
    "DescribeStudio"
    "fixture/DescribeStudio.yaml"

requestModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
requestModifyInstanceGroups = req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

requestStartNotebookExecution :: StartNotebookExecution -> TestTree
requestStartNotebookExecution = req
    "StartNotebookExecution"
    "fixture/StartNotebookExecution.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps = req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestCreateStudioSessionMapping :: CreateStudioSessionMapping -> TestTree
requestCreateStudioSessionMapping = req
    "CreateStudioSessionMapping"
    "fixture/CreateStudioSessionMapping.yaml"

requestAddInstanceFleet :: AddInstanceFleet -> TestTree
requestAddInstanceFleet = req
    "AddInstanceFleet"
    "fixture/AddInstanceFleet.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio = req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios = req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestPutManagedScalingPolicy :: PutManagedScalingPolicy -> TestTree
requestPutManagedScalingPolicy = req
    "PutManagedScalingPolicy"
    "fixture/PutManagedScalingPolicy.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups = req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestGetStudioSessionMapping :: GetStudioSessionMapping -> TestTree
requestGetStudioSessionMapping = req
    "GetStudioSessionMapping"
    "fixture/GetStudioSessionMapping.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration = req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestModifyInstanceFleet :: ModifyInstanceFleet -> TestTree
requestModifyInstanceFleet = req
    "ModifyInstanceFleet"
    "fixture/ModifyInstanceFleet.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups = req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfiguration -> TestTree
requestGetBlockPublicAccessConfiguration = req
    "GetBlockPublicAccessConfiguration"
    "fixture/GetBlockPublicAccessConfiguration.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestPutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> TestTree
requestPutBlockPublicAccessConfiguration = req
    "PutBlockPublicAccessConfiguration"
    "fixture/PutBlockPublicAccessConfiguration.yaml"

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

requestPutAutoScalingPolicy :: PutAutoScalingPolicy -> TestTree
requestPutAutoScalingPolicy = req
    "PutAutoScalingPolicy"
    "fixture/PutAutoScalingPolicy.yaml"

requestDeleteStudioSessionMapping :: DeleteStudioSessionMapping -> TestTree
requestDeleteStudioSessionMapping = req
    "DeleteStudioSessionMapping"
    "fixture/DeleteStudioSessionMapping.yaml"

requestUpdateStudioSessionMapping :: UpdateStudioSessionMapping -> TestTree
requestUpdateStudioSessionMapping = req
    "UpdateStudioSessionMapping"
    "fixture/UpdateStudioSessionMapping.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeSecurityConfiguration :: DescribeSecurityConfiguration -> TestTree
requestDescribeSecurityConfiguration = req
    "DescribeSecurityConfiguration"
    "fixture/DescribeSecurityConfiguration.yaml"

requestStopNotebookExecution :: StopNotebookExecution -> TestTree
requestStopNotebookExecution = req
    "StopNotebookExecution"
    "fixture/StopNotebookExecution.yaml"

requestListStudioSessionMappings :: ListStudioSessionMappings -> TestTree
requestListStudioSessionMappings = req
    "ListStudioSessionMappings"
    "fixture/ListStudioSessionMappings.yaml"

requestGetManagedScalingPolicy :: GetManagedScalingPolicy -> TestTree
requestGetManagedScalingPolicy = req
    "GetManagedScalingPolicy"
    "fixture/GetManagedScalingPolicy.yaml"

requestListInstanceFleets :: ListInstanceFleets -> TestTree
requestListInstanceFleets = req
    "ListInstanceFleets"
    "fixture/ListInstanceFleets.yaml"

requestRemoveManagedScalingPolicy :: RemoveManagedScalingPolicy -> TestTree
requestRemoveManagedScalingPolicy = req
    "RemoveManagedScalingPolicy"
    "fixture/RemoveManagedScalingPolicy.yaml"

requestDescribeNotebookExecution :: DescribeNotebookExecution -> TestTree
requestDescribeNotebookExecution = req
    "DescribeNotebookExecution"
    "fixture/DescribeNotebookExecution.yaml"

-- Responses

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow = res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RunJobFlow)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy = res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveAutoScalingPolicy)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio = res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStudio)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers = res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetVisibleToAllUsers)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows = res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TerminateJobFlows)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep = res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTags)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCluster)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations = res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecurityConfigurations)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps = res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelSteps)

responseListNotebookExecutions :: ListNotebookExecutionsResponse -> TestTree
responseListNotebookExecutions = res
    "ListNotebookExecutionsResponse"
    "fixture/ListNotebookExecutionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListNotebookExecutions)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration = res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSecurityConfiguration)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection = res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetTerminationProtection)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps = res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddJobFlowSteps)

responseDescribeStudio :: DescribeStudioResponse -> TestTree
responseDescribeStudio = res
    "DescribeStudioResponse"
    "fixture/DescribeStudioResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStudio)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups = res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceGroups)

responseStartNotebookExecution :: StartNotebookExecutionResponse -> TestTree
responseStartNotebookExecution = res
    "StartNotebookExecutionResponse"
    "fixture/StartNotebookExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartNotebookExecution)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps = res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSteps)

responseCreateStudioSessionMapping :: CreateStudioSessionMappingResponse -> TestTree
responseCreateStudioSessionMapping = res
    "CreateStudioSessionMappingResponse"
    "fixture/CreateStudioSessionMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStudioSessionMapping)

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet = res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddInstanceFleet)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio = res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStudio)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios = res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStudios)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy = res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutManagedScalingPolicy)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups = res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddInstanceGroups)

responseGetStudioSessionMapping :: GetStudioSessionMappingResponse -> TestTree
responseGetStudioSessionMapping = res
    "GetStudioSessionMappingResponse"
    "fixture/GetStudioSessionMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStudioSessionMapping)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration = res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet = res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceFleet)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups = res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceGroups)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration = res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBlockPublicAccessConfiguration)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCluster)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration = res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBlockPublicAccessConfiguration)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions = res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBootstrapActions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTags)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstances)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy = res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAutoScalingPolicy)

responseDeleteStudioSessionMapping :: DeleteStudioSessionMappingResponse -> TestTree
responseDeleteStudioSessionMapping = res
    "DeleteStudioSessionMappingResponse"
    "fixture/DeleteStudioSessionMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStudioSessionMapping)

responseUpdateStudioSessionMapping :: UpdateStudioSessionMappingResponse -> TestTree
responseUpdateStudioSessionMapping = res
    "UpdateStudioSessionMappingResponse"
    "fixture/UpdateStudioSessionMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStudioSessionMapping)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListClusters)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration = res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSecurityConfiguration)

responseStopNotebookExecution :: StopNotebookExecutionResponse -> TestTree
responseStopNotebookExecution = res
    "StopNotebookExecutionResponse"
    "fixture/StopNotebookExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopNotebookExecution)

responseListStudioSessionMappings :: ListStudioSessionMappingsResponse -> TestTree
responseListStudioSessionMappings = res
    "ListStudioSessionMappingsResponse"
    "fixture/ListStudioSessionMappingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStudioSessionMappings)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy = res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetManagedScalingPolicy)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets = res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceFleets)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy = res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveManagedScalingPolicy)

responseDescribeNotebookExecution :: DescribeNotebookExecutionResponse -> TestTree
responseDescribeNotebookExecution = res
    "DescribeNotebookExecutionResponse"
    "fixture/DescribeNotebookExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNotebookExecution)
