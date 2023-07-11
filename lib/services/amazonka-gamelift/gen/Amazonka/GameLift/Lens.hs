{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Lens
  ( -- * Operations

    -- ** AcceptMatch
    acceptMatch_ticketId,
    acceptMatch_playerIds,
    acceptMatch_acceptanceType,
    acceptMatchResponse_httpStatus,

    -- ** ClaimGameServer
    claimGameServer_gameServerData,
    claimGameServer_gameServerId,
    claimGameServer_gameServerGroupName,
    claimGameServerResponse_gameServer,
    claimGameServerResponse_httpStatus,

    -- ** CreateAlias
    createAlias_description,
    createAlias_tags,
    createAlias_name,
    createAlias_routingStrategy,
    createAliasResponse_alias,
    createAliasResponse_httpStatus,

    -- ** CreateBuild
    createBuild_name,
    createBuild_operatingSystem,
    createBuild_serverSdkVersion,
    createBuild_storageLocation,
    createBuild_tags,
    createBuild_version,
    createBuildResponse_build,
    createBuildResponse_storageLocation,
    createBuildResponse_uploadCredentials,
    createBuildResponse_httpStatus,

    -- ** CreateFleet
    createFleet_anywhereConfiguration,
    createFleet_buildId,
    createFleet_certificateConfiguration,
    createFleet_computeType,
    createFleet_description,
    createFleet_eC2InboundPermissions,
    createFleet_eC2InstanceType,
    createFleet_fleetType,
    createFleet_instanceRoleArn,
    createFleet_locations,
    createFleet_logPaths,
    createFleet_metricGroups,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_peerVpcAwsAccountId,
    createFleet_peerVpcId,
    createFleet_resourceCreationLimitPolicy,
    createFleet_runtimeConfiguration,
    createFleet_scriptId,
    createFleet_serverLaunchParameters,
    createFleet_serverLaunchPath,
    createFleet_tags,
    createFleet_name,
    createFleetResponse_fleetAttributes,
    createFleetResponse_locationStates,
    createFleetResponse_httpStatus,

    -- ** CreateFleetLocations
    createFleetLocations_fleetId,
    createFleetLocations_locations,
    createFleetLocationsResponse_fleetArn,
    createFleetLocationsResponse_fleetId,
    createFleetLocationsResponse_locationStates,
    createFleetLocationsResponse_httpStatus,

    -- ** CreateGameServerGroup
    createGameServerGroup_autoScalingPolicy,
    createGameServerGroup_balancingStrategy,
    createGameServerGroup_gameServerProtectionPolicy,
    createGameServerGroup_tags,
    createGameServerGroup_vpcSubnets,
    createGameServerGroup_gameServerGroupName,
    createGameServerGroup_roleArn,
    createGameServerGroup_minSize,
    createGameServerGroup_maxSize,
    createGameServerGroup_launchTemplate,
    createGameServerGroup_instanceDefinitions,
    createGameServerGroupResponse_gameServerGroup,
    createGameServerGroupResponse_httpStatus,

    -- ** CreateGameSession
    createGameSession_aliasId,
    createGameSession_creatorId,
    createGameSession_fleetId,
    createGameSession_gameProperties,
    createGameSession_gameSessionData,
    createGameSession_gameSessionId,
    createGameSession_idempotencyToken,
    createGameSession_location,
    createGameSession_name,
    createGameSession_maximumPlayerSessionCount,
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,

    -- ** CreateGameSessionQueue
    createGameSessionQueue_customEventData,
    createGameSessionQueue_destinations,
    createGameSessionQueue_filterConfiguration,
    createGameSessionQueue_notificationTarget,
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_priorityConfiguration,
    createGameSessionQueue_tags,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_name,
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,

    -- ** CreateLocation
    createLocation_tags,
    createLocation_locationName,
    createLocationResponse_location,
    createLocationResponse_httpStatus,

    -- ** CreateMatchmakingConfiguration
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_name,
    createMatchmakingConfiguration_requestTimeoutSeconds,
    createMatchmakingConfiguration_acceptanceRequired,
    createMatchmakingConfiguration_ruleSetName,
    createMatchmakingConfigurationResponse_configuration,
    createMatchmakingConfigurationResponse_httpStatus,

    -- ** CreateMatchmakingRuleSet
    createMatchmakingRuleSet_tags,
    createMatchmakingRuleSet_name,
    createMatchmakingRuleSet_ruleSetBody,
    createMatchmakingRuleSetResponse_httpStatus,
    createMatchmakingRuleSetResponse_ruleSet,

    -- ** CreatePlayerSession
    createPlayerSession_playerData,
    createPlayerSession_gameSessionId,
    createPlayerSession_playerId,
    createPlayerSessionResponse_playerSession,
    createPlayerSessionResponse_httpStatus,

    -- ** CreatePlayerSessions
    createPlayerSessions_playerDataMap,
    createPlayerSessions_gameSessionId,
    createPlayerSessions_playerIds,
    createPlayerSessionsResponse_playerSessions,
    createPlayerSessionsResponse_httpStatus,

    -- ** CreateScript
    createScript_name,
    createScript_storageLocation,
    createScript_tags,
    createScript_version,
    createScript_zipFile,
    createScriptResponse_script,
    createScriptResponse_httpStatus,

    -- ** CreateVpcPeeringAuthorization
    createVpcPeeringAuthorization_gameLiftAwsAccountId,
    createVpcPeeringAuthorization_peerVpcId,
    createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization,
    createVpcPeeringAuthorizationResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_fleetId,
    createVpcPeeringConnection_peerVpcAwsAccountId,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_aliasId,

    -- ** DeleteBuild
    deleteBuild_buildId,

    -- ** DeleteFleet
    deleteFleet_fleetId,

    -- ** DeleteFleetLocations
    deleteFleetLocations_fleetId,
    deleteFleetLocations_locations,
    deleteFleetLocationsResponse_fleetArn,
    deleteFleetLocationsResponse_fleetId,
    deleteFleetLocationsResponse_locationStates,
    deleteFleetLocationsResponse_httpStatus,

    -- ** DeleteGameServerGroup
    deleteGameServerGroup_deleteOption,
    deleteGameServerGroup_gameServerGroupName,
    deleteGameServerGroupResponse_gameServerGroup,
    deleteGameServerGroupResponse_httpStatus,

    -- ** DeleteGameSessionQueue
    deleteGameSessionQueue_name,
    deleteGameSessionQueueResponse_httpStatus,

    -- ** DeleteLocation
    deleteLocation_locationName,
    deleteLocationResponse_httpStatus,

    -- ** DeleteMatchmakingConfiguration
    deleteMatchmakingConfiguration_name,
    deleteMatchmakingConfigurationResponse_httpStatus,

    -- ** DeleteMatchmakingRuleSet
    deleteMatchmakingRuleSet_name,
    deleteMatchmakingRuleSetResponse_httpStatus,

    -- ** DeleteScalingPolicy
    deleteScalingPolicy_name,
    deleteScalingPolicy_fleetId,

    -- ** DeleteScript
    deleteScript_scriptId,

    -- ** DeleteVpcPeeringAuthorization
    deleteVpcPeeringAuthorization_gameLiftAwsAccountId,
    deleteVpcPeeringAuthorization_peerVpcId,
    deleteVpcPeeringAuthorizationResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_fleetId,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** DeregisterCompute
    deregisterCompute_fleetId,
    deregisterCompute_computeName,
    deregisterComputeResponse_httpStatus,

    -- ** DeregisterGameServer
    deregisterGameServer_gameServerGroupName,
    deregisterGameServer_gameServerId,

    -- ** DescribeAlias
    describeAlias_aliasId,
    describeAliasResponse_alias,
    describeAliasResponse_httpStatus,

    -- ** DescribeBuild
    describeBuild_buildId,
    describeBuildResponse_build,
    describeBuildResponse_httpStatus,

    -- ** DescribeCompute
    describeCompute_fleetId,
    describeCompute_computeName,
    describeComputeResponse_compute,
    describeComputeResponse_httpStatus,

    -- ** DescribeEC2InstanceLimits
    describeEC2InstanceLimits_eC2InstanceType,
    describeEC2InstanceLimits_location,
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,

    -- ** DescribeFleetAttributes
    describeFleetAttributes_fleetIds,
    describeFleetAttributes_limit,
    describeFleetAttributes_nextToken,
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_httpStatus,

    -- ** DescribeFleetCapacity
    describeFleetCapacity_fleetIds,
    describeFleetCapacity_limit,
    describeFleetCapacity_nextToken,
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_httpStatus,

    -- ** DescribeFleetEvents
    describeFleetEvents_endTime,
    describeFleetEvents_limit,
    describeFleetEvents_nextToken,
    describeFleetEvents_startTime,
    describeFleetEvents_fleetId,
    describeFleetEventsResponse_events,
    describeFleetEventsResponse_nextToken,
    describeFleetEventsResponse_httpStatus,

    -- ** DescribeFleetLocationAttributes
    describeFleetLocationAttributes_limit,
    describeFleetLocationAttributes_locations,
    describeFleetLocationAttributes_nextToken,
    describeFleetLocationAttributes_fleetId,
    describeFleetLocationAttributesResponse_fleetArn,
    describeFleetLocationAttributesResponse_fleetId,
    describeFleetLocationAttributesResponse_locationAttributes,
    describeFleetLocationAttributesResponse_nextToken,
    describeFleetLocationAttributesResponse_httpStatus,

    -- ** DescribeFleetLocationCapacity
    describeFleetLocationCapacity_fleetId,
    describeFleetLocationCapacity_location,
    describeFleetLocationCapacityResponse_fleetCapacity,
    describeFleetLocationCapacityResponse_httpStatus,

    -- ** DescribeFleetLocationUtilization
    describeFleetLocationUtilization_fleetId,
    describeFleetLocationUtilization_location,
    describeFleetLocationUtilizationResponse_fleetUtilization,
    describeFleetLocationUtilizationResponse_httpStatus,

    -- ** DescribeFleetPortSettings
    describeFleetPortSettings_location,
    describeFleetPortSettings_fleetId,
    describeFleetPortSettingsResponse_fleetArn,
    describeFleetPortSettingsResponse_fleetId,
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_location,
    describeFleetPortSettingsResponse_updateStatus,
    describeFleetPortSettingsResponse_httpStatus,

    -- ** DescribeFleetUtilization
    describeFleetUtilization_fleetIds,
    describeFleetUtilization_limit,
    describeFleetUtilization_nextToken,
    describeFleetUtilizationResponse_fleetUtilization,
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_httpStatus,

    -- ** DescribeGameServer
    describeGameServer_gameServerGroupName,
    describeGameServer_gameServerId,
    describeGameServerResponse_gameServer,
    describeGameServerResponse_httpStatus,

    -- ** DescribeGameServerGroup
    describeGameServerGroup_gameServerGroupName,
    describeGameServerGroupResponse_gameServerGroup,
    describeGameServerGroupResponse_httpStatus,

    -- ** DescribeGameServerInstances
    describeGameServerInstances_instanceIds,
    describeGameServerInstances_limit,
    describeGameServerInstances_nextToken,
    describeGameServerInstances_gameServerGroupName,
    describeGameServerInstancesResponse_gameServerInstances,
    describeGameServerInstancesResponse_nextToken,
    describeGameServerInstancesResponse_httpStatus,

    -- ** DescribeGameSessionDetails
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_limit,
    describeGameSessionDetails_location,
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_statusFilter,
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_httpStatus,

    -- ** DescribeGameSessionPlacement
    describeGameSessionPlacement_placementId,
    describeGameSessionPlacementResponse_gameSessionPlacement,
    describeGameSessionPlacementResponse_httpStatus,

    -- ** DescribeGameSessionQueues
    describeGameSessionQueues_limit,
    describeGameSessionQueues_names,
    describeGameSessionQueues_nextToken,
    describeGameSessionQueuesResponse_gameSessionQueues,
    describeGameSessionQueuesResponse_nextToken,
    describeGameSessionQueuesResponse_httpStatus,

    -- ** DescribeGameSessions
    describeGameSessions_aliasId,
    describeGameSessions_fleetId,
    describeGameSessions_gameSessionId,
    describeGameSessions_limit,
    describeGameSessions_location,
    describeGameSessions_nextToken,
    describeGameSessions_statusFilter,
    describeGameSessionsResponse_gameSessions,
    describeGameSessionsResponse_nextToken,
    describeGameSessionsResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_instanceId,
    describeInstances_limit,
    describeInstances_location,
    describeInstances_nextToken,
    describeInstances_fleetId,
    describeInstancesResponse_instances,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_httpStatus,

    -- ** DescribeMatchmaking
    describeMatchmaking_ticketIds,
    describeMatchmakingResponse_ticketList,
    describeMatchmakingResponse_httpStatus,

    -- ** DescribeMatchmakingConfigurations
    describeMatchmakingConfigurations_limit,
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_ruleSetName,
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_httpStatus,

    -- ** DescribeMatchmakingRuleSets
    describeMatchmakingRuleSets_limit,
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_nextToken,
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,

    -- ** DescribePlayerSessions
    describePlayerSessions_gameSessionId,
    describePlayerSessions_limit,
    describePlayerSessions_nextToken,
    describePlayerSessions_playerId,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_playerSessionStatusFilter,
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,

    -- ** DescribeRuntimeConfiguration
    describeRuntimeConfiguration_fleetId,
    describeRuntimeConfigurationResponse_runtimeConfiguration,
    describeRuntimeConfigurationResponse_httpStatus,

    -- ** DescribeScalingPolicies
    describeScalingPolicies_limit,
    describeScalingPolicies_location,
    describeScalingPolicies_nextToken,
    describeScalingPolicies_statusFilter,
    describeScalingPolicies_fleetId,
    describeScalingPoliciesResponse_nextToken,
    describeScalingPoliciesResponse_scalingPolicies,
    describeScalingPoliciesResponse_httpStatus,

    -- ** DescribeScript
    describeScript_scriptId,
    describeScriptResponse_script,
    describeScriptResponse_httpStatus,

    -- ** DescribeVpcPeeringAuthorizations
    describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations,
    describeVpcPeeringAuthorizationsResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_fleetId,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** GetComputeAccess
    getComputeAccess_fleetId,
    getComputeAccess_computeName,
    getComputeAccessResponse_computeArn,
    getComputeAccessResponse_computeName,
    getComputeAccessResponse_credentials,
    getComputeAccessResponse_fleetArn,
    getComputeAccessResponse_fleetId,
    getComputeAccessResponse_httpStatus,

    -- ** GetComputeAuthToken
    getComputeAuthToken_fleetId,
    getComputeAuthToken_computeName,
    getComputeAuthTokenResponse_authToken,
    getComputeAuthTokenResponse_computeArn,
    getComputeAuthTokenResponse_computeName,
    getComputeAuthTokenResponse_expirationTimestamp,
    getComputeAuthTokenResponse_fleetArn,
    getComputeAuthTokenResponse_fleetId,
    getComputeAuthTokenResponse_httpStatus,

    -- ** GetGameSessionLogUrl
    getGameSessionLogUrl_gameSessionId,
    getGameSessionLogUrlResponse_preSignedUrl,
    getGameSessionLogUrlResponse_httpStatus,

    -- ** GetInstanceAccess
    getInstanceAccess_fleetId,
    getInstanceAccess_instanceId,
    getInstanceAccessResponse_instanceAccess,
    getInstanceAccessResponse_httpStatus,

    -- ** ListAliases
    listAliases_limit,
    listAliases_name,
    listAliases_nextToken,
    listAliases_routingStrategyType,
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_limit,
    listBuilds_nextToken,
    listBuilds_status,
    listBuildsResponse_builds,
    listBuildsResponse_nextToken,
    listBuildsResponse_httpStatus,

    -- ** ListCompute
    listCompute_limit,
    listCompute_location,
    listCompute_nextToken,
    listCompute_fleetId,
    listComputeResponse_computeList,
    listComputeResponse_nextToken,
    listComputeResponse_httpStatus,

    -- ** ListFleets
    listFleets_buildId,
    listFleets_limit,
    listFleets_nextToken,
    listFleets_scriptId,
    listFleetsResponse_fleetIds,
    listFleetsResponse_nextToken,
    listFleetsResponse_httpStatus,

    -- ** ListGameServerGroups
    listGameServerGroups_limit,
    listGameServerGroups_nextToken,
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_httpStatus,

    -- ** ListGameServers
    listGameServers_limit,
    listGameServers_nextToken,
    listGameServers_sortOrder,
    listGameServers_gameServerGroupName,
    listGameServersResponse_gameServers,
    listGameServersResponse_nextToken,
    listGameServersResponse_httpStatus,

    -- ** ListLocations
    listLocations_filters,
    listLocations_limit,
    listLocations_nextToken,
    listLocationsResponse_locations,
    listLocationsResponse_nextToken,
    listLocationsResponse_httpStatus,

    -- ** ListScripts
    listScripts_limit,
    listScripts_nextToken,
    listScriptsResponse_nextToken,
    listScriptsResponse_scripts,
    listScriptsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_comparisonOperator,
    putScalingPolicy_evaluationPeriods,
    putScalingPolicy_policyType,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_scalingAdjustmentType,
    putScalingPolicy_targetConfiguration,
    putScalingPolicy_threshold,
    putScalingPolicy_name,
    putScalingPolicy_fleetId,
    putScalingPolicy_metricName,
    putScalingPolicyResponse_name,
    putScalingPolicyResponse_httpStatus,

    -- ** RegisterCompute
    registerCompute_certificatePath,
    registerCompute_dnsName,
    registerCompute_ipAddress,
    registerCompute_location,
    registerCompute_fleetId,
    registerCompute_computeName,
    registerComputeResponse_compute,
    registerComputeResponse_httpStatus,

    -- ** RegisterGameServer
    registerGameServer_connectionInfo,
    registerGameServer_gameServerData,
    registerGameServer_gameServerGroupName,
    registerGameServer_gameServerId,
    registerGameServer_instanceId,
    registerGameServerResponse_gameServer,
    registerGameServerResponse_httpStatus,

    -- ** RequestUploadCredentials
    requestUploadCredentials_buildId,
    requestUploadCredentialsResponse_storageLocation,
    requestUploadCredentialsResponse_uploadCredentials,
    requestUploadCredentialsResponse_httpStatus,

    -- ** ResolveAlias
    resolveAlias_aliasId,
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_fleetId,
    resolveAliasResponse_httpStatus,

    -- ** ResumeGameServerGroup
    resumeGameServerGroup_gameServerGroupName,
    resumeGameServerGroup_resumeActions,
    resumeGameServerGroupResponse_gameServerGroup,
    resumeGameServerGroupResponse_httpStatus,

    -- ** SearchGameSessions
    searchGameSessions_aliasId,
    searchGameSessions_filterExpression,
    searchGameSessions_fleetId,
    searchGameSessions_limit,
    searchGameSessions_location,
    searchGameSessions_nextToken,
    searchGameSessions_sortExpression,
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_httpStatus,

    -- ** StartFleetActions
    startFleetActions_location,
    startFleetActions_fleetId,
    startFleetActions_actions,
    startFleetActionsResponse_fleetArn,
    startFleetActionsResponse_fleetId,
    startFleetActionsResponse_httpStatus,

    -- ** StartGameSessionPlacement
    startGameSessionPlacement_desiredPlayerSessions,
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_playerLatencies,
    startGameSessionPlacement_placementId,
    startGameSessionPlacement_gameSessionQueueName,
    startGameSessionPlacement_maximumPlayerSessionCount,
    startGameSessionPlacementResponse_gameSessionPlacement,
    startGameSessionPlacementResponse_httpStatus,

    -- ** StartMatchBackfill
    startMatchBackfill_gameSessionArn,
    startMatchBackfill_ticketId,
    startMatchBackfill_configurationName,
    startMatchBackfill_players,
    startMatchBackfillResponse_matchmakingTicket,
    startMatchBackfillResponse_httpStatus,

    -- ** StartMatchmaking
    startMatchmaking_ticketId,
    startMatchmaking_configurationName,
    startMatchmaking_players,
    startMatchmakingResponse_matchmakingTicket,
    startMatchmakingResponse_httpStatus,

    -- ** StopFleetActions
    stopFleetActions_location,
    stopFleetActions_fleetId,
    stopFleetActions_actions,
    stopFleetActionsResponse_fleetArn,
    stopFleetActionsResponse_fleetId,
    stopFleetActionsResponse_httpStatus,

    -- ** StopGameSessionPlacement
    stopGameSessionPlacement_placementId,
    stopGameSessionPlacementResponse_gameSessionPlacement,
    stopGameSessionPlacementResponse_httpStatus,

    -- ** StopMatchmaking
    stopMatchmaking_ticketId,
    stopMatchmakingResponse_httpStatus,

    -- ** SuspendGameServerGroup
    suspendGameServerGroup_gameServerGroupName,
    suspendGameServerGroup_suspendActions,
    suspendGameServerGroupResponse_gameServerGroup,
    suspendGameServerGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAlias
    updateAlias_description,
    updateAlias_name,
    updateAlias_routingStrategy,
    updateAlias_aliasId,
    updateAliasResponse_alias,
    updateAliasResponse_httpStatus,

    -- ** UpdateBuild
    updateBuild_name,
    updateBuild_version,
    updateBuild_buildId,
    updateBuildResponse_build,
    updateBuildResponse_httpStatus,

    -- ** UpdateFleetAttributes
    updateFleetAttributes_anywhereConfiguration,
    updateFleetAttributes_description,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_name,
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_fleetId,
    updateFleetAttributesResponse_fleetArn,
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,

    -- ** UpdateFleetCapacity
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_location,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_minSize,
    updateFleetCapacity_fleetId,
    updateFleetCapacityResponse_fleetArn,
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_location,
    updateFleetCapacityResponse_httpStatus,

    -- ** UpdateFleetPortSettings
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_fleetId,
    updateFleetPortSettingsResponse_fleetArn,
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,

    -- ** UpdateGameServer
    updateGameServer_gameServerData,
    updateGameServer_healthCheck,
    updateGameServer_utilizationStatus,
    updateGameServer_gameServerGroupName,
    updateGameServer_gameServerId,
    updateGameServerResponse_gameServer,
    updateGameServerResponse_httpStatus,

    -- ** UpdateGameServerGroup
    updateGameServerGroup_balancingStrategy,
    updateGameServerGroup_gameServerProtectionPolicy,
    updateGameServerGroup_instanceDefinitions,
    updateGameServerGroup_roleArn,
    updateGameServerGroup_gameServerGroupName,
    updateGameServerGroupResponse_gameServerGroup,
    updateGameServerGroupResponse_httpStatus,

    -- ** UpdateGameSession
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_name,
    updateGameSession_playerSessionCreationPolicy,
    updateGameSession_protectionPolicy,
    updateGameSession_gameSessionId,
    updateGameSessionResponse_gameSession,
    updateGameSessionResponse_httpStatus,

    -- ** UpdateGameSessionQueue
    updateGameSessionQueue_customEventData,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_filterConfiguration,
    updateGameSessionQueue_notificationTarget,
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_priorityConfiguration,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_name,
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,

    -- ** UpdateMatchmakingConfiguration
    updateMatchmakingConfiguration_acceptanceRequired,
    updateMatchmakingConfiguration_acceptanceTimeoutSeconds,
    updateMatchmakingConfiguration_additionalPlayerCount,
    updateMatchmakingConfiguration_backfillMode,
    updateMatchmakingConfiguration_customEventData,
    updateMatchmakingConfiguration_description,
    updateMatchmakingConfiguration_flexMatchMode,
    updateMatchmakingConfiguration_gameProperties,
    updateMatchmakingConfiguration_gameSessionData,
    updateMatchmakingConfiguration_gameSessionQueueArns,
    updateMatchmakingConfiguration_notificationTarget,
    updateMatchmakingConfiguration_requestTimeoutSeconds,
    updateMatchmakingConfiguration_ruleSetName,
    updateMatchmakingConfiguration_name,
    updateMatchmakingConfigurationResponse_configuration,
    updateMatchmakingConfigurationResponse_httpStatus,

    -- ** UpdateRuntimeConfiguration
    updateRuntimeConfiguration_fleetId,
    updateRuntimeConfiguration_runtimeConfiguration,
    updateRuntimeConfigurationResponse_runtimeConfiguration,
    updateRuntimeConfigurationResponse_httpStatus,

    -- ** UpdateScript
    updateScript_name,
    updateScript_storageLocation,
    updateScript_version,
    updateScript_zipFile,
    updateScript_scriptId,
    updateScriptResponse_script,
    updateScriptResponse_httpStatus,

    -- ** ValidateMatchmakingRuleSet
    validateMatchmakingRuleSet_ruleSetBody,
    validateMatchmakingRuleSetResponse_valid,
    validateMatchmakingRuleSetResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_aliasArn,
    alias_aliasId,
    alias_creationTime,
    alias_description,
    alias_lastUpdatedTime,
    alias_name,
    alias_routingStrategy,

    -- ** AnywhereConfiguration
    anywhereConfiguration_cost,

    -- ** AttributeValue
    attributeValue_n,
    attributeValue_s,
    attributeValue_sdm,
    attributeValue_sl,

    -- ** AwsCredentials
    awsCredentials_accessKeyId,
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,

    -- ** Build
    build_buildArn,
    build_buildId,
    build_creationTime,
    build_name,
    build_operatingSystem,
    build_serverSdkVersion,
    build_sizeOnDisk,
    build_status,
    build_version,

    -- ** CertificateConfiguration
    certificateConfiguration_certificateType,

    -- ** Compute
    compute_computeArn,
    compute_computeName,
    compute_computeStatus,
    compute_creationTime,
    compute_dnsName,
    compute_fleetArn,
    compute_fleetId,
    compute_gameLiftServiceSdkEndpoint,
    compute_ipAddress,
    compute_location,
    compute_operatingSystem,
    compute_type,

    -- ** DesiredPlayerSession
    desiredPlayerSession_playerData,
    desiredPlayerSession_playerId,

    -- ** EC2InstanceCounts
    eC2InstanceCounts_active,
    eC2InstanceCounts_desired,
    eC2InstanceCounts_idle,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_terminating,

    -- ** EC2InstanceLimit
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_instanceLimit,
    eC2InstanceLimit_location,

    -- ** Event
    event_eventCode,
    event_eventId,
    event_eventTime,
    event_message,
    event_preSignedLogUrl,
    event_resourceId,

    -- ** FilterConfiguration
    filterConfiguration_allowedLocations,

    -- ** FleetAttributes
    fleetAttributes_anywhereConfiguration,
    fleetAttributes_buildArn,
    fleetAttributes_buildId,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_computeType,
    fleetAttributes_creationTime,
    fleetAttributes_description,
    fleetAttributes_fleetArn,
    fleetAttributes_fleetId,
    fleetAttributes_fleetType,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_instanceType,
    fleetAttributes_logPaths,
    fleetAttributes_metricGroups,
    fleetAttributes_name,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_operatingSystem,
    fleetAttributes_resourceCreationLimitPolicy,
    fleetAttributes_scriptArn,
    fleetAttributes_scriptId,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_status,
    fleetAttributes_stoppedActions,
    fleetAttributes_terminationTime,

    -- ** FleetCapacity
    fleetCapacity_fleetArn,
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,
    fleetCapacity_instanceType,
    fleetCapacity_location,

    -- ** FleetUtilization
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_activeServerProcessCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_fleetArn,
    fleetUtilization_fleetId,
    fleetUtilization_location,
    fleetUtilization_maximumPlayerSessionCount,

    -- ** GameProperty
    gameProperty_key,
    gameProperty_value,

    -- ** GameServer
    gameServer_claimStatus,
    gameServer_connectionInfo,
    gameServer_gameServerData,
    gameServer_gameServerGroupArn,
    gameServer_gameServerGroupName,
    gameServer_gameServerId,
    gameServer_instanceId,
    gameServer_lastClaimTime,
    gameServer_lastHealthCheckTime,
    gameServer_registrationTime,
    gameServer_utilizationStatus,

    -- ** GameServerGroup
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_balancingStrategy,
    gameServerGroup_creationTime,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_lastUpdatedTime,
    gameServerGroup_roleArn,
    gameServerGroup_status,
    gameServerGroup_statusReason,
    gameServerGroup_suspendedActions,

    -- ** GameServerGroupAutoScalingPolicy
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- ** GameServerInstance
    gameServerInstance_gameServerGroupArn,
    gameServerInstance_gameServerGroupName,
    gameServerInstance_instanceId,
    gameServerInstance_instanceStatus,

    -- ** GameSession
    gameSession_creationTime,
    gameSession_creatorId,
    gameSession_currentPlayerSessionCount,
    gameSession_dnsName,
    gameSession_fleetArn,
    gameSession_fleetId,
    gameSession_gameProperties,
    gameSession_gameSessionData,
    gameSession_gameSessionId,
    gameSession_ipAddress,
    gameSession_location,
    gameSession_matchmakerData,
    gameSession_maximumPlayerSessionCount,
    gameSession_name,
    gameSession_playerSessionCreationPolicy,
    gameSession_port,
    gameSession_status,
    gameSession_statusReason,
    gameSession_terminationTime,

    -- ** GameSessionConnectionInfo
    gameSessionConnectionInfo_dnsName,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_port,

    -- ** GameSessionDetail
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- ** GameSessionPlacement
    gameSessionPlacement_dnsName,
    gameSessionPlacement_endTime,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_placementId,
    gameSessionPlacement_playerLatencies,
    gameSessionPlacement_port,
    gameSessionPlacement_startTime,
    gameSessionPlacement_status,

    -- ** GameSessionQueue
    gameSessionQueue_customEventData,
    gameSessionQueue_destinations,
    gameSessionQueue_filterConfiguration,
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_name,
    gameSessionQueue_notificationTarget,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_priorityConfiguration,
    gameSessionQueue_timeoutInSeconds,

    -- ** GameSessionQueueDestination
    gameSessionQueueDestination_destinationArn,

    -- ** Instance
    instance_creationTime,
    instance_dnsName,
    instance_fleetArn,
    instance_fleetId,
    instance_instanceId,
    instance_ipAddress,
    instance_location,
    instance_operatingSystem,
    instance_status,
    instance_type,

    -- ** InstanceAccess
    instanceAccess_credentials,
    instanceAccess_fleetId,
    instanceAccess_instanceId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,

    -- ** InstanceCredentials
    instanceCredentials_secret,
    instanceCredentials_userName,

    -- ** InstanceDefinition
    instanceDefinition_weightedCapacity,
    instanceDefinition_instanceType,

    -- ** IpPermission
    ipPermission_fromPort,
    ipPermission_toPort,
    ipPermission_ipRange,
    ipPermission_protocol,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LocationAttributes
    locationAttributes_locationState,
    locationAttributes_stoppedActions,
    locationAttributes_updateStatus,

    -- ** LocationConfiguration
    locationConfiguration_location,

    -- ** LocationModel
    locationModel_locationArn,
    locationModel_locationName,

    -- ** LocationState
    locationState_location,
    locationState_status,

    -- ** MatchedPlayerSession
    matchedPlayerSession_playerId,
    matchedPlayerSession_playerSessionId,

    -- ** MatchmakingConfiguration
    matchmakingConfiguration_acceptanceRequired,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_description,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_name,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_ruleSetArn,
    matchmakingConfiguration_ruleSetName,

    -- ** MatchmakingRuleSet
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetBody,

    -- ** MatchmakingTicket
    matchmakingTicket_configurationArn,
    matchmakingTicket_configurationName,
    matchmakingTicket_endTime,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_players,
    matchmakingTicket_startTime,
    matchmakingTicket_status,
    matchmakingTicket_statusMessage,
    matchmakingTicket_statusReason,
    matchmakingTicket_ticketId,

    -- ** PlacedPlayerSession
    placedPlayerSession_playerId,
    placedPlayerSession_playerSessionId,

    -- ** Player
    player_latencyInMs,
    player_playerAttributes,
    player_playerId,
    player_team,

    -- ** PlayerLatency
    playerLatency_latencyInMilliseconds,
    playerLatency_playerId,
    playerLatency_regionIdentifier,

    -- ** PlayerLatencyPolicy
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,
    playerLatencyPolicy_policyDurationSeconds,

    -- ** PlayerSession
    playerSession_creationTime,
    playerSession_dnsName,
    playerSession_fleetArn,
    playerSession_fleetId,
    playerSession_gameSessionId,
    playerSession_ipAddress,
    playerSession_playerData,
    playerSession_playerId,
    playerSession_playerSessionId,
    playerSession_port,
    playerSession_status,
    playerSession_terminationTime,

    -- ** PriorityConfiguration
    priorityConfiguration_locationOrder,
    priorityConfiguration_priorityOrder,

    -- ** ResourceCreationLimitPolicy
    resourceCreationLimitPolicy_newGameSessionsPerCreator,
    resourceCreationLimitPolicy_policyPeriodInMinutes,

    -- ** RoutingStrategy
    routingStrategy_fleetId,
    routingStrategy_message,
    routingStrategy_type,

    -- ** RuntimeConfiguration
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_maxConcurrentGameSessionActivations,
    runtimeConfiguration_serverProcesses,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,
    s3Location_objectVersion,
    s3Location_roleArn,

    -- ** ScalingPolicy
    scalingPolicy_comparisonOperator,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_fleetArn,
    scalingPolicy_fleetId,
    scalingPolicy_location,
    scalingPolicy_metricName,
    scalingPolicy_name,
    scalingPolicy_policyType,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_scalingAdjustmentType,
    scalingPolicy_status,
    scalingPolicy_targetConfiguration,
    scalingPolicy_threshold,
    scalingPolicy_updateStatus,

    -- ** Script
    script_creationTime,
    script_name,
    script_scriptArn,
    script_scriptId,
    script_sizeOnDisk,
    script_storageLocation,
    script_version,

    -- ** ServerProcess
    serverProcess_parameters,
    serverProcess_launchPath,
    serverProcess_concurrentExecutions,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetConfiguration
    targetConfiguration_targetValue,

    -- ** TargetTrackingConfiguration
    targetTrackingConfiguration_targetValue,

    -- ** VpcPeeringAuthorization
    vpcPeeringAuthorization_creationTime,
    vpcPeeringAuthorization_expirationTime,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_peerVpcId,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_fleetId,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_peerVpcId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_vpcPeeringConnectionId,

    -- ** VpcPeeringConnectionStatus
    vpcPeeringConnectionStatus_code,
    vpcPeeringConnectionStatus_message,
  )
where

import Amazonka.GameLift.AcceptMatch
import Amazonka.GameLift.ClaimGameServer
import Amazonka.GameLift.CreateAlias
import Amazonka.GameLift.CreateBuild
import Amazonka.GameLift.CreateFleet
import Amazonka.GameLift.CreateFleetLocations
import Amazonka.GameLift.CreateGameServerGroup
import Amazonka.GameLift.CreateGameSession
import Amazonka.GameLift.CreateGameSessionQueue
import Amazonka.GameLift.CreateLocation
import Amazonka.GameLift.CreateMatchmakingConfiguration
import Amazonka.GameLift.CreateMatchmakingRuleSet
import Amazonka.GameLift.CreatePlayerSession
import Amazonka.GameLift.CreatePlayerSessions
import Amazonka.GameLift.CreateScript
import Amazonka.GameLift.CreateVpcPeeringAuthorization
import Amazonka.GameLift.CreateVpcPeeringConnection
import Amazonka.GameLift.DeleteAlias
import Amazonka.GameLift.DeleteBuild
import Amazonka.GameLift.DeleteFleet
import Amazonka.GameLift.DeleteFleetLocations
import Amazonka.GameLift.DeleteGameServerGroup
import Amazonka.GameLift.DeleteGameSessionQueue
import Amazonka.GameLift.DeleteLocation
import Amazonka.GameLift.DeleteMatchmakingConfiguration
import Amazonka.GameLift.DeleteMatchmakingRuleSet
import Amazonka.GameLift.DeleteScalingPolicy
import Amazonka.GameLift.DeleteScript
import Amazonka.GameLift.DeleteVpcPeeringAuthorization
import Amazonka.GameLift.DeleteVpcPeeringConnection
import Amazonka.GameLift.DeregisterCompute
import Amazonka.GameLift.DeregisterGameServer
import Amazonka.GameLift.DescribeAlias
import Amazonka.GameLift.DescribeBuild
import Amazonka.GameLift.DescribeCompute
import Amazonka.GameLift.DescribeEC2InstanceLimits
import Amazonka.GameLift.DescribeFleetAttributes
import Amazonka.GameLift.DescribeFleetCapacity
import Amazonka.GameLift.DescribeFleetEvents
import Amazonka.GameLift.DescribeFleetLocationAttributes
import Amazonka.GameLift.DescribeFleetLocationCapacity
import Amazonka.GameLift.DescribeFleetLocationUtilization
import Amazonka.GameLift.DescribeFleetPortSettings
import Amazonka.GameLift.DescribeFleetUtilization
import Amazonka.GameLift.DescribeGameServer
import Amazonka.GameLift.DescribeGameServerGroup
import Amazonka.GameLift.DescribeGameServerInstances
import Amazonka.GameLift.DescribeGameSessionDetails
import Amazonka.GameLift.DescribeGameSessionPlacement
import Amazonka.GameLift.DescribeGameSessionQueues
import Amazonka.GameLift.DescribeGameSessions
import Amazonka.GameLift.DescribeInstances
import Amazonka.GameLift.DescribeMatchmaking
import Amazonka.GameLift.DescribeMatchmakingConfigurations
import Amazonka.GameLift.DescribeMatchmakingRuleSets
import Amazonka.GameLift.DescribePlayerSessions
import Amazonka.GameLift.DescribeRuntimeConfiguration
import Amazonka.GameLift.DescribeScalingPolicies
import Amazonka.GameLift.DescribeScript
import Amazonka.GameLift.DescribeVpcPeeringAuthorizations
import Amazonka.GameLift.DescribeVpcPeeringConnections
import Amazonka.GameLift.GetComputeAccess
import Amazonka.GameLift.GetComputeAuthToken
import Amazonka.GameLift.GetGameSessionLogUrl
import Amazonka.GameLift.GetInstanceAccess
import Amazonka.GameLift.ListAliases
import Amazonka.GameLift.ListBuilds
import Amazonka.GameLift.ListCompute
import Amazonka.GameLift.ListFleets
import Amazonka.GameLift.ListGameServerGroups
import Amazonka.GameLift.ListGameServers
import Amazonka.GameLift.ListLocations
import Amazonka.GameLift.ListScripts
import Amazonka.GameLift.ListTagsForResource
import Amazonka.GameLift.PutScalingPolicy
import Amazonka.GameLift.RegisterCompute
import Amazonka.GameLift.RegisterGameServer
import Amazonka.GameLift.RequestUploadCredentials
import Amazonka.GameLift.ResolveAlias
import Amazonka.GameLift.ResumeGameServerGroup
import Amazonka.GameLift.SearchGameSessions
import Amazonka.GameLift.StartFleetActions
import Amazonka.GameLift.StartGameSessionPlacement
import Amazonka.GameLift.StartMatchBackfill
import Amazonka.GameLift.StartMatchmaking
import Amazonka.GameLift.StopFleetActions
import Amazonka.GameLift.StopGameSessionPlacement
import Amazonka.GameLift.StopMatchmaking
import Amazonka.GameLift.SuspendGameServerGroup
import Amazonka.GameLift.TagResource
import Amazonka.GameLift.Types.Alias
import Amazonka.GameLift.Types.AnywhereConfiguration
import Amazonka.GameLift.Types.AttributeValue
import Amazonka.GameLift.Types.AwsCredentials
import Amazonka.GameLift.Types.Build
import Amazonka.GameLift.Types.CertificateConfiguration
import Amazonka.GameLift.Types.Compute
import Amazonka.GameLift.Types.DesiredPlayerSession
import Amazonka.GameLift.Types.EC2InstanceCounts
import Amazonka.GameLift.Types.EC2InstanceLimit
import Amazonka.GameLift.Types.Event
import Amazonka.GameLift.Types.FilterConfiguration
import Amazonka.GameLift.Types.FleetAttributes
import Amazonka.GameLift.Types.FleetCapacity
import Amazonka.GameLift.Types.FleetUtilization
import Amazonka.GameLift.Types.GameProperty
import Amazonka.GameLift.Types.GameServer
import Amazonka.GameLift.Types.GameServerGroup
import Amazonka.GameLift.Types.GameServerGroupAutoScalingPolicy
import Amazonka.GameLift.Types.GameServerInstance
import Amazonka.GameLift.Types.GameSession
import Amazonka.GameLift.Types.GameSessionConnectionInfo
import Amazonka.GameLift.Types.GameSessionDetail
import Amazonka.GameLift.Types.GameSessionPlacement
import Amazonka.GameLift.Types.GameSessionQueue
import Amazonka.GameLift.Types.GameSessionQueueDestination
import Amazonka.GameLift.Types.Instance
import Amazonka.GameLift.Types.InstanceAccess
import Amazonka.GameLift.Types.InstanceCredentials
import Amazonka.GameLift.Types.InstanceDefinition
import Amazonka.GameLift.Types.IpPermission
import Amazonka.GameLift.Types.LaunchTemplateSpecification
import Amazonka.GameLift.Types.LocationAttributes
import Amazonka.GameLift.Types.LocationConfiguration
import Amazonka.GameLift.Types.LocationModel
import Amazonka.GameLift.Types.LocationState
import Amazonka.GameLift.Types.MatchedPlayerSession
import Amazonka.GameLift.Types.MatchmakingConfiguration
import Amazonka.GameLift.Types.MatchmakingRuleSet
import Amazonka.GameLift.Types.MatchmakingTicket
import Amazonka.GameLift.Types.PlacedPlayerSession
import Amazonka.GameLift.Types.Player
import Amazonka.GameLift.Types.PlayerLatency
import Amazonka.GameLift.Types.PlayerLatencyPolicy
import Amazonka.GameLift.Types.PlayerSession
import Amazonka.GameLift.Types.PriorityConfiguration
import Amazonka.GameLift.Types.ResourceCreationLimitPolicy
import Amazonka.GameLift.Types.RoutingStrategy
import Amazonka.GameLift.Types.RuntimeConfiguration
import Amazonka.GameLift.Types.S3Location
import Amazonka.GameLift.Types.ScalingPolicy
import Amazonka.GameLift.Types.Script
import Amazonka.GameLift.Types.ServerProcess
import Amazonka.GameLift.Types.Tag
import Amazonka.GameLift.Types.TargetConfiguration
import Amazonka.GameLift.Types.TargetTrackingConfiguration
import Amazonka.GameLift.Types.VpcPeeringAuthorization
import Amazonka.GameLift.Types.VpcPeeringConnection
import Amazonka.GameLift.Types.VpcPeeringConnectionStatus
import Amazonka.GameLift.UntagResource
import Amazonka.GameLift.UpdateAlias
import Amazonka.GameLift.UpdateBuild
import Amazonka.GameLift.UpdateFleetAttributes
import Amazonka.GameLift.UpdateFleetCapacity
import Amazonka.GameLift.UpdateFleetPortSettings
import Amazonka.GameLift.UpdateGameServer
import Amazonka.GameLift.UpdateGameServerGroup
import Amazonka.GameLift.UpdateGameSession
import Amazonka.GameLift.UpdateGameSessionQueue
import Amazonka.GameLift.UpdateMatchmakingConfiguration
import Amazonka.GameLift.UpdateRuntimeConfiguration
import Amazonka.GameLift.UpdateScript
import Amazonka.GameLift.ValidateMatchmakingRuleSet
