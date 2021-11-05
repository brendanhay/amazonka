{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Lens
  ( -- * Operations

    -- ** StopMatchmaking
    stopMatchmaking_ticketId,
    stopMatchmakingResponse_httpStatus,

    -- ** DescribeGameServerInstances
    describeGameServerInstances_nextToken,
    describeGameServerInstances_instanceIds,
    describeGameServerInstances_limit,
    describeGameServerInstances_gameServerGroupName,
    describeGameServerInstancesResponse_gameServerInstances,
    describeGameServerInstancesResponse_nextToken,
    describeGameServerInstancesResponse_httpStatus,

    -- ** CreateGameSession
    createGameSession_idempotencyToken,
    createGameSession_gameProperties,
    createGameSession_location,
    createGameSession_gameSessionId,
    createGameSession_aliasId,
    createGameSession_name,
    createGameSession_gameSessionData,
    createGameSession_fleetId,
    createGameSession_creatorId,
    createGameSession_maximumPlayerSessionCount,
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,

    -- ** DeleteScalingPolicy
    deleteScalingPolicy_name,
    deleteScalingPolicy_fleetId,

    -- ** PutScalingPolicy
    putScalingPolicy_scalingAdjustmentType,
    putScalingPolicy_evaluationPeriods,
    putScalingPolicy_policyType,
    putScalingPolicy_comparisonOperator,
    putScalingPolicy_threshold,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_targetConfiguration,
    putScalingPolicy_name,
    putScalingPolicy_fleetId,
    putScalingPolicy_metricName,
    putScalingPolicyResponse_name,
    putScalingPolicyResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_status,
    listBuilds_nextToken,
    listBuilds_limit,
    listBuildsResponse_builds,
    listBuildsResponse_nextToken,
    listBuildsResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_fleetId,

    -- ** CreateBuild
    createBuild_storageLocation,
    createBuild_operatingSystem,
    createBuild_name,
    createBuild_version,
    createBuild_tags,
    createBuildResponse_storageLocation,
    createBuildResponse_uploadCredentials,
    createBuildResponse_build,
    createBuildResponse_httpStatus,

    -- ** RequestUploadCredentials
    requestUploadCredentials_buildId,
    requestUploadCredentialsResponse_storageLocation,
    requestUploadCredentialsResponse_uploadCredentials,
    requestUploadCredentialsResponse_httpStatus,

    -- ** CreateAlias
    createAlias_description,
    createAlias_tags,
    createAlias_name,
    createAlias_routingStrategy,
    createAliasResponse_alias,
    createAliasResponse_httpStatus,

    -- ** ListGameServers
    listGameServers_nextToken,
    listGameServers_sortOrder,
    listGameServers_limit,
    listGameServers_gameServerGroupName,
    listGameServersResponse_gameServers,
    listGameServersResponse_nextToken,
    listGameServersResponse_httpStatus,

    -- ** ResolveAlias
    resolveAlias_aliasId,
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_fleetId,
    resolveAliasResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterGameServer
    registerGameServer_gameServerData,
    registerGameServer_connectionInfo,
    registerGameServer_gameServerGroupName,
    registerGameServer_gameServerId,
    registerGameServer_instanceId,
    registerGameServerResponse_gameServer,
    registerGameServerResponse_httpStatus,

    -- ** ListAliases
    listAliases_routingStrategyType,
    listAliases_nextToken,
    listAliases_name,
    listAliases_limit,
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,

    -- ** UpdateRuntimeConfiguration
    updateRuntimeConfiguration_fleetId,
    updateRuntimeConfiguration_runtimeConfiguration,
    updateRuntimeConfigurationResponse_runtimeConfiguration,
    updateRuntimeConfigurationResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_fleetId,
    createVpcPeeringConnection_peerVpcAwsAccountId,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** ListGameServerGroups
    listGameServerGroups_nextToken,
    listGameServerGroups_limit,
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_httpStatus,

    -- ** CreateGameSessionQueue
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_filterConfiguration,
    createGameSessionQueue_notificationTarget,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_destinations,
    createGameSessionQueue_customEventData,
    createGameSessionQueue_priorityConfiguration,
    createGameSessionQueue_tags,
    createGameSessionQueue_name,
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,

    -- ** SearchGameSessions
    searchGameSessions_filterExpression,
    searchGameSessions_location,
    searchGameSessions_sortExpression,
    searchGameSessions_aliasId,
    searchGameSessions_nextToken,
    searchGameSessions_limit,
    searchGameSessions_fleetId,
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_httpStatus,

    -- ** CreateVpcPeeringAuthorization
    createVpcPeeringAuthorization_gameLiftAwsAccountId,
    createVpcPeeringAuthorization_peerVpcId,
    createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization,
    createVpcPeeringAuthorizationResponse_httpStatus,

    -- ** UpdateGameSessionQueue
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_filterConfiguration,
    updateGameSessionQueue_notificationTarget,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_customEventData,
    updateGameSessionQueue_priorityConfiguration,
    updateGameSessionQueue_name,
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,

    -- ** DeleteGameSessionQueue
    deleteGameSessionQueue_name,
    deleteGameSessionQueueResponse_httpStatus,

    -- ** CreateGameServerGroup
    createGameServerGroup_vpcSubnets,
    createGameServerGroup_balancingStrategy,
    createGameServerGroup_autoScalingPolicy,
    createGameServerGroup_gameServerProtectionPolicy,
    createGameServerGroup_tags,
    createGameServerGroup_gameServerGroupName,
    createGameServerGroup_roleArn,
    createGameServerGroup_minSize,
    createGameServerGroup_maxSize,
    createGameServerGroup_launchTemplate,
    createGameServerGroup_instanceDefinitions,
    createGameServerGroupResponse_gameServerGroup,
    createGameServerGroupResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_fleetId,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** StartFleetActions
    startFleetActions_location,
    startFleetActions_fleetId,
    startFleetActions_actions,
    startFleetActionsResponse_fleetArn,
    startFleetActionsResponse_fleetId,
    startFleetActionsResponse_httpStatus,

    -- ** DeregisterGameServer
    deregisterGameServer_gameServerGroupName,
    deregisterGameServer_gameServerId,

    -- ** GetInstanceAccess
    getInstanceAccess_fleetId,
    getInstanceAccess_instanceId,
    getInstanceAccessResponse_instanceAccess,
    getInstanceAccessResponse_httpStatus,

    -- ** DescribeScalingPolicies
    describeScalingPolicies_location,
    describeScalingPolicies_nextToken,
    describeScalingPolicies_statusFilter,
    describeScalingPolicies_limit,
    describeScalingPolicies_fleetId,
    describeScalingPoliciesResponse_nextToken,
    describeScalingPoliciesResponse_scalingPolicies,
    describeScalingPoliciesResponse_httpStatus,

    -- ** DescribeMatchmakingRuleSets
    describeMatchmakingRuleSets_nextToken,
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_limit,
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,

    -- ** DescribeGameSessions
    describeGameSessions_location,
    describeGameSessions_gameSessionId,
    describeGameSessions_aliasId,
    describeGameSessions_nextToken,
    describeGameSessions_statusFilter,
    describeGameSessions_limit,
    describeGameSessions_fleetId,
    describeGameSessionsResponse_gameSessions,
    describeGameSessionsResponse_nextToken,
    describeGameSessionsResponse_httpStatus,

    -- ** DescribeGameServer
    describeGameServer_gameServerGroupName,
    describeGameServer_gameServerId,
    describeGameServerResponse_gameServer,
    describeGameServerResponse_httpStatus,

    -- ** UpdateScript
    updateScript_storageLocation,
    updateScript_zipFile,
    updateScript_name,
    updateScript_version,
    updateScript_scriptId,
    updateScriptResponse_script,
    updateScriptResponse_httpStatus,

    -- ** DeleteScript
    deleteScript_scriptId,

    -- ** StartGameSessionPlacement
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_playerLatencies,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_desiredPlayerSessions,
    startGameSessionPlacement_placementId,
    startGameSessionPlacement_gameSessionQueueName,
    startGameSessionPlacement_maximumPlayerSessionCount,
    startGameSessionPlacementResponse_gameSessionPlacement,
    startGameSessionPlacementResponse_httpStatus,

    -- ** DescribeFleetUtilization
    describeFleetUtilization_nextToken,
    describeFleetUtilization_limit,
    describeFleetUtilization_fleetIds,
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_fleetUtilization,
    describeFleetUtilizationResponse_httpStatus,

    -- ** DescribeRuntimeConfiguration
    describeRuntimeConfiguration_fleetId,
    describeRuntimeConfigurationResponse_runtimeConfiguration,
    describeRuntimeConfigurationResponse_httpStatus,

    -- ** GetGameSessionLogUrl
    getGameSessionLogUrl_gameSessionId,
    getGameSessionLogUrlResponse_preSignedUrl,
    getGameSessionLogUrlResponse_httpStatus,

    -- ** DescribeFleetAttributes
    describeFleetAttributes_nextToken,
    describeFleetAttributes_limit,
    describeFleetAttributes_fleetIds,
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_httpStatus,

    -- ** DescribeGameSessionPlacement
    describeGameSessionPlacement_placementId,
    describeGameSessionPlacementResponse_gameSessionPlacement,
    describeGameSessionPlacementResponse_httpStatus,

    -- ** DescribeFleetEvents
    describeFleetEvents_startTime,
    describeFleetEvents_nextToken,
    describeFleetEvents_endTime,
    describeFleetEvents_limit,
    describeFleetEvents_fleetId,
    describeFleetEventsResponse_nextToken,
    describeFleetEventsResponse_events,
    describeFleetEventsResponse_httpStatus,

    -- ** CreateFleetLocations
    createFleetLocations_fleetId,
    createFleetLocations_locations,
    createFleetLocationsResponse_fleetArn,
    createFleetLocationsResponse_locationStates,
    createFleetLocationsResponse_fleetId,
    createFleetLocationsResponse_httpStatus,

    -- ** StartMatchmaking
    startMatchmaking_ticketId,
    startMatchmaking_configurationName,
    startMatchmaking_players,
    startMatchmakingResponse_matchmakingTicket,
    startMatchmakingResponse_httpStatus,

    -- ** CreateMatchmakingRuleSet
    createMatchmakingRuleSet_tags,
    createMatchmakingRuleSet_name,
    createMatchmakingRuleSet_ruleSetBody,
    createMatchmakingRuleSetResponse_httpStatus,
    createMatchmakingRuleSetResponse_ruleSet,

    -- ** DescribeFleetLocationUtilization
    describeFleetLocationUtilization_fleetId,
    describeFleetLocationUtilization_location,
    describeFleetLocationUtilizationResponse_fleetUtilization,
    describeFleetLocationUtilizationResponse_httpStatus,

    -- ** DescribeFleetCapacity
    describeFleetCapacity_nextToken,
    describeFleetCapacity_limit,
    describeFleetCapacity_fleetIds,
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_httpStatus,

    -- ** DeleteBuild
    deleteBuild_buildId,

    -- ** UpdateBuild
    updateBuild_name,
    updateBuild_version,
    updateBuild_buildId,
    updateBuildResponse_build,
    updateBuildResponse_httpStatus,

    -- ** ListFleets
    listFleets_buildId,
    listFleets_nextToken,
    listFleets_scriptId,
    listFleets_limit,
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetIds,
    listFleetsResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_aliasId,

    -- ** UpdateAlias
    updateAlias_routingStrategy,
    updateAlias_name,
    updateAlias_description,
    updateAlias_aliasId,
    updateAliasResponse_alias,
    updateAliasResponse_httpStatus,

    -- ** StartMatchBackfill
    startMatchBackfill_ticketId,
    startMatchBackfill_gameSessionArn,
    startMatchBackfill_configurationName,
    startMatchBackfill_players,
    startMatchBackfillResponse_matchmakingTicket,
    startMatchBackfillResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_instanceId,
    describeInstances_location,
    describeInstances_nextToken,
    describeInstances_limit,
    describeInstances_fleetId,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DescribeGameSessionDetails
    describeGameSessionDetails_location,
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_statusFilter,
    describeGameSessionDetails_limit,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_httpStatus,

    -- ** DescribeFleetPortSettings
    describeFleetPortSettings_location,
    describeFleetPortSettings_fleetId,
    describeFleetPortSettingsResponse_location,
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_fleetArn,
    describeFleetPortSettingsResponse_updateStatus,
    describeFleetPortSettingsResponse_fleetId,
    describeFleetPortSettingsResponse_httpStatus,

    -- ** DescribeGameSessionQueues
    describeGameSessionQueues_nextToken,
    describeGameSessionQueues_names,
    describeGameSessionQueues_limit,
    describeGameSessionQueuesResponse_nextToken,
    describeGameSessionQueuesResponse_gameSessionQueues,
    describeGameSessionQueuesResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_fleetId,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** DescribeScript
    describeScript_scriptId,
    describeScriptResponse_script,
    describeScriptResponse_httpStatus,

    -- ** CreatePlayerSessions
    createPlayerSessions_playerDataMap,
    createPlayerSessions_gameSessionId,
    createPlayerSessions_playerIds,
    createPlayerSessionsResponse_playerSessions,
    createPlayerSessionsResponse_httpStatus,

    -- ** DescribeMatchmakingConfigurations
    describeMatchmakingConfigurations_ruleSetName,
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_limit,
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_httpStatus,

    -- ** DescribeVpcPeeringAuthorizations
    describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations,
    describeVpcPeeringAuthorizationsResponse_httpStatus,

    -- ** UpdateGameServer
    updateGameServer_healthCheck,
    updateGameServer_gameServerData,
    updateGameServer_utilizationStatus,
    updateGameServer_gameServerGroupName,
    updateGameServer_gameServerId,
    updateGameServerResponse_gameServer,
    updateGameServerResponse_httpStatus,

    -- ** CreateFleet
    createFleet_serverLaunchParameters,
    createFleet_logPaths,
    createFleet_peerVpcId,
    createFleet_buildId,
    createFleet_fleetType,
    createFleet_peerVpcAwsAccountId,
    createFleet_eC2InboundPermissions,
    createFleet_runtimeConfiguration,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_scriptId,
    createFleet_certificateConfiguration,
    createFleet_serverLaunchPath,
    createFleet_instanceRoleArn,
    createFleet_metricGroups,
    createFleet_description,
    createFleet_resourceCreationLimitPolicy,
    createFleet_locations,
    createFleet_tags,
    createFleet_name,
    createFleet_eC2InstanceType,
    createFleetResponse_locationStates,
    createFleetResponse_fleetAttributes,
    createFleetResponse_httpStatus,

    -- ** DescribeFleetLocationCapacity
    describeFleetLocationCapacity_fleetId,
    describeFleetLocationCapacity_location,
    describeFleetLocationCapacityResponse_fleetCapacity,
    describeFleetLocationCapacityResponse_httpStatus,

    -- ** DeleteMatchmakingConfiguration
    deleteMatchmakingConfiguration_name,
    deleteMatchmakingConfigurationResponse_httpStatus,

    -- ** UpdateMatchmakingConfiguration
    updateMatchmakingConfiguration_backfillMode,
    updateMatchmakingConfiguration_gameProperties,
    updateMatchmakingConfiguration_ruleSetName,
    updateMatchmakingConfiguration_acceptanceTimeoutSeconds,
    updateMatchmakingConfiguration_requestTimeoutSeconds,
    updateMatchmakingConfiguration_notificationTarget,
    updateMatchmakingConfiguration_flexMatchMode,
    updateMatchmakingConfiguration_gameSessionQueueArns,
    updateMatchmakingConfiguration_customEventData,
    updateMatchmakingConfiguration_acceptanceRequired,
    updateMatchmakingConfiguration_gameSessionData,
    updateMatchmakingConfiguration_description,
    updateMatchmakingConfiguration_additionalPlayerCount,
    updateMatchmakingConfiguration_name,
    updateMatchmakingConfigurationResponse_configuration,
    updateMatchmakingConfigurationResponse_httpStatus,

    -- ** DeleteGameServerGroup
    deleteGameServerGroup_deleteOption,
    deleteGameServerGroup_gameServerGroupName,
    deleteGameServerGroupResponse_gameServerGroup,
    deleteGameServerGroupResponse_httpStatus,

    -- ** UpdateGameServerGroup
    updateGameServerGroup_instanceDefinitions,
    updateGameServerGroup_balancingStrategy,
    updateGameServerGroup_gameServerProtectionPolicy,
    updateGameServerGroup_roleArn,
    updateGameServerGroup_gameServerGroupName,
    updateGameServerGroupResponse_gameServerGroup,
    updateGameServerGroupResponse_httpStatus,

    -- ** ResumeGameServerGroup
    resumeGameServerGroup_gameServerGroupName,
    resumeGameServerGroup_resumeActions,
    resumeGameServerGroupResponse_gameServerGroup,
    resumeGameServerGroupResponse_httpStatus,

    -- ** DeleteVpcPeeringAuthorization
    deleteVpcPeeringAuthorization_gameLiftAwsAccountId,
    deleteVpcPeeringAuthorization_peerVpcId,
    deleteVpcPeeringAuthorizationResponse_httpStatus,

    -- ** UpdateFleetAttributes
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_name,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_description,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_fleetId,
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateMatchmakingConfiguration
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_name,
    createMatchmakingConfiguration_requestTimeoutSeconds,
    createMatchmakingConfiguration_acceptanceRequired,
    createMatchmakingConfiguration_ruleSetName,
    createMatchmakingConfigurationResponse_configuration,
    createMatchmakingConfigurationResponse_httpStatus,

    -- ** DescribePlayerSessions
    describePlayerSessions_gameSessionId,
    describePlayerSessions_nextToken,
    describePlayerSessions_limit,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_playerId,
    describePlayerSessions_playerSessionStatusFilter,
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,

    -- ** StopFleetActions
    stopFleetActions_location,
    stopFleetActions_fleetId,
    stopFleetActions_actions,
    stopFleetActionsResponse_fleetArn,
    stopFleetActionsResponse_fleetId,
    stopFleetActionsResponse_httpStatus,

    -- ** DescribeBuild
    describeBuild_buildId,
    describeBuildResponse_build,
    describeBuildResponse_httpStatus,

    -- ** UpdateFleetPortSettings
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_fleetId,
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,

    -- ** UpdateFleetCapacity
    updateFleetCapacity_location,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_minSize,
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_fleetId,
    updateFleetCapacityResponse_location,
    updateFleetCapacityResponse_fleetArn,
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_httpStatus,

    -- ** CreateScript
    createScript_storageLocation,
    createScript_zipFile,
    createScript_name,
    createScript_version,
    createScript_tags,
    createScriptResponse_script,
    createScriptResponse_httpStatus,

    -- ** AcceptMatch
    acceptMatch_ticketId,
    acceptMatch_playerIds,
    acceptMatch_acceptanceType,
    acceptMatchResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeAlias
    describeAlias_aliasId,
    describeAliasResponse_alias,
    describeAliasResponse_httpStatus,

    -- ** ValidateMatchmakingRuleSet
    validateMatchmakingRuleSet_ruleSetBody,
    validateMatchmakingRuleSetResponse_valid,
    validateMatchmakingRuleSetResponse_httpStatus,

    -- ** ListScripts
    listScripts_nextToken,
    listScripts_limit,
    listScriptsResponse_scripts,
    listScriptsResponse_nextToken,
    listScriptsResponse_httpStatus,

    -- ** DescribeEC2InstanceLimits
    describeEC2InstanceLimits_location,
    describeEC2InstanceLimits_eC2InstanceType,
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,

    -- ** SuspendGameServerGroup
    suspendGameServerGroup_gameServerGroupName,
    suspendGameServerGroup_suspendActions,
    suspendGameServerGroupResponse_gameServerGroup,
    suspendGameServerGroupResponse_httpStatus,

    -- ** DeleteFleetLocations
    deleteFleetLocations_fleetId,
    deleteFleetLocations_locations,
    deleteFleetLocationsResponse_fleetArn,
    deleteFleetLocationsResponse_locationStates,
    deleteFleetLocationsResponse_fleetId,
    deleteFleetLocationsResponse_httpStatus,

    -- ** DeleteMatchmakingRuleSet
    deleteMatchmakingRuleSet_name,
    deleteMatchmakingRuleSetResponse_httpStatus,

    -- ** StopGameSessionPlacement
    stopGameSessionPlacement_placementId,
    stopGameSessionPlacementResponse_gameSessionPlacement,
    stopGameSessionPlacementResponse_httpStatus,

    -- ** ClaimGameServer
    claimGameServer_gameServerData,
    claimGameServer_gameServerId,
    claimGameServer_gameServerGroupName,
    claimGameServerResponse_gameServer,
    claimGameServerResponse_httpStatus,

    -- ** UpdateGameSession
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_playerSessionCreationPolicy,
    updateGameSession_name,
    updateGameSession_protectionPolicy,
    updateGameSession_gameSessionId,
    updateGameSessionResponse_gameSession,
    updateGameSessionResponse_httpStatus,

    -- ** DescribeFleetLocationAttributes
    describeFleetLocationAttributes_nextToken,
    describeFleetLocationAttributes_limit,
    describeFleetLocationAttributes_locations,
    describeFleetLocationAttributes_fleetId,
    describeFleetLocationAttributesResponse_fleetArn,
    describeFleetLocationAttributesResponse_nextToken,
    describeFleetLocationAttributesResponse_locationAttributes,
    describeFleetLocationAttributesResponse_fleetId,
    describeFleetLocationAttributesResponse_httpStatus,

    -- ** DescribeMatchmaking
    describeMatchmaking_ticketIds,
    describeMatchmakingResponse_ticketList,
    describeMatchmakingResponse_httpStatus,

    -- ** CreatePlayerSession
    createPlayerSession_playerData,
    createPlayerSession_gameSessionId,
    createPlayerSession_playerId,
    createPlayerSessionResponse_playerSession,
    createPlayerSessionResponse_httpStatus,

    -- ** DescribeGameServerGroup
    describeGameServerGroup_gameServerGroupName,
    describeGameServerGroupResponse_gameServerGroup,
    describeGameServerGroupResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_creationTime,
    alias_lastUpdatedTime,
    alias_aliasId,
    alias_routingStrategy,
    alias_name,
    alias_aliasArn,
    alias_description,

    -- ** AttributeValue
    attributeValue_sl,
    attributeValue_sdm,
    attributeValue_n,
    attributeValue_s,

    -- ** AwsCredentials
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,
    awsCredentials_accessKeyId,

    -- ** Build
    build_creationTime,
    build_status,
    build_operatingSystem,
    build_buildId,
    build_name,
    build_version,
    build_buildArn,
    build_sizeOnDisk,

    -- ** CertificateConfiguration
    certificateConfiguration_certificateType,

    -- ** DesiredPlayerSession
    desiredPlayerSession_playerData,
    desiredPlayerSession_playerId,

    -- ** EC2InstanceCounts
    eC2InstanceCounts_idle,
    eC2InstanceCounts_terminating,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_desired,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_active,

    -- ** EC2InstanceLimit
    eC2InstanceLimit_location,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_instanceLimit,

    -- ** Event
    event_resourceId,
    event_preSignedLogUrl,
    event_eventTime,
    event_message,
    event_eventCode,
    event_eventId,

    -- ** FilterConfiguration
    filterConfiguration_allowedLocations,

    -- ** FleetAttributes
    fleetAttributes_creationTime,
    fleetAttributes_status,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_logPaths,
    fleetAttributes_operatingSystem,
    fleetAttributes_buildId,
    fleetAttributes_fleetArn,
    fleetAttributes_fleetType,
    fleetAttributes_terminationTime,
    fleetAttributes_instanceType,
    fleetAttributes_stoppedActions,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_name,
    fleetAttributes_scriptId,
    fleetAttributes_scriptArn,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_metricGroups,
    fleetAttributes_buildArn,
    fleetAttributes_fleetId,
    fleetAttributes_description,
    fleetAttributes_resourceCreationLimitPolicy,

    -- ** FleetCapacity
    fleetCapacity_location,
    fleetCapacity_fleetArn,
    fleetCapacity_instanceType,
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,

    -- ** FleetUtilization
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_location,
    fleetUtilization_fleetArn,
    fleetUtilization_maximumPlayerSessionCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_fleetId,
    fleetUtilization_activeServerProcessCount,

    -- ** GameProperty
    gameProperty_key,
    gameProperty_value,

    -- ** GameServer
    gameServer_instanceId,
    gameServer_lastClaimTime,
    gameServer_gameServerGroupName,
    gameServer_gameServerData,
    gameServer_claimStatus,
    gameServer_gameServerId,
    gameServer_utilizationStatus,
    gameServer_registrationTime,
    gameServer_lastHealthCheckTime,
    gameServer_connectionInfo,
    gameServer_gameServerGroupArn,

    -- ** GameServerGroup
    gameServerGroup_creationTime,
    gameServerGroup_status,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_lastUpdatedTime,
    gameServerGroup_balancingStrategy,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_suspendedActions,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_statusReason,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_roleArn,

    -- ** GameServerGroupAutoScalingPolicy
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- ** GameServerInstance
    gameServerInstance_instanceId,
    gameServerInstance_gameServerGroupName,
    gameServerInstance_instanceStatus,
    gameServerInstance_gameServerGroupArn,

    -- ** GameSession
    gameSession_creationTime,
    gameSession_status,
    gameSession_gameProperties,
    gameSession_ipAddress,
    gameSession_location,
    gameSession_gameSessionId,
    gameSession_matchmakerData,
    gameSession_fleetArn,
    gameSession_maximumPlayerSessionCount,
    gameSession_terminationTime,
    gameSession_playerSessionCreationPolicy,
    gameSession_name,
    gameSession_currentPlayerSessionCount,
    gameSession_statusReason,
    gameSession_gameSessionData,
    gameSession_fleetId,
    gameSession_dnsName,
    gameSession_creatorId,
    gameSession_port,

    -- ** GameSessionConnectionInfo
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_dnsName,
    gameSessionConnectionInfo_port,

    -- ** GameSessionDetail
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- ** GameSessionPlacement
    gameSessionPlacement_status,
    gameSessionPlacement_placementId,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_startTime,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_endTime,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_playerLatencies,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_port,

    -- ** GameSessionQueue
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_filterConfiguration,
    gameSessionQueue_notificationTarget,
    gameSessionQueue_timeoutInSeconds,
    gameSessionQueue_destinations,
    gameSessionQueue_name,
    gameSessionQueue_customEventData,
    gameSessionQueue_priorityConfiguration,

    -- ** GameSessionQueueDestination
    gameSessionQueueDestination_destinationArn,

    -- ** Instance
    instance_creationTime,
    instance_instanceId,
    instance_status,
    instance_ipAddress,
    instance_location,
    instance_operatingSystem,
    instance_fleetArn,
    instance_type,
    instance_fleetId,
    instance_dnsName,

    -- ** InstanceAccess
    instanceAccess_instanceId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,
    instanceAccess_credentials,
    instanceAccess_fleetId,

    -- ** InstanceCredentials
    instanceCredentials_userName,
    instanceCredentials_secret,

    -- ** InstanceDefinition
    instanceDefinition_weightedCapacity,
    instanceDefinition_instanceType,

    -- ** IpPermission
    ipPermission_fromPort,
    ipPermission_toPort,
    ipPermission_ipRange,
    ipPermission_protocol,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- ** LocationAttributes
    locationAttributes_stoppedActions,
    locationAttributes_locationState,
    locationAttributes_updateStatus,

    -- ** LocationConfiguration
    locationConfiguration_location,

    -- ** LocationState
    locationState_status,
    locationState_location,

    -- ** MatchedPlayerSession
    matchedPlayerSession_playerSessionId,
    matchedPlayerSession_playerId,

    -- ** MatchmakingConfiguration
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_ruleSetName,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_name,
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_acceptanceRequired,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_description,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_ruleSetArn,

    -- ** MatchmakingRuleSet
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_ruleSetBody,

    -- ** MatchmakingTicket
    matchmakingTicket_status,
    matchmakingTicket_configurationName,
    matchmakingTicket_startTime,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_ticketId,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_statusMessage,
    matchmakingTicket_endTime,
    matchmakingTicket_configurationArn,
    matchmakingTicket_statusReason,
    matchmakingTicket_players,

    -- ** PlacedPlayerSession
    placedPlayerSession_playerSessionId,
    placedPlayerSession_playerId,

    -- ** Player
    player_playerAttributes,
    player_team,
    player_playerId,
    player_latencyInMs,

    -- ** PlayerLatency
    playerLatency_latencyInMilliseconds,
    playerLatency_regionIdentifier,
    playerLatency_playerId,

    -- ** PlayerLatencyPolicy
    playerLatencyPolicy_policyDurationSeconds,
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,

    -- ** PlayerSession
    playerSession_creationTime,
    playerSession_status,
    playerSession_ipAddress,
    playerSession_gameSessionId,
    playerSession_fleetArn,
    playerSession_terminationTime,
    playerSession_playerSessionId,
    playerSession_fleetId,
    playerSession_playerData,
    playerSession_playerId,
    playerSession_dnsName,
    playerSession_port,

    -- ** PriorityConfiguration
    priorityConfiguration_priorityOrder,
    priorityConfiguration_locationOrder,

    -- ** ResourceCreationLimitPolicy
    resourceCreationLimitPolicy_newGameSessionsPerCreator,
    resourceCreationLimitPolicy_policyPeriodInMinutes,

    -- ** RoutingStrategy
    routingStrategy_type,
    routingStrategy_message,
    routingStrategy_fleetId,

    -- ** RuntimeConfiguration
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_serverProcesses,
    runtimeConfiguration_maxConcurrentGameSessionActivations,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,
    s3Location_objectVersion,
    s3Location_roleArn,

    -- ** ScalingPolicy
    scalingPolicy_status,
    scalingPolicy_scalingAdjustmentType,
    scalingPolicy_location,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_policyType,
    scalingPolicy_metricName,
    scalingPolicy_fleetArn,
    scalingPolicy_comparisonOperator,
    scalingPolicy_name,
    scalingPolicy_threshold,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_updateStatus,
    scalingPolicy_fleetId,
    scalingPolicy_targetConfiguration,

    -- ** Script
    script_creationTime,
    script_storageLocation,
    script_name,
    script_scriptId,
    script_version,
    script_scriptArn,
    script_sizeOnDisk,

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
    vpcPeeringAuthorization_peerVpcId,
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_expirationTime,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_peerVpcId,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_fleetId,

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
import Amazonka.GameLift.DeleteMatchmakingConfiguration
import Amazonka.GameLift.DeleteMatchmakingRuleSet
import Amazonka.GameLift.DeleteScalingPolicy
import Amazonka.GameLift.DeleteScript
import Amazonka.GameLift.DeleteVpcPeeringAuthorization
import Amazonka.GameLift.DeleteVpcPeeringConnection
import Amazonka.GameLift.DeregisterGameServer
import Amazonka.GameLift.DescribeAlias
import Amazonka.GameLift.DescribeBuild
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
import Amazonka.GameLift.GetGameSessionLogUrl
import Amazonka.GameLift.GetInstanceAccess
import Amazonka.GameLift.ListAliases
import Amazonka.GameLift.ListBuilds
import Amazonka.GameLift.ListFleets
import Amazonka.GameLift.ListGameServerGroups
import Amazonka.GameLift.ListGameServers
import Amazonka.GameLift.ListScripts
import Amazonka.GameLift.ListTagsForResource
import Amazonka.GameLift.PutScalingPolicy
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
import Amazonka.GameLift.Types.AttributeValue
import Amazonka.GameLift.Types.AwsCredentials
import Amazonka.GameLift.Types.Build
import Amazonka.GameLift.Types.CertificateConfiguration
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
