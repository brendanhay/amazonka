{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    createAlias_tags,
    createAlias_description,
    createAlias_name,
    createAlias_routingStrategy,
    createAliasResponse_alias,
    createAliasResponse_httpStatus,

    -- ** CreateBuild
    createBuild_tags,
    createBuild_operatingSystem,
    createBuild_name,
    createBuild_storageLocation,
    createBuild_version,
    createBuildResponse_build,
    createBuildResponse_uploadCredentials,
    createBuildResponse_storageLocation,
    createBuildResponse_httpStatus,

    -- ** CreateFleet
    createFleet_tags,
    createFleet_serverLaunchPath,
    createFleet_fleetType,
    createFleet_certificateConfiguration,
    createFleet_instanceRoleArn,
    createFleet_buildId,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_description,
    createFleet_logPaths,
    createFleet_runtimeConfiguration,
    createFleet_peerVpcId,
    createFleet_metricGroups,
    createFleet_serverLaunchParameters,
    createFleet_locations,
    createFleet_eC2InboundPermissions,
    createFleet_scriptId,
    createFleet_peerVpcAwsAccountId,
    createFleet_resourceCreationLimitPolicy,
    createFleet_name,
    createFleet_eC2InstanceType,
    createFleetResponse_fleetAttributes,
    createFleetResponse_locationStates,
    createFleetResponse_httpStatus,

    -- ** CreateFleetLocations
    createFleetLocations_fleetId,
    createFleetLocations_locations,
    createFleetLocationsResponse_fleetId,
    createFleetLocationsResponse_locationStates,
    createFleetLocationsResponse_fleetArn,
    createFleetLocationsResponse_httpStatus,

    -- ** CreateGameServerGroup
    createGameServerGroup_tags,
    createGameServerGroup_gameServerProtectionPolicy,
    createGameServerGroup_vpcSubnets,
    createGameServerGroup_balancingStrategy,
    createGameServerGroup_autoScalingPolicy,
    createGameServerGroup_gameServerGroupName,
    createGameServerGroup_roleArn,
    createGameServerGroup_minSize,
    createGameServerGroup_maxSize,
    createGameServerGroup_launchTemplate,
    createGameServerGroup_instanceDefinitions,
    createGameServerGroupResponse_gameServerGroup,
    createGameServerGroupResponse_httpStatus,

    -- ** CreateGameSession
    createGameSession_gameSessionId,
    createGameSession_fleetId,
    createGameSession_creatorId,
    createGameSession_name,
    createGameSession_gameSessionData,
    createGameSession_aliasId,
    createGameSession_idempotencyToken,
    createGameSession_location,
    createGameSession_gameProperties,
    createGameSession_maximumPlayerSessionCount,
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,

    -- ** CreateGameSessionQueue
    createGameSessionQueue_tags,
    createGameSessionQueue_notificationTarget,
    createGameSessionQueue_priorityConfiguration,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_destinations,
    createGameSessionQueue_filterConfiguration,
    createGameSessionQueue_customEventData,
    createGameSessionQueue_name,
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,

    -- ** CreateMatchmakingConfiguration
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_customEventData,
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
    createScript_tags,
    createScript_name,
    createScript_zipFile,
    createScript_storageLocation,
    createScript_version,
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
    deleteFleetLocationsResponse_fleetId,
    deleteFleetLocationsResponse_locationStates,
    deleteFleetLocationsResponse_fleetArn,
    deleteFleetLocationsResponse_httpStatus,

    -- ** DeleteGameServerGroup
    deleteGameServerGroup_deleteOption,
    deleteGameServerGroup_gameServerGroupName,
    deleteGameServerGroupResponse_gameServerGroup,
    deleteGameServerGroupResponse_httpStatus,

    -- ** DeleteGameSessionQueue
    deleteGameSessionQueue_name,
    deleteGameSessionQueueResponse_httpStatus,

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

    -- ** DescribeEC2InstanceLimits
    describeEC2InstanceLimits_eC2InstanceType,
    describeEC2InstanceLimits_location,
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,

    -- ** DescribeFleetAttributes
    describeFleetAttributes_nextToken,
    describeFleetAttributes_fleetIds,
    describeFleetAttributes_limit,
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_httpStatus,

    -- ** DescribeFleetCapacity
    describeFleetCapacity_nextToken,
    describeFleetCapacity_fleetIds,
    describeFleetCapacity_limit,
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_httpStatus,

    -- ** DescribeFleetEvents
    describeFleetEvents_nextToken,
    describeFleetEvents_endTime,
    describeFleetEvents_limit,
    describeFleetEvents_startTime,
    describeFleetEvents_fleetId,
    describeFleetEventsResponse_nextToken,
    describeFleetEventsResponse_events,
    describeFleetEventsResponse_httpStatus,

    -- ** DescribeFleetLocationAttributes
    describeFleetLocationAttributes_nextToken,
    describeFleetLocationAttributes_limit,
    describeFleetLocationAttributes_locations,
    describeFleetLocationAttributes_fleetId,
    describeFleetLocationAttributesResponse_fleetId,
    describeFleetLocationAttributesResponse_nextToken,
    describeFleetLocationAttributesResponse_locationAttributes,
    describeFleetLocationAttributesResponse_fleetArn,
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
    describeFleetPortSettingsResponse_fleetId,
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_updateStatus,
    describeFleetPortSettingsResponse_location,
    describeFleetPortSettingsResponse_fleetArn,
    describeFleetPortSettingsResponse_httpStatus,

    -- ** DescribeFleetUtilization
    describeFleetUtilization_nextToken,
    describeFleetUtilization_fleetIds,
    describeFleetUtilization_limit,
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_fleetUtilization,
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
    describeGameServerInstances_nextToken,
    describeGameServerInstances_limit,
    describeGameServerInstances_instanceIds,
    describeGameServerInstances_gameServerGroupName,
    describeGameServerInstancesResponse_nextToken,
    describeGameServerInstancesResponse_gameServerInstances,
    describeGameServerInstancesResponse_httpStatus,

    -- ** DescribeGameSessionDetails
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_location,
    describeGameSessionDetails_limit,
    describeGameSessionDetails_statusFilter,
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_httpStatus,

    -- ** DescribeGameSessionPlacement
    describeGameSessionPlacement_placementId,
    describeGameSessionPlacementResponse_gameSessionPlacement,
    describeGameSessionPlacementResponse_httpStatus,

    -- ** DescribeGameSessionQueues
    describeGameSessionQueues_nextToken,
    describeGameSessionQueues_names,
    describeGameSessionQueues_limit,
    describeGameSessionQueuesResponse_nextToken,
    describeGameSessionQueuesResponse_gameSessionQueues,
    describeGameSessionQueuesResponse_httpStatus,

    -- ** DescribeGameSessions
    describeGameSessions_gameSessionId,
    describeGameSessions_fleetId,
    describeGameSessions_nextToken,
    describeGameSessions_aliasId,
    describeGameSessions_location,
    describeGameSessions_limit,
    describeGameSessions_statusFilter,
    describeGameSessionsResponse_nextToken,
    describeGameSessionsResponse_gameSessions,
    describeGameSessionsResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_nextToken,
    describeInstances_location,
    describeInstances_instanceId,
    describeInstances_limit,
    describeInstances_fleetId,
    describeInstancesResponse_instances,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_httpStatus,

    -- ** DescribeMatchmaking
    describeMatchmaking_ticketIds,
    describeMatchmakingResponse_ticketList,
    describeMatchmakingResponse_httpStatus,

    -- ** DescribeMatchmakingConfigurations
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_ruleSetName,
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_limit,
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_httpStatus,

    -- ** DescribeMatchmakingRuleSets
    describeMatchmakingRuleSets_nextToken,
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_limit,
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,

    -- ** DescribePlayerSessions
    describePlayerSessions_gameSessionId,
    describePlayerSessions_nextToken,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_playerSessionStatusFilter,
    describePlayerSessions_playerId,
    describePlayerSessions_limit,
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,

    -- ** DescribeRuntimeConfiguration
    describeRuntimeConfiguration_fleetId,
    describeRuntimeConfigurationResponse_runtimeConfiguration,
    describeRuntimeConfigurationResponse_httpStatus,

    -- ** DescribeScalingPolicies
    describeScalingPolicies_nextToken,
    describeScalingPolicies_location,
    describeScalingPolicies_limit,
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
    listAliases_name,
    listAliases_nextToken,
    listAliases_limit,
    listAliases_routingStrategyType,
    listAliasesResponse_nextToken,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_nextToken,
    listBuilds_status,
    listBuilds_limit,
    listBuildsResponse_nextToken,
    listBuildsResponse_builds,
    listBuildsResponse_httpStatus,

    -- ** ListFleets
    listFleets_nextToken,
    listFleets_buildId,
    listFleets_limit,
    listFleets_scriptId,
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetIds,
    listFleetsResponse_httpStatus,

    -- ** ListGameServerGroups
    listGameServerGroups_nextToken,
    listGameServerGroups_limit,
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_httpStatus,

    -- ** ListGameServers
    listGameServers_sortOrder,
    listGameServers_nextToken,
    listGameServers_limit,
    listGameServers_gameServerGroupName,
    listGameServersResponse_nextToken,
    listGameServersResponse_gameServers,
    listGameServersResponse_httpStatus,

    -- ** ListScripts
    listScripts_nextToken,
    listScripts_limit,
    listScriptsResponse_nextToken,
    listScriptsResponse_scripts,
    listScriptsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_policyType,
    putScalingPolicy_evaluationPeriods,
    putScalingPolicy_targetConfiguration,
    putScalingPolicy_threshold,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_comparisonOperator,
    putScalingPolicy_scalingAdjustmentType,
    putScalingPolicy_name,
    putScalingPolicy_fleetId,
    putScalingPolicy_metricName,
    putScalingPolicyResponse_name,
    putScalingPolicyResponse_httpStatus,

    -- ** RegisterGameServer
    registerGameServer_gameServerData,
    registerGameServer_connectionInfo,
    registerGameServer_gameServerGroupName,
    registerGameServer_gameServerId,
    registerGameServer_instanceId,
    registerGameServerResponse_gameServer,
    registerGameServerResponse_httpStatus,

    -- ** RequestUploadCredentials
    requestUploadCredentials_buildId,
    requestUploadCredentialsResponse_uploadCredentials,
    requestUploadCredentialsResponse_storageLocation,
    requestUploadCredentialsResponse_httpStatus,

    -- ** ResolveAlias
    resolveAlias_aliasId,
    resolveAliasResponse_fleetId,
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_httpStatus,

    -- ** ResumeGameServerGroup
    resumeGameServerGroup_gameServerGroupName,
    resumeGameServerGroup_resumeActions,
    resumeGameServerGroupResponse_gameServerGroup,
    resumeGameServerGroupResponse_httpStatus,

    -- ** SearchGameSessions
    searchGameSessions_fleetId,
    searchGameSessions_sortExpression,
    searchGameSessions_nextToken,
    searchGameSessions_aliasId,
    searchGameSessions_filterExpression,
    searchGameSessions_location,
    searchGameSessions_limit,
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_httpStatus,

    -- ** StartFleetActions
    startFleetActions_location,
    startFleetActions_fleetId,
    startFleetActions_actions,
    startFleetActionsResponse_fleetId,
    startFleetActionsResponse_fleetArn,
    startFleetActionsResponse_httpStatus,

    -- ** StartGameSessionPlacement
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_desiredPlayerSessions,
    startGameSessionPlacement_playerLatencies,
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_placementId,
    startGameSessionPlacement_gameSessionQueueName,
    startGameSessionPlacement_maximumPlayerSessionCount,
    startGameSessionPlacementResponse_gameSessionPlacement,
    startGameSessionPlacementResponse_httpStatus,

    -- ** StartMatchBackfill
    startMatchBackfill_ticketId,
    startMatchBackfill_gameSessionArn,
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
    stopFleetActionsResponse_fleetId,
    stopFleetActionsResponse_fleetArn,
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
    updateAlias_name,
    updateAlias_routingStrategy,
    updateAlias_description,
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
    updateFleetAttributes_name,
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_description,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_fleetId,
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,

    -- ** UpdateFleetCapacity
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_location,
    updateFleetCapacity_minSize,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_fleetId,
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_location,
    updateFleetCapacityResponse_fleetArn,
    updateFleetCapacityResponse_httpStatus,

    -- ** UpdateFleetPortSettings
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_fleetId,
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
    updateGameServerGroup_roleArn,
    updateGameServerGroup_instanceDefinitions,
    updateGameServerGroup_gameServerProtectionPolicy,
    updateGameServerGroup_balancingStrategy,
    updateGameServerGroup_gameServerGroupName,
    updateGameServerGroupResponse_gameServerGroup,
    updateGameServerGroupResponse_httpStatus,

    -- ** UpdateGameSession
    updateGameSession_name,
    updateGameSession_protectionPolicy,
    updateGameSession_playerSessionCreationPolicy,
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_gameSessionId,
    updateGameSessionResponse_gameSession,
    updateGameSessionResponse_httpStatus,

    -- ** UpdateGameSessionQueue
    updateGameSessionQueue_notificationTarget,
    updateGameSessionQueue_priorityConfiguration,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_filterConfiguration,
    updateGameSessionQueue_customEventData,
    updateGameSessionQueue_name,
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,

    -- ** UpdateMatchmakingConfiguration
    updateMatchmakingConfiguration_notificationTarget,
    updateMatchmakingConfiguration_acceptanceTimeoutSeconds,
    updateMatchmakingConfiguration_ruleSetName,
    updateMatchmakingConfiguration_acceptanceRequired,
    updateMatchmakingConfiguration_additionalPlayerCount,
    updateMatchmakingConfiguration_gameSessionData,
    updateMatchmakingConfiguration_flexMatchMode,
    updateMatchmakingConfiguration_description,
    updateMatchmakingConfiguration_backfillMode,
    updateMatchmakingConfiguration_gameSessionQueueArns,
    updateMatchmakingConfiguration_gameProperties,
    updateMatchmakingConfiguration_requestTimeoutSeconds,
    updateMatchmakingConfiguration_customEventData,
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
    updateScript_zipFile,
    updateScript_storageLocation,
    updateScript_version,
    updateScript_scriptId,
    updateScriptResponse_script,
    updateScriptResponse_httpStatus,

    -- ** ValidateMatchmakingRuleSet
    validateMatchmakingRuleSet_ruleSetBody,
    validateMatchmakingRuleSetResponse_valid,
    validateMatchmakingRuleSetResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_name,
    alias_aliasId,
    alias_aliasArn,
    alias_routingStrategy,
    alias_lastUpdatedTime,
    alias_description,
    alias_creationTime,

    -- ** AttributeValue
    attributeValue_sl,
    attributeValue_s,
    attributeValue_sdm,
    attributeValue_n,

    -- ** AwsCredentials
    awsCredentials_sessionToken,
    awsCredentials_secretAccessKey,
    awsCredentials_accessKeyId,

    -- ** Build
    build_operatingSystem,
    build_name,
    build_buildId,
    build_buildArn,
    build_sizeOnDisk,
    build_status,
    build_creationTime,
    build_version,

    -- ** CertificateConfiguration
    certificateConfiguration_certificateType,

    -- ** DesiredPlayerSession
    desiredPlayerSession_playerId,
    desiredPlayerSession_playerData,

    -- ** EC2InstanceCounts
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_active,
    eC2InstanceCounts_terminating,
    eC2InstanceCounts_desired,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_idle,

    -- ** EC2InstanceLimit
    eC2InstanceLimit_instanceLimit,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_location,

    -- ** Event
    event_resourceId,
    event_eventCode,
    event_message,
    event_eventId,
    event_eventTime,
    event_preSignedLogUrl,

    -- ** FilterConfiguration
    filterConfiguration_allowedLocations,

    -- ** FleetAttributes
    fleetAttributes_scriptArn,
    fleetAttributes_operatingSystem,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_fleetId,
    fleetAttributes_name,
    fleetAttributes_fleetType,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_buildId,
    fleetAttributes_buildArn,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_stoppedActions,
    fleetAttributes_status,
    fleetAttributes_description,
    fleetAttributes_instanceType,
    fleetAttributes_fleetArn,
    fleetAttributes_terminationTime,
    fleetAttributes_logPaths,
    fleetAttributes_creationTime,
    fleetAttributes_metricGroups,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_scriptId,
    fleetAttributes_resourceCreationLimitPolicy,

    -- ** FleetCapacity
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,
    fleetCapacity_location,
    fleetCapacity_instanceType,
    fleetCapacity_fleetArn,

    -- ** FleetUtilization
    fleetUtilization_fleetId,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_location,
    fleetUtilization_fleetArn,
    fleetUtilization_maximumPlayerSessionCount,
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_activeServerProcessCount,

    -- ** GameProperty
    gameProperty_key,
    gameProperty_value,

    -- ** GameServer
    gameServer_gameServerData,
    gameServer_claimStatus,
    gameServer_gameServerGroupName,
    gameServer_lastClaimTime,
    gameServer_instanceId,
    gameServer_gameServerGroupArn,
    gameServer_registrationTime,
    gameServer_lastHealthCheckTime,
    gameServer_connectionInfo,
    gameServer_utilizationStatus,
    gameServer_gameServerId,

    -- ** GameServerGroup
    gameServerGroup_suspendedActions,
    gameServerGroup_roleArn,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_statusReason,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_status,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_lastUpdatedTime,
    gameServerGroup_balancingStrategy,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_creationTime,

    -- ** GameServerGroupAutoScalingPolicy
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- ** GameServerInstance
    gameServerInstance_instanceStatus,
    gameServerInstance_gameServerGroupName,
    gameServerInstance_instanceId,
    gameServerInstance_gameServerGroupArn,

    -- ** GameSession
    gameSession_port,
    gameSession_matchmakerData,
    gameSession_gameSessionId,
    gameSession_fleetId,
    gameSession_creatorId,
    gameSession_name,
    gameSession_currentPlayerSessionCount,
    gameSession_gameSessionData,
    gameSession_playerSessionCreationPolicy,
    gameSession_statusReason,
    gameSession_status,
    gameSession_location,
    gameSession_fleetArn,
    gameSession_terminationTime,
    gameSession_maximumPlayerSessionCount,
    gameSession_gameProperties,
    gameSession_creationTime,
    gameSession_dnsName,
    gameSession_ipAddress,

    -- ** GameSessionConnectionInfo
    gameSessionConnectionInfo_port,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_dnsName,
    gameSessionConnectionInfo_ipAddress,

    -- ** GameSessionDetail
    gameSessionDetail_protectionPolicy,
    gameSessionDetail_gameSession,

    -- ** GameSessionPlacement
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_port,
    gameSessionPlacement_placementId,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_status,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_endTime,
    gameSessionPlacement_playerLatencies,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_startTime,
    gameSessionPlacement_ipAddress,

    -- ** GameSessionQueue
    gameSessionQueue_notificationTarget,
    gameSessionQueue_name,
    gameSessionQueue_priorityConfiguration,
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_timeoutInSeconds,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_destinations,
    gameSessionQueue_filterConfiguration,
    gameSessionQueue_customEventData,

    -- ** GameSessionQueueDestination
    gameSessionQueueDestination_destinationArn,

    -- ** Instance
    instance_operatingSystem,
    instance_fleetId,
    instance_type,
    instance_status,
    instance_location,
    instance_instanceId,
    instance_fleetArn,
    instance_creationTime,
    instance_dnsName,
    instance_ipAddress,

    -- ** InstanceAccess
    instanceAccess_operatingSystem,
    instanceAccess_fleetId,
    instanceAccess_instanceId,
    instanceAccess_credentials,
    instanceAccess_ipAddress,

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
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- ** LocationAttributes
    locationAttributes_updateStatus,
    locationAttributes_stoppedActions,
    locationAttributes_locationState,

    -- ** LocationConfiguration
    locationConfiguration_location,

    -- ** LocationState
    locationState_status,
    locationState_location,

    -- ** MatchedPlayerSession
    matchedPlayerSession_playerSessionId,
    matchedPlayerSession_playerId,

    -- ** MatchmakingConfiguration
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_name,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_ruleSetName,
    matchmakingConfiguration_acceptanceRequired,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_ruleSetArn,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_description,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_customEventData,

    -- ** MatchmakingRuleSet
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetBody,

    -- ** MatchmakingTicket
    matchmakingTicket_players,
    matchmakingTicket_configurationArn,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_statusReason,
    matchmakingTicket_status,
    matchmakingTicket_endTime,
    matchmakingTicket_ticketId,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_configurationName,
    matchmakingTicket_statusMessage,
    matchmakingTicket_startTime,

    -- ** PlacedPlayerSession
    placedPlayerSession_playerSessionId,
    placedPlayerSession_playerId,

    -- ** Player
    player_team,
    player_playerAttributes,
    player_latencyInMs,
    player_playerId,

    -- ** PlayerLatency
    playerLatency_latencyInMilliseconds,
    playerLatency_playerId,
    playerLatency_regionIdentifier,

    -- ** PlayerLatencyPolicy
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,
    playerLatencyPolicy_policyDurationSeconds,

    -- ** PlayerSession
    playerSession_port,
    playerSession_gameSessionId,
    playerSession_fleetId,
    playerSession_playerSessionId,
    playerSession_status,
    playerSession_playerId,
    playerSession_fleetArn,
    playerSession_terminationTime,
    playerSession_creationTime,
    playerSession_dnsName,
    playerSession_playerData,
    playerSession_ipAddress,

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
    runtimeConfiguration_maxConcurrentGameSessionActivations,
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_serverProcesses,

    -- ** S3Location
    s3Location_key,
    s3Location_roleArn,
    s3Location_bucket,
    s3Location_objectVersion,

    -- ** ScalingPolicy
    scalingPolicy_fleetId,
    scalingPolicy_name,
    scalingPolicy_policyType,
    scalingPolicy_updateStatus,
    scalingPolicy_status,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_location,
    scalingPolicy_fleetArn,
    scalingPolicy_metricName,
    scalingPolicy_targetConfiguration,
    scalingPolicy_threshold,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_comparisonOperator,
    scalingPolicy_scalingAdjustmentType,

    -- ** Script
    script_scriptArn,
    script_name,
    script_sizeOnDisk,
    script_storageLocation,
    script_creationTime,
    script_scriptId,
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
    vpcPeeringAuthorization_expirationTime,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_creationTime,
    vpcPeeringAuthorization_peerVpcId,
    vpcPeeringAuthorization_peerVpcAwsAccountId,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_fleetId,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_peerVpcId,

    -- ** VpcPeeringConnectionStatus
    vpcPeeringConnectionStatus_message,
    vpcPeeringConnectionStatus_code,
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
