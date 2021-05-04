{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Lens
  ( -- * Operations

    -- ** DescribeGameSessionQueues
    describeGameSessionQueues_names,
    describeGameSessionQueues_nextToken,
    describeGameSessionQueues_limit,
    describeGameSessionQueuesResponse_nextToken,
    describeGameSessionQueuesResponse_gameSessionQueues,
    describeGameSessionQueuesResponse_httpStatus,

    -- ** DeleteBuild
    deleteBuild_buildId,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_fleetId,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** DescribeFleetPortSettings
    describeFleetPortSettings_fleetId,
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_nextToken,
    describeInstances_instanceId,
    describeInstances_limit,
    describeInstances_fleetId,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_aliasId,

    -- ** DescribeFleetCapacity
    describeFleetCapacity_nextToken,
    describeFleetCapacity_fleetIds,
    describeFleetCapacity_limit,
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_status,
    listBuilds_nextToken,
    listBuilds_limit,
    listBuildsResponse_nextToken,
    listBuildsResponse_builds,
    listBuildsResponse_httpStatus,

    -- ** UpdateBuild
    updateBuild_version,
    updateBuild_name,
    updateBuild_buildId,
    updateBuildResponse_build,
    updateBuildResponse_httpStatus,

    -- ** UpdateAlias
    updateAlias_routingStrategy,
    updateAlias_name,
    updateAlias_description,
    updateAlias_aliasId,
    updateAliasResponse_alias,
    updateAliasResponse_httpStatus,

    -- ** DescribeFleetAttributes
    describeFleetAttributes_nextToken,
    describeFleetAttributes_fleetIds,
    describeFleetAttributes_limit,
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_threshold,
    putScalingPolicy_targetConfiguration,
    putScalingPolicy_comparisonOperator,
    putScalingPolicy_policyType,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_evaluationPeriods,
    putScalingPolicy_scalingAdjustmentType,
    putScalingPolicy_name,
    putScalingPolicy_fleetId,
    putScalingPolicy_metricName,
    putScalingPolicyResponse_name,
    putScalingPolicyResponse_httpStatus,

    -- ** DescribeFleetEvents
    describeFleetEvents_nextToken,
    describeFleetEvents_startTime,
    describeFleetEvents_endTime,
    describeFleetEvents_limit,
    describeFleetEvents_fleetId,
    describeFleetEventsResponse_nextToken,
    describeFleetEventsResponse_events,
    describeFleetEventsResponse_httpStatus,

    -- ** DescribeFleetUtilization
    describeFleetUtilization_nextToken,
    describeFleetUtilization_fleetIds,
    describeFleetUtilization_limit,
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_fleetUtilization,
    describeFleetUtilizationResponse_httpStatus,

    -- ** ClaimGameServer
    claimGameServer_gameServerData,
    claimGameServer_gameServerId,
    claimGameServer_gameServerGroupName,
    claimGameServerResponse_gameServer,
    claimGameServerResponse_httpStatus,

    -- ** UpdateGameSession
    updateGameSession_playerSessionCreationPolicy,
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_name,
    updateGameSession_protectionPolicy,
    updateGameSession_gameSessionId,
    updateGameSessionResponse_gameSession,
    updateGameSessionResponse_httpStatus,

    -- ** DescribeGameServerGroup
    describeGameServerGroup_gameServerGroupName,
    describeGameServerGroupResponse_gameServerGroup,
    describeGameServerGroupResponse_httpStatus,

    -- ** DescribeMatchmaking
    describeMatchmaking_ticketIds,
    describeMatchmakingResponse_ticketList,
    describeMatchmakingResponse_httpStatus,

    -- ** GetGameSessionLogUrl
    getGameSessionLogUrl_gameSessionId,
    getGameSessionLogUrlResponse_preSignedUrl,
    getGameSessionLogUrlResponse_httpStatus,

    -- ** CreatePlayerSession
    createPlayerSession_playerData,
    createPlayerSession_gameSessionId,
    createPlayerSession_playerId,
    createPlayerSessionResponse_playerSession,
    createPlayerSessionResponse_httpStatus,

    -- ** DescribeRuntimeConfiguration
    describeRuntimeConfiguration_fleetId,
    describeRuntimeConfigurationResponse_runtimeConfiguration,
    describeRuntimeConfigurationResponse_httpStatus,

    -- ** DescribeScalingPolicies
    describeScalingPolicies_nextToken,
    describeScalingPolicies_statusFilter,
    describeScalingPolicies_limit,
    describeScalingPolicies_fleetId,
    describeScalingPoliciesResponse_nextToken,
    describeScalingPoliciesResponse_scalingPolicies,
    describeScalingPoliciesResponse_httpStatus,

    -- ** SuspendGameServerGroup
    suspendGameServerGroup_gameServerGroupName,
    suspendGameServerGroup_suspendActions,
    suspendGameServerGroupResponse_gameServerGroup,
    suspendGameServerGroupResponse_httpStatus,

    -- ** DescribeMatchmakingRuleSets
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_nextToken,
    describeMatchmakingRuleSets_limit,
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,

    -- ** ValidateMatchmakingRuleSet
    validateMatchmakingRuleSet_ruleSetBody,
    validateMatchmakingRuleSetResponse_valid,
    validateMatchmakingRuleSetResponse_httpStatus,

    -- ** UpdateFleetPortSettings
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_fleetId,
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,

    -- ** DescribeBuild
    describeBuild_buildId,
    describeBuildResponse_build,
    describeBuildResponse_httpStatus,

    -- ** AcceptMatch
    acceptMatch_ticketId,
    acceptMatch_playerIds,
    acceptMatch_acceptanceType,
    acceptMatchResponse_httpStatus,

    -- ** DeregisterGameServer
    deregisterGameServer_gameServerGroupName,
    deregisterGameServer_gameServerId,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateFleetCapacity
    updateFleetCapacity_minSize,
    updateFleetCapacity_maxSize,
    updateFleetCapacity_desiredInstances,
    updateFleetCapacity_fleetId,
    updateFleetCapacityResponse_fleetId,
    updateFleetCapacityResponse_httpStatus,

    -- ** DescribeAlias
    describeAlias_aliasId,
    describeAliasResponse_alias,
    describeAliasResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_fleetId,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** UpdateFleetAttributes
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_name,
    updateFleetAttributes_description,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_fleetId,
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,

    -- ** UpdateGameSessionQueue
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_name,
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteGameSessionQueue
    deleteGameSessionQueue_name,
    deleteGameSessionQueueResponse_httpStatus,

    -- ** UpdateMatchmakingConfiguration
    updateMatchmakingConfiguration_customEventData,
    updateMatchmakingConfiguration_gameProperties,
    updateMatchmakingConfiguration_flexMatchMode,
    updateMatchmakingConfiguration_backfillMode,
    updateMatchmakingConfiguration_additionalPlayerCount,
    updateMatchmakingConfiguration_acceptanceTimeoutSeconds,
    updateMatchmakingConfiguration_gameSessionData,
    updateMatchmakingConfiguration_gameSessionQueueArns,
    updateMatchmakingConfiguration_notificationTarget,
    updateMatchmakingConfiguration_requestTimeoutSeconds,
    updateMatchmakingConfiguration_description,
    updateMatchmakingConfiguration_ruleSetName,
    updateMatchmakingConfiguration_acceptanceRequired,
    updateMatchmakingConfiguration_name,
    updateMatchmakingConfigurationResponse_configuration,
    updateMatchmakingConfigurationResponse_httpStatus,

    -- ** DeleteMatchmakingConfiguration
    deleteMatchmakingConfiguration_name,
    deleteMatchmakingConfigurationResponse_httpStatus,

    -- ** DescribeMatchmakingConfigurations
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_ruleSetName,
    describeMatchmakingConfigurations_limit,
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_httpStatus,

    -- ** CreateFleet
    createFleet_fleetType,
    createFleet_peerVpcAwsAccountId,
    createFleet_instanceRoleArn,
    createFleet_certificateConfiguration,
    createFleet_serverLaunchPath,
    createFleet_serverLaunchParameters,
    createFleet_logPaths,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_runtimeConfiguration,
    createFleet_tags,
    createFleet_eC2InboundPermissions,
    createFleet_description,
    createFleet_resourceCreationLimitPolicy,
    createFleet_buildId,
    createFleet_metricGroups,
    createFleet_peerVpcId,
    createFleet_scriptId,
    createFleet_name,
    createFleet_eC2InstanceType,
    createFleetResponse_fleetAttributes,
    createFleetResponse_httpStatus,

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
    requestUploadCredentialsResponse_storageLocation,
    requestUploadCredentialsResponse_uploadCredentials,
    requestUploadCredentialsResponse_httpStatus,

    -- ** ResolveAlias
    resolveAlias_aliasId,
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_fleetId,
    resolveAliasResponse_httpStatus,

    -- ** StartMatchBackfill
    startMatchBackfill_ticketId,
    startMatchBackfill_gameSessionArn,
    startMatchBackfill_configurationName,
    startMatchBackfill_players,
    startMatchBackfillResponse_matchmakingTicket,
    startMatchBackfillResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_fleetId,

    -- ** DescribeGameSessionDetails
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_statusFilter,
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_limit,
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_httpStatus,

    -- ** ListFleets
    listFleets_nextToken,
    listFleets_buildId,
    listFleets_limit,
    listFleets_scriptId,
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetIds,
    listFleetsResponse_httpStatus,

    -- ** StopMatchmaking
    stopMatchmaking_ticketId,
    stopMatchmakingResponse_httpStatus,

    -- ** DescribeGameServerInstances
    describeGameServerInstances_instanceIds,
    describeGameServerInstances_nextToken,
    describeGameServerInstances_limit,
    describeGameServerInstances_gameServerGroupName,
    describeGameServerInstancesResponse_nextToken,
    describeGameServerInstancesResponse_gameServerInstances,
    describeGameServerInstancesResponse_httpStatus,

    -- ** CreateGameSession
    createGameSession_gameProperties,
    createGameSession_idempotencyToken,
    createGameSession_creatorId,
    createGameSession_fleetId,
    createGameSession_gameSessionData,
    createGameSession_gameSessionId,
    createGameSession_name,
    createGameSession_aliasId,
    createGameSession_maximumPlayerSessionCount,
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,

    -- ** CreateMatchmakingRuleSet
    createMatchmakingRuleSet_tags,
    createMatchmakingRuleSet_name,
    createMatchmakingRuleSet_ruleSetBody,
    createMatchmakingRuleSetResponse_httpStatus,
    createMatchmakingRuleSetResponse_ruleSet,

    -- ** StartMatchmaking
    startMatchmaking_ticketId,
    startMatchmaking_configurationName,
    startMatchmaking_players,
    startMatchmakingResponse_matchmakingTicket,
    startMatchmakingResponse_httpStatus,

    -- ** DescribeGameSessionPlacement
    describeGameSessionPlacement_placementId,
    describeGameSessionPlacementResponse_gameSessionPlacement,
    describeGameSessionPlacementResponse_httpStatus,

    -- ** DeleteScalingPolicy
    deleteScalingPolicy_name,
    deleteScalingPolicy_fleetId,

    -- ** StopGameSessionPlacement
    stopGameSessionPlacement_placementId,
    stopGameSessionPlacementResponse_gameSessionPlacement,
    stopGameSessionPlacementResponse_httpStatus,

    -- ** StartGameSessionPlacement
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_desiredPlayerSessions,
    startGameSessionPlacement_playerLatencies,
    startGameSessionPlacement_placementId,
    startGameSessionPlacement_gameSessionQueueName,
    startGameSessionPlacement_maximumPlayerSessionCount,
    startGameSessionPlacementResponse_gameSessionPlacement,
    startGameSessionPlacementResponse_httpStatus,

    -- ** DeleteMatchmakingRuleSet
    deleteMatchmakingRuleSet_name,
    deleteMatchmakingRuleSetResponse_httpStatus,

    -- ** DescribeGameServer
    describeGameServer_gameServerGroupName,
    describeGameServer_gameServerId,
    describeGameServerResponse_gameServer,
    describeGameServerResponse_httpStatus,

    -- ** DeleteScript
    deleteScript_scriptId,

    -- ** ListScripts
    listScripts_nextToken,
    listScripts_limit,
    listScriptsResponse_nextToken,
    listScriptsResponse_scripts,
    listScriptsResponse_httpStatus,

    -- ** DescribeGameSessions
    describeGameSessions_nextToken,
    describeGameSessions_fleetId,
    describeGameSessions_gameSessionId,
    describeGameSessions_statusFilter,
    describeGameSessions_aliasId,
    describeGameSessions_limit,
    describeGameSessionsResponse_nextToken,
    describeGameSessionsResponse_gameSessions,
    describeGameSessionsResponse_httpStatus,

    -- ** UpdateScript
    updateScript_zipFile,
    updateScript_version,
    updateScript_name,
    updateScript_storageLocation,
    updateScript_scriptId,
    updateScriptResponse_script,
    updateScriptResponse_httpStatus,

    -- ** DescribeEC2InstanceLimits
    describeEC2InstanceLimits_eC2InstanceType,
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,

    -- ** StopFleetActions
    stopFleetActions_fleetId,
    stopFleetActions_actions,
    stopFleetActionsResponse_httpStatus,

    -- ** GetInstanceAccess
    getInstanceAccess_fleetId,
    getInstanceAccess_instanceId,
    getInstanceAccessResponse_instanceAccess,
    getInstanceAccessResponse_httpStatus,

    -- ** StartFleetActions
    startFleetActions_fleetId,
    startFleetActions_actions,
    startFleetActionsResponse_httpStatus,

    -- ** DescribePlayerSessions
    describePlayerSessions_nextToken,
    describePlayerSessions_playerSessionStatusFilter,
    describePlayerSessions_playerId,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_gameSessionId,
    describePlayerSessions_limit,
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,

    -- ** CreateScript
    createScript_zipFile,
    createScript_version,
    createScript_name,
    createScript_storageLocation,
    createScript_tags,
    createScriptResponse_script,
    createScriptResponse_httpStatus,

    -- ** CreateMatchmakingConfiguration
    createMatchmakingConfiguration_customEventData,
    createMatchmakingConfiguration_gameProperties,
    createMatchmakingConfiguration_flexMatchMode,
    createMatchmakingConfiguration_backfillMode,
    createMatchmakingConfiguration_additionalPlayerCount,
    createMatchmakingConfiguration_acceptanceTimeoutSeconds,
    createMatchmakingConfiguration_gameSessionData,
    createMatchmakingConfiguration_gameSessionQueueArns,
    createMatchmakingConfiguration_notificationTarget,
    createMatchmakingConfiguration_tags,
    createMatchmakingConfiguration_description,
    createMatchmakingConfiguration_name,
    createMatchmakingConfiguration_requestTimeoutSeconds,
    createMatchmakingConfiguration_acceptanceRequired,
    createMatchmakingConfiguration_ruleSetName,
    createMatchmakingConfigurationResponse_configuration,
    createMatchmakingConfigurationResponse_httpStatus,

    -- ** CreateVpcPeeringAuthorization
    createVpcPeeringAuthorization_gameLiftAwsAccountId,
    createVpcPeeringAuthorization_peerVpcId,
    createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization,
    createVpcPeeringAuthorizationResponse_httpStatus,

    -- ** CreateGameServerGroup
    createGameServerGroup_autoScalingPolicy,
    createGameServerGroup_tags,
    createGameServerGroup_balancingStrategy,
    createGameServerGroup_gameServerProtectionPolicy,
    createGameServerGroup_vpcSubnets,
    createGameServerGroup_gameServerGroupName,
    createGameServerGroup_roleArn,
    createGameServerGroup_minSize,
    createGameServerGroup_maxSize,
    createGameServerGroup_launchTemplate,
    createGameServerGroup_instanceDefinitions,
    createGameServerGroupResponse_gameServerGroup,
    createGameServerGroupResponse_httpStatus,

    -- ** UpdateGameServerGroup
    updateGameServerGroup_roleArn,
    updateGameServerGroup_instanceDefinitions,
    updateGameServerGroup_balancingStrategy,
    updateGameServerGroup_gameServerProtectionPolicy,
    updateGameServerGroup_gameServerGroupName,
    updateGameServerGroupResponse_gameServerGroup,
    updateGameServerGroupResponse_httpStatus,

    -- ** SearchGameSessions
    searchGameSessions_nextToken,
    searchGameSessions_fleetId,
    searchGameSessions_sortExpression,
    searchGameSessions_filterExpression,
    searchGameSessions_aliasId,
    searchGameSessions_limit,
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_httpStatus,

    -- ** DeleteGameServerGroup
    deleteGameServerGroup_deleteOption,
    deleteGameServerGroup_gameServerGroupName,
    deleteGameServerGroupResponse_gameServerGroup,
    deleteGameServerGroupResponse_httpStatus,

    -- ** ListGameServerGroups
    listGameServerGroups_nextToken,
    listGameServerGroups_limit,
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_httpStatus,

    -- ** UpdateRuntimeConfiguration
    updateRuntimeConfiguration_fleetId,
    updateRuntimeConfiguration_runtimeConfiguration,
    updateRuntimeConfigurationResponse_runtimeConfiguration,
    updateRuntimeConfigurationResponse_httpStatus,

    -- ** CreateGameSessionQueue
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_destinations,
    createGameSessionQueue_tags,
    createGameSessionQueue_name,
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,

    -- ** ResumeGameServerGroup
    resumeGameServerGroup_gameServerGroupName,
    resumeGameServerGroup_resumeActions,
    resumeGameServerGroupResponse_gameServerGroup,
    resumeGameServerGroupResponse_httpStatus,

    -- ** DeleteVpcPeeringAuthorization
    deleteVpcPeeringAuthorization_gameLiftAwsAccountId,
    deleteVpcPeeringAuthorization_peerVpcId,
    deleteVpcPeeringAuthorizationResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_fleetId,
    createVpcPeeringConnection_peerVpcAwsAccountId,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeScript
    describeScript_scriptId,
    describeScriptResponse_script,
    describeScriptResponse_httpStatus,

    -- ** CreateAlias
    createAlias_tags,
    createAlias_description,
    createAlias_name,
    createAlias_routingStrategy,
    createAliasResponse_alias,
    createAliasResponse_httpStatus,

    -- ** CreatePlayerSessions
    createPlayerSessions_playerDataMap,
    createPlayerSessions_gameSessionId,
    createPlayerSessions_playerIds,
    createPlayerSessionsResponse_playerSessions,
    createPlayerSessionsResponse_httpStatus,

    -- ** UpdateGameServer
    updateGameServer_utilizationStatus,
    updateGameServer_gameServerData,
    updateGameServer_healthCheck,
    updateGameServer_gameServerGroupName,
    updateGameServer_gameServerId,
    updateGameServerResponse_gameServer,
    updateGameServerResponse_httpStatus,

    -- ** DescribeVpcPeeringAuthorizations
    describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations,
    describeVpcPeeringAuthorizationsResponse_httpStatus,

    -- ** ListGameServers
    listGameServers_sortOrder,
    listGameServers_nextToken,
    listGameServers_limit,
    listGameServers_gameServerGroupName,
    listGameServersResponse_nextToken,
    listGameServersResponse_gameServers,
    listGameServersResponse_httpStatus,

    -- ** CreateBuild
    createBuild_version,
    createBuild_name,
    createBuild_storageLocation,
    createBuild_tags,
    createBuild_operatingSystem,
    createBuildResponse_build,
    createBuildResponse_storageLocation,
    createBuildResponse_uploadCredentials,
    createBuildResponse_httpStatus,

    -- ** ListAliases
    listAliases_nextToken,
    listAliases_routingStrategyType,
    listAliases_name,
    listAliases_limit,
    listAliasesResponse_nextToken,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_routingStrategy,
    alias_creationTime,
    alias_name,
    alias_description,
    alias_aliasArn,
    alias_aliasId,
    alias_lastUpdatedTime,

    -- ** AttributeValue
    attributeValue_sl,
    attributeValue_n,
    attributeValue_s,
    attributeValue_sdm,

    -- ** AwsCredentials
    awsCredentials_secretAccessKey,
    awsCredentials_accessKeyId,
    awsCredentials_sessionToken,

    -- ** Build
    build_status,
    build_creationTime,
    build_version,
    build_name,
    build_sizeOnDisk,
    build_buildArn,
    build_buildId,
    build_operatingSystem,

    -- ** CertificateConfiguration
    certificateConfiguration_certificateType,

    -- ** DesiredPlayerSession
    desiredPlayerSession_playerId,
    desiredPlayerSession_playerData,

    -- ** EC2InstanceCounts
    eC2InstanceCounts_idle,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_active,
    eC2InstanceCounts_terminating,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_desired,

    -- ** EC2InstanceLimit
    eC2InstanceLimit_instanceLimit,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_eC2InstanceType,

    -- ** Event
    event_resourceId,
    event_eventCode,
    event_eventId,
    event_message,
    event_eventTime,
    event_preSignedLogUrl,

    -- ** FleetAttributes
    fleetAttributes_status,
    fleetAttributes_creationTime,
    fleetAttributes_instanceType,
    fleetAttributes_fleetType,
    fleetAttributes_fleetArn,
    fleetAttributes_fleetId,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_scriptArn,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_logPaths,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_name,
    fleetAttributes_stoppedActions,
    fleetAttributes_terminationTime,
    fleetAttributes_description,
    fleetAttributes_resourceCreationLimitPolicy,
    fleetAttributes_buildArn,
    fleetAttributes_buildId,
    fleetAttributes_operatingSystem,
    fleetAttributes_metricGroups,
    fleetAttributes_scriptId,

    -- ** FleetCapacity
    fleetCapacity_instanceType,
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,

    -- ** FleetUtilization
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_maximumPlayerSessionCount,
    fleetUtilization_fleetId,
    fleetUtilization_activeServerProcessCount,

    -- ** GameProperty
    gameProperty_key,
    gameProperty_value,

    -- ** GameServer
    gameServer_instanceId,
    gameServer_utilizationStatus,
    gameServer_claimStatus,
    gameServer_gameServerData,
    gameServer_lastClaimTime,
    gameServer_registrationTime,
    gameServer_gameServerGroupArn,
    gameServer_gameServerId,
    gameServer_gameServerGroupName,
    gameServer_connectionInfo,
    gameServer_lastHealthCheckTime,

    -- ** GameServerGroup
    gameServerGroup_status,
    gameServerGroup_creationTime,
    gameServerGroup_roleArn,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_suspendedActions,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_balancingStrategy,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_statusReason,
    gameServerGroup_lastUpdatedTime,

    -- ** GameServerGroupAutoScalingPolicy
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- ** GameServerInstance
    gameServerInstance_instanceId,
    gameServerInstance_instanceStatus,
    gameServerInstance_gameServerGroupArn,
    gameServerInstance_gameServerGroupName,

    -- ** GameSession
    gameSession_gameProperties,
    gameSession_currentPlayerSessionCount,
    gameSession_status,
    gameSession_playerSessionCreationPolicy,
    gameSession_creationTime,
    gameSession_creatorId,
    gameSession_maximumPlayerSessionCount,
    gameSession_fleetArn,
    gameSession_fleetId,
    gameSession_matchmakerData,
    gameSession_gameSessionData,
    gameSession_gameSessionId,
    gameSession_ipAddress,
    gameSession_name,
    gameSession_terminationTime,
    gameSession_port,
    gameSession_dnsName,
    gameSession_statusReason,

    -- ** GameSessionConnectionInfo
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_port,
    gameSessionConnectionInfo_dnsName,

    -- ** GameSessionDetail
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- ** GameSessionPlacement
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_status,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_startTime,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_endTime,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_placementId,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_port,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_playerLatencies,

    -- ** GameSessionQueue
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_timeoutInSeconds,
    gameSessionQueue_destinations,
    gameSessionQueue_name,
    gameSessionQueue_gameSessionQueueArn,

    -- ** GameSessionQueueDestination
    gameSessionQueueDestination_destinationArn,

    -- ** Instance
    instance_status,
    instance_instanceId,
    instance_creationTime,
    instance_fleetId,
    instance_ipAddress,
    instance_dnsName,
    instance_type,
    instance_operatingSystem,

    -- ** InstanceAccess
    instanceAccess_instanceId,
    instanceAccess_fleetId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,
    instanceAccess_credentials,

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

    -- ** MatchedPlayerSession
    matchedPlayerSession_playerId,
    matchedPlayerSession_playerSessionId,

    -- ** MatchmakingConfiguration
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_name,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_ruleSetArn,
    matchmakingConfiguration_description,
    matchmakingConfiguration_ruleSetName,
    matchmakingConfiguration_acceptanceRequired,

    -- ** MatchmakingRuleSet
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetBody,

    -- ** MatchmakingTicket
    matchmakingTicket_statusMessage,
    matchmakingTicket_status,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_ticketId,
    matchmakingTicket_players,
    matchmakingTicket_startTime,
    matchmakingTicket_configurationArn,
    matchmakingTicket_endTime,
    matchmakingTicket_configurationName,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_statusReason,

    -- ** PlacedPlayerSession
    placedPlayerSession_playerId,
    placedPlayerSession_playerSessionId,

    -- ** Player
    player_playerAttributes,
    player_latencyInMs,
    player_playerId,
    player_team,

    -- ** PlayerLatency
    playerLatency_playerId,
    playerLatency_latencyInMilliseconds,
    playerLatency_regionIdentifier,

    -- ** PlayerLatencyPolicy
    playerLatencyPolicy_policyDurationSeconds,
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,

    -- ** PlayerSession
    playerSession_status,
    playerSession_creationTime,
    playerSession_playerId,
    playerSession_fleetArn,
    playerSession_fleetId,
    playerSession_playerSessionId,
    playerSession_gameSessionId,
    playerSession_ipAddress,
    playerSession_terminationTime,
    playerSession_port,
    playerSession_dnsName,
    playerSession_playerData,

    -- ** ResourceCreationLimitPolicy
    resourceCreationLimitPolicy_policyPeriodInMinutes,
    resourceCreationLimitPolicy_newGameSessionsPerCreator,

    -- ** RoutingStrategy
    routingStrategy_fleetId,
    routingStrategy_message,
    routingStrategy_type,

    -- ** RuntimeConfiguration
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_serverProcesses,
    runtimeConfiguration_maxConcurrentGameSessionActivations,

    -- ** S3Location
    s3Location_objectVersion,
    s3Location_key,
    s3Location_roleArn,
    s3Location_bucket,

    -- ** ScalingPolicy
    scalingPolicy_threshold,
    scalingPolicy_status,
    scalingPolicy_targetConfiguration,
    scalingPolicy_comparisonOperator,
    scalingPolicy_fleetId,
    scalingPolicy_metricName,
    scalingPolicy_policyType,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_name,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_scalingAdjustmentType,

    -- ** Script
    script_creationTime,
    script_scriptArn,
    script_version,
    script_name,
    script_storageLocation,
    script_sizeOnDisk,
    script_scriptId,

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
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_expirationTime,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_peerVpcId,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_status,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_fleetId,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_peerVpcId,

    -- ** VpcPeeringConnectionStatus
    vpcPeeringConnectionStatus_message,
    vpcPeeringConnectionStatus_code,
  )
where

import Network.AWS.GameLift.AcceptMatch
import Network.AWS.GameLift.ClaimGameServer
import Network.AWS.GameLift.CreateAlias
import Network.AWS.GameLift.CreateBuild
import Network.AWS.GameLift.CreateFleet
import Network.AWS.GameLift.CreateGameServerGroup
import Network.AWS.GameLift.CreateGameSession
import Network.AWS.GameLift.CreateGameSessionQueue
import Network.AWS.GameLift.CreateMatchmakingConfiguration
import Network.AWS.GameLift.CreateMatchmakingRuleSet
import Network.AWS.GameLift.CreatePlayerSession
import Network.AWS.GameLift.CreatePlayerSessions
import Network.AWS.GameLift.CreateScript
import Network.AWS.GameLift.CreateVpcPeeringAuthorization
import Network.AWS.GameLift.CreateVpcPeeringConnection
import Network.AWS.GameLift.DeleteAlias
import Network.AWS.GameLift.DeleteBuild
import Network.AWS.GameLift.DeleteFleet
import Network.AWS.GameLift.DeleteGameServerGroup
import Network.AWS.GameLift.DeleteGameSessionQueue
import Network.AWS.GameLift.DeleteMatchmakingConfiguration
import Network.AWS.GameLift.DeleteMatchmakingRuleSet
import Network.AWS.GameLift.DeleteScalingPolicy
import Network.AWS.GameLift.DeleteScript
import Network.AWS.GameLift.DeleteVpcPeeringAuthorization
import Network.AWS.GameLift.DeleteVpcPeeringConnection
import Network.AWS.GameLift.DeregisterGameServer
import Network.AWS.GameLift.DescribeAlias
import Network.AWS.GameLift.DescribeBuild
import Network.AWS.GameLift.DescribeEC2InstanceLimits
import Network.AWS.GameLift.DescribeFleetAttributes
import Network.AWS.GameLift.DescribeFleetCapacity
import Network.AWS.GameLift.DescribeFleetEvents
import Network.AWS.GameLift.DescribeFleetPortSettings
import Network.AWS.GameLift.DescribeFleetUtilization
import Network.AWS.GameLift.DescribeGameServer
import Network.AWS.GameLift.DescribeGameServerGroup
import Network.AWS.GameLift.DescribeGameServerInstances
import Network.AWS.GameLift.DescribeGameSessionDetails
import Network.AWS.GameLift.DescribeGameSessionPlacement
import Network.AWS.GameLift.DescribeGameSessionQueues
import Network.AWS.GameLift.DescribeGameSessions
import Network.AWS.GameLift.DescribeInstances
import Network.AWS.GameLift.DescribeMatchmaking
import Network.AWS.GameLift.DescribeMatchmakingConfigurations
import Network.AWS.GameLift.DescribeMatchmakingRuleSets
import Network.AWS.GameLift.DescribePlayerSessions
import Network.AWS.GameLift.DescribeRuntimeConfiguration
import Network.AWS.GameLift.DescribeScalingPolicies
import Network.AWS.GameLift.DescribeScript
import Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
import Network.AWS.GameLift.DescribeVpcPeeringConnections
import Network.AWS.GameLift.GetGameSessionLogUrl
import Network.AWS.GameLift.GetInstanceAccess
import Network.AWS.GameLift.ListAliases
import Network.AWS.GameLift.ListBuilds
import Network.AWS.GameLift.ListFleets
import Network.AWS.GameLift.ListGameServerGroups
import Network.AWS.GameLift.ListGameServers
import Network.AWS.GameLift.ListScripts
import Network.AWS.GameLift.ListTagsForResource
import Network.AWS.GameLift.PutScalingPolicy
import Network.AWS.GameLift.RegisterGameServer
import Network.AWS.GameLift.RequestUploadCredentials
import Network.AWS.GameLift.ResolveAlias
import Network.AWS.GameLift.ResumeGameServerGroup
import Network.AWS.GameLift.SearchGameSessions
import Network.AWS.GameLift.StartFleetActions
import Network.AWS.GameLift.StartGameSessionPlacement
import Network.AWS.GameLift.StartMatchBackfill
import Network.AWS.GameLift.StartMatchmaking
import Network.AWS.GameLift.StopFleetActions
import Network.AWS.GameLift.StopGameSessionPlacement
import Network.AWS.GameLift.StopMatchmaking
import Network.AWS.GameLift.SuspendGameServerGroup
import Network.AWS.GameLift.TagResource
import Network.AWS.GameLift.Types.Alias
import Network.AWS.GameLift.Types.AttributeValue
import Network.AWS.GameLift.Types.AwsCredentials
import Network.AWS.GameLift.Types.Build
import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.DesiredPlayerSession
import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceLimit
import Network.AWS.GameLift.Types.Event
import Network.AWS.GameLift.Types.FleetAttributes
import Network.AWS.GameLift.Types.FleetCapacity
import Network.AWS.GameLift.Types.FleetUtilization
import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameServer
import Network.AWS.GameLift.Types.GameServerGroup
import Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
import Network.AWS.GameLift.Types.GameServerInstance
import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.GameSessionDetail
import Network.AWS.GameLift.Types.GameSessionPlacement
import Network.AWS.GameLift.Types.GameSessionQueue
import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.Instance
import Network.AWS.GameLift.Types.InstanceAccess
import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.InstanceDefinition
import Network.AWS.GameLift.Types.IpPermission
import Network.AWS.GameLift.Types.LaunchTemplateSpecification
import Network.AWS.GameLift.Types.MatchedPlayerSession
import Network.AWS.GameLift.Types.MatchmakingConfiguration
import Network.AWS.GameLift.Types.MatchmakingRuleSet
import Network.AWS.GameLift.Types.MatchmakingTicket
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.Player
import Network.AWS.GameLift.Types.PlayerLatency
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import Network.AWS.GameLift.Types.PlayerSession
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import Network.AWS.GameLift.Types.RoutingStrategy
import Network.AWS.GameLift.Types.RuntimeConfiguration
import Network.AWS.GameLift.Types.S3Location
import Network.AWS.GameLift.Types.ScalingPolicy
import Network.AWS.GameLift.Types.Script
import Network.AWS.GameLift.Types.ServerProcess
import Network.AWS.GameLift.Types.Tag
import Network.AWS.GameLift.Types.TargetConfiguration
import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import Network.AWS.GameLift.Types.VpcPeeringAuthorization
import Network.AWS.GameLift.Types.VpcPeeringConnection
import Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
import Network.AWS.GameLift.UntagResource
import Network.AWS.GameLift.UpdateAlias
import Network.AWS.GameLift.UpdateBuild
import Network.AWS.GameLift.UpdateFleetAttributes
import Network.AWS.GameLift.UpdateFleetCapacity
import Network.AWS.GameLift.UpdateFleetPortSettings
import Network.AWS.GameLift.UpdateGameServer
import Network.AWS.GameLift.UpdateGameServerGroup
import Network.AWS.GameLift.UpdateGameSession
import Network.AWS.GameLift.UpdateGameSessionQueue
import Network.AWS.GameLift.UpdateMatchmakingConfiguration
import Network.AWS.GameLift.UpdateRuntimeConfiguration
import Network.AWS.GameLift.UpdateScript
import Network.AWS.GameLift.ValidateMatchmakingRuleSet
