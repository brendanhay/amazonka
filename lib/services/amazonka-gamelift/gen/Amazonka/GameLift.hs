{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GameLift
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-10-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon GameLift provides solutions for hosting session-based multiplayer
-- game servers in the cloud, including tools for deploying, operating, and
-- scaling game servers. Built on Amazon Web Services global computing
-- infrastructure, GameLift helps you deliver high-performance,
-- high-reliability, low-cost game servers while dynamically scaling your
-- resource usage to meet player demand.
--
-- __About GameLift solutions__
--
-- Get more information on these GameLift solutions in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/ GameLift Developer Guide>.
--
-- -   GameLift managed hosting -- GameLift offers a fully managed service
--     to set up and maintain computing machines for hosting, manage game
--     session and player session life cycle, and handle security, storage,
--     and performance tracking. You can use automatic scaling tools to
--     balance player demand and hosting costs, configure your game session
--     management to minimize player latency, and add FlexMatch for
--     matchmaking.
--
-- -   Managed hosting with Realtime Servers -- With GameLift Realtime
--     Servers, you can quickly configure and set up ready-to-go game
--     servers for your game. Realtime Servers provides a game server
--     framework with core GameLift infrastructure already built in. Then
--     use the full range of GameLift managed hosting features, including
--     FlexMatch, for your game.
--
-- -   GameLift FleetIQ -- Use GameLift FleetIQ as a standalone service
--     while hosting your games using EC2 instances and Auto Scaling
--     groups. GameLift FleetIQ provides optimizations for game hosting,
--     including boosting the viability of low-cost Spot Instances gaming.
--     For a complete solution, pair the GameLift FleetIQ and FlexMatch
--     standalone services.
--
-- -   GameLift FlexMatch -- Add matchmaking to your game hosting solution.
--     FlexMatch is a customizable matchmaking service for multiplayer
--     games. Use FlexMatch as integrated with GameLift managed hosting or
--     incorporate FlexMatch as a standalone service into your own hosting
--     solution.
--
-- __About this API Reference__
--
-- This reference guide describes the low-level service API for Amazon
-- GameLift. With each topic in this guide, you can find links to
-- language-specific SDK guides and the Amazon Web Services CLI reference.
-- Useful links:
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html GameLift API operations listed by tasks>
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-components.html GameLift tools and resources>
module Amazonka.GameLift
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** FleetCapacityExceededException
    _FleetCapacityExceededException,

    -- ** GameSessionFullException
    _GameSessionFullException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidFleetStatusException
    _InvalidFleetStatusException,

    -- ** InvalidGameSessionStatusException
    _InvalidGameSessionStatusException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** OutOfCapacityException
    _OutOfCapacityException,

    -- ** TaggingFailedException
    _TaggingFailedException,

    -- ** TerminalRoutingStrategyException
    _TerminalRoutingStrategyException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** UnsupportedRegionException
    _UnsupportedRegionException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptMatch
    AcceptMatch (AcceptMatch'),
    newAcceptMatch,
    AcceptMatchResponse (AcceptMatchResponse'),
    newAcceptMatchResponse,

    -- ** ClaimGameServer
    ClaimGameServer (ClaimGameServer'),
    newClaimGameServer,
    ClaimGameServerResponse (ClaimGameServerResponse'),
    newClaimGameServerResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreateBuild
    CreateBuild (CreateBuild'),
    newCreateBuild,
    CreateBuildResponse (CreateBuildResponse'),
    newCreateBuildResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CreateFleetLocations
    CreateFleetLocations (CreateFleetLocations'),
    newCreateFleetLocations,
    CreateFleetLocationsResponse (CreateFleetLocationsResponse'),
    newCreateFleetLocationsResponse,

    -- ** CreateGameServerGroup
    CreateGameServerGroup (CreateGameServerGroup'),
    newCreateGameServerGroup,
    CreateGameServerGroupResponse (CreateGameServerGroupResponse'),
    newCreateGameServerGroupResponse,

    -- ** CreateGameSession
    CreateGameSession (CreateGameSession'),
    newCreateGameSession,
    CreateGameSessionResponse (CreateGameSessionResponse'),
    newCreateGameSessionResponse,

    -- ** CreateGameSessionQueue
    CreateGameSessionQueue (CreateGameSessionQueue'),
    newCreateGameSessionQueue,
    CreateGameSessionQueueResponse (CreateGameSessionQueueResponse'),
    newCreateGameSessionQueueResponse,

    -- ** CreateLocation
    CreateLocation (CreateLocation'),
    newCreateLocation,
    CreateLocationResponse (CreateLocationResponse'),
    newCreateLocationResponse,

    -- ** CreateMatchmakingConfiguration
    CreateMatchmakingConfiguration (CreateMatchmakingConfiguration'),
    newCreateMatchmakingConfiguration,
    CreateMatchmakingConfigurationResponse (CreateMatchmakingConfigurationResponse'),
    newCreateMatchmakingConfigurationResponse,

    -- ** CreateMatchmakingRuleSet
    CreateMatchmakingRuleSet (CreateMatchmakingRuleSet'),
    newCreateMatchmakingRuleSet,
    CreateMatchmakingRuleSetResponse (CreateMatchmakingRuleSetResponse'),
    newCreateMatchmakingRuleSetResponse,

    -- ** CreatePlayerSession
    CreatePlayerSession (CreatePlayerSession'),
    newCreatePlayerSession,
    CreatePlayerSessionResponse (CreatePlayerSessionResponse'),
    newCreatePlayerSessionResponse,

    -- ** CreatePlayerSessions
    CreatePlayerSessions (CreatePlayerSessions'),
    newCreatePlayerSessions,
    CreatePlayerSessionsResponse (CreatePlayerSessionsResponse'),
    newCreatePlayerSessionsResponse,

    -- ** CreateScript
    CreateScript (CreateScript'),
    newCreateScript,
    CreateScriptResponse (CreateScriptResponse'),
    newCreateScriptResponse,

    -- ** CreateVpcPeeringAuthorization
    CreateVpcPeeringAuthorization (CreateVpcPeeringAuthorization'),
    newCreateVpcPeeringAuthorization,
    CreateVpcPeeringAuthorizationResponse (CreateVpcPeeringAuthorizationResponse'),
    newCreateVpcPeeringAuthorizationResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** DeleteBuild
    DeleteBuild (DeleteBuild'),
    newDeleteBuild,
    DeleteBuildResponse (DeleteBuildResponse'),
    newDeleteBuildResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** DeleteFleetLocations
    DeleteFleetLocations (DeleteFleetLocations'),
    newDeleteFleetLocations,
    DeleteFleetLocationsResponse (DeleteFleetLocationsResponse'),
    newDeleteFleetLocationsResponse,

    -- ** DeleteGameServerGroup
    DeleteGameServerGroup (DeleteGameServerGroup'),
    newDeleteGameServerGroup,
    DeleteGameServerGroupResponse (DeleteGameServerGroupResponse'),
    newDeleteGameServerGroupResponse,

    -- ** DeleteGameSessionQueue
    DeleteGameSessionQueue (DeleteGameSessionQueue'),
    newDeleteGameSessionQueue,
    DeleteGameSessionQueueResponse (DeleteGameSessionQueueResponse'),
    newDeleteGameSessionQueueResponse,

    -- ** DeleteLocation
    DeleteLocation (DeleteLocation'),
    newDeleteLocation,
    DeleteLocationResponse (DeleteLocationResponse'),
    newDeleteLocationResponse,

    -- ** DeleteMatchmakingConfiguration
    DeleteMatchmakingConfiguration (DeleteMatchmakingConfiguration'),
    newDeleteMatchmakingConfiguration,
    DeleteMatchmakingConfigurationResponse (DeleteMatchmakingConfigurationResponse'),
    newDeleteMatchmakingConfigurationResponse,

    -- ** DeleteMatchmakingRuleSet
    DeleteMatchmakingRuleSet (DeleteMatchmakingRuleSet'),
    newDeleteMatchmakingRuleSet,
    DeleteMatchmakingRuleSetResponse (DeleteMatchmakingRuleSetResponse'),
    newDeleteMatchmakingRuleSetResponse,

    -- ** DeleteScalingPolicy
    DeleteScalingPolicy (DeleteScalingPolicy'),
    newDeleteScalingPolicy,
    DeleteScalingPolicyResponse (DeleteScalingPolicyResponse'),
    newDeleteScalingPolicyResponse,

    -- ** DeleteScript
    DeleteScript (DeleteScript'),
    newDeleteScript,
    DeleteScriptResponse (DeleteScriptResponse'),
    newDeleteScriptResponse,

    -- ** DeleteVpcPeeringAuthorization
    DeleteVpcPeeringAuthorization (DeleteVpcPeeringAuthorization'),
    newDeleteVpcPeeringAuthorization,
    DeleteVpcPeeringAuthorizationResponse (DeleteVpcPeeringAuthorizationResponse'),
    newDeleteVpcPeeringAuthorizationResponse,

    -- ** DeleteVpcPeeringConnection
    DeleteVpcPeeringConnection (DeleteVpcPeeringConnection'),
    newDeleteVpcPeeringConnection,
    DeleteVpcPeeringConnectionResponse (DeleteVpcPeeringConnectionResponse'),
    newDeleteVpcPeeringConnectionResponse,

    -- ** DeregisterCompute
    DeregisterCompute (DeregisterCompute'),
    newDeregisterCompute,
    DeregisterComputeResponse (DeregisterComputeResponse'),
    newDeregisterComputeResponse,

    -- ** DeregisterGameServer
    DeregisterGameServer (DeregisterGameServer'),
    newDeregisterGameServer,
    DeregisterGameServerResponse (DeregisterGameServerResponse'),
    newDeregisterGameServerResponse,

    -- ** DescribeAlias
    DescribeAlias (DescribeAlias'),
    newDescribeAlias,
    DescribeAliasResponse (DescribeAliasResponse'),
    newDescribeAliasResponse,

    -- ** DescribeBuild
    DescribeBuild (DescribeBuild'),
    newDescribeBuild,
    DescribeBuildResponse (DescribeBuildResponse'),
    newDescribeBuildResponse,

    -- ** DescribeCompute
    DescribeCompute (DescribeCompute'),
    newDescribeCompute,
    DescribeComputeResponse (DescribeComputeResponse'),
    newDescribeComputeResponse,

    -- ** DescribeEC2InstanceLimits
    DescribeEC2InstanceLimits (DescribeEC2InstanceLimits'),
    newDescribeEC2InstanceLimits,
    DescribeEC2InstanceLimitsResponse (DescribeEC2InstanceLimitsResponse'),
    newDescribeEC2InstanceLimitsResponse,

    -- ** DescribeFleetAttributes (Paginated)
    DescribeFleetAttributes (DescribeFleetAttributes'),
    newDescribeFleetAttributes,
    DescribeFleetAttributesResponse (DescribeFleetAttributesResponse'),
    newDescribeFleetAttributesResponse,

    -- ** DescribeFleetCapacity (Paginated)
    DescribeFleetCapacity (DescribeFleetCapacity'),
    newDescribeFleetCapacity,
    DescribeFleetCapacityResponse (DescribeFleetCapacityResponse'),
    newDescribeFleetCapacityResponse,

    -- ** DescribeFleetEvents (Paginated)
    DescribeFleetEvents (DescribeFleetEvents'),
    newDescribeFleetEvents,
    DescribeFleetEventsResponse (DescribeFleetEventsResponse'),
    newDescribeFleetEventsResponse,

    -- ** DescribeFleetLocationAttributes
    DescribeFleetLocationAttributes (DescribeFleetLocationAttributes'),
    newDescribeFleetLocationAttributes,
    DescribeFleetLocationAttributesResponse (DescribeFleetLocationAttributesResponse'),
    newDescribeFleetLocationAttributesResponse,

    -- ** DescribeFleetLocationCapacity
    DescribeFleetLocationCapacity (DescribeFleetLocationCapacity'),
    newDescribeFleetLocationCapacity,
    DescribeFleetLocationCapacityResponse (DescribeFleetLocationCapacityResponse'),
    newDescribeFleetLocationCapacityResponse,

    -- ** DescribeFleetLocationUtilization
    DescribeFleetLocationUtilization (DescribeFleetLocationUtilization'),
    newDescribeFleetLocationUtilization,
    DescribeFleetLocationUtilizationResponse (DescribeFleetLocationUtilizationResponse'),
    newDescribeFleetLocationUtilizationResponse,

    -- ** DescribeFleetPortSettings
    DescribeFleetPortSettings (DescribeFleetPortSettings'),
    newDescribeFleetPortSettings,
    DescribeFleetPortSettingsResponse (DescribeFleetPortSettingsResponse'),
    newDescribeFleetPortSettingsResponse,

    -- ** DescribeFleetUtilization (Paginated)
    DescribeFleetUtilization (DescribeFleetUtilization'),
    newDescribeFleetUtilization,
    DescribeFleetUtilizationResponse (DescribeFleetUtilizationResponse'),
    newDescribeFleetUtilizationResponse,

    -- ** DescribeGameServer
    DescribeGameServer (DescribeGameServer'),
    newDescribeGameServer,
    DescribeGameServerResponse (DescribeGameServerResponse'),
    newDescribeGameServerResponse,

    -- ** DescribeGameServerGroup
    DescribeGameServerGroup (DescribeGameServerGroup'),
    newDescribeGameServerGroup,
    DescribeGameServerGroupResponse (DescribeGameServerGroupResponse'),
    newDescribeGameServerGroupResponse,

    -- ** DescribeGameServerInstances (Paginated)
    DescribeGameServerInstances (DescribeGameServerInstances'),
    newDescribeGameServerInstances,
    DescribeGameServerInstancesResponse (DescribeGameServerInstancesResponse'),
    newDescribeGameServerInstancesResponse,

    -- ** DescribeGameSessionDetails (Paginated)
    DescribeGameSessionDetails (DescribeGameSessionDetails'),
    newDescribeGameSessionDetails,
    DescribeGameSessionDetailsResponse (DescribeGameSessionDetailsResponse'),
    newDescribeGameSessionDetailsResponse,

    -- ** DescribeGameSessionPlacement
    DescribeGameSessionPlacement (DescribeGameSessionPlacement'),
    newDescribeGameSessionPlacement,
    DescribeGameSessionPlacementResponse (DescribeGameSessionPlacementResponse'),
    newDescribeGameSessionPlacementResponse,

    -- ** DescribeGameSessionQueues (Paginated)
    DescribeGameSessionQueues (DescribeGameSessionQueues'),
    newDescribeGameSessionQueues,
    DescribeGameSessionQueuesResponse (DescribeGameSessionQueuesResponse'),
    newDescribeGameSessionQueuesResponse,

    -- ** DescribeGameSessions (Paginated)
    DescribeGameSessions (DescribeGameSessions'),
    newDescribeGameSessions,
    DescribeGameSessionsResponse (DescribeGameSessionsResponse'),
    newDescribeGameSessionsResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DescribeMatchmaking
    DescribeMatchmaking (DescribeMatchmaking'),
    newDescribeMatchmaking,
    DescribeMatchmakingResponse (DescribeMatchmakingResponse'),
    newDescribeMatchmakingResponse,

    -- ** DescribeMatchmakingConfigurations (Paginated)
    DescribeMatchmakingConfigurations (DescribeMatchmakingConfigurations'),
    newDescribeMatchmakingConfigurations,
    DescribeMatchmakingConfigurationsResponse (DescribeMatchmakingConfigurationsResponse'),
    newDescribeMatchmakingConfigurationsResponse,

    -- ** DescribeMatchmakingRuleSets (Paginated)
    DescribeMatchmakingRuleSets (DescribeMatchmakingRuleSets'),
    newDescribeMatchmakingRuleSets,
    DescribeMatchmakingRuleSetsResponse (DescribeMatchmakingRuleSetsResponse'),
    newDescribeMatchmakingRuleSetsResponse,

    -- ** DescribePlayerSessions (Paginated)
    DescribePlayerSessions (DescribePlayerSessions'),
    newDescribePlayerSessions,
    DescribePlayerSessionsResponse (DescribePlayerSessionsResponse'),
    newDescribePlayerSessionsResponse,

    -- ** DescribeRuntimeConfiguration
    DescribeRuntimeConfiguration (DescribeRuntimeConfiguration'),
    newDescribeRuntimeConfiguration,
    DescribeRuntimeConfigurationResponse (DescribeRuntimeConfigurationResponse'),
    newDescribeRuntimeConfigurationResponse,

    -- ** DescribeScalingPolicies (Paginated)
    DescribeScalingPolicies (DescribeScalingPolicies'),
    newDescribeScalingPolicies,
    DescribeScalingPoliciesResponse (DescribeScalingPoliciesResponse'),
    newDescribeScalingPoliciesResponse,

    -- ** DescribeScript
    DescribeScript (DescribeScript'),
    newDescribeScript,
    DescribeScriptResponse (DescribeScriptResponse'),
    newDescribeScriptResponse,

    -- ** DescribeVpcPeeringAuthorizations
    DescribeVpcPeeringAuthorizations (DescribeVpcPeeringAuthorizations'),
    newDescribeVpcPeeringAuthorizations,
    DescribeVpcPeeringAuthorizationsResponse (DescribeVpcPeeringAuthorizationsResponse'),
    newDescribeVpcPeeringAuthorizationsResponse,

    -- ** DescribeVpcPeeringConnections
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** GetComputeAccess
    GetComputeAccess (GetComputeAccess'),
    newGetComputeAccess,
    GetComputeAccessResponse (GetComputeAccessResponse'),
    newGetComputeAccessResponse,

    -- ** GetComputeAuthToken
    GetComputeAuthToken (GetComputeAuthToken'),
    newGetComputeAuthToken,
    GetComputeAuthTokenResponse (GetComputeAuthTokenResponse'),
    newGetComputeAuthTokenResponse,

    -- ** GetGameSessionLogUrl
    GetGameSessionLogUrl (GetGameSessionLogUrl'),
    newGetGameSessionLogUrl,
    GetGameSessionLogUrlResponse (GetGameSessionLogUrlResponse'),
    newGetGameSessionLogUrlResponse,

    -- ** GetInstanceAccess
    GetInstanceAccess (GetInstanceAccess'),
    newGetInstanceAccess,
    GetInstanceAccessResponse (GetInstanceAccessResponse'),
    newGetInstanceAccessResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** ListBuilds (Paginated)
    ListBuilds (ListBuilds'),
    newListBuilds,
    ListBuildsResponse (ListBuildsResponse'),
    newListBuildsResponse,

    -- ** ListCompute (Paginated)
    ListCompute (ListCompute'),
    newListCompute,
    ListComputeResponse (ListComputeResponse'),
    newListComputeResponse,

    -- ** ListFleets (Paginated)
    ListFleets (ListFleets'),
    newListFleets,
    ListFleetsResponse (ListFleetsResponse'),
    newListFleetsResponse,

    -- ** ListGameServerGroups (Paginated)
    ListGameServerGroups (ListGameServerGroups'),
    newListGameServerGroups,
    ListGameServerGroupsResponse (ListGameServerGroupsResponse'),
    newListGameServerGroupsResponse,

    -- ** ListGameServers (Paginated)
    ListGameServers (ListGameServers'),
    newListGameServers,
    ListGameServersResponse (ListGameServersResponse'),
    newListGameServersResponse,

    -- ** ListLocations (Paginated)
    ListLocations (ListLocations'),
    newListLocations,
    ListLocationsResponse (ListLocationsResponse'),
    newListLocationsResponse,

    -- ** ListScripts (Paginated)
    ListScripts (ListScripts'),
    newListScripts,
    ListScriptsResponse (ListScriptsResponse'),
    newListScriptsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutScalingPolicy
    PutScalingPolicy (PutScalingPolicy'),
    newPutScalingPolicy,
    PutScalingPolicyResponse (PutScalingPolicyResponse'),
    newPutScalingPolicyResponse,

    -- ** RegisterCompute
    RegisterCompute (RegisterCompute'),
    newRegisterCompute,
    RegisterComputeResponse (RegisterComputeResponse'),
    newRegisterComputeResponse,

    -- ** RegisterGameServer
    RegisterGameServer (RegisterGameServer'),
    newRegisterGameServer,
    RegisterGameServerResponse (RegisterGameServerResponse'),
    newRegisterGameServerResponse,

    -- ** RequestUploadCredentials
    RequestUploadCredentials (RequestUploadCredentials'),
    newRequestUploadCredentials,
    RequestUploadCredentialsResponse (RequestUploadCredentialsResponse'),
    newRequestUploadCredentialsResponse,

    -- ** ResolveAlias
    ResolveAlias (ResolveAlias'),
    newResolveAlias,
    ResolveAliasResponse (ResolveAliasResponse'),
    newResolveAliasResponse,

    -- ** ResumeGameServerGroup
    ResumeGameServerGroup (ResumeGameServerGroup'),
    newResumeGameServerGroup,
    ResumeGameServerGroupResponse (ResumeGameServerGroupResponse'),
    newResumeGameServerGroupResponse,

    -- ** SearchGameSessions (Paginated)
    SearchGameSessions (SearchGameSessions'),
    newSearchGameSessions,
    SearchGameSessionsResponse (SearchGameSessionsResponse'),
    newSearchGameSessionsResponse,

    -- ** StartFleetActions
    StartFleetActions (StartFleetActions'),
    newStartFleetActions,
    StartFleetActionsResponse (StartFleetActionsResponse'),
    newStartFleetActionsResponse,

    -- ** StartGameSessionPlacement
    StartGameSessionPlacement (StartGameSessionPlacement'),
    newStartGameSessionPlacement,
    StartGameSessionPlacementResponse (StartGameSessionPlacementResponse'),
    newStartGameSessionPlacementResponse,

    -- ** StartMatchBackfill
    StartMatchBackfill (StartMatchBackfill'),
    newStartMatchBackfill,
    StartMatchBackfillResponse (StartMatchBackfillResponse'),
    newStartMatchBackfillResponse,

    -- ** StartMatchmaking
    StartMatchmaking (StartMatchmaking'),
    newStartMatchmaking,
    StartMatchmakingResponse (StartMatchmakingResponse'),
    newStartMatchmakingResponse,

    -- ** StopFleetActions
    StopFleetActions (StopFleetActions'),
    newStopFleetActions,
    StopFleetActionsResponse (StopFleetActionsResponse'),
    newStopFleetActionsResponse,

    -- ** StopGameSessionPlacement
    StopGameSessionPlacement (StopGameSessionPlacement'),
    newStopGameSessionPlacement,
    StopGameSessionPlacementResponse (StopGameSessionPlacementResponse'),
    newStopGameSessionPlacementResponse,

    -- ** StopMatchmaking
    StopMatchmaking (StopMatchmaking'),
    newStopMatchmaking,
    StopMatchmakingResponse (StopMatchmakingResponse'),
    newStopMatchmakingResponse,

    -- ** SuspendGameServerGroup
    SuspendGameServerGroup (SuspendGameServerGroup'),
    newSuspendGameServerGroup,
    SuspendGameServerGroupResponse (SuspendGameServerGroupResponse'),
    newSuspendGameServerGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAlias
    UpdateAlias (UpdateAlias'),
    newUpdateAlias,
    UpdateAliasResponse (UpdateAliasResponse'),
    newUpdateAliasResponse,

    -- ** UpdateBuild
    UpdateBuild (UpdateBuild'),
    newUpdateBuild,
    UpdateBuildResponse (UpdateBuildResponse'),
    newUpdateBuildResponse,

    -- ** UpdateFleetAttributes
    UpdateFleetAttributes (UpdateFleetAttributes'),
    newUpdateFleetAttributes,
    UpdateFleetAttributesResponse (UpdateFleetAttributesResponse'),
    newUpdateFleetAttributesResponse,

    -- ** UpdateFleetCapacity
    UpdateFleetCapacity (UpdateFleetCapacity'),
    newUpdateFleetCapacity,
    UpdateFleetCapacityResponse (UpdateFleetCapacityResponse'),
    newUpdateFleetCapacityResponse,

    -- ** UpdateFleetPortSettings
    UpdateFleetPortSettings (UpdateFleetPortSettings'),
    newUpdateFleetPortSettings,
    UpdateFleetPortSettingsResponse (UpdateFleetPortSettingsResponse'),
    newUpdateFleetPortSettingsResponse,

    -- ** UpdateGameServer
    UpdateGameServer (UpdateGameServer'),
    newUpdateGameServer,
    UpdateGameServerResponse (UpdateGameServerResponse'),
    newUpdateGameServerResponse,

    -- ** UpdateGameServerGroup
    UpdateGameServerGroup (UpdateGameServerGroup'),
    newUpdateGameServerGroup,
    UpdateGameServerGroupResponse (UpdateGameServerGroupResponse'),
    newUpdateGameServerGroupResponse,

    -- ** UpdateGameSession
    UpdateGameSession (UpdateGameSession'),
    newUpdateGameSession,
    UpdateGameSessionResponse (UpdateGameSessionResponse'),
    newUpdateGameSessionResponse,

    -- ** UpdateGameSessionQueue
    UpdateGameSessionQueue (UpdateGameSessionQueue'),
    newUpdateGameSessionQueue,
    UpdateGameSessionQueueResponse (UpdateGameSessionQueueResponse'),
    newUpdateGameSessionQueueResponse,

    -- ** UpdateMatchmakingConfiguration
    UpdateMatchmakingConfiguration (UpdateMatchmakingConfiguration'),
    newUpdateMatchmakingConfiguration,
    UpdateMatchmakingConfigurationResponse (UpdateMatchmakingConfigurationResponse'),
    newUpdateMatchmakingConfigurationResponse,

    -- ** UpdateRuntimeConfiguration
    UpdateRuntimeConfiguration (UpdateRuntimeConfiguration'),
    newUpdateRuntimeConfiguration,
    UpdateRuntimeConfigurationResponse (UpdateRuntimeConfigurationResponse'),
    newUpdateRuntimeConfigurationResponse,

    -- ** UpdateScript
    UpdateScript (UpdateScript'),
    newUpdateScript,
    UpdateScriptResponse (UpdateScriptResponse'),
    newUpdateScriptResponse,

    -- ** ValidateMatchmakingRuleSet
    ValidateMatchmakingRuleSet (ValidateMatchmakingRuleSet'),
    newValidateMatchmakingRuleSet,
    ValidateMatchmakingRuleSetResponse (ValidateMatchmakingRuleSetResponse'),
    newValidateMatchmakingRuleSetResponse,

    -- * Types

    -- ** AcceptanceType
    AcceptanceType (..),

    -- ** BackfillMode
    BackfillMode (..),

    -- ** BalancingStrategy
    BalancingStrategy (..),

    -- ** BuildStatus
    BuildStatus (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** ComparisonOperatorType
    ComparisonOperatorType (..),

    -- ** ComputeStatus
    ComputeStatus (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** EC2InstanceType
    EC2InstanceType (..),

    -- ** EventCode
    EventCode (..),

    -- ** FleetAction
    FleetAction (..),

    -- ** FleetStatus
    FleetStatus (..),

    -- ** FleetType
    FleetType (..),

    -- ** FlexMatchMode
    FlexMatchMode (..),

    -- ** GameServerClaimStatus
    GameServerClaimStatus (..),

    -- ** GameServerGroupAction
    GameServerGroupAction (..),

    -- ** GameServerGroupDeleteOption
    GameServerGroupDeleteOption (..),

    -- ** GameServerGroupInstanceType
    GameServerGroupInstanceType (..),

    -- ** GameServerGroupStatus
    GameServerGroupStatus (..),

    -- ** GameServerHealthCheck
    GameServerHealthCheck (..),

    -- ** GameServerInstanceStatus
    GameServerInstanceStatus (..),

    -- ** GameServerProtectionPolicy
    GameServerProtectionPolicy (..),

    -- ** GameServerUtilizationStatus
    GameServerUtilizationStatus (..),

    -- ** GameSessionPlacementState
    GameSessionPlacementState (..),

    -- ** GameSessionStatus
    GameSessionStatus (..),

    -- ** GameSessionStatusReason
    GameSessionStatusReason (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** IpProtocol
    IpProtocol (..),

    -- ** LocationFilter
    LocationFilter (..),

    -- ** LocationUpdateStatus
    LocationUpdateStatus (..),

    -- ** MatchmakingConfigurationStatus
    MatchmakingConfigurationStatus (..),

    -- ** MetricName
    MetricName (..),

    -- ** OperatingSystem
    OperatingSystem (..),

    -- ** PlayerSessionCreationPolicy
    PlayerSessionCreationPolicy (..),

    -- ** PlayerSessionStatus
    PlayerSessionStatus (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** PriorityType
    PriorityType (..),

    -- ** ProtectionPolicy
    ProtectionPolicy (..),

    -- ** RoutingStrategyType
    RoutingStrategyType (..),

    -- ** ScalingAdjustmentType
    ScalingAdjustmentType (..),

    -- ** ScalingStatusType
    ScalingStatusType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** Alias
    Alias (Alias'),
    newAlias,

    -- ** AnywhereConfiguration
    AnywhereConfiguration (AnywhereConfiguration'),
    newAnywhereConfiguration,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** AwsCredentials
    AwsCredentials (AwsCredentials'),
    newAwsCredentials,

    -- ** Build
    Build (Build'),
    newBuild,

    -- ** CertificateConfiguration
    CertificateConfiguration (CertificateConfiguration'),
    newCertificateConfiguration,

    -- ** Compute
    Compute (Compute'),
    newCompute,

    -- ** DesiredPlayerSession
    DesiredPlayerSession (DesiredPlayerSession'),
    newDesiredPlayerSession,

    -- ** EC2InstanceCounts
    EC2InstanceCounts (EC2InstanceCounts'),
    newEC2InstanceCounts,

    -- ** EC2InstanceLimit
    EC2InstanceLimit (EC2InstanceLimit'),
    newEC2InstanceLimit,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** FilterConfiguration
    FilterConfiguration (FilterConfiguration'),
    newFilterConfiguration,

    -- ** FleetAttributes
    FleetAttributes (FleetAttributes'),
    newFleetAttributes,

    -- ** FleetCapacity
    FleetCapacity (FleetCapacity'),
    newFleetCapacity,

    -- ** FleetUtilization
    FleetUtilization (FleetUtilization'),
    newFleetUtilization,

    -- ** GameProperty
    GameProperty (GameProperty'),
    newGameProperty,

    -- ** GameServer
    GameServer (GameServer'),
    newGameServer,

    -- ** GameServerGroup
    GameServerGroup (GameServerGroup'),
    newGameServerGroup,

    -- ** GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy (GameServerGroupAutoScalingPolicy'),
    newGameServerGroupAutoScalingPolicy,

    -- ** GameServerInstance
    GameServerInstance (GameServerInstance'),
    newGameServerInstance,

    -- ** GameSession
    GameSession (GameSession'),
    newGameSession,

    -- ** GameSessionConnectionInfo
    GameSessionConnectionInfo (GameSessionConnectionInfo'),
    newGameSessionConnectionInfo,

    -- ** GameSessionDetail
    GameSessionDetail (GameSessionDetail'),
    newGameSessionDetail,

    -- ** GameSessionPlacement
    GameSessionPlacement (GameSessionPlacement'),
    newGameSessionPlacement,

    -- ** GameSessionQueue
    GameSessionQueue (GameSessionQueue'),
    newGameSessionQueue,

    -- ** GameSessionQueueDestination
    GameSessionQueueDestination (GameSessionQueueDestination'),
    newGameSessionQueueDestination,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceAccess
    InstanceAccess (InstanceAccess'),
    newInstanceAccess,

    -- ** InstanceCredentials
    InstanceCredentials (InstanceCredentials'),
    newInstanceCredentials,

    -- ** InstanceDefinition
    InstanceDefinition (InstanceDefinition'),
    newInstanceDefinition,

    -- ** IpPermission
    IpPermission (IpPermission'),
    newIpPermission,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (LaunchTemplateSpecification'),
    newLaunchTemplateSpecification,

    -- ** LocationAttributes
    LocationAttributes (LocationAttributes'),
    newLocationAttributes,

    -- ** LocationConfiguration
    LocationConfiguration (LocationConfiguration'),
    newLocationConfiguration,

    -- ** LocationModel
    LocationModel (LocationModel'),
    newLocationModel,

    -- ** LocationState
    LocationState (LocationState'),
    newLocationState,

    -- ** MatchedPlayerSession
    MatchedPlayerSession (MatchedPlayerSession'),
    newMatchedPlayerSession,

    -- ** MatchmakingConfiguration
    MatchmakingConfiguration (MatchmakingConfiguration'),
    newMatchmakingConfiguration,

    -- ** MatchmakingRuleSet
    MatchmakingRuleSet (MatchmakingRuleSet'),
    newMatchmakingRuleSet,

    -- ** MatchmakingTicket
    MatchmakingTicket (MatchmakingTicket'),
    newMatchmakingTicket,

    -- ** PlacedPlayerSession
    PlacedPlayerSession (PlacedPlayerSession'),
    newPlacedPlayerSession,

    -- ** Player
    Player (Player'),
    newPlayer,

    -- ** PlayerLatency
    PlayerLatency (PlayerLatency'),
    newPlayerLatency,

    -- ** PlayerLatencyPolicy
    PlayerLatencyPolicy (PlayerLatencyPolicy'),
    newPlayerLatencyPolicy,

    -- ** PlayerSession
    PlayerSession (PlayerSession'),
    newPlayerSession,

    -- ** PriorityConfiguration
    PriorityConfiguration (PriorityConfiguration'),
    newPriorityConfiguration,

    -- ** ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy (ResourceCreationLimitPolicy'),
    newResourceCreationLimitPolicy,

    -- ** RoutingStrategy
    RoutingStrategy (RoutingStrategy'),
    newRoutingStrategy,

    -- ** RuntimeConfiguration
    RuntimeConfiguration (RuntimeConfiguration'),
    newRuntimeConfiguration,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** ScalingPolicy
    ScalingPolicy (ScalingPolicy'),
    newScalingPolicy,

    -- ** Script
    Script (Script'),
    newScript,

    -- ** ServerProcess
    ServerProcess (ServerProcess'),
    newServerProcess,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TargetConfiguration
    TargetConfiguration (TargetConfiguration'),
    newTargetConfiguration,

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (TargetTrackingConfiguration'),
    newTargetTrackingConfiguration,

    -- ** VpcPeeringAuthorization
    VpcPeeringAuthorization (VpcPeeringAuthorization'),
    newVpcPeeringAuthorization,

    -- ** VpcPeeringConnection
    VpcPeeringConnection (VpcPeeringConnection'),
    newVpcPeeringConnection,

    -- ** VpcPeeringConnectionStatus
    VpcPeeringConnectionStatus (VpcPeeringConnectionStatus'),
    newVpcPeeringConnectionStatus,
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
import Amazonka.GameLift.Lens
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
import Amazonka.GameLift.Types
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
import Amazonka.GameLift.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GameLift'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
