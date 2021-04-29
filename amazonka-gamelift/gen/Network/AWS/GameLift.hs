{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon GameLift Service
--
-- GameLift provides solutions for hosting session-based multiplayer game
-- servers in the cloud, including tools for deploying, operating, and
-- scaling game servers. Built on AWS global computing infrastructure,
-- GameLift helps you deliver high-performance, high-reliability, low-cost
-- game servers while dynamically scaling your resource usage to meet
-- player demand.
--
-- __About GameLift solutions__
--
-- Get more information on these GameLift solutions in the
-- <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide>.
--
-- -   Managed GameLift -- GameLift offers a fully managed service to set
--     up and maintain computing machines for hosting, manage game session
--     and player session life cycle, and handle security, storage, and
--     performance tracking. You can use automatic scaling tools to balance
--     hosting costs against meeting player demand., configure your game
--     session management to minimize player latency, or add FlexMatch for
--     matchmaking.
--
-- -   Managed GameLift with Realtime Servers – With GameLift Realtime
--     Servers, you can quickly configure and set up game servers for your
--     game. Realtime Servers provides a game server framework with core
--     Amazon GameLift infrastructure already built in.
--
-- -   GameLift FleetIQ – Use GameLift FleetIQ as a standalone feature
--     while managing your own EC2 instances and Auto Scaling groups for
--     game hosting. GameLift FleetIQ provides optimizations that make
--     low-cost Spot Instances viable for game hosting.
--
-- __About this API Reference__
--
-- This reference guide describes the low-level service API for Amazon
-- GameLift. You can find links to language-specific SDK guides and the AWS
-- CLI reference with each operation and data type topic. Useful links:
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html GameLift API operations listed by tasks>
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-components.html GameLift tools and resources>
module Network.AWS.GameLift
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** OutOfCapacityException
    _OutOfCapacityException,

    -- ** TaggingFailedException
    _TaggingFailedException,

    -- ** FleetCapacityExceededException
    _FleetCapacityExceededException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidGameSessionStatusException
    _InvalidGameSessionStatusException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** TerminalRoutingStrategyException
    _TerminalRoutingStrategyException,

    -- ** GameSessionFullException
    _GameSessionFullException,

    -- ** InvalidFleetStatusException
    _InvalidFleetStatusException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** UnsupportedRegionException
    _UnsupportedRegionException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeGameSessionQueues (Paginated)
    DescribeGameSessionQueues (DescribeGameSessionQueues'),
    newDescribeGameSessionQueues,
    DescribeGameSessionQueuesResponse (DescribeGameSessionQueuesResponse'),
    newDescribeGameSessionQueuesResponse,

    -- ** DeleteBuild
    DeleteBuild (DeleteBuild'),
    newDeleteBuild,
    DeleteBuildResponse (DeleteBuildResponse'),
    newDeleteBuildResponse,

    -- ** DescribeVpcPeeringConnections
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** DescribeFleetPortSettings
    DescribeFleetPortSettings (DescribeFleetPortSettings'),
    newDescribeFleetPortSettings,
    DescribeFleetPortSettingsResponse (DescribeFleetPortSettingsResponse'),
    newDescribeFleetPortSettingsResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** DescribeFleetCapacity (Paginated)
    DescribeFleetCapacity (DescribeFleetCapacity'),
    newDescribeFleetCapacity,
    DescribeFleetCapacityResponse (DescribeFleetCapacityResponse'),
    newDescribeFleetCapacityResponse,

    -- ** ListBuilds (Paginated)
    ListBuilds (ListBuilds'),
    newListBuilds,
    ListBuildsResponse (ListBuildsResponse'),
    newListBuildsResponse,

    -- ** UpdateBuild
    UpdateBuild (UpdateBuild'),
    newUpdateBuild,
    UpdateBuildResponse (UpdateBuildResponse'),
    newUpdateBuildResponse,

    -- ** UpdateAlias
    UpdateAlias (UpdateAlias'),
    newUpdateAlias,
    UpdateAliasResponse (UpdateAliasResponse'),
    newUpdateAliasResponse,

    -- ** DescribeFleetAttributes (Paginated)
    DescribeFleetAttributes (DescribeFleetAttributes'),
    newDescribeFleetAttributes,
    DescribeFleetAttributesResponse (DescribeFleetAttributesResponse'),
    newDescribeFleetAttributesResponse,

    -- ** PutScalingPolicy
    PutScalingPolicy (PutScalingPolicy'),
    newPutScalingPolicy,
    PutScalingPolicyResponse (PutScalingPolicyResponse'),
    newPutScalingPolicyResponse,

    -- ** DescribeFleetEvents (Paginated)
    DescribeFleetEvents (DescribeFleetEvents'),
    newDescribeFleetEvents,
    DescribeFleetEventsResponse (DescribeFleetEventsResponse'),
    newDescribeFleetEventsResponse,

    -- ** DescribeFleetUtilization (Paginated)
    DescribeFleetUtilization (DescribeFleetUtilization'),
    newDescribeFleetUtilization,
    DescribeFleetUtilizationResponse (DescribeFleetUtilizationResponse'),
    newDescribeFleetUtilizationResponse,

    -- ** ClaimGameServer
    ClaimGameServer (ClaimGameServer'),
    newClaimGameServer,
    ClaimGameServerResponse (ClaimGameServerResponse'),
    newClaimGameServerResponse,

    -- ** UpdateGameSession
    UpdateGameSession (UpdateGameSession'),
    newUpdateGameSession,
    UpdateGameSessionResponse (UpdateGameSessionResponse'),
    newUpdateGameSessionResponse,

    -- ** DescribeGameServerGroup
    DescribeGameServerGroup (DescribeGameServerGroup'),
    newDescribeGameServerGroup,
    DescribeGameServerGroupResponse (DescribeGameServerGroupResponse'),
    newDescribeGameServerGroupResponse,

    -- ** DescribeMatchmaking
    DescribeMatchmaking (DescribeMatchmaking'),
    newDescribeMatchmaking,
    DescribeMatchmakingResponse (DescribeMatchmakingResponse'),
    newDescribeMatchmakingResponse,

    -- ** GetGameSessionLogUrl
    GetGameSessionLogUrl (GetGameSessionLogUrl'),
    newGetGameSessionLogUrl,
    GetGameSessionLogUrlResponse (GetGameSessionLogUrlResponse'),
    newGetGameSessionLogUrlResponse,

    -- ** CreatePlayerSession
    CreatePlayerSession (CreatePlayerSession'),
    newCreatePlayerSession,
    CreatePlayerSessionResponse (CreatePlayerSessionResponse'),
    newCreatePlayerSessionResponse,

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

    -- ** SuspendGameServerGroup
    SuspendGameServerGroup (SuspendGameServerGroup'),
    newSuspendGameServerGroup,
    SuspendGameServerGroupResponse (SuspendGameServerGroupResponse'),
    newSuspendGameServerGroupResponse,

    -- ** DescribeMatchmakingRuleSets (Paginated)
    DescribeMatchmakingRuleSets (DescribeMatchmakingRuleSets'),
    newDescribeMatchmakingRuleSets,
    DescribeMatchmakingRuleSetsResponse (DescribeMatchmakingRuleSetsResponse'),
    newDescribeMatchmakingRuleSetsResponse,

    -- ** ValidateMatchmakingRuleSet
    ValidateMatchmakingRuleSet (ValidateMatchmakingRuleSet'),
    newValidateMatchmakingRuleSet,
    ValidateMatchmakingRuleSetResponse (ValidateMatchmakingRuleSetResponse'),
    newValidateMatchmakingRuleSetResponse,

    -- ** UpdateFleetPortSettings
    UpdateFleetPortSettings (UpdateFleetPortSettings'),
    newUpdateFleetPortSettings,
    UpdateFleetPortSettingsResponse (UpdateFleetPortSettingsResponse'),
    newUpdateFleetPortSettingsResponse,

    -- ** DescribeBuild
    DescribeBuild (DescribeBuild'),
    newDescribeBuild,
    DescribeBuildResponse (DescribeBuildResponse'),
    newDescribeBuildResponse,

    -- ** AcceptMatch
    AcceptMatch (AcceptMatch'),
    newAcceptMatch,
    AcceptMatchResponse (AcceptMatchResponse'),
    newAcceptMatchResponse,

    -- ** DeregisterGameServer
    DeregisterGameServer (DeregisterGameServer'),
    newDeregisterGameServer,
    DeregisterGameServerResponse (DeregisterGameServerResponse'),
    newDeregisterGameServerResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateFleetCapacity
    UpdateFleetCapacity (UpdateFleetCapacity'),
    newUpdateFleetCapacity,
    UpdateFleetCapacityResponse (UpdateFleetCapacityResponse'),
    newUpdateFleetCapacityResponse,

    -- ** DescribeAlias
    DescribeAlias (DescribeAlias'),
    newDescribeAlias,
    DescribeAliasResponse (DescribeAliasResponse'),
    newDescribeAliasResponse,

    -- ** DeleteVpcPeeringConnection
    DeleteVpcPeeringConnection (DeleteVpcPeeringConnection'),
    newDeleteVpcPeeringConnection,
    DeleteVpcPeeringConnectionResponse (DeleteVpcPeeringConnectionResponse'),
    newDeleteVpcPeeringConnectionResponse,

    -- ** UpdateFleetAttributes
    UpdateFleetAttributes (UpdateFleetAttributes'),
    newUpdateFleetAttributes,
    UpdateFleetAttributesResponse (UpdateFleetAttributesResponse'),
    newUpdateFleetAttributesResponse,

    -- ** UpdateGameSessionQueue
    UpdateGameSessionQueue (UpdateGameSessionQueue'),
    newUpdateGameSessionQueue,
    UpdateGameSessionQueueResponse (UpdateGameSessionQueueResponse'),
    newUpdateGameSessionQueueResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteGameSessionQueue
    DeleteGameSessionQueue (DeleteGameSessionQueue'),
    newDeleteGameSessionQueue,
    DeleteGameSessionQueueResponse (DeleteGameSessionQueueResponse'),
    newDeleteGameSessionQueueResponse,

    -- ** UpdateMatchmakingConfiguration
    UpdateMatchmakingConfiguration (UpdateMatchmakingConfiguration'),
    newUpdateMatchmakingConfiguration,
    UpdateMatchmakingConfigurationResponse (UpdateMatchmakingConfigurationResponse'),
    newUpdateMatchmakingConfigurationResponse,

    -- ** DeleteMatchmakingConfiguration
    DeleteMatchmakingConfiguration (DeleteMatchmakingConfiguration'),
    newDeleteMatchmakingConfiguration,
    DeleteMatchmakingConfigurationResponse (DeleteMatchmakingConfigurationResponse'),
    newDeleteMatchmakingConfigurationResponse,

    -- ** DescribeMatchmakingConfigurations (Paginated)
    DescribeMatchmakingConfigurations (DescribeMatchmakingConfigurations'),
    newDescribeMatchmakingConfigurations,
    DescribeMatchmakingConfigurationsResponse (DescribeMatchmakingConfigurationsResponse'),
    newDescribeMatchmakingConfigurationsResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

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

    -- ** StartMatchBackfill
    StartMatchBackfill (StartMatchBackfill'),
    newStartMatchBackfill,
    StartMatchBackfillResponse (StartMatchBackfillResponse'),
    newStartMatchBackfillResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** DescribeGameSessionDetails (Paginated)
    DescribeGameSessionDetails (DescribeGameSessionDetails'),
    newDescribeGameSessionDetails,
    DescribeGameSessionDetailsResponse (DescribeGameSessionDetailsResponse'),
    newDescribeGameSessionDetailsResponse,

    -- ** ListFleets (Paginated)
    ListFleets (ListFleets'),
    newListFleets,
    ListFleetsResponse (ListFleetsResponse'),
    newListFleetsResponse,

    -- ** StopMatchmaking
    StopMatchmaking (StopMatchmaking'),
    newStopMatchmaking,
    StopMatchmakingResponse (StopMatchmakingResponse'),
    newStopMatchmakingResponse,

    -- ** DescribeGameServerInstances (Paginated)
    DescribeGameServerInstances (DescribeGameServerInstances'),
    newDescribeGameServerInstances,
    DescribeGameServerInstancesResponse (DescribeGameServerInstancesResponse'),
    newDescribeGameServerInstancesResponse,

    -- ** CreateGameSession
    CreateGameSession (CreateGameSession'),
    newCreateGameSession,
    CreateGameSessionResponse (CreateGameSessionResponse'),
    newCreateGameSessionResponse,

    -- ** CreateMatchmakingRuleSet
    CreateMatchmakingRuleSet (CreateMatchmakingRuleSet'),
    newCreateMatchmakingRuleSet,
    CreateMatchmakingRuleSetResponse (CreateMatchmakingRuleSetResponse'),
    newCreateMatchmakingRuleSetResponse,

    -- ** StartMatchmaking
    StartMatchmaking (StartMatchmaking'),
    newStartMatchmaking,
    StartMatchmakingResponse (StartMatchmakingResponse'),
    newStartMatchmakingResponse,

    -- ** DescribeGameSessionPlacement
    DescribeGameSessionPlacement (DescribeGameSessionPlacement'),
    newDescribeGameSessionPlacement,
    DescribeGameSessionPlacementResponse (DescribeGameSessionPlacementResponse'),
    newDescribeGameSessionPlacementResponse,

    -- ** DeleteScalingPolicy
    DeleteScalingPolicy (DeleteScalingPolicy'),
    newDeleteScalingPolicy,
    DeleteScalingPolicyResponse (DeleteScalingPolicyResponse'),
    newDeleteScalingPolicyResponse,

    -- ** StopGameSessionPlacement
    StopGameSessionPlacement (StopGameSessionPlacement'),
    newStopGameSessionPlacement,
    StopGameSessionPlacementResponse (StopGameSessionPlacementResponse'),
    newStopGameSessionPlacementResponse,

    -- ** StartGameSessionPlacement
    StartGameSessionPlacement (StartGameSessionPlacement'),
    newStartGameSessionPlacement,
    StartGameSessionPlacementResponse (StartGameSessionPlacementResponse'),
    newStartGameSessionPlacementResponse,

    -- ** DeleteMatchmakingRuleSet
    DeleteMatchmakingRuleSet (DeleteMatchmakingRuleSet'),
    newDeleteMatchmakingRuleSet,
    DeleteMatchmakingRuleSetResponse (DeleteMatchmakingRuleSetResponse'),
    newDeleteMatchmakingRuleSetResponse,

    -- ** DescribeGameServer
    DescribeGameServer (DescribeGameServer'),
    newDescribeGameServer,
    DescribeGameServerResponse (DescribeGameServerResponse'),
    newDescribeGameServerResponse,

    -- ** DeleteScript
    DeleteScript (DeleteScript'),
    newDeleteScript,
    DeleteScriptResponse (DeleteScriptResponse'),
    newDeleteScriptResponse,

    -- ** ListScripts (Paginated)
    ListScripts (ListScripts'),
    newListScripts,
    ListScriptsResponse (ListScriptsResponse'),
    newListScriptsResponse,

    -- ** DescribeGameSessions (Paginated)
    DescribeGameSessions (DescribeGameSessions'),
    newDescribeGameSessions,
    DescribeGameSessionsResponse (DescribeGameSessionsResponse'),
    newDescribeGameSessionsResponse,

    -- ** UpdateScript
    UpdateScript (UpdateScript'),
    newUpdateScript,
    UpdateScriptResponse (UpdateScriptResponse'),
    newUpdateScriptResponse,

    -- ** DescribeEC2InstanceLimits
    DescribeEC2InstanceLimits (DescribeEC2InstanceLimits'),
    newDescribeEC2InstanceLimits,
    DescribeEC2InstanceLimitsResponse (DescribeEC2InstanceLimitsResponse'),
    newDescribeEC2InstanceLimitsResponse,

    -- ** StopFleetActions
    StopFleetActions (StopFleetActions'),
    newStopFleetActions,
    StopFleetActionsResponse (StopFleetActionsResponse'),
    newStopFleetActionsResponse,

    -- ** GetInstanceAccess
    GetInstanceAccess (GetInstanceAccess'),
    newGetInstanceAccess,
    GetInstanceAccessResponse (GetInstanceAccessResponse'),
    newGetInstanceAccessResponse,

    -- ** StartFleetActions
    StartFleetActions (StartFleetActions'),
    newStartFleetActions,
    StartFleetActionsResponse (StartFleetActionsResponse'),
    newStartFleetActionsResponse,

    -- ** DescribePlayerSessions (Paginated)
    DescribePlayerSessions (DescribePlayerSessions'),
    newDescribePlayerSessions,
    DescribePlayerSessionsResponse (DescribePlayerSessionsResponse'),
    newDescribePlayerSessionsResponse,

    -- ** CreateScript
    CreateScript (CreateScript'),
    newCreateScript,
    CreateScriptResponse (CreateScriptResponse'),
    newCreateScriptResponse,

    -- ** CreateMatchmakingConfiguration
    CreateMatchmakingConfiguration (CreateMatchmakingConfiguration'),
    newCreateMatchmakingConfiguration,
    CreateMatchmakingConfigurationResponse (CreateMatchmakingConfigurationResponse'),
    newCreateMatchmakingConfigurationResponse,

    -- ** CreateVpcPeeringAuthorization
    CreateVpcPeeringAuthorization (CreateVpcPeeringAuthorization'),
    newCreateVpcPeeringAuthorization,
    CreateVpcPeeringAuthorizationResponse (CreateVpcPeeringAuthorizationResponse'),
    newCreateVpcPeeringAuthorizationResponse,

    -- ** CreateGameServerGroup
    CreateGameServerGroup (CreateGameServerGroup'),
    newCreateGameServerGroup,
    CreateGameServerGroupResponse (CreateGameServerGroupResponse'),
    newCreateGameServerGroupResponse,

    -- ** UpdateGameServerGroup
    UpdateGameServerGroup (UpdateGameServerGroup'),
    newUpdateGameServerGroup,
    UpdateGameServerGroupResponse (UpdateGameServerGroupResponse'),
    newUpdateGameServerGroupResponse,

    -- ** SearchGameSessions (Paginated)
    SearchGameSessions (SearchGameSessions'),
    newSearchGameSessions,
    SearchGameSessionsResponse (SearchGameSessionsResponse'),
    newSearchGameSessionsResponse,

    -- ** DeleteGameServerGroup
    DeleteGameServerGroup (DeleteGameServerGroup'),
    newDeleteGameServerGroup,
    DeleteGameServerGroupResponse (DeleteGameServerGroupResponse'),
    newDeleteGameServerGroupResponse,

    -- ** ListGameServerGroups (Paginated)
    ListGameServerGroups (ListGameServerGroups'),
    newListGameServerGroups,
    ListGameServerGroupsResponse (ListGameServerGroupsResponse'),
    newListGameServerGroupsResponse,

    -- ** UpdateRuntimeConfiguration
    UpdateRuntimeConfiguration (UpdateRuntimeConfiguration'),
    newUpdateRuntimeConfiguration,
    UpdateRuntimeConfigurationResponse (UpdateRuntimeConfigurationResponse'),
    newUpdateRuntimeConfigurationResponse,

    -- ** CreateGameSessionQueue
    CreateGameSessionQueue (CreateGameSessionQueue'),
    newCreateGameSessionQueue,
    CreateGameSessionQueueResponse (CreateGameSessionQueueResponse'),
    newCreateGameSessionQueueResponse,

    -- ** ResumeGameServerGroup
    ResumeGameServerGroup (ResumeGameServerGroup'),
    newResumeGameServerGroup,
    ResumeGameServerGroupResponse (ResumeGameServerGroupResponse'),
    newResumeGameServerGroupResponse,

    -- ** DeleteVpcPeeringAuthorization
    DeleteVpcPeeringAuthorization (DeleteVpcPeeringAuthorization'),
    newDeleteVpcPeeringAuthorization,
    DeleteVpcPeeringAuthorizationResponse (DeleteVpcPeeringAuthorizationResponse'),
    newDeleteVpcPeeringAuthorizationResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeScript
    DescribeScript (DescribeScript'),
    newDescribeScript,
    DescribeScriptResponse (DescribeScriptResponse'),
    newDescribeScriptResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreatePlayerSessions
    CreatePlayerSessions (CreatePlayerSessions'),
    newCreatePlayerSessions,
    CreatePlayerSessionsResponse (CreatePlayerSessionsResponse'),
    newCreatePlayerSessionsResponse,

    -- ** UpdateGameServer
    UpdateGameServer (UpdateGameServer'),
    newUpdateGameServer,
    UpdateGameServerResponse (UpdateGameServerResponse'),
    newUpdateGameServerResponse,

    -- ** DescribeVpcPeeringAuthorizations
    DescribeVpcPeeringAuthorizations (DescribeVpcPeeringAuthorizations'),
    newDescribeVpcPeeringAuthorizations,
    DescribeVpcPeeringAuthorizationsResponse (DescribeVpcPeeringAuthorizationsResponse'),
    newDescribeVpcPeeringAuthorizationsResponse,

    -- ** ListGameServers (Paginated)
    ListGameServers (ListGameServers'),
    newListGameServers,
    ListGameServersResponse (ListGameServersResponse'),
    newListGameServersResponse,

    -- ** CreateBuild
    CreateBuild (CreateBuild'),
    newCreateBuild,
    CreateBuildResponse (CreateBuildResponse'),
    newCreateBuildResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

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
import Network.AWS.GameLift.Lens
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
import Network.AWS.GameLift.Types
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
import Network.AWS.GameLift.Waiters

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
