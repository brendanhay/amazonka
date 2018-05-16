{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types
    (
    -- * Service Configuration
      gameLift

    -- * Errors
    , _InvalidFleetStatusException
    , _InvalidRequestException
    , _ConflictException
    , _TerminalRoutingStrategyException
    , _NotFoundException
    , _GameSessionFullException
    , _UnsupportedRegionException
    , _InvalidGameSessionStatusException
    , _InternalServiceException
    , _IdempotentParameterMismatchException
    , _UnauthorizedException
    , _FleetCapacityExceededException
    , _LimitExceededException

    -- * AcceptanceType
    , AcceptanceType (..)

    -- * BuildStatus
    , BuildStatus (..)

    -- * ComparisonOperatorType
    , ComparisonOperatorType (..)

    -- * EC2InstanceType
    , EC2InstanceType (..)

    -- * EventCode
    , EventCode (..)

    -- * FleetAction
    , FleetAction (..)

    -- * FleetStatus
    , FleetStatus (..)

    -- * FleetType
    , FleetType (..)

    -- * GameSessionPlacementState
    , GameSessionPlacementState (..)

    -- * GameSessionStatus
    , GameSessionStatus (..)

    -- * GameSessionStatusReason
    , GameSessionStatusReason (..)

    -- * IPProtocol
    , IPProtocol (..)

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * MatchmakingConfigurationStatus
    , MatchmakingConfigurationStatus (..)

    -- * MetricName
    , MetricName (..)

    -- * OperatingSystem
    , OperatingSystem (..)

    -- * PlayerSessionCreationPolicy
    , PlayerSessionCreationPolicy (..)

    -- * PlayerSessionStatus
    , PlayerSessionStatus (..)

    -- * PolicyType
    , PolicyType (..)

    -- * ProtectionPolicy
    , ProtectionPolicy (..)

    -- * RoutingStrategyType
    , RoutingStrategyType (..)

    -- * ScalingAdjustmentType
    , ScalingAdjustmentType (..)

    -- * ScalingStatusType
    , ScalingStatusType (..)

    -- * AWSCredentials
    , AWSCredentials
    , awsCredentials
    , acSecretAccessKey
    , acSessionToken
    , acAccessKeyId

    -- * Alias
    , Alias
    , alias
    , aCreationTime
    , aLastUpdatedTime
    , aAliasId
    , aRoutingStrategy
    , aName
    , aAliasARN
    , aDescription

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avSL
    , avSDM
    , avN
    , avS

    -- * Build
    , Build
    , build
    , bCreationTime
    , bStatus
    , bOperatingSystem
    , bBuildId
    , bName
    , bVersion
    , bSizeOnDisk

    -- * DesiredPlayerSession
    , DesiredPlayerSession
    , desiredPlayerSession
    , dpsPlayerData
    , dpsPlayerId

    -- * EC2InstanceCounts
    , EC2InstanceCounts
    , ec2InstanceCounts
    , eicIdLE
    , eicTERMINATING
    , eicPENDING
    , eicMAXIMUM
    , eicDESIRED
    , eicMINIMUM
    , eicACTIVE

    -- * EC2InstanceLimit
    , EC2InstanceLimit
    , ec2InstanceLimit
    , eilEC2InstanceType
    , eilCurrentInstances
    , eilInstanceLimit

    -- * Event
    , Event
    , event
    , eResourceId
    , ePreSignedLogURL
    , eEventTime
    , eMessage
    , eEventCode
    , eEventId

    -- * FleetAttributes
    , FleetAttributes
    , fleetAttributes
    , faCreationTime
    , faStatus
    , faServerLaunchParameters
    , faLogPaths
    , faOperatingSystem
    , faBuildId
    , faFleetARN
    , faFleetType
    , faTerminationTime
    , faInstanceType
    , faStoppedActions
    , faNewGameSessionProtectionPolicy
    , faName
    , faServerLaunchPath
    , faMetricGroups
    , faFleetId
    , faDescription
    , faResourceCreationLimitPolicy

    -- * FleetCapacity
    , FleetCapacity
    , fleetCapacity
    , fcInstanceType
    , fcFleetId
    , fcInstanceCounts

    -- * FleetUtilization
    , FleetUtilization
    , fleetUtilization
    , fuActiveGameSessionCount
    , fuMaximumPlayerSessionCount
    , fuCurrentPlayerSessionCount
    , fuFleetId
    , fuActiveServerProcessCount

    -- * GameProperty
    , GameProperty
    , gameProperty
    , gpKey
    , gpValue

    -- * GameSession
    , GameSession
    , gameSession
    , gsCreationTime
    , gsStatus
    , gsGameProperties
    , gsIPAddress
    , gsGameSessionId
    , gsMatchmakerData
    , gsMaximumPlayerSessionCount
    , gsTerminationTime
    , gsPlayerSessionCreationPolicy
    , gsName
    , gsCurrentPlayerSessionCount
    , gsStatusReason
    , gsGameSessionData
    , gsFleetId
    , gsCreatorId
    , gsPort

    -- * GameSessionConnectionInfo
    , GameSessionConnectionInfo
    , gameSessionConnectionInfo
    , gsciMatchedPlayerSessions
    , gsciIPAddress
    , gsciGameSessionARN
    , gsciPort

    -- * GameSessionDetail
    , GameSessionDetail
    , gameSessionDetail
    , gsdGameSession
    , gsdProtectionPolicy

    -- * GameSessionPlacement
    , GameSessionPlacement
    , gameSessionPlacement
    , gspStatus
    , gspPlacementId
    , gspGameProperties
    , gspIPAddress
    , gspGameSessionName
    , gspStartTime
    , gspGameSessionId
    , gspGameSessionRegion
    , gspMatchmakerData
    , gspMaximumPlayerSessionCount
    , gspEndTime
    , gspGameSessionARN
    , gspPlayerLatencies
    , gspGameSessionData
    , gspGameSessionQueueName
    , gspPlacedPlayerSessions
    , gspPort

    -- * GameSessionQueue
    , GameSessionQueue
    , gameSessionQueue
    , gsqGameSessionQueueARN
    , gsqPlayerLatencyPolicies
    , gsqTimeoutInSeconds
    , gsqDestinations
    , gsqName

    -- * GameSessionQueueDestination
    , GameSessionQueueDestination
    , gameSessionQueueDestination
    , gsqdDestinationARN

    -- * IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipToPort
    , ipIPRange
    , ipProtocol

    -- * Instance
    , Instance
    , instance'
    , iCreationTime
    , iInstanceId
    , iStatus
    , iIPAddress
    , iOperatingSystem
    , iType
    , iFleetId

    -- * InstanceAccess
    , InstanceAccess
    , instanceAccess
    , iaInstanceId
    , iaIPAddress
    , iaOperatingSystem
    , iaCredentials
    , iaFleetId

    -- * InstanceCredentials
    , InstanceCredentials
    , instanceCredentials
    , icUserName
    , icSecret

    -- * MatchedPlayerSession
    , MatchedPlayerSession
    , matchedPlayerSession
    , mpsPlayerSessionId
    , mpsPlayerId

    -- * MatchmakingConfiguration
    , MatchmakingConfiguration
    , matchmakingConfiguration
    , mcCreationTime
    , mcGameProperties
    , mcRuleSetName
    , mcAcceptanceTimeoutSeconds
    , mcRequestTimeoutSeconds
    , mcNotificationTarget
    , mcGameSessionQueueARNs
    , mcName
    , mcCustomEventData
    , mcAcceptanceRequired
    , mcGameSessionData
    , mcDescription
    , mcAdditionalPlayerCount

    -- * MatchmakingRuleSet
    , MatchmakingRuleSet
    , matchmakingRuleSet
    , mrsCreationTime
    , mrsRuleSetName
    , mrsRuleSetBody

    -- * MatchmakingTicket
    , MatchmakingTicket
    , matchmakingTicket
    , mtStatus
    , mtConfigurationName
    , mtStartTime
    , mtGameSessionConnectionInfo
    , mtTicketId
    , mtEstimatedWaitTime
    , mtStatusMessage
    , mtEndTime
    , mtStatusReason
    , mtPlayers

    -- * PlacedPlayerSession
    , PlacedPlayerSession
    , placedPlayerSession
    , ppsPlayerSessionId
    , ppsPlayerId

    -- * Player
    , Player
    , player
    , pPlayerAttributes
    , pTeam
    , pPlayerId
    , pLatencyInMs

    -- * PlayerLatency
    , PlayerLatency
    , playerLatency
    , plLatencyInMilliseconds
    , plRegionIdentifier
    , plPlayerId

    -- * PlayerLatencyPolicy
    , PlayerLatencyPolicy
    , playerLatencyPolicy
    , plpPolicyDurationSeconds
    , plpMaximumIndividualPlayerLatencyMilliseconds

    -- * PlayerSession
    , PlayerSession
    , playerSession
    , psCreationTime
    , psStatus
    , psIPAddress
    , psGameSessionId
    , psTerminationTime
    , psPlayerSessionId
    , psFleetId
    , psPlayerData
    , psPlayerId
    , psPort

    -- * ResourceCreationLimitPolicy
    , ResourceCreationLimitPolicy
    , resourceCreationLimitPolicy
    , rclpNewGameSessionsPerCreator
    , rclpPolicyPeriodInMinutes

    -- * RoutingStrategy
    , RoutingStrategy
    , routingStrategy
    , rsType
    , rsMessage
    , rsFleetId

    -- * RuntimeConfiguration
    , RuntimeConfiguration
    , runtimeConfiguration
    , rcGameSessionActivationTimeoutSeconds
    , rcServerProcesses
    , rcMaxConcurrentGameSessionActivations

    -- * S3Location
    , S3Location
    , s3Location
    , slBucket
    , slKey
    , slRoleARN

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spStatus
    , spScalingAdjustmentType
    , spEvaluationPeriods
    , spPolicyType
    , spMetricName
    , spComparisonOperator
    , spName
    , spThreshold
    , spScalingAdjustment
    , spFleetId
    , spTargetConfiguration

    -- * ServerProcess
    , ServerProcess
    , serverProcess
    , spParameters
    , spLaunchPath
    , spConcurrentExecutions

    -- * TargetConfiguration
    , TargetConfiguration
    , targetConfiguration
    , tcTargetValue

    -- * VPCPeeringAuthorization
    , VPCPeeringAuthorization
    , vpcPeeringAuthorization
    , vpaCreationTime
    , vpaPeerVPCId
    , vpaPeerVPCAWSAccountId
    , vpaGameLiftAWSAccountId
    , vpaExpirationTime

    -- * VPCPeeringConnection
    , VPCPeeringConnection
    , vpcPeeringConnection
    , vpcVPCPeeringConnectionId
    , vpcStatus
    , vpcPeerVPCId
    , vpcIPV4CidrBlock
    , vpcGameLiftVPCId
    , vpcFleetId

    -- * VPCPeeringConnectionStatus
    , VPCPeeringConnectionStatus
    , vpcPeeringConnectionStatus
    , vpcsCode
    , vpcsMessage
    ) where

import Network.AWS.GameLift.Types.Product
import Network.AWS.GameLift.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
gameLift :: Service
gameLift =
  Service
    { _svcAbbrev = "GameLift"
    , _svcSigner = v4
    , _svcPrefix = "gamelift"
    , _svcVersion = "2015-10-01"
    , _svcEndpoint = defaultEndpoint gameLift
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "GameLift"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The requested operation would cause a conflict with the current state of a resource associated with the request and/or the fleet. Resolve the conflict before retrying.
--
--
_InvalidFleetStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFleetStatusException =
  _MatchServiceError gameLift "InvalidFleetStatusException"


-- | One or more parameter values in the request are invalid. Correct the invalid parameter values before retrying.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError gameLift "InvalidRequestException"


-- | The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException = _MatchServiceError gameLift "ConflictException"


-- | The service is unable to resolve the routing for a particular alias because it has a terminal 'RoutingStrategy' associated with it. The message returned in this exception is the message defined in the routing strategy itself. Such requests should only be retried if the routing strategy for the specified alias is modified.
--
--
_TerminalRoutingStrategyException :: AsError a => Getting (First ServiceError) a ServiceError
_TerminalRoutingStrategyException =
  _MatchServiceError gameLift "TerminalRoutingStrategyException"


-- | A service resource associated with the request could not be found. Clients should not retry such requests.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError gameLift "NotFoundException"


-- | The game instance is currently full and cannot allow the requested player(s) to join. Clients can retry such requests immediately or after a waiting period.
--
--
_GameSessionFullException :: AsError a => Getting (First ServiceError) a ServiceError
_GameSessionFullException =
  _MatchServiceError gameLift "GameSessionFullException"


-- | The requested operation is not supported in the region specified.
--
--
_UnsupportedRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedRegionException =
  _MatchServiceError gameLift "UnsupportedRegionException"


-- | The requested operation would cause a conflict with the current state of a resource associated with the request and/or the game instance. Resolve the conflict before retrying.
--
--
_InvalidGameSessionStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGameSessionStatusException =
  _MatchServiceError gameLift "InvalidGameSessionStatusException"


-- | The service encountered an unrecoverable internal failure while processing the request. Clients can retry such requests immediately or after a waiting period.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError gameLift "InternalServiceException"


-- | A game session with this custom ID string already exists in this fleet. Resolve this conflict before retrying this request.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException =
  _MatchServiceError gameLift "IdempotentParameterMismatchException"


-- | The client failed authentication. Clients should not retry such requests.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException = _MatchServiceError gameLift "UnauthorizedException"


-- | The specified fleet has no available instances to fulfill a @CreateGameSession@ request. Clients can retry such requests immediately or after a waiting period.
--
--
_FleetCapacityExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_FleetCapacityExceededException =
  _MatchServiceError gameLift "FleetCapacityExceededException"


-- | The requested operation would cause the resource to exceed the allowed service limit. Resolve the issue before retrying.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError gameLift "LimitExceededException"

