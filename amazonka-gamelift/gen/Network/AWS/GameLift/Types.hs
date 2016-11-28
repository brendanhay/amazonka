{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _InvalidGameSessionStatusException
    , _InternalServiceException
    , _UnauthorizedException
    , _FleetCapacityExceededException
    , _LimitExceededException

    -- * BuildStatus
    , BuildStatus (..)

    -- * ComparisonOperatorType
    , ComparisonOperatorType (..)

    -- * EC2InstanceType
    , EC2InstanceType (..)

    -- * EventCode
    , EventCode (..)

    -- * FleetStatus
    , FleetStatus (..)

    -- * GameSessionStatus
    , GameSessionStatus (..)

    -- * IPProtocol
    , IPProtocol (..)

    -- * MetricName
    , MetricName (..)

    -- * OperatingSystem
    , OperatingSystem (..)

    -- * PlayerSessionCreationPolicy
    , PlayerSessionCreationPolicy (..)

    -- * PlayerSessionStatus
    , PlayerSessionStatus (..)

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
    , aDescription

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
    , faTerminationTime
    , faNewGameSessionProtectionPolicy
    , faName
    , faServerLaunchPath
    , faFleetId
    , faDescription

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
    , gsMaximumPlayerSessionCount
    , gsTerminationTime
    , gsPlayerSessionCreationPolicy
    , gsName
    , gsCurrentPlayerSessionCount
    , gsFleetId
    , gsPort

    -- * GameSessionDetail
    , GameSessionDetail
    , gameSessionDetail
    , gsdGameSession
    , gsdProtectionPolicy

    -- * IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipToPort
    , ipIPRange
    , ipProtocol

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
    , psPlayerId
    , psPort

    -- * RoutingStrategy
    , RoutingStrategy
    , routingStrategy
    , rsType
    , rsMessage
    , rsFleetId

    -- * RuntimeConfiguration
    , RuntimeConfiguration
    , runtimeConfiguration
    , rcServerProcesses

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
    , spMetricName
    , spComparisonOperator
    , spName
    , spThreshold
    , spScalingAdjustment
    , spFleetId

    -- * ServerProcess
    , ServerProcess
    , serverProcess
    , spParameters
    , spLaunchPath
    , spConcurrentExecutions
    ) where

import           Network.AWS.GameLift.Types.Product
import           Network.AWS.GameLift.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
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
    _ServiceError . hasCode "InvalidFleetStatusException"

-- | One or more parameters specified as part of the request are invalid. Correct the invalid parameters before retrying.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _ServiceError . hasCode "InvalidRequestException"

-- | The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException = _ServiceError . hasCode "ConflictException"

-- | The service is unable to resolve the routing for a particular alias because it has a terminal 'RoutingStrategy' associated with it. The message returned in this exception is the message defined in the routing strategy itself. Such requests should only be retried if the routing strategy for the specified alias is modified.
--
--
_TerminalRoutingStrategyException :: AsError a => Getting (First ServiceError) a ServiceError
_TerminalRoutingStrategyException =
    _ServiceError . hasCode "TerminalRoutingStrategyException"

-- | A service resource associated with the request could not be found. Clients should not retry such requests
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasCode "NotFoundException"

-- | The game instance is currently full and cannot allow the requested player(s) to join. This exception occurs in response to a 'CreatePlayerSession' request.
--
--
_GameSessionFullException :: AsError a => Getting (First ServiceError) a ServiceError
_GameSessionFullException = _ServiceError . hasCode "GameSessionFullException"

-- | The requested operation would cause a conflict with the current state of a resource associated with the request and/or the game instance. Clients should not retry such requests without resolving the conflict.
--
--
_InvalidGameSessionStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGameSessionStatusException =
    _ServiceError . hasCode "InvalidGameSessionStatusException"

-- | The service encountered an unrecoverable internal failure while processing the request. Clients can retry such requests, either immediately or after a back-off period.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException = _ServiceError . hasCode "InternalServiceException"

-- | The client failed authentication. Clients should not retry such requests
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException = _ServiceError . hasCode "UnauthorizedException"

-- | The specified fleet has no available instances to fulfill a request to create a new game session. Such requests should only be retried once the fleet capacity has been increased.
--
--
_FleetCapacityExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_FleetCapacityExceededException =
    _ServiceError . hasCode "FleetCapacityExceededException"

-- | The requested operation would cause the resource to exceed the allowed service limit. Resolve the issue before retrying.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
