{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types
    (
    -- * Service Configuration
      mq

    -- * Errors
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _InternalServerErrorException
    , _UnauthorizedException
    , _BadRequestException

    -- * BrokerState
    , BrokerState (..)

    -- * ChangeType
    , ChangeType (..)

    -- * DayOfWeek
    , DayOfWeek (..)

    -- * DeploymentMode
    , DeploymentMode (..)

    -- * EngineType
    , EngineType (..)

    -- * SanitizationWarningReason
    , SanitizationWarningReason (..)

    -- * BrokerInstance
    , BrokerInstance
    , brokerInstance
    , biConsoleURL
    , biEndpoints

    -- * BrokerSummary
    , BrokerSummary
    , brokerSummary
    , bsBrokerName
    , bsBrokerState
    , bsDeploymentMode
    , bsBrokerId
    , bsBrokerARN
    , bsHostInstanceType

    -- * Configuration
    , Configuration
    , configuration
    , cEngineVersion
    , cARN
    , cLatestRevision
    , cName
    , cId
    , cDescription
    , cEngineType

    -- * ConfigurationId
    , ConfigurationId
    , configurationId
    , ciId
    , ciRevision

    -- * ConfigurationRevision
    , ConfigurationRevision
    , configurationRevision
    , crRevision
    , crDescription

    -- * Configurations
    , Configurations
    , configurations
    , cPending
    , cHistory
    , cCurrent

    -- * SanitizationWarning
    , SanitizationWarning
    , sanitizationWarning
    , swReason
    , swAttributeName
    , swElementName

    -- * User
    , User
    , user
    , uGroups
    , uConsoleAccess
    , uUsername
    , uPassword

    -- * UserPendingChanges
    , UserPendingChanges
    , userPendingChanges
    , upcGroups
    , upcConsoleAccess
    , upcPendingChange

    -- * UserSummary
    , UserSummary
    , userSummary
    , usUsername
    , usPendingChange

    -- * WeeklyStartTime
    , WeeklyStartTime
    , weeklyStartTime
    , wstTimeOfDay
    , wstTimeZone
    , wstDayOfWeek
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types.Product
import Network.AWS.MQ.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
mq :: Service
mq =
  Service
    { _svcAbbrev = "MQ"
    , _svcSigner = v4
    , _svcPrefix = "mq"
    , _svcVersion = "2017-11-27"
    , _svcEndpoint = defaultEndpoint mq
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MQ"
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


-- | Returns information about an error.
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException = _MatchServiceError mq "ConflictException" . hasStatus 409


-- | Returns information about an error.
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException = _MatchServiceError mq "ForbiddenException" . hasStatus 403


-- | Returns information about an error.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError mq "NotFoundException" . hasStatus 404


-- | Returns information about an error.
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
  _MatchServiceError mq "InternalServerErrorException" . hasStatus 500


-- | Returns information about an error.
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError mq "UnauthorizedException" . hasStatus 401


-- | Returns information about an error.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError mq "BadRequestException" . hasStatus 400

