-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _InternalServerErrorException,
    _UnauthorizedException,
    _BadRequestException,

    -- * EngineVersion
    EngineVersion (..),
    mkEngineVersion,
    evName,

    -- * BrokerState
    BrokerState (..),

    -- * WeeklyStartTime
    WeeklyStartTime (..),
    mkWeeklyStartTime,
    wstDayOfWeek,
    wstTimeOfDay,
    wstTimeZone,

    -- * LdapServerMetadataInput
    LdapServerMetadataInput (..),
    mkLdapServerMetadataInput,
    lsmiHosts,
    lsmiRoleBase,
    lsmiRoleName,
    lsmiRoleSearchMatching,
    lsmiRoleSearchSubtree,
    lsmiServiceAccountPassword,
    lsmiServiceAccountUsername,
    lsmiUserBase,
    lsmiUserRoleName,
    lsmiUserSearchMatching,
    lsmiUserSearchSubtree,

    -- * BrokerSummary
    BrokerSummary (..),
    mkBrokerSummary,
    bsBrokerArn,
    bsBrokerId,
    bsBrokerName,
    bsBrokerState,
    bsCreated,
    bsDeploymentMode,
    bsEngineType,
    bsHostInstanceType,

    -- * BrokerInstance
    BrokerInstance (..),
    mkBrokerInstance,
    biConsoleURL,
    biEndpoints,
    biIpAddress,

    -- * ConfigurationId
    ConfigurationId (..),
    mkConfigurationId,
    ciId,
    ciRevision,

    -- * ConfigurationRevision
    ConfigurationRevision (..),
    mkConfigurationRevision,
    crCreated,
    crDescription,
    crRevision,

    -- * LogsSummary
    LogsSummary (..),
    mkLogsSummary,
    lsAudit,
    lsAuditLogGroup,
    lsGeneral,
    lsGeneralLogGroup,
    lsPending,

    -- * Configurations
    Configurations (..),
    mkConfigurations,
    cCurrent,
    cHistory,
    cPending,

    -- * AuthenticationStrategy
    AuthenticationStrategy (..),

    -- * LdapServerMetadataOutput
    LdapServerMetadataOutput (..),
    mkLdapServerMetadataOutput,
    lsmoHosts,
    lsmoRoleBase,
    lsmoRoleName,
    lsmoRoleSearchMatching,
    lsmoRoleSearchSubtree,
    lsmoServiceAccountUsername,
    lsmoUserBase,
    lsmoUserRoleName,
    lsmoUserSearchMatching,
    lsmoUserSearchSubtree,

    -- * SanitizationWarningReason
    SanitizationWarningReason (..),

    -- * User
    User (..),
    mkUser,
    uConsoleAccess,
    uGroups,
    uPassword,
    uUsername,

    -- * Logs
    Logs (..),
    mkLogs,
    lAudit,
    lGeneral,

    -- * EncryptionOptions
    EncryptionOptions (..),
    mkEncryptionOptions,
    eoUseAwsOwnedKey,
    eoKmsKeyId,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- * DeploymentMode
    DeploymentMode (..),

    -- * BrokerStorageType
    BrokerStorageType (..),

    -- * UserSummary
    UserSummary (..),
    mkUserSummary,
    usPendingChange,
    usUsername,

    -- * SanitizationWarning
    SanitizationWarning (..),
    mkSanitizationWarning,
    swAttributeName,
    swElementName,
    swReason,

    -- * BrokerEngineType
    BrokerEngineType (..),
    mkBrokerEngineType,
    betEngineType,
    betEngineVersions,

    -- * Configuration
    Configuration (..),
    mkConfiguration,
    cArn,
    cAuthenticationStrategy,
    cCreated,
    cDescription,
    cEngineType,
    cEngineVersion,
    cId,
    cLatestRevision,
    cName,
    cTags,

    -- * PendingLogs
    PendingLogs (..),
    mkPendingLogs,
    plAudit,
    plGeneral,

    -- * UserPendingChanges
    UserPendingChanges (..),
    mkUserPendingChanges,
    upcConsoleAccess,
    upcGroups,
    upcPendingChange,

    -- * ChangeType
    ChangeType (..),

    -- * EngineType
    EngineType (..),

    -- * BrokerInstanceOption
    BrokerInstanceOption (..),
    mkBrokerInstanceOption,
    bioAvailabilityZones,
    bioEngineType,
    bioHostInstanceType,
    bioStorageType,
    bioSupportedDeploymentModes,
    bioSupportedEngineVersions,

    -- * DayOfWeek
    DayOfWeek (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.AuthenticationStrategy
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerEngineType
import Network.AWS.MQ.Types.BrokerInstance
import Network.AWS.MQ.Types.BrokerInstanceOption
import Network.AWS.MQ.Types.BrokerState
import Network.AWS.MQ.Types.BrokerStorageType
import Network.AWS.MQ.Types.BrokerSummary
import Network.AWS.MQ.Types.ChangeType
import Network.AWS.MQ.Types.Configuration
import Network.AWS.MQ.Types.ConfigurationId
import Network.AWS.MQ.Types.ConfigurationRevision
import Network.AWS.MQ.Types.Configurations
import Network.AWS.MQ.Types.DayOfWeek
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EncryptionOptions
import Network.AWS.MQ.Types.EngineType
import Network.AWS.MQ.Types.EngineVersion
import Network.AWS.MQ.Types.LdapServerMetadataInput
import Network.AWS.MQ.Types.LdapServerMetadataOutput
import Network.AWS.MQ.Types.Logs
import Network.AWS.MQ.Types.LogsSummary
import Network.AWS.MQ.Types.PendingLogs
import Network.AWS.MQ.Types.SanitizationWarning
import Network.AWS.MQ.Types.SanitizationWarningReason
import Network.AWS.MQ.Types.User
import Network.AWS.MQ.Types.UserPendingChanges
import Network.AWS.MQ.Types.UserSummary
import Network.AWS.MQ.Types.WeeklyStartTime
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MQ",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "mq",
      Core._svcVersion = "2017-11-27",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MQ",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Returns information about an error.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | Returns information about an error.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError mkServiceConfig "ForbiddenException"
    Core.. Core.hasStatues 403
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead." #-}

-- | Returns information about an error.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Returns information about an error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | Returns information about an error.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError mkServiceConfig "UnauthorizedException"
    Core.. Core.hasStatues 401
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead." #-}

-- | Returns information about an error.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}
