{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _InternalServerErrorException,
    _UnauthorizedException,
    _BadRequestException,

    -- * AuthenticationStrategy
    AuthenticationStrategy (..),

    -- * BrokerState
    BrokerState (..),

    -- * BrokerStorageType
    BrokerStorageType (..),

    -- * ChangeType
    ChangeType (..),

    -- * DayOfWeek
    DayOfWeek (..),

    -- * DeploymentMode
    DeploymentMode (..),

    -- * EngineType
    EngineType (..),

    -- * SanitizationWarningReason
    SanitizationWarningReason (..),

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * BrokerEngineType
    BrokerEngineType (..),
    newBrokerEngineType,
    brokerEngineType_engineVersions,
    brokerEngineType_engineType,

    -- * BrokerInstance
    BrokerInstance (..),
    newBrokerInstance,
    brokerInstance_ipAddress,
    brokerInstance_consoleURL,
    brokerInstance_endpoints,

    -- * BrokerInstanceOption
    BrokerInstanceOption (..),
    newBrokerInstanceOption,
    brokerInstanceOption_supportedEngineVersions,
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_engineType,
    brokerInstanceOption_hostInstanceType,
    brokerInstanceOption_storageType,

    -- * BrokerSummary
    BrokerSummary (..),
    newBrokerSummary,
    brokerSummary_brokerName,
    brokerSummary_brokerState,
    brokerSummary_created,
    brokerSummary_brokerId,
    brokerSummary_brokerArn,
    brokerSummary_hostInstanceType,
    brokerSummary_deploymentMode,
    brokerSummary_engineType,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_tags,
    configuration_description,
    configuration_engineVersion,
    configuration_latestRevision,
    configuration_authenticationStrategy,
    configuration_engineType,
    configuration_id,
    configuration_arn,
    configuration_name,
    configuration_created,

    -- * ConfigurationId
    ConfigurationId (..),
    newConfigurationId,
    configurationId_revision,
    configurationId_id,

    -- * ConfigurationRevision
    ConfigurationRevision (..),
    newConfigurationRevision,
    configurationRevision_description,
    configurationRevision_revision,
    configurationRevision_created,

    -- * Configurations
    Configurations (..),
    newConfigurations,
    configurations_pending,
    configurations_history,
    configurations_current,

    -- * EncryptionOptions
    EncryptionOptions (..),
    newEncryptionOptions,
    encryptionOptions_kmsKeyId,
    encryptionOptions_useAwsOwnedKey,

    -- * EngineVersion
    EngineVersion (..),
    newEngineVersion,
    engineVersion_name,

    -- * LdapServerMetadataInput
    LdapServerMetadataInput (..),
    newLdapServerMetadataInput,
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_roleSearchSubtree,
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_hosts,
    ldapServerMetadataInput_userSearchMatching,
    ldapServerMetadataInput_userBase,
    ldapServerMetadataInput_roleSearchMatching,
    ldapServerMetadataInput_serviceAccountUsername,
    ldapServerMetadataInput_roleBase,
    ldapServerMetadataInput_serviceAccountPassword,

    -- * LdapServerMetadataOutput
    LdapServerMetadataOutput (..),
    newLdapServerMetadataOutput,
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_userSearchSubtree,
    ldapServerMetadataOutput_roleSearchSubtree,
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_hosts,
    ldapServerMetadataOutput_userSearchMatching,
    ldapServerMetadataOutput_userBase,
    ldapServerMetadataOutput_roleSearchMatching,
    ldapServerMetadataOutput_serviceAccountUsername,
    ldapServerMetadataOutput_roleBase,

    -- * Logs
    Logs (..),
    newLogs,
    logs_audit,
    logs_general,

    -- * LogsSummary
    LogsSummary (..),
    newLogsSummary,
    logsSummary_pending,
    logsSummary_audit,
    logsSummary_auditLogGroup,
    logsSummary_generalLogGroup,
    logsSummary_general,

    -- * PendingLogs
    PendingLogs (..),
    newPendingLogs,
    pendingLogs_audit,
    pendingLogs_general,

    -- * SanitizationWarning
    SanitizationWarning (..),
    newSanitizationWarning,
    sanitizationWarning_attributeName,
    sanitizationWarning_elementName,
    sanitizationWarning_reason,

    -- * User
    User (..),
    newUser,
    user_groups,
    user_consoleAccess,
    user_username,
    user_password,

    -- * UserPendingChanges
    UserPendingChanges (..),
    newUserPendingChanges,
    userPendingChanges_groups,
    userPendingChanges_consoleAccess,
    userPendingChanges_pendingChange,

    -- * UserSummary
    UserSummary (..),
    newUserSummary,
    userSummary_pendingChange,
    userSummary_username,

    -- * WeeklyStartTime
    WeeklyStartTime (..),
    newWeeklyStartTime,
    weeklyStartTime_timeZone,
    weeklyStartTime_timeOfDay,
    weeklyStartTime_dayOfWeek,
  )
where

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MQ",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mq",
      Core._serviceSigningName = "mq",
      Core._serviceVersion = "2017-11-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MQ",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Returns information about an error.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Returns information about an error.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Returns information about an error.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Returns information about an error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Returns information about an error.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | Returns information about an error.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
