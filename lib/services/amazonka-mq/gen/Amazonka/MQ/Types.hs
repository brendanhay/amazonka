{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MQ.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnauthorizedException,
    _NotFoundException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
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

    -- * ActionRequired
    ActionRequired (..),
    newActionRequired,
    actionRequired_actionRequiredInfo,
    actionRequired_actionRequiredCode,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * BrokerEngineType
    BrokerEngineType (..),
    newBrokerEngineType,
    brokerEngineType_engineType,
    brokerEngineType_engineVersions,

    -- * BrokerInstance
    BrokerInstance (..),
    newBrokerInstance,
    brokerInstance_consoleURL,
    brokerInstance_endpoints,
    brokerInstance_ipAddress,

    -- * BrokerInstanceOption
    BrokerInstanceOption (..),
    newBrokerInstanceOption,
    brokerInstanceOption_engineType,
    brokerInstanceOption_supportedDeploymentModes,
    brokerInstanceOption_availabilityZones,
    brokerInstanceOption_storageType,
    brokerInstanceOption_hostInstanceType,
    brokerInstanceOption_supportedEngineVersions,

    -- * BrokerSummary
    BrokerSummary (..),
    newBrokerSummary,
    brokerSummary_brokerName,
    brokerSummary_created,
    brokerSummary_brokerState,
    brokerSummary_brokerId,
    brokerSummary_hostInstanceType,
    brokerSummary_brokerArn,
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
    configurations_history,
    configurations_current,
    configurations_pending,

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
    ldapServerMetadataInput_roleName,
    ldapServerMetadataInput_userSearchSubtree,
    ldapServerMetadataInput_userRoleName,
    ldapServerMetadataInput_roleSearchSubtree,
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
    ldapServerMetadataOutput_roleName,
    ldapServerMetadataOutput_userSearchSubtree,
    ldapServerMetadataOutput_userRoleName,
    ldapServerMetadataOutput_roleSearchSubtree,
    ldapServerMetadataOutput_hosts,
    ldapServerMetadataOutput_userSearchMatching,
    ldapServerMetadataOutput_userBase,
    ldapServerMetadataOutput_roleSearchMatching,
    ldapServerMetadataOutput_serviceAccountUsername,
    ldapServerMetadataOutput_roleBase,

    -- * Logs
    Logs (..),
    newLogs,
    logs_general,
    logs_audit,

    -- * LogsSummary
    LogsSummary (..),
    newLogsSummary,
    logsSummary_auditLogGroup,
    logsSummary_audit,
    logsSummary_pending,
    logsSummary_generalLogGroup,
    logsSummary_general,

    -- * PendingLogs
    PendingLogs (..),
    newPendingLogs,
    pendingLogs_general,
    pendingLogs_audit,

    -- * SanitizationWarning
    SanitizationWarning (..),
    newSanitizationWarning,
    sanitizationWarning_elementName,
    sanitizationWarning_attributeName,
    sanitizationWarning_reason,

    -- * User
    User (..),
    newUser,
    user_consoleAccess,
    user_groups,
    user_username,
    user_password,

    -- * UserPendingChanges
    UserPendingChanges (..),
    newUserPendingChanges,
    userPendingChanges_consoleAccess,
    userPendingChanges_groups,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MQ.Types.ActionRequired
import Amazonka.MQ.Types.AuthenticationStrategy
import Amazonka.MQ.Types.AvailabilityZone
import Amazonka.MQ.Types.BrokerEngineType
import Amazonka.MQ.Types.BrokerInstance
import Amazonka.MQ.Types.BrokerInstanceOption
import Amazonka.MQ.Types.BrokerState
import Amazonka.MQ.Types.BrokerStorageType
import Amazonka.MQ.Types.BrokerSummary
import Amazonka.MQ.Types.ChangeType
import Amazonka.MQ.Types.Configuration
import Amazonka.MQ.Types.ConfigurationId
import Amazonka.MQ.Types.ConfigurationRevision
import Amazonka.MQ.Types.Configurations
import Amazonka.MQ.Types.DayOfWeek
import Amazonka.MQ.Types.DeploymentMode
import Amazonka.MQ.Types.EncryptionOptions
import Amazonka.MQ.Types.EngineType
import Amazonka.MQ.Types.EngineVersion
import Amazonka.MQ.Types.LdapServerMetadataInput
import Amazonka.MQ.Types.LdapServerMetadataOutput
import Amazonka.MQ.Types.Logs
import Amazonka.MQ.Types.LogsSummary
import Amazonka.MQ.Types.PendingLogs
import Amazonka.MQ.Types.SanitizationWarning
import Amazonka.MQ.Types.SanitizationWarningReason
import Amazonka.MQ.Types.User
import Amazonka.MQ.Types.UserPendingChanges
import Amazonka.MQ.Types.UserSummary
import Amazonka.MQ.Types.WeeklyStartTime
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MQ",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mq",
      Core.signingName = "mq",
      Core.version = "2017-11-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MQ",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Returns information about an error.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

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
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Returns information about an error.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Returns information about an error.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
