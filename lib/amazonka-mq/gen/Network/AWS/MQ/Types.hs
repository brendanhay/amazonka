{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types
  ( -- * Service Configuration
    mq,

    -- * Errors

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
    AvailabilityZone,
    availabilityZone,
    azName,

    -- * BrokerEngineType
    BrokerEngineType,
    brokerEngineType,
    betEngineVersions,
    betEngineType,

    -- * BrokerInstance
    BrokerInstance,
    brokerInstance,
    biIPAddress,
    biConsoleURL,
    biEndpoints,

    -- * BrokerInstanceOption
    BrokerInstanceOption,
    brokerInstanceOption,
    bioSupportedEngineVersions,
    bioAvailabilityZones,
    bioSupportedDeploymentModes,
    bioEngineType,
    bioHostInstanceType,
    bioStorageType,

    -- * BrokerSummary
    BrokerSummary,
    brokerSummary,
    bsBrokerName,
    bsBrokerState,
    bsCreated,
    bsDeploymentMode,
    bsBrokerId,
    bsEngineType,
    bsBrokerARN,
    bsHostInstanceType,

    -- * Configuration
    Configuration,
    configuration,
    cEngineVersion,
    cARN,
    cLatestRevision,
    cCreated,
    cAuthenticationStrategy,
    cName,
    cId,
    cDescription,
    cEngineType,
    cTags,

    -- * ConfigurationId
    ConfigurationId,
    configurationId,
    ciId,
    ciRevision,

    -- * ConfigurationRevision
    ConfigurationRevision,
    configurationRevision,
    crCreated,
    crRevision,
    crDescription,

    -- * Configurations
    Configurations,
    configurations,
    cPending,
    cHistory,
    cCurrent,

    -- * EncryptionOptions
    EncryptionOptions,
    encryptionOptions,
    eoKMSKeyId,
    eoUseAWSOwnedKey,

    -- * EngineVersion
    EngineVersion,
    engineVersion,
    evName,

    -- * LdapServerMetadataInput
    LdapServerMetadataInput,
    ldapServerMetadataInput,
    lsmiUserBase,
    lsmiUserSearchMatching,
    lsmiUserRoleName,
    lsmiServiceAccountUsername,
    lsmiUserSearchSubtree,
    lsmiRoleSearchSubtree,
    lsmiHosts,
    lsmiRoleName,
    lsmiServiceAccountPassword,
    lsmiRoleSearchMatching,
    lsmiRoleBase,

    -- * LdapServerMetadataOutput
    LdapServerMetadataOutput,
    ldapServerMetadataOutput,
    lsmoUserBase,
    lsmoUserSearchMatching,
    lsmoUserRoleName,
    lsmoServiceAccountUsername,
    lsmoUserSearchSubtree,
    lsmoRoleSearchSubtree,
    lsmoHosts,
    lsmoRoleName,
    lsmoRoleSearchMatching,
    lsmoRoleBase,

    -- * Logs
    Logs,
    logs,
    lAudit,
    lGeneral,

    -- * LogsSummary
    LogsSummary,
    logsSummary,
    lsPending,
    lsAudit,
    lsGeneral,
    lsGeneralLogGroup,
    lsAuditLogGroup,

    -- * PendingLogs
    PendingLogs,
    pendingLogs,
    plAudit,
    plGeneral,

    -- * SanitizationWarning
    SanitizationWarning,
    sanitizationWarning,
    swReason,
    swAttributeName,
    swElementName,

    -- * User
    User,
    user,
    uGroups,
    uConsoleAccess,
    uUsername,
    uPassword,

    -- * UserPendingChanges
    UserPendingChanges,
    userPendingChanges,
    upcGroups,
    upcConsoleAccess,
    upcPendingChange,

    -- * UserSummary
    UserSummary,
    userSummary,
    usUsername,
    usPendingChange,

    -- * WeeklyStartTime
    WeeklyStartTime,
    weeklyStartTime,
    wstTimeOfDay,
    wstTimeZone,
    wstDayOfWeek,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
mq :: Service
mq =
  Service
    { _svcAbbrev = "MQ",
      _svcSigner = v4,
      _svcPrefix = "mq",
      _svcVersion = "2017-11-27",
      _svcEndpoint = defaultEndpoint mq,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "MQ",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
