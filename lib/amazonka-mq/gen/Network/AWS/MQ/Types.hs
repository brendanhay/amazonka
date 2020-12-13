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
    mqService,

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
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- * BrokerEngineType
    BrokerEngineType (..),
    mkBrokerEngineType,
    betEngineVersions,
    betEngineType,

    -- * BrokerInstance
    BrokerInstance (..),
    mkBrokerInstance,
    biIPAddress,
    biConsoleURL,
    biEndpoints,

    -- * BrokerInstanceOption
    BrokerInstanceOption (..),
    mkBrokerInstanceOption,
    bioSupportedEngineVersions,
    bioAvailabilityZones,
    bioSupportedDeploymentModes,
    bioEngineType,
    bioHostInstanceType,
    bioStorageType,

    -- * BrokerSummary
    BrokerSummary (..),
    mkBrokerSummary,
    bsBrokerName,
    bsBrokerState,
    bsCreated,
    bsDeploymentMode,
    bsBrokerId,
    bsEngineType,
    bsBrokerARN,
    bsHostInstanceType,

    -- * Configuration
    Configuration (..),
    mkConfiguration,
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
    ConfigurationId (..),
    mkConfigurationId,
    ciId,
    ciRevision,

    -- * ConfigurationRevision
    ConfigurationRevision (..),
    mkConfigurationRevision,
    crCreated,
    crRevision,
    crDescription,

    -- * Configurations
    Configurations (..),
    mkConfigurations,
    cPending,
    cHistory,
    cCurrent,

    -- * EncryptionOptions
    EncryptionOptions (..),
    mkEncryptionOptions,
    eoUseAWSOwnedKey,
    eoKMSKeyId,

    -- * EngineVersion
    EngineVersion (..),
    mkEngineVersion,
    evName,

    -- * LdapServerMetadataInput
    LdapServerMetadataInput (..),
    mkLdapServerMetadataInput,
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
    LdapServerMetadataOutput (..),
    mkLdapServerMetadataOutput,
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
    Logs (..),
    mkLogs,
    lAudit,
    lGeneral,

    -- * LogsSummary
    LogsSummary (..),
    mkLogsSummary,
    lsPending,
    lsAudit,
    lsGeneral,
    lsGeneralLogGroup,
    lsAuditLogGroup,

    -- * PendingLogs
    PendingLogs (..),
    mkPendingLogs,
    plAudit,
    plGeneral,

    -- * SanitizationWarning
    SanitizationWarning (..),
    mkSanitizationWarning,
    swReason,
    swAttributeName,
    swElementName,

    -- * User
    User (..),
    mkUser,
    uGroups,
    uConsoleAccess,
    uUsername,
    uPassword,

    -- * UserPendingChanges
    UserPendingChanges (..),
    mkUserPendingChanges,
    upcGroups,
    upcConsoleAccess,
    upcPendingChange,

    -- * UserSummary
    UserSummary (..),
    mkUserSummary,
    usUsername,
    usPendingChange,

    -- * WeeklyStartTime
    WeeklyStartTime (..),
    mkWeeklyStartTime,
    wstTimeOfDay,
    wstTimeZone,
    wstDayOfWeek,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon MQ SDK configuration.
mqService :: Lude.Service
mqService =
  Lude.Service
    { Lude._svcAbbrev = "MQ",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "mq",
      Lude._svcVersion = "2017-11-27",
      Lude._svcEndpoint = Lude.defaultEndpoint mqService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "MQ",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
