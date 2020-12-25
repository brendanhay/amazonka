{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon MQ is a managed message broker service for Apache ActiveMQ and RabbitMQ that makes it easy to set up and operate message brokers in the cloud. A message broker allows software applications and components to communicate using various programming languages, operating systems, and formal messaging protocols.
module Network.AWS.MQ
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConfiguration
    module Network.AWS.MQ.CreateConfiguration,

    -- ** CreateBroker
    module Network.AWS.MQ.CreateBroker,

    -- ** DeleteBroker
    module Network.AWS.MQ.DeleteBroker,

    -- ** UpdateBroker
    module Network.AWS.MQ.UpdateBroker,

    -- ** RebootBroker
    module Network.AWS.MQ.RebootBroker,

    -- ** ListConfigurationRevisions
    module Network.AWS.MQ.ListConfigurationRevisions,

    -- ** CreateTags
    module Network.AWS.MQ.CreateTags,

    -- ** ListUsers
    module Network.AWS.MQ.ListUsers,

    -- ** DeleteTags
    module Network.AWS.MQ.DeleteTags,

    -- ** ListConfigurations
    module Network.AWS.MQ.ListConfigurations,

    -- ** DescribeUser
    module Network.AWS.MQ.DescribeUser,

    -- ** DescribeBrokerInstanceOptions
    module Network.AWS.MQ.DescribeBrokerInstanceOptions,

    -- ** ListBrokers (Paginated)
    module Network.AWS.MQ.ListBrokers,

    -- ** CreateUser
    module Network.AWS.MQ.CreateUser,

    -- ** DescribeConfiguration
    module Network.AWS.MQ.DescribeConfiguration,

    -- ** UpdateUser
    module Network.AWS.MQ.UpdateUser,

    -- ** DeleteUser
    module Network.AWS.MQ.DeleteUser,

    -- ** ListTags
    module Network.AWS.MQ.ListTags,

    -- ** DescribeBrokerEngineTypes
    module Network.AWS.MQ.DescribeBrokerEngineTypes,

    -- ** DescribeConfigurationRevision
    module Network.AWS.MQ.DescribeConfigurationRevision,

    -- ** DescribeBroker
    module Network.AWS.MQ.DescribeBroker,

    -- ** UpdateConfiguration
    module Network.AWS.MQ.UpdateConfiguration,

    -- * Types

    -- ** EngineVersion
    EngineVersion (..),
    mkEngineVersion,
    evName,

    -- ** BrokerState
    BrokerState (..),

    -- ** WeeklyStartTime
    WeeklyStartTime (..),
    mkWeeklyStartTime,
    wstDayOfWeek,
    wstTimeOfDay,
    wstTimeZone,

    -- ** LdapServerMetadataInput
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

    -- ** BrokerSummary
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

    -- ** BrokerInstance
    BrokerInstance (..),
    mkBrokerInstance,
    biConsoleURL,
    biEndpoints,
    biIpAddress,

    -- ** ConfigurationId
    ConfigurationId (..),
    mkConfigurationId,
    ciId,
    ciRevision,

    -- ** ConfigurationRevision
    ConfigurationRevision (..),
    mkConfigurationRevision,
    crCreated,
    crDescription,
    crRevision,

    -- ** LogsSummary
    LogsSummary (..),
    mkLogsSummary,
    lsAudit,
    lsAuditLogGroup,
    lsGeneral,
    lsGeneralLogGroup,
    lsPending,

    -- ** Configurations
    Configurations (..),
    mkConfigurations,
    cCurrent,
    cHistory,
    cPending,

    -- ** AuthenticationStrategy
    AuthenticationStrategy (..),

    -- ** LdapServerMetadataOutput
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

    -- ** SanitizationWarningReason
    SanitizationWarningReason (..),

    -- ** User
    User (..),
    mkUser,
    uConsoleAccess,
    uGroups,
    uPassword,
    uUsername,

    -- ** Logs
    Logs (..),
    mkLogs,
    lAudit,
    lGeneral,

    -- ** EncryptionOptions
    EncryptionOptions (..),
    mkEncryptionOptions,
    eoUseAwsOwnedKey,
    eoKmsKeyId,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- ** DeploymentMode
    DeploymentMode (..),

    -- ** BrokerStorageType
    BrokerStorageType (..),

    -- ** UserSummary
    UserSummary (..),
    mkUserSummary,
    usPendingChange,
    usUsername,

    -- ** SanitizationWarning
    SanitizationWarning (..),
    mkSanitizationWarning,
    swAttributeName,
    swElementName,
    swReason,

    -- ** BrokerEngineType
    BrokerEngineType (..),
    mkBrokerEngineType,
    betEngineType,
    betEngineVersions,

    -- ** Configuration
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

    -- ** PendingLogs
    PendingLogs (..),
    mkPendingLogs,
    plAudit,
    plGeneral,

    -- ** UserPendingChanges
    UserPendingChanges (..),
    mkUserPendingChanges,
    upcConsoleAccess,
    upcGroups,
    upcPendingChange,

    -- ** ChangeType
    ChangeType (..),

    -- ** EngineType
    EngineType (..),

    -- ** BrokerInstanceOption
    BrokerInstanceOption (..),
    mkBrokerInstanceOption,
    bioAvailabilityZones,
    bioEngineType,
    bioHostInstanceType,
    bioStorageType,
    bioSupportedDeploymentModes,
    bioSupportedEngineVersions,

    -- ** DayOfWeek
    DayOfWeek (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.MQ.CreateBroker
import Network.AWS.MQ.CreateConfiguration
import Network.AWS.MQ.CreateTags
import Network.AWS.MQ.CreateUser
import Network.AWS.MQ.DeleteBroker
import Network.AWS.MQ.DeleteTags
import Network.AWS.MQ.DeleteUser
import Network.AWS.MQ.DescribeBroker
import Network.AWS.MQ.DescribeBrokerEngineTypes
import Network.AWS.MQ.DescribeBrokerInstanceOptions
import Network.AWS.MQ.DescribeConfiguration
import Network.AWS.MQ.DescribeConfigurationRevision
import Network.AWS.MQ.DescribeUser
import Network.AWS.MQ.ListBrokers
import Network.AWS.MQ.ListConfigurationRevisions
import Network.AWS.MQ.ListConfigurations
import Network.AWS.MQ.ListTags
import Network.AWS.MQ.ListUsers
import Network.AWS.MQ.RebootBroker
import Network.AWS.MQ.Types
import Network.AWS.MQ.UpdateBroker
import Network.AWS.MQ.UpdateConfiguration
import Network.AWS.MQ.UpdateUser
import Network.AWS.MQ.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MQ'.

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
