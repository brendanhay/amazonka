{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon MQ is a managed message broker service for Apache ActiveMQ and
-- RabbitMQ that makes it easy to set up and operate message brokers in the
-- cloud. A message broker allows software applications and components to
-- communicate using various programming languages, operating systems, and
-- formal messaging protocols.
module Network.AWS.MQ
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** ConflictException
    _ConflictException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateBroker
    CreateBroker (CreateBroker'),
    newCreateBroker,
    CreateBrokerResponse (CreateBrokerResponse'),
    newCreateBrokerResponse,

    -- ** DescribeBrokerInstanceOptions
    DescribeBrokerInstanceOptions (DescribeBrokerInstanceOptions'),
    newDescribeBrokerInstanceOptions,
    DescribeBrokerInstanceOptionsResponse (DescribeBrokerInstanceOptionsResponse'),
    newDescribeBrokerInstanceOptionsResponse,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,
    UpdateConfigurationResponse (UpdateConfigurationResponse'),
    newUpdateConfigurationResponse,

    -- ** ListConfigurations
    ListConfigurations (ListConfigurations'),
    newListConfigurations,
    ListConfigurationsResponse (ListConfigurationsResponse'),
    newListConfigurationsResponse,

    -- ** DescribeBroker
    DescribeBroker (DescribeBroker'),
    newDescribeBroker,
    DescribeBrokerResponse (DescribeBrokerResponse'),
    newDescribeBrokerResponse,

    -- ** DescribeBrokerEngineTypes
    DescribeBrokerEngineTypes (DescribeBrokerEngineTypes'),
    newDescribeBrokerEngineTypes,
    DescribeBrokerEngineTypesResponse (DescribeBrokerEngineTypesResponse'),
    newDescribeBrokerEngineTypesResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** ListBrokers (Paginated)
    ListBrokers (ListBrokers'),
    newListBrokers,
    ListBrokersResponse (ListBrokersResponse'),
    newListBrokersResponse,

    -- ** UpdateBroker
    UpdateBroker (UpdateBroker'),
    newUpdateBroker,
    UpdateBrokerResponse (UpdateBrokerResponse'),
    newUpdateBrokerResponse,

    -- ** DeleteBroker
    DeleteBroker (DeleteBroker'),
    newDeleteBroker,
    DeleteBrokerResponse (DeleteBrokerResponse'),
    newDeleteBrokerResponse,

    -- ** RebootBroker
    RebootBroker (RebootBroker'),
    newRebootBroker,
    RebootBrokerResponse (RebootBrokerResponse'),
    newRebootBrokerResponse,

    -- ** ListConfigurationRevisions
    ListConfigurationRevisions (ListConfigurationRevisions'),
    newListConfigurationRevisions,
    ListConfigurationRevisionsResponse (ListConfigurationRevisionsResponse'),
    newListConfigurationRevisionsResponse,

    -- ** CreateConfiguration
    CreateConfiguration (CreateConfiguration'),
    newCreateConfiguration,
    CreateConfigurationResponse (CreateConfigurationResponse'),
    newCreateConfigurationResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** DescribeConfigurationRevision
    DescribeConfigurationRevision (DescribeConfigurationRevision'),
    newDescribeConfigurationRevision,
    DescribeConfigurationRevisionResponse (DescribeConfigurationRevisionResponse'),
    newDescribeConfigurationRevisionResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** ListUsers
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** DescribeConfiguration
    DescribeConfiguration (DescribeConfiguration'),
    newDescribeConfiguration,
    DescribeConfigurationResponse (DescribeConfigurationResponse'),
    newDescribeConfigurationResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- * Types

    -- ** AuthenticationStrategy
    AuthenticationStrategy (..),

    -- ** BrokerState
    BrokerState (..),

    -- ** BrokerStorageType
    BrokerStorageType (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** DayOfWeek
    DayOfWeek (..),

    -- ** DeploymentMode
    DeploymentMode (..),

    -- ** EngineType
    EngineType (..),

    -- ** SanitizationWarningReason
    SanitizationWarningReason (..),

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** BrokerEngineType
    BrokerEngineType (BrokerEngineType'),
    newBrokerEngineType,

    -- ** BrokerInstance
    BrokerInstance (BrokerInstance'),
    newBrokerInstance,

    -- ** BrokerInstanceOption
    BrokerInstanceOption (BrokerInstanceOption'),
    newBrokerInstanceOption,

    -- ** BrokerSummary
    BrokerSummary (BrokerSummary'),
    newBrokerSummary,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** ConfigurationId
    ConfigurationId (ConfigurationId'),
    newConfigurationId,

    -- ** ConfigurationRevision
    ConfigurationRevision (ConfigurationRevision'),
    newConfigurationRevision,

    -- ** Configurations
    Configurations (Configurations'),
    newConfigurations,

    -- ** EncryptionOptions
    EncryptionOptions (EncryptionOptions'),
    newEncryptionOptions,

    -- ** EngineVersion
    EngineVersion (EngineVersion'),
    newEngineVersion,

    -- ** LdapServerMetadataInput
    LdapServerMetadataInput (LdapServerMetadataInput'),
    newLdapServerMetadataInput,

    -- ** LdapServerMetadataOutput
    LdapServerMetadataOutput (LdapServerMetadataOutput'),
    newLdapServerMetadataOutput,

    -- ** Logs
    Logs (Logs'),
    newLogs,

    -- ** LogsSummary
    LogsSummary (LogsSummary'),
    newLogsSummary,

    -- ** PendingLogs
    PendingLogs (PendingLogs'),
    newPendingLogs,

    -- ** SanitizationWarning
    SanitizationWarning (SanitizationWarning'),
    newSanitizationWarning,

    -- ** User
    User (User'),
    newUser,

    -- ** UserPendingChanges
    UserPendingChanges (UserPendingChanges'),
    newUserPendingChanges,

    -- ** UserSummary
    UserSummary (UserSummary'),
    newUserSummary,

    -- ** WeeklyStartTime
    WeeklyStartTime (WeeklyStartTime'),
    newWeeklyStartTime,
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
import Network.AWS.MQ.Lens
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
