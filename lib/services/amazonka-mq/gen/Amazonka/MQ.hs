{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MQ
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon MQ is a managed message broker service for Apache ActiveMQ and
-- RabbitMQ that makes it easy to set up and operate message brokers in the
-- cloud. A message broker allows software applications and components to
-- communicate using various programming languages, operating systems, and
-- formal messaging protocols.
module Amazonka.MQ
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateBroker
    CreateBroker (CreateBroker'),
    newCreateBroker,
    CreateBrokerResponse (CreateBrokerResponse'),
    newCreateBrokerResponse,

    -- ** CreateConfiguration
    CreateConfiguration (CreateConfiguration'),
    newCreateConfiguration,
    CreateConfigurationResponse (CreateConfigurationResponse'),
    newCreateConfigurationResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteBroker
    DeleteBroker (DeleteBroker'),
    newDeleteBroker,
    DeleteBrokerResponse (DeleteBrokerResponse'),
    newDeleteBrokerResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

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

    -- ** DescribeBrokerInstanceOptions
    DescribeBrokerInstanceOptions (DescribeBrokerInstanceOptions'),
    newDescribeBrokerInstanceOptions,
    DescribeBrokerInstanceOptionsResponse (DescribeBrokerInstanceOptionsResponse'),
    newDescribeBrokerInstanceOptionsResponse,

    -- ** DescribeConfiguration
    DescribeConfiguration (DescribeConfiguration'),
    newDescribeConfiguration,
    DescribeConfigurationResponse (DescribeConfigurationResponse'),
    newDescribeConfigurationResponse,

    -- ** DescribeConfigurationRevision
    DescribeConfigurationRevision (DescribeConfigurationRevision'),
    newDescribeConfigurationRevision,
    DescribeConfigurationRevisionResponse (DescribeConfigurationRevisionResponse'),
    newDescribeConfigurationRevisionResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** ListBrokers (Paginated)
    ListBrokers (ListBrokers'),
    newListBrokers,
    ListBrokersResponse (ListBrokersResponse'),
    newListBrokersResponse,

    -- ** ListConfigurationRevisions
    ListConfigurationRevisions (ListConfigurationRevisions'),
    newListConfigurationRevisions,
    ListConfigurationRevisionsResponse (ListConfigurationRevisionsResponse'),
    newListConfigurationRevisionsResponse,

    -- ** ListConfigurations
    ListConfigurations (ListConfigurations'),
    newListConfigurations,
    ListConfigurationsResponse (ListConfigurationsResponse'),
    newListConfigurationsResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ListUsers
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** RebootBroker
    RebootBroker (RebootBroker'),
    newRebootBroker,
    RebootBrokerResponse (RebootBrokerResponse'),
    newRebootBrokerResponse,

    -- ** UpdateBroker
    UpdateBroker (UpdateBroker'),
    newUpdateBroker,
    UpdateBrokerResponse (UpdateBrokerResponse'),
    newUpdateBrokerResponse,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,
    UpdateConfigurationResponse (UpdateConfigurationResponse'),
    newUpdateConfigurationResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

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

    -- ** ActionRequired
    ActionRequired (ActionRequired'),
    newActionRequired,

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

import Amazonka.MQ.CreateBroker
import Amazonka.MQ.CreateConfiguration
import Amazonka.MQ.CreateTags
import Amazonka.MQ.CreateUser
import Amazonka.MQ.DeleteBroker
import Amazonka.MQ.DeleteTags
import Amazonka.MQ.DeleteUser
import Amazonka.MQ.DescribeBroker
import Amazonka.MQ.DescribeBrokerEngineTypes
import Amazonka.MQ.DescribeBrokerInstanceOptions
import Amazonka.MQ.DescribeConfiguration
import Amazonka.MQ.DescribeConfigurationRevision
import Amazonka.MQ.DescribeUser
import Amazonka.MQ.Lens
import Amazonka.MQ.ListBrokers
import Amazonka.MQ.ListConfigurationRevisions
import Amazonka.MQ.ListConfigurations
import Amazonka.MQ.ListTags
import Amazonka.MQ.ListUsers
import Amazonka.MQ.RebootBroker
import Amazonka.MQ.Types
import Amazonka.MQ.UpdateBroker
import Amazonka.MQ.UpdateConfiguration
import Amazonka.MQ.UpdateUser
import Amazonka.MQ.Waiters

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
