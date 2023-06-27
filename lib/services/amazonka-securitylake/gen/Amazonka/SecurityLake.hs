{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SecurityLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Security Lake is a fully managed security data lake service. You
-- can use Security Lake to automatically centralize security data from
-- cloud, on-premises, and custom sources into a data lake that\'s stored
-- in your Amazon Web Services account. Amazon Web Services Organizations
-- is an account management service that lets you consolidate multiple
-- Amazon Web Services accounts into an organization that you create and
-- centrally manage. With Organizations, you can create member accounts and
-- invite existing accounts to join your organization. Security Lake helps
-- you analyze security data for a more complete understanding of your
-- security posture across the entire organization. It can also help you
-- improve the protection of your workloads, applications, and data.
--
-- The data lake is backed by Amazon Simple Storage Service (Amazon S3)
-- buckets, and you retain ownership over your data.
--
-- Amazon Security Lake integrates with CloudTrail, a service that provides
-- a record of actions taken by a user, role, or an Amazon Web Services
-- service. In Security Lake, CloudTrail captures API calls for Security
-- Lake as events. The calls captured include calls from the Security Lake
-- console and code calls to the Security Lake API operations. If you
-- create a trail, you can enable continuous delivery of CloudTrail events
-- to an Amazon S3 bucket, including events for Security Lake. If you
-- don\'t configure a trail, you can still view the most recent events in
-- the CloudTrail console in Event history. Using the information collected
-- by CloudTrail you can determine the request that was made to Security
-- Lake, the IP address from which the request was made, who made the
-- request, when it was made, and additional details. To learn more about
-- Security Lake information in CloudTrail, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/securitylake-cloudtrail.html Amazon Security Lake User Guide>.
--
-- Security Lake automates the collection of security-related log and event
-- data from integrated Amazon Web Services and third-party services. It
-- also helps you manage the lifecycle of data with customizable retention
-- and replication settings. Security Lake converts ingested data into
-- Apache Parquet format and a standard open-source schema called the Open
-- Cybersecurity Schema Framework (OCSF).
--
-- Other Amazon Web Services and third-party services can subscribe to the
-- data that\'s stored in Security Lake for incident response and security
-- data analytics.
module Amazonka.SecurityLake
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAwsLogSource
    CreateAwsLogSource (CreateAwsLogSource'),
    newCreateAwsLogSource,
    CreateAwsLogSourceResponse (CreateAwsLogSourceResponse'),
    newCreateAwsLogSourceResponse,

    -- ** CreateCustomLogSource
    CreateCustomLogSource (CreateCustomLogSource'),
    newCreateCustomLogSource,
    CreateCustomLogSourceResponse (CreateCustomLogSourceResponse'),
    newCreateCustomLogSourceResponse,

    -- ** CreateDataLake
    CreateDataLake (CreateDataLake'),
    newCreateDataLake,
    CreateDataLakeResponse (CreateDataLakeResponse'),
    newCreateDataLakeResponse,

    -- ** CreateDataLakeExceptionSubscription
    CreateDataLakeExceptionSubscription (CreateDataLakeExceptionSubscription'),
    newCreateDataLakeExceptionSubscription,
    CreateDataLakeExceptionSubscriptionResponse (CreateDataLakeExceptionSubscriptionResponse'),
    newCreateDataLakeExceptionSubscriptionResponse,

    -- ** CreateDataLakeOrganizationConfiguration
    CreateDataLakeOrganizationConfiguration (CreateDataLakeOrganizationConfiguration'),
    newCreateDataLakeOrganizationConfiguration,
    CreateDataLakeOrganizationConfigurationResponse (CreateDataLakeOrganizationConfigurationResponse'),
    newCreateDataLakeOrganizationConfigurationResponse,

    -- ** CreateSubscriber
    CreateSubscriber (CreateSubscriber'),
    newCreateSubscriber,
    CreateSubscriberResponse (CreateSubscriberResponse'),
    newCreateSubscriberResponse,

    -- ** CreateSubscriberNotification
    CreateSubscriberNotification (CreateSubscriberNotification'),
    newCreateSubscriberNotification,
    CreateSubscriberNotificationResponse (CreateSubscriberNotificationResponse'),
    newCreateSubscriberNotificationResponse,

    -- ** DeleteAwsLogSource
    DeleteAwsLogSource (DeleteAwsLogSource'),
    newDeleteAwsLogSource,
    DeleteAwsLogSourceResponse (DeleteAwsLogSourceResponse'),
    newDeleteAwsLogSourceResponse,

    -- ** DeleteCustomLogSource
    DeleteCustomLogSource (DeleteCustomLogSource'),
    newDeleteCustomLogSource,
    DeleteCustomLogSourceResponse (DeleteCustomLogSourceResponse'),
    newDeleteCustomLogSourceResponse,

    -- ** DeleteDataLake
    DeleteDataLake (DeleteDataLake'),
    newDeleteDataLake,
    DeleteDataLakeResponse (DeleteDataLakeResponse'),
    newDeleteDataLakeResponse,

    -- ** DeleteDataLakeExceptionSubscription
    DeleteDataLakeExceptionSubscription (DeleteDataLakeExceptionSubscription'),
    newDeleteDataLakeExceptionSubscription,
    DeleteDataLakeExceptionSubscriptionResponse (DeleteDataLakeExceptionSubscriptionResponse'),
    newDeleteDataLakeExceptionSubscriptionResponse,

    -- ** DeleteDataLakeOrganizationConfiguration
    DeleteDataLakeOrganizationConfiguration (DeleteDataLakeOrganizationConfiguration'),
    newDeleteDataLakeOrganizationConfiguration,
    DeleteDataLakeOrganizationConfigurationResponse (DeleteDataLakeOrganizationConfigurationResponse'),
    newDeleteDataLakeOrganizationConfigurationResponse,

    -- ** DeleteSubscriber
    DeleteSubscriber (DeleteSubscriber'),
    newDeleteSubscriber,
    DeleteSubscriberResponse (DeleteSubscriberResponse'),
    newDeleteSubscriberResponse,

    -- ** DeleteSubscriberNotification
    DeleteSubscriberNotification (DeleteSubscriberNotification'),
    newDeleteSubscriberNotification,
    DeleteSubscriberNotificationResponse (DeleteSubscriberNotificationResponse'),
    newDeleteSubscriberNotificationResponse,

    -- ** DeregisterDataLakeDelegatedAdministrator
    DeregisterDataLakeDelegatedAdministrator (DeregisterDataLakeDelegatedAdministrator'),
    newDeregisterDataLakeDelegatedAdministrator,
    DeregisterDataLakeDelegatedAdministratorResponse (DeregisterDataLakeDelegatedAdministratorResponse'),
    newDeregisterDataLakeDelegatedAdministratorResponse,

    -- ** GetDataLakeExceptionSubscription
    GetDataLakeExceptionSubscription (GetDataLakeExceptionSubscription'),
    newGetDataLakeExceptionSubscription,
    GetDataLakeExceptionSubscriptionResponse (GetDataLakeExceptionSubscriptionResponse'),
    newGetDataLakeExceptionSubscriptionResponse,

    -- ** GetDataLakeOrganizationConfiguration
    GetDataLakeOrganizationConfiguration (GetDataLakeOrganizationConfiguration'),
    newGetDataLakeOrganizationConfiguration,
    GetDataLakeOrganizationConfigurationResponse (GetDataLakeOrganizationConfigurationResponse'),
    newGetDataLakeOrganizationConfigurationResponse,

    -- ** GetDataLakeSources (Paginated)
    GetDataLakeSources (GetDataLakeSources'),
    newGetDataLakeSources,
    GetDataLakeSourcesResponse (GetDataLakeSourcesResponse'),
    newGetDataLakeSourcesResponse,

    -- ** GetSubscriber
    GetSubscriber (GetSubscriber'),
    newGetSubscriber,
    GetSubscriberResponse (GetSubscriberResponse'),
    newGetSubscriberResponse,

    -- ** ListDataLakeExceptions (Paginated)
    ListDataLakeExceptions (ListDataLakeExceptions'),
    newListDataLakeExceptions,
    ListDataLakeExceptionsResponse (ListDataLakeExceptionsResponse'),
    newListDataLakeExceptionsResponse,

    -- ** ListDataLakes
    ListDataLakes (ListDataLakes'),
    newListDataLakes,
    ListDataLakesResponse (ListDataLakesResponse'),
    newListDataLakesResponse,

    -- ** ListLogSources (Paginated)
    ListLogSources (ListLogSources'),
    newListLogSources,
    ListLogSourcesResponse (ListLogSourcesResponse'),
    newListLogSourcesResponse,

    -- ** ListSubscribers (Paginated)
    ListSubscribers (ListSubscribers'),
    newListSubscribers,
    ListSubscribersResponse (ListSubscribersResponse'),
    newListSubscribersResponse,

    -- ** RegisterDataLakeDelegatedAdministrator
    RegisterDataLakeDelegatedAdministrator (RegisterDataLakeDelegatedAdministrator'),
    newRegisterDataLakeDelegatedAdministrator,
    RegisterDataLakeDelegatedAdministratorResponse (RegisterDataLakeDelegatedAdministratorResponse'),
    newRegisterDataLakeDelegatedAdministratorResponse,

    -- ** UpdateDataLake
    UpdateDataLake (UpdateDataLake'),
    newUpdateDataLake,
    UpdateDataLakeResponse (UpdateDataLakeResponse'),
    newUpdateDataLakeResponse,

    -- ** UpdateDataLakeExceptionSubscription
    UpdateDataLakeExceptionSubscription (UpdateDataLakeExceptionSubscription'),
    newUpdateDataLakeExceptionSubscription,
    UpdateDataLakeExceptionSubscriptionResponse (UpdateDataLakeExceptionSubscriptionResponse'),
    newUpdateDataLakeExceptionSubscriptionResponse,

    -- ** UpdateSubscriber
    UpdateSubscriber (UpdateSubscriber'),
    newUpdateSubscriber,
    UpdateSubscriberResponse (UpdateSubscriberResponse'),
    newUpdateSubscriberResponse,

    -- ** UpdateSubscriberNotification
    UpdateSubscriberNotification (UpdateSubscriberNotification'),
    newUpdateSubscriberNotification,
    UpdateSubscriberNotificationResponse (UpdateSubscriberNotificationResponse'),
    newUpdateSubscriberNotificationResponse,

    -- * Types

    -- ** AccessType
    AccessType (..),

    -- ** AwsLogSourceName
    AwsLogSourceName (..),

    -- ** DataLakeStatus
    DataLakeStatus (..),

    -- ** HttpMethod
    HttpMethod (..),

    -- ** SourceCollectionStatus
    SourceCollectionStatus (..),

    -- ** SubscriberStatus
    SubscriberStatus (..),

    -- ** AwsIdentity
    AwsIdentity (AwsIdentity'),
    newAwsIdentity,

    -- ** AwsLogSourceConfiguration
    AwsLogSourceConfiguration (AwsLogSourceConfiguration'),
    newAwsLogSourceConfiguration,

    -- ** AwsLogSourceResource
    AwsLogSourceResource (AwsLogSourceResource'),
    newAwsLogSourceResource,

    -- ** CustomLogSourceAttributes
    CustomLogSourceAttributes (CustomLogSourceAttributes'),
    newCustomLogSourceAttributes,

    -- ** CustomLogSourceConfiguration
    CustomLogSourceConfiguration (CustomLogSourceConfiguration'),
    newCustomLogSourceConfiguration,

    -- ** CustomLogSourceCrawlerConfiguration
    CustomLogSourceCrawlerConfiguration (CustomLogSourceCrawlerConfiguration'),
    newCustomLogSourceCrawlerConfiguration,

    -- ** CustomLogSourceProvider
    CustomLogSourceProvider (CustomLogSourceProvider'),
    newCustomLogSourceProvider,

    -- ** CustomLogSourceResource
    CustomLogSourceResource (CustomLogSourceResource'),
    newCustomLogSourceResource,

    -- ** DataLakeAutoEnableNewAccountConfiguration
    DataLakeAutoEnableNewAccountConfiguration (DataLakeAutoEnableNewAccountConfiguration'),
    newDataLakeAutoEnableNewAccountConfiguration,

    -- ** DataLakeConfiguration
    DataLakeConfiguration (DataLakeConfiguration'),
    newDataLakeConfiguration,

    -- ** DataLakeEncryptionConfiguration
    DataLakeEncryptionConfiguration (DataLakeEncryptionConfiguration'),
    newDataLakeEncryptionConfiguration,

    -- ** DataLakeException
    DataLakeException (DataLakeException'),
    newDataLakeException,

    -- ** DataLakeLifecycleConfiguration
    DataLakeLifecycleConfiguration (DataLakeLifecycleConfiguration'),
    newDataLakeLifecycleConfiguration,

    -- ** DataLakeLifecycleExpiration
    DataLakeLifecycleExpiration (DataLakeLifecycleExpiration'),
    newDataLakeLifecycleExpiration,

    -- ** DataLakeLifecycleTransition
    DataLakeLifecycleTransition (DataLakeLifecycleTransition'),
    newDataLakeLifecycleTransition,

    -- ** DataLakeReplicationConfiguration
    DataLakeReplicationConfiguration (DataLakeReplicationConfiguration'),
    newDataLakeReplicationConfiguration,

    -- ** DataLakeResource
    DataLakeResource (DataLakeResource'),
    newDataLakeResource,

    -- ** DataLakeSource
    DataLakeSource (DataLakeSource'),
    newDataLakeSource,

    -- ** DataLakeSourceStatus
    DataLakeSourceStatus (DataLakeSourceStatus'),
    newDataLakeSourceStatus,

    -- ** DataLakeUpdateException
    DataLakeUpdateException (DataLakeUpdateException'),
    newDataLakeUpdateException,

    -- ** DataLakeUpdateStatus
    DataLakeUpdateStatus (DataLakeUpdateStatus'),
    newDataLakeUpdateStatus,

    -- ** HttpsNotificationConfiguration
    HttpsNotificationConfiguration (HttpsNotificationConfiguration'),
    newHttpsNotificationConfiguration,

    -- ** LogSource
    LogSource (LogSource'),
    newLogSource,

    -- ** LogSourceResource
    LogSourceResource (LogSourceResource'),
    newLogSourceResource,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** SqsNotificationConfiguration
    SqsNotificationConfiguration (SqsNotificationConfiguration'),
    newSqsNotificationConfiguration,

    -- ** SubscriberResource
    SubscriberResource (SubscriberResource'),
    newSubscriberResource,
  )
where

import Amazonka.SecurityLake.CreateAwsLogSource
import Amazonka.SecurityLake.CreateCustomLogSource
import Amazonka.SecurityLake.CreateDataLake
import Amazonka.SecurityLake.CreateDataLakeExceptionSubscription
import Amazonka.SecurityLake.CreateDataLakeOrganizationConfiguration
import Amazonka.SecurityLake.CreateSubscriber
import Amazonka.SecurityLake.CreateSubscriberNotification
import Amazonka.SecurityLake.DeleteAwsLogSource
import Amazonka.SecurityLake.DeleteCustomLogSource
import Amazonka.SecurityLake.DeleteDataLake
import Amazonka.SecurityLake.DeleteDataLakeExceptionSubscription
import Amazonka.SecurityLake.DeleteDataLakeOrganizationConfiguration
import Amazonka.SecurityLake.DeleteSubscriber
import Amazonka.SecurityLake.DeleteSubscriberNotification
import Amazonka.SecurityLake.DeregisterDataLakeDelegatedAdministrator
import Amazonka.SecurityLake.GetDataLakeExceptionSubscription
import Amazonka.SecurityLake.GetDataLakeOrganizationConfiguration
import Amazonka.SecurityLake.GetDataLakeSources
import Amazonka.SecurityLake.GetSubscriber
import Amazonka.SecurityLake.Lens
import Amazonka.SecurityLake.ListDataLakeExceptions
import Amazonka.SecurityLake.ListDataLakes
import Amazonka.SecurityLake.ListLogSources
import Amazonka.SecurityLake.ListSubscribers
import Amazonka.SecurityLake.RegisterDataLakeDelegatedAdministrator
import Amazonka.SecurityLake.Types
import Amazonka.SecurityLake.UpdateDataLake
import Amazonka.SecurityLake.UpdateDataLakeExceptionSubscription
import Amazonka.SecurityLake.UpdateSubscriber
import Amazonka.SecurityLake.UpdateSubscriberNotification
import Amazonka.SecurityLake.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SecurityLake'.

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
