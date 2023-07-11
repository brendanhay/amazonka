{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SecurityLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Security Lake is in preview release. Your use of the Security
-- Lake preview is subject to Section 2 of the
-- <http://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>(\"Betas
-- and Previews\").
--
-- Amazon Security Lake is a fully managed security data lake service. You
-- can use Security Lake to automatically centralize security data from
-- cloud, on-premises, and custom sources into a data lake that\'s stored
-- in your Amazon Web Servicesaccount. Amazon Web Services Organizations is
-- an account management service that lets you consolidate multiple Amazon
-- Web Services accounts into an organization that you create and centrally
-- manage. With Organizations, you can create member accounts and invite
-- existing accounts to join your organization. Security Lake helps you
-- analyze security data for a more complete understanding of your security
-- posture across the entire organization. It can also help you improve the
-- protection of your workloads, applications, and data.
--
-- The data lake is backed by Amazon Simple Storage Service (Amazon S3)
-- buckets, and you retain ownership over your data.
--
-- Amazon Security Lake integrates with CloudTrail, a service that provides
-- a record of actions taken by a user, role, or an Amazon Web Services
-- service in Security Lake CloudTrail captures API calls for Security Lake
-- as events. The calls captured include calls from the Security Lake
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

    -- ** AccountNotFoundException
    _AccountNotFoundException,

    -- ** BucketNotFoundException
    _BucketNotFoundException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ConflictSourceNamesException
    _ConflictSourceNamesException,

    -- ** ConflictSubscriptionException
    _ConflictSubscriptionException,

    -- ** EventBridgeException
    _EventBridgeException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** S3Exception
    _S3Exception,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

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

    -- ** CreateDatalake
    CreateDatalake (CreateDatalake'),
    newCreateDatalake,
    CreateDatalakeResponse (CreateDatalakeResponse'),
    newCreateDatalakeResponse,

    -- ** CreateDatalakeAutoEnable
    CreateDatalakeAutoEnable (CreateDatalakeAutoEnable'),
    newCreateDatalakeAutoEnable,
    CreateDatalakeAutoEnableResponse (CreateDatalakeAutoEnableResponse'),
    newCreateDatalakeAutoEnableResponse,

    -- ** CreateDatalakeDelegatedAdmin
    CreateDatalakeDelegatedAdmin (CreateDatalakeDelegatedAdmin'),
    newCreateDatalakeDelegatedAdmin,
    CreateDatalakeDelegatedAdminResponse (CreateDatalakeDelegatedAdminResponse'),
    newCreateDatalakeDelegatedAdminResponse,

    -- ** CreateDatalakeExceptionsSubscription
    CreateDatalakeExceptionsSubscription (CreateDatalakeExceptionsSubscription'),
    newCreateDatalakeExceptionsSubscription,
    CreateDatalakeExceptionsSubscriptionResponse (CreateDatalakeExceptionsSubscriptionResponse'),
    newCreateDatalakeExceptionsSubscriptionResponse,

    -- ** CreateSubscriber
    CreateSubscriber (CreateSubscriber'),
    newCreateSubscriber,
    CreateSubscriberResponse (CreateSubscriberResponse'),
    newCreateSubscriberResponse,

    -- ** CreateSubscriptionNotificationConfiguration
    CreateSubscriptionNotificationConfiguration (CreateSubscriptionNotificationConfiguration'),
    newCreateSubscriptionNotificationConfiguration,
    CreateSubscriptionNotificationConfigurationResponse (CreateSubscriptionNotificationConfigurationResponse'),
    newCreateSubscriptionNotificationConfigurationResponse,

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

    -- ** DeleteDatalake
    DeleteDatalake (DeleteDatalake'),
    newDeleteDatalake,
    DeleteDatalakeResponse (DeleteDatalakeResponse'),
    newDeleteDatalakeResponse,

    -- ** DeleteDatalakeAutoEnable
    DeleteDatalakeAutoEnable (DeleteDatalakeAutoEnable'),
    newDeleteDatalakeAutoEnable,
    DeleteDatalakeAutoEnableResponse (DeleteDatalakeAutoEnableResponse'),
    newDeleteDatalakeAutoEnableResponse,

    -- ** DeleteDatalakeDelegatedAdmin
    DeleteDatalakeDelegatedAdmin (DeleteDatalakeDelegatedAdmin'),
    newDeleteDatalakeDelegatedAdmin,
    DeleteDatalakeDelegatedAdminResponse (DeleteDatalakeDelegatedAdminResponse'),
    newDeleteDatalakeDelegatedAdminResponse,

    -- ** DeleteDatalakeExceptionsSubscription
    DeleteDatalakeExceptionsSubscription (DeleteDatalakeExceptionsSubscription'),
    newDeleteDatalakeExceptionsSubscription,
    DeleteDatalakeExceptionsSubscriptionResponse (DeleteDatalakeExceptionsSubscriptionResponse'),
    newDeleteDatalakeExceptionsSubscriptionResponse,

    -- ** DeleteSubscriber
    DeleteSubscriber (DeleteSubscriber'),
    newDeleteSubscriber,
    DeleteSubscriberResponse (DeleteSubscriberResponse'),
    newDeleteSubscriberResponse,

    -- ** DeleteSubscriptionNotificationConfiguration
    DeleteSubscriptionNotificationConfiguration (DeleteSubscriptionNotificationConfiguration'),
    newDeleteSubscriptionNotificationConfiguration,
    DeleteSubscriptionNotificationConfigurationResponse (DeleteSubscriptionNotificationConfigurationResponse'),
    newDeleteSubscriptionNotificationConfigurationResponse,

    -- ** GetDatalake
    GetDatalake (GetDatalake'),
    newGetDatalake,
    GetDatalakeResponse (GetDatalakeResponse'),
    newGetDatalakeResponse,

    -- ** GetDatalakeAutoEnable
    GetDatalakeAutoEnable (GetDatalakeAutoEnable'),
    newGetDatalakeAutoEnable,
    GetDatalakeAutoEnableResponse (GetDatalakeAutoEnableResponse'),
    newGetDatalakeAutoEnableResponse,

    -- ** GetDatalakeExceptionsExpiry
    GetDatalakeExceptionsExpiry (GetDatalakeExceptionsExpiry'),
    newGetDatalakeExceptionsExpiry,
    GetDatalakeExceptionsExpiryResponse (GetDatalakeExceptionsExpiryResponse'),
    newGetDatalakeExceptionsExpiryResponse,

    -- ** GetDatalakeExceptionsSubscription
    GetDatalakeExceptionsSubscription (GetDatalakeExceptionsSubscription'),
    newGetDatalakeExceptionsSubscription,
    GetDatalakeExceptionsSubscriptionResponse (GetDatalakeExceptionsSubscriptionResponse'),
    newGetDatalakeExceptionsSubscriptionResponse,

    -- ** GetDatalakeStatus (Paginated)
    GetDatalakeStatus (GetDatalakeStatus'),
    newGetDatalakeStatus,
    GetDatalakeStatusResponse (GetDatalakeStatusResponse'),
    newGetDatalakeStatusResponse,

    -- ** GetSubscriber
    GetSubscriber (GetSubscriber'),
    newGetSubscriber,
    GetSubscriberResponse (GetSubscriberResponse'),
    newGetSubscriberResponse,

    -- ** ListDatalakeExceptions (Paginated)
    ListDatalakeExceptions (ListDatalakeExceptions'),
    newListDatalakeExceptions,
    ListDatalakeExceptionsResponse (ListDatalakeExceptionsResponse'),
    newListDatalakeExceptionsResponse,

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

    -- ** UpdateDatalake
    UpdateDatalake (UpdateDatalake'),
    newUpdateDatalake,
    UpdateDatalakeResponse (UpdateDatalakeResponse'),
    newUpdateDatalakeResponse,

    -- ** UpdateDatalakeExceptionsExpiry
    UpdateDatalakeExceptionsExpiry (UpdateDatalakeExceptionsExpiry'),
    newUpdateDatalakeExceptionsExpiry,
    UpdateDatalakeExceptionsExpiryResponse (UpdateDatalakeExceptionsExpiryResponse'),
    newUpdateDatalakeExceptionsExpiryResponse,

    -- ** UpdateDatalakeExceptionsSubscription
    UpdateDatalakeExceptionsSubscription (UpdateDatalakeExceptionsSubscription'),
    newUpdateDatalakeExceptionsSubscription,
    UpdateDatalakeExceptionsSubscriptionResponse (UpdateDatalakeExceptionsSubscriptionResponse'),
    newUpdateDatalakeExceptionsSubscriptionResponse,

    -- ** UpdateSubscriber
    UpdateSubscriber (UpdateSubscriber'),
    newUpdateSubscriber,
    UpdateSubscriberResponse (UpdateSubscriberResponse'),
    newUpdateSubscriberResponse,

    -- ** UpdateSubscriptionNotificationConfiguration
    UpdateSubscriptionNotificationConfiguration (UpdateSubscriptionNotificationConfiguration'),
    newUpdateSubscriptionNotificationConfiguration,
    UpdateSubscriptionNotificationConfigurationResponse (UpdateSubscriptionNotificationConfigurationResponse'),
    newUpdateSubscriptionNotificationConfigurationResponse,

    -- * Types

    -- ** AccessType
    AccessType (..),

    -- ** AwsLogSourceType
    AwsLogSourceType (..),

    -- ** Dimension
    Dimension (..),

    -- ** EndpointProtocol
    EndpointProtocol (..),

    -- ** HttpsMethod
    HttpsMethod (..),

    -- ** OcsfEventClass
    OcsfEventClass (..),

    -- ** Region
    Region (..),

    -- ** SettingsStatus
    SettingsStatus (..),

    -- ** SourceStatus
    SourceStatus (..),

    -- ** StorageClass
    StorageClass (..),

    -- ** SubscriptionProtocolType
    SubscriptionProtocolType (..),

    -- ** SubscriptionStatus
    SubscriptionStatus (..),

    -- ** AccountSources
    AccountSources (AccountSources'),
    newAccountSources,

    -- ** AutoEnableNewRegionConfiguration
    AutoEnableNewRegionConfiguration (AutoEnableNewRegionConfiguration'),
    newAutoEnableNewRegionConfiguration,

    -- ** Failures
    Failures (Failures'),
    newFailures,

    -- ** FailuresResponse
    FailuresResponse (FailuresResponse'),
    newFailuresResponse,

    -- ** LakeConfigurationRequest
    LakeConfigurationRequest (LakeConfigurationRequest'),
    newLakeConfigurationRequest,

    -- ** LakeConfigurationResponse
    LakeConfigurationResponse (LakeConfigurationResponse'),
    newLakeConfigurationResponse,

    -- ** LogsStatus
    LogsStatus (LogsStatus'),
    newLogsStatus,

    -- ** ProtocolAndNotificationEndpoint
    ProtocolAndNotificationEndpoint (ProtocolAndNotificationEndpoint'),
    newProtocolAndNotificationEndpoint,

    -- ** RetentionSetting
    RetentionSetting (RetentionSetting'),
    newRetentionSetting,

    -- ** SourceType
    SourceType (SourceType'),
    newSourceType,

    -- ** SubscriberResource
    SubscriberResource (SubscriberResource'),
    newSubscriberResource,
  )
where

import Amazonka.SecurityLake.CreateAwsLogSource
import Amazonka.SecurityLake.CreateCustomLogSource
import Amazonka.SecurityLake.CreateDatalake
import Amazonka.SecurityLake.CreateDatalakeAutoEnable
import Amazonka.SecurityLake.CreateDatalakeDelegatedAdmin
import Amazonka.SecurityLake.CreateDatalakeExceptionsSubscription
import Amazonka.SecurityLake.CreateSubscriber
import Amazonka.SecurityLake.CreateSubscriptionNotificationConfiguration
import Amazonka.SecurityLake.DeleteAwsLogSource
import Amazonka.SecurityLake.DeleteCustomLogSource
import Amazonka.SecurityLake.DeleteDatalake
import Amazonka.SecurityLake.DeleteDatalakeAutoEnable
import Amazonka.SecurityLake.DeleteDatalakeDelegatedAdmin
import Amazonka.SecurityLake.DeleteDatalakeExceptionsSubscription
import Amazonka.SecurityLake.DeleteSubscriber
import Amazonka.SecurityLake.DeleteSubscriptionNotificationConfiguration
import Amazonka.SecurityLake.GetDatalake
import Amazonka.SecurityLake.GetDatalakeAutoEnable
import Amazonka.SecurityLake.GetDatalakeExceptionsExpiry
import Amazonka.SecurityLake.GetDatalakeExceptionsSubscription
import Amazonka.SecurityLake.GetDatalakeStatus
import Amazonka.SecurityLake.GetSubscriber
import Amazonka.SecurityLake.Lens
import Amazonka.SecurityLake.ListDatalakeExceptions
import Amazonka.SecurityLake.ListLogSources
import Amazonka.SecurityLake.ListSubscribers
import Amazonka.SecurityLake.Types
import Amazonka.SecurityLake.UpdateDatalake
import Amazonka.SecurityLake.UpdateDatalakeExceptionsExpiry
import Amazonka.SecurityLake.UpdateDatalakeExceptionsSubscription
import Amazonka.SecurityLake.UpdateSubscriber
import Amazonka.SecurityLake.UpdateSubscriptionNotificationConfiguration
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
