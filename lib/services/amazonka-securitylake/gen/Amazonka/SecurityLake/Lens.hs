{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityLake.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Lens
  ( -- * Operations

    -- ** CreateAwsLogSource
    createAwsLogSource_sources,
    createAwsLogSourceResponse_failed,
    createAwsLogSourceResponse_httpStatus,

    -- ** CreateCustomLogSource
    createCustomLogSource_configuration,
    createCustomLogSource_eventClasses,
    createCustomLogSource_sourceVersion,
    createCustomLogSource_sourceName,
    createCustomLogSourceResponse_source,
    createCustomLogSourceResponse_httpStatus,

    -- ** CreateDataLake
    createDataLake_configurations,
    createDataLake_metaStoreManagerRoleArn,
    createDataLakeResponse_dataLakes,
    createDataLakeResponse_httpStatus,

    -- ** CreateDataLakeExceptionSubscription
    createDataLakeExceptionSubscription_exceptionTimeToLive,
    createDataLakeExceptionSubscription_notificationEndpoint,
    createDataLakeExceptionSubscription_subscriptionProtocol,
    createDataLakeExceptionSubscriptionResponse_httpStatus,

    -- ** CreateDataLakeOrganizationConfiguration
    createDataLakeOrganizationConfiguration_autoEnableNewAccount,
    createDataLakeOrganizationConfigurationResponse_httpStatus,

    -- ** CreateSubscriber
    createSubscriber_accessTypes,
    createSubscriber_subscriberDescription,
    createSubscriber_sources,
    createSubscriber_subscriberIdentity,
    createSubscriber_subscriberName,
    createSubscriberResponse_subscriber,
    createSubscriberResponse_httpStatus,

    -- ** CreateSubscriberNotification
    createSubscriberNotification_configuration,
    createSubscriberNotification_subscriberId,
    createSubscriberNotificationResponse_subscriberEndpoint,
    createSubscriberNotificationResponse_httpStatus,

    -- ** DeleteAwsLogSource
    deleteAwsLogSource_sources,
    deleteAwsLogSourceResponse_failed,
    deleteAwsLogSourceResponse_httpStatus,

    -- ** DeleteCustomLogSource
    deleteCustomLogSource_sourceVersion,
    deleteCustomLogSource_sourceName,
    deleteCustomLogSourceResponse_httpStatus,

    -- ** DeleteDataLake
    deleteDataLake_regions,
    deleteDataLakeResponse_httpStatus,

    -- ** DeleteDataLakeExceptionSubscription
    deleteDataLakeExceptionSubscriptionResponse_httpStatus,

    -- ** DeleteDataLakeOrganizationConfiguration
    deleteDataLakeOrganizationConfiguration_autoEnableNewAccount,
    deleteDataLakeOrganizationConfigurationResponse_httpStatus,

    -- ** DeleteSubscriber
    deleteSubscriber_subscriberId,
    deleteSubscriberResponse_httpStatus,

    -- ** DeleteSubscriberNotification
    deleteSubscriberNotification_subscriberId,
    deleteSubscriberNotificationResponse_httpStatus,

    -- ** DeregisterDataLakeDelegatedAdministrator
    deregisterDataLakeDelegatedAdministratorResponse_httpStatus,

    -- ** GetDataLakeExceptionSubscription
    getDataLakeExceptionSubscriptionResponse_exceptionTimeToLive,
    getDataLakeExceptionSubscriptionResponse_notificationEndpoint,
    getDataLakeExceptionSubscriptionResponse_subscriptionProtocol,
    getDataLakeExceptionSubscriptionResponse_httpStatus,

    -- ** GetDataLakeOrganizationConfiguration
    getDataLakeOrganizationConfigurationResponse_autoEnableNewAccount,
    getDataLakeOrganizationConfigurationResponse_httpStatus,

    -- ** GetDataLakeSources
    getDataLakeSources_accounts,
    getDataLakeSources_maxResults,
    getDataLakeSources_nextToken,
    getDataLakeSourcesResponse_dataLakeArn,
    getDataLakeSourcesResponse_dataLakeSources,
    getDataLakeSourcesResponse_nextToken,
    getDataLakeSourcesResponse_httpStatus,

    -- ** GetSubscriber
    getSubscriber_subscriberId,
    getSubscriberResponse_subscriber,
    getSubscriberResponse_httpStatus,

    -- ** ListDataLakeExceptions
    listDataLakeExceptions_maxResults,
    listDataLakeExceptions_nextToken,
    listDataLakeExceptions_regions,
    listDataLakeExceptionsResponse_exceptions,
    listDataLakeExceptionsResponse_nextToken,
    listDataLakeExceptionsResponse_httpStatus,

    -- ** ListDataLakes
    listDataLakes_regions,
    listDataLakesResponse_dataLakes,
    listDataLakesResponse_httpStatus,

    -- ** ListLogSources
    listLogSources_accounts,
    listLogSources_maxResults,
    listLogSources_nextToken,
    listLogSources_regions,
    listLogSources_sources,
    listLogSourcesResponse_nextToken,
    listLogSourcesResponse_sources,
    listLogSourcesResponse_httpStatus,

    -- ** ListSubscribers
    listSubscribers_maxResults,
    listSubscribers_nextToken,
    listSubscribersResponse_nextToken,
    listSubscribersResponse_subscribers,
    listSubscribersResponse_httpStatus,

    -- ** RegisterDataLakeDelegatedAdministrator
    registerDataLakeDelegatedAdministrator_accountId,
    registerDataLakeDelegatedAdministratorResponse_httpStatus,

    -- ** UpdateDataLake
    updateDataLake_configurations,
    updateDataLakeResponse_dataLakes,
    updateDataLakeResponse_httpStatus,

    -- ** UpdateDataLakeExceptionSubscription
    updateDataLakeExceptionSubscription_exceptionTimeToLive,
    updateDataLakeExceptionSubscription_notificationEndpoint,
    updateDataLakeExceptionSubscription_subscriptionProtocol,
    updateDataLakeExceptionSubscriptionResponse_httpStatus,

    -- ** UpdateSubscriber
    updateSubscriber_sources,
    updateSubscriber_subscriberDescription,
    updateSubscriber_subscriberIdentity,
    updateSubscriber_subscriberName,
    updateSubscriber_subscriberId,
    updateSubscriberResponse_subscriber,
    updateSubscriberResponse_httpStatus,

    -- ** UpdateSubscriberNotification
    updateSubscriberNotification_configuration,
    updateSubscriberNotification_subscriberId,
    updateSubscriberNotificationResponse_subscriberEndpoint,
    updateSubscriberNotificationResponse_httpStatus,

    -- * Types

    -- ** AwsIdentity
    awsIdentity_externalId,
    awsIdentity_principal,

    -- ** AwsLogSourceConfiguration
    awsLogSourceConfiguration_accounts,
    awsLogSourceConfiguration_sourceVersion,
    awsLogSourceConfiguration_regions,
    awsLogSourceConfiguration_sourceName,

    -- ** AwsLogSourceResource
    awsLogSourceResource_sourceName,
    awsLogSourceResource_sourceVersion,

    -- ** CustomLogSourceAttributes
    customLogSourceAttributes_crawlerArn,
    customLogSourceAttributes_databaseArn,
    customLogSourceAttributes_tableArn,

    -- ** CustomLogSourceConfiguration
    customLogSourceConfiguration_crawlerConfiguration,
    customLogSourceConfiguration_providerIdentity,

    -- ** CustomLogSourceCrawlerConfiguration
    customLogSourceCrawlerConfiguration_roleArn,

    -- ** CustomLogSourceProvider
    customLogSourceProvider_location,
    customLogSourceProvider_roleArn,

    -- ** CustomLogSourceResource
    customLogSourceResource_attributes,
    customLogSourceResource_provider,
    customLogSourceResource_sourceName,
    customLogSourceResource_sourceVersion,

    -- ** DataLakeAutoEnableNewAccountConfiguration
    dataLakeAutoEnableNewAccountConfiguration_region,
    dataLakeAutoEnableNewAccountConfiguration_sources,

    -- ** DataLakeConfiguration
    dataLakeConfiguration_encryptionConfiguration,
    dataLakeConfiguration_lifecycleConfiguration,
    dataLakeConfiguration_replicationConfiguration,
    dataLakeConfiguration_region,

    -- ** DataLakeEncryptionConfiguration
    dataLakeEncryptionConfiguration_kmsKeyId,

    -- ** DataLakeException
    dataLakeException_exception,
    dataLakeException_region,
    dataLakeException_remediation,
    dataLakeException_timestamp,

    -- ** DataLakeLifecycleConfiguration
    dataLakeLifecycleConfiguration_expiration,
    dataLakeLifecycleConfiguration_transitions,

    -- ** DataLakeLifecycleExpiration
    dataLakeLifecycleExpiration_days,

    -- ** DataLakeLifecycleTransition
    dataLakeLifecycleTransition_days,
    dataLakeLifecycleTransition_storageClass,

    -- ** DataLakeReplicationConfiguration
    dataLakeReplicationConfiguration_regions,
    dataLakeReplicationConfiguration_roleArn,

    -- ** DataLakeResource
    dataLakeResource_createStatus,
    dataLakeResource_encryptionConfiguration,
    dataLakeResource_lifecycleConfiguration,
    dataLakeResource_replicationConfiguration,
    dataLakeResource_s3BucketArn,
    dataLakeResource_updateStatus,
    dataLakeResource_dataLakeArn,
    dataLakeResource_region,

    -- ** DataLakeSource
    dataLakeSource_account,
    dataLakeSource_eventClasses,
    dataLakeSource_sourceName,
    dataLakeSource_sourceStatuses,

    -- ** DataLakeSourceStatus
    dataLakeSourceStatus_resource,
    dataLakeSourceStatus_status,

    -- ** DataLakeUpdateException
    dataLakeUpdateException_code,
    dataLakeUpdateException_reason,

    -- ** DataLakeUpdateStatus
    dataLakeUpdateStatus_exception,
    dataLakeUpdateStatus_requestId,
    dataLakeUpdateStatus_status,

    -- ** HttpsNotificationConfiguration
    httpsNotificationConfiguration_authorizationApiKeyName,
    httpsNotificationConfiguration_authorizationApiKeyValue,
    httpsNotificationConfiguration_httpMethod,
    httpsNotificationConfiguration_endpoint,
    httpsNotificationConfiguration_targetRoleArn,

    -- ** LogSource
    logSource_account,
    logSource_region,
    logSource_sources,

    -- ** LogSourceResource
    logSourceResource_awsLogSource,
    logSourceResource_customLogSource,

    -- ** NotificationConfiguration
    notificationConfiguration_httpsNotificationConfiguration,
    notificationConfiguration_sqsNotificationConfiguration,

    -- ** SqsNotificationConfiguration

    -- ** SubscriberResource
    subscriberResource_accessTypes,
    subscriberResource_createdAt,
    subscriberResource_resourceShareArn,
    subscriberResource_resourceShareName,
    subscriberResource_roleArn,
    subscriberResource_s3BucketArn,
    subscriberResource_subscriberDescription,
    subscriberResource_subscriberEndpoint,
    subscriberResource_subscriberStatus,
    subscriberResource_updatedAt,
    subscriberResource_sources,
    subscriberResource_subscriberArn,
    subscriberResource_subscriberId,
    subscriberResource_subscriberIdentity,
    subscriberResource_subscriberName,
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
import Amazonka.SecurityLake.ListDataLakeExceptions
import Amazonka.SecurityLake.ListDataLakes
import Amazonka.SecurityLake.ListLogSources
import Amazonka.SecurityLake.ListSubscribers
import Amazonka.SecurityLake.RegisterDataLakeDelegatedAdministrator
import Amazonka.SecurityLake.Types.AwsIdentity
import Amazonka.SecurityLake.Types.AwsLogSourceConfiguration
import Amazonka.SecurityLake.Types.AwsLogSourceResource
import Amazonka.SecurityLake.Types.CustomLogSourceAttributes
import Amazonka.SecurityLake.Types.CustomLogSourceConfiguration
import Amazonka.SecurityLake.Types.CustomLogSourceCrawlerConfiguration
import Amazonka.SecurityLake.Types.CustomLogSourceProvider
import Amazonka.SecurityLake.Types.CustomLogSourceResource
import Amazonka.SecurityLake.Types.DataLakeAutoEnableNewAccountConfiguration
import Amazonka.SecurityLake.Types.DataLakeConfiguration
import Amazonka.SecurityLake.Types.DataLakeEncryptionConfiguration
import Amazonka.SecurityLake.Types.DataLakeException
import Amazonka.SecurityLake.Types.DataLakeLifecycleConfiguration
import Amazonka.SecurityLake.Types.DataLakeLifecycleExpiration
import Amazonka.SecurityLake.Types.DataLakeLifecycleTransition
import Amazonka.SecurityLake.Types.DataLakeReplicationConfiguration
import Amazonka.SecurityLake.Types.DataLakeResource
import Amazonka.SecurityLake.Types.DataLakeSource
import Amazonka.SecurityLake.Types.DataLakeSourceStatus
import Amazonka.SecurityLake.Types.DataLakeUpdateException
import Amazonka.SecurityLake.Types.DataLakeUpdateStatus
import Amazonka.SecurityLake.Types.HttpsNotificationConfiguration
import Amazonka.SecurityLake.Types.LogSource
import Amazonka.SecurityLake.Types.LogSourceResource
import Amazonka.SecurityLake.Types.NotificationConfiguration
import Amazonka.SecurityLake.Types.SqsNotificationConfiguration
import Amazonka.SecurityLake.Types.SubscriberResource
import Amazonka.SecurityLake.UpdateDataLake
import Amazonka.SecurityLake.UpdateDataLakeExceptionSubscription
import Amazonka.SecurityLake.UpdateSubscriber
import Amazonka.SecurityLake.UpdateSubscriberNotification
