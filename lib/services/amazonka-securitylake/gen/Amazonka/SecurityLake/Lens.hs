{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityLake.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Lens
  ( -- * Operations

    -- ** CreateAwsLogSource
    createAwsLogSource_enableAllDimensions,
    createAwsLogSource_enableSingleDimension,
    createAwsLogSource_enableTwoDimensions,
    createAwsLogSource_inputOrder,
    createAwsLogSourceResponse_failed,
    createAwsLogSourceResponse_processing,
    createAwsLogSourceResponse_httpStatus,

    -- ** CreateCustomLogSource
    createCustomLogSource_customSourceName,
    createCustomLogSource_eventClass,
    createCustomLogSource_glueInvocationRoleArn,
    createCustomLogSource_logProviderAccountId,
    createCustomLogSourceResponse_httpStatus,
    createCustomLogSourceResponse_customDataLocation,
    createCustomLogSourceResponse_glueCrawlerName,
    createCustomLogSourceResponse_glueDatabaseName,
    createCustomLogSourceResponse_glueTableName,
    createCustomLogSourceResponse_logProviderAccessRoleArn,

    -- ** CreateDatalake
    createDatalake_configurations,
    createDatalake_enableAll,
    createDatalake_metaStoreManagerRoleArn,
    createDatalake_regions,
    createDatalakeResponse_httpStatus,

    -- ** CreateDatalakeAutoEnable
    createDatalakeAutoEnable_configurationForNewAccounts,
    createDatalakeAutoEnableResponse_httpStatus,

    -- ** CreateDatalakeDelegatedAdmin
    createDatalakeDelegatedAdmin_account,
    createDatalakeDelegatedAdminResponse_httpStatus,

    -- ** CreateDatalakeExceptionsSubscription
    createDatalakeExceptionsSubscription_notificationEndpoint,
    createDatalakeExceptionsSubscription_subscriptionProtocol,
    createDatalakeExceptionsSubscriptionResponse_httpStatus,

    -- ** CreateSubscriber
    createSubscriber_accessTypes,
    createSubscriber_subscriberDescription,
    createSubscriber_accountId,
    createSubscriber_externalId,
    createSubscriber_sourceTypes,
    createSubscriber_subscriberName,
    createSubscriberResponse_roleArn,
    createSubscriberResponse_s3BucketArn,
    createSubscriberResponse_snsArn,
    createSubscriberResponse_httpStatus,
    createSubscriberResponse_subscriptionId,

    -- ** CreateSubscriptionNotificationConfiguration
    createSubscriptionNotificationConfiguration_createSqs,
    createSubscriptionNotificationConfiguration_httpsApiKeyName,
    createSubscriptionNotificationConfiguration_httpsApiKeyValue,
    createSubscriptionNotificationConfiguration_httpsMethod,
    createSubscriptionNotificationConfiguration_roleArn,
    createSubscriptionNotificationConfiguration_subscriptionEndpoint,
    createSubscriptionNotificationConfiguration_subscriptionId,
    createSubscriptionNotificationConfigurationResponse_queueArn,
    createSubscriptionNotificationConfigurationResponse_httpStatus,

    -- ** DeleteAwsLogSource
    deleteAwsLogSource_disableAllDimensions,
    deleteAwsLogSource_disableSingleDimension,
    deleteAwsLogSource_disableTwoDimensions,
    deleteAwsLogSource_inputOrder,
    deleteAwsLogSourceResponse_failed,
    deleteAwsLogSourceResponse_processing,
    deleteAwsLogSourceResponse_httpStatus,

    -- ** DeleteCustomLogSource
    deleteCustomLogSource_customSourceName,
    deleteCustomLogSourceResponse_httpStatus,
    deleteCustomLogSourceResponse_customDataLocation,

    -- ** DeleteDatalake
    deleteDatalakeResponse_httpStatus,

    -- ** DeleteDatalakeAutoEnable
    deleteDatalakeAutoEnable_removeFromConfigurationForNewAccounts,
    deleteDatalakeAutoEnableResponse_httpStatus,

    -- ** DeleteDatalakeDelegatedAdmin
    deleteDatalakeDelegatedAdmin_account,
    deleteDatalakeDelegatedAdminResponse_httpStatus,

    -- ** DeleteDatalakeExceptionsSubscription
    deleteDatalakeExceptionsSubscriptionResponse_httpStatus,
    deleteDatalakeExceptionsSubscriptionResponse_status,

    -- ** DeleteSubscriber
    deleteSubscriber_id,
    deleteSubscriberResponse_httpStatus,

    -- ** DeleteSubscriptionNotificationConfiguration
    deleteSubscriptionNotificationConfiguration_subscriptionId,
    deleteSubscriptionNotificationConfigurationResponse_httpStatus,

    -- ** GetDatalake
    getDatalakeResponse_httpStatus,
    getDatalakeResponse_configurations,

    -- ** GetDatalakeAutoEnable
    getDatalakeAutoEnableResponse_httpStatus,
    getDatalakeAutoEnableResponse_autoEnableNewAccounts,

    -- ** GetDatalakeExceptionsExpiry
    getDatalakeExceptionsExpiryResponse_httpStatus,
    getDatalakeExceptionsExpiryResponse_exceptionMessageExpiry,

    -- ** GetDatalakeExceptionsSubscription
    getDatalakeExceptionsSubscriptionResponse_httpStatus,
    getDatalakeExceptionsSubscriptionResponse_protocolAndNotificationEndpoint,

    -- ** GetDatalakeStatus
    getDatalakeStatus_accountSet,
    getDatalakeStatus_maxAccountResults,
    getDatalakeStatus_nextToken,
    getDatalakeStatusResponse_nextToken,
    getDatalakeStatusResponse_httpStatus,
    getDatalakeStatusResponse_accountSourcesList,

    -- ** GetSubscriber
    getSubscriber_id,
    getSubscriberResponse_subscriber,
    getSubscriberResponse_httpStatus,

    -- ** ListDatalakeExceptions
    listDatalakeExceptions_maxFailures,
    listDatalakeExceptions_nextToken,
    listDatalakeExceptions_regionSet,
    listDatalakeExceptionsResponse_nextToken,
    listDatalakeExceptionsResponse_httpStatus,
    listDatalakeExceptionsResponse_nonRetryableFailures,

    -- ** ListLogSources
    listLogSources_inputOrder,
    listLogSources_listAllDimensions,
    listLogSources_listSingleDimension,
    listLogSources_listTwoDimensions,
    listLogSources_maxResults,
    listLogSources_nextToken,
    listLogSourcesResponse_nextToken,
    listLogSourcesResponse_httpStatus,
    listLogSourcesResponse_regionSourceTypesAccountsList,

    -- ** ListSubscribers
    listSubscribers_maxResults,
    listSubscribers_nextToken,
    listSubscribersResponse_nextToken,
    listSubscribersResponse_httpStatus,
    listSubscribersResponse_subscribers,

    -- ** UpdateDatalake
    updateDatalake_configurations,
    updateDatalakeResponse_httpStatus,

    -- ** UpdateDatalakeExceptionsExpiry
    updateDatalakeExceptionsExpiry_exceptionMessageExpiry,
    updateDatalakeExceptionsExpiryResponse_httpStatus,

    -- ** UpdateDatalakeExceptionsSubscription
    updateDatalakeExceptionsSubscription_notificationEndpoint,
    updateDatalakeExceptionsSubscription_subscriptionProtocol,
    updateDatalakeExceptionsSubscriptionResponse_httpStatus,

    -- ** UpdateSubscriber
    updateSubscriber_externalId,
    updateSubscriber_sourceTypes,
    updateSubscriber_subscriberDescription,
    updateSubscriber_subscriberName,
    updateSubscriber_id,
    updateSubscriberResponse_subscriber,
    updateSubscriberResponse_httpStatus,

    -- ** UpdateSubscriptionNotificationConfiguration
    updateSubscriptionNotificationConfiguration_createSqs,
    updateSubscriptionNotificationConfiguration_httpsApiKeyName,
    updateSubscriptionNotificationConfiguration_httpsApiKeyValue,
    updateSubscriptionNotificationConfiguration_httpsMethod,
    updateSubscriptionNotificationConfiguration_roleArn,
    updateSubscriptionNotificationConfiguration_subscriptionEndpoint,
    updateSubscriptionNotificationConfiguration_subscriptionId,
    updateSubscriptionNotificationConfigurationResponse_queueArn,
    updateSubscriptionNotificationConfigurationResponse_httpStatus,

    -- * Types

    -- ** AccountSources
    accountSources_eventClass,
    accountSources_logsStatus,
    accountSources_account,
    accountSources_sourceType,

    -- ** AutoEnableNewRegionConfiguration
    autoEnableNewRegionConfiguration_region,
    autoEnableNewRegionConfiguration_sources,

    -- ** Failures
    failures_exceptionMessage,
    failures_remediation,
    failures_timestamp,

    -- ** FailuresResponse
    failuresResponse_failures,
    failuresResponse_region,

    -- ** LakeConfigurationRequest
    lakeConfigurationRequest_encryptionKey,
    lakeConfigurationRequest_replicationDestinationRegions,
    lakeConfigurationRequest_replicationRoleArn,
    lakeConfigurationRequest_retentionSettings,
    lakeConfigurationRequest_tagsMap,

    -- ** LakeConfigurationResponse
    lakeConfigurationResponse_encryptionKey,
    lakeConfigurationResponse_replicationDestinationRegions,
    lakeConfigurationResponse_replicationRoleArn,
    lakeConfigurationResponse_retentionSettings,
    lakeConfigurationResponse_s3BucketArn,
    lakeConfigurationResponse_status,
    lakeConfigurationResponse_tagsMap,

    -- ** LogsStatus
    logsStatus_healthStatus,
    logsStatus_pathToLogs,

    -- ** ProtocolAndNotificationEndpoint
    protocolAndNotificationEndpoint_endpoint,
    protocolAndNotificationEndpoint_protocol,

    -- ** RetentionSetting
    retentionSetting_retentionPeriod,
    retentionSetting_storageClass,

    -- ** SourceType
    sourceType_awsSourceType,
    sourceType_customSourceType,

    -- ** SubscriberResource
    subscriberResource_accessTypes,
    subscriberResource_createdAt,
    subscriberResource_externalId,
    subscriberResource_roleArn,
    subscriberResource_s3BucketArn,
    subscriberResource_snsArn,
    subscriberResource_subscriberDescription,
    subscriberResource_subscriberName,
    subscriberResource_subscriptionEndpoint,
    subscriberResource_subscriptionProtocol,
    subscriberResource_subscriptionStatus,
    subscriberResource_updatedAt,
    subscriberResource_accountId,
    subscriberResource_sourceTypes,
    subscriberResource_subscriptionId,
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
import Amazonka.SecurityLake.ListDatalakeExceptions
import Amazonka.SecurityLake.ListLogSources
import Amazonka.SecurityLake.ListSubscribers
import Amazonka.SecurityLake.Types.AccountSources
import Amazonka.SecurityLake.Types.AutoEnableNewRegionConfiguration
import Amazonka.SecurityLake.Types.Failures
import Amazonka.SecurityLake.Types.FailuresResponse
import Amazonka.SecurityLake.Types.LakeConfigurationRequest
import Amazonka.SecurityLake.Types.LakeConfigurationResponse
import Amazonka.SecurityLake.Types.LogsStatus
import Amazonka.SecurityLake.Types.ProtocolAndNotificationEndpoint
import Amazonka.SecurityLake.Types.RetentionSetting
import Amazonka.SecurityLake.Types.SourceType
import Amazonka.SecurityLake.Types.SubscriberResource
import Amazonka.SecurityLake.UpdateDatalake
import Amazonka.SecurityLake.UpdateDatalakeExceptionsExpiry
import Amazonka.SecurityLake.UpdateDatalakeExceptionsSubscription
import Amazonka.SecurityLake.UpdateSubscriber
import Amazonka.SecurityLake.UpdateSubscriptionNotificationConfiguration
