{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityLake.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BadRequestException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,

    -- * AccessType
    AccessType (..),

    -- * AwsLogSourceName
    AwsLogSourceName (..),

    -- * DataLakeStatus
    DataLakeStatus (..),

    -- * HttpMethod
    HttpMethod (..),

    -- * SourceCollectionStatus
    SourceCollectionStatus (..),

    -- * SubscriberStatus
    SubscriberStatus (..),

    -- * AwsIdentity
    AwsIdentity (..),
    newAwsIdentity,
    awsIdentity_externalId,
    awsIdentity_principal,

    -- * AwsLogSourceConfiguration
    AwsLogSourceConfiguration (..),
    newAwsLogSourceConfiguration,
    awsLogSourceConfiguration_accounts,
    awsLogSourceConfiguration_sourceVersion,
    awsLogSourceConfiguration_regions,
    awsLogSourceConfiguration_sourceName,

    -- * AwsLogSourceResource
    AwsLogSourceResource (..),
    newAwsLogSourceResource,
    awsLogSourceResource_sourceName,
    awsLogSourceResource_sourceVersion,

    -- * CustomLogSourceAttributes
    CustomLogSourceAttributes (..),
    newCustomLogSourceAttributes,
    customLogSourceAttributes_crawlerArn,
    customLogSourceAttributes_databaseArn,
    customLogSourceAttributes_tableArn,

    -- * CustomLogSourceConfiguration
    CustomLogSourceConfiguration (..),
    newCustomLogSourceConfiguration,
    customLogSourceConfiguration_crawlerConfiguration,
    customLogSourceConfiguration_providerIdentity,

    -- * CustomLogSourceCrawlerConfiguration
    CustomLogSourceCrawlerConfiguration (..),
    newCustomLogSourceCrawlerConfiguration,
    customLogSourceCrawlerConfiguration_roleArn,

    -- * CustomLogSourceProvider
    CustomLogSourceProvider (..),
    newCustomLogSourceProvider,
    customLogSourceProvider_location,
    customLogSourceProvider_roleArn,

    -- * CustomLogSourceResource
    CustomLogSourceResource (..),
    newCustomLogSourceResource,
    customLogSourceResource_attributes,
    customLogSourceResource_provider,
    customLogSourceResource_sourceName,
    customLogSourceResource_sourceVersion,

    -- * DataLakeAutoEnableNewAccountConfiguration
    DataLakeAutoEnableNewAccountConfiguration (..),
    newDataLakeAutoEnableNewAccountConfiguration,
    dataLakeAutoEnableNewAccountConfiguration_region,
    dataLakeAutoEnableNewAccountConfiguration_sources,

    -- * DataLakeConfiguration
    DataLakeConfiguration (..),
    newDataLakeConfiguration,
    dataLakeConfiguration_encryptionConfiguration,
    dataLakeConfiguration_lifecycleConfiguration,
    dataLakeConfiguration_replicationConfiguration,
    dataLakeConfiguration_region,

    -- * DataLakeEncryptionConfiguration
    DataLakeEncryptionConfiguration (..),
    newDataLakeEncryptionConfiguration,
    dataLakeEncryptionConfiguration_kmsKeyId,

    -- * DataLakeException
    DataLakeException (..),
    newDataLakeException,
    dataLakeException_exception,
    dataLakeException_region,
    dataLakeException_remediation,
    dataLakeException_timestamp,

    -- * DataLakeLifecycleConfiguration
    DataLakeLifecycleConfiguration (..),
    newDataLakeLifecycleConfiguration,
    dataLakeLifecycleConfiguration_expiration,
    dataLakeLifecycleConfiguration_transitions,

    -- * DataLakeLifecycleExpiration
    DataLakeLifecycleExpiration (..),
    newDataLakeLifecycleExpiration,
    dataLakeLifecycleExpiration_days,

    -- * DataLakeLifecycleTransition
    DataLakeLifecycleTransition (..),
    newDataLakeLifecycleTransition,
    dataLakeLifecycleTransition_days,
    dataLakeLifecycleTransition_storageClass,

    -- * DataLakeReplicationConfiguration
    DataLakeReplicationConfiguration (..),
    newDataLakeReplicationConfiguration,
    dataLakeReplicationConfiguration_regions,
    dataLakeReplicationConfiguration_roleArn,

    -- * DataLakeResource
    DataLakeResource (..),
    newDataLakeResource,
    dataLakeResource_createStatus,
    dataLakeResource_encryptionConfiguration,
    dataLakeResource_lifecycleConfiguration,
    dataLakeResource_replicationConfiguration,
    dataLakeResource_s3BucketArn,
    dataLakeResource_updateStatus,
    dataLakeResource_dataLakeArn,
    dataLakeResource_region,

    -- * DataLakeSource
    DataLakeSource (..),
    newDataLakeSource,
    dataLakeSource_account,
    dataLakeSource_eventClasses,
    dataLakeSource_sourceName,
    dataLakeSource_sourceStatuses,

    -- * DataLakeSourceStatus
    DataLakeSourceStatus (..),
    newDataLakeSourceStatus,
    dataLakeSourceStatus_resource,
    dataLakeSourceStatus_status,

    -- * DataLakeUpdateException
    DataLakeUpdateException (..),
    newDataLakeUpdateException,
    dataLakeUpdateException_code,
    dataLakeUpdateException_reason,

    -- * DataLakeUpdateStatus
    DataLakeUpdateStatus (..),
    newDataLakeUpdateStatus,
    dataLakeUpdateStatus_exception,
    dataLakeUpdateStatus_requestId,
    dataLakeUpdateStatus_status,

    -- * HttpsNotificationConfiguration
    HttpsNotificationConfiguration (..),
    newHttpsNotificationConfiguration,
    httpsNotificationConfiguration_authorizationApiKeyName,
    httpsNotificationConfiguration_authorizationApiKeyValue,
    httpsNotificationConfiguration_httpMethod,
    httpsNotificationConfiguration_endpoint,
    httpsNotificationConfiguration_targetRoleArn,

    -- * LogSource
    LogSource (..),
    newLogSource,
    logSource_account,
    logSource_region,
    logSource_sources,

    -- * LogSourceResource
    LogSourceResource (..),
    newLogSourceResource,
    logSourceResource_awsLogSource,
    logSourceResource_customLogSource,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_httpsNotificationConfiguration,
    notificationConfiguration_sqsNotificationConfiguration,

    -- * SqsNotificationConfiguration
    SqsNotificationConfiguration (..),
    newSqsNotificationConfiguration,

    -- * SubscriberResource
    SubscriberResource (..),
    newSubscriberResource,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AccessType
import Amazonka.SecurityLake.Types.AwsIdentity
import Amazonka.SecurityLake.Types.AwsLogSourceConfiguration
import Amazonka.SecurityLake.Types.AwsLogSourceName
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
import Amazonka.SecurityLake.Types.DataLakeStatus
import Amazonka.SecurityLake.Types.DataLakeUpdateException
import Amazonka.SecurityLake.Types.DataLakeUpdateStatus
import Amazonka.SecurityLake.Types.HttpMethod
import Amazonka.SecurityLake.Types.HttpsNotificationConfiguration
import Amazonka.SecurityLake.Types.LogSource
import Amazonka.SecurityLake.Types.LogSourceResource
import Amazonka.SecurityLake.Types.NotificationConfiguration
import Amazonka.SecurityLake.Types.SourceCollectionStatus
import Amazonka.SecurityLake.Types.SqsNotificationConfiguration
import Amazonka.SecurityLake.Types.SubscriberResource
import Amazonka.SecurityLake.Types.SubscriberStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon Security Lake SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SecurityLake",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "securitylake",
      Core.signingName = "securitylake",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SecurityLake",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action. Access denied
-- errors appear when Amazon Security Lake explicitly or implicitly denies
-- an authorization request. An explicit denial occurs when a policy
-- contains a Deny statement for the specific Amazon Web Services action.
-- An implicit denial occurs when there is no applicable Deny statement and
-- also no applicable Allow statement.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request is malformed or contains an error such as an invalid
-- parameter value or a missing required parameter.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Occurs when a conflict with a previous successful write is detected.
-- This generally occurs when the previous write did not have time to
-- propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Internal service exceptions are sometimes caused by transient issues.
-- Before you start troubleshooting, perform the operation again.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429
