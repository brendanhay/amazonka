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
    _AccountNotFoundException,
    _BucketNotFoundException,
    _ConcurrentModificationException,
    _ConflictException,
    _ConflictSourceNamesException,
    _ConflictSubscriptionException,
    _EventBridgeException,
    _InternalServerException,
    _InvalidInputException,
    _ResourceNotFoundException,
    _S3Exception,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AccessType
    AccessType (..),

    -- * AwsLogSourceType
    AwsLogSourceType (..),

    -- * Dimension
    Dimension (..),

    -- * EndpointProtocol
    EndpointProtocol (..),

    -- * HttpsMethod
    HttpsMethod (..),

    -- * OcsfEventClass
    OcsfEventClass (..),

    -- * Region
    Region (..),

    -- * SettingsStatus
    SettingsStatus (..),

    -- * SourceStatus
    SourceStatus (..),

    -- * StorageClass
    StorageClass (..),

    -- * SubscriptionProtocolType
    SubscriptionProtocolType (..),

    -- * SubscriptionStatus
    SubscriptionStatus (..),

    -- * AccountSources
    AccountSources (..),
    newAccountSources,
    accountSources_eventClass,
    accountSources_logsStatus,
    accountSources_account,
    accountSources_sourceType,

    -- * AutoEnableNewRegionConfiguration
    AutoEnableNewRegionConfiguration (..),
    newAutoEnableNewRegionConfiguration,
    autoEnableNewRegionConfiguration_region,
    autoEnableNewRegionConfiguration_sources,

    -- * Failures
    Failures (..),
    newFailures,
    failures_exceptionMessage,
    failures_remediation,
    failures_timestamp,

    -- * FailuresResponse
    FailuresResponse (..),
    newFailuresResponse,
    failuresResponse_failures,
    failuresResponse_region,

    -- * LakeConfigurationRequest
    LakeConfigurationRequest (..),
    newLakeConfigurationRequest,
    lakeConfigurationRequest_encryptionKey,
    lakeConfigurationRequest_replicationDestinationRegions,
    lakeConfigurationRequest_replicationRoleArn,
    lakeConfigurationRequest_retentionSettings,
    lakeConfigurationRequest_tagsMap,

    -- * LakeConfigurationResponse
    LakeConfigurationResponse (..),
    newLakeConfigurationResponse,
    lakeConfigurationResponse_encryptionKey,
    lakeConfigurationResponse_replicationDestinationRegions,
    lakeConfigurationResponse_replicationRoleArn,
    lakeConfigurationResponse_retentionSettings,
    lakeConfigurationResponse_s3BucketArn,
    lakeConfigurationResponse_status,
    lakeConfigurationResponse_tagsMap,

    -- * LogsStatus
    LogsStatus (..),
    newLogsStatus,
    logsStatus_healthStatus,
    logsStatus_pathToLogs,

    -- * ProtocolAndNotificationEndpoint
    ProtocolAndNotificationEndpoint (..),
    newProtocolAndNotificationEndpoint,
    protocolAndNotificationEndpoint_endpoint,
    protocolAndNotificationEndpoint_protocol,

    -- * RetentionSetting
    RetentionSetting (..),
    newRetentionSetting,
    retentionSetting_retentionPeriod,
    retentionSetting_storageClass,

    -- * SourceType
    SourceType (..),
    newSourceType,
    sourceType_awsSourceType,
    sourceType_customSourceType,

    -- * SubscriberResource
    SubscriberResource (..),
    newSubscriberResource,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AccessType
import Amazonka.SecurityLake.Types.AccountSources
import Amazonka.SecurityLake.Types.AutoEnableNewRegionConfiguration
import Amazonka.SecurityLake.Types.AwsLogSourceType
import Amazonka.SecurityLake.Types.Dimension
import Amazonka.SecurityLake.Types.EndpointProtocol
import Amazonka.SecurityLake.Types.Failures
import Amazonka.SecurityLake.Types.FailuresResponse
import Amazonka.SecurityLake.Types.HttpsMethod
import Amazonka.SecurityLake.Types.LakeConfigurationRequest
import Amazonka.SecurityLake.Types.LakeConfigurationResponse
import Amazonka.SecurityLake.Types.LogsStatus
import Amazonka.SecurityLake.Types.OcsfEventClass
import Amazonka.SecurityLake.Types.ProtocolAndNotificationEndpoint
import Amazonka.SecurityLake.Types.Region
import Amazonka.SecurityLake.Types.RetentionSetting
import Amazonka.SecurityLake.Types.SettingsStatus
import Amazonka.SecurityLake.Types.SourceStatus
import Amazonka.SecurityLake.Types.SourceType
import Amazonka.SecurityLake.Types.StorageClass
import Amazonka.SecurityLake.Types.SubscriberResource
import Amazonka.SecurityLake.Types.SubscriptionProtocolType
import Amazonka.SecurityLake.Types.SubscriptionStatus
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

-- | Amazon Security Lake cannot find an Amazon Web Services account with the
-- accountID that you specified, or the account whose credentials you used
-- to make this request isn\'t a member of an organization.
_AccountNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountNotFoundException =
  Core._MatchServiceError
    defaultService
    "AccountNotFoundException"
    Prelude.. Core.hasStatus 403

-- | Amazon Security Lake generally returns 404 errors if the requested
-- object is missing from the bucket.
_BucketNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BucketNotFoundException =
  Core._MatchServiceError
    defaultService
    "BucketNotFoundException"
    Prelude.. Core.hasStatus 409

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 409

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

-- | There was a conflict when you attempted to modify a Security Lake source
-- name.
_ConflictSourceNamesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictSourceNamesException =
  Core._MatchServiceError
    defaultService
    "ConflictSourceNamesException"
    Prelude.. Core.hasStatus 400

-- | A conflicting subscription exception operation is in progress.
_ConflictSubscriptionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictSubscriptionException =
  Core._MatchServiceError
    defaultService
    "ConflictSubscriptionException"
    Prelude.. Core.hasStatus 400

-- | Represents an error interacting with the Amazon EventBridge service.
_EventBridgeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventBridgeException =
  Core._MatchServiceError
    defaultService
    "EventBridgeException"
    Prelude.. Core.hasStatus 400

-- | Internal service exceptions are sometimes caused by transient issues.
-- Before you start troubleshooting, perform the operation again.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request was rejected because a value that\'s not valid or is out of
-- range was supplied for an input parameter.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
    Prelude.. Core.hasStatus 400

-- | The resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides an extension of the AmazonServiceException for errors reported
-- by Amazon S3 while processing a request. In particular, this class
-- provides access to the Amazon S3 extended request ID. If Amazon S3 is
-- incorrectly handling a request and you need to contact Amazon, this
-- extended request ID may provide useful debugging information.
_S3Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_S3Exception =
  Core._MatchServiceError
    defaultService
    "S3Exception"
    Prelude.. Core.hasStatus 400

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use Service Quotas to request
-- a service quota increase.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Your signing certificate could not be validated.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
