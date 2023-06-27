{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _InvalidRequestException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AutoScalingMetric
    AutoScalingMetric (..),

    -- * ChangeType
    ChangeType (..),

    -- * ChangesetStatus
    ChangesetStatus (..),

    -- * DnsStatus
    DnsStatus (..),

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * ErrorDetails
    ErrorDetails (..),

    -- * FederationMode
    FederationMode (..),

    -- * IPAddressType
    IPAddressType (..),

    -- * KxAzMode
    KxAzMode (..),

    -- * KxClusterStatus
    KxClusterStatus (..),

    -- * KxClusterType
    KxClusterType (..),

    -- * KxSavedownStorageType
    KxSavedownStorageType (..),

    -- * TgwStatus
    TgwStatus (..),

    -- * AutoScalingConfiguration
    AutoScalingConfiguration (..),
    newAutoScalingConfiguration,
    autoScalingConfiguration_autoScalingMetric,
    autoScalingConfiguration_maxNodeCount,
    autoScalingConfiguration_metricTarget,
    autoScalingConfiguration_minNodeCount,
    autoScalingConfiguration_scaleInCooldownSeconds,
    autoScalingConfiguration_scaleOutCooldownSeconds,

    -- * CapacityConfiguration
    CapacityConfiguration (..),
    newCapacityConfiguration,
    capacityConfiguration_nodeCount,
    capacityConfiguration_nodeType,

    -- * ChangeRequest
    ChangeRequest (..),
    newChangeRequest,
    changeRequest_s3Path,
    changeRequest_changeType,
    changeRequest_dbPath,

    -- * CodeConfiguration
    CodeConfiguration (..),
    newCodeConfiguration,
    codeConfiguration_s3Bucket,
    codeConfiguration_s3Key,
    codeConfiguration_s3ObjectVersion,

    -- * CustomDNSServer
    CustomDNSServer (..),
    newCustomDNSServer,
    customDNSServer_customDNSServerName,
    customDNSServer_customDNSServerIP,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_awsAccountId,
    environment_dedicatedServiceAccountId,
    environment_description,
    environment_environmentArn,
    environment_environmentId,
    environment_environmentUrl,
    environment_federationMode,
    environment_federationParameters,
    environment_kmsKeyId,
    environment_name,
    environment_sageMakerStudioDomainUrl,
    environment_status,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_errorMessage,
    errorInfo_errorType,

    -- * FederationParameters
    FederationParameters (..),
    newFederationParameters,
    federationParameters_applicationCallBackURL,
    federationParameters_attributeMap,
    federationParameters_federationProviderName,
    federationParameters_federationURN,
    federationParameters_samlMetadataDocument,
    federationParameters_samlMetadataURL,

    -- * KxCacheStorageConfiguration
    KxCacheStorageConfiguration (..),
    newKxCacheStorageConfiguration,
    kxCacheStorageConfiguration_type,
    kxCacheStorageConfiguration_size,

    -- * KxChangesetListEntry
    KxChangesetListEntry (..),
    newKxChangesetListEntry,
    kxChangesetListEntry_activeFromTimestamp,
    kxChangesetListEntry_changesetId,
    kxChangesetListEntry_createdTimestamp,
    kxChangesetListEntry_lastModifiedTimestamp,
    kxChangesetListEntry_status,

    -- * KxCluster
    KxCluster (..),
    newKxCluster,
    kxCluster_availabilityZoneId,
    kxCluster_azMode,
    kxCluster_clusterDescription,
    kxCluster_clusterName,
    kxCluster_clusterType,
    kxCluster_createdTimestamp,
    kxCluster_executionRole,
    kxCluster_initializationScript,
    kxCluster_lastModifiedTimestamp,
    kxCluster_releaseLabel,
    kxCluster_status,
    kxCluster_statusReason,

    -- * KxCommandLineArgument
    KxCommandLineArgument (..),
    newKxCommandLineArgument,
    kxCommandLineArgument_key,
    kxCommandLineArgument_value,

    -- * KxDatabaseCacheConfiguration
    KxDatabaseCacheConfiguration (..),
    newKxDatabaseCacheConfiguration,
    kxDatabaseCacheConfiguration_cacheType,
    kxDatabaseCacheConfiguration_dbPaths,

    -- * KxDatabaseConfiguration
    KxDatabaseConfiguration (..),
    newKxDatabaseConfiguration,
    kxDatabaseConfiguration_cacheConfigurations,
    kxDatabaseConfiguration_changesetId,
    kxDatabaseConfiguration_databaseName,

    -- * KxDatabaseListEntry
    KxDatabaseListEntry (..),
    newKxDatabaseListEntry,
    kxDatabaseListEntry_createdTimestamp,
    kxDatabaseListEntry_databaseName,
    kxDatabaseListEntry_lastModifiedTimestamp,

    -- * KxEnvironment
    KxEnvironment (..),
    newKxEnvironment,
    kxEnvironment_availabilityZoneIds,
    kxEnvironment_awsAccountId,
    kxEnvironment_certificateAuthorityArn,
    kxEnvironment_creationTimestamp,
    kxEnvironment_customDNSConfiguration,
    kxEnvironment_dedicatedServiceAccountId,
    kxEnvironment_description,
    kxEnvironment_dnsStatus,
    kxEnvironment_environmentArn,
    kxEnvironment_environmentId,
    kxEnvironment_errorMessage,
    kxEnvironment_kmsKeyId,
    kxEnvironment_name,
    kxEnvironment_status,
    kxEnvironment_tgwStatus,
    kxEnvironment_transitGatewayConfiguration,
    kxEnvironment_updateTimestamp,

    -- * KxNode
    KxNode (..),
    newKxNode,
    kxNode_availabilityZoneId,
    kxNode_launchTime,
    kxNode_nodeId,

    -- * KxSavedownStorageConfiguration
    KxSavedownStorageConfiguration (..),
    newKxSavedownStorageConfiguration,
    kxSavedownStorageConfiguration_type,
    kxSavedownStorageConfiguration_size,

    -- * KxUser
    KxUser (..),
    newKxUser,
    kxUser_createTimestamp,
    kxUser_iamRole,
    kxUser_updateTimestamp,
    kxUser_userArn,
    kxUser_userName,

    -- * SuperuserParameters
    SuperuserParameters (..),
    newSuperuserParameters,
    superuserParameters_emailAddress,
    superuserParameters_firstName,
    superuserParameters_lastName,

    -- * TransitGatewayConfiguration
    TransitGatewayConfiguration (..),
    newTransitGatewayConfiguration,
    transitGatewayConfiguration_transitGatewayID,
    transitGatewayConfiguration_routableCIDRSpace,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_ipAddressType,
    vpcConfiguration_securityGroupIds,
    vpcConfiguration_subnetIds,
    vpcConfiguration_vpcId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpace.Types.AutoScalingConfiguration
import Amazonka.FinSpace.Types.AutoScalingMetric
import Amazonka.FinSpace.Types.CapacityConfiguration
import Amazonka.FinSpace.Types.ChangeRequest
import Amazonka.FinSpace.Types.ChangeType
import Amazonka.FinSpace.Types.ChangesetStatus
import Amazonka.FinSpace.Types.CodeConfiguration
import Amazonka.FinSpace.Types.CustomDNSServer
import Amazonka.FinSpace.Types.DnsStatus
import Amazonka.FinSpace.Types.Environment
import Amazonka.FinSpace.Types.EnvironmentStatus
import Amazonka.FinSpace.Types.ErrorDetails
import Amazonka.FinSpace.Types.ErrorInfo
import Amazonka.FinSpace.Types.FederationMode
import Amazonka.FinSpace.Types.FederationParameters
import Amazonka.FinSpace.Types.IPAddressType
import Amazonka.FinSpace.Types.KxAzMode
import Amazonka.FinSpace.Types.KxCacheStorageConfiguration
import Amazonka.FinSpace.Types.KxChangesetListEntry
import Amazonka.FinSpace.Types.KxCluster
import Amazonka.FinSpace.Types.KxClusterStatus
import Amazonka.FinSpace.Types.KxClusterType
import Amazonka.FinSpace.Types.KxCommandLineArgument
import Amazonka.FinSpace.Types.KxDatabaseCacheConfiguration
import Amazonka.FinSpace.Types.KxDatabaseConfiguration
import Amazonka.FinSpace.Types.KxDatabaseListEntry
import Amazonka.FinSpace.Types.KxEnvironment
import Amazonka.FinSpace.Types.KxNode
import Amazonka.FinSpace.Types.KxSavedownStorageConfiguration
import Amazonka.FinSpace.Types.KxSavedownStorageType
import Amazonka.FinSpace.Types.KxUser
import Amazonka.FinSpace.Types.SuperuserParameters
import Amazonka.FinSpace.Types.TgwStatus
import Amazonka.FinSpace.Types.TransitGatewayConfiguration
import Amazonka.FinSpace.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-03-12@ of the Amazon FinSpace User Environment Management service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FinSpace",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "finspace",
      Core.signingName = "finspace",
      Core.version = "2021-03-12",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FinSpace",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict with this action, and it could not be completed.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request is invalid. Something is wrong with the input to the
-- request.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A service limit or quota is exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified resource group already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use Service Quotas to request
-- a service quota increase.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
