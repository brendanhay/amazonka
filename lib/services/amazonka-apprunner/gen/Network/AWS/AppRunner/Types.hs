{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppRunner.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppRunner.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ServiceQuotaExceededException,
    _InternalServiceErrorException,
    _ResourceNotFoundException,
    _InvalidStateException,

    -- * AutoScalingConfigurationStatus
    AutoScalingConfigurationStatus (..),

    -- * CertificateValidationRecordStatus
    CertificateValidationRecordStatus (..),

    -- * ConfigurationSource
    ConfigurationSource (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * CustomDomainAssociationStatus
    CustomDomainAssociationStatus (..),

    -- * HealthCheckProtocol
    HealthCheckProtocol (..),

    -- * ImageRepositoryType
    ImageRepositoryType (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationType
    OperationType (..),

    -- * ProviderType
    ProviderType (..),

    -- * Runtime
    Runtime (..),

    -- * ServiceStatus
    ServiceStatus (..),

    -- * SourceCodeVersionType
    SourceCodeVersionType (..),

    -- * AuthenticationConfiguration
    AuthenticationConfiguration (..),
    newAuthenticationConfiguration,
    authenticationConfiguration_accessRoleArn,
    authenticationConfiguration_connectionArn,

    -- * AutoScalingConfiguration
    AutoScalingConfiguration (..),
    newAutoScalingConfiguration,
    autoScalingConfiguration_status,
    autoScalingConfiguration_autoScalingConfigurationName,
    autoScalingConfiguration_createdAt,
    autoScalingConfiguration_maxSize,
    autoScalingConfiguration_autoScalingConfigurationRevision,
    autoScalingConfiguration_autoScalingConfigurationArn,
    autoScalingConfiguration_minSize,
    autoScalingConfiguration_deletedAt,
    autoScalingConfiguration_latest,
    autoScalingConfiguration_maxConcurrency,

    -- * AutoScalingConfigurationSummary
    AutoScalingConfigurationSummary (..),
    newAutoScalingConfigurationSummary,
    autoScalingConfigurationSummary_autoScalingConfigurationName,
    autoScalingConfigurationSummary_autoScalingConfigurationRevision,
    autoScalingConfigurationSummary_autoScalingConfigurationArn,

    -- * CertificateValidationRecord
    CertificateValidationRecord (..),
    newCertificateValidationRecord,
    certificateValidationRecord_status,
    certificateValidationRecord_value,
    certificateValidationRecord_name,
    certificateValidationRecord_type,

    -- * CodeConfiguration
    CodeConfiguration (..),
    newCodeConfiguration,
    codeConfiguration_codeConfigurationValues,
    codeConfiguration_configurationSource,

    -- * CodeConfigurationValues
    CodeConfigurationValues (..),
    newCodeConfigurationValues,
    codeConfigurationValues_startCommand,
    codeConfigurationValues_runtimeEnvironmentVariables,
    codeConfigurationValues_buildCommand,
    codeConfigurationValues_port,
    codeConfigurationValues_runtime,

    -- * CodeRepository
    CodeRepository (..),
    newCodeRepository,
    codeRepository_codeConfiguration,
    codeRepository_repositoryUrl,
    codeRepository_sourceCodeVersion,

    -- * Connection
    Connection (..),
    newConnection,
    connection_status,
    connection_createdAt,
    connection_providerType,
    connection_connectionName,
    connection_connectionArn,

    -- * ConnectionSummary
    ConnectionSummary (..),
    newConnectionSummary,
    connectionSummary_status,
    connectionSummary_createdAt,
    connectionSummary_providerType,
    connectionSummary_connectionName,
    connectionSummary_connectionArn,

    -- * CustomDomain
    CustomDomain (..),
    newCustomDomain,
    customDomain_certificateValidationRecords,
    customDomain_domainName,
    customDomain_enableWWWSubdomain,
    customDomain_status,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsKey,

    -- * HealthCheckConfiguration
    HealthCheckConfiguration (..),
    newHealthCheckConfiguration,
    healthCheckConfiguration_healthyThreshold,
    healthCheckConfiguration_path,
    healthCheckConfiguration_protocol,
    healthCheckConfiguration_interval,
    healthCheckConfiguration_timeout,
    healthCheckConfiguration_unhealthyThreshold,

    -- * ImageConfiguration
    ImageConfiguration (..),
    newImageConfiguration,
    imageConfiguration_startCommand,
    imageConfiguration_runtimeEnvironmentVariables,
    imageConfiguration_port,

    -- * ImageRepository
    ImageRepository (..),
    newImageRepository,
    imageRepository_imageConfiguration,
    imageRepository_imageIdentifier,
    imageRepository_imageRepositoryType,

    -- * InstanceConfiguration
    InstanceConfiguration (..),
    newInstanceConfiguration,
    instanceConfiguration_memory,
    instanceConfiguration_instanceRoleArn,
    instanceConfiguration_cpu,

    -- * OperationSummary
    OperationSummary (..),
    newOperationSummary,
    operationSummary_status,
    operationSummary_targetArn,
    operationSummary_endedAt,
    operationSummary_startedAt,
    operationSummary_id,
    operationSummary_type,
    operationSummary_updatedAt,

    -- * Service
    Service (..),
    newService,
    service_encryptionConfiguration,
    service_healthCheckConfiguration,
    service_deletedAt,
    service_serviceName,
    service_serviceId,
    service_serviceArn,
    service_serviceUrl,
    service_createdAt,
    service_updatedAt,
    service_status,
    service_sourceConfiguration,
    service_instanceConfiguration,
    service_autoScalingConfigurationSummary,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_status,
    serviceSummary_createdAt,
    serviceSummary_serviceUrl,
    serviceSummary_serviceName,
    serviceSummary_updatedAt,
    serviceSummary_serviceArn,
    serviceSummary_serviceId,

    -- * SourceCodeVersion
    SourceCodeVersion (..),
    newSourceCodeVersion,
    sourceCodeVersion_type,
    sourceCodeVersion_value,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_imageRepository,
    sourceConfiguration_codeRepository,
    sourceConfiguration_autoDeploymentsEnabled,
    sourceConfiguration_authenticationConfiguration,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import Network.AWS.AppRunner.Types.AuthenticationConfiguration
import Network.AWS.AppRunner.Types.AutoScalingConfiguration
import Network.AWS.AppRunner.Types.AutoScalingConfigurationStatus
import Network.AWS.AppRunner.Types.AutoScalingConfigurationSummary
import Network.AWS.AppRunner.Types.CertificateValidationRecord
import Network.AWS.AppRunner.Types.CertificateValidationRecordStatus
import Network.AWS.AppRunner.Types.CodeConfiguration
import Network.AWS.AppRunner.Types.CodeConfigurationValues
import Network.AWS.AppRunner.Types.CodeRepository
import Network.AWS.AppRunner.Types.ConfigurationSource
import Network.AWS.AppRunner.Types.Connection
import Network.AWS.AppRunner.Types.ConnectionStatus
import Network.AWS.AppRunner.Types.ConnectionSummary
import Network.AWS.AppRunner.Types.CustomDomain
import Network.AWS.AppRunner.Types.CustomDomainAssociationStatus
import Network.AWS.AppRunner.Types.EncryptionConfiguration
import Network.AWS.AppRunner.Types.HealthCheckConfiguration
import Network.AWS.AppRunner.Types.HealthCheckProtocol
import Network.AWS.AppRunner.Types.ImageConfiguration
import Network.AWS.AppRunner.Types.ImageRepository
import Network.AWS.AppRunner.Types.ImageRepositoryType
import Network.AWS.AppRunner.Types.InstanceConfiguration
import Network.AWS.AppRunner.Types.OperationStatus
import Network.AWS.AppRunner.Types.OperationSummary
import Network.AWS.AppRunner.Types.OperationType
import Network.AWS.AppRunner.Types.ProviderType
import Network.AWS.AppRunner.Types.Runtime
import Network.AWS.AppRunner.Types.Service
import Network.AWS.AppRunner.Types.ServiceStatus
import Network.AWS.AppRunner.Types.ServiceSummary
import Network.AWS.AppRunner.Types.SourceCodeVersion
import Network.AWS.AppRunner.Types.SourceCodeVersionType
import Network.AWS.AppRunner.Types.SourceConfiguration
import Network.AWS.AppRunner.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-05-15@ of the Amazon App Runner SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppRunner",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "apprunner",
      Core._serviceSigningName = "apprunner",
      Core._serviceVersion = "2020-05-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppRunner",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more input parameters aren\'t valid. Refer to the API action\'s
-- document page, correct the input parameters, and try the action again.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | App Runner can\'t create this resource. You\'ve reached your account
-- quota for this resource type.
--
-- For App Runner per-resource quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/apprunner.html App Runner endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | An unexpected service exception occurred.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | A resource doesn\'t exist for the specified Amazon Resource Name (ARN)
-- in your Amazon Web Services account.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You can\'t perform this action when the resource is in its current
-- state.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"
