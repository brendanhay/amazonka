{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppRunner.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServiceErrorException,
    _InvalidRequestException,
    _InvalidStateException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,

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

    -- * EgressType
    EgressType (..),

    -- * HealthCheckProtocol
    HealthCheckProtocol (..),

    -- * ImageRepositoryType
    ImageRepositoryType (..),

    -- * ObservabilityConfigurationStatus
    ObservabilityConfigurationStatus (..),

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

    -- * TracingVendor
    TracingVendor (..),

    -- * VpcConnectorStatus
    VpcConnectorStatus (..),

    -- * VpcIngressConnectionStatus
    VpcIngressConnectionStatus (..),

    -- * AuthenticationConfiguration
    AuthenticationConfiguration (..),
    newAuthenticationConfiguration,
    authenticationConfiguration_accessRoleArn,
    authenticationConfiguration_connectionArn,

    -- * AutoScalingConfiguration
    AutoScalingConfiguration (..),
    newAutoScalingConfiguration,
    autoScalingConfiguration_autoScalingConfigurationArn,
    autoScalingConfiguration_autoScalingConfigurationName,
    autoScalingConfiguration_autoScalingConfigurationRevision,
    autoScalingConfiguration_createdAt,
    autoScalingConfiguration_deletedAt,
    autoScalingConfiguration_latest,
    autoScalingConfiguration_maxConcurrency,
    autoScalingConfiguration_maxSize,
    autoScalingConfiguration_minSize,
    autoScalingConfiguration_status,

    -- * AutoScalingConfigurationSummary
    AutoScalingConfigurationSummary (..),
    newAutoScalingConfigurationSummary,
    autoScalingConfigurationSummary_autoScalingConfigurationArn,
    autoScalingConfigurationSummary_autoScalingConfigurationName,
    autoScalingConfigurationSummary_autoScalingConfigurationRevision,

    -- * CertificateValidationRecord
    CertificateValidationRecord (..),
    newCertificateValidationRecord,
    certificateValidationRecord_name,
    certificateValidationRecord_status,
    certificateValidationRecord_type,
    certificateValidationRecord_value,

    -- * CodeConfiguration
    CodeConfiguration (..),
    newCodeConfiguration,
    codeConfiguration_codeConfigurationValues,
    codeConfiguration_configurationSource,

    -- * CodeConfigurationValues
    CodeConfigurationValues (..),
    newCodeConfigurationValues,
    codeConfigurationValues_buildCommand,
    codeConfigurationValues_port,
    codeConfigurationValues_runtimeEnvironmentSecrets,
    codeConfigurationValues_runtimeEnvironmentVariables,
    codeConfigurationValues_startCommand,
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
    connection_connectionArn,
    connection_connectionName,
    connection_createdAt,
    connection_providerType,
    connection_status,

    -- * ConnectionSummary
    ConnectionSummary (..),
    newConnectionSummary,
    connectionSummary_connectionArn,
    connectionSummary_connectionName,
    connectionSummary_createdAt,
    connectionSummary_providerType,
    connectionSummary_status,

    -- * CustomDomain
    CustomDomain (..),
    newCustomDomain,
    customDomain_certificateValidationRecords,
    customDomain_domainName,
    customDomain_enableWWWSubdomain,
    customDomain_status,

    -- * EgressConfiguration
    EgressConfiguration (..),
    newEgressConfiguration,
    egressConfiguration_egressType,
    egressConfiguration_vpcConnectorArn,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsKey,

    -- * HealthCheckConfiguration
    HealthCheckConfiguration (..),
    newHealthCheckConfiguration,
    healthCheckConfiguration_healthyThreshold,
    healthCheckConfiguration_interval,
    healthCheckConfiguration_path,
    healthCheckConfiguration_protocol,
    healthCheckConfiguration_timeout,
    healthCheckConfiguration_unhealthyThreshold,

    -- * ImageConfiguration
    ImageConfiguration (..),
    newImageConfiguration,
    imageConfiguration_port,
    imageConfiguration_runtimeEnvironmentSecrets,
    imageConfiguration_runtimeEnvironmentVariables,
    imageConfiguration_startCommand,

    -- * ImageRepository
    ImageRepository (..),
    newImageRepository,
    imageRepository_imageConfiguration,
    imageRepository_imageIdentifier,
    imageRepository_imageRepositoryType,

    -- * IngressConfiguration
    IngressConfiguration (..),
    newIngressConfiguration,
    ingressConfiguration_isPubliclyAccessible,

    -- * IngressVpcConfiguration
    IngressVpcConfiguration (..),
    newIngressVpcConfiguration,
    ingressVpcConfiguration_vpcEndpointId,
    ingressVpcConfiguration_vpcId,

    -- * InstanceConfiguration
    InstanceConfiguration (..),
    newInstanceConfiguration,
    instanceConfiguration_cpu,
    instanceConfiguration_instanceRoleArn,
    instanceConfiguration_memory,

    -- * ListVpcIngressConnectionsFilter
    ListVpcIngressConnectionsFilter (..),
    newListVpcIngressConnectionsFilter,
    listVpcIngressConnectionsFilter_serviceArn,
    listVpcIngressConnectionsFilter_vpcEndpointId,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_egressConfiguration,
    networkConfiguration_ingressConfiguration,

    -- * ObservabilityConfiguration
    ObservabilityConfiguration (..),
    newObservabilityConfiguration,
    observabilityConfiguration_createdAt,
    observabilityConfiguration_deletedAt,
    observabilityConfiguration_latest,
    observabilityConfiguration_observabilityConfigurationArn,
    observabilityConfiguration_observabilityConfigurationName,
    observabilityConfiguration_observabilityConfigurationRevision,
    observabilityConfiguration_status,
    observabilityConfiguration_traceConfiguration,

    -- * ObservabilityConfigurationSummary
    ObservabilityConfigurationSummary (..),
    newObservabilityConfigurationSummary,
    observabilityConfigurationSummary_observabilityConfigurationArn,
    observabilityConfigurationSummary_observabilityConfigurationName,
    observabilityConfigurationSummary_observabilityConfigurationRevision,

    -- * OperationSummary
    OperationSummary (..),
    newOperationSummary,
    operationSummary_endedAt,
    operationSummary_id,
    operationSummary_startedAt,
    operationSummary_status,
    operationSummary_targetArn,
    operationSummary_type,
    operationSummary_updatedAt,

    -- * Service
    Service (..),
    newService,
    service_deletedAt,
    service_encryptionConfiguration,
    service_healthCheckConfiguration,
    service_observabilityConfiguration,
    service_serviceUrl,
    service_serviceName,
    service_serviceId,
    service_serviceArn,
    service_createdAt,
    service_updatedAt,
    service_status,
    service_sourceConfiguration,
    service_instanceConfiguration,
    service_autoScalingConfigurationSummary,
    service_networkConfiguration,

    -- * ServiceObservabilityConfiguration
    ServiceObservabilityConfiguration (..),
    newServiceObservabilityConfiguration,
    serviceObservabilityConfiguration_observabilityConfigurationArn,
    serviceObservabilityConfiguration_observabilityEnabled,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_createdAt,
    serviceSummary_serviceArn,
    serviceSummary_serviceId,
    serviceSummary_serviceName,
    serviceSummary_serviceUrl,
    serviceSummary_status,
    serviceSummary_updatedAt,

    -- * SourceCodeVersion
    SourceCodeVersion (..),
    newSourceCodeVersion,
    sourceCodeVersion_type,
    sourceCodeVersion_value,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_authenticationConfiguration,
    sourceConfiguration_autoDeploymentsEnabled,
    sourceConfiguration_codeRepository,
    sourceConfiguration_imageRepository,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TraceConfiguration
    TraceConfiguration (..),
    newTraceConfiguration,
    traceConfiguration_vendor,

    -- * VpcConnector
    VpcConnector (..),
    newVpcConnector,
    vpcConnector_createdAt,
    vpcConnector_deletedAt,
    vpcConnector_securityGroups,
    vpcConnector_status,
    vpcConnector_subnets,
    vpcConnector_vpcConnectorArn,
    vpcConnector_vpcConnectorName,
    vpcConnector_vpcConnectorRevision,

    -- * VpcDNSTarget
    VpcDNSTarget (..),
    newVpcDNSTarget,
    vpcDNSTarget_domainName,
    vpcDNSTarget_vpcId,
    vpcDNSTarget_vpcIngressConnectionArn,

    -- * VpcIngressConnection
    VpcIngressConnection (..),
    newVpcIngressConnection,
    vpcIngressConnection_accountId,
    vpcIngressConnection_createdAt,
    vpcIngressConnection_deletedAt,
    vpcIngressConnection_domainName,
    vpcIngressConnection_ingressVpcConfiguration,
    vpcIngressConnection_serviceArn,
    vpcIngressConnection_status,
    vpcIngressConnection_vpcIngressConnectionArn,
    vpcIngressConnection_vpcIngressConnectionName,

    -- * VpcIngressConnectionSummary
    VpcIngressConnectionSummary (..),
    newVpcIngressConnectionSummary,
    vpcIngressConnectionSummary_serviceArn,
    vpcIngressConnectionSummary_vpcIngressConnectionArn,
  )
where

import Amazonka.AppRunner.Types.AuthenticationConfiguration
import Amazonka.AppRunner.Types.AutoScalingConfiguration
import Amazonka.AppRunner.Types.AutoScalingConfigurationStatus
import Amazonka.AppRunner.Types.AutoScalingConfigurationSummary
import Amazonka.AppRunner.Types.CertificateValidationRecord
import Amazonka.AppRunner.Types.CertificateValidationRecordStatus
import Amazonka.AppRunner.Types.CodeConfiguration
import Amazonka.AppRunner.Types.CodeConfigurationValues
import Amazonka.AppRunner.Types.CodeRepository
import Amazonka.AppRunner.Types.ConfigurationSource
import Amazonka.AppRunner.Types.Connection
import Amazonka.AppRunner.Types.ConnectionStatus
import Amazonka.AppRunner.Types.ConnectionSummary
import Amazonka.AppRunner.Types.CustomDomain
import Amazonka.AppRunner.Types.CustomDomainAssociationStatus
import Amazonka.AppRunner.Types.EgressConfiguration
import Amazonka.AppRunner.Types.EgressType
import Amazonka.AppRunner.Types.EncryptionConfiguration
import Amazonka.AppRunner.Types.HealthCheckConfiguration
import Amazonka.AppRunner.Types.HealthCheckProtocol
import Amazonka.AppRunner.Types.ImageConfiguration
import Amazonka.AppRunner.Types.ImageRepository
import Amazonka.AppRunner.Types.ImageRepositoryType
import Amazonka.AppRunner.Types.IngressConfiguration
import Amazonka.AppRunner.Types.IngressVpcConfiguration
import Amazonka.AppRunner.Types.InstanceConfiguration
import Amazonka.AppRunner.Types.ListVpcIngressConnectionsFilter
import Amazonka.AppRunner.Types.NetworkConfiguration
import Amazonka.AppRunner.Types.ObservabilityConfiguration
import Amazonka.AppRunner.Types.ObservabilityConfigurationStatus
import Amazonka.AppRunner.Types.ObservabilityConfigurationSummary
import Amazonka.AppRunner.Types.OperationStatus
import Amazonka.AppRunner.Types.OperationSummary
import Amazonka.AppRunner.Types.OperationType
import Amazonka.AppRunner.Types.ProviderType
import Amazonka.AppRunner.Types.Runtime
import Amazonka.AppRunner.Types.Service
import Amazonka.AppRunner.Types.ServiceObservabilityConfiguration
import Amazonka.AppRunner.Types.ServiceStatus
import Amazonka.AppRunner.Types.ServiceSummary
import Amazonka.AppRunner.Types.SourceCodeVersion
import Amazonka.AppRunner.Types.SourceCodeVersionType
import Amazonka.AppRunner.Types.SourceConfiguration
import Amazonka.AppRunner.Types.Tag
import Amazonka.AppRunner.Types.TraceConfiguration
import Amazonka.AppRunner.Types.TracingVendor
import Amazonka.AppRunner.Types.VpcConnector
import Amazonka.AppRunner.Types.VpcConnectorStatus
import Amazonka.AppRunner.Types.VpcDNSTarget
import Amazonka.AppRunner.Types.VpcIngressConnection
import Amazonka.AppRunner.Types.VpcIngressConnectionStatus
import Amazonka.AppRunner.Types.VpcIngressConnectionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-05-15@ of the Amazon App Runner SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppRunner",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "apprunner",
      Core.signingName = "apprunner",
      Core.version = "2020-05-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppRunner",
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

-- | An unexpected service exception occurred.
_InternalServiceErrorException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | One or more input parameters aren\'t valid. Refer to the API action\'s
-- document page, correct the input parameters, and try the action again.
_InvalidRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | You can\'t perform this action when the resource is in its current
-- state.
_InvalidStateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | A resource doesn\'t exist for the specified Amazon Resource Name (ARN)
-- in your Amazon Web Services account.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | App Runner can\'t create this resource. You\'ve reached your account
-- quota for this resource type.
--
-- For App Runner per-resource quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/apprunner.html App Runner endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
