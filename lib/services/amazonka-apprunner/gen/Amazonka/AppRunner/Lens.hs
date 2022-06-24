{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppRunner.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Lens
  ( -- * Operations

    -- ** AssociateCustomDomain
    associateCustomDomain_enableWWWSubdomain,
    associateCustomDomain_serviceArn,
    associateCustomDomain_domainName,
    associateCustomDomainResponse_httpStatus,
    associateCustomDomainResponse_dNSTarget,
    associateCustomDomainResponse_serviceArn,
    associateCustomDomainResponse_customDomain,

    -- ** CreateAutoScalingConfiguration
    createAutoScalingConfiguration_tags,
    createAutoScalingConfiguration_minSize,
    createAutoScalingConfiguration_maxConcurrency,
    createAutoScalingConfiguration_maxSize,
    createAutoScalingConfiguration_autoScalingConfigurationName,
    createAutoScalingConfigurationResponse_httpStatus,
    createAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** CreateConnection
    createConnection_tags,
    createConnection_connectionName,
    createConnection_providerType,
    createConnectionResponse_httpStatus,
    createConnectionResponse_connection,

    -- ** CreateService
    createService_tags,
    createService_instanceConfiguration,
    createService_encryptionConfiguration,
    createService_autoScalingConfigurationArn,
    createService_healthCheckConfiguration,
    createService_serviceName,
    createService_sourceConfiguration,
    createServiceResponse_httpStatus,
    createServiceResponse_service,
    createServiceResponse_operationId,

    -- ** DeleteAutoScalingConfiguration
    deleteAutoScalingConfiguration_autoScalingConfigurationArn,
    deleteAutoScalingConfigurationResponse_httpStatus,
    deleteAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** DeleteConnection
    deleteConnection_connectionArn,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteService
    deleteService_serviceArn,
    deleteServiceResponse_httpStatus,
    deleteServiceResponse_service,
    deleteServiceResponse_operationId,

    -- ** DescribeAutoScalingConfiguration
    describeAutoScalingConfiguration_autoScalingConfigurationArn,
    describeAutoScalingConfigurationResponse_httpStatus,
    describeAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** DescribeCustomDomains
    describeCustomDomains_nextToken,
    describeCustomDomains_maxResults,
    describeCustomDomains_serviceArn,
    describeCustomDomainsResponse_nextToken,
    describeCustomDomainsResponse_httpStatus,
    describeCustomDomainsResponse_dNSTarget,
    describeCustomDomainsResponse_serviceArn,
    describeCustomDomainsResponse_customDomains,

    -- ** DescribeService
    describeService_serviceArn,
    describeServiceResponse_httpStatus,
    describeServiceResponse_service,

    -- ** DisassociateCustomDomain
    disassociateCustomDomain_serviceArn,
    disassociateCustomDomain_domainName,
    disassociateCustomDomainResponse_httpStatus,
    disassociateCustomDomainResponse_dNSTarget,
    disassociateCustomDomainResponse_serviceArn,
    disassociateCustomDomainResponse_customDomain,

    -- ** ListAutoScalingConfigurations
    listAutoScalingConfigurations_nextToken,
    listAutoScalingConfigurations_latestOnly,
    listAutoScalingConfigurations_maxResults,
    listAutoScalingConfigurations_autoScalingConfigurationName,
    listAutoScalingConfigurationsResponse_nextToken,
    listAutoScalingConfigurationsResponse_httpStatus,
    listAutoScalingConfigurationsResponse_autoScalingConfigurationSummaryList,

    -- ** ListConnections
    listConnections_nextToken,
    listConnections_maxResults,
    listConnections_connectionName,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,
    listConnectionsResponse_connectionSummaryList,

    -- ** ListOperations
    listOperations_nextToken,
    listOperations_maxResults,
    listOperations_serviceArn,
    listOperationsResponse_nextToken,
    listOperationsResponse_operationSummaryList,
    listOperationsResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,
    listServicesResponse_serviceSummaryList,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PauseService
    pauseService_serviceArn,
    pauseServiceResponse_operationId,
    pauseServiceResponse_httpStatus,
    pauseServiceResponse_service,

    -- ** ResumeService
    resumeService_serviceArn,
    resumeServiceResponse_operationId,
    resumeServiceResponse_httpStatus,
    resumeServiceResponse_service,

    -- ** StartDeployment
    startDeployment_serviceArn,
    startDeploymentResponse_httpStatus,
    startDeploymentResponse_operationId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateService
    updateService_sourceConfiguration,
    updateService_instanceConfiguration,
    updateService_autoScalingConfigurationArn,
    updateService_healthCheckConfiguration,
    updateService_serviceArn,
    updateServiceResponse_httpStatus,
    updateServiceResponse_service,
    updateServiceResponse_operationId,

    -- * Types

    -- ** AuthenticationConfiguration
    authenticationConfiguration_accessRoleArn,
    authenticationConfiguration_connectionArn,

    -- ** AutoScalingConfiguration
    autoScalingConfiguration_status,
    autoScalingConfiguration_deletedAt,
    autoScalingConfiguration_latest,
    autoScalingConfiguration_autoScalingConfigurationName,
    autoScalingConfiguration_minSize,
    autoScalingConfiguration_maxConcurrency,
    autoScalingConfiguration_autoScalingConfigurationArn,
    autoScalingConfiguration_maxSize,
    autoScalingConfiguration_autoScalingConfigurationRevision,
    autoScalingConfiguration_createdAt,

    -- ** AutoScalingConfigurationSummary
    autoScalingConfigurationSummary_autoScalingConfigurationName,
    autoScalingConfigurationSummary_autoScalingConfigurationArn,
    autoScalingConfigurationSummary_autoScalingConfigurationRevision,

    -- ** CertificateValidationRecord
    certificateValidationRecord_name,
    certificateValidationRecord_type,
    certificateValidationRecord_status,
    certificateValidationRecord_value,

    -- ** CodeConfiguration
    codeConfiguration_codeConfigurationValues,
    codeConfiguration_configurationSource,

    -- ** CodeConfigurationValues
    codeConfigurationValues_port,
    codeConfigurationValues_startCommand,
    codeConfigurationValues_buildCommand,
    codeConfigurationValues_runtimeEnvironmentVariables,
    codeConfigurationValues_runtime,

    -- ** CodeRepository
    codeRepository_codeConfiguration,
    codeRepository_repositoryUrl,
    codeRepository_sourceCodeVersion,

    -- ** Connection
    connection_status,
    connection_connectionArn,
    connection_providerType,
    connection_createdAt,
    connection_connectionName,

    -- ** ConnectionSummary
    connectionSummary_status,
    connectionSummary_connectionArn,
    connectionSummary_providerType,
    connectionSummary_createdAt,
    connectionSummary_connectionName,

    -- ** CustomDomain
    customDomain_certificateValidationRecords,
    customDomain_domainName,
    customDomain_enableWWWSubdomain,
    customDomain_status,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,

    -- ** HealthCheckConfiguration
    healthCheckConfiguration_timeout,
    healthCheckConfiguration_interval,
    healthCheckConfiguration_path,
    healthCheckConfiguration_healthyThreshold,
    healthCheckConfiguration_unhealthyThreshold,
    healthCheckConfiguration_protocol,

    -- ** ImageConfiguration
    imageConfiguration_port,
    imageConfiguration_startCommand,
    imageConfiguration_runtimeEnvironmentVariables,

    -- ** ImageRepository
    imageRepository_imageConfiguration,
    imageRepository_imageIdentifier,
    imageRepository_imageRepositoryType,

    -- ** InstanceConfiguration
    instanceConfiguration_cpu,
    instanceConfiguration_memory,
    instanceConfiguration_instanceRoleArn,

    -- ** OperationSummary
    operationSummary_type,
    operationSummary_endedAt,
    operationSummary_targetArn,
    operationSummary_status,
    operationSummary_id,
    operationSummary_startedAt,
    operationSummary_updatedAt,

    -- ** Service
    service_deletedAt,
    service_encryptionConfiguration,
    service_healthCheckConfiguration,
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

    -- ** ServiceSummary
    serviceSummary_status,
    serviceSummary_serviceName,
    serviceSummary_serviceUrl,
    serviceSummary_createdAt,
    serviceSummary_serviceArn,
    serviceSummary_updatedAt,
    serviceSummary_serviceId,

    -- ** SourceCodeVersion
    sourceCodeVersion_type,
    sourceCodeVersion_value,

    -- ** SourceConfiguration
    sourceConfiguration_codeRepository,
    sourceConfiguration_autoDeploymentsEnabled,
    sourceConfiguration_imageRepository,
    sourceConfiguration_authenticationConfiguration,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.AppRunner.AssociateCustomDomain
import Amazonka.AppRunner.CreateAutoScalingConfiguration
import Amazonka.AppRunner.CreateConnection
import Amazonka.AppRunner.CreateService
import Amazonka.AppRunner.DeleteAutoScalingConfiguration
import Amazonka.AppRunner.DeleteConnection
import Amazonka.AppRunner.DeleteService
import Amazonka.AppRunner.DescribeAutoScalingConfiguration
import Amazonka.AppRunner.DescribeCustomDomains
import Amazonka.AppRunner.DescribeService
import Amazonka.AppRunner.DisassociateCustomDomain
import Amazonka.AppRunner.ListAutoScalingConfigurations
import Amazonka.AppRunner.ListConnections
import Amazonka.AppRunner.ListOperations
import Amazonka.AppRunner.ListServices
import Amazonka.AppRunner.ListTagsForResource
import Amazonka.AppRunner.PauseService
import Amazonka.AppRunner.ResumeService
import Amazonka.AppRunner.StartDeployment
import Amazonka.AppRunner.TagResource
import Amazonka.AppRunner.Types.AuthenticationConfiguration
import Amazonka.AppRunner.Types.AutoScalingConfiguration
import Amazonka.AppRunner.Types.AutoScalingConfigurationSummary
import Amazonka.AppRunner.Types.CertificateValidationRecord
import Amazonka.AppRunner.Types.CodeConfiguration
import Amazonka.AppRunner.Types.CodeConfigurationValues
import Amazonka.AppRunner.Types.CodeRepository
import Amazonka.AppRunner.Types.Connection
import Amazonka.AppRunner.Types.ConnectionSummary
import Amazonka.AppRunner.Types.CustomDomain
import Amazonka.AppRunner.Types.EncryptionConfiguration
import Amazonka.AppRunner.Types.HealthCheckConfiguration
import Amazonka.AppRunner.Types.ImageConfiguration
import Amazonka.AppRunner.Types.ImageRepository
import Amazonka.AppRunner.Types.InstanceConfiguration
import Amazonka.AppRunner.Types.OperationSummary
import Amazonka.AppRunner.Types.Service
import Amazonka.AppRunner.Types.ServiceSummary
import Amazonka.AppRunner.Types.SourceCodeVersion
import Amazonka.AppRunner.Types.SourceConfiguration
import Amazonka.AppRunner.Types.Tag
import Amazonka.AppRunner.UntagResource
import Amazonka.AppRunner.UpdateService
