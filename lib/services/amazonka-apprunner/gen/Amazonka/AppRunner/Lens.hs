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

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,
    listServicesResponse_serviceSummaryList,

    -- ** ResumeService
    resumeService_serviceArn,
    resumeServiceResponse_operationId,
    resumeServiceResponse_httpStatus,
    resumeServiceResponse_service,

    -- ** DeleteService
    deleteService_serviceArn,
    deleteServiceResponse_httpStatus,
    deleteServiceResponse_service,
    deleteServiceResponse_operationId,

    -- ** UpdateService
    updateService_autoScalingConfigurationArn,
    updateService_healthCheckConfiguration,
    updateService_sourceConfiguration,
    updateService_instanceConfiguration,
    updateService_serviceArn,
    updateServiceResponse_httpStatus,
    updateServiceResponse_service,
    updateServiceResponse_operationId,

    -- ** ListOperations
    listOperations_nextToken,
    listOperations_maxResults,
    listOperations_serviceArn,
    listOperationsResponse_nextToken,
    listOperationsResponse_operationSummaryList,
    listOperationsResponse_httpStatus,

    -- ** AssociateCustomDomain
    associateCustomDomain_enableWWWSubdomain,
    associateCustomDomain_serviceArn,
    associateCustomDomain_domainName,
    associateCustomDomainResponse_httpStatus,
    associateCustomDomainResponse_dNSTarget,
    associateCustomDomainResponse_serviceArn,
    associateCustomDomainResponse_customDomain,

    -- ** ListConnections
    listConnections_connectionName,
    listConnections_nextToken,
    listConnections_maxResults,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,
    listConnectionsResponse_connectionSummaryList,

    -- ** DeleteConnection
    deleteConnection_connectionArn,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** DescribeAutoScalingConfiguration
    describeAutoScalingConfiguration_autoScalingConfigurationArn,
    describeAutoScalingConfigurationResponse_httpStatus,
    describeAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateConnection
    createConnection_tags,
    createConnection_connectionName,
    createConnection_providerType,
    createConnectionResponse_httpStatus,
    createConnectionResponse_connection,

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

    -- ** DeleteAutoScalingConfiguration
    deleteAutoScalingConfiguration_autoScalingConfigurationArn,
    deleteAutoScalingConfigurationResponse_httpStatus,
    deleteAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** ListAutoScalingConfigurations
    listAutoScalingConfigurations_autoScalingConfigurationName,
    listAutoScalingConfigurations_nextToken,
    listAutoScalingConfigurations_latestOnly,
    listAutoScalingConfigurations_maxResults,
    listAutoScalingConfigurationsResponse_nextToken,
    listAutoScalingConfigurationsResponse_httpStatus,
    listAutoScalingConfigurationsResponse_autoScalingConfigurationSummaryList,

    -- ** DisassociateCustomDomain
    disassociateCustomDomain_serviceArn,
    disassociateCustomDomain_domainName,
    disassociateCustomDomainResponse_httpStatus,
    disassociateCustomDomainResponse_dNSTarget,
    disassociateCustomDomainResponse_serviceArn,
    disassociateCustomDomainResponse_customDomain,

    -- ** PauseService
    pauseService_serviceArn,
    pauseServiceResponse_operationId,
    pauseServiceResponse_httpStatus,
    pauseServiceResponse_service,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateAutoScalingConfiguration
    createAutoScalingConfiguration_maxSize,
    createAutoScalingConfiguration_minSize,
    createAutoScalingConfiguration_tags,
    createAutoScalingConfiguration_maxConcurrency,
    createAutoScalingConfiguration_autoScalingConfigurationName,
    createAutoScalingConfigurationResponse_httpStatus,
    createAutoScalingConfigurationResponse_autoScalingConfiguration,

    -- ** StartDeployment
    startDeployment_serviceArn,
    startDeploymentResponse_httpStatus,
    startDeploymentResponse_operationId,

    -- ** CreateService
    createService_autoScalingConfigurationArn,
    createService_encryptionConfiguration,
    createService_healthCheckConfiguration,
    createService_tags,
    createService_instanceConfiguration,
    createService_serviceName,
    createService_sourceConfiguration,
    createServiceResponse_httpStatus,
    createServiceResponse_service,
    createServiceResponse_operationId,

    -- * Types

    -- ** AuthenticationConfiguration
    authenticationConfiguration_accessRoleArn,
    authenticationConfiguration_connectionArn,

    -- ** AutoScalingConfiguration
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

    -- ** AutoScalingConfigurationSummary
    autoScalingConfigurationSummary_autoScalingConfigurationName,
    autoScalingConfigurationSummary_autoScalingConfigurationRevision,
    autoScalingConfigurationSummary_autoScalingConfigurationArn,

    -- ** CertificateValidationRecord
    certificateValidationRecord_status,
    certificateValidationRecord_value,
    certificateValidationRecord_name,
    certificateValidationRecord_type,

    -- ** CodeConfiguration
    codeConfiguration_codeConfigurationValues,
    codeConfiguration_configurationSource,

    -- ** CodeConfigurationValues
    codeConfigurationValues_startCommand,
    codeConfigurationValues_runtimeEnvironmentVariables,
    codeConfigurationValues_buildCommand,
    codeConfigurationValues_port,
    codeConfigurationValues_runtime,

    -- ** CodeRepository
    codeRepository_codeConfiguration,
    codeRepository_repositoryUrl,
    codeRepository_sourceCodeVersion,

    -- ** Connection
    connection_status,
    connection_createdAt,
    connection_providerType,
    connection_connectionName,
    connection_connectionArn,

    -- ** ConnectionSummary
    connectionSummary_status,
    connectionSummary_createdAt,
    connectionSummary_providerType,
    connectionSummary_connectionName,
    connectionSummary_connectionArn,

    -- ** CustomDomain
    customDomain_certificateValidationRecords,
    customDomain_domainName,
    customDomain_enableWWWSubdomain,
    customDomain_status,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,

    -- ** HealthCheckConfiguration
    healthCheckConfiguration_healthyThreshold,
    healthCheckConfiguration_path,
    healthCheckConfiguration_protocol,
    healthCheckConfiguration_interval,
    healthCheckConfiguration_timeout,
    healthCheckConfiguration_unhealthyThreshold,

    -- ** ImageConfiguration
    imageConfiguration_startCommand,
    imageConfiguration_runtimeEnvironmentVariables,
    imageConfiguration_port,

    -- ** ImageRepository
    imageRepository_imageConfiguration,
    imageRepository_imageIdentifier,
    imageRepository_imageRepositoryType,

    -- ** InstanceConfiguration
    instanceConfiguration_memory,
    instanceConfiguration_instanceRoleArn,
    instanceConfiguration_cpu,

    -- ** OperationSummary
    operationSummary_status,
    operationSummary_targetArn,
    operationSummary_endedAt,
    operationSummary_startedAt,
    operationSummary_id,
    operationSummary_type,
    operationSummary_updatedAt,

    -- ** Service
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

    -- ** ServiceSummary
    serviceSummary_status,
    serviceSummary_createdAt,
    serviceSummary_serviceUrl,
    serviceSummary_serviceName,
    serviceSummary_updatedAt,
    serviceSummary_serviceArn,
    serviceSummary_serviceId,

    -- ** SourceCodeVersion
    sourceCodeVersion_type,
    sourceCodeVersion_value,

    -- ** SourceConfiguration
    sourceConfiguration_imageRepository,
    sourceConfiguration_codeRepository,
    sourceConfiguration_autoDeploymentsEnabled,
    sourceConfiguration_authenticationConfiguration,

    -- ** Tag
    tag_value,
    tag_key,
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
