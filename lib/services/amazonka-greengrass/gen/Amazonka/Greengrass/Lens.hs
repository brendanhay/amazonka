{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Greengrass.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Lens
  ( -- * Operations

    -- ** AssociateRoleToGroup
    associateRoleToGroup_groupId,
    associateRoleToGroup_roleArn,
    associateRoleToGroupResponse_associatedAt,
    associateRoleToGroupResponse_httpStatus,

    -- ** AssociateServiceRoleToAccount
    associateServiceRoleToAccount_roleArn,
    associateServiceRoleToAccountResponse_associatedAt,
    associateServiceRoleToAccountResponse_httpStatus,

    -- ** CreateConnectorDefinition
    createConnectorDefinition_amznClientToken,
    createConnectorDefinition_initialVersion,
    createConnectorDefinition_name,
    createConnectorDefinition_tags,
    createConnectorDefinitionResponse_arn,
    createConnectorDefinitionResponse_creationTimestamp,
    createConnectorDefinitionResponse_id,
    createConnectorDefinitionResponse_lastUpdatedTimestamp,
    createConnectorDefinitionResponse_latestVersion,
    createConnectorDefinitionResponse_latestVersionArn,
    createConnectorDefinitionResponse_name,
    createConnectorDefinitionResponse_httpStatus,

    -- ** CreateConnectorDefinitionVersion
    createConnectorDefinitionVersion_amznClientToken,
    createConnectorDefinitionVersion_connectors,
    createConnectorDefinitionVersion_connectorDefinitionId,
    createConnectorDefinitionVersionResponse_arn,
    createConnectorDefinitionVersionResponse_creationTimestamp,
    createConnectorDefinitionVersionResponse_id,
    createConnectorDefinitionVersionResponse_version,
    createConnectorDefinitionVersionResponse_httpStatus,

    -- ** CreateCoreDefinition
    createCoreDefinition_amznClientToken,
    createCoreDefinition_initialVersion,
    createCoreDefinition_name,
    createCoreDefinition_tags,
    createCoreDefinitionResponse_arn,
    createCoreDefinitionResponse_creationTimestamp,
    createCoreDefinitionResponse_id,
    createCoreDefinitionResponse_lastUpdatedTimestamp,
    createCoreDefinitionResponse_latestVersion,
    createCoreDefinitionResponse_latestVersionArn,
    createCoreDefinitionResponse_name,
    createCoreDefinitionResponse_httpStatus,

    -- ** CreateCoreDefinitionVersion
    createCoreDefinitionVersion_amznClientToken,
    createCoreDefinitionVersion_cores,
    createCoreDefinitionVersion_coreDefinitionId,
    createCoreDefinitionVersionResponse_arn,
    createCoreDefinitionVersionResponse_creationTimestamp,
    createCoreDefinitionVersionResponse_id,
    createCoreDefinitionVersionResponse_version,
    createCoreDefinitionVersionResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_amznClientToken,
    createDeployment_deploymentId,
    createDeployment_groupVersionId,
    createDeployment_groupId,
    createDeployment_deploymentType,
    createDeploymentResponse_deploymentArn,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateDeviceDefinition
    createDeviceDefinition_amznClientToken,
    createDeviceDefinition_initialVersion,
    createDeviceDefinition_name,
    createDeviceDefinition_tags,
    createDeviceDefinitionResponse_arn,
    createDeviceDefinitionResponse_creationTimestamp,
    createDeviceDefinitionResponse_id,
    createDeviceDefinitionResponse_lastUpdatedTimestamp,
    createDeviceDefinitionResponse_latestVersion,
    createDeviceDefinitionResponse_latestVersionArn,
    createDeviceDefinitionResponse_name,
    createDeviceDefinitionResponse_httpStatus,

    -- ** CreateDeviceDefinitionVersion
    createDeviceDefinitionVersion_amznClientToken,
    createDeviceDefinitionVersion_devices,
    createDeviceDefinitionVersion_deviceDefinitionId,
    createDeviceDefinitionVersionResponse_arn,
    createDeviceDefinitionVersionResponse_creationTimestamp,
    createDeviceDefinitionVersionResponse_id,
    createDeviceDefinitionVersionResponse_version,
    createDeviceDefinitionVersionResponse_httpStatus,

    -- ** CreateFunctionDefinition
    createFunctionDefinition_amznClientToken,
    createFunctionDefinition_initialVersion,
    createFunctionDefinition_name,
    createFunctionDefinition_tags,
    createFunctionDefinitionResponse_arn,
    createFunctionDefinitionResponse_creationTimestamp,
    createFunctionDefinitionResponse_id,
    createFunctionDefinitionResponse_lastUpdatedTimestamp,
    createFunctionDefinitionResponse_latestVersion,
    createFunctionDefinitionResponse_latestVersionArn,
    createFunctionDefinitionResponse_name,
    createFunctionDefinitionResponse_httpStatus,

    -- ** CreateFunctionDefinitionVersion
    createFunctionDefinitionVersion_amznClientToken,
    createFunctionDefinitionVersion_defaultConfig,
    createFunctionDefinitionVersion_functions,
    createFunctionDefinitionVersion_functionDefinitionId,
    createFunctionDefinitionVersionResponse_arn,
    createFunctionDefinitionVersionResponse_creationTimestamp,
    createFunctionDefinitionVersionResponse_id,
    createFunctionDefinitionVersionResponse_version,
    createFunctionDefinitionVersionResponse_httpStatus,

    -- ** CreateGroup
    createGroup_amznClientToken,
    createGroup_initialVersion,
    createGroup_tags,
    createGroup_name,
    createGroupResponse_arn,
    createGroupResponse_creationTimestamp,
    createGroupResponse_id,
    createGroupResponse_lastUpdatedTimestamp,
    createGroupResponse_latestVersion,
    createGroupResponse_latestVersionArn,
    createGroupResponse_name,
    createGroupResponse_httpStatus,

    -- ** CreateGroupCertificateAuthority
    createGroupCertificateAuthority_amznClientToken,
    createGroupCertificateAuthority_groupId,
    createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    createGroupCertificateAuthorityResponse_httpStatus,

    -- ** CreateGroupVersion
    createGroupVersion_amznClientToken,
    createGroupVersion_connectorDefinitionVersionArn,
    createGroupVersion_coreDefinitionVersionArn,
    createGroupVersion_deviceDefinitionVersionArn,
    createGroupVersion_functionDefinitionVersionArn,
    createGroupVersion_loggerDefinitionVersionArn,
    createGroupVersion_resourceDefinitionVersionArn,
    createGroupVersion_subscriptionDefinitionVersionArn,
    createGroupVersion_groupId,
    createGroupVersionResponse_arn,
    createGroupVersionResponse_creationTimestamp,
    createGroupVersionResponse_id,
    createGroupVersionResponse_version,
    createGroupVersionResponse_httpStatus,

    -- ** CreateLoggerDefinition
    createLoggerDefinition_amznClientToken,
    createLoggerDefinition_initialVersion,
    createLoggerDefinition_name,
    createLoggerDefinition_tags,
    createLoggerDefinitionResponse_arn,
    createLoggerDefinitionResponse_creationTimestamp,
    createLoggerDefinitionResponse_id,
    createLoggerDefinitionResponse_lastUpdatedTimestamp,
    createLoggerDefinitionResponse_latestVersion,
    createLoggerDefinitionResponse_latestVersionArn,
    createLoggerDefinitionResponse_name,
    createLoggerDefinitionResponse_httpStatus,

    -- ** CreateLoggerDefinitionVersion
    createLoggerDefinitionVersion_amznClientToken,
    createLoggerDefinitionVersion_loggers,
    createLoggerDefinitionVersion_loggerDefinitionId,
    createLoggerDefinitionVersionResponse_arn,
    createLoggerDefinitionVersionResponse_creationTimestamp,
    createLoggerDefinitionVersionResponse_id,
    createLoggerDefinitionVersionResponse_version,
    createLoggerDefinitionVersionResponse_httpStatus,

    -- ** CreateResourceDefinition
    createResourceDefinition_amznClientToken,
    createResourceDefinition_initialVersion,
    createResourceDefinition_name,
    createResourceDefinition_tags,
    createResourceDefinitionResponse_arn,
    createResourceDefinitionResponse_creationTimestamp,
    createResourceDefinitionResponse_id,
    createResourceDefinitionResponse_lastUpdatedTimestamp,
    createResourceDefinitionResponse_latestVersion,
    createResourceDefinitionResponse_latestVersionArn,
    createResourceDefinitionResponse_name,
    createResourceDefinitionResponse_httpStatus,

    -- ** CreateResourceDefinitionVersion
    createResourceDefinitionVersion_amznClientToken,
    createResourceDefinitionVersion_resources,
    createResourceDefinitionVersion_resourceDefinitionId,
    createResourceDefinitionVersionResponse_arn,
    createResourceDefinitionVersionResponse_creationTimestamp,
    createResourceDefinitionVersionResponse_id,
    createResourceDefinitionVersionResponse_version,
    createResourceDefinitionVersionResponse_httpStatus,

    -- ** CreateSoftwareUpdateJob
    createSoftwareUpdateJob_amznClientToken,
    createSoftwareUpdateJob_updateAgentLogLevel,
    createSoftwareUpdateJob_s3UrlSignerRole,
    createSoftwareUpdateJob_updateTargetsArchitecture,
    createSoftwareUpdateJob_softwareToUpdate,
    createSoftwareUpdateJob_updateTargets,
    createSoftwareUpdateJob_updateTargetsOperatingSystem,
    createSoftwareUpdateJobResponse_iotJobArn,
    createSoftwareUpdateJobResponse_iotJobId,
    createSoftwareUpdateJobResponse_platformSoftwareVersion,
    createSoftwareUpdateJobResponse_httpStatus,

    -- ** CreateSubscriptionDefinition
    createSubscriptionDefinition_amznClientToken,
    createSubscriptionDefinition_initialVersion,
    createSubscriptionDefinition_name,
    createSubscriptionDefinition_tags,
    createSubscriptionDefinitionResponse_arn,
    createSubscriptionDefinitionResponse_creationTimestamp,
    createSubscriptionDefinitionResponse_id,
    createSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    createSubscriptionDefinitionResponse_latestVersion,
    createSubscriptionDefinitionResponse_latestVersionArn,
    createSubscriptionDefinitionResponse_name,
    createSubscriptionDefinitionResponse_httpStatus,

    -- ** CreateSubscriptionDefinitionVersion
    createSubscriptionDefinitionVersion_amznClientToken,
    createSubscriptionDefinitionVersion_subscriptions,
    createSubscriptionDefinitionVersion_subscriptionDefinitionId,
    createSubscriptionDefinitionVersionResponse_arn,
    createSubscriptionDefinitionVersionResponse_creationTimestamp,
    createSubscriptionDefinitionVersionResponse_id,
    createSubscriptionDefinitionVersionResponse_version,
    createSubscriptionDefinitionVersionResponse_httpStatus,

    -- ** DeleteConnectorDefinition
    deleteConnectorDefinition_connectorDefinitionId,
    deleteConnectorDefinitionResponse_httpStatus,

    -- ** DeleteCoreDefinition
    deleteCoreDefinition_coreDefinitionId,
    deleteCoreDefinitionResponse_httpStatus,

    -- ** DeleteDeviceDefinition
    deleteDeviceDefinition_deviceDefinitionId,
    deleteDeviceDefinitionResponse_httpStatus,

    -- ** DeleteFunctionDefinition
    deleteFunctionDefinition_functionDefinitionId,
    deleteFunctionDefinitionResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupId,
    deleteGroupResponse_httpStatus,

    -- ** DeleteLoggerDefinition
    deleteLoggerDefinition_loggerDefinitionId,
    deleteLoggerDefinitionResponse_httpStatus,

    -- ** DeleteResourceDefinition
    deleteResourceDefinition_resourceDefinitionId,
    deleteResourceDefinitionResponse_httpStatus,

    -- ** DeleteSubscriptionDefinition
    deleteSubscriptionDefinition_subscriptionDefinitionId,
    deleteSubscriptionDefinitionResponse_httpStatus,

    -- ** DisassociateRoleFromGroup
    disassociateRoleFromGroup_groupId,
    disassociateRoleFromGroupResponse_disassociatedAt,
    disassociateRoleFromGroupResponse_httpStatus,

    -- ** DisassociateServiceRoleFromAccount
    disassociateServiceRoleFromAccountResponse_disassociatedAt,
    disassociateServiceRoleFromAccountResponse_httpStatus,

    -- ** GetAssociatedRole
    getAssociatedRole_groupId,
    getAssociatedRoleResponse_associatedAt,
    getAssociatedRoleResponse_roleArn,
    getAssociatedRoleResponse_httpStatus,

    -- ** GetBulkDeploymentStatus
    getBulkDeploymentStatus_bulkDeploymentId,
    getBulkDeploymentStatusResponse_bulkDeploymentMetrics,
    getBulkDeploymentStatusResponse_bulkDeploymentStatus,
    getBulkDeploymentStatusResponse_createdAt,
    getBulkDeploymentStatusResponse_errorDetails,
    getBulkDeploymentStatusResponse_errorMessage,
    getBulkDeploymentStatusResponse_tags,
    getBulkDeploymentStatusResponse_httpStatus,

    -- ** GetConnectivityInfo
    getConnectivityInfo_thingName,
    getConnectivityInfoResponse_connectivityInfo,
    getConnectivityInfoResponse_message,
    getConnectivityInfoResponse_httpStatus,

    -- ** GetConnectorDefinition
    getConnectorDefinition_connectorDefinitionId,
    getConnectorDefinitionResponse_arn,
    getConnectorDefinitionResponse_creationTimestamp,
    getConnectorDefinitionResponse_id,
    getConnectorDefinitionResponse_lastUpdatedTimestamp,
    getConnectorDefinitionResponse_latestVersion,
    getConnectorDefinitionResponse_latestVersionArn,
    getConnectorDefinitionResponse_name,
    getConnectorDefinitionResponse_tags,
    getConnectorDefinitionResponse_httpStatus,

    -- ** GetConnectorDefinitionVersion
    getConnectorDefinitionVersion_nextToken,
    getConnectorDefinitionVersion_connectorDefinitionId,
    getConnectorDefinitionVersion_connectorDefinitionVersionId,
    getConnectorDefinitionVersionResponse_arn,
    getConnectorDefinitionVersionResponse_creationTimestamp,
    getConnectorDefinitionVersionResponse_definition,
    getConnectorDefinitionVersionResponse_id,
    getConnectorDefinitionVersionResponse_nextToken,
    getConnectorDefinitionVersionResponse_version,
    getConnectorDefinitionVersionResponse_httpStatus,

    -- ** GetCoreDefinition
    getCoreDefinition_coreDefinitionId,
    getCoreDefinitionResponse_arn,
    getCoreDefinitionResponse_creationTimestamp,
    getCoreDefinitionResponse_id,
    getCoreDefinitionResponse_lastUpdatedTimestamp,
    getCoreDefinitionResponse_latestVersion,
    getCoreDefinitionResponse_latestVersionArn,
    getCoreDefinitionResponse_name,
    getCoreDefinitionResponse_tags,
    getCoreDefinitionResponse_httpStatus,

    -- ** GetCoreDefinitionVersion
    getCoreDefinitionVersion_coreDefinitionId,
    getCoreDefinitionVersion_coreDefinitionVersionId,
    getCoreDefinitionVersionResponse_arn,
    getCoreDefinitionVersionResponse_creationTimestamp,
    getCoreDefinitionVersionResponse_definition,
    getCoreDefinitionVersionResponse_id,
    getCoreDefinitionVersionResponse_nextToken,
    getCoreDefinitionVersionResponse_version,
    getCoreDefinitionVersionResponse_httpStatus,

    -- ** GetDeploymentStatus
    getDeploymentStatus_groupId,
    getDeploymentStatus_deploymentId,
    getDeploymentStatusResponse_deploymentStatus,
    getDeploymentStatusResponse_deploymentType,
    getDeploymentStatusResponse_errorDetails,
    getDeploymentStatusResponse_errorMessage,
    getDeploymentStatusResponse_updatedAt,
    getDeploymentStatusResponse_httpStatus,

    -- ** GetDeviceDefinition
    getDeviceDefinition_deviceDefinitionId,
    getDeviceDefinitionResponse_arn,
    getDeviceDefinitionResponse_creationTimestamp,
    getDeviceDefinitionResponse_id,
    getDeviceDefinitionResponse_lastUpdatedTimestamp,
    getDeviceDefinitionResponse_latestVersion,
    getDeviceDefinitionResponse_latestVersionArn,
    getDeviceDefinitionResponse_name,
    getDeviceDefinitionResponse_tags,
    getDeviceDefinitionResponse_httpStatus,

    -- ** GetDeviceDefinitionVersion
    getDeviceDefinitionVersion_nextToken,
    getDeviceDefinitionVersion_deviceDefinitionVersionId,
    getDeviceDefinitionVersion_deviceDefinitionId,
    getDeviceDefinitionVersionResponse_arn,
    getDeviceDefinitionVersionResponse_creationTimestamp,
    getDeviceDefinitionVersionResponse_definition,
    getDeviceDefinitionVersionResponse_id,
    getDeviceDefinitionVersionResponse_nextToken,
    getDeviceDefinitionVersionResponse_version,
    getDeviceDefinitionVersionResponse_httpStatus,

    -- ** GetFunctionDefinition
    getFunctionDefinition_functionDefinitionId,
    getFunctionDefinitionResponse_arn,
    getFunctionDefinitionResponse_creationTimestamp,
    getFunctionDefinitionResponse_id,
    getFunctionDefinitionResponse_lastUpdatedTimestamp,
    getFunctionDefinitionResponse_latestVersion,
    getFunctionDefinitionResponse_latestVersionArn,
    getFunctionDefinitionResponse_name,
    getFunctionDefinitionResponse_tags,
    getFunctionDefinitionResponse_httpStatus,

    -- ** GetFunctionDefinitionVersion
    getFunctionDefinitionVersion_nextToken,
    getFunctionDefinitionVersion_functionDefinitionId,
    getFunctionDefinitionVersion_functionDefinitionVersionId,
    getFunctionDefinitionVersionResponse_arn,
    getFunctionDefinitionVersionResponse_creationTimestamp,
    getFunctionDefinitionVersionResponse_definition,
    getFunctionDefinitionVersionResponse_id,
    getFunctionDefinitionVersionResponse_nextToken,
    getFunctionDefinitionVersionResponse_version,
    getFunctionDefinitionVersionResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupId,
    getGroupResponse_arn,
    getGroupResponse_creationTimestamp,
    getGroupResponse_id,
    getGroupResponse_lastUpdatedTimestamp,
    getGroupResponse_latestVersion,
    getGroupResponse_latestVersionArn,
    getGroupResponse_name,
    getGroupResponse_tags,
    getGroupResponse_httpStatus,

    -- ** GetGroupCertificateAuthority
    getGroupCertificateAuthority_certificateAuthorityId,
    getGroupCertificateAuthority_groupId,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityId,
    getGroupCertificateAuthorityResponse_pemEncodedCertificate,
    getGroupCertificateAuthorityResponse_httpStatus,

    -- ** GetGroupCertificateConfiguration
    getGroupCertificateConfiguration_groupId,
    getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_groupId,
    getGroupCertificateConfigurationResponse_httpStatus,

    -- ** GetGroupVersion
    getGroupVersion_groupVersionId,
    getGroupVersion_groupId,
    getGroupVersionResponse_arn,
    getGroupVersionResponse_creationTimestamp,
    getGroupVersionResponse_definition,
    getGroupVersionResponse_id,
    getGroupVersionResponse_version,
    getGroupVersionResponse_httpStatus,

    -- ** GetLoggerDefinition
    getLoggerDefinition_loggerDefinitionId,
    getLoggerDefinitionResponse_arn,
    getLoggerDefinitionResponse_creationTimestamp,
    getLoggerDefinitionResponse_id,
    getLoggerDefinitionResponse_lastUpdatedTimestamp,
    getLoggerDefinitionResponse_latestVersion,
    getLoggerDefinitionResponse_latestVersionArn,
    getLoggerDefinitionResponse_name,
    getLoggerDefinitionResponse_tags,
    getLoggerDefinitionResponse_httpStatus,

    -- ** GetLoggerDefinitionVersion
    getLoggerDefinitionVersion_nextToken,
    getLoggerDefinitionVersion_loggerDefinitionVersionId,
    getLoggerDefinitionVersion_loggerDefinitionId,
    getLoggerDefinitionVersionResponse_arn,
    getLoggerDefinitionVersionResponse_creationTimestamp,
    getLoggerDefinitionVersionResponse_definition,
    getLoggerDefinitionVersionResponse_id,
    getLoggerDefinitionVersionResponse_version,
    getLoggerDefinitionVersionResponse_httpStatus,

    -- ** GetResourceDefinition
    getResourceDefinition_resourceDefinitionId,
    getResourceDefinitionResponse_arn,
    getResourceDefinitionResponse_creationTimestamp,
    getResourceDefinitionResponse_id,
    getResourceDefinitionResponse_lastUpdatedTimestamp,
    getResourceDefinitionResponse_latestVersion,
    getResourceDefinitionResponse_latestVersionArn,
    getResourceDefinitionResponse_name,
    getResourceDefinitionResponse_tags,
    getResourceDefinitionResponse_httpStatus,

    -- ** GetResourceDefinitionVersion
    getResourceDefinitionVersion_resourceDefinitionVersionId,
    getResourceDefinitionVersion_resourceDefinitionId,
    getResourceDefinitionVersionResponse_arn,
    getResourceDefinitionVersionResponse_creationTimestamp,
    getResourceDefinitionVersionResponse_definition,
    getResourceDefinitionVersionResponse_id,
    getResourceDefinitionVersionResponse_version,
    getResourceDefinitionVersionResponse_httpStatus,

    -- ** GetServiceRoleForAccount
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_httpStatus,

    -- ** GetSubscriptionDefinition
    getSubscriptionDefinition_subscriptionDefinitionId,
    getSubscriptionDefinitionResponse_arn,
    getSubscriptionDefinitionResponse_creationTimestamp,
    getSubscriptionDefinitionResponse_id,
    getSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    getSubscriptionDefinitionResponse_latestVersion,
    getSubscriptionDefinitionResponse_latestVersionArn,
    getSubscriptionDefinitionResponse_name,
    getSubscriptionDefinitionResponse_tags,
    getSubscriptionDefinitionResponse_httpStatus,

    -- ** GetSubscriptionDefinitionVersion
    getSubscriptionDefinitionVersion_nextToken,
    getSubscriptionDefinitionVersion_subscriptionDefinitionId,
    getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId,
    getSubscriptionDefinitionVersionResponse_arn,
    getSubscriptionDefinitionVersionResponse_creationTimestamp,
    getSubscriptionDefinitionVersionResponse_definition,
    getSubscriptionDefinitionVersionResponse_id,
    getSubscriptionDefinitionVersionResponse_nextToken,
    getSubscriptionDefinitionVersionResponse_version,
    getSubscriptionDefinitionVersionResponse_httpStatus,

    -- ** GetThingRuntimeConfiguration
    getThingRuntimeConfiguration_thingName,
    getThingRuntimeConfigurationResponse_runtimeConfiguration,
    getThingRuntimeConfigurationResponse_httpStatus,

    -- ** ListBulkDeploymentDetailedReports
    listBulkDeploymentDetailedReports_maxResults,
    listBulkDeploymentDetailedReports_nextToken,
    listBulkDeploymentDetailedReports_bulkDeploymentId,
    listBulkDeploymentDetailedReportsResponse_deployments,
    listBulkDeploymentDetailedReportsResponse_nextToken,
    listBulkDeploymentDetailedReportsResponse_httpStatus,

    -- ** ListBulkDeployments
    listBulkDeployments_maxResults,
    listBulkDeployments_nextToken,
    listBulkDeploymentsResponse_bulkDeployments,
    listBulkDeploymentsResponse_nextToken,
    listBulkDeploymentsResponse_httpStatus,

    -- ** ListConnectorDefinitionVersions
    listConnectorDefinitionVersions_maxResults,
    listConnectorDefinitionVersions_nextToken,
    listConnectorDefinitionVersions_connectorDefinitionId,
    listConnectorDefinitionVersionsResponse_nextToken,
    listConnectorDefinitionVersionsResponse_versions,
    listConnectorDefinitionVersionsResponse_httpStatus,

    -- ** ListConnectorDefinitions
    listConnectorDefinitions_maxResults,
    listConnectorDefinitions_nextToken,
    listConnectorDefinitionsResponse_definitions,
    listConnectorDefinitionsResponse_nextToken,
    listConnectorDefinitionsResponse_httpStatus,

    -- ** ListCoreDefinitionVersions
    listCoreDefinitionVersions_maxResults,
    listCoreDefinitionVersions_nextToken,
    listCoreDefinitionVersions_coreDefinitionId,
    listCoreDefinitionVersionsResponse_nextToken,
    listCoreDefinitionVersionsResponse_versions,
    listCoreDefinitionVersionsResponse_httpStatus,

    -- ** ListCoreDefinitions
    listCoreDefinitions_maxResults,
    listCoreDefinitions_nextToken,
    listCoreDefinitionsResponse_definitions,
    listCoreDefinitionsResponse_nextToken,
    listCoreDefinitionsResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_groupId,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** ListDeviceDefinitionVersions
    listDeviceDefinitionVersions_maxResults,
    listDeviceDefinitionVersions_nextToken,
    listDeviceDefinitionVersions_deviceDefinitionId,
    listDeviceDefinitionVersionsResponse_nextToken,
    listDeviceDefinitionVersionsResponse_versions,
    listDeviceDefinitionVersionsResponse_httpStatus,

    -- ** ListDeviceDefinitions
    listDeviceDefinitions_maxResults,
    listDeviceDefinitions_nextToken,
    listDeviceDefinitionsResponse_definitions,
    listDeviceDefinitionsResponse_nextToken,
    listDeviceDefinitionsResponse_httpStatus,

    -- ** ListFunctionDefinitionVersions
    listFunctionDefinitionVersions_maxResults,
    listFunctionDefinitionVersions_nextToken,
    listFunctionDefinitionVersions_functionDefinitionId,
    listFunctionDefinitionVersionsResponse_nextToken,
    listFunctionDefinitionVersionsResponse_versions,
    listFunctionDefinitionVersionsResponse_httpStatus,

    -- ** ListFunctionDefinitions
    listFunctionDefinitions_maxResults,
    listFunctionDefinitions_nextToken,
    listFunctionDefinitionsResponse_definitions,
    listFunctionDefinitionsResponse_nextToken,
    listFunctionDefinitionsResponse_httpStatus,

    -- ** ListGroupCertificateAuthorities
    listGroupCertificateAuthorities_groupId,
    listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities,
    listGroupCertificateAuthoritiesResponse_httpStatus,

    -- ** ListGroupVersions
    listGroupVersions_maxResults,
    listGroupVersions_nextToken,
    listGroupVersions_groupId,
    listGroupVersionsResponse_nextToken,
    listGroupVersionsResponse_versions,
    listGroupVersionsResponse_httpStatus,

    -- ** ListGroups
    listGroups_maxResults,
    listGroups_nextToken,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** ListLoggerDefinitionVersions
    listLoggerDefinitionVersions_maxResults,
    listLoggerDefinitionVersions_nextToken,
    listLoggerDefinitionVersions_loggerDefinitionId,
    listLoggerDefinitionVersionsResponse_nextToken,
    listLoggerDefinitionVersionsResponse_versions,
    listLoggerDefinitionVersionsResponse_httpStatus,

    -- ** ListLoggerDefinitions
    listLoggerDefinitions_maxResults,
    listLoggerDefinitions_nextToken,
    listLoggerDefinitionsResponse_definitions,
    listLoggerDefinitionsResponse_nextToken,
    listLoggerDefinitionsResponse_httpStatus,

    -- ** ListResourceDefinitionVersions
    listResourceDefinitionVersions_maxResults,
    listResourceDefinitionVersions_nextToken,
    listResourceDefinitionVersions_resourceDefinitionId,
    listResourceDefinitionVersionsResponse_nextToken,
    listResourceDefinitionVersionsResponse_versions,
    listResourceDefinitionVersionsResponse_httpStatus,

    -- ** ListResourceDefinitions
    listResourceDefinitions_maxResults,
    listResourceDefinitions_nextToken,
    listResourceDefinitionsResponse_definitions,
    listResourceDefinitionsResponse_nextToken,
    listResourceDefinitionsResponse_httpStatus,

    -- ** ListSubscriptionDefinitionVersions
    listSubscriptionDefinitionVersions_maxResults,
    listSubscriptionDefinitionVersions_nextToken,
    listSubscriptionDefinitionVersions_subscriptionDefinitionId,
    listSubscriptionDefinitionVersionsResponse_nextToken,
    listSubscriptionDefinitionVersionsResponse_versions,
    listSubscriptionDefinitionVersionsResponse_httpStatus,

    -- ** ListSubscriptionDefinitions
    listSubscriptionDefinitions_maxResults,
    listSubscriptionDefinitions_nextToken,
    listSubscriptionDefinitionsResponse_definitions,
    listSubscriptionDefinitionsResponse_nextToken,
    listSubscriptionDefinitionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ResetDeployments
    resetDeployments_amznClientToken,
    resetDeployments_force,
    resetDeployments_groupId,
    resetDeploymentsResponse_deploymentArn,
    resetDeploymentsResponse_deploymentId,
    resetDeploymentsResponse_httpStatus,

    -- ** StartBulkDeployment
    startBulkDeployment_amznClientToken,
    startBulkDeployment_tags,
    startBulkDeployment_executionRoleArn,
    startBulkDeployment_inputFileUri,
    startBulkDeploymentResponse_bulkDeploymentArn,
    startBulkDeploymentResponse_bulkDeploymentId,
    startBulkDeploymentResponse_httpStatus,

    -- ** StopBulkDeployment
    stopBulkDeployment_bulkDeploymentId,
    stopBulkDeploymentResponse_httpStatus,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateConnectivityInfo
    updateConnectivityInfo_connectivityInfo,
    updateConnectivityInfo_thingName,
    updateConnectivityInfoResponse_message,
    updateConnectivityInfoResponse_version,
    updateConnectivityInfoResponse_httpStatus,

    -- ** UpdateConnectorDefinition
    updateConnectorDefinition_name,
    updateConnectorDefinition_connectorDefinitionId,
    updateConnectorDefinitionResponse_httpStatus,

    -- ** UpdateCoreDefinition
    updateCoreDefinition_name,
    updateCoreDefinition_coreDefinitionId,
    updateCoreDefinitionResponse_httpStatus,

    -- ** UpdateDeviceDefinition
    updateDeviceDefinition_name,
    updateDeviceDefinition_deviceDefinitionId,
    updateDeviceDefinitionResponse_httpStatus,

    -- ** UpdateFunctionDefinition
    updateFunctionDefinition_name,
    updateFunctionDefinition_functionDefinitionId,
    updateFunctionDefinitionResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_name,
    updateGroup_groupId,
    updateGroupResponse_httpStatus,

    -- ** UpdateGroupCertificateConfiguration
    updateGroupCertificateConfiguration_certificateExpiryInMilliseconds,
    updateGroupCertificateConfiguration_groupId,
    updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_groupId,
    updateGroupCertificateConfigurationResponse_httpStatus,

    -- ** UpdateLoggerDefinition
    updateLoggerDefinition_name,
    updateLoggerDefinition_loggerDefinitionId,
    updateLoggerDefinitionResponse_httpStatus,

    -- ** UpdateResourceDefinition
    updateResourceDefinition_name,
    updateResourceDefinition_resourceDefinitionId,
    updateResourceDefinitionResponse_httpStatus,

    -- ** UpdateSubscriptionDefinition
    updateSubscriptionDefinition_name,
    updateSubscriptionDefinition_subscriptionDefinitionId,
    updateSubscriptionDefinitionResponse_httpStatus,

    -- ** UpdateThingRuntimeConfiguration
    updateThingRuntimeConfiguration_telemetryConfiguration,
    updateThingRuntimeConfiguration_thingName,
    updateThingRuntimeConfigurationResponse_httpStatus,

    -- * Types

    -- ** BulkDeployment
    bulkDeployment_bulkDeploymentArn,
    bulkDeployment_bulkDeploymentId,
    bulkDeployment_createdAt,

    -- ** BulkDeploymentMetrics
    bulkDeploymentMetrics_invalidInputRecords,
    bulkDeploymentMetrics_recordsProcessed,
    bulkDeploymentMetrics_retryAttempts,

    -- ** BulkDeploymentResult
    bulkDeploymentResult_createdAt,
    bulkDeploymentResult_deploymentArn,
    bulkDeploymentResult_deploymentId,
    bulkDeploymentResult_deploymentStatus,
    bulkDeploymentResult_deploymentType,
    bulkDeploymentResult_errorDetails,
    bulkDeploymentResult_errorMessage,
    bulkDeploymentResult_groupArn,

    -- ** ConnectivityInfo
    connectivityInfo_hostAddress,
    connectivityInfo_id,
    connectivityInfo_metadata,
    connectivityInfo_portNumber,

    -- ** Connector
    connector_parameters,
    connector_connectorArn,
    connector_id,

    -- ** ConnectorDefinitionVersion
    connectorDefinitionVersion_connectors,

    -- ** Core
    core_syncShadow,
    core_thingArn,
    core_id,
    core_certificateArn,

    -- ** CoreDefinitionVersion
    coreDefinitionVersion_cores,

    -- ** DefinitionInformation
    definitionInformation_arn,
    definitionInformation_creationTimestamp,
    definitionInformation_id,
    definitionInformation_lastUpdatedTimestamp,
    definitionInformation_latestVersion,
    definitionInformation_latestVersionArn,
    definitionInformation_name,
    definitionInformation_tags,

    -- ** Deployment
    deployment_createdAt,
    deployment_deploymentArn,
    deployment_deploymentId,
    deployment_deploymentType,
    deployment_groupArn,

    -- ** Device
    device_syncShadow,
    device_thingArn,
    device_id,
    device_certificateArn,

    -- ** DeviceDefinitionVersion
    deviceDefinitionVersion_devices,

    -- ** ErrorDetail
    errorDetail_detailedErrorCode,
    errorDetail_detailedErrorMessage,

    -- ** Function
    function_functionArn,
    function_functionConfiguration,
    function_id,

    -- ** FunctionConfiguration
    functionConfiguration_encodingType,
    functionConfiguration_environment,
    functionConfiguration_execArgs,
    functionConfiguration_executable,
    functionConfiguration_functionRuntimeOverride,
    functionConfiguration_memorySize,
    functionConfiguration_pinned,
    functionConfiguration_timeout,

    -- ** FunctionConfigurationEnvironment
    functionConfigurationEnvironment_accessSysfs,
    functionConfigurationEnvironment_execution,
    functionConfigurationEnvironment_resourceAccessPolicies,
    functionConfigurationEnvironment_variables,

    -- ** FunctionDefaultConfig
    functionDefaultConfig_execution,

    -- ** FunctionDefaultExecutionConfig
    functionDefaultExecutionConfig_isolationMode,
    functionDefaultExecutionConfig_runAs,

    -- ** FunctionDefinitionVersion
    functionDefinitionVersion_defaultConfig,
    functionDefinitionVersion_functions,

    -- ** FunctionExecutionConfig
    functionExecutionConfig_isolationMode,
    functionExecutionConfig_runAs,

    -- ** FunctionRunAsConfig
    functionRunAsConfig_gid,
    functionRunAsConfig_uid,

    -- ** GreengrassLogger
    greengrassLogger_space,
    greengrassLogger_type,
    greengrassLogger_level,
    greengrassLogger_id,
    greengrassLogger_component,

    -- ** GroupCertificateAuthorityProperties
    groupCertificateAuthorityProperties_groupCertificateAuthorityArn,
    groupCertificateAuthorityProperties_groupCertificateAuthorityId,

    -- ** GroupInformation
    groupInformation_arn,
    groupInformation_creationTimestamp,
    groupInformation_id,
    groupInformation_lastUpdatedTimestamp,
    groupInformation_latestVersion,
    groupInformation_latestVersionArn,
    groupInformation_name,

    -- ** GroupOwnerSetting
    groupOwnerSetting_autoAddGroupOwner,
    groupOwnerSetting_groupOwner,

    -- ** GroupVersion
    groupVersion_connectorDefinitionVersionArn,
    groupVersion_coreDefinitionVersionArn,
    groupVersion_deviceDefinitionVersionArn,
    groupVersion_functionDefinitionVersionArn,
    groupVersion_loggerDefinitionVersionArn,
    groupVersion_resourceDefinitionVersionArn,
    groupVersion_subscriptionDefinitionVersionArn,

    -- ** LocalDeviceResourceData
    localDeviceResourceData_groupOwnerSetting,
    localDeviceResourceData_sourcePath,

    -- ** LocalVolumeResourceData
    localVolumeResourceData_destinationPath,
    localVolumeResourceData_groupOwnerSetting,
    localVolumeResourceData_sourcePath,

    -- ** LoggerDefinitionVersion
    loggerDefinitionVersion_loggers,

    -- ** Resource
    resource_resourceDataContainer,
    resource_id,
    resource_name,

    -- ** ResourceAccessPolicy
    resourceAccessPolicy_permission,
    resourceAccessPolicy_resourceId,

    -- ** ResourceDataContainer
    resourceDataContainer_localDeviceResourceData,
    resourceDataContainer_localVolumeResourceData,
    resourceDataContainer_s3MachineLearningModelResourceData,
    resourceDataContainer_sageMakerMachineLearningModelResourceData,
    resourceDataContainer_secretsManagerSecretResourceData,

    -- ** ResourceDefinitionVersion
    resourceDefinitionVersion_resources,

    -- ** ResourceDownloadOwnerSetting
    resourceDownloadOwnerSetting_groupOwner,
    resourceDownloadOwnerSetting_groupPermission,

    -- ** RuntimeConfiguration
    runtimeConfiguration_telemetryConfiguration,

    -- ** S3MachineLearningModelResourceData
    s3MachineLearningModelResourceData_destinationPath,
    s3MachineLearningModelResourceData_ownerSetting,
    s3MachineLearningModelResourceData_s3Uri,

    -- ** SageMakerMachineLearningModelResourceData
    sageMakerMachineLearningModelResourceData_destinationPath,
    sageMakerMachineLearningModelResourceData_ownerSetting,
    sageMakerMachineLearningModelResourceData_sageMakerJobArn,

    -- ** SecretsManagerSecretResourceData
    secretsManagerSecretResourceData_arn,
    secretsManagerSecretResourceData_additionalStagingLabelsToDownload,

    -- ** Subscription
    subscription_target,
    subscription_id,
    subscription_subject,
    subscription_source,

    -- ** SubscriptionDefinitionVersion
    subscriptionDefinitionVersion_subscriptions,

    -- ** TelemetryConfiguration
    telemetryConfiguration_configurationSyncStatus,
    telemetryConfiguration_telemetry,

    -- ** TelemetryConfigurationUpdate
    telemetryConfigurationUpdate_telemetry,

    -- ** VersionInformation
    versionInformation_arn,
    versionInformation_creationTimestamp,
    versionInformation_id,
    versionInformation_version,
  )
where

import Amazonka.Greengrass.AssociateRoleToGroup
import Amazonka.Greengrass.AssociateServiceRoleToAccount
import Amazonka.Greengrass.CreateConnectorDefinition
import Amazonka.Greengrass.CreateConnectorDefinitionVersion
import Amazonka.Greengrass.CreateCoreDefinition
import Amazonka.Greengrass.CreateCoreDefinitionVersion
import Amazonka.Greengrass.CreateDeployment
import Amazonka.Greengrass.CreateDeviceDefinition
import Amazonka.Greengrass.CreateDeviceDefinitionVersion
import Amazonka.Greengrass.CreateFunctionDefinition
import Amazonka.Greengrass.CreateFunctionDefinitionVersion
import Amazonka.Greengrass.CreateGroup
import Amazonka.Greengrass.CreateGroupCertificateAuthority
import Amazonka.Greengrass.CreateGroupVersion
import Amazonka.Greengrass.CreateLoggerDefinition
import Amazonka.Greengrass.CreateLoggerDefinitionVersion
import Amazonka.Greengrass.CreateResourceDefinition
import Amazonka.Greengrass.CreateResourceDefinitionVersion
import Amazonka.Greengrass.CreateSoftwareUpdateJob
import Amazonka.Greengrass.CreateSubscriptionDefinition
import Amazonka.Greengrass.CreateSubscriptionDefinitionVersion
import Amazonka.Greengrass.DeleteConnectorDefinition
import Amazonka.Greengrass.DeleteCoreDefinition
import Amazonka.Greengrass.DeleteDeviceDefinition
import Amazonka.Greengrass.DeleteFunctionDefinition
import Amazonka.Greengrass.DeleteGroup
import Amazonka.Greengrass.DeleteLoggerDefinition
import Amazonka.Greengrass.DeleteResourceDefinition
import Amazonka.Greengrass.DeleteSubscriptionDefinition
import Amazonka.Greengrass.DisassociateRoleFromGroup
import Amazonka.Greengrass.DisassociateServiceRoleFromAccount
import Amazonka.Greengrass.GetAssociatedRole
import Amazonka.Greengrass.GetBulkDeploymentStatus
import Amazonka.Greengrass.GetConnectivityInfo
import Amazonka.Greengrass.GetConnectorDefinition
import Amazonka.Greengrass.GetConnectorDefinitionVersion
import Amazonka.Greengrass.GetCoreDefinition
import Amazonka.Greengrass.GetCoreDefinitionVersion
import Amazonka.Greengrass.GetDeploymentStatus
import Amazonka.Greengrass.GetDeviceDefinition
import Amazonka.Greengrass.GetDeviceDefinitionVersion
import Amazonka.Greengrass.GetFunctionDefinition
import Amazonka.Greengrass.GetFunctionDefinitionVersion
import Amazonka.Greengrass.GetGroup
import Amazonka.Greengrass.GetGroupCertificateAuthority
import Amazonka.Greengrass.GetGroupCertificateConfiguration
import Amazonka.Greengrass.GetGroupVersion
import Amazonka.Greengrass.GetLoggerDefinition
import Amazonka.Greengrass.GetLoggerDefinitionVersion
import Amazonka.Greengrass.GetResourceDefinition
import Amazonka.Greengrass.GetResourceDefinitionVersion
import Amazonka.Greengrass.GetServiceRoleForAccount
import Amazonka.Greengrass.GetSubscriptionDefinition
import Amazonka.Greengrass.GetSubscriptionDefinitionVersion
import Amazonka.Greengrass.GetThingRuntimeConfiguration
import Amazonka.Greengrass.ListBulkDeploymentDetailedReports
import Amazonka.Greengrass.ListBulkDeployments
import Amazonka.Greengrass.ListConnectorDefinitionVersions
import Amazonka.Greengrass.ListConnectorDefinitions
import Amazonka.Greengrass.ListCoreDefinitionVersions
import Amazonka.Greengrass.ListCoreDefinitions
import Amazonka.Greengrass.ListDeployments
import Amazonka.Greengrass.ListDeviceDefinitionVersions
import Amazonka.Greengrass.ListDeviceDefinitions
import Amazonka.Greengrass.ListFunctionDefinitionVersions
import Amazonka.Greengrass.ListFunctionDefinitions
import Amazonka.Greengrass.ListGroupCertificateAuthorities
import Amazonka.Greengrass.ListGroupVersions
import Amazonka.Greengrass.ListGroups
import Amazonka.Greengrass.ListLoggerDefinitionVersions
import Amazonka.Greengrass.ListLoggerDefinitions
import Amazonka.Greengrass.ListResourceDefinitionVersions
import Amazonka.Greengrass.ListResourceDefinitions
import Amazonka.Greengrass.ListSubscriptionDefinitionVersions
import Amazonka.Greengrass.ListSubscriptionDefinitions
import Amazonka.Greengrass.ListTagsForResource
import Amazonka.Greengrass.ResetDeployments
import Amazonka.Greengrass.StartBulkDeployment
import Amazonka.Greengrass.StopBulkDeployment
import Amazonka.Greengrass.TagResource
import Amazonka.Greengrass.Types.BulkDeployment
import Amazonka.Greengrass.Types.BulkDeploymentMetrics
import Amazonka.Greengrass.Types.BulkDeploymentResult
import Amazonka.Greengrass.Types.ConnectivityInfo
import Amazonka.Greengrass.Types.Connector
import Amazonka.Greengrass.Types.ConnectorDefinitionVersion
import Amazonka.Greengrass.Types.Core
import Amazonka.Greengrass.Types.CoreDefinitionVersion
import Amazonka.Greengrass.Types.DefinitionInformation
import Amazonka.Greengrass.Types.Deployment
import Amazonka.Greengrass.Types.Device
import Amazonka.Greengrass.Types.DeviceDefinitionVersion
import Amazonka.Greengrass.Types.ErrorDetail
import Amazonka.Greengrass.Types.Function
import Amazonka.Greengrass.Types.FunctionConfiguration
import Amazonka.Greengrass.Types.FunctionConfigurationEnvironment
import Amazonka.Greengrass.Types.FunctionDefaultConfig
import Amazonka.Greengrass.Types.FunctionDefaultExecutionConfig
import Amazonka.Greengrass.Types.FunctionDefinitionVersion
import Amazonka.Greengrass.Types.FunctionExecutionConfig
import Amazonka.Greengrass.Types.FunctionRunAsConfig
import Amazonka.Greengrass.Types.GreengrassLogger
import Amazonka.Greengrass.Types.GroupCertificateAuthorityProperties
import Amazonka.Greengrass.Types.GroupInformation
import Amazonka.Greengrass.Types.GroupOwnerSetting
import Amazonka.Greengrass.Types.GroupVersion
import Amazonka.Greengrass.Types.LocalDeviceResourceData
import Amazonka.Greengrass.Types.LocalVolumeResourceData
import Amazonka.Greengrass.Types.LoggerDefinitionVersion
import Amazonka.Greengrass.Types.Resource
import Amazonka.Greengrass.Types.ResourceAccessPolicy
import Amazonka.Greengrass.Types.ResourceDataContainer
import Amazonka.Greengrass.Types.ResourceDefinitionVersion
import Amazonka.Greengrass.Types.ResourceDownloadOwnerSetting
import Amazonka.Greengrass.Types.RuntimeConfiguration
import Amazonka.Greengrass.Types.S3MachineLearningModelResourceData
import Amazonka.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Amazonka.Greengrass.Types.SecretsManagerSecretResourceData
import Amazonka.Greengrass.Types.Subscription
import Amazonka.Greengrass.Types.SubscriptionDefinitionVersion
import Amazonka.Greengrass.Types.TelemetryConfiguration
import Amazonka.Greengrass.Types.TelemetryConfigurationUpdate
import Amazonka.Greengrass.Types.VersionInformation
import Amazonka.Greengrass.UntagResource
import Amazonka.Greengrass.UpdateConnectivityInfo
import Amazonka.Greengrass.UpdateConnectorDefinition
import Amazonka.Greengrass.UpdateCoreDefinition
import Amazonka.Greengrass.UpdateDeviceDefinition
import Amazonka.Greengrass.UpdateFunctionDefinition
import Amazonka.Greengrass.UpdateGroup
import Amazonka.Greengrass.UpdateGroupCertificateConfiguration
import Amazonka.Greengrass.UpdateLoggerDefinition
import Amazonka.Greengrass.UpdateResourceDefinition
import Amazonka.Greengrass.UpdateSubscriptionDefinition
import Amazonka.Greengrass.UpdateThingRuntimeConfiguration
