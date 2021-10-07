{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Lens
  ( -- * Operations

    -- ** DeleteCoreDefinition
    deleteCoreDefinition_coreDefinitionId,
    deleteCoreDefinitionResponse_httpStatus,

    -- ** UpdateCoreDefinition
    updateCoreDefinition_name,
    updateCoreDefinition_coreDefinitionId,
    updateCoreDefinitionResponse_httpStatus,

    -- ** DeleteSubscriptionDefinition
    deleteSubscriptionDefinition_subscriptionDefinitionId,
    deleteSubscriptionDefinitionResponse_httpStatus,

    -- ** UpdateSubscriptionDefinition
    updateSubscriptionDefinition_name,
    updateSubscriptionDefinition_subscriptionDefinitionId,
    updateSubscriptionDefinitionResponse_httpStatus,

    -- ** AssociateServiceRoleToAccount
    associateServiceRoleToAccount_roleArn,
    associateServiceRoleToAccountResponse_associatedAt,
    associateServiceRoleToAccountResponse_httpStatus,

    -- ** GetGroupCertificateConfiguration
    getGroupCertificateConfiguration_groupId,
    getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_groupId,
    getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_httpStatus,

    -- ** AssociateRoleToGroup
    associateRoleToGroup_groupId,
    associateRoleToGroup_roleArn,
    associateRoleToGroupResponse_associatedAt,
    associateRoleToGroupResponse_httpStatus,

    -- ** ListFunctionDefinitionVersions
    listFunctionDefinitionVersions_nextToken,
    listFunctionDefinitionVersions_maxResults,
    listFunctionDefinitionVersions_functionDefinitionId,
    listFunctionDefinitionVersionsResponse_nextToken,
    listFunctionDefinitionVersionsResponse_versions,
    listFunctionDefinitionVersionsResponse_httpStatus,

    -- ** StopBulkDeployment
    stopBulkDeployment_bulkDeploymentId,
    stopBulkDeploymentResponse_httpStatus,

    -- ** CreateFunctionDefinitionVersion
    createFunctionDefinitionVersion_functions,
    createFunctionDefinitionVersion_defaultConfig,
    createFunctionDefinitionVersion_amznClientToken,
    createFunctionDefinitionVersion_functionDefinitionId,
    createFunctionDefinitionVersionResponse_creationTimestamp,
    createFunctionDefinitionVersionResponse_arn,
    createFunctionDefinitionVersionResponse_id,
    createFunctionDefinitionVersionResponse_version,
    createFunctionDefinitionVersionResponse_httpStatus,

    -- ** UpdateThingRuntimeConfiguration
    updateThingRuntimeConfiguration_telemetryConfiguration,
    updateThingRuntimeConfiguration_thingName,
    updateThingRuntimeConfigurationResponse_httpStatus,

    -- ** GetFunctionDefinition
    getFunctionDefinition_functionDefinitionId,
    getFunctionDefinitionResponse_creationTimestamp,
    getFunctionDefinitionResponse_latestVersionArn,
    getFunctionDefinitionResponse_latestVersion,
    getFunctionDefinitionResponse_arn,
    getFunctionDefinitionResponse_id,
    getFunctionDefinitionResponse_name,
    getFunctionDefinitionResponse_tags,
    getFunctionDefinitionResponse_lastUpdatedTimestamp,
    getFunctionDefinitionResponse_httpStatus,

    -- ** StartBulkDeployment
    startBulkDeployment_tags,
    startBulkDeployment_amznClientToken,
    startBulkDeployment_executionRoleArn,
    startBulkDeployment_inputFileUri,
    startBulkDeploymentResponse_bulkDeploymentId,
    startBulkDeploymentResponse_bulkDeploymentArn,
    startBulkDeploymentResponse_httpStatus,

    -- ** GetThingRuntimeConfiguration
    getThingRuntimeConfiguration_thingName,
    getThingRuntimeConfigurationResponse_runtimeConfiguration,
    getThingRuntimeConfigurationResponse_httpStatus,

    -- ** ListResourceDefinitions
    listResourceDefinitions_nextToken,
    listResourceDefinitions_maxResults,
    listResourceDefinitionsResponse_nextToken,
    listResourceDefinitionsResponse_definitions,
    listResourceDefinitionsResponse_httpStatus,

    -- ** ListDeviceDefinitionVersions
    listDeviceDefinitionVersions_nextToken,
    listDeviceDefinitionVersions_maxResults,
    listDeviceDefinitionVersions_deviceDefinitionId,
    listDeviceDefinitionVersionsResponse_nextToken,
    listDeviceDefinitionVersionsResponse_versions,
    listDeviceDefinitionVersionsResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_maxResults,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** DeleteResourceDefinition
    deleteResourceDefinition_resourceDefinitionId,
    deleteResourceDefinitionResponse_httpStatus,

    -- ** UpdateResourceDefinition
    updateResourceDefinition_name,
    updateResourceDefinition_resourceDefinitionId,
    updateResourceDefinitionResponse_httpStatus,

    -- ** GetGroupVersion
    getGroupVersion_groupVersionId,
    getGroupVersion_groupId,
    getGroupVersionResponse_creationTimestamp,
    getGroupVersionResponse_arn,
    getGroupVersionResponse_id,
    getGroupVersionResponse_version,
    getGroupVersionResponse_definition,
    getGroupVersionResponse_httpStatus,

    -- ** CreateDeviceDefinitionVersion
    createDeviceDefinitionVersion_devices,
    createDeviceDefinitionVersion_amznClientToken,
    createDeviceDefinitionVersion_deviceDefinitionId,
    createDeviceDefinitionVersionResponse_creationTimestamp,
    createDeviceDefinitionVersionResponse_arn,
    createDeviceDefinitionVersionResponse_id,
    createDeviceDefinitionVersionResponse_version,
    createDeviceDefinitionVersionResponse_httpStatus,

    -- ** CreateResourceDefinition
    createResourceDefinition_name,
    createResourceDefinition_initialVersion,
    createResourceDefinition_tags,
    createResourceDefinition_amznClientToken,
    createResourceDefinitionResponse_creationTimestamp,
    createResourceDefinitionResponse_latestVersionArn,
    createResourceDefinitionResponse_latestVersion,
    createResourceDefinitionResponse_arn,
    createResourceDefinitionResponse_id,
    createResourceDefinitionResponse_name,
    createResourceDefinitionResponse_lastUpdatedTimestamp,
    createResourceDefinitionResponse_httpStatus,

    -- ** GetDeviceDefinition
    getDeviceDefinition_deviceDefinitionId,
    getDeviceDefinitionResponse_creationTimestamp,
    getDeviceDefinitionResponse_latestVersionArn,
    getDeviceDefinitionResponse_latestVersion,
    getDeviceDefinitionResponse_arn,
    getDeviceDefinitionResponse_id,
    getDeviceDefinitionResponse_name,
    getDeviceDefinitionResponse_tags,
    getDeviceDefinitionResponse_lastUpdatedTimestamp,
    getDeviceDefinitionResponse_httpStatus,

    -- ** CreateGroup
    createGroup_initialVersion,
    createGroup_tags,
    createGroup_amznClientToken,
    createGroup_name,
    createGroupResponse_creationTimestamp,
    createGroupResponse_latestVersionArn,
    createGroupResponse_latestVersion,
    createGroupResponse_arn,
    createGroupResponse_id,
    createGroupResponse_name,
    createGroupResponse_lastUpdatedTimestamp,
    createGroupResponse_httpStatus,

    -- ** UpdateGroupCertificateConfiguration
    updateGroupCertificateConfiguration_certificateExpiryInMilliseconds,
    updateGroupCertificateConfiguration_groupId,
    updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_groupId,
    updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** GetResourceDefinitionVersion
    getResourceDefinitionVersion_resourceDefinitionVersionId,
    getResourceDefinitionVersion_resourceDefinitionId,
    getResourceDefinitionVersionResponse_creationTimestamp,
    getResourceDefinitionVersionResponse_arn,
    getResourceDefinitionVersionResponse_id,
    getResourceDefinitionVersionResponse_version,
    getResourceDefinitionVersionResponse_definition,
    getResourceDefinitionVersionResponse_httpStatus,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,

    -- ** ListLoggerDefinitions
    listLoggerDefinitions_nextToken,
    listLoggerDefinitions_maxResults,
    listLoggerDefinitionsResponse_nextToken,
    listLoggerDefinitionsResponse_definitions,
    listLoggerDefinitionsResponse_httpStatus,

    -- ** DeleteLoggerDefinition
    deleteLoggerDefinition_loggerDefinitionId,
    deleteLoggerDefinitionResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_maxResults,
    listDeployments_groupId,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,

    -- ** CreateSubscriptionDefinitionVersion
    createSubscriptionDefinitionVersion_subscriptions,
    createSubscriptionDefinitionVersion_amznClientToken,
    createSubscriptionDefinitionVersion_subscriptionDefinitionId,
    createSubscriptionDefinitionVersionResponse_creationTimestamp,
    createSubscriptionDefinitionVersionResponse_arn,
    createSubscriptionDefinitionVersionResponse_id,
    createSubscriptionDefinitionVersionResponse_version,
    createSubscriptionDefinitionVersionResponse_httpStatus,

    -- ** CreateCoreDefinitionVersion
    createCoreDefinitionVersion_cores,
    createCoreDefinitionVersion_amznClientToken,
    createCoreDefinitionVersion_coreDefinitionId,
    createCoreDefinitionVersionResponse_creationTimestamp,
    createCoreDefinitionVersionResponse_arn,
    createCoreDefinitionVersionResponse_id,
    createCoreDefinitionVersionResponse_version,
    createCoreDefinitionVersionResponse_httpStatus,

    -- ** CreateConnectorDefinitionVersion
    createConnectorDefinitionVersion_connectors,
    createConnectorDefinitionVersion_amznClientToken,
    createConnectorDefinitionVersion_connectorDefinitionId,
    createConnectorDefinitionVersionResponse_creationTimestamp,
    createConnectorDefinitionVersionResponse_arn,
    createConnectorDefinitionVersionResponse_id,
    createConnectorDefinitionVersionResponse_version,
    createConnectorDefinitionVersionResponse_httpStatus,

    -- ** ListBulkDeployments
    listBulkDeployments_nextToken,
    listBulkDeployments_maxResults,
    listBulkDeploymentsResponse_nextToken,
    listBulkDeploymentsResponse_bulkDeployments,
    listBulkDeploymentsResponse_httpStatus,

    -- ** UpdateLoggerDefinition
    updateLoggerDefinition_name,
    updateLoggerDefinition_loggerDefinitionId,
    updateLoggerDefinitionResponse_httpStatus,

    -- ** CreateSoftwareUpdateJob
    createSoftwareUpdateJob_updateAgentLogLevel,
    createSoftwareUpdateJob_amznClientToken,
    createSoftwareUpdateJob_s3UrlSignerRole,
    createSoftwareUpdateJob_updateTargetsArchitecture,
    createSoftwareUpdateJob_softwareToUpdate,
    createSoftwareUpdateJob_updateTargets,
    createSoftwareUpdateJob_updateTargetsOperatingSystem,
    createSoftwareUpdateJobResponse_iotJobId,
    createSoftwareUpdateJobResponse_iotJobArn,
    createSoftwareUpdateJobResponse_platformSoftwareVersion,
    createSoftwareUpdateJobResponse_httpStatus,

    -- ** ListSubscriptionDefinitionVersions
    listSubscriptionDefinitionVersions_nextToken,
    listSubscriptionDefinitionVersions_maxResults,
    listSubscriptionDefinitionVersions_subscriptionDefinitionId,
    listSubscriptionDefinitionVersionsResponse_nextToken,
    listSubscriptionDefinitionVersionsResponse_versions,
    listSubscriptionDefinitionVersionsResponse_httpStatus,

    -- ** GetConnectivityInfo
    getConnectivityInfo_thingName,
    getConnectivityInfoResponse_message,
    getConnectivityInfoResponse_connectivityInfo,
    getConnectivityInfoResponse_httpStatus,

    -- ** ListConnectorDefinitionVersions
    listConnectorDefinitionVersions_nextToken,
    listConnectorDefinitionVersions_maxResults,
    listConnectorDefinitionVersions_connectorDefinitionId,
    listConnectorDefinitionVersionsResponse_nextToken,
    listConnectorDefinitionVersionsResponse_versions,
    listConnectorDefinitionVersionsResponse_httpStatus,

    -- ** ListCoreDefinitionVersions
    listCoreDefinitionVersions_nextToken,
    listCoreDefinitionVersions_maxResults,
    listCoreDefinitionVersions_coreDefinitionId,
    listCoreDefinitionVersionsResponse_nextToken,
    listCoreDefinitionVersionsResponse_versions,
    listCoreDefinitionVersionsResponse_httpStatus,

    -- ** GetAssociatedRole
    getAssociatedRole_groupId,
    getAssociatedRoleResponse_roleArn,
    getAssociatedRoleResponse_associatedAt,
    getAssociatedRoleResponse_httpStatus,

    -- ** CreateCoreDefinition
    createCoreDefinition_name,
    createCoreDefinition_initialVersion,
    createCoreDefinition_tags,
    createCoreDefinition_amznClientToken,
    createCoreDefinitionResponse_creationTimestamp,
    createCoreDefinitionResponse_latestVersionArn,
    createCoreDefinitionResponse_latestVersion,
    createCoreDefinitionResponse_arn,
    createCoreDefinitionResponse_id,
    createCoreDefinitionResponse_name,
    createCoreDefinitionResponse_lastUpdatedTimestamp,
    createCoreDefinitionResponse_httpStatus,

    -- ** UpdateConnectivityInfo
    updateConnectivityInfo_connectivityInfo,
    updateConnectivityInfo_thingName,
    updateConnectivityInfoResponse_message,
    updateConnectivityInfoResponse_version,
    updateConnectivityInfoResponse_httpStatus,

    -- ** CreateSubscriptionDefinition
    createSubscriptionDefinition_name,
    createSubscriptionDefinition_initialVersion,
    createSubscriptionDefinition_tags,
    createSubscriptionDefinition_amznClientToken,
    createSubscriptionDefinitionResponse_creationTimestamp,
    createSubscriptionDefinitionResponse_latestVersionArn,
    createSubscriptionDefinitionResponse_latestVersion,
    createSubscriptionDefinitionResponse_arn,
    createSubscriptionDefinitionResponse_id,
    createSubscriptionDefinitionResponse_name,
    createSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    createSubscriptionDefinitionResponse_httpStatus,

    -- ** CreateConnectorDefinition
    createConnectorDefinition_name,
    createConnectorDefinition_initialVersion,
    createConnectorDefinition_tags,
    createConnectorDefinition_amznClientToken,
    createConnectorDefinitionResponse_creationTimestamp,
    createConnectorDefinitionResponse_latestVersionArn,
    createConnectorDefinitionResponse_latestVersion,
    createConnectorDefinitionResponse_arn,
    createConnectorDefinitionResponse_id,
    createConnectorDefinitionResponse_name,
    createConnectorDefinitionResponse_lastUpdatedTimestamp,
    createConnectorDefinitionResponse_httpStatus,

    -- ** ListConnectorDefinitions
    listConnectorDefinitions_nextToken,
    listConnectorDefinitions_maxResults,
    listConnectorDefinitionsResponse_nextToken,
    listConnectorDefinitionsResponse_definitions,
    listConnectorDefinitionsResponse_httpStatus,

    -- ** GetLoggerDefinition
    getLoggerDefinition_loggerDefinitionId,
    getLoggerDefinitionResponse_creationTimestamp,
    getLoggerDefinitionResponse_latestVersionArn,
    getLoggerDefinitionResponse_latestVersion,
    getLoggerDefinitionResponse_arn,
    getLoggerDefinitionResponse_id,
    getLoggerDefinitionResponse_name,
    getLoggerDefinitionResponse_tags,
    getLoggerDefinitionResponse_lastUpdatedTimestamp,
    getLoggerDefinitionResponse_httpStatus,

    -- ** DeleteConnectorDefinition
    deleteConnectorDefinition_connectorDefinitionId,
    deleteConnectorDefinitionResponse_httpStatus,

    -- ** CreateGroupCertificateAuthority
    createGroupCertificateAuthority_amznClientToken,
    createGroupCertificateAuthority_groupId,
    createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    createGroupCertificateAuthorityResponse_httpStatus,

    -- ** ListGroupCertificateAuthorities
    listGroupCertificateAuthorities_groupId,
    listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities,
    listGroupCertificateAuthoritiesResponse_httpStatus,

    -- ** DisassociateRoleFromGroup
    disassociateRoleFromGroup_groupId,
    disassociateRoleFromGroupResponse_disassociatedAt,
    disassociateRoleFromGroupResponse_httpStatus,

    -- ** ListCoreDefinitions
    listCoreDefinitions_nextToken,
    listCoreDefinitions_maxResults,
    listCoreDefinitionsResponse_nextToken,
    listCoreDefinitionsResponse_definitions,
    listCoreDefinitionsResponse_httpStatus,

    -- ** UpdateConnectorDefinition
    updateConnectorDefinition_name,
    updateConnectorDefinition_connectorDefinitionId,
    updateConnectorDefinitionResponse_httpStatus,

    -- ** ListSubscriptionDefinitions
    listSubscriptionDefinitions_nextToken,
    listSubscriptionDefinitions_maxResults,
    listSubscriptionDefinitionsResponse_nextToken,
    listSubscriptionDefinitionsResponse_definitions,
    listSubscriptionDefinitionsResponse_httpStatus,

    -- ** CreateLoggerDefinitionVersion
    createLoggerDefinitionVersion_loggers,
    createLoggerDefinitionVersion_amznClientToken,
    createLoggerDefinitionVersion_loggerDefinitionId,
    createLoggerDefinitionVersionResponse_creationTimestamp,
    createLoggerDefinitionVersionResponse_arn,
    createLoggerDefinitionVersionResponse_id,
    createLoggerDefinitionVersionResponse_version,
    createLoggerDefinitionVersionResponse_httpStatus,

    -- ** ResetDeployments
    resetDeployments_force,
    resetDeployments_amznClientToken,
    resetDeployments_groupId,
    resetDeploymentsResponse_deploymentId,
    resetDeploymentsResponse_deploymentArn,
    resetDeploymentsResponse_httpStatus,

    -- ** DeleteDeviceDefinition
    deleteDeviceDefinition_deviceDefinitionId,
    deleteDeviceDefinitionResponse_httpStatus,

    -- ** DisassociateServiceRoleFromAccount
    disassociateServiceRoleFromAccountResponse_disassociatedAt,
    disassociateServiceRoleFromAccountResponse_httpStatus,

    -- ** ListDeviceDefinitions
    listDeviceDefinitions_nextToken,
    listDeviceDefinitions_maxResults,
    listDeviceDefinitionsResponse_nextToken,
    listDeviceDefinitionsResponse_definitions,
    listDeviceDefinitionsResponse_httpStatus,

    -- ** ListGroupVersions
    listGroupVersions_nextToken,
    listGroupVersions_maxResults,
    listGroupVersions_groupId,
    listGroupVersionsResponse_nextToken,
    listGroupVersionsResponse_versions,
    listGroupVersionsResponse_httpStatus,

    -- ** UpdateDeviceDefinition
    updateDeviceDefinition_name,
    updateDeviceDefinition_deviceDefinitionId,
    updateDeviceDefinitionResponse_httpStatus,

    -- ** ListResourceDefinitionVersions
    listResourceDefinitionVersions_nextToken,
    listResourceDefinitionVersions_maxResults,
    listResourceDefinitionVersions_resourceDefinitionId,
    listResourceDefinitionVersionsResponse_nextToken,
    listResourceDefinitionVersionsResponse_versions,
    listResourceDefinitionVersionsResponse_httpStatus,

    -- ** CreateDeviceDefinition
    createDeviceDefinition_name,
    createDeviceDefinition_initialVersion,
    createDeviceDefinition_tags,
    createDeviceDefinition_amznClientToken,
    createDeviceDefinitionResponse_creationTimestamp,
    createDeviceDefinitionResponse_latestVersionArn,
    createDeviceDefinitionResponse_latestVersion,
    createDeviceDefinitionResponse_arn,
    createDeviceDefinitionResponse_id,
    createDeviceDefinitionResponse_name,
    createDeviceDefinitionResponse_lastUpdatedTimestamp,
    createDeviceDefinitionResponse_httpStatus,

    -- ** GetResourceDefinition
    getResourceDefinition_resourceDefinitionId,
    getResourceDefinitionResponse_creationTimestamp,
    getResourceDefinitionResponse_latestVersionArn,
    getResourceDefinitionResponse_latestVersion,
    getResourceDefinitionResponse_arn,
    getResourceDefinitionResponse_id,
    getResourceDefinitionResponse_name,
    getResourceDefinitionResponse_tags,
    getResourceDefinitionResponse_lastUpdatedTimestamp,
    getResourceDefinitionResponse_httpStatus,

    -- ** CreateResourceDefinitionVersion
    createResourceDefinitionVersion_resources,
    createResourceDefinitionVersion_amznClientToken,
    createResourceDefinitionVersion_resourceDefinitionId,
    createResourceDefinitionVersionResponse_creationTimestamp,
    createResourceDefinitionVersionResponse_arn,
    createResourceDefinitionVersionResponse_id,
    createResourceDefinitionVersionResponse_version,
    createResourceDefinitionVersionResponse_httpStatus,

    -- ** CreateGroupVersion
    createGroupVersion_subscriptionDefinitionVersionArn,
    createGroupVersion_coreDefinitionVersionArn,
    createGroupVersion_connectorDefinitionVersionArn,
    createGroupVersion_loggerDefinitionVersionArn,
    createGroupVersion_resourceDefinitionVersionArn,
    createGroupVersion_functionDefinitionVersionArn,
    createGroupVersion_amznClientToken,
    createGroupVersion_deviceDefinitionVersionArn,
    createGroupVersion_groupId,
    createGroupVersionResponse_creationTimestamp,
    createGroupVersionResponse_arn,
    createGroupVersionResponse_id,
    createGroupVersionResponse_version,
    createGroupVersionResponse_httpStatus,

    -- ** GetDeviceDefinitionVersion
    getDeviceDefinitionVersion_nextToken,
    getDeviceDefinitionVersion_deviceDefinitionVersionId,
    getDeviceDefinitionVersion_deviceDefinitionId,
    getDeviceDefinitionVersionResponse_creationTimestamp,
    getDeviceDefinitionVersionResponse_nextToken,
    getDeviceDefinitionVersionResponse_arn,
    getDeviceDefinitionVersionResponse_id,
    getDeviceDefinitionVersionResponse_version,
    getDeviceDefinitionVersionResponse_definition,
    getDeviceDefinitionVersionResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupId,
    getGroupResponse_creationTimestamp,
    getGroupResponse_latestVersionArn,
    getGroupResponse_latestVersion,
    getGroupResponse_arn,
    getGroupResponse_id,
    getGroupResponse_name,
    getGroupResponse_tags,
    getGroupResponse_lastUpdatedTimestamp,
    getGroupResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupId,
    deleteGroupResponse_httpStatus,

    -- ** ListFunctionDefinitions
    listFunctionDefinitions_nextToken,
    listFunctionDefinitions_maxResults,
    listFunctionDefinitionsResponse_nextToken,
    listFunctionDefinitionsResponse_definitions,
    listFunctionDefinitionsResponse_httpStatus,

    -- ** DeleteFunctionDefinition
    deleteFunctionDefinition_functionDefinitionId,
    deleteFunctionDefinitionResponse_httpStatus,

    -- ** UpdateFunctionDefinition
    updateFunctionDefinition_name,
    updateFunctionDefinition_functionDefinitionId,
    updateFunctionDefinitionResponse_httpStatus,

    -- ** ListBulkDeploymentDetailedReports
    listBulkDeploymentDetailedReports_nextToken,
    listBulkDeploymentDetailedReports_maxResults,
    listBulkDeploymentDetailedReports_bulkDeploymentId,
    listBulkDeploymentDetailedReportsResponse_nextToken,
    listBulkDeploymentDetailedReportsResponse_deployments,
    listBulkDeploymentDetailedReportsResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_name,
    updateGroup_groupId,
    updateGroupResponse_httpStatus,

    -- ** GetDeploymentStatus
    getDeploymentStatus_groupId,
    getDeploymentStatus_deploymentId,
    getDeploymentStatusResponse_deploymentType,
    getDeploymentStatusResponse_updatedAt,
    getDeploymentStatusResponse_deploymentStatus,
    getDeploymentStatusResponse_errorMessage,
    getDeploymentStatusResponse_errorDetails,
    getDeploymentStatusResponse_httpStatus,

    -- ** GetFunctionDefinitionVersion
    getFunctionDefinitionVersion_nextToken,
    getFunctionDefinitionVersion_functionDefinitionId,
    getFunctionDefinitionVersion_functionDefinitionVersionId,
    getFunctionDefinitionVersionResponse_creationTimestamp,
    getFunctionDefinitionVersionResponse_nextToken,
    getFunctionDefinitionVersionResponse_arn,
    getFunctionDefinitionVersionResponse_id,
    getFunctionDefinitionVersionResponse_version,
    getFunctionDefinitionVersionResponse_definition,
    getFunctionDefinitionVersionResponse_httpStatus,

    -- ** GetBulkDeploymentStatus
    getBulkDeploymentStatus_bulkDeploymentId,
    getBulkDeploymentStatusResponse_bulkDeploymentStatus,
    getBulkDeploymentStatusResponse_createdAt,
    getBulkDeploymentStatusResponse_tags,
    getBulkDeploymentStatusResponse_bulkDeploymentMetrics,
    getBulkDeploymentStatusResponse_errorMessage,
    getBulkDeploymentStatusResponse_errorDetails,
    getBulkDeploymentStatusResponse_httpStatus,

    -- ** CreateFunctionDefinition
    createFunctionDefinition_name,
    createFunctionDefinition_initialVersion,
    createFunctionDefinition_tags,
    createFunctionDefinition_amznClientToken,
    createFunctionDefinitionResponse_creationTimestamp,
    createFunctionDefinitionResponse_latestVersionArn,
    createFunctionDefinitionResponse_latestVersion,
    createFunctionDefinitionResponse_arn,
    createFunctionDefinitionResponse_id,
    createFunctionDefinitionResponse_name,
    createFunctionDefinitionResponse_lastUpdatedTimestamp,
    createFunctionDefinitionResponse_httpStatus,

    -- ** GetConnectorDefinition
    getConnectorDefinition_connectorDefinitionId,
    getConnectorDefinitionResponse_creationTimestamp,
    getConnectorDefinitionResponse_latestVersionArn,
    getConnectorDefinitionResponse_latestVersion,
    getConnectorDefinitionResponse_arn,
    getConnectorDefinitionResponse_id,
    getConnectorDefinitionResponse_name,
    getConnectorDefinitionResponse_tags,
    getConnectorDefinitionResponse_lastUpdatedTimestamp,
    getConnectorDefinitionResponse_httpStatus,

    -- ** GetSubscriptionDefinition
    getSubscriptionDefinition_subscriptionDefinitionId,
    getSubscriptionDefinitionResponse_creationTimestamp,
    getSubscriptionDefinitionResponse_latestVersionArn,
    getSubscriptionDefinitionResponse_latestVersion,
    getSubscriptionDefinitionResponse_arn,
    getSubscriptionDefinitionResponse_id,
    getSubscriptionDefinitionResponse_name,
    getSubscriptionDefinitionResponse_tags,
    getSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    getSubscriptionDefinitionResponse_httpStatus,

    -- ** GetCoreDefinition
    getCoreDefinition_coreDefinitionId,
    getCoreDefinitionResponse_creationTimestamp,
    getCoreDefinitionResponse_latestVersionArn,
    getCoreDefinitionResponse_latestVersion,
    getCoreDefinitionResponse_arn,
    getCoreDefinitionResponse_id,
    getCoreDefinitionResponse_name,
    getCoreDefinitionResponse_tags,
    getCoreDefinitionResponse_lastUpdatedTimestamp,
    getCoreDefinitionResponse_httpStatus,

    -- ** GetServiceRoleForAccount
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_deploymentId,
    createDeployment_groupVersionId,
    createDeployment_amznClientToken,
    createDeployment_groupId,
    createDeployment_deploymentType,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_deploymentArn,
    createDeploymentResponse_httpStatus,

    -- ** GetLoggerDefinitionVersion
    getLoggerDefinitionVersion_nextToken,
    getLoggerDefinitionVersion_loggerDefinitionVersionId,
    getLoggerDefinitionVersion_loggerDefinitionId,
    getLoggerDefinitionVersionResponse_creationTimestamp,
    getLoggerDefinitionVersionResponse_arn,
    getLoggerDefinitionVersionResponse_id,
    getLoggerDefinitionVersionResponse_version,
    getLoggerDefinitionVersionResponse_definition,
    getLoggerDefinitionVersionResponse_httpStatus,

    -- ** CreateLoggerDefinition
    createLoggerDefinition_name,
    createLoggerDefinition_initialVersion,
    createLoggerDefinition_tags,
    createLoggerDefinition_amznClientToken,
    createLoggerDefinitionResponse_creationTimestamp,
    createLoggerDefinitionResponse_latestVersionArn,
    createLoggerDefinitionResponse_latestVersion,
    createLoggerDefinitionResponse_arn,
    createLoggerDefinitionResponse_id,
    createLoggerDefinitionResponse_name,
    createLoggerDefinitionResponse_lastUpdatedTimestamp,
    createLoggerDefinitionResponse_httpStatus,

    -- ** GetGroupCertificateAuthority
    getGroupCertificateAuthority_certificateAuthorityId,
    getGroupCertificateAuthority_groupId,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    getGroupCertificateAuthorityResponse_pemEncodedCertificate,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityId,
    getGroupCertificateAuthorityResponse_httpStatus,

    -- ** GetConnectorDefinitionVersion
    getConnectorDefinitionVersion_nextToken,
    getConnectorDefinitionVersion_connectorDefinitionId,
    getConnectorDefinitionVersion_connectorDefinitionVersionId,
    getConnectorDefinitionVersionResponse_creationTimestamp,
    getConnectorDefinitionVersionResponse_nextToken,
    getConnectorDefinitionVersionResponse_arn,
    getConnectorDefinitionVersionResponse_id,
    getConnectorDefinitionVersionResponse_version,
    getConnectorDefinitionVersionResponse_definition,
    getConnectorDefinitionVersionResponse_httpStatus,

    -- ** ListLoggerDefinitionVersions
    listLoggerDefinitionVersions_nextToken,
    listLoggerDefinitionVersions_maxResults,
    listLoggerDefinitionVersions_loggerDefinitionId,
    listLoggerDefinitionVersionsResponse_nextToken,
    listLoggerDefinitionVersionsResponse_versions,
    listLoggerDefinitionVersionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetSubscriptionDefinitionVersion
    getSubscriptionDefinitionVersion_nextToken,
    getSubscriptionDefinitionVersion_subscriptionDefinitionId,
    getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId,
    getSubscriptionDefinitionVersionResponse_creationTimestamp,
    getSubscriptionDefinitionVersionResponse_nextToken,
    getSubscriptionDefinitionVersionResponse_arn,
    getSubscriptionDefinitionVersionResponse_id,
    getSubscriptionDefinitionVersionResponse_version,
    getSubscriptionDefinitionVersionResponse_definition,
    getSubscriptionDefinitionVersionResponse_httpStatus,

    -- ** GetCoreDefinitionVersion
    getCoreDefinitionVersion_coreDefinitionId,
    getCoreDefinitionVersion_coreDefinitionVersionId,
    getCoreDefinitionVersionResponse_creationTimestamp,
    getCoreDefinitionVersionResponse_nextToken,
    getCoreDefinitionVersionResponse_arn,
    getCoreDefinitionVersionResponse_id,
    getCoreDefinitionVersionResponse_version,
    getCoreDefinitionVersionResponse_definition,
    getCoreDefinitionVersionResponse_httpStatus,

    -- * Types

    -- ** BulkDeployment
    bulkDeployment_bulkDeploymentId,
    bulkDeployment_createdAt,
    bulkDeployment_bulkDeploymentArn,

    -- ** BulkDeploymentMetrics
    bulkDeploymentMetrics_recordsProcessed,
    bulkDeploymentMetrics_retryAttempts,
    bulkDeploymentMetrics_invalidInputRecords,

    -- ** BulkDeploymentResult
    bulkDeploymentResult_deploymentId,
    bulkDeploymentResult_deploymentType,
    bulkDeploymentResult_createdAt,
    bulkDeploymentResult_deploymentStatus,
    bulkDeploymentResult_deploymentArn,
    bulkDeploymentResult_errorMessage,
    bulkDeploymentResult_groupArn,
    bulkDeploymentResult_errorDetails,

    -- ** ConnectivityInfo
    connectivityInfo_metadata,
    connectivityInfo_id,
    connectivityInfo_portNumber,
    connectivityInfo_hostAddress,

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
    definitionInformation_creationTimestamp,
    definitionInformation_latestVersionArn,
    definitionInformation_latestVersion,
    definitionInformation_arn,
    definitionInformation_id,
    definitionInformation_name,
    definitionInformation_tags,
    definitionInformation_lastUpdatedTimestamp,

    -- ** Deployment
    deployment_deploymentId,
    deployment_deploymentType,
    deployment_createdAt,
    deployment_deploymentArn,
    deployment_groupArn,

    -- ** Device
    device_syncShadow,
    device_thingArn,
    device_id,
    device_certificateArn,

    -- ** DeviceDefinitionVersion
    deviceDefinitionVersion_devices,

    -- ** ErrorDetail
    errorDetail_detailedErrorMessage,
    errorDetail_detailedErrorCode,

    -- ** Function
    function_functionConfiguration,
    function_functionArn,
    function_id,

    -- ** FunctionConfiguration
    functionConfiguration_memorySize,
    functionConfiguration_execArgs,
    functionConfiguration_timeout,
    functionConfiguration_encodingType,
    functionConfiguration_executable,
    functionConfiguration_pinned,
    functionConfiguration_environment,

    -- ** FunctionConfigurationEnvironment
    functionConfigurationEnvironment_accessSysfs,
    functionConfigurationEnvironment_variables,
    functionConfigurationEnvironment_execution,
    functionConfigurationEnvironment_resourceAccessPolicies,

    -- ** FunctionDefaultConfig
    functionDefaultConfig_execution,

    -- ** FunctionDefaultExecutionConfig
    functionDefaultExecutionConfig_isolationMode,
    functionDefaultExecutionConfig_runAs,

    -- ** FunctionDefinitionVersion
    functionDefinitionVersion_functions,
    functionDefinitionVersion_defaultConfig,

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
    groupInformation_creationTimestamp,
    groupInformation_latestVersionArn,
    groupInformation_latestVersion,
    groupInformation_arn,
    groupInformation_id,
    groupInformation_name,
    groupInformation_lastUpdatedTimestamp,

    -- ** GroupOwnerSetting
    groupOwnerSetting_groupOwner,
    groupOwnerSetting_autoAddGroupOwner,

    -- ** GroupVersion
    groupVersion_subscriptionDefinitionVersionArn,
    groupVersion_coreDefinitionVersionArn,
    groupVersion_connectorDefinitionVersionArn,
    groupVersion_loggerDefinitionVersionArn,
    groupVersion_resourceDefinitionVersionArn,
    groupVersion_functionDefinitionVersionArn,
    groupVersion_deviceDefinitionVersionArn,

    -- ** LocalDeviceResourceData
    localDeviceResourceData_sourcePath,
    localDeviceResourceData_groupOwnerSetting,

    -- ** LocalVolumeResourceData
    localVolumeResourceData_destinationPath,
    localVolumeResourceData_sourcePath,
    localVolumeResourceData_groupOwnerSetting,

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
    resourceDataContainer_localVolumeResourceData,
    resourceDataContainer_localDeviceResourceData,
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
    s3MachineLearningModelResourceData_ownerSetting,
    s3MachineLearningModelResourceData_destinationPath,
    s3MachineLearningModelResourceData_s3Uri,

    -- ** SageMakerMachineLearningModelResourceData
    sageMakerMachineLearningModelResourceData_ownerSetting,
    sageMakerMachineLearningModelResourceData_destinationPath,
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
    versionInformation_creationTimestamp,
    versionInformation_arn,
    versionInformation_id,
    versionInformation_version,
  )
where

import Network.AWS.Greengrass.AssociateRoleToGroup
import Network.AWS.Greengrass.AssociateServiceRoleToAccount
import Network.AWS.Greengrass.CreateConnectorDefinition
import Network.AWS.Greengrass.CreateConnectorDefinitionVersion
import Network.AWS.Greengrass.CreateCoreDefinition
import Network.AWS.Greengrass.CreateCoreDefinitionVersion
import Network.AWS.Greengrass.CreateDeployment
import Network.AWS.Greengrass.CreateDeviceDefinition
import Network.AWS.Greengrass.CreateDeviceDefinitionVersion
import Network.AWS.Greengrass.CreateFunctionDefinition
import Network.AWS.Greengrass.CreateFunctionDefinitionVersion
import Network.AWS.Greengrass.CreateGroup
import Network.AWS.Greengrass.CreateGroupCertificateAuthority
import Network.AWS.Greengrass.CreateGroupVersion
import Network.AWS.Greengrass.CreateLoggerDefinition
import Network.AWS.Greengrass.CreateLoggerDefinitionVersion
import Network.AWS.Greengrass.CreateResourceDefinition
import Network.AWS.Greengrass.CreateResourceDefinitionVersion
import Network.AWS.Greengrass.CreateSoftwareUpdateJob
import Network.AWS.Greengrass.CreateSubscriptionDefinition
import Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
import Network.AWS.Greengrass.DeleteConnectorDefinition
import Network.AWS.Greengrass.DeleteCoreDefinition
import Network.AWS.Greengrass.DeleteDeviceDefinition
import Network.AWS.Greengrass.DeleteFunctionDefinition
import Network.AWS.Greengrass.DeleteGroup
import Network.AWS.Greengrass.DeleteLoggerDefinition
import Network.AWS.Greengrass.DeleteResourceDefinition
import Network.AWS.Greengrass.DeleteSubscriptionDefinition
import Network.AWS.Greengrass.DisassociateRoleFromGroup
import Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
import Network.AWS.Greengrass.GetAssociatedRole
import Network.AWS.Greengrass.GetBulkDeploymentStatus
import Network.AWS.Greengrass.GetConnectivityInfo
import Network.AWS.Greengrass.GetConnectorDefinition
import Network.AWS.Greengrass.GetConnectorDefinitionVersion
import Network.AWS.Greengrass.GetCoreDefinition
import Network.AWS.Greengrass.GetCoreDefinitionVersion
import Network.AWS.Greengrass.GetDeploymentStatus
import Network.AWS.Greengrass.GetDeviceDefinition
import Network.AWS.Greengrass.GetDeviceDefinitionVersion
import Network.AWS.Greengrass.GetFunctionDefinition
import Network.AWS.Greengrass.GetFunctionDefinitionVersion
import Network.AWS.Greengrass.GetGroup
import Network.AWS.Greengrass.GetGroupCertificateAuthority
import Network.AWS.Greengrass.GetGroupCertificateConfiguration
import Network.AWS.Greengrass.GetGroupVersion
import Network.AWS.Greengrass.GetLoggerDefinition
import Network.AWS.Greengrass.GetLoggerDefinitionVersion
import Network.AWS.Greengrass.GetResourceDefinition
import Network.AWS.Greengrass.GetResourceDefinitionVersion
import Network.AWS.Greengrass.GetServiceRoleForAccount
import Network.AWS.Greengrass.GetSubscriptionDefinition
import Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
import Network.AWS.Greengrass.GetThingRuntimeConfiguration
import Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
import Network.AWS.Greengrass.ListBulkDeployments
import Network.AWS.Greengrass.ListConnectorDefinitionVersions
import Network.AWS.Greengrass.ListConnectorDefinitions
import Network.AWS.Greengrass.ListCoreDefinitionVersions
import Network.AWS.Greengrass.ListCoreDefinitions
import Network.AWS.Greengrass.ListDeployments
import Network.AWS.Greengrass.ListDeviceDefinitionVersions
import Network.AWS.Greengrass.ListDeviceDefinitions
import Network.AWS.Greengrass.ListFunctionDefinitionVersions
import Network.AWS.Greengrass.ListFunctionDefinitions
import Network.AWS.Greengrass.ListGroupCertificateAuthorities
import Network.AWS.Greengrass.ListGroupVersions
import Network.AWS.Greengrass.ListGroups
import Network.AWS.Greengrass.ListLoggerDefinitionVersions
import Network.AWS.Greengrass.ListLoggerDefinitions
import Network.AWS.Greengrass.ListResourceDefinitionVersions
import Network.AWS.Greengrass.ListResourceDefinitions
import Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
import Network.AWS.Greengrass.ListSubscriptionDefinitions
import Network.AWS.Greengrass.ListTagsForResource
import Network.AWS.Greengrass.ResetDeployments
import Network.AWS.Greengrass.StartBulkDeployment
import Network.AWS.Greengrass.StopBulkDeployment
import Network.AWS.Greengrass.TagResource
import Network.AWS.Greengrass.Types.BulkDeployment
import Network.AWS.Greengrass.Types.BulkDeploymentMetrics
import Network.AWS.Greengrass.Types.BulkDeploymentResult
import Network.AWS.Greengrass.Types.ConnectivityInfo
import Network.AWS.Greengrass.Types.Connector
import Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
import Network.AWS.Greengrass.Types.Core
import Network.AWS.Greengrass.Types.CoreDefinitionVersion
import Network.AWS.Greengrass.Types.DefinitionInformation
import Network.AWS.Greengrass.Types.Deployment
import Network.AWS.Greengrass.Types.Device
import Network.AWS.Greengrass.Types.DeviceDefinitionVersion
import Network.AWS.Greengrass.Types.ErrorDetail
import Network.AWS.Greengrass.Types.Function
import Network.AWS.Greengrass.Types.FunctionConfiguration
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import Network.AWS.Greengrass.Types.FunctionDefaultConfig
import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import Network.AWS.Greengrass.Types.FunctionDefinitionVersion
import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import Network.AWS.Greengrass.Types.GreengrassLogger
import Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
import Network.AWS.Greengrass.Types.GroupInformation
import Network.AWS.Greengrass.Types.GroupOwnerSetting
import Network.AWS.Greengrass.Types.GroupVersion
import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.LoggerDefinitionVersion
import Network.AWS.Greengrass.Types.Resource
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import Network.AWS.Greengrass.Types.ResourceDataContainer
import Network.AWS.Greengrass.Types.ResourceDefinitionVersion
import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import Network.AWS.Greengrass.Types.RuntimeConfiguration
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import Network.AWS.Greengrass.Types.Subscription
import Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
import Network.AWS.Greengrass.Types.TelemetryConfiguration
import Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
import Network.AWS.Greengrass.Types.VersionInformation
import Network.AWS.Greengrass.UntagResource
import Network.AWS.Greengrass.UpdateConnectivityInfo
import Network.AWS.Greengrass.UpdateConnectorDefinition
import Network.AWS.Greengrass.UpdateCoreDefinition
import Network.AWS.Greengrass.UpdateDeviceDefinition
import Network.AWS.Greengrass.UpdateFunctionDefinition
import Network.AWS.Greengrass.UpdateGroup
import Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
import Network.AWS.Greengrass.UpdateLoggerDefinition
import Network.AWS.Greengrass.UpdateResourceDefinition
import Network.AWS.Greengrass.UpdateSubscriptionDefinition
import Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
