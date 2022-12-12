{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Lens
  ( -- * Operations

    -- ** AcceptEnvironmentAccountConnection
    acceptEnvironmentAccountConnection_id,
    acceptEnvironmentAccountConnectionResponse_httpStatus,
    acceptEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** CancelComponentDeployment
    cancelComponentDeployment_componentName,
    cancelComponentDeploymentResponse_httpStatus,
    cancelComponentDeploymentResponse_component,

    -- ** CancelEnvironmentDeployment
    cancelEnvironmentDeployment_environmentName,
    cancelEnvironmentDeploymentResponse_httpStatus,
    cancelEnvironmentDeploymentResponse_environment,

    -- ** CancelServiceInstanceDeployment
    cancelServiceInstanceDeployment_serviceInstanceName,
    cancelServiceInstanceDeployment_serviceName,
    cancelServiceInstanceDeploymentResponse_httpStatus,
    cancelServiceInstanceDeploymentResponse_serviceInstance,

    -- ** CancelServicePipelineDeployment
    cancelServicePipelineDeployment_serviceName,
    cancelServicePipelineDeploymentResponse_httpStatus,
    cancelServicePipelineDeploymentResponse_pipeline,

    -- ** CreateComponent
    createComponent_description,
    createComponent_environmentName,
    createComponent_serviceInstanceName,
    createComponent_serviceName,
    createComponent_serviceSpec,
    createComponent_tags,
    createComponent_manifest,
    createComponent_name,
    createComponent_templateFile,
    createComponentResponse_httpStatus,
    createComponentResponse_component,

    -- ** CreateEnvironment
    createEnvironment_codebuildRoleArn,
    createEnvironment_componentRoleArn,
    createEnvironment_description,
    createEnvironment_environmentAccountConnectionId,
    createEnvironment_protonServiceRoleArn,
    createEnvironment_provisioningRepository,
    createEnvironment_tags,
    createEnvironment_templateMinorVersion,
    createEnvironment_name,
    createEnvironment_spec,
    createEnvironment_templateMajorVersion,
    createEnvironment_templateName,
    createEnvironmentResponse_httpStatus,
    createEnvironmentResponse_environment,

    -- ** CreateEnvironmentAccountConnection
    createEnvironmentAccountConnection_clientToken,
    createEnvironmentAccountConnection_codebuildRoleArn,
    createEnvironmentAccountConnection_componentRoleArn,
    createEnvironmentAccountConnection_roleArn,
    createEnvironmentAccountConnection_tags,
    createEnvironmentAccountConnection_environmentName,
    createEnvironmentAccountConnection_managementAccountId,
    createEnvironmentAccountConnectionResponse_httpStatus,
    createEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** CreateEnvironmentTemplate
    createEnvironmentTemplate_description,
    createEnvironmentTemplate_displayName,
    createEnvironmentTemplate_encryptionKey,
    createEnvironmentTemplate_provisioning,
    createEnvironmentTemplate_tags,
    createEnvironmentTemplate_name,
    createEnvironmentTemplateResponse_httpStatus,
    createEnvironmentTemplateResponse_environmentTemplate,

    -- ** CreateEnvironmentTemplateVersion
    createEnvironmentTemplateVersion_clientToken,
    createEnvironmentTemplateVersion_description,
    createEnvironmentTemplateVersion_majorVersion,
    createEnvironmentTemplateVersion_tags,
    createEnvironmentTemplateVersion_source,
    createEnvironmentTemplateVersion_templateName,
    createEnvironmentTemplateVersionResponse_httpStatus,
    createEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** CreateRepository
    createRepository_encryptionKey,
    createRepository_tags,
    createRepository_connectionArn,
    createRepository_name,
    createRepository_provider,
    createRepositoryResponse_httpStatus,
    createRepositoryResponse_repository,

    -- ** CreateService
    createService_branchName,
    createService_description,
    createService_repositoryConnectionArn,
    createService_repositoryId,
    createService_tags,
    createService_templateMinorVersion,
    createService_name,
    createService_spec,
    createService_templateMajorVersion,
    createService_templateName,
    createServiceResponse_httpStatus,
    createServiceResponse_service,

    -- ** CreateServiceTemplate
    createServiceTemplate_description,
    createServiceTemplate_displayName,
    createServiceTemplate_encryptionKey,
    createServiceTemplate_pipelineProvisioning,
    createServiceTemplate_tags,
    createServiceTemplate_name,
    createServiceTemplateResponse_httpStatus,
    createServiceTemplateResponse_serviceTemplate,

    -- ** CreateServiceTemplateVersion
    createServiceTemplateVersion_clientToken,
    createServiceTemplateVersion_description,
    createServiceTemplateVersion_majorVersion,
    createServiceTemplateVersion_supportedComponentSources,
    createServiceTemplateVersion_tags,
    createServiceTemplateVersion_compatibleEnvironmentTemplates,
    createServiceTemplateVersion_source,
    createServiceTemplateVersion_templateName,
    createServiceTemplateVersionResponse_httpStatus,
    createServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** CreateTemplateSyncConfig
    createTemplateSyncConfig_subdirectory,
    createTemplateSyncConfig_branch,
    createTemplateSyncConfig_repositoryName,
    createTemplateSyncConfig_repositoryProvider,
    createTemplateSyncConfig_templateName,
    createTemplateSyncConfig_templateType,
    createTemplateSyncConfigResponse_templateSyncConfig,
    createTemplateSyncConfigResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_name,
    deleteComponentResponse_component,
    deleteComponentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_name,
    deleteEnvironmentResponse_environment,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironmentAccountConnection
    deleteEnvironmentAccountConnection_id,
    deleteEnvironmentAccountConnectionResponse_environmentAccountConnection,
    deleteEnvironmentAccountConnectionResponse_httpStatus,

    -- ** DeleteEnvironmentTemplate
    deleteEnvironmentTemplate_name,
    deleteEnvironmentTemplateResponse_environmentTemplate,
    deleteEnvironmentTemplateResponse_httpStatus,

    -- ** DeleteEnvironmentTemplateVersion
    deleteEnvironmentTemplateVersion_majorVersion,
    deleteEnvironmentTemplateVersion_minorVersion,
    deleteEnvironmentTemplateVersion_templateName,
    deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion,
    deleteEnvironmentTemplateVersionResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_name,
    deleteRepository_provider,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** DeleteService
    deleteService_name,
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,

    -- ** DeleteServiceTemplate
    deleteServiceTemplate_name,
    deleteServiceTemplateResponse_serviceTemplate,
    deleteServiceTemplateResponse_httpStatus,

    -- ** DeleteServiceTemplateVersion
    deleteServiceTemplateVersion_majorVersion,
    deleteServiceTemplateVersion_minorVersion,
    deleteServiceTemplateVersion_templateName,
    deleteServiceTemplateVersionResponse_serviceTemplateVersion,
    deleteServiceTemplateVersionResponse_httpStatus,

    -- ** DeleteTemplateSyncConfig
    deleteTemplateSyncConfig_templateName,
    deleteTemplateSyncConfig_templateType,
    deleteTemplateSyncConfigResponse_templateSyncConfig,
    deleteTemplateSyncConfigResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** GetComponent
    getComponent_name,
    getComponentResponse_component,
    getComponentResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_name,
    getEnvironmentResponse_httpStatus,
    getEnvironmentResponse_environment,

    -- ** GetEnvironmentAccountConnection
    getEnvironmentAccountConnection_id,
    getEnvironmentAccountConnectionResponse_httpStatus,
    getEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** GetEnvironmentTemplate
    getEnvironmentTemplate_name,
    getEnvironmentTemplateResponse_httpStatus,
    getEnvironmentTemplateResponse_environmentTemplate,

    -- ** GetEnvironmentTemplateVersion
    getEnvironmentTemplateVersion_majorVersion,
    getEnvironmentTemplateVersion_minorVersion,
    getEnvironmentTemplateVersion_templateName,
    getEnvironmentTemplateVersionResponse_httpStatus,
    getEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** GetRepository
    getRepository_name,
    getRepository_provider,
    getRepositoryResponse_httpStatus,
    getRepositoryResponse_repository,

    -- ** GetRepositorySyncStatus
    getRepositorySyncStatus_branch,
    getRepositorySyncStatus_repositoryName,
    getRepositorySyncStatus_repositoryProvider,
    getRepositorySyncStatus_syncType,
    getRepositorySyncStatusResponse_latestSync,
    getRepositorySyncStatusResponse_httpStatus,

    -- ** GetService
    getService_name,
    getServiceResponse_service,
    getServiceResponse_httpStatus,

    -- ** GetServiceInstance
    getServiceInstance_name,
    getServiceInstance_serviceName,
    getServiceInstanceResponse_httpStatus,
    getServiceInstanceResponse_serviceInstance,

    -- ** GetServiceTemplate
    getServiceTemplate_name,
    getServiceTemplateResponse_httpStatus,
    getServiceTemplateResponse_serviceTemplate,

    -- ** GetServiceTemplateVersion
    getServiceTemplateVersion_majorVersion,
    getServiceTemplateVersion_minorVersion,
    getServiceTemplateVersion_templateName,
    getServiceTemplateVersionResponse_httpStatus,
    getServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** GetTemplateSyncConfig
    getTemplateSyncConfig_templateName,
    getTemplateSyncConfig_templateType,
    getTemplateSyncConfigResponse_templateSyncConfig,
    getTemplateSyncConfigResponse_httpStatus,

    -- ** GetTemplateSyncStatus
    getTemplateSyncStatus_templateName,
    getTemplateSyncStatus_templateType,
    getTemplateSyncStatus_templateVersion,
    getTemplateSyncStatusResponse_desiredState,
    getTemplateSyncStatusResponse_latestSuccessfulSync,
    getTemplateSyncStatusResponse_latestSync,
    getTemplateSyncStatusResponse_httpStatus,

    -- ** ListComponentOutputs
    listComponentOutputs_nextToken,
    listComponentOutputs_componentName,
    listComponentOutputsResponse_nextToken,
    listComponentOutputsResponse_httpStatus,
    listComponentOutputsResponse_outputs,

    -- ** ListComponentProvisionedResources
    listComponentProvisionedResources_nextToken,
    listComponentProvisionedResources_componentName,
    listComponentProvisionedResourcesResponse_nextToken,
    listComponentProvisionedResourcesResponse_httpStatus,
    listComponentProvisionedResourcesResponse_provisionedResources,

    -- ** ListComponents
    listComponents_environmentName,
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_serviceInstanceName,
    listComponents_serviceName,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,
    listComponentsResponse_components,

    -- ** ListEnvironmentAccountConnections
    listEnvironmentAccountConnections_environmentName,
    listEnvironmentAccountConnections_maxResults,
    listEnvironmentAccountConnections_nextToken,
    listEnvironmentAccountConnections_statuses,
    listEnvironmentAccountConnections_requestedBy,
    listEnvironmentAccountConnectionsResponse_nextToken,
    listEnvironmentAccountConnectionsResponse_httpStatus,
    listEnvironmentAccountConnectionsResponse_environmentAccountConnections,

    -- ** ListEnvironmentOutputs
    listEnvironmentOutputs_nextToken,
    listEnvironmentOutputs_environmentName,
    listEnvironmentOutputsResponse_nextToken,
    listEnvironmentOutputsResponse_httpStatus,
    listEnvironmentOutputsResponse_outputs,

    -- ** ListEnvironmentProvisionedResources
    listEnvironmentProvisionedResources_nextToken,
    listEnvironmentProvisionedResources_environmentName,
    listEnvironmentProvisionedResourcesResponse_nextToken,
    listEnvironmentProvisionedResourcesResponse_httpStatus,
    listEnvironmentProvisionedResourcesResponse_provisionedResources,

    -- ** ListEnvironmentTemplateVersions
    listEnvironmentTemplateVersions_majorVersion,
    listEnvironmentTemplateVersions_maxResults,
    listEnvironmentTemplateVersions_nextToken,
    listEnvironmentTemplateVersions_templateName,
    listEnvironmentTemplateVersionsResponse_nextToken,
    listEnvironmentTemplateVersionsResponse_httpStatus,
    listEnvironmentTemplateVersionsResponse_templateVersions,

    -- ** ListEnvironmentTemplates
    listEnvironmentTemplates_maxResults,
    listEnvironmentTemplates_nextToken,
    listEnvironmentTemplatesResponse_nextToken,
    listEnvironmentTemplatesResponse_httpStatus,
    listEnvironmentTemplatesResponse_templates,

    -- ** ListEnvironments
    listEnvironments_environmentTemplates,
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,

    -- ** ListRepositories
    listRepositories_maxResults,
    listRepositories_nextToken,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_httpStatus,
    listRepositoriesResponse_repositories,

    -- ** ListRepositorySyncDefinitions
    listRepositorySyncDefinitions_nextToken,
    listRepositorySyncDefinitions_repositoryName,
    listRepositorySyncDefinitions_repositoryProvider,
    listRepositorySyncDefinitions_syncType,
    listRepositorySyncDefinitionsResponse_nextToken,
    listRepositorySyncDefinitionsResponse_httpStatus,
    listRepositorySyncDefinitionsResponse_syncDefinitions,

    -- ** ListServiceInstanceOutputs
    listServiceInstanceOutputs_nextToken,
    listServiceInstanceOutputs_serviceInstanceName,
    listServiceInstanceOutputs_serviceName,
    listServiceInstanceOutputsResponse_nextToken,
    listServiceInstanceOutputsResponse_httpStatus,
    listServiceInstanceOutputsResponse_outputs,

    -- ** ListServiceInstanceProvisionedResources
    listServiceInstanceProvisionedResources_nextToken,
    listServiceInstanceProvisionedResources_serviceInstanceName,
    listServiceInstanceProvisionedResources_serviceName,
    listServiceInstanceProvisionedResourcesResponse_nextToken,
    listServiceInstanceProvisionedResourcesResponse_httpStatus,
    listServiceInstanceProvisionedResourcesResponse_provisionedResources,

    -- ** ListServiceInstances
    listServiceInstances_filters,
    listServiceInstances_maxResults,
    listServiceInstances_nextToken,
    listServiceInstances_serviceName,
    listServiceInstances_sortBy,
    listServiceInstances_sortOrder,
    listServiceInstancesResponse_nextToken,
    listServiceInstancesResponse_httpStatus,
    listServiceInstancesResponse_serviceInstances,

    -- ** ListServicePipelineOutputs
    listServicePipelineOutputs_nextToken,
    listServicePipelineOutputs_serviceName,
    listServicePipelineOutputsResponse_nextToken,
    listServicePipelineOutputsResponse_httpStatus,
    listServicePipelineOutputsResponse_outputs,

    -- ** ListServicePipelineProvisionedResources
    listServicePipelineProvisionedResources_nextToken,
    listServicePipelineProvisionedResources_serviceName,
    listServicePipelineProvisionedResourcesResponse_nextToken,
    listServicePipelineProvisionedResourcesResponse_httpStatus,
    listServicePipelineProvisionedResourcesResponse_provisionedResources,

    -- ** ListServiceTemplateVersions
    listServiceTemplateVersions_majorVersion,
    listServiceTemplateVersions_maxResults,
    listServiceTemplateVersions_nextToken,
    listServiceTemplateVersions_templateName,
    listServiceTemplateVersionsResponse_nextToken,
    listServiceTemplateVersionsResponse_httpStatus,
    listServiceTemplateVersionsResponse_templateVersions,

    -- ** ListServiceTemplates
    listServiceTemplates_maxResults,
    listServiceTemplates_nextToken,
    listServiceTemplatesResponse_nextToken,
    listServiceTemplatesResponse_httpStatus,
    listServiceTemplatesResponse_templates,

    -- ** ListServices
    listServices_maxResults,
    listServices_nextToken,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,
    listServicesResponse_services,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** NotifyResourceDeploymentStatusChange
    notifyResourceDeploymentStatusChange_deploymentId,
    notifyResourceDeploymentStatusChange_outputs,
    notifyResourceDeploymentStatusChange_status,
    notifyResourceDeploymentStatusChange_statusMessage,
    notifyResourceDeploymentStatusChange_resourceArn,
    notifyResourceDeploymentStatusChangeResponse_httpStatus,

    -- ** RejectEnvironmentAccountConnection
    rejectEnvironmentAccountConnection_id,
    rejectEnvironmentAccountConnectionResponse_httpStatus,
    rejectEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccountSettings
    updateAccountSettings_deletePipelineProvisioningRepository,
    updateAccountSettings_pipelineCodebuildRoleArn,
    updateAccountSettings_pipelineProvisioningRepository,
    updateAccountSettings_pipelineServiceRoleArn,
    updateAccountSettingsResponse_httpStatus,
    updateAccountSettingsResponse_accountSettings,

    -- ** UpdateComponent
    updateComponent_description,
    updateComponent_serviceInstanceName,
    updateComponent_serviceName,
    updateComponent_serviceSpec,
    updateComponent_templateFile,
    updateComponent_deploymentType,
    updateComponent_name,
    updateComponentResponse_httpStatus,
    updateComponentResponse_component,

    -- ** UpdateEnvironment
    updateEnvironment_codebuildRoleArn,
    updateEnvironment_componentRoleArn,
    updateEnvironment_description,
    updateEnvironment_environmentAccountConnectionId,
    updateEnvironment_protonServiceRoleArn,
    updateEnvironment_provisioningRepository,
    updateEnvironment_spec,
    updateEnvironment_templateMajorVersion,
    updateEnvironment_templateMinorVersion,
    updateEnvironment_deploymentType,
    updateEnvironment_name,
    updateEnvironmentResponse_httpStatus,
    updateEnvironmentResponse_environment,

    -- ** UpdateEnvironmentAccountConnection
    updateEnvironmentAccountConnection_codebuildRoleArn,
    updateEnvironmentAccountConnection_componentRoleArn,
    updateEnvironmentAccountConnection_roleArn,
    updateEnvironmentAccountConnection_id,
    updateEnvironmentAccountConnectionResponse_httpStatus,
    updateEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** UpdateEnvironmentTemplate
    updateEnvironmentTemplate_description,
    updateEnvironmentTemplate_displayName,
    updateEnvironmentTemplate_name,
    updateEnvironmentTemplateResponse_httpStatus,
    updateEnvironmentTemplateResponse_environmentTemplate,

    -- ** UpdateEnvironmentTemplateVersion
    updateEnvironmentTemplateVersion_description,
    updateEnvironmentTemplateVersion_status,
    updateEnvironmentTemplateVersion_majorVersion,
    updateEnvironmentTemplateVersion_minorVersion,
    updateEnvironmentTemplateVersion_templateName,
    updateEnvironmentTemplateVersionResponse_httpStatus,
    updateEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** UpdateService
    updateService_description,
    updateService_spec,
    updateService_name,
    updateServiceResponse_httpStatus,
    updateServiceResponse_service,

    -- ** UpdateServiceInstance
    updateServiceInstance_spec,
    updateServiceInstance_templateMajorVersion,
    updateServiceInstance_templateMinorVersion,
    updateServiceInstance_deploymentType,
    updateServiceInstance_name,
    updateServiceInstance_serviceName,
    updateServiceInstanceResponse_httpStatus,
    updateServiceInstanceResponse_serviceInstance,

    -- ** UpdateServicePipeline
    updateServicePipeline_templateMajorVersion,
    updateServicePipeline_templateMinorVersion,
    updateServicePipeline_deploymentType,
    updateServicePipeline_serviceName,
    updateServicePipeline_spec,
    updateServicePipelineResponse_httpStatus,
    updateServicePipelineResponse_pipeline,

    -- ** UpdateServiceTemplate
    updateServiceTemplate_description,
    updateServiceTemplate_displayName,
    updateServiceTemplate_name,
    updateServiceTemplateResponse_httpStatus,
    updateServiceTemplateResponse_serviceTemplate,

    -- ** UpdateServiceTemplateVersion
    updateServiceTemplateVersion_compatibleEnvironmentTemplates,
    updateServiceTemplateVersion_description,
    updateServiceTemplateVersion_status,
    updateServiceTemplateVersion_supportedComponentSources,
    updateServiceTemplateVersion_majorVersion,
    updateServiceTemplateVersion_minorVersion,
    updateServiceTemplateVersion_templateName,
    updateServiceTemplateVersionResponse_httpStatus,
    updateServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** UpdateTemplateSyncConfig
    updateTemplateSyncConfig_subdirectory,
    updateTemplateSyncConfig_branch,
    updateTemplateSyncConfig_repositoryName,
    updateTemplateSyncConfig_repositoryProvider,
    updateTemplateSyncConfig_templateName,
    updateTemplateSyncConfig_templateType,
    updateTemplateSyncConfigResponse_templateSyncConfig,
    updateTemplateSyncConfigResponse_httpStatus,

    -- * Types

    -- ** AccountSettings
    accountSettings_pipelineCodebuildRoleArn,
    accountSettings_pipelineProvisioningRepository,
    accountSettings_pipelineServiceRoleArn,

    -- ** CompatibleEnvironmentTemplate
    compatibleEnvironmentTemplate_majorVersion,
    compatibleEnvironmentTemplate_templateName,

    -- ** CompatibleEnvironmentTemplateInput
    compatibleEnvironmentTemplateInput_majorVersion,
    compatibleEnvironmentTemplateInput_templateName,

    -- ** Component
    component_deploymentStatusMessage,
    component_description,
    component_lastDeploymentAttemptedAt,
    component_lastDeploymentSucceededAt,
    component_serviceInstanceName,
    component_serviceName,
    component_serviceSpec,
    component_arn,
    component_createdAt,
    component_deploymentStatus,
    component_environmentName,
    component_lastModifiedAt,
    component_name,

    -- ** ComponentSummary
    componentSummary_deploymentStatusMessage,
    componentSummary_lastDeploymentAttemptedAt,
    componentSummary_lastDeploymentSucceededAt,
    componentSummary_serviceInstanceName,
    componentSummary_serviceName,
    componentSummary_arn,
    componentSummary_createdAt,
    componentSummary_deploymentStatus,
    componentSummary_environmentName,
    componentSummary_lastModifiedAt,
    componentSummary_name,

    -- ** Environment
    environment_codebuildRoleArn,
    environment_componentRoleArn,
    environment_deploymentStatusMessage,
    environment_description,
    environment_environmentAccountConnectionId,
    environment_environmentAccountId,
    environment_protonServiceRoleArn,
    environment_provisioning,
    environment_provisioningRepository,
    environment_spec,
    environment_arn,
    environment_createdAt,
    environment_deploymentStatus,
    environment_lastDeploymentAttemptedAt,
    environment_lastDeploymentSucceededAt,
    environment_name,
    environment_templateMajorVersion,
    environment_templateMinorVersion,
    environment_templateName,

    -- ** EnvironmentAccountConnection
    environmentAccountConnection_codebuildRoleArn,
    environmentAccountConnection_componentRoleArn,
    environmentAccountConnection_arn,
    environmentAccountConnection_environmentAccountId,
    environmentAccountConnection_environmentName,
    environmentAccountConnection_id,
    environmentAccountConnection_lastModifiedAt,
    environmentAccountConnection_managementAccountId,
    environmentAccountConnection_requestedAt,
    environmentAccountConnection_roleArn,
    environmentAccountConnection_status,

    -- ** EnvironmentAccountConnectionSummary
    environmentAccountConnectionSummary_componentRoleArn,
    environmentAccountConnectionSummary_arn,
    environmentAccountConnectionSummary_environmentAccountId,
    environmentAccountConnectionSummary_environmentName,
    environmentAccountConnectionSummary_id,
    environmentAccountConnectionSummary_lastModifiedAt,
    environmentAccountConnectionSummary_managementAccountId,
    environmentAccountConnectionSummary_requestedAt,
    environmentAccountConnectionSummary_roleArn,
    environmentAccountConnectionSummary_status,

    -- ** EnvironmentSummary
    environmentSummary_componentRoleArn,
    environmentSummary_deploymentStatusMessage,
    environmentSummary_description,
    environmentSummary_environmentAccountConnectionId,
    environmentSummary_environmentAccountId,
    environmentSummary_protonServiceRoleArn,
    environmentSummary_provisioning,
    environmentSummary_arn,
    environmentSummary_createdAt,
    environmentSummary_deploymentStatus,
    environmentSummary_lastDeploymentAttemptedAt,
    environmentSummary_lastDeploymentSucceededAt,
    environmentSummary_name,
    environmentSummary_templateMajorVersion,
    environmentSummary_templateMinorVersion,
    environmentSummary_templateName,

    -- ** EnvironmentTemplate
    environmentTemplate_description,
    environmentTemplate_displayName,
    environmentTemplate_encryptionKey,
    environmentTemplate_provisioning,
    environmentTemplate_recommendedVersion,
    environmentTemplate_arn,
    environmentTemplate_createdAt,
    environmentTemplate_lastModifiedAt,
    environmentTemplate_name,

    -- ** EnvironmentTemplateFilter
    environmentTemplateFilter_majorVersion,
    environmentTemplateFilter_templateName,

    -- ** EnvironmentTemplateSummary
    environmentTemplateSummary_description,
    environmentTemplateSummary_displayName,
    environmentTemplateSummary_provisioning,
    environmentTemplateSummary_recommendedVersion,
    environmentTemplateSummary_arn,
    environmentTemplateSummary_createdAt,
    environmentTemplateSummary_lastModifiedAt,
    environmentTemplateSummary_name,

    -- ** EnvironmentTemplateVersion
    environmentTemplateVersion_description,
    environmentTemplateVersion_recommendedMinorVersion,
    environmentTemplateVersion_schema,
    environmentTemplateVersion_statusMessage,
    environmentTemplateVersion_arn,
    environmentTemplateVersion_createdAt,
    environmentTemplateVersion_lastModifiedAt,
    environmentTemplateVersion_majorVersion,
    environmentTemplateVersion_minorVersion,
    environmentTemplateVersion_status,
    environmentTemplateVersion_templateName,

    -- ** EnvironmentTemplateVersionSummary
    environmentTemplateVersionSummary_description,
    environmentTemplateVersionSummary_recommendedMinorVersion,
    environmentTemplateVersionSummary_statusMessage,
    environmentTemplateVersionSummary_arn,
    environmentTemplateVersionSummary_createdAt,
    environmentTemplateVersionSummary_lastModifiedAt,
    environmentTemplateVersionSummary_majorVersion,
    environmentTemplateVersionSummary_minorVersion,
    environmentTemplateVersionSummary_status,
    environmentTemplateVersionSummary_templateName,

    -- ** ListServiceInstancesFilter
    listServiceInstancesFilter_key,
    listServiceInstancesFilter_value,

    -- ** Output
    output_key,
    output_valueString,

    -- ** ProvisionedResource
    provisionedResource_identifier,
    provisionedResource_name,
    provisionedResource_provisioningEngine,

    -- ** Repository
    repository_encryptionKey,
    repository_arn,
    repository_connectionArn,
    repository_name,
    repository_provider,

    -- ** RepositoryBranch
    repositoryBranch_arn,
    repositoryBranch_branch,
    repositoryBranch_name,
    repositoryBranch_provider,

    -- ** RepositoryBranchInput
    repositoryBranchInput_branch,
    repositoryBranchInput_name,
    repositoryBranchInput_provider,

    -- ** RepositorySummary
    repositorySummary_arn,
    repositorySummary_name,
    repositorySummary_provider,

    -- ** RepositorySyncAttempt
    repositorySyncAttempt_events,
    repositorySyncAttempt_startedAt,
    repositorySyncAttempt_status,

    -- ** RepositorySyncDefinition
    repositorySyncDefinition_branch,
    repositorySyncDefinition_directory,
    repositorySyncDefinition_parent,
    repositorySyncDefinition_target,

    -- ** RepositorySyncEvent
    repositorySyncEvent_externalId,
    repositorySyncEvent_event,
    repositorySyncEvent_time,
    repositorySyncEvent_type,

    -- ** ResourceSyncAttempt
    resourceSyncAttempt_events,
    resourceSyncAttempt_initialRevision,
    resourceSyncAttempt_startedAt,
    resourceSyncAttempt_status,
    resourceSyncAttempt_target,
    resourceSyncAttempt_targetRevision,

    -- ** ResourceSyncEvent
    resourceSyncEvent_externalId,
    resourceSyncEvent_event,
    resourceSyncEvent_time,
    resourceSyncEvent_type,

    -- ** Revision
    revision_branch,
    revision_directory,
    revision_repositoryName,
    revision_repositoryProvider,
    revision_sha,

    -- ** S3ObjectSource
    s3ObjectSource_bucket,
    s3ObjectSource_key,

    -- ** Service
    service_branchName,
    service_description,
    service_pipeline,
    service_repositoryConnectionArn,
    service_repositoryId,
    service_statusMessage,
    service_arn,
    service_createdAt,
    service_lastModifiedAt,
    service_name,
    service_spec,
    service_status,
    service_templateName,

    -- ** ServiceInstance
    serviceInstance_deploymentStatusMessage,
    serviceInstance_spec,
    serviceInstance_arn,
    serviceInstance_createdAt,
    serviceInstance_deploymentStatus,
    serviceInstance_environmentName,
    serviceInstance_lastDeploymentAttemptedAt,
    serviceInstance_lastDeploymentSucceededAt,
    serviceInstance_name,
    serviceInstance_serviceName,
    serviceInstance_templateMajorVersion,
    serviceInstance_templateMinorVersion,
    serviceInstance_templateName,

    -- ** ServiceInstanceSummary
    serviceInstanceSummary_deploymentStatusMessage,
    serviceInstanceSummary_arn,
    serviceInstanceSummary_createdAt,
    serviceInstanceSummary_deploymentStatus,
    serviceInstanceSummary_environmentName,
    serviceInstanceSummary_lastDeploymentAttemptedAt,
    serviceInstanceSummary_lastDeploymentSucceededAt,
    serviceInstanceSummary_name,
    serviceInstanceSummary_serviceName,
    serviceInstanceSummary_templateMajorVersion,
    serviceInstanceSummary_templateMinorVersion,
    serviceInstanceSummary_templateName,

    -- ** ServicePipeline
    servicePipeline_deploymentStatusMessage,
    servicePipeline_spec,
    servicePipeline_arn,
    servicePipeline_createdAt,
    servicePipeline_deploymentStatus,
    servicePipeline_lastDeploymentAttemptedAt,
    servicePipeline_lastDeploymentSucceededAt,
    servicePipeline_templateMajorVersion,
    servicePipeline_templateMinorVersion,
    servicePipeline_templateName,

    -- ** ServiceSummary
    serviceSummary_description,
    serviceSummary_statusMessage,
    serviceSummary_arn,
    serviceSummary_createdAt,
    serviceSummary_lastModifiedAt,
    serviceSummary_name,
    serviceSummary_status,
    serviceSummary_templateName,

    -- ** ServiceTemplate
    serviceTemplate_description,
    serviceTemplate_displayName,
    serviceTemplate_encryptionKey,
    serviceTemplate_pipelineProvisioning,
    serviceTemplate_recommendedVersion,
    serviceTemplate_arn,
    serviceTemplate_createdAt,
    serviceTemplate_lastModifiedAt,
    serviceTemplate_name,

    -- ** ServiceTemplateSummary
    serviceTemplateSummary_description,
    serviceTemplateSummary_displayName,
    serviceTemplateSummary_pipelineProvisioning,
    serviceTemplateSummary_recommendedVersion,
    serviceTemplateSummary_arn,
    serviceTemplateSummary_createdAt,
    serviceTemplateSummary_lastModifiedAt,
    serviceTemplateSummary_name,

    -- ** ServiceTemplateVersion
    serviceTemplateVersion_description,
    serviceTemplateVersion_recommendedMinorVersion,
    serviceTemplateVersion_schema,
    serviceTemplateVersion_statusMessage,
    serviceTemplateVersion_supportedComponentSources,
    serviceTemplateVersion_arn,
    serviceTemplateVersion_compatibleEnvironmentTemplates,
    serviceTemplateVersion_createdAt,
    serviceTemplateVersion_lastModifiedAt,
    serviceTemplateVersion_majorVersion,
    serviceTemplateVersion_minorVersion,
    serviceTemplateVersion_status,
    serviceTemplateVersion_templateName,

    -- ** ServiceTemplateVersionSummary
    serviceTemplateVersionSummary_description,
    serviceTemplateVersionSummary_recommendedMinorVersion,
    serviceTemplateVersionSummary_statusMessage,
    serviceTemplateVersionSummary_arn,
    serviceTemplateVersionSummary_createdAt,
    serviceTemplateVersionSummary_lastModifiedAt,
    serviceTemplateVersionSummary_majorVersion,
    serviceTemplateVersionSummary_minorVersion,
    serviceTemplateVersionSummary_status,
    serviceTemplateVersionSummary_templateName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateSyncConfig
    templateSyncConfig_subdirectory,
    templateSyncConfig_branch,
    templateSyncConfig_repositoryName,
    templateSyncConfig_repositoryProvider,
    templateSyncConfig_templateName,
    templateSyncConfig_templateType,

    -- ** TemplateVersionSourceInput
    templateVersionSourceInput_s3,
  )
where

import Amazonka.Proton.AcceptEnvironmentAccountConnection
import Amazonka.Proton.CancelComponentDeployment
import Amazonka.Proton.CancelEnvironmentDeployment
import Amazonka.Proton.CancelServiceInstanceDeployment
import Amazonka.Proton.CancelServicePipelineDeployment
import Amazonka.Proton.CreateComponent
import Amazonka.Proton.CreateEnvironment
import Amazonka.Proton.CreateEnvironmentAccountConnection
import Amazonka.Proton.CreateEnvironmentTemplate
import Amazonka.Proton.CreateEnvironmentTemplateVersion
import Amazonka.Proton.CreateRepository
import Amazonka.Proton.CreateService
import Amazonka.Proton.CreateServiceTemplate
import Amazonka.Proton.CreateServiceTemplateVersion
import Amazonka.Proton.CreateTemplateSyncConfig
import Amazonka.Proton.DeleteComponent
import Amazonka.Proton.DeleteEnvironment
import Amazonka.Proton.DeleteEnvironmentAccountConnection
import Amazonka.Proton.DeleteEnvironmentTemplate
import Amazonka.Proton.DeleteEnvironmentTemplateVersion
import Amazonka.Proton.DeleteRepository
import Amazonka.Proton.DeleteService
import Amazonka.Proton.DeleteServiceTemplate
import Amazonka.Proton.DeleteServiceTemplateVersion
import Amazonka.Proton.DeleteTemplateSyncConfig
import Amazonka.Proton.GetAccountSettings
import Amazonka.Proton.GetComponent
import Amazonka.Proton.GetEnvironment
import Amazonka.Proton.GetEnvironmentAccountConnection
import Amazonka.Proton.GetEnvironmentTemplate
import Amazonka.Proton.GetEnvironmentTemplateVersion
import Amazonka.Proton.GetRepository
import Amazonka.Proton.GetRepositorySyncStatus
import Amazonka.Proton.GetService
import Amazonka.Proton.GetServiceInstance
import Amazonka.Proton.GetServiceTemplate
import Amazonka.Proton.GetServiceTemplateVersion
import Amazonka.Proton.GetTemplateSyncConfig
import Amazonka.Proton.GetTemplateSyncStatus
import Amazonka.Proton.ListComponentOutputs
import Amazonka.Proton.ListComponentProvisionedResources
import Amazonka.Proton.ListComponents
import Amazonka.Proton.ListEnvironmentAccountConnections
import Amazonka.Proton.ListEnvironmentOutputs
import Amazonka.Proton.ListEnvironmentProvisionedResources
import Amazonka.Proton.ListEnvironmentTemplateVersions
import Amazonka.Proton.ListEnvironmentTemplates
import Amazonka.Proton.ListEnvironments
import Amazonka.Proton.ListRepositories
import Amazonka.Proton.ListRepositorySyncDefinitions
import Amazonka.Proton.ListServiceInstanceOutputs
import Amazonka.Proton.ListServiceInstanceProvisionedResources
import Amazonka.Proton.ListServiceInstances
import Amazonka.Proton.ListServicePipelineOutputs
import Amazonka.Proton.ListServicePipelineProvisionedResources
import Amazonka.Proton.ListServiceTemplateVersions
import Amazonka.Proton.ListServiceTemplates
import Amazonka.Proton.ListServices
import Amazonka.Proton.ListTagsForResource
import Amazonka.Proton.NotifyResourceDeploymentStatusChange
import Amazonka.Proton.RejectEnvironmentAccountConnection
import Amazonka.Proton.TagResource
import Amazonka.Proton.Types.AccountSettings
import Amazonka.Proton.Types.CompatibleEnvironmentTemplate
import Amazonka.Proton.Types.CompatibleEnvironmentTemplateInput
import Amazonka.Proton.Types.Component
import Amazonka.Proton.Types.ComponentSummary
import Amazonka.Proton.Types.Environment
import Amazonka.Proton.Types.EnvironmentAccountConnection
import Amazonka.Proton.Types.EnvironmentAccountConnectionSummary
import Amazonka.Proton.Types.EnvironmentSummary
import Amazonka.Proton.Types.EnvironmentTemplate
import Amazonka.Proton.Types.EnvironmentTemplateFilter
import Amazonka.Proton.Types.EnvironmentTemplateSummary
import Amazonka.Proton.Types.EnvironmentTemplateVersion
import Amazonka.Proton.Types.EnvironmentTemplateVersionSummary
import Amazonka.Proton.Types.ListServiceInstancesFilter
import Amazonka.Proton.Types.Output
import Amazonka.Proton.Types.ProvisionedResource
import Amazonka.Proton.Types.Repository
import Amazonka.Proton.Types.RepositoryBranch
import Amazonka.Proton.Types.RepositoryBranchInput
import Amazonka.Proton.Types.RepositorySummary
import Amazonka.Proton.Types.RepositorySyncAttempt
import Amazonka.Proton.Types.RepositorySyncDefinition
import Amazonka.Proton.Types.RepositorySyncEvent
import Amazonka.Proton.Types.ResourceSyncAttempt
import Amazonka.Proton.Types.ResourceSyncEvent
import Amazonka.Proton.Types.Revision
import Amazonka.Proton.Types.S3ObjectSource
import Amazonka.Proton.Types.Service
import Amazonka.Proton.Types.ServiceInstance
import Amazonka.Proton.Types.ServiceInstanceSummary
import Amazonka.Proton.Types.ServicePipeline
import Amazonka.Proton.Types.ServiceSummary
import Amazonka.Proton.Types.ServiceTemplate
import Amazonka.Proton.Types.ServiceTemplateSummary
import Amazonka.Proton.Types.ServiceTemplateVersion
import Amazonka.Proton.Types.ServiceTemplateVersionSummary
import Amazonka.Proton.Types.Tag
import Amazonka.Proton.Types.TemplateSyncConfig
import Amazonka.Proton.Types.TemplateVersionSourceInput
import Amazonka.Proton.UntagResource
import Amazonka.Proton.UpdateAccountSettings
import Amazonka.Proton.UpdateComponent
import Amazonka.Proton.UpdateEnvironment
import Amazonka.Proton.UpdateEnvironmentAccountConnection
import Amazonka.Proton.UpdateEnvironmentTemplate
import Amazonka.Proton.UpdateEnvironmentTemplateVersion
import Amazonka.Proton.UpdateService
import Amazonka.Proton.UpdateServiceInstance
import Amazonka.Proton.UpdateServicePipeline
import Amazonka.Proton.UpdateServiceTemplate
import Amazonka.Proton.UpdateServiceTemplateVersion
import Amazonka.Proton.UpdateTemplateSyncConfig
