{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Proton.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Proton.Lens
  ( -- * Operations

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,
    listServicesResponse_services,

    -- ** ListEnvironments
    listEnvironments_environmentTemplates,
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,

    -- ** UpdateEnvironment
    updateEnvironment_protonServiceRoleArn,
    updateEnvironment_environmentAccountConnectionId,
    updateEnvironment_spec,
    updateEnvironment_templateMinorVersion,
    updateEnvironment_description,
    updateEnvironment_templateMajorVersion,
    updateEnvironment_deploymentType,
    updateEnvironment_name,
    updateEnvironmentResponse_httpStatus,
    updateEnvironmentResponse_environment,

    -- ** DeleteEnvironment
    deleteEnvironment_name,
    deleteEnvironmentResponse_environment,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteService
    deleteService_name,
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,

    -- ** UpdateService
    updateService_spec,
    updateService_description,
    updateService_name,
    updateServiceResponse_httpStatus,
    updateServiceResponse_service,

    -- ** GetServiceInstance
    getServiceInstance_name,
    getServiceInstance_serviceName,
    getServiceInstanceResponse_httpStatus,
    getServiceInstanceResponse_serviceInstance,

    -- ** AcceptEnvironmentAccountConnection
    acceptEnvironmentAccountConnection_id,
    acceptEnvironmentAccountConnectionResponse_httpStatus,
    acceptEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** UpdateAccountSettings
    updateAccountSettings_pipelineServiceRoleArn,
    updateAccountSettingsResponse_httpStatus,
    updateAccountSettingsResponse_accountSettings,

    -- ** RejectEnvironmentAccountConnection
    rejectEnvironmentAccountConnection_id,
    rejectEnvironmentAccountConnectionResponse_httpStatus,
    rejectEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** ListServiceInstances
    listServiceInstances_nextToken,
    listServiceInstances_serviceName,
    listServiceInstances_maxResults,
    listServiceInstancesResponse_nextToken,
    listServiceInstancesResponse_httpStatus,
    listServiceInstancesResponse_serviceInstances,

    -- ** CancelServicePipelineDeployment
    cancelServicePipelineDeployment_serviceName,
    cancelServicePipelineDeploymentResponse_httpStatus,
    cancelServicePipelineDeploymentResponse_pipeline,

    -- ** CreateServiceTemplateVersion
    createServiceTemplateVersion_clientToken,
    createServiceTemplateVersion_majorVersion,
    createServiceTemplateVersion_description,
    createServiceTemplateVersion_tags,
    createServiceTemplateVersion_compatibleEnvironmentTemplates,
    createServiceTemplateVersion_source,
    createServiceTemplateVersion_templateName,
    createServiceTemplateVersionResponse_httpStatus,
    createServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** GetServiceTemplate
    getServiceTemplate_name,
    getServiceTemplateResponse_httpStatus,
    getServiceTemplateResponse_serviceTemplate,

    -- ** CreateEnvironmentTemplateVersion
    createEnvironmentTemplateVersion_clientToken,
    createEnvironmentTemplateVersion_majorVersion,
    createEnvironmentTemplateVersion_description,
    createEnvironmentTemplateVersion_tags,
    createEnvironmentTemplateVersion_source,
    createEnvironmentTemplateVersion_templateName,
    createEnvironmentTemplateVersionResponse_httpStatus,
    createEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** CancelServiceInstanceDeployment
    cancelServiceInstanceDeployment_serviceInstanceName,
    cancelServiceInstanceDeployment_serviceName,
    cancelServiceInstanceDeploymentResponse_httpStatus,
    cancelServiceInstanceDeploymentResponse_serviceInstance,

    -- ** GetEnvironmentTemplate
    getEnvironmentTemplate_name,
    getEnvironmentTemplateResponse_httpStatus,
    getEnvironmentTemplateResponse_environmentTemplate,

    -- ** UpdateServicePipeline
    updateServicePipeline_templateMinorVersion,
    updateServicePipeline_templateMajorVersion,
    updateServicePipeline_deploymentType,
    updateServicePipeline_serviceName,
    updateServicePipeline_spec,
    updateServicePipelineResponse_httpStatus,
    updateServicePipelineResponse_pipeline,

    -- ** ListServiceTemplateVersions
    listServiceTemplateVersions_majorVersion,
    listServiceTemplateVersions_nextToken,
    listServiceTemplateVersions_maxResults,
    listServiceTemplateVersions_templateName,
    listServiceTemplateVersionsResponse_nextToken,
    listServiceTemplateVersionsResponse_httpStatus,
    listServiceTemplateVersionsResponse_templateVersions,

    -- ** CreateEnvironmentAccountConnection
    createEnvironmentAccountConnection_clientToken,
    createEnvironmentAccountConnection_environmentName,
    createEnvironmentAccountConnection_managementAccountId,
    createEnvironmentAccountConnection_roleArn,
    createEnvironmentAccountConnectionResponse_httpStatus,
    createEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** ListEnvironmentTemplateVersions
    listEnvironmentTemplateVersions_majorVersion,
    listEnvironmentTemplateVersions_nextToken,
    listEnvironmentTemplateVersions_maxResults,
    listEnvironmentTemplateVersions_templateName,
    listEnvironmentTemplateVersionsResponse_nextToken,
    listEnvironmentTemplateVersionsResponse_httpStatus,
    listEnvironmentTemplateVersionsResponse_templateVersions,

    -- ** GetEnvironmentTemplateVersion
    getEnvironmentTemplateVersion_majorVersion,
    getEnvironmentTemplateVersion_minorVersion,
    getEnvironmentTemplateVersion_templateName,
    getEnvironmentTemplateVersionResponse_httpStatus,
    getEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** CreateServiceTemplate
    createServiceTemplate_displayName,
    createServiceTemplate_encryptionKey,
    createServiceTemplate_pipelineProvisioning,
    createServiceTemplate_description,
    createServiceTemplate_tags,
    createServiceTemplate_name,
    createServiceTemplateResponse_httpStatus,
    createServiceTemplateResponse_serviceTemplate,

    -- ** GetServiceTemplateVersion
    getServiceTemplateVersion_majorVersion,
    getServiceTemplateVersion_minorVersion,
    getServiceTemplateVersion_templateName,
    getServiceTemplateVersionResponse_httpStatus,
    getServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** CreateEnvironmentTemplate
    createEnvironmentTemplate_provisioning,
    createEnvironmentTemplate_displayName,
    createEnvironmentTemplate_encryptionKey,
    createEnvironmentTemplate_description,
    createEnvironmentTemplate_tags,
    createEnvironmentTemplate_name,
    createEnvironmentTemplateResponse_httpStatus,
    createEnvironmentTemplateResponse_environmentTemplate,

    -- ** DeleteEnvironmentTemplate
    deleteEnvironmentTemplate_name,
    deleteEnvironmentTemplateResponse_environmentTemplate,
    deleteEnvironmentTemplateResponse_httpStatus,

    -- ** UpdateEnvironmentTemplate
    updateEnvironmentTemplate_displayName,
    updateEnvironmentTemplate_description,
    updateEnvironmentTemplate_name,
    updateEnvironmentTemplateResponse_httpStatus,
    updateEnvironmentTemplateResponse_environmentTemplate,

    -- ** GetEnvironmentAccountConnection
    getEnvironmentAccountConnection_id,
    getEnvironmentAccountConnectionResponse_httpStatus,
    getEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** ListEnvironmentTemplates
    listEnvironmentTemplates_nextToken,
    listEnvironmentTemplates_maxResults,
    listEnvironmentTemplatesResponse_nextToken,
    listEnvironmentTemplatesResponse_httpStatus,
    listEnvironmentTemplatesResponse_templates,

    -- ** DeleteServiceTemplate
    deleteServiceTemplate_name,
    deleteServiceTemplateResponse_serviceTemplate,
    deleteServiceTemplateResponse_httpStatus,

    -- ** UpdateServiceTemplate
    updateServiceTemplate_displayName,
    updateServiceTemplate_description,
    updateServiceTemplate_name,
    updateServiceTemplateResponse_httpStatus,
    updateServiceTemplateResponse_serviceTemplate,

    -- ** ListServiceTemplates
    listServiceTemplates_nextToken,
    listServiceTemplates_maxResults,
    listServiceTemplatesResponse_nextToken,
    listServiceTemplatesResponse_httpStatus,
    listServiceTemplatesResponse_templates,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** CancelEnvironmentDeployment
    cancelEnvironmentDeployment_environmentName,
    cancelEnvironmentDeploymentResponse_httpStatus,
    cancelEnvironmentDeploymentResponse_environment,

    -- ** UpdateServiceInstance
    updateServiceInstance_spec,
    updateServiceInstance_templateMinorVersion,
    updateServiceInstance_templateMajorVersion,
    updateServiceInstance_deploymentType,
    updateServiceInstance_name,
    updateServiceInstance_serviceName,
    updateServiceInstanceResponse_httpStatus,
    updateServiceInstanceResponse_serviceInstance,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_name,
    getEnvironmentResponse_httpStatus,
    getEnvironmentResponse_environment,

    -- ** ListEnvironmentAccountConnections
    listEnvironmentAccountConnections_nextToken,
    listEnvironmentAccountConnections_environmentName,
    listEnvironmentAccountConnections_statuses,
    listEnvironmentAccountConnections_maxResults,
    listEnvironmentAccountConnections_requestedBy,
    listEnvironmentAccountConnectionsResponse_nextToken,
    listEnvironmentAccountConnectionsResponse_httpStatus,
    listEnvironmentAccountConnectionsResponse_environmentAccountConnections,

    -- ** GetService
    getService_name,
    getServiceResponse_service,
    getServiceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteEnvironmentAccountConnection
    deleteEnvironmentAccountConnection_id,
    deleteEnvironmentAccountConnectionResponse_environmentAccountConnection,
    deleteEnvironmentAccountConnectionResponse_httpStatus,

    -- ** UpdateEnvironmentAccountConnection
    updateEnvironmentAccountConnection_id,
    updateEnvironmentAccountConnection_roleArn,
    updateEnvironmentAccountConnectionResponse_httpStatus,
    updateEnvironmentAccountConnectionResponse_environmentAccountConnection,

    -- ** DeleteServiceTemplateVersion
    deleteServiceTemplateVersion_majorVersion,
    deleteServiceTemplateVersion_minorVersion,
    deleteServiceTemplateVersion_templateName,
    deleteServiceTemplateVersionResponse_serviceTemplateVersion,
    deleteServiceTemplateVersionResponse_httpStatus,

    -- ** UpdateServiceTemplateVersion
    updateServiceTemplateVersion_status,
    updateServiceTemplateVersion_compatibleEnvironmentTemplates,
    updateServiceTemplateVersion_description,
    updateServiceTemplateVersion_majorVersion,
    updateServiceTemplateVersion_minorVersion,
    updateServiceTemplateVersion_templateName,
    updateServiceTemplateVersionResponse_httpStatus,
    updateServiceTemplateVersionResponse_serviceTemplateVersion,

    -- ** UpdateEnvironmentTemplateVersion
    updateEnvironmentTemplateVersion_status,
    updateEnvironmentTemplateVersion_description,
    updateEnvironmentTemplateVersion_majorVersion,
    updateEnvironmentTemplateVersion_minorVersion,
    updateEnvironmentTemplateVersion_templateName,
    updateEnvironmentTemplateVersionResponse_httpStatus,
    updateEnvironmentTemplateVersionResponse_environmentTemplateVersion,

    -- ** DeleteEnvironmentTemplateVersion
    deleteEnvironmentTemplateVersion_majorVersion,
    deleteEnvironmentTemplateVersion_minorVersion,
    deleteEnvironmentTemplateVersion_templateName,
    deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion,
    deleteEnvironmentTemplateVersionResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_protonServiceRoleArn,
    createEnvironment_environmentAccountConnectionId,
    createEnvironment_templateMinorVersion,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironment_spec,
    createEnvironment_templateMajorVersion,
    createEnvironment_templateName,
    createEnvironmentResponse_httpStatus,
    createEnvironmentResponse_environment,

    -- ** CreateService
    createService_branchName,
    createService_repositoryId,
    createService_templateMinorVersion,
    createService_description,
    createService_repositoryConnectionArn,
    createService_tags,
    createService_name,
    createService_spec,
    createService_templateMajorVersion,
    createService_templateName,
    createServiceResponse_httpStatus,
    createServiceResponse_service,

    -- * Types

    -- ** AccountSettings
    accountSettings_pipelineServiceRoleArn,

    -- ** CompatibleEnvironmentTemplate
    compatibleEnvironmentTemplate_majorVersion,
    compatibleEnvironmentTemplate_templateName,

    -- ** CompatibleEnvironmentTemplateInput
    compatibleEnvironmentTemplateInput_majorVersion,
    compatibleEnvironmentTemplateInput_templateName,

    -- ** Environment
    environment_deploymentStatusMessage,
    environment_environmentAccountId,
    environment_provisioning,
    environment_protonServiceRoleArn,
    environment_environmentAccountConnectionId,
    environment_spec,
    environment_description,
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
    environmentSummary_deploymentStatusMessage,
    environmentSummary_environmentAccountId,
    environmentSummary_provisioning,
    environmentSummary_protonServiceRoleArn,
    environmentSummary_environmentAccountConnectionId,
    environmentSummary_description,
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
    environmentTemplate_provisioning,
    environmentTemplate_recommendedVersion,
    environmentTemplate_displayName,
    environmentTemplate_encryptionKey,
    environmentTemplate_description,
    environmentTemplate_arn,
    environmentTemplate_createdAt,
    environmentTemplate_lastModifiedAt,
    environmentTemplate_name,

    -- ** EnvironmentTemplateFilter
    environmentTemplateFilter_majorVersion,
    environmentTemplateFilter_templateName,

    -- ** EnvironmentTemplateSummary
    environmentTemplateSummary_provisioning,
    environmentTemplateSummary_recommendedVersion,
    environmentTemplateSummary_displayName,
    environmentTemplateSummary_description,
    environmentTemplateSummary_arn,
    environmentTemplateSummary_createdAt,
    environmentTemplateSummary_lastModifiedAt,
    environmentTemplateSummary_name,

    -- ** EnvironmentTemplateVersion
    environmentTemplateVersion_schema,
    environmentTemplateVersion_statusMessage,
    environmentTemplateVersion_recommendedMinorVersion,
    environmentTemplateVersion_description,
    environmentTemplateVersion_arn,
    environmentTemplateVersion_createdAt,
    environmentTemplateVersion_lastModifiedAt,
    environmentTemplateVersion_majorVersion,
    environmentTemplateVersion_minorVersion,
    environmentTemplateVersion_status,
    environmentTemplateVersion_templateName,

    -- ** EnvironmentTemplateVersionSummary
    environmentTemplateVersionSummary_statusMessage,
    environmentTemplateVersionSummary_recommendedMinorVersion,
    environmentTemplateVersionSummary_description,
    environmentTemplateVersionSummary_arn,
    environmentTemplateVersionSummary_createdAt,
    environmentTemplateVersionSummary_lastModifiedAt,
    environmentTemplateVersionSummary_majorVersion,
    environmentTemplateVersionSummary_minorVersion,
    environmentTemplateVersionSummary_status,
    environmentTemplateVersionSummary_templateName,

    -- ** S3ObjectSource
    s3ObjectSource_bucket,
    s3ObjectSource_key,

    -- ** Service
    service_branchName,
    service_statusMessage,
    service_repositoryId,
    service_pipeline,
    service_description,
    service_repositoryConnectionArn,
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
    serviceSummary_statusMessage,
    serviceSummary_description,
    serviceSummary_arn,
    serviceSummary_createdAt,
    serviceSummary_lastModifiedAt,
    serviceSummary_name,
    serviceSummary_status,
    serviceSummary_templateName,

    -- ** ServiceTemplate
    serviceTemplate_recommendedVersion,
    serviceTemplate_displayName,
    serviceTemplate_encryptionKey,
    serviceTemplate_pipelineProvisioning,
    serviceTemplate_description,
    serviceTemplate_arn,
    serviceTemplate_createdAt,
    serviceTemplate_lastModifiedAt,
    serviceTemplate_name,

    -- ** ServiceTemplateSummary
    serviceTemplateSummary_recommendedVersion,
    serviceTemplateSummary_displayName,
    serviceTemplateSummary_pipelineProvisioning,
    serviceTemplateSummary_description,
    serviceTemplateSummary_arn,
    serviceTemplateSummary_createdAt,
    serviceTemplateSummary_lastModifiedAt,
    serviceTemplateSummary_name,

    -- ** ServiceTemplateVersion
    serviceTemplateVersion_schema,
    serviceTemplateVersion_statusMessage,
    serviceTemplateVersion_recommendedMinorVersion,
    serviceTemplateVersion_description,
    serviceTemplateVersion_arn,
    serviceTemplateVersion_compatibleEnvironmentTemplates,
    serviceTemplateVersion_createdAt,
    serviceTemplateVersion_lastModifiedAt,
    serviceTemplateVersion_majorVersion,
    serviceTemplateVersion_minorVersion,
    serviceTemplateVersion_status,
    serviceTemplateVersion_templateName,

    -- ** ServiceTemplateVersionSummary
    serviceTemplateVersionSummary_statusMessage,
    serviceTemplateVersionSummary_recommendedMinorVersion,
    serviceTemplateVersionSummary_description,
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

    -- ** TemplateVersionSourceInput
    templateVersionSourceInput_s3,
  )
where

import Network.AWS.Proton.AcceptEnvironmentAccountConnection
import Network.AWS.Proton.CancelEnvironmentDeployment
import Network.AWS.Proton.CancelServiceInstanceDeployment
import Network.AWS.Proton.CancelServicePipelineDeployment
import Network.AWS.Proton.CreateEnvironment
import Network.AWS.Proton.CreateEnvironmentAccountConnection
import Network.AWS.Proton.CreateEnvironmentTemplate
import Network.AWS.Proton.CreateEnvironmentTemplateVersion
import Network.AWS.Proton.CreateService
import Network.AWS.Proton.CreateServiceTemplate
import Network.AWS.Proton.CreateServiceTemplateVersion
import Network.AWS.Proton.DeleteEnvironment
import Network.AWS.Proton.DeleteEnvironmentAccountConnection
import Network.AWS.Proton.DeleteEnvironmentTemplate
import Network.AWS.Proton.DeleteEnvironmentTemplateVersion
import Network.AWS.Proton.DeleteService
import Network.AWS.Proton.DeleteServiceTemplate
import Network.AWS.Proton.DeleteServiceTemplateVersion
import Network.AWS.Proton.GetAccountSettings
import Network.AWS.Proton.GetEnvironment
import Network.AWS.Proton.GetEnvironmentAccountConnection
import Network.AWS.Proton.GetEnvironmentTemplate
import Network.AWS.Proton.GetEnvironmentTemplateVersion
import Network.AWS.Proton.GetService
import Network.AWS.Proton.GetServiceInstance
import Network.AWS.Proton.GetServiceTemplate
import Network.AWS.Proton.GetServiceTemplateVersion
import Network.AWS.Proton.ListEnvironmentAccountConnections
import Network.AWS.Proton.ListEnvironmentTemplateVersions
import Network.AWS.Proton.ListEnvironmentTemplates
import Network.AWS.Proton.ListEnvironments
import Network.AWS.Proton.ListServiceInstances
import Network.AWS.Proton.ListServiceTemplateVersions
import Network.AWS.Proton.ListServiceTemplates
import Network.AWS.Proton.ListServices
import Network.AWS.Proton.ListTagsForResource
import Network.AWS.Proton.RejectEnvironmentAccountConnection
import Network.AWS.Proton.TagResource
import Network.AWS.Proton.Types.AccountSettings
import Network.AWS.Proton.Types.CompatibleEnvironmentTemplate
import Network.AWS.Proton.Types.CompatibleEnvironmentTemplateInput
import Network.AWS.Proton.Types.Environment
import Network.AWS.Proton.Types.EnvironmentAccountConnection
import Network.AWS.Proton.Types.EnvironmentAccountConnectionSummary
import Network.AWS.Proton.Types.EnvironmentSummary
import Network.AWS.Proton.Types.EnvironmentTemplate
import Network.AWS.Proton.Types.EnvironmentTemplateFilter
import Network.AWS.Proton.Types.EnvironmentTemplateSummary
import Network.AWS.Proton.Types.EnvironmentTemplateVersion
import Network.AWS.Proton.Types.EnvironmentTemplateVersionSummary
import Network.AWS.Proton.Types.S3ObjectSource
import Network.AWS.Proton.Types.Service
import Network.AWS.Proton.Types.ServiceInstance
import Network.AWS.Proton.Types.ServiceInstanceSummary
import Network.AWS.Proton.Types.ServicePipeline
import Network.AWS.Proton.Types.ServiceSummary
import Network.AWS.Proton.Types.ServiceTemplate
import Network.AWS.Proton.Types.ServiceTemplateSummary
import Network.AWS.Proton.Types.ServiceTemplateVersion
import Network.AWS.Proton.Types.ServiceTemplateVersionSummary
import Network.AWS.Proton.Types.Tag
import Network.AWS.Proton.Types.TemplateVersionSourceInput
import Network.AWS.Proton.UntagResource
import Network.AWS.Proton.UpdateAccountSettings
import Network.AWS.Proton.UpdateEnvironment
import Network.AWS.Proton.UpdateEnvironmentAccountConnection
import Network.AWS.Proton.UpdateEnvironmentTemplate
import Network.AWS.Proton.UpdateEnvironmentTemplateVersion
import Network.AWS.Proton.UpdateService
import Network.AWS.Proton.UpdateServiceInstance
import Network.AWS.Proton.UpdateServicePipeline
import Network.AWS.Proton.UpdateServiceTemplate
import Network.AWS.Proton.UpdateServiceTemplateVersion
