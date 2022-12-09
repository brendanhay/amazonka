{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_description,
    createApplication_tags,
    createApplication_name,
    application_description,
    application_id,
    application_name,

    -- ** CreateConfigurationProfile
    createConfigurationProfile_description,
    createConfigurationProfile_retrievalRoleArn,
    createConfigurationProfile_tags,
    createConfigurationProfile_type,
    createConfigurationProfile_validators,
    createConfigurationProfile_applicationId,
    createConfigurationProfile_name,
    createConfigurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_description,
    configurationProfile_id,
    configurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_type,
    configurationProfile_validators,

    -- ** CreateDeploymentStrategy
    createDeploymentStrategy_description,
    createDeploymentStrategy_finalBakeTimeInMinutes,
    createDeploymentStrategy_growthType,
    createDeploymentStrategy_replicateTo,
    createDeploymentStrategy_tags,
    createDeploymentStrategy_name,
    createDeploymentStrategy_deploymentDurationInMinutes,
    createDeploymentStrategy_growthFactor,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,

    -- ** CreateEnvironment
    createEnvironment_description,
    createEnvironment_monitors,
    createEnvironment_tags,
    createEnvironment_applicationId,
    createEnvironment_name,
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,

    -- ** CreateExtension
    createExtension_description,
    createExtension_latestVersionNumber,
    createExtension_parameters,
    createExtension_tags,
    createExtension_name,
    createExtension_actions,
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,

    -- ** CreateExtensionAssociation
    createExtensionAssociation_extensionVersionNumber,
    createExtensionAssociation_parameters,
    createExtensionAssociation_tags,
    createExtensionAssociation_extensionIdentifier,
    createExtensionAssociation_resourceIdentifier,
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,

    -- ** CreateHostedConfigurationVersion
    createHostedConfigurationVersion_description,
    createHostedConfigurationVersion_latestVersionNumber,
    createHostedConfigurationVersion_applicationId,
    createHostedConfigurationVersion_configurationProfileId,
    createHostedConfigurationVersion_content,
    createHostedConfigurationVersion_contentType,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,

    -- ** DeleteApplication
    deleteApplication_applicationId,

    -- ** DeleteConfigurationProfile
    deleteConfigurationProfile_applicationId,
    deleteConfigurationProfile_configurationProfileId,

    -- ** DeleteDeploymentStrategy
    deleteDeploymentStrategy_deploymentStrategyId,

    -- ** DeleteEnvironment
    deleteEnvironment_applicationId,
    deleteEnvironment_environmentId,

    -- ** DeleteExtension
    deleteExtension_versionNumber,
    deleteExtension_extensionIdentifier,

    -- ** DeleteExtensionAssociation
    deleteExtensionAssociation_extensionAssociationId,

    -- ** DeleteHostedConfigurationVersion
    deleteHostedConfigurationVersion_applicationId,
    deleteHostedConfigurationVersion_configurationProfileId,
    deleteHostedConfigurationVersion_versionNumber,

    -- ** GetApplication
    getApplication_applicationId,
    application_description,
    application_id,
    application_name,

    -- ** GetConfigurationProfile
    getConfigurationProfile_applicationId,
    getConfigurationProfile_configurationProfileId,
    configurationProfile_applicationId,
    configurationProfile_description,
    configurationProfile_id,
    configurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_type,
    configurationProfile_validators,

    -- ** GetDeployment
    getDeployment_applicationId,
    getDeployment_environmentId,
    getDeployment_deploymentNumber,
    deployment_applicationId,
    deployment_appliedExtensions,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_configurationName,
    deployment_configurationProfileId,
    deployment_configurationVersion,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_description,
    deployment_environmentId,
    deployment_eventLog,
    deployment_finalBakeTimeInMinutes,
    deployment_growthFactor,
    deployment_growthType,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_state,

    -- ** GetDeploymentStrategy
    getDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,

    -- ** GetEnvironment
    getEnvironment_applicationId,
    getEnvironment_environmentId,
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,

    -- ** GetExtension
    getExtension_versionNumber,
    getExtension_extensionIdentifier,
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,

    -- ** GetExtensionAssociation
    getExtensionAssociation_extensionAssociationId,
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,

    -- ** GetHostedConfigurationVersion
    getHostedConfigurationVersion_applicationId,
    getHostedConfigurationVersion_configurationProfileId,
    getHostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,

    -- ** ListApplications
    listApplications_maxResults,
    listApplications_nextToken,
    listApplicationsResponse_items,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListConfigurationProfiles
    listConfigurationProfiles_maxResults,
    listConfigurationProfiles_nextToken,
    listConfigurationProfiles_type,
    listConfigurationProfiles_applicationId,
    listConfigurationProfilesResponse_items,
    listConfigurationProfilesResponse_nextToken,
    listConfigurationProfilesResponse_httpStatus,

    -- ** ListDeploymentStrategies
    listDeploymentStrategies_maxResults,
    listDeploymentStrategies_nextToken,
    listDeploymentStrategiesResponse_items,
    listDeploymentStrategiesResponse_nextToken,
    listDeploymentStrategiesResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_applicationId,
    listDeployments_environmentId,
    listDeploymentsResponse_items,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironments_applicationId,
    listEnvironmentsResponse_items,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListExtensionAssociations
    listExtensionAssociations_extensionIdentifier,
    listExtensionAssociations_extensionVersionNumber,
    listExtensionAssociations_maxResults,
    listExtensionAssociations_nextToken,
    listExtensionAssociations_resourceIdentifier,
    listExtensionAssociationsResponse_items,
    listExtensionAssociationsResponse_nextToken,
    listExtensionAssociationsResponse_httpStatus,

    -- ** ListExtensions
    listExtensions_maxResults,
    listExtensions_name,
    listExtensions_nextToken,
    listExtensionsResponse_items,
    listExtensionsResponse_nextToken,
    listExtensionsResponse_httpStatus,

    -- ** ListHostedConfigurationVersions
    listHostedConfigurationVersions_maxResults,
    listHostedConfigurationVersions_nextToken,
    listHostedConfigurationVersions_applicationId,
    listHostedConfigurationVersions_configurationProfileId,
    listHostedConfigurationVersionsResponse_items,
    listHostedConfigurationVersionsResponse_nextToken,
    listHostedConfigurationVersionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartDeployment
    startDeployment_description,
    startDeployment_tags,
    startDeployment_applicationId,
    startDeployment_environmentId,
    startDeployment_deploymentStrategyId,
    startDeployment_configurationProfileId,
    startDeployment_configurationVersion,
    deployment_applicationId,
    deployment_appliedExtensions,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_configurationName,
    deployment_configurationProfileId,
    deployment_configurationVersion,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_description,
    deployment_environmentId,
    deployment_eventLog,
    deployment_finalBakeTimeInMinutes,
    deployment_growthFactor,
    deployment_growthType,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_state,

    -- ** StopDeployment
    stopDeployment_applicationId,
    stopDeployment_environmentId,
    stopDeployment_deploymentNumber,
    deployment_applicationId,
    deployment_appliedExtensions,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_configurationName,
    deployment_configurationProfileId,
    deployment_configurationVersion,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_description,
    deployment_environmentId,
    deployment_eventLog,
    deployment_finalBakeTimeInMinutes,
    deployment_growthFactor,
    deployment_growthType,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_state,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_name,
    updateApplication_applicationId,
    application_description,
    application_id,
    application_name,

    -- ** UpdateConfigurationProfile
    updateConfigurationProfile_description,
    updateConfigurationProfile_name,
    updateConfigurationProfile_retrievalRoleArn,
    updateConfigurationProfile_validators,
    updateConfigurationProfile_applicationId,
    updateConfigurationProfile_configurationProfileId,
    configurationProfile_applicationId,
    configurationProfile_description,
    configurationProfile_id,
    configurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_type,
    configurationProfile_validators,

    -- ** UpdateDeploymentStrategy
    updateDeploymentStrategy_deploymentDurationInMinutes,
    updateDeploymentStrategy_description,
    updateDeploymentStrategy_finalBakeTimeInMinutes,
    updateDeploymentStrategy_growthFactor,
    updateDeploymentStrategy_growthType,
    updateDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,

    -- ** UpdateEnvironment
    updateEnvironment_description,
    updateEnvironment_monitors,
    updateEnvironment_name,
    updateEnvironment_applicationId,
    updateEnvironment_environmentId,
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,

    -- ** UpdateExtension
    updateExtension_actions,
    updateExtension_description,
    updateExtension_parameters,
    updateExtension_versionNumber,
    updateExtension_extensionIdentifier,
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,

    -- ** UpdateExtensionAssociation
    updateExtensionAssociation_parameters,
    updateExtensionAssociation_extensionAssociationId,
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,

    -- ** ValidateConfiguration
    validateConfiguration_applicationId,
    validateConfiguration_configurationProfileId,
    validateConfiguration_configurationVersion,

    -- * Types

    -- ** Action
    action_description,
    action_name,
    action_roleArn,
    action_uri,

    -- ** ActionInvocation
    actionInvocation_actionName,
    actionInvocation_errorCode,
    actionInvocation_errorMessage,
    actionInvocation_extensionIdentifier,
    actionInvocation_invocationId,
    actionInvocation_roleArn,
    actionInvocation_uri,

    -- ** Application
    application_description,
    application_id,
    application_name,

    -- ** AppliedExtension
    appliedExtension_extensionAssociationId,
    appliedExtension_extensionId,
    appliedExtension_parameters,
    appliedExtension_versionNumber,

    -- ** ConfigurationProfile
    configurationProfile_applicationId,
    configurationProfile_description,
    configurationProfile_id,
    configurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_type,
    configurationProfile_validators,

    -- ** ConfigurationProfileSummary
    configurationProfileSummary_applicationId,
    configurationProfileSummary_id,
    configurationProfileSummary_locationUri,
    configurationProfileSummary_name,
    configurationProfileSummary_type,
    configurationProfileSummary_validatorTypes,

    -- ** Deployment
    deployment_applicationId,
    deployment_appliedExtensions,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_configurationName,
    deployment_configurationProfileId,
    deployment_configurationVersion,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_description,
    deployment_environmentId,
    deployment_eventLog,
    deployment_finalBakeTimeInMinutes,
    deployment_growthFactor,
    deployment_growthType,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_state,

    -- ** DeploymentEvent
    deploymentEvent_actionInvocations,
    deploymentEvent_description,
    deploymentEvent_eventType,
    deploymentEvent_occurredAt,
    deploymentEvent_triggeredBy,

    -- ** DeploymentStrategy
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,

    -- ** DeploymentSummary
    deploymentSummary_completedAt,
    deploymentSummary_configurationName,
    deploymentSummary_configurationVersion,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_deploymentNumber,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_growthFactor,
    deploymentSummary_growthType,
    deploymentSummary_percentageComplete,
    deploymentSummary_startedAt,
    deploymentSummary_state,

    -- ** Environment
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,

    -- ** Extension
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,

    -- ** ExtensionAssociation
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,

    -- ** ExtensionAssociationSummary
    extensionAssociationSummary_extensionArn,
    extensionAssociationSummary_id,
    extensionAssociationSummary_resourceArn,

    -- ** ExtensionSummary
    extensionSummary_arn,
    extensionSummary_description,
    extensionSummary_id,
    extensionSummary_name,
    extensionSummary_versionNumber,

    -- ** HostedConfigurationVersion
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,

    -- ** HostedConfigurationVersionSummary
    hostedConfigurationVersionSummary_applicationId,
    hostedConfigurationVersionSummary_configurationProfileId,
    hostedConfigurationVersionSummary_contentType,
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_versionNumber,

    -- ** Monitor
    monitor_alarmRoleArn,
    monitor_alarmArn,

    -- ** Parameter
    parameter_description,
    parameter_required,

    -- ** Validator
    validator_type,
    validator_content,
  )
where

import Amazonka.AppConfig.CreateApplication
import Amazonka.AppConfig.CreateConfigurationProfile
import Amazonka.AppConfig.CreateDeploymentStrategy
import Amazonka.AppConfig.CreateEnvironment
import Amazonka.AppConfig.CreateExtension
import Amazonka.AppConfig.CreateExtensionAssociation
import Amazonka.AppConfig.CreateHostedConfigurationVersion
import Amazonka.AppConfig.DeleteApplication
import Amazonka.AppConfig.DeleteConfigurationProfile
import Amazonka.AppConfig.DeleteDeploymentStrategy
import Amazonka.AppConfig.DeleteEnvironment
import Amazonka.AppConfig.DeleteExtension
import Amazonka.AppConfig.DeleteExtensionAssociation
import Amazonka.AppConfig.DeleteHostedConfigurationVersion
import Amazonka.AppConfig.GetApplication
import Amazonka.AppConfig.GetConfigurationProfile
import Amazonka.AppConfig.GetDeployment
import Amazonka.AppConfig.GetDeploymentStrategy
import Amazonka.AppConfig.GetEnvironment
import Amazonka.AppConfig.GetExtension
import Amazonka.AppConfig.GetExtensionAssociation
import Amazonka.AppConfig.GetHostedConfigurationVersion
import Amazonka.AppConfig.ListApplications
import Amazonka.AppConfig.ListConfigurationProfiles
import Amazonka.AppConfig.ListDeploymentStrategies
import Amazonka.AppConfig.ListDeployments
import Amazonka.AppConfig.ListEnvironments
import Amazonka.AppConfig.ListExtensionAssociations
import Amazonka.AppConfig.ListExtensions
import Amazonka.AppConfig.ListHostedConfigurationVersions
import Amazonka.AppConfig.ListTagsForResource
import Amazonka.AppConfig.StartDeployment
import Amazonka.AppConfig.StopDeployment
import Amazonka.AppConfig.TagResource
import Amazonka.AppConfig.Types.Action
import Amazonka.AppConfig.Types.ActionInvocation
import Amazonka.AppConfig.Types.Application
import Amazonka.AppConfig.Types.AppliedExtension
import Amazonka.AppConfig.Types.ConfigurationProfile
import Amazonka.AppConfig.Types.ConfigurationProfileSummary
import Amazonka.AppConfig.Types.Deployment
import Amazonka.AppConfig.Types.DeploymentEvent
import Amazonka.AppConfig.Types.DeploymentStrategy
import Amazonka.AppConfig.Types.DeploymentSummary
import Amazonka.AppConfig.Types.Environment
import Amazonka.AppConfig.Types.Extension
import Amazonka.AppConfig.Types.ExtensionAssociation
import Amazonka.AppConfig.Types.ExtensionAssociationSummary
import Amazonka.AppConfig.Types.ExtensionSummary
import Amazonka.AppConfig.Types.HostedConfigurationVersion
import Amazonka.AppConfig.Types.HostedConfigurationVersionSummary
import Amazonka.AppConfig.Types.Monitor
import Amazonka.AppConfig.Types.Parameter
import Amazonka.AppConfig.Types.Validator
import Amazonka.AppConfig.UntagResource
import Amazonka.AppConfig.UpdateApplication
import Amazonka.AppConfig.UpdateConfigurationProfile
import Amazonka.AppConfig.UpdateDeploymentStrategy
import Amazonka.AppConfig.UpdateEnvironment
import Amazonka.AppConfig.UpdateExtension
import Amazonka.AppConfig.UpdateExtensionAssociation
import Amazonka.AppConfig.ValidateConfiguration
