{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppConfig.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppConfig.Lens
  ( -- * Operations

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironments_applicationId,
    listEnvironmentsResponse_items,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_monitors,
    updateEnvironment_name,
    updateEnvironment_description,
    updateEnvironment_applicationId,
    updateEnvironment_environmentId,
    environment_state,
    environment_monitors,
    environment_applicationId,
    environment_name,
    environment_id,
    environment_description,

    -- ** DeleteEnvironment
    deleteEnvironment_applicationId,
    deleteEnvironment_environmentId,

    -- ** GetDeploymentStrategy
    getDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,

    -- ** CreateConfigurationProfile
    createConfigurationProfile_retrievalRoleArn,
    createConfigurationProfile_validators,
    createConfigurationProfile_description,
    createConfigurationProfile_tags,
    createConfigurationProfile_applicationId,
    createConfigurationProfile_name,
    createConfigurationProfile_locationUri,
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,

    -- ** GetDeployment
    getDeployment_applicationId,
    getDeployment_environmentId,
    getDeployment_deploymentNumber,
    deployment_growthFactor,
    deployment_configurationName,
    deployment_state,
    deployment_deploymentStrategyId,
    deployment_deploymentNumber,
    deployment_configurationVersion,
    deployment_eventLog,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_applicationId,
    deployment_deploymentDurationInMinutes,
    deployment_environmentId,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_finalBakeTimeInMinutes,
    deployment_description,
    deployment_configurationProfileId,
    deployment_growthType,

    -- ** UpdateConfigurationProfile
    updateConfigurationProfile_retrievalRoleArn,
    updateConfigurationProfile_validators,
    updateConfigurationProfile_name,
    updateConfigurationProfile_description,
    updateConfigurationProfile_applicationId,
    updateConfigurationProfile_configurationProfileId,
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,

    -- ** DeleteConfigurationProfile
    deleteConfigurationProfile_applicationId,
    deleteConfigurationProfile_configurationProfileId,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListHostedConfigurationVersions
    listHostedConfigurationVersions_nextToken,
    listHostedConfigurationVersions_maxResults,
    listHostedConfigurationVersions_applicationId,
    listHostedConfigurationVersions_configurationProfileId,
    listHostedConfigurationVersionsResponse_items,
    listHostedConfigurationVersionsResponse_nextToken,
    listHostedConfigurationVersionsResponse_httpStatus,

    -- ** GetConfigurationProfile
    getConfigurationProfile_applicationId,
    getConfigurationProfile_configurationProfileId,
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,

    -- ** DeleteApplication
    deleteApplication_applicationId,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_applicationId,
    application_name,
    application_id,
    application_description,

    -- ** UpdateDeploymentStrategy
    updateDeploymentStrategy_growthFactor,
    updateDeploymentStrategy_deploymentDurationInMinutes,
    updateDeploymentStrategy_finalBakeTimeInMinutes,
    updateDeploymentStrategy_description,
    updateDeploymentStrategy_growthType,
    updateDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,

    -- ** DeleteDeploymentStrategy
    deleteDeploymentStrategy_deploymentStrategyId,

    -- ** CreateApplication
    createApplication_description,
    createApplication_tags,
    createApplication_name,
    application_name,
    application_id,
    application_description,

    -- ** ValidateConfiguration
    validateConfiguration_applicationId,
    validateConfiguration_configurationProfileId,
    validateConfiguration_configurationVersion,

    -- ** StopDeployment
    stopDeployment_applicationId,
    stopDeployment_environmentId,
    stopDeployment_deploymentNumber,
    deployment_growthFactor,
    deployment_configurationName,
    deployment_state,
    deployment_deploymentStrategyId,
    deployment_deploymentNumber,
    deployment_configurationVersion,
    deployment_eventLog,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_applicationId,
    deployment_deploymentDurationInMinutes,
    deployment_environmentId,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_finalBakeTimeInMinutes,
    deployment_description,
    deployment_configurationProfileId,
    deployment_growthType,

    -- ** GetApplication
    getApplication_applicationId,
    application_name,
    application_id,
    application_description,

    -- ** CreateHostedConfigurationVersion
    createHostedConfigurationVersion_latestVersionNumber,
    createHostedConfigurationVersion_description,
    createHostedConfigurationVersion_applicationId,
    createHostedConfigurationVersion_configurationProfileId,
    createHostedConfigurationVersion_content,
    createHostedConfigurationVersion_contentType,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- ** ListConfigurationProfiles
    listConfigurationProfiles_nextToken,
    listConfigurationProfiles_maxResults,
    listConfigurationProfiles_applicationId,
    listConfigurationProfilesResponse_items,
    listConfigurationProfilesResponse_nextToken,
    listConfigurationProfilesResponse_httpStatus,

    -- ** DeleteHostedConfigurationVersion
    deleteHostedConfigurationVersion_applicationId,
    deleteHostedConfigurationVersion_configurationProfileId,
    deleteHostedConfigurationVersion_versionNumber,

    -- ** GetHostedConfigurationVersion
    getHostedConfigurationVersion_applicationId,
    getHostedConfigurationVersion_configurationProfileId,
    getHostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_maxResults,
    listDeployments_applicationId,
    listDeployments_environmentId,
    listDeploymentsResponse_items,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** GetEnvironment
    getEnvironment_applicationId,
    getEnvironment_environmentId,
    environment_state,
    environment_monitors,
    environment_applicationId,
    environment_name,
    environment_id,
    environment_description,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_items,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** ListDeploymentStrategies
    listDeploymentStrategies_nextToken,
    listDeploymentStrategies_maxResults,
    listDeploymentStrategiesResponse_items,
    listDeploymentStrategiesResponse_nextToken,
    listDeploymentStrategiesResponse_httpStatus,

    -- ** GetConfiguration
    getConfiguration_clientConfigurationVersion,
    getConfiguration_application,
    getConfiguration_environment,
    getConfiguration_configuration,
    getConfiguration_clientId,
    getConfigurationResponse_configurationVersion,
    getConfigurationResponse_content,
    getConfigurationResponse_contentType,
    getConfigurationResponse_httpStatus,

    -- ** CreateDeploymentStrategy
    createDeploymentStrategy_finalBakeTimeInMinutes,
    createDeploymentStrategy_description,
    createDeploymentStrategy_growthType,
    createDeploymentStrategy_tags,
    createDeploymentStrategy_name,
    createDeploymentStrategy_deploymentDurationInMinutes,
    createDeploymentStrategy_growthFactor,
    createDeploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,

    -- ** StartDeployment
    startDeployment_description,
    startDeployment_tags,
    startDeployment_applicationId,
    startDeployment_environmentId,
    startDeployment_deploymentStrategyId,
    startDeployment_configurationProfileId,
    startDeployment_configurationVersion,
    deployment_growthFactor,
    deployment_configurationName,
    deployment_state,
    deployment_deploymentStrategyId,
    deployment_deploymentNumber,
    deployment_configurationVersion,
    deployment_eventLog,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_applicationId,
    deployment_deploymentDurationInMinutes,
    deployment_environmentId,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_finalBakeTimeInMinutes,
    deployment_description,
    deployment_configurationProfileId,
    deployment_growthType,

    -- ** CreateEnvironment
    createEnvironment_monitors,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_applicationId,
    createEnvironment_name,
    environment_state,
    environment_monitors,
    environment_applicationId,
    environment_name,
    environment_id,
    environment_description,

    -- * Types

    -- ** Application
    application_name,
    application_id,
    application_description,

    -- ** ConfigurationProfile
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,

    -- ** ConfigurationProfileSummary
    configurationProfileSummary_locationUri,
    configurationProfileSummary_applicationId,
    configurationProfileSummary_name,
    configurationProfileSummary_id,
    configurationProfileSummary_validatorTypes,

    -- ** Deployment
    deployment_growthFactor,
    deployment_configurationName,
    deployment_state,
    deployment_deploymentStrategyId,
    deployment_deploymentNumber,
    deployment_configurationVersion,
    deployment_eventLog,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_applicationId,
    deployment_deploymentDurationInMinutes,
    deployment_environmentId,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_finalBakeTimeInMinutes,
    deployment_description,
    deployment_configurationProfileId,
    deployment_growthType,

    -- ** DeploymentEvent
    deploymentEvent_triggeredBy,
    deploymentEvent_occurredAt,
    deploymentEvent_eventType,
    deploymentEvent_description,

    -- ** DeploymentStrategy
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,

    -- ** DeploymentSummary
    deploymentSummary_growthFactor,
    deploymentSummary_configurationName,
    deploymentSummary_state,
    deploymentSummary_deploymentNumber,
    deploymentSummary_configurationVersion,
    deploymentSummary_percentageComplete,
    deploymentSummary_startedAt,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_completedAt,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_growthType,

    -- ** Environment
    environment_state,
    environment_monitors,
    environment_applicationId,
    environment_name,
    environment_id,
    environment_description,

    -- ** HostedConfigurationVersion
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- ** HostedConfigurationVersionSummary
    hostedConfigurationVersionSummary_versionNumber,
    hostedConfigurationVersionSummary_applicationId,
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_configurationProfileId,
    hostedConfigurationVersionSummary_contentType,

    -- ** Monitor
    monitor_alarmRoleArn,
    monitor_alarmArn,

    -- ** Validator
    validator_type,
    validator_content,
  )
where

import Network.AWS.AppConfig.CreateApplication
import Network.AWS.AppConfig.CreateConfigurationProfile
import Network.AWS.AppConfig.CreateDeploymentStrategy
import Network.AWS.AppConfig.CreateEnvironment
import Network.AWS.AppConfig.CreateHostedConfigurationVersion
import Network.AWS.AppConfig.DeleteApplication
import Network.AWS.AppConfig.DeleteConfigurationProfile
import Network.AWS.AppConfig.DeleteDeploymentStrategy
import Network.AWS.AppConfig.DeleteEnvironment
import Network.AWS.AppConfig.DeleteHostedConfigurationVersion
import Network.AWS.AppConfig.GetApplication
import Network.AWS.AppConfig.GetConfiguration
import Network.AWS.AppConfig.GetConfigurationProfile
import Network.AWS.AppConfig.GetDeployment
import Network.AWS.AppConfig.GetDeploymentStrategy
import Network.AWS.AppConfig.GetEnvironment
import Network.AWS.AppConfig.GetHostedConfigurationVersion
import Network.AWS.AppConfig.ListApplications
import Network.AWS.AppConfig.ListConfigurationProfiles
import Network.AWS.AppConfig.ListDeploymentStrategies
import Network.AWS.AppConfig.ListDeployments
import Network.AWS.AppConfig.ListEnvironments
import Network.AWS.AppConfig.ListHostedConfigurationVersions
import Network.AWS.AppConfig.ListTagsForResource
import Network.AWS.AppConfig.StartDeployment
import Network.AWS.AppConfig.StopDeployment
import Network.AWS.AppConfig.TagResource
import Network.AWS.AppConfig.Types.Application
import Network.AWS.AppConfig.Types.ConfigurationProfile
import Network.AWS.AppConfig.Types.ConfigurationProfileSummary
import Network.AWS.AppConfig.Types.Deployment
import Network.AWS.AppConfig.Types.DeploymentEvent
import Network.AWS.AppConfig.Types.DeploymentStrategy
import Network.AWS.AppConfig.Types.DeploymentSummary
import Network.AWS.AppConfig.Types.Environment
import Network.AWS.AppConfig.Types.HostedConfigurationVersion
import Network.AWS.AppConfig.Types.HostedConfigurationVersionSummary
import Network.AWS.AppConfig.Types.Monitor
import Network.AWS.AppConfig.Types.Validator
import Network.AWS.AppConfig.UntagResource
import Network.AWS.AppConfig.UpdateApplication
import Network.AWS.AppConfig.UpdateConfigurationProfile
import Network.AWS.AppConfig.UpdateDeploymentStrategy
import Network.AWS.AppConfig.UpdateEnvironment
import Network.AWS.AppConfig.ValidateConfiguration
