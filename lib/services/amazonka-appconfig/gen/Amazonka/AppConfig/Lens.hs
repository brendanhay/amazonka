{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_tags,
    createApplication_description,
    createApplication_name,
    application_name,
    application_id,
    application_description,

    -- ** CreateConfigurationProfile
    createConfigurationProfile_tags,
    createConfigurationProfile_retrievalRoleArn,
    createConfigurationProfile_description,
    createConfigurationProfile_validators,
    createConfigurationProfile_applicationId,
    createConfigurationProfile_name,
    createConfigurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,

    -- ** CreateDeploymentStrategy
    createDeploymentStrategy_tags,
    createDeploymentStrategy_growthType,
    createDeploymentStrategy_description,
    createDeploymentStrategy_finalBakeTimeInMinutes,
    createDeploymentStrategy_name,
    createDeploymentStrategy_deploymentDurationInMinutes,
    createDeploymentStrategy_growthFactor,
    createDeploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_growthType,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_id,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_monitors,
    createEnvironment_description,
    createEnvironment_applicationId,
    createEnvironment_name,
    environment_name,
    environment_state,
    environment_monitors,
    environment_id,
    environment_description,
    environment_applicationId,

    -- ** CreateHostedConfigurationVersion
    createHostedConfigurationVersion_latestVersionNumber,
    createHostedConfigurationVersion_description,
    createHostedConfigurationVersion_applicationId,
    createHostedConfigurationVersion_configurationProfileId,
    createHostedConfigurationVersion_content,
    createHostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

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

    -- ** DeleteHostedConfigurationVersion
    deleteHostedConfigurationVersion_applicationId,
    deleteHostedConfigurationVersion_configurationProfileId,
    deleteHostedConfigurationVersion_versionNumber,

    -- ** GetApplication
    getApplication_applicationId,
    application_name,
    application_id,
    application_description,

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

    -- ** GetConfigurationProfile
    getConfigurationProfile_applicationId,
    getConfigurationProfile_configurationProfileId,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,

    -- ** GetDeployment
    getDeployment_applicationId,
    getDeployment_environmentId,
    getDeployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_growthType,
    deployment_state,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_description,
    deployment_finalBakeTimeInMinutes,
    deployment_startedAt,
    deployment_configurationName,
    deployment_growthFactor,
    deployment_eventLog,
    deployment_configurationVersion,
    deployment_environmentId,
    deployment_percentageComplete,
    deployment_configurationLocationUri,
    deployment_applicationId,
    deployment_completedAt,
    deployment_configurationProfileId,

    -- ** GetDeploymentStrategy
    getDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_name,
    deploymentStrategy_growthType,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_id,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,

    -- ** GetEnvironment
    getEnvironment_applicationId,
    getEnvironment_environmentId,
    environment_name,
    environment_state,
    environment_monitors,
    environment_id,
    environment_description,
    environment_applicationId,

    -- ** GetHostedConfigurationVersion
    getHostedConfigurationVersion_applicationId,
    getHostedConfigurationVersion_configurationProfileId,
    getHostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_items,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListConfigurationProfiles
    listConfigurationProfiles_nextToken,
    listConfigurationProfiles_maxResults,
    listConfigurationProfiles_applicationId,
    listConfigurationProfilesResponse_items,
    listConfigurationProfilesResponse_nextToken,
    listConfigurationProfilesResponse_httpStatus,

    -- ** ListDeploymentStrategies
    listDeploymentStrategies_nextToken,
    listDeploymentStrategies_maxResults,
    listDeploymentStrategiesResponse_items,
    listDeploymentStrategiesResponse_nextToken,
    listDeploymentStrategiesResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_maxResults,
    listDeployments_applicationId,
    listDeployments_environmentId,
    listDeploymentsResponse_items,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironments_applicationId,
    listEnvironmentsResponse_items,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListHostedConfigurationVersions
    listHostedConfigurationVersions_nextToken,
    listHostedConfigurationVersions_maxResults,
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
    startDeployment_tags,
    startDeployment_description,
    startDeployment_applicationId,
    startDeployment_environmentId,
    startDeployment_deploymentStrategyId,
    startDeployment_configurationProfileId,
    startDeployment_configurationVersion,
    deployment_deploymentStrategyId,
    deployment_growthType,
    deployment_state,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_description,
    deployment_finalBakeTimeInMinutes,
    deployment_startedAt,
    deployment_configurationName,
    deployment_growthFactor,
    deployment_eventLog,
    deployment_configurationVersion,
    deployment_environmentId,
    deployment_percentageComplete,
    deployment_configurationLocationUri,
    deployment_applicationId,
    deployment_completedAt,
    deployment_configurationProfileId,

    -- ** StopDeployment
    stopDeployment_applicationId,
    stopDeployment_environmentId,
    stopDeployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_growthType,
    deployment_state,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_description,
    deployment_finalBakeTimeInMinutes,
    deployment_startedAt,
    deployment_configurationName,
    deployment_growthFactor,
    deployment_eventLog,
    deployment_configurationVersion,
    deployment_environmentId,
    deployment_percentageComplete,
    deployment_configurationLocationUri,
    deployment_applicationId,
    deployment_completedAt,
    deployment_configurationProfileId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_applicationId,
    application_name,
    application_id,
    application_description,

    -- ** UpdateConfigurationProfile
    updateConfigurationProfile_name,
    updateConfigurationProfile_retrievalRoleArn,
    updateConfigurationProfile_description,
    updateConfigurationProfile_validators,
    updateConfigurationProfile_applicationId,
    updateConfigurationProfile_configurationProfileId,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,

    -- ** UpdateDeploymentStrategy
    updateDeploymentStrategy_growthType,
    updateDeploymentStrategy_deploymentDurationInMinutes,
    updateDeploymentStrategy_description,
    updateDeploymentStrategy_finalBakeTimeInMinutes,
    updateDeploymentStrategy_growthFactor,
    updateDeploymentStrategy_deploymentStrategyId,
    deploymentStrategy_name,
    deploymentStrategy_growthType,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_id,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,

    -- ** UpdateEnvironment
    updateEnvironment_name,
    updateEnvironment_monitors,
    updateEnvironment_description,
    updateEnvironment_applicationId,
    updateEnvironment_environmentId,
    environment_name,
    environment_state,
    environment_monitors,
    environment_id,
    environment_description,
    environment_applicationId,

    -- ** ValidateConfiguration
    validateConfiguration_applicationId,
    validateConfiguration_configurationProfileId,
    validateConfiguration_configurationVersion,

    -- * Types

    -- ** Application
    application_name,
    application_id,
    application_description,

    -- ** ConfigurationProfile
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,

    -- ** ConfigurationProfileSummary
    configurationProfileSummary_name,
    configurationProfileSummary_validatorTypes,
    configurationProfileSummary_id,
    configurationProfileSummary_locationUri,
    configurationProfileSummary_applicationId,

    -- ** Deployment
    deployment_deploymentStrategyId,
    deployment_growthType,
    deployment_state,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_description,
    deployment_finalBakeTimeInMinutes,
    deployment_startedAt,
    deployment_configurationName,
    deployment_growthFactor,
    deployment_eventLog,
    deployment_configurationVersion,
    deployment_environmentId,
    deployment_percentageComplete,
    deployment_configurationLocationUri,
    deployment_applicationId,
    deployment_completedAt,
    deployment_configurationProfileId,

    -- ** DeploymentEvent
    deploymentEvent_eventType,
    deploymentEvent_occurredAt,
    deploymentEvent_description,
    deploymentEvent_triggeredBy,

    -- ** DeploymentStrategy
    deploymentStrategy_name,
    deploymentStrategy_growthType,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_id,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,

    -- ** DeploymentSummary
    deploymentSummary_growthType,
    deploymentSummary_state,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_deploymentNumber,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_startedAt,
    deploymentSummary_configurationName,
    deploymentSummary_growthFactor,
    deploymentSummary_configurationVersion,
    deploymentSummary_percentageComplete,
    deploymentSummary_completedAt,

    -- ** Environment
    environment_name,
    environment_state,
    environment_monitors,
    environment_id,
    environment_description,
    environment_applicationId,

    -- ** HostedConfigurationVersion
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- ** HostedConfigurationVersionSummary
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_versionNumber,
    hostedConfigurationVersionSummary_applicationId,
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

import Amazonka.AppConfig.CreateApplication
import Amazonka.AppConfig.CreateConfigurationProfile
import Amazonka.AppConfig.CreateDeploymentStrategy
import Amazonka.AppConfig.CreateEnvironment
import Amazonka.AppConfig.CreateHostedConfigurationVersion
import Amazonka.AppConfig.DeleteApplication
import Amazonka.AppConfig.DeleteConfigurationProfile
import Amazonka.AppConfig.DeleteDeploymentStrategy
import Amazonka.AppConfig.DeleteEnvironment
import Amazonka.AppConfig.DeleteHostedConfigurationVersion
import Amazonka.AppConfig.GetApplication
import Amazonka.AppConfig.GetConfiguration
import Amazonka.AppConfig.GetConfigurationProfile
import Amazonka.AppConfig.GetDeployment
import Amazonka.AppConfig.GetDeploymentStrategy
import Amazonka.AppConfig.GetEnvironment
import Amazonka.AppConfig.GetHostedConfigurationVersion
import Amazonka.AppConfig.ListApplications
import Amazonka.AppConfig.ListConfigurationProfiles
import Amazonka.AppConfig.ListDeploymentStrategies
import Amazonka.AppConfig.ListDeployments
import Amazonka.AppConfig.ListEnvironments
import Amazonka.AppConfig.ListHostedConfigurationVersions
import Amazonka.AppConfig.ListTagsForResource
import Amazonka.AppConfig.StartDeployment
import Amazonka.AppConfig.StopDeployment
import Amazonka.AppConfig.TagResource
import Amazonka.AppConfig.Types.Application
import Amazonka.AppConfig.Types.ConfigurationProfile
import Amazonka.AppConfig.Types.ConfigurationProfileSummary
import Amazonka.AppConfig.Types.Deployment
import Amazonka.AppConfig.Types.DeploymentEvent
import Amazonka.AppConfig.Types.DeploymentStrategy
import Amazonka.AppConfig.Types.DeploymentSummary
import Amazonka.AppConfig.Types.Environment
import Amazonka.AppConfig.Types.HostedConfigurationVersion
import Amazonka.AppConfig.Types.HostedConfigurationVersionSummary
import Amazonka.AppConfig.Types.Monitor
import Amazonka.AppConfig.Types.Validator
import Amazonka.AppConfig.UntagResource
import Amazonka.AppConfig.UpdateApplication
import Amazonka.AppConfig.UpdateConfigurationProfile
import Amazonka.AppConfig.UpdateDeploymentStrategy
import Amazonka.AppConfig.UpdateEnvironment
import Amazonka.AppConfig.ValidateConfiguration
