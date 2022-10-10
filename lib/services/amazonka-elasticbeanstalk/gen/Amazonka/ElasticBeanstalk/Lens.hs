{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Lens
  ( -- * Operations

    -- ** AbortEnvironmentUpdate
    abortEnvironmentUpdate_environmentName,
    abortEnvironmentUpdate_environmentId,

    -- ** ApplyEnvironmentManagedAction
    applyEnvironmentManagedAction_environmentName,
    applyEnvironmentManagedAction_environmentId,
    applyEnvironmentManagedAction_actionId,
    applyEnvironmentManagedActionResponse_actionType,
    applyEnvironmentManagedActionResponse_status,
    applyEnvironmentManagedActionResponse_actionId,
    applyEnvironmentManagedActionResponse_actionDescription,
    applyEnvironmentManagedActionResponse_httpStatus,

    -- ** AssociateEnvironmentOperationsRole
    associateEnvironmentOperationsRole_environmentName,
    associateEnvironmentOperationsRole_operationsRole,

    -- ** CheckDNSAvailability
    checkDNSAvailability_cNAMEPrefix,
    checkDNSAvailabilityResponse_available,
    checkDNSAvailabilityResponse_fullyQualifiedCNAME,
    checkDNSAvailabilityResponse_httpStatus,

    -- ** ComposeEnvironments
    composeEnvironments_versionLabels,
    composeEnvironments_groupName,
    composeEnvironments_applicationName,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_resourceLifecycleConfig,
    createApplication_description,
    createApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** CreateApplicationVersion
    createApplicationVersion_tags,
    createApplicationVersion_autoCreateApplication,
    createApplicationVersion_description,
    createApplicationVersion_sourceBundle,
    createApplicationVersion_sourceBuildInformation,
    createApplicationVersion_process,
    createApplicationVersion_buildConfiguration,
    createApplicationVersion_applicationName,
    createApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** CreateConfigurationTemplate
    createConfigurationTemplate_tags,
    createConfigurationTemplate_sourceConfiguration,
    createConfigurationTemplate_description,
    createConfigurationTemplate_solutionStackName,
    createConfigurationTemplate_platformArn,
    createConfigurationTemplate_environmentId,
    createConfigurationTemplate_optionSettings,
    createConfigurationTemplate_applicationName,
    createConfigurationTemplate_templateName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_description,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_optionSettings,

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_templateName,
    createEnvironment_environmentName,
    createEnvironment_groupName,
    createEnvironment_description,
    createEnvironment_cNAMEPrefix,
    createEnvironment_tier,
    createEnvironment_solutionStackName,
    createEnvironment_optionsToRemove,
    createEnvironment_platformArn,
    createEnvironment_operationsRole,
    createEnvironment_versionLabel,
    createEnvironment_optionSettings,
    createEnvironment_applicationName,
    environmentDescription_templateName,
    environmentDescription_cname,
    environmentDescription_environmentName,
    environmentDescription_healthStatus,
    environmentDescription_status,
    environmentDescription_description,
    environmentDescription_tier,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_health,
    environmentDescription_solutionStackName,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_environmentArn,
    environmentDescription_environmentLinks,
    environmentDescription_resources,
    environmentDescription_platformArn,
    environmentDescription_environmentId,
    environmentDescription_operationsRole,
    environmentDescription_versionLabel,
    environmentDescription_applicationName,

    -- ** CreatePlatformVersion
    createPlatformVersion_tags,
    createPlatformVersion_environmentName,
    createPlatformVersion_optionSettings,
    createPlatformVersion_platformName,
    createPlatformVersion_platformVersion,
    createPlatformVersion_platformDefinitionBundle,
    createPlatformVersionResponse_platformSummary,
    createPlatformVersionResponse_builder,
    createPlatformVersionResponse_httpStatus,

    -- ** CreateStorageLocation
    createStorageLocationResponse_s3Bucket,
    createStorageLocationResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_terminateEnvByForce,
    deleteApplication_applicationName,

    -- ** DeleteApplicationVersion
    deleteApplicationVersion_deleteSourceBundle,
    deleteApplicationVersion_applicationName,
    deleteApplicationVersion_versionLabel,

    -- ** DeleteConfigurationTemplate
    deleteConfigurationTemplate_applicationName,
    deleteConfigurationTemplate_templateName,

    -- ** DeleteEnvironmentConfiguration
    deleteEnvironmentConfiguration_applicationName,
    deleteEnvironmentConfiguration_environmentName,

    -- ** DeletePlatformVersion
    deletePlatformVersion_platformArn,
    deletePlatformVersionResponse_platformSummary,
    deletePlatformVersionResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_resourceQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeApplicationVersions
    describeApplicationVersions_nextToken,
    describeApplicationVersions_versionLabels,
    describeApplicationVersions_maxRecords,
    describeApplicationVersions_applicationName,
    describeApplicationVersionsResponse_nextToken,
    describeApplicationVersionsResponse_applicationVersions,
    describeApplicationVersionsResponse_httpStatus,

    -- ** DescribeApplications
    describeApplications_applicationNames,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,

    -- ** DescribeConfigurationOptions
    describeConfigurationOptions_templateName,
    describeConfigurationOptions_environmentName,
    describeConfigurationOptions_options,
    describeConfigurationOptions_solutionStackName,
    describeConfigurationOptions_platformArn,
    describeConfigurationOptions_applicationName,
    describeConfigurationOptionsResponse_options,
    describeConfigurationOptionsResponse_solutionStackName,
    describeConfigurationOptionsResponse_platformArn,
    describeConfigurationOptionsResponse_httpStatus,

    -- ** DescribeConfigurationSettings
    describeConfigurationSettings_templateName,
    describeConfigurationSettings_environmentName,
    describeConfigurationSettings_applicationName,
    describeConfigurationSettingsResponse_configurationSettings,
    describeConfigurationSettingsResponse_httpStatus,

    -- ** DescribeEnvironmentHealth
    describeEnvironmentHealth_environmentName,
    describeEnvironmentHealth_attributeNames,
    describeEnvironmentHealth_environmentId,
    describeEnvironmentHealthResponse_instancesHealth,
    describeEnvironmentHealthResponse_environmentName,
    describeEnvironmentHealthResponse_color,
    describeEnvironmentHealthResponse_healthStatus,
    describeEnvironmentHealthResponse_status,
    describeEnvironmentHealthResponse_applicationMetrics,
    describeEnvironmentHealthResponse_causes,
    describeEnvironmentHealthResponse_refreshedAt,
    describeEnvironmentHealthResponse_httpStatus,

    -- ** DescribeEnvironmentManagedActionHistory
    describeEnvironmentManagedActionHistory_nextToken,
    describeEnvironmentManagedActionHistory_environmentName,
    describeEnvironmentManagedActionHistory_maxItems,
    describeEnvironmentManagedActionHistory_environmentId,
    describeEnvironmentManagedActionHistoryResponse_nextToken,
    describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems,
    describeEnvironmentManagedActionHistoryResponse_httpStatus,

    -- ** DescribeEnvironmentManagedActions
    describeEnvironmentManagedActions_environmentName,
    describeEnvironmentManagedActions_status,
    describeEnvironmentManagedActions_environmentId,
    describeEnvironmentManagedActionsResponse_managedActions,
    describeEnvironmentManagedActionsResponse_httpStatus,

    -- ** DescribeEnvironmentResources
    describeEnvironmentResources_environmentName,
    describeEnvironmentResources_environmentId,
    describeEnvironmentResourcesResponse_environmentResources,
    describeEnvironmentResourcesResponse_httpStatus,

    -- ** DescribeEnvironments
    describeEnvironments_nextToken,
    describeEnvironments_includeDeleted,
    describeEnvironments_maxRecords,
    describeEnvironments_environmentIds,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_environmentNames,
    describeEnvironments_versionLabel,
    describeEnvironments_applicationName,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** DescribeEvents
    describeEvents_severity,
    describeEvents_templateName,
    describeEvents_nextToken,
    describeEvents_environmentName,
    describeEvents_requestId,
    describeEvents_endTime,
    describeEvents_maxRecords,
    describeEvents_platformArn,
    describeEvents_environmentId,
    describeEvents_startTime,
    describeEvents_versionLabel,
    describeEvents_applicationName,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeInstancesHealth
    describeInstancesHealth_nextToken,
    describeInstancesHealth_environmentName,
    describeInstancesHealth_attributeNames,
    describeInstancesHealth_environmentId,
    describeInstancesHealthResponse_nextToken,
    describeInstancesHealthResponse_instanceHealthList,
    describeInstancesHealthResponse_refreshedAt,
    describeInstancesHealthResponse_httpStatus,

    -- ** DescribePlatformVersion
    describePlatformVersion_platformArn,
    describePlatformVersionResponse_platformDescription,
    describePlatformVersionResponse_httpStatus,

    -- ** DisassociateEnvironmentOperationsRole
    disassociateEnvironmentOperationsRole_environmentName,

    -- ** ListAvailableSolutionStacks
    listAvailableSolutionStacksResponse_solutionStacks,
    listAvailableSolutionStacksResponse_solutionStackDetails,
    listAvailableSolutionStacksResponse_httpStatus,

    -- ** ListPlatformBranches
    listPlatformBranches_nextToken,
    listPlatformBranches_filters,
    listPlatformBranches_maxRecords,
    listPlatformBranchesResponse_nextToken,
    listPlatformBranchesResponse_platformBranchSummaryList,
    listPlatformBranchesResponse_httpStatus,

    -- ** ListPlatformVersions
    listPlatformVersions_nextToken,
    listPlatformVersions_filters,
    listPlatformVersions_maxRecords,
    listPlatformVersionsResponse_nextToken,
    listPlatformVersionsResponse_platformSummaryList,
    listPlatformVersionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** RebuildEnvironment
    rebuildEnvironment_environmentName,
    rebuildEnvironment_environmentId,

    -- ** RequestEnvironmentInfo
    requestEnvironmentInfo_environmentName,
    requestEnvironmentInfo_environmentId,
    requestEnvironmentInfo_infoType,

    -- ** RestartAppServer
    restartAppServer_environmentName,
    restartAppServer_environmentId,

    -- ** RetrieveEnvironmentInfo
    retrieveEnvironmentInfo_environmentName,
    retrieveEnvironmentInfo_environmentId,
    retrieveEnvironmentInfo_infoType,
    retrieveEnvironmentInfoResponse_environmentInfo,
    retrieveEnvironmentInfoResponse_httpStatus,

    -- ** SwapEnvironmentCNAMEs
    swapEnvironmentCNAMEs_sourceEnvironmentId,
    swapEnvironmentCNAMEs_sourceEnvironmentName,
    swapEnvironmentCNAMEs_destinationEnvironmentId,
    swapEnvironmentCNAMEs_destinationEnvironmentName,

    -- ** TerminateEnvironment
    terminateEnvironment_environmentName,
    terminateEnvironment_forceTerminate,
    terminateEnvironment_environmentId,
    terminateEnvironment_terminateResources,
    environmentDescription_templateName,
    environmentDescription_cname,
    environmentDescription_environmentName,
    environmentDescription_healthStatus,
    environmentDescription_status,
    environmentDescription_description,
    environmentDescription_tier,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_health,
    environmentDescription_solutionStackName,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_environmentArn,
    environmentDescription_environmentLinks,
    environmentDescription_resources,
    environmentDescription_platformArn,
    environmentDescription_environmentId,
    environmentDescription_operationsRole,
    environmentDescription_versionLabel,
    environmentDescription_applicationName,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** UpdateApplicationResourceLifecycle
    updateApplicationResourceLifecycle_applicationName,
    updateApplicationResourceLifecycle_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_applicationName,
    updateApplicationResourceLifecycleResponse_httpStatus,

    -- ** UpdateApplicationVersion
    updateApplicationVersion_description,
    updateApplicationVersion_applicationName,
    updateApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** UpdateConfigurationTemplate
    updateConfigurationTemplate_description,
    updateConfigurationTemplate_optionsToRemove,
    updateConfigurationTemplate_optionSettings,
    updateConfigurationTemplate_applicationName,
    updateConfigurationTemplate_templateName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_description,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_optionSettings,

    -- ** UpdateEnvironment
    updateEnvironment_templateName,
    updateEnvironment_environmentName,
    updateEnvironment_groupName,
    updateEnvironment_description,
    updateEnvironment_tier,
    updateEnvironment_solutionStackName,
    updateEnvironment_optionsToRemove,
    updateEnvironment_platformArn,
    updateEnvironment_environmentId,
    updateEnvironment_versionLabel,
    updateEnvironment_applicationName,
    updateEnvironment_optionSettings,
    environmentDescription_templateName,
    environmentDescription_cname,
    environmentDescription_environmentName,
    environmentDescription_healthStatus,
    environmentDescription_status,
    environmentDescription_description,
    environmentDescription_tier,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_health,
    environmentDescription_solutionStackName,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_environmentArn,
    environmentDescription_environmentLinks,
    environmentDescription_resources,
    environmentDescription_platformArn,
    environmentDescription_environmentId,
    environmentDescription_operationsRole,
    environmentDescription_versionLabel,
    environmentDescription_applicationName,

    -- ** UpdateTagsForResource
    updateTagsForResource_tagsToAdd,
    updateTagsForResource_tagsToRemove,
    updateTagsForResource_resourceArn,

    -- ** ValidateConfigurationSettings
    validateConfigurationSettings_templateName,
    validateConfigurationSettings_environmentName,
    validateConfigurationSettings_applicationName,
    validateConfigurationSettings_optionSettings,
    validateConfigurationSettingsResponse_messages,
    validateConfigurationSettingsResponse_httpStatus,

    -- * Types

    -- ** ApplicationDescription
    applicationDescription_applicationArn,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_description,
    applicationDescription_configurationTemplates,
    applicationDescription_versions,
    applicationDescription_dateUpdated,
    applicationDescription_dateCreated,
    applicationDescription_applicationName,

    -- ** ApplicationDescriptionMessage
    applicationDescriptionMessage_application,

    -- ** ApplicationMetrics
    applicationMetrics_latency,
    applicationMetrics_duration,
    applicationMetrics_requestCount,
    applicationMetrics_statusCodes,

    -- ** ApplicationResourceLifecycleConfig
    applicationResourceLifecycleConfig_versionLifecycleConfig,
    applicationResourceLifecycleConfig_serviceRole,

    -- ** ApplicationVersionDescription
    applicationVersionDescription_buildArn,
    applicationVersionDescription_status,
    applicationVersionDescription_description,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_versionLabel,
    applicationVersionDescription_applicationName,

    -- ** ApplicationVersionDescriptionMessage
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** ApplicationVersionLifecycleConfig
    applicationVersionLifecycleConfig_maxCountRule,
    applicationVersionLifecycleConfig_maxAgeRule,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** BuildConfiguration
    buildConfiguration_artifactName,
    buildConfiguration_timeoutInMinutes,
    buildConfiguration_computeType,
    buildConfiguration_codeBuildServiceRole,
    buildConfiguration_image,

    -- ** Builder
    builder_arn,

    -- ** CPUUtilization
    cPUUtilization_nice,
    cPUUtilization_user,
    cPUUtilization_softIRQ,
    cPUUtilization_iOWait,
    cPUUtilization_privileged,
    cPUUtilization_irq,
    cPUUtilization_system,
    cPUUtilization_idle,

    -- ** ConfigurationOptionDescription
    configurationOptionDescription_name,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_regex,
    configurationOptionDescription_defaultValue,
    configurationOptionDescription_minValue,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_valueType,
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_namespace,
    configurationOptionDescription_maxValue,

    -- ** ConfigurationOptionSetting
    configurationOptionSetting_resourceName,
    configurationOptionSetting_optionName,
    configurationOptionSetting_namespace,
    configurationOptionSetting_value,

    -- ** ConfigurationSettingsDescription
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_description,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_optionSettings,

    -- ** CustomAmi
    customAmi_virtualizationType,
    customAmi_imageId,

    -- ** Deployment
    deployment_deploymentId,
    deployment_deploymentTime,
    deployment_status,
    deployment_versionLabel,

    -- ** EnvironmentDescription
    environmentDescription_templateName,
    environmentDescription_cname,
    environmentDescription_environmentName,
    environmentDescription_healthStatus,
    environmentDescription_status,
    environmentDescription_description,
    environmentDescription_tier,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_health,
    environmentDescription_solutionStackName,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_environmentArn,
    environmentDescription_environmentLinks,
    environmentDescription_resources,
    environmentDescription_platformArn,
    environmentDescription_environmentId,
    environmentDescription_operationsRole,
    environmentDescription_versionLabel,
    environmentDescription_applicationName,

    -- ** EnvironmentDescriptionsMessage
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** EnvironmentInfoDescription
    environmentInfoDescription_message,
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_sampleTimestamp,
    environmentInfoDescription_infoType,

    -- ** EnvironmentLink
    environmentLink_environmentName,
    environmentLink_linkName,

    -- ** EnvironmentResourceDescription
    environmentResourceDescription_instances,
    environmentResourceDescription_autoScalingGroups,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_triggers,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_queues,
    environmentResourceDescription_launchConfigurations,

    -- ** EnvironmentResourcesDescription
    environmentResourcesDescription_loadBalancer,

    -- ** EnvironmentTier
    environmentTier_name,
    environmentTier_type,
    environmentTier_version,

    -- ** EventDescription
    eventDescription_message,
    eventDescription_severity,
    eventDescription_templateName,
    eventDescription_environmentName,
    eventDescription_requestId,
    eventDescription_eventDate,
    eventDescription_platformArn,
    eventDescription_versionLabel,
    eventDescription_applicationName,

    -- ** Instance
    instance_id,

    -- ** InstanceHealthSummary
    instanceHealthSummary_info,
    instanceHealthSummary_warning,
    instanceHealthSummary_degraded,
    instanceHealthSummary_noData,
    instanceHealthSummary_unknown,
    instanceHealthSummary_ok,
    instanceHealthSummary_severe,
    instanceHealthSummary_pending,

    -- ** Latency
    latency_p999,
    latency_p50,
    latency_p85,
    latency_p90,
    latency_p95,
    latency_p99,
    latency_p10,
    latency_p75,

    -- ** LaunchConfiguration
    launchConfiguration_name,

    -- ** LaunchTemplate
    launchTemplate_id,

    -- ** Listener
    listener_port,
    listener_protocol,

    -- ** LoadBalancer
    loadBalancer_name,

    -- ** LoadBalancerDescription
    loadBalancerDescription_listeners,
    loadBalancerDescription_loadBalancerName,
    loadBalancerDescription_domain,

    -- ** ManagedAction
    managedAction_actionType,
    managedAction_windowStartTime,
    managedAction_status,
    managedAction_actionId,
    managedAction_actionDescription,

    -- ** ManagedActionHistoryItem
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_actionType,
    managedActionHistoryItem_failureType,
    managedActionHistoryItem_status,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_finishedTime,

    -- ** MaxAgeRule
    maxAgeRule_maxAgeInDays,
    maxAgeRule_deleteSourceFromS3,
    maxAgeRule_enabled,

    -- ** MaxCountRule
    maxCountRule_maxCount,
    maxCountRule_deleteSourceFromS3,
    maxCountRule_enabled,

    -- ** OptionRestrictionRegex
    optionRestrictionRegex_pattern,
    optionRestrictionRegex_label,

    -- ** OptionSpecification
    optionSpecification_resourceName,
    optionSpecification_optionName,
    optionSpecification_namespace,

    -- ** PlatformBranchSummary
    platformBranchSummary_branchName,
    platformBranchSummary_supportedTierList,
    platformBranchSummary_branchOrder,
    platformBranchSummary_platformName,
    platformBranchSummary_lifecycleState,

    -- ** PlatformDescription
    platformDescription_platformBranchLifecycleState,
    platformDescription_operatingSystemName,
    platformDescription_operatingSystemVersion,
    platformDescription_programmingLanguages,
    platformDescription_supportedTierList,
    platformDescription_platformName,
    platformDescription_maintainer,
    platformDescription_description,
    platformDescription_platformStatus,
    platformDescription_solutionStackName,
    platformDescription_customAmiList,
    platformDescription_platformCategory,
    platformDescription_dateUpdated,
    platformDescription_platformVersion,
    platformDescription_platformLifecycleState,
    platformDescription_frameworks,
    platformDescription_dateCreated,
    platformDescription_platformOwner,
    platformDescription_supportedAddonList,
    platformDescription_platformBranchName,
    platformDescription_platformArn,

    -- ** PlatformFilter
    platformFilter_type,
    platformFilter_operator,
    platformFilter_values,

    -- ** PlatformFramework
    platformFramework_name,
    platformFramework_version,

    -- ** PlatformProgrammingLanguage
    platformProgrammingLanguage_name,
    platformProgrammingLanguage_version,

    -- ** PlatformSummary
    platformSummary_platformBranchLifecycleState,
    platformSummary_operatingSystemName,
    platformSummary_operatingSystemVersion,
    platformSummary_supportedTierList,
    platformSummary_platformStatus,
    platformSummary_platformCategory,
    platformSummary_platformVersion,
    platformSummary_platformLifecycleState,
    platformSummary_platformOwner,
    platformSummary_supportedAddonList,
    platformSummary_platformBranchName,
    platformSummary_platformArn,

    -- ** Queue
    queue_name,
    queue_url,

    -- ** ResourceQuota
    resourceQuota_maximum,

    -- ** ResourceQuotas
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_environmentQuota,
    resourceQuotas_configurationTemplateQuota,
    resourceQuotas_customPlatformQuota,
    resourceQuotas_applicationQuota,

    -- ** S3Location
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- ** SearchFilter
    searchFilter_attribute,
    searchFilter_operator,
    searchFilter_values,

    -- ** SingleInstanceHealth
    singleInstanceHealth_color,
    singleInstanceHealth_deployment,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_instanceId,
    singleInstanceHealth_applicationMetrics,
    singleInstanceHealth_launchedAt,
    singleInstanceHealth_system,
    singleInstanceHealth_causes,

    -- ** SolutionStackDescription
    solutionStackDescription_permittedFileTypes,
    solutionStackDescription_solutionStackName,

    -- ** SourceBuildInformation
    sourceBuildInformation_sourceType,
    sourceBuildInformation_sourceRepository,
    sourceBuildInformation_sourceLocation,

    -- ** SourceConfiguration
    sourceConfiguration_templateName,
    sourceConfiguration_applicationName,

    -- ** StatusCodes
    statusCodes_status2xx,
    statusCodes_status5xx,
    statusCodes_status3xx,
    statusCodes_status4xx,

    -- ** SystemStatus
    systemStatus_loadAverage,
    systemStatus_cPUUtilization,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trigger
    trigger_name,

    -- ** ValidationMessage
    validationMessage_message,
    validationMessage_severity,
    validationMessage_optionName,
    validationMessage_namespace,
  )
where

import Amazonka.ElasticBeanstalk.AbortEnvironmentUpdate
import Amazonka.ElasticBeanstalk.ApplyEnvironmentManagedAction
import Amazonka.ElasticBeanstalk.AssociateEnvironmentOperationsRole
import Amazonka.ElasticBeanstalk.CheckDNSAvailability
import Amazonka.ElasticBeanstalk.ComposeEnvironments
import Amazonka.ElasticBeanstalk.CreateApplication
import Amazonka.ElasticBeanstalk.CreateApplicationVersion
import Amazonka.ElasticBeanstalk.CreateConfigurationTemplate
import Amazonka.ElasticBeanstalk.CreateEnvironment
import Amazonka.ElasticBeanstalk.CreatePlatformVersion
import Amazonka.ElasticBeanstalk.CreateStorageLocation
import Amazonka.ElasticBeanstalk.DeleteApplication
import Amazonka.ElasticBeanstalk.DeleteApplicationVersion
import Amazonka.ElasticBeanstalk.DeleteConfigurationTemplate
import Amazonka.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Amazonka.ElasticBeanstalk.DeletePlatformVersion
import Amazonka.ElasticBeanstalk.DescribeAccountAttributes
import Amazonka.ElasticBeanstalk.DescribeApplicationVersions
import Amazonka.ElasticBeanstalk.DescribeApplications
import Amazonka.ElasticBeanstalk.DescribeConfigurationOptions
import Amazonka.ElasticBeanstalk.DescribeConfigurationSettings
import Amazonka.ElasticBeanstalk.DescribeEnvironmentHealth
import Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
import Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActions
import Amazonka.ElasticBeanstalk.DescribeEnvironmentResources
import Amazonka.ElasticBeanstalk.DescribeEnvironments
import Amazonka.ElasticBeanstalk.DescribeEvents
import Amazonka.ElasticBeanstalk.DescribeInstancesHealth
import Amazonka.ElasticBeanstalk.DescribePlatformVersion
import Amazonka.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
import Amazonka.ElasticBeanstalk.ListAvailableSolutionStacks
import Amazonka.ElasticBeanstalk.ListPlatformBranches
import Amazonka.ElasticBeanstalk.ListPlatformVersions
import Amazonka.ElasticBeanstalk.ListTagsForResource
import Amazonka.ElasticBeanstalk.RebuildEnvironment
import Amazonka.ElasticBeanstalk.RequestEnvironmentInfo
import Amazonka.ElasticBeanstalk.RestartAppServer
import Amazonka.ElasticBeanstalk.RetrieveEnvironmentInfo
import Amazonka.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Amazonka.ElasticBeanstalk.TerminateEnvironment
import Amazonka.ElasticBeanstalk.Types.ApplicationDescription
import Amazonka.ElasticBeanstalk.Types.ApplicationDescriptionMessage
import Amazonka.ElasticBeanstalk.Types.ApplicationMetrics
import Amazonka.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescription
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Amazonka.ElasticBeanstalk.Types.AutoScalingGroup
import Amazonka.ElasticBeanstalk.Types.BuildConfiguration
import Amazonka.ElasticBeanstalk.Types.Builder
import Amazonka.ElasticBeanstalk.Types.CPUUtilization
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionDescription
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Amazonka.ElasticBeanstalk.Types.ConfigurationSettingsDescription
import Amazonka.ElasticBeanstalk.Types.CustomAmi
import Amazonka.ElasticBeanstalk.Types.Deployment
import Amazonka.ElasticBeanstalk.Types.EnvironmentDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
import Amazonka.ElasticBeanstalk.Types.EnvironmentInfoDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentLink
import Amazonka.ElasticBeanstalk.Types.EnvironmentResourceDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentTier
import Amazonka.ElasticBeanstalk.Types.EventDescription
import Amazonka.ElasticBeanstalk.Types.Instance
import Amazonka.ElasticBeanstalk.Types.InstanceHealthSummary
import Amazonka.ElasticBeanstalk.Types.Latency
import Amazonka.ElasticBeanstalk.Types.LaunchConfiguration
import Amazonka.ElasticBeanstalk.Types.LaunchTemplate
import Amazonka.ElasticBeanstalk.Types.Listener
import Amazonka.ElasticBeanstalk.Types.LoadBalancer
import Amazonka.ElasticBeanstalk.Types.LoadBalancerDescription
import Amazonka.ElasticBeanstalk.Types.ManagedAction
import Amazonka.ElasticBeanstalk.Types.ManagedActionHistoryItem
import Amazonka.ElasticBeanstalk.Types.MaxAgeRule
import Amazonka.ElasticBeanstalk.Types.MaxCountRule
import Amazonka.ElasticBeanstalk.Types.OptionRestrictionRegex
import Amazonka.ElasticBeanstalk.Types.OptionSpecification
import Amazonka.ElasticBeanstalk.Types.PlatformBranchSummary
import Amazonka.ElasticBeanstalk.Types.PlatformDescription
import Amazonka.ElasticBeanstalk.Types.PlatformFilter
import Amazonka.ElasticBeanstalk.Types.PlatformFramework
import Amazonka.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Amazonka.ElasticBeanstalk.Types.PlatformSummary
import Amazonka.ElasticBeanstalk.Types.Queue
import Amazonka.ElasticBeanstalk.Types.ResourceQuota
import Amazonka.ElasticBeanstalk.Types.ResourceQuotas
import Amazonka.ElasticBeanstalk.Types.S3Location
import Amazonka.ElasticBeanstalk.Types.SearchFilter
import Amazonka.ElasticBeanstalk.Types.SingleInstanceHealth
import Amazonka.ElasticBeanstalk.Types.SolutionStackDescription
import Amazonka.ElasticBeanstalk.Types.SourceBuildInformation
import Amazonka.ElasticBeanstalk.Types.SourceConfiguration
import Amazonka.ElasticBeanstalk.Types.StatusCodes
import Amazonka.ElasticBeanstalk.Types.SystemStatus
import Amazonka.ElasticBeanstalk.Types.Tag
import Amazonka.ElasticBeanstalk.Types.Trigger
import Amazonka.ElasticBeanstalk.Types.ValidationMessage
import Amazonka.ElasticBeanstalk.UpdateApplication
import Amazonka.ElasticBeanstalk.UpdateApplicationResourceLifecycle
import Amazonka.ElasticBeanstalk.UpdateApplicationVersion
import Amazonka.ElasticBeanstalk.UpdateConfigurationTemplate
import Amazonka.ElasticBeanstalk.UpdateEnvironment
import Amazonka.ElasticBeanstalk.UpdateTagsForResource
import Amazonka.ElasticBeanstalk.ValidateConfigurationSettings
