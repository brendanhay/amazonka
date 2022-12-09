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
    abortEnvironmentUpdate_environmentId,
    abortEnvironmentUpdate_environmentName,

    -- ** ApplyEnvironmentManagedAction
    applyEnvironmentManagedAction_environmentId,
    applyEnvironmentManagedAction_environmentName,
    applyEnvironmentManagedAction_actionId,
    applyEnvironmentManagedActionResponse_actionDescription,
    applyEnvironmentManagedActionResponse_actionId,
    applyEnvironmentManagedActionResponse_actionType,
    applyEnvironmentManagedActionResponse_status,
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
    composeEnvironments_applicationName,
    composeEnvironments_groupName,
    composeEnvironments_versionLabels,
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,

    -- ** CreateApplication
    createApplication_description,
    createApplication_resourceLifecycleConfig,
    createApplication_tags,
    createApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** CreateApplicationVersion
    createApplicationVersion_autoCreateApplication,
    createApplicationVersion_buildConfiguration,
    createApplicationVersion_description,
    createApplicationVersion_process,
    createApplicationVersion_sourceBuildInformation,
    createApplicationVersion_sourceBundle,
    createApplicationVersion_tags,
    createApplicationVersion_applicationName,
    createApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** CreateConfigurationTemplate
    createConfigurationTemplate_description,
    createConfigurationTemplate_environmentId,
    createConfigurationTemplate_optionSettings,
    createConfigurationTemplate_platformArn,
    createConfigurationTemplate_solutionStackName,
    createConfigurationTemplate_sourceConfiguration,
    createConfigurationTemplate_tags,
    createConfigurationTemplate_applicationName,
    createConfigurationTemplate_templateName,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_description,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_templateName,

    -- ** CreateEnvironment
    createEnvironment_cNAMEPrefix,
    createEnvironment_description,
    createEnvironment_environmentName,
    createEnvironment_groupName,
    createEnvironment_operationsRole,
    createEnvironment_optionSettings,
    createEnvironment_optionsToRemove,
    createEnvironment_platformArn,
    createEnvironment_solutionStackName,
    createEnvironment_tags,
    createEnvironment_templateName,
    createEnvironment_tier,
    createEnvironment_versionLabel,
    createEnvironment_applicationName,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,

    -- ** CreatePlatformVersion
    createPlatformVersion_environmentName,
    createPlatformVersion_optionSettings,
    createPlatformVersion_tags,
    createPlatformVersion_platformName,
    createPlatformVersion_platformVersion,
    createPlatformVersion_platformDefinitionBundle,
    createPlatformVersionResponse_builder,
    createPlatformVersionResponse_platformSummary,
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
    describeApplicationVersions_applicationName,
    describeApplicationVersions_maxRecords,
    describeApplicationVersions_nextToken,
    describeApplicationVersions_versionLabels,
    describeApplicationVersionsResponse_applicationVersions,
    describeApplicationVersionsResponse_nextToken,
    describeApplicationVersionsResponse_httpStatus,

    -- ** DescribeApplications
    describeApplications_applicationNames,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,

    -- ** DescribeConfigurationOptions
    describeConfigurationOptions_applicationName,
    describeConfigurationOptions_environmentName,
    describeConfigurationOptions_options,
    describeConfigurationOptions_platformArn,
    describeConfigurationOptions_solutionStackName,
    describeConfigurationOptions_templateName,
    describeConfigurationOptionsResponse_options,
    describeConfigurationOptionsResponse_platformArn,
    describeConfigurationOptionsResponse_solutionStackName,
    describeConfigurationOptionsResponse_httpStatus,

    -- ** DescribeConfigurationSettings
    describeConfigurationSettings_environmentName,
    describeConfigurationSettings_templateName,
    describeConfigurationSettings_applicationName,
    describeConfigurationSettingsResponse_configurationSettings,
    describeConfigurationSettingsResponse_httpStatus,

    -- ** DescribeEnvironmentHealth
    describeEnvironmentHealth_attributeNames,
    describeEnvironmentHealth_environmentId,
    describeEnvironmentHealth_environmentName,
    describeEnvironmentHealthResponse_applicationMetrics,
    describeEnvironmentHealthResponse_causes,
    describeEnvironmentHealthResponse_color,
    describeEnvironmentHealthResponse_environmentName,
    describeEnvironmentHealthResponse_healthStatus,
    describeEnvironmentHealthResponse_instancesHealth,
    describeEnvironmentHealthResponse_refreshedAt,
    describeEnvironmentHealthResponse_status,
    describeEnvironmentHealthResponse_httpStatus,

    -- ** DescribeEnvironmentManagedActionHistory
    describeEnvironmentManagedActionHistory_environmentId,
    describeEnvironmentManagedActionHistory_environmentName,
    describeEnvironmentManagedActionHistory_maxItems,
    describeEnvironmentManagedActionHistory_nextToken,
    describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems,
    describeEnvironmentManagedActionHistoryResponse_nextToken,
    describeEnvironmentManagedActionHistoryResponse_httpStatus,

    -- ** DescribeEnvironmentManagedActions
    describeEnvironmentManagedActions_environmentId,
    describeEnvironmentManagedActions_environmentName,
    describeEnvironmentManagedActions_status,
    describeEnvironmentManagedActionsResponse_managedActions,
    describeEnvironmentManagedActionsResponse_httpStatus,

    -- ** DescribeEnvironmentResources
    describeEnvironmentResources_environmentId,
    describeEnvironmentResources_environmentName,
    describeEnvironmentResourcesResponse_environmentResources,
    describeEnvironmentResourcesResponse_httpStatus,

    -- ** DescribeEnvironments
    describeEnvironments_applicationName,
    describeEnvironments_environmentIds,
    describeEnvironments_environmentNames,
    describeEnvironments_includeDeleted,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_maxRecords,
    describeEnvironments_nextToken,
    describeEnvironments_versionLabel,
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,

    -- ** DescribeEvents
    describeEvents_applicationName,
    describeEvents_endTime,
    describeEvents_environmentId,
    describeEvents_environmentName,
    describeEvents_maxRecords,
    describeEvents_nextToken,
    describeEvents_platformArn,
    describeEvents_requestId,
    describeEvents_severity,
    describeEvents_startTime,
    describeEvents_templateName,
    describeEvents_versionLabel,
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,

    -- ** DescribeInstancesHealth
    describeInstancesHealth_attributeNames,
    describeInstancesHealth_environmentId,
    describeInstancesHealth_environmentName,
    describeInstancesHealth_nextToken,
    describeInstancesHealthResponse_instanceHealthList,
    describeInstancesHealthResponse_nextToken,
    describeInstancesHealthResponse_refreshedAt,
    describeInstancesHealthResponse_httpStatus,

    -- ** DescribePlatformVersion
    describePlatformVersion_platformArn,
    describePlatformVersionResponse_platformDescription,
    describePlatformVersionResponse_httpStatus,

    -- ** DisassociateEnvironmentOperationsRole
    disassociateEnvironmentOperationsRole_environmentName,

    -- ** ListAvailableSolutionStacks
    listAvailableSolutionStacksResponse_solutionStackDetails,
    listAvailableSolutionStacksResponse_solutionStacks,
    listAvailableSolutionStacksResponse_httpStatus,

    -- ** ListPlatformBranches
    listPlatformBranches_filters,
    listPlatformBranches_maxRecords,
    listPlatformBranches_nextToken,
    listPlatformBranchesResponse_nextToken,
    listPlatformBranchesResponse_platformBranchSummaryList,
    listPlatformBranchesResponse_httpStatus,

    -- ** ListPlatformVersions
    listPlatformVersions_filters,
    listPlatformVersions_maxRecords,
    listPlatformVersions_nextToken,
    listPlatformVersionsResponse_nextToken,
    listPlatformVersionsResponse_platformSummaryList,
    listPlatformVersionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- ** RebuildEnvironment
    rebuildEnvironment_environmentId,
    rebuildEnvironment_environmentName,

    -- ** RequestEnvironmentInfo
    requestEnvironmentInfo_environmentId,
    requestEnvironmentInfo_environmentName,
    requestEnvironmentInfo_infoType,

    -- ** RestartAppServer
    restartAppServer_environmentId,
    restartAppServer_environmentName,

    -- ** RetrieveEnvironmentInfo
    retrieveEnvironmentInfo_environmentId,
    retrieveEnvironmentInfo_environmentName,
    retrieveEnvironmentInfo_infoType,
    retrieveEnvironmentInfoResponse_environmentInfo,
    retrieveEnvironmentInfoResponse_httpStatus,

    -- ** SwapEnvironmentCNAMEs
    swapEnvironmentCNAMEs_destinationEnvironmentId,
    swapEnvironmentCNAMEs_destinationEnvironmentName,
    swapEnvironmentCNAMEs_sourceEnvironmentId,
    swapEnvironmentCNAMEs_sourceEnvironmentName,

    -- ** TerminateEnvironment
    terminateEnvironment_environmentId,
    terminateEnvironment_environmentName,
    terminateEnvironment_forceTerminate,
    terminateEnvironment_terminateResources,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** UpdateApplicationResourceLifecycle
    updateApplicationResourceLifecycle_applicationName,
    updateApplicationResourceLifecycle_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_applicationName,
    updateApplicationResourceLifecycleResponse_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_httpStatus,

    -- ** UpdateApplicationVersion
    updateApplicationVersion_description,
    updateApplicationVersion_applicationName,
    updateApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** UpdateConfigurationTemplate
    updateConfigurationTemplate_description,
    updateConfigurationTemplate_optionSettings,
    updateConfigurationTemplate_optionsToRemove,
    updateConfigurationTemplate_applicationName,
    updateConfigurationTemplate_templateName,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_description,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_templateName,

    -- ** UpdateEnvironment
    updateEnvironment_applicationName,
    updateEnvironment_description,
    updateEnvironment_environmentId,
    updateEnvironment_environmentName,
    updateEnvironment_groupName,
    updateEnvironment_optionSettings,
    updateEnvironment_optionsToRemove,
    updateEnvironment_platformArn,
    updateEnvironment_solutionStackName,
    updateEnvironment_templateName,
    updateEnvironment_tier,
    updateEnvironment_versionLabel,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,

    -- ** UpdateTagsForResource
    updateTagsForResource_tagsToAdd,
    updateTagsForResource_tagsToRemove,
    updateTagsForResource_resourceArn,

    -- ** ValidateConfigurationSettings
    validateConfigurationSettings_environmentName,
    validateConfigurationSettings_templateName,
    validateConfigurationSettings_applicationName,
    validateConfigurationSettings_optionSettings,
    validateConfigurationSettingsResponse_messages,
    validateConfigurationSettingsResponse_httpStatus,

    -- * Types

    -- ** ApplicationDescription
    applicationDescription_applicationArn,
    applicationDescription_applicationName,
    applicationDescription_configurationTemplates,
    applicationDescription_dateCreated,
    applicationDescription_dateUpdated,
    applicationDescription_description,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_versions,

    -- ** ApplicationDescriptionMessage
    applicationDescriptionMessage_application,

    -- ** ApplicationMetrics
    applicationMetrics_duration,
    applicationMetrics_latency,
    applicationMetrics_requestCount,
    applicationMetrics_statusCodes,

    -- ** ApplicationResourceLifecycleConfig
    applicationResourceLifecycleConfig_serviceRole,
    applicationResourceLifecycleConfig_versionLifecycleConfig,

    -- ** ApplicationVersionDescription
    applicationVersionDescription_applicationName,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_buildArn,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_description,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_status,
    applicationVersionDescription_versionLabel,

    -- ** ApplicationVersionDescriptionMessage
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** ApplicationVersionLifecycleConfig
    applicationVersionLifecycleConfig_maxAgeRule,
    applicationVersionLifecycleConfig_maxCountRule,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** BuildConfiguration
    buildConfiguration_artifactName,
    buildConfiguration_computeType,
    buildConfiguration_timeoutInMinutes,
    buildConfiguration_codeBuildServiceRole,
    buildConfiguration_image,

    -- ** Builder
    builder_arn,

    -- ** CPUUtilization
    cPUUtilization_iOWait,
    cPUUtilization_irq,
    cPUUtilization_idle,
    cPUUtilization_nice,
    cPUUtilization_privileged,
    cPUUtilization_softIRQ,
    cPUUtilization_system,
    cPUUtilization_user,

    -- ** ConfigurationOptionDescription
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_defaultValue,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_maxValue,
    configurationOptionDescription_minValue,
    configurationOptionDescription_name,
    configurationOptionDescription_namespace,
    configurationOptionDescription_regex,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_valueType,

    -- ** ConfigurationOptionSetting
    configurationOptionSetting_namespace,
    configurationOptionSetting_optionName,
    configurationOptionSetting_resourceName,
    configurationOptionSetting_value,

    -- ** ConfigurationSettingsDescription
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_description,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_templateName,

    -- ** CustomAmi
    customAmi_imageId,
    customAmi_virtualizationType,

    -- ** Deployment
    deployment_deploymentId,
    deployment_deploymentTime,
    deployment_status,
    deployment_versionLabel,

    -- ** EnvironmentDescription
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,

    -- ** EnvironmentDescriptionsMessage
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,

    -- ** EnvironmentInfoDescription
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_infoType,
    environmentInfoDescription_message,
    environmentInfoDescription_sampleTimestamp,

    -- ** EnvironmentLink
    environmentLink_environmentName,
    environmentLink_linkName,

    -- ** EnvironmentResourceDescription
    environmentResourceDescription_autoScalingGroups,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_instances,
    environmentResourceDescription_launchConfigurations,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_queues,
    environmentResourceDescription_triggers,

    -- ** EnvironmentResourcesDescription
    environmentResourcesDescription_loadBalancer,

    -- ** EnvironmentTier
    environmentTier_name,
    environmentTier_type,
    environmentTier_version,

    -- ** EventDescription
    eventDescription_applicationName,
    eventDescription_environmentName,
    eventDescription_eventDate,
    eventDescription_message,
    eventDescription_platformArn,
    eventDescription_requestId,
    eventDescription_severity,
    eventDescription_templateName,
    eventDescription_versionLabel,

    -- ** Instance
    instance_id,

    -- ** InstanceHealthSummary
    instanceHealthSummary_degraded,
    instanceHealthSummary_info,
    instanceHealthSummary_noData,
    instanceHealthSummary_ok,
    instanceHealthSummary_pending,
    instanceHealthSummary_severe,
    instanceHealthSummary_unknown,
    instanceHealthSummary_warning,

    -- ** Latency
    latency_p10,
    latency_p50,
    latency_p75,
    latency_p85,
    latency_p90,
    latency_p95,
    latency_p99,
    latency_p999,

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
    loadBalancerDescription_domain,
    loadBalancerDescription_listeners,
    loadBalancerDescription_loadBalancerName,

    -- ** ManagedAction
    managedAction_actionDescription,
    managedAction_actionId,
    managedAction_actionType,
    managedAction_status,
    managedAction_windowStartTime,

    -- ** ManagedActionHistoryItem
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_actionType,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_failureType,
    managedActionHistoryItem_finishedTime,
    managedActionHistoryItem_status,

    -- ** MaxAgeRule
    maxAgeRule_deleteSourceFromS3,
    maxAgeRule_maxAgeInDays,
    maxAgeRule_enabled,

    -- ** MaxCountRule
    maxCountRule_deleteSourceFromS3,
    maxCountRule_maxCount,
    maxCountRule_enabled,

    -- ** OptionRestrictionRegex
    optionRestrictionRegex_label,
    optionRestrictionRegex_pattern,

    -- ** OptionSpecification
    optionSpecification_namespace,
    optionSpecification_optionName,
    optionSpecification_resourceName,

    -- ** PlatformBranchSummary
    platformBranchSummary_branchName,
    platformBranchSummary_branchOrder,
    platformBranchSummary_lifecycleState,
    platformBranchSummary_platformName,
    platformBranchSummary_supportedTierList,

    -- ** PlatformDescription
    platformDescription_customAmiList,
    platformDescription_dateCreated,
    platformDescription_dateUpdated,
    platformDescription_description,
    platformDescription_frameworks,
    platformDescription_maintainer,
    platformDescription_operatingSystemName,
    platformDescription_operatingSystemVersion,
    platformDescription_platformArn,
    platformDescription_platformBranchLifecycleState,
    platformDescription_platformBranchName,
    platformDescription_platformCategory,
    platformDescription_platformLifecycleState,
    platformDescription_platformName,
    platformDescription_platformOwner,
    platformDescription_platformStatus,
    platformDescription_platformVersion,
    platformDescription_programmingLanguages,
    platformDescription_solutionStackName,
    platformDescription_supportedAddonList,
    platformDescription_supportedTierList,

    -- ** PlatformFilter
    platformFilter_operator,
    platformFilter_type,
    platformFilter_values,

    -- ** PlatformFramework
    platformFramework_name,
    platformFramework_version,

    -- ** PlatformProgrammingLanguage
    platformProgrammingLanguage_name,
    platformProgrammingLanguage_version,

    -- ** PlatformSummary
    platformSummary_operatingSystemName,
    platformSummary_operatingSystemVersion,
    platformSummary_platformArn,
    platformSummary_platformBranchLifecycleState,
    platformSummary_platformBranchName,
    platformSummary_platformCategory,
    platformSummary_platformLifecycleState,
    platformSummary_platformOwner,
    platformSummary_platformStatus,
    platformSummary_platformVersion,
    platformSummary_supportedAddonList,
    platformSummary_supportedTierList,

    -- ** Queue
    queue_name,
    queue_url,

    -- ** ResourceQuota
    resourceQuota_maximum,

    -- ** ResourceQuotas
    resourceQuotas_applicationQuota,
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_configurationTemplateQuota,
    resourceQuotas_customPlatformQuota,
    resourceQuotas_environmentQuota,

    -- ** S3Location
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- ** SearchFilter
    searchFilter_attribute,
    searchFilter_operator,
    searchFilter_values,

    -- ** SingleInstanceHealth
    singleInstanceHealth_applicationMetrics,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_causes,
    singleInstanceHealth_color,
    singleInstanceHealth_deployment,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_instanceId,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_launchedAt,
    singleInstanceHealth_system,

    -- ** SolutionStackDescription
    solutionStackDescription_permittedFileTypes,
    solutionStackDescription_solutionStackName,

    -- ** SourceBuildInformation
    sourceBuildInformation_sourceType,
    sourceBuildInformation_sourceRepository,
    sourceBuildInformation_sourceLocation,

    -- ** SourceConfiguration
    sourceConfiguration_applicationName,
    sourceConfiguration_templateName,

    -- ** StatusCodes
    statusCodes_status2xx,
    statusCodes_status3xx,
    statusCodes_status4xx,
    statusCodes_status5xx,

    -- ** SystemStatus
    systemStatus_cPUUtilization,
    systemStatus_loadAverage,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trigger
    trigger_name,

    -- ** ValidationMessage
    validationMessage_message,
    validationMessage_namespace,
    validationMessage_optionName,
    validationMessage_severity,
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
