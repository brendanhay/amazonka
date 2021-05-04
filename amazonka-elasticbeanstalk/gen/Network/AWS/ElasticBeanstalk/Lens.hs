{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Lens
  ( -- * Operations

    -- ** SwapEnvironmentCNAMEs
    swapEnvironmentCNAMEs_sourceEnvironmentName,
    swapEnvironmentCNAMEs_destinationEnvironmentId,
    swapEnvironmentCNAMEs_destinationEnvironmentName,
    swapEnvironmentCNAMEs_sourceEnvironmentId,

    -- ** ListPlatformBranches
    listPlatformBranches_nextToken,
    listPlatformBranches_filters,
    listPlatformBranches_maxRecords,
    listPlatformBranchesResponse_nextToken,
    listPlatformBranchesResponse_platformBranchSummaryList,
    listPlatformBranchesResponse_httpStatus,

    -- ** ListAvailableSolutionStacks
    listAvailableSolutionStacksResponse_solutionStacks,
    listAvailableSolutionStacksResponse_solutionStackDetails,
    listAvailableSolutionStacksResponse_httpStatus,

    -- ** DescribeEnvironmentHealth
    describeEnvironmentHealth_environmentId,
    describeEnvironmentHealth_environmentName,
    describeEnvironmentHealth_attributeNames,
    describeEnvironmentHealthResponse_status,
    describeEnvironmentHealthResponse_refreshedAt,
    describeEnvironmentHealthResponse_color,
    describeEnvironmentHealthResponse_causes,
    describeEnvironmentHealthResponse_environmentName,
    describeEnvironmentHealthResponse_instancesHealth,
    describeEnvironmentHealthResponse_healthStatus,
    describeEnvironmentHealthResponse_applicationMetrics,
    describeEnvironmentHealthResponse_httpStatus,

    -- ** CreateConfigurationTemplate
    createConfigurationTemplate_solutionStackName,
    createConfigurationTemplate_environmentId,
    createConfigurationTemplate_platformArn,
    createConfigurationTemplate_tags,
    createConfigurationTemplate_optionSettings,
    createConfigurationTemplate_description,
    createConfigurationTemplate_sourceConfiguration,
    createConfigurationTemplate_applicationName,
    createConfigurationTemplate_templateName,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_description,
    configurationSettingsDescription_applicationName,

    -- ** DescribeApplications
    describeApplications_applicationNames,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,

    -- ** ListPlatformVersions
    listPlatformVersions_nextToken,
    listPlatformVersions_filters,
    listPlatformVersions_maxRecords,
    listPlatformVersionsResponse_nextToken,
    listPlatformVersionsResponse_platformSummaryList,
    listPlatformVersionsResponse_httpStatus,

    -- ** CreateApplicationVersion
    createApplicationVersion_sourceBundle,
    createApplicationVersion_buildConfiguration,
    createApplicationVersion_sourceBuildInformation,
    createApplicationVersion_tags,
    createApplicationVersion_autoCreateApplication,
    createApplicationVersion_description,
    createApplicationVersion_process,
    createApplicationVersion_applicationName,
    createApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** TerminateEnvironment
    terminateEnvironment_forceTerminate,
    terminateEnvironment_environmentId,
    terminateEnvironment_terminateResources,
    terminateEnvironment_environmentName,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,

    -- ** DescribeEnvironmentResources
    describeEnvironmentResources_environmentId,
    describeEnvironmentResources_environmentName,
    describeEnvironmentResourcesResponse_environmentResources,
    describeEnvironmentResourcesResponse_httpStatus,

    -- ** UpdateApplicationVersion
    updateApplicationVersion_description,
    updateApplicationVersion_applicationName,
    updateApplicationVersion_versionLabel,
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** CreatePlatformVersion
    createPlatformVersion_environmentName,
    createPlatformVersion_tags,
    createPlatformVersion_optionSettings,
    createPlatformVersion_platformName,
    createPlatformVersion_platformVersion,
    createPlatformVersion_platformDefinitionBundle,
    createPlatformVersionResponse_platformSummary,
    createPlatformVersionResponse_builder,
    createPlatformVersionResponse_httpStatus,

    -- ** DeleteApplicationVersion
    deleteApplicationVersion_deleteSourceBundle,
    deleteApplicationVersion_applicationName,
    deleteApplicationVersion_versionLabel,

    -- ** CreateEnvironment
    createEnvironment_templateName,
    createEnvironment_groupName,
    createEnvironment_solutionStackName,
    createEnvironment_optionsToRemove,
    createEnvironment_environmentName,
    createEnvironment_platformArn,
    createEnvironment_versionLabel,
    createEnvironment_cNAMEPrefix,
    createEnvironment_tags,
    createEnvironment_optionSettings,
    createEnvironment_description,
    createEnvironment_tier,
    createEnvironment_operationsRole,
    createEnvironment_applicationName,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_resourceLifecycleConfig,
    createApplication_description,
    createApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** ComposeEnvironments
    composeEnvironments_groupName,
    composeEnvironments_versionLabels,
    composeEnvironments_applicationName,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** CheckDNSAvailability
    checkDNSAvailability_cNAMEPrefix,
    checkDNSAvailabilityResponse_available,
    checkDNSAvailabilityResponse_fullyQualifiedCNAME,
    checkDNSAvailabilityResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_resourceQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** ValidateConfigurationSettings
    validateConfigurationSettings_templateName,
    validateConfigurationSettings_environmentName,
    validateConfigurationSettings_applicationName,
    validateConfigurationSettings_optionSettings,
    validateConfigurationSettingsResponse_messages,
    validateConfigurationSettingsResponse_httpStatus,

    -- ** CreateStorageLocation
    createStorageLocationResponse_s3Bucket,
    createStorageLocationResponse_httpStatus,

    -- ** DescribeEnvironmentManagedActions
    describeEnvironmentManagedActions_status,
    describeEnvironmentManagedActions_environmentId,
    describeEnvironmentManagedActions_environmentName,
    describeEnvironmentManagedActionsResponse_managedActions,
    describeEnvironmentManagedActionsResponse_httpStatus,

    -- ** DescribeConfigurationSettings
    describeConfigurationSettings_templateName,
    describeConfigurationSettings_environmentName,
    describeConfigurationSettings_applicationName,
    describeConfigurationSettingsResponse_configurationSettings,
    describeConfigurationSettingsResponse_httpStatus,

    -- ** DescribeConfigurationOptions
    describeConfigurationOptions_templateName,
    describeConfigurationOptions_options,
    describeConfigurationOptions_solutionStackName,
    describeConfigurationOptions_environmentName,
    describeConfigurationOptions_platformArn,
    describeConfigurationOptions_applicationName,
    describeConfigurationOptionsResponse_options,
    describeConfigurationOptionsResponse_solutionStackName,
    describeConfigurationOptionsResponse_platformArn,
    describeConfigurationOptionsResponse_httpStatus,

    -- ** RetrieveEnvironmentInfo
    retrieveEnvironmentInfo_environmentId,
    retrieveEnvironmentInfo_environmentName,
    retrieveEnvironmentInfo_infoType,
    retrieveEnvironmentInfoResponse_environmentInfo,
    retrieveEnvironmentInfoResponse_httpStatus,

    -- ** RequestEnvironmentInfo
    requestEnvironmentInfo_environmentId,
    requestEnvironmentInfo_environmentName,
    requestEnvironmentInfo_infoType,

    -- ** DescribeEvents
    describeEvents_templateName,
    describeEvents_nextToken,
    describeEvents_severity,
    describeEvents_environmentId,
    describeEvents_startTime,
    describeEvents_environmentName,
    describeEvents_endTime,
    describeEvents_platformArn,
    describeEvents_versionLabel,
    describeEvents_requestId,
    describeEvents_applicationName,
    describeEvents_maxRecords,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** ApplyEnvironmentManagedAction
    applyEnvironmentManagedAction_environmentId,
    applyEnvironmentManagedAction_environmentName,
    applyEnvironmentManagedAction_actionId,
    applyEnvironmentManagedActionResponse_status,
    applyEnvironmentManagedActionResponse_actionType,
    applyEnvironmentManagedActionResponse_actionId,
    applyEnvironmentManagedActionResponse_actionDescription,
    applyEnvironmentManagedActionResponse_httpStatus,

    -- ** UpdateApplicationResourceLifecycle
    updateApplicationResourceLifecycle_applicationName,
    updateApplicationResourceLifecycle_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_applicationName,
    updateApplicationResourceLifecycleResponse_httpStatus,

    -- ** RebuildEnvironment
    rebuildEnvironment_environmentId,
    rebuildEnvironment_environmentName,

    -- ** UpdateEnvironment
    updateEnvironment_templateName,
    updateEnvironment_groupName,
    updateEnvironment_solutionStackName,
    updateEnvironment_environmentId,
    updateEnvironment_optionsToRemove,
    updateEnvironment_environmentName,
    updateEnvironment_platformArn,
    updateEnvironment_versionLabel,
    updateEnvironment_optionSettings,
    updateEnvironment_description,
    updateEnvironment_applicationName,
    updateEnvironment_tier,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,

    -- ** DeletePlatformVersion
    deletePlatformVersion_platformArn,
    deletePlatformVersionResponse_platformSummary,
    deletePlatformVersionResponse_httpStatus,

    -- ** DeleteEnvironmentConfiguration
    deleteEnvironmentConfiguration_applicationName,
    deleteEnvironmentConfiguration_environmentName,

    -- ** DescribeEnvironmentManagedActionHistory
    describeEnvironmentManagedActionHistory_nextToken,
    describeEnvironmentManagedActionHistory_environmentId,
    describeEnvironmentManagedActionHistory_environmentName,
    describeEnvironmentManagedActionHistory_maxItems,
    describeEnvironmentManagedActionHistoryResponse_nextToken,
    describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems,
    describeEnvironmentManagedActionHistoryResponse_httpStatus,

    -- ** UpdateConfigurationTemplate
    updateConfigurationTemplate_optionsToRemove,
    updateConfigurationTemplate_optionSettings,
    updateConfigurationTemplate_description,
    updateConfigurationTemplate_applicationName,
    updateConfigurationTemplate_templateName,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_description,
    configurationSettingsDescription_applicationName,

    -- ** DeleteConfigurationTemplate
    deleteConfigurationTemplate_applicationName,
    deleteConfigurationTemplate_templateName,

    -- ** UpdateTagsForResource
    updateTagsForResource_tagsToRemove,
    updateTagsForResource_tagsToAdd,
    updateTagsForResource_resourceArn,

    -- ** DescribeApplicationVersions
    describeApplicationVersions_nextToken,
    describeApplicationVersions_versionLabels,
    describeApplicationVersions_applicationName,
    describeApplicationVersions_maxRecords,
    describeApplicationVersionsResponse_nextToken,
    describeApplicationVersionsResponse_applicationVersions,
    describeApplicationVersionsResponse_httpStatus,

    -- ** AbortEnvironmentUpdate
    abortEnvironmentUpdate_environmentId,
    abortEnvironmentUpdate_environmentName,

    -- ** DescribeEnvironments
    describeEnvironments_nextToken,
    describeEnvironments_environmentNames,
    describeEnvironments_environmentIds,
    describeEnvironments_versionLabel,
    describeEnvironments_includeDeleted,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_applicationName,
    describeEnvironments_maxRecords,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** RestartAppServer
    restartAppServer_environmentId,
    restartAppServer_environmentName,

    -- ** AssociateEnvironmentOperationsRole
    associateEnvironmentOperationsRole_environmentName,
    associateEnvironmentOperationsRole_operationsRole,

    -- ** DeleteApplication
    deleteApplication_terminateEnvByForce,
    deleteApplication_applicationName,

    -- ** DescribeInstancesHealth
    describeInstancesHealth_nextToken,
    describeInstancesHealth_environmentId,
    describeInstancesHealth_environmentName,
    describeInstancesHealth_attributeNames,
    describeInstancesHealthResponse_instanceHealthList,
    describeInstancesHealthResponse_nextToken,
    describeInstancesHealthResponse_refreshedAt,
    describeInstancesHealthResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_applicationName,
    applicationDescriptionMessage_application,

    -- ** DescribePlatformVersion
    describePlatformVersion_platformArn,
    describePlatformVersionResponse_platformDescription,
    describePlatformVersionResponse_httpStatus,

    -- ** DisassociateEnvironmentOperationsRole
    disassociateEnvironmentOperationsRole_environmentName,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** ApplicationDescription
    applicationDescription_applicationArn,
    applicationDescription_dateCreated,
    applicationDescription_versions,
    applicationDescription_dateUpdated,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_description,
    applicationDescription_configurationTemplates,
    applicationDescription_applicationName,

    -- ** ApplicationDescriptionMessage
    applicationDescriptionMessage_application,

    -- ** ApplicationMetrics
    applicationMetrics_duration,
    applicationMetrics_statusCodes,
    applicationMetrics_requestCount,
    applicationMetrics_latency,

    -- ** ApplicationResourceLifecycleConfig
    applicationResourceLifecycleConfig_serviceRole,
    applicationResourceLifecycleConfig_versionLifecycleConfig,

    -- ** ApplicationVersionDescription
    applicationVersionDescription_status,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_versionLabel,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_description,
    applicationVersionDescription_buildArn,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_applicationName,

    -- ** ApplicationVersionDescriptionMessage
    applicationVersionDescriptionMessage_applicationVersion,

    -- ** ApplicationVersionLifecycleConfig
    applicationVersionLifecycleConfig_maxAgeRule,
    applicationVersionLifecycleConfig_maxCountRule,

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
    cPUUtilization_idle,
    cPUUtilization_user,
    cPUUtilization_privileged,
    cPUUtilization_iOWait,
    cPUUtilization_softIRQ,
    cPUUtilization_nice,
    cPUUtilization_system,
    cPUUtilization_irq,

    -- ** ConfigurationOptionDescription
    configurationOptionDescription_maxValue,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_valueType,
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_regex,
    configurationOptionDescription_name,
    configurationOptionDescription_minValue,
    configurationOptionDescription_namespace,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_defaultValue,

    -- ** ConfigurationOptionSetting
    configurationOptionSetting_optionName,
    configurationOptionSetting_value,
    configurationOptionSetting_namespace,
    configurationOptionSetting_resourceName,

    -- ** ConfigurationSettingsDescription
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_description,
    configurationSettingsDescription_applicationName,

    -- ** CustomAmi
    customAmi_virtualizationType,
    customAmi_imageId,

    -- ** Deployment
    deployment_status,
    deployment_deploymentId,
    deployment_versionLabel,
    deployment_deploymentTime,

    -- ** EnvironmentDescription
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,

    -- ** EnvironmentDescriptionsMessage
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- ** EnvironmentInfoDescription
    environmentInfoDescription_message,
    environmentInfoDescription_infoType,
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_sampleTimestamp,

    -- ** EnvironmentLink
    environmentLink_environmentName,
    environmentLink_linkName,

    -- ** EnvironmentResourceDescription
    environmentResourceDescription_launchConfigurations,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_triggers,
    environmentResourceDescription_instances,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_queues,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_autoScalingGroups,

    -- ** EnvironmentResourcesDescription
    environmentResourcesDescription_loadBalancer,

    -- ** EnvironmentTier
    environmentTier_version,
    environmentTier_name,
    environmentTier_type,

    -- ** EventDescription
    eventDescription_templateName,
    eventDescription_severity,
    eventDescription_message,
    eventDescription_eventDate,
    eventDescription_environmentName,
    eventDescription_platformArn,
    eventDescription_versionLabel,
    eventDescription_requestId,
    eventDescription_applicationName,

    -- ** Instance
    instance_id,

    -- ** InstanceHealthSummary
    instanceHealthSummary_ok,
    instanceHealthSummary_noData,
    instanceHealthSummary_info,
    instanceHealthSummary_severe,
    instanceHealthSummary_warning,
    instanceHealthSummary_pending,
    instanceHealthSummary_degraded,
    instanceHealthSummary_unknown,

    -- ** Latency
    latency_p95,
    latency_p10,
    latency_p999,
    latency_p99,
    latency_p85,
    latency_p50,
    latency_p90,
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
    loadBalancerDescription_domain,
    loadBalancerDescription_listeners,
    loadBalancerDescription_loadBalancerName,

    -- ** ManagedAction
    managedAction_status,
    managedAction_actionType,
    managedAction_actionId,
    managedAction_actionDescription,
    managedAction_windowStartTime,

    -- ** ManagedActionHistoryItem
    managedActionHistoryItem_status,
    managedActionHistoryItem_actionType,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_finishedTime,
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_failureType,

    -- ** MaxAgeRule
    maxAgeRule_deleteSourceFromS3,
    maxAgeRule_maxAgeInDays,
    maxAgeRule_enabled,

    -- ** MaxCountRule
    maxCountRule_maxCount,
    maxCountRule_deleteSourceFromS3,
    maxCountRule_enabled,

    -- ** OptionRestrictionRegex
    optionRestrictionRegex_label,
    optionRestrictionRegex_pattern,

    -- ** OptionSpecification
    optionSpecification_optionName,
    optionSpecification_namespace,
    optionSpecification_resourceName,

    -- ** PlatformBranchSummary
    platformBranchSummary_branchName,
    platformBranchSummary_branchOrder,
    platformBranchSummary_lifecycleState,
    platformBranchSummary_supportedTierList,
    platformBranchSummary_platformName,

    -- ** PlatformDescription
    platformDescription_platformCategory,
    platformDescription_operatingSystemName,
    platformDescription_platformBranchName,
    platformDescription_supportedAddonList,
    platformDescription_dateCreated,
    platformDescription_customAmiList,
    platformDescription_platformOwner,
    platformDescription_platformStatus,
    platformDescription_solutionStackName,
    platformDescription_platformVersion,
    platformDescription_platformBranchLifecycleState,
    platformDescription_platformArn,
    platformDescription_frameworks,
    platformDescription_dateUpdated,
    platformDescription_supportedTierList,
    platformDescription_platformLifecycleState,
    platformDescription_maintainer,
    platformDescription_description,
    platformDescription_platformName,
    platformDescription_programmingLanguages,
    platformDescription_operatingSystemVersion,

    -- ** PlatformFilter
    platformFilter_values,
    platformFilter_operator,
    platformFilter_type,

    -- ** PlatformFramework
    platformFramework_version,
    platformFramework_name,

    -- ** PlatformProgrammingLanguage
    platformProgrammingLanguage_version,
    platformProgrammingLanguage_name,

    -- ** PlatformSummary
    platformSummary_platformCategory,
    platformSummary_operatingSystemName,
    platformSummary_platformBranchName,
    platformSummary_supportedAddonList,
    platformSummary_platformOwner,
    platformSummary_platformStatus,
    platformSummary_platformVersion,
    platformSummary_platformBranchLifecycleState,
    platformSummary_platformArn,
    platformSummary_supportedTierList,
    platformSummary_platformLifecycleState,
    platformSummary_operatingSystemVersion,

    -- ** Queue
    queue_name,
    queue_url,

    -- ** ResourceQuota
    resourceQuota_maximum,

    -- ** ResourceQuotas
    resourceQuotas_applicationQuota,
    resourceQuotas_configurationTemplateQuota,
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_environmentQuota,
    resourceQuotas_customPlatformQuota,

    -- ** S3Location
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- ** SearchFilter
    searchFilter_values,
    searchFilter_operator,
    searchFilter_attribute,

    -- ** SingleInstanceHealth
    singleInstanceHealth_instanceId,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_color,
    singleInstanceHealth_causes,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_deployment,
    singleInstanceHealth_launchedAt,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_system,
    singleInstanceHealth_applicationMetrics,

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
    statusCodes_status3xx,
    statusCodes_status5xx,
    statusCodes_status2xx,
    statusCodes_status4xx,

    -- ** SystemStatus
    systemStatus_cPUUtilization,
    systemStatus_loadAverage,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Trigger
    trigger_name,

    -- ** ValidationMessage
    validationMessage_optionName,
    validationMessage_severity,
    validationMessage_message,
    validationMessage_namespace,
  )
where

import Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
import Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
import Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.ComposeEnvironments
import Network.AWS.ElasticBeanstalk.CreateApplication
import Network.AWS.ElasticBeanstalk.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.CreateEnvironment
import Network.AWS.ElasticBeanstalk.CreatePlatformVersion
import Network.AWS.ElasticBeanstalk.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.DeleteApplication
import Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.DeletePlatformVersion
import Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
import Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.DescribeApplications
import Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.DescribeEvents
import Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
import Network.AWS.ElasticBeanstalk.DescribePlatformVersion
import Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.ListPlatformBranches
import Network.AWS.ElasticBeanstalk.ListPlatformVersions
import Network.AWS.ElasticBeanstalk.ListTagsForResource
import Network.AWS.ElasticBeanstalk.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.RestartAppServer
import Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
import Network.AWS.ElasticBeanstalk.Types.Builder
import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
import Network.AWS.ElasticBeanstalk.Types.CustomAmi
import Network.AWS.ElasticBeanstalk.Types.Deployment
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
import Network.AWS.ElasticBeanstalk.Types.EventDescription
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.Listener
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import Network.AWS.ElasticBeanstalk.Types.ManagedAction
import Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import Network.AWS.ElasticBeanstalk.Types.OptionSpecification
import Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
import Network.AWS.ElasticBeanstalk.Types.PlatformDescription
import Network.AWS.ElasticBeanstalk.Types.PlatformFilter
import Network.AWS.ElasticBeanstalk.Types.PlatformFramework
import Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Network.AWS.ElasticBeanstalk.Types.PlatformSummary
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
import Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SearchFilter
import Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
import Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
import Network.AWS.ElasticBeanstalk.Types.Tag
import Network.AWS.ElasticBeanstalk.Types.Trigger
import Network.AWS.ElasticBeanstalk.Types.ValidationMessage
import Network.AWS.ElasticBeanstalk.UpdateApplication
import Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
import Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.UpdateTagsForResource
import Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
