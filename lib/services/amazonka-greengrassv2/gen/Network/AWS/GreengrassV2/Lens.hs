{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GreengrassV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Lens
  ( -- * Operations

    -- ** ListComponentVersions
    listComponentVersions_nextToken,
    listComponentVersions_maxResults,
    listComponentVersions_arn,
    listComponentVersionsResponse_nextToken,
    listComponentVersionsResponse_componentVersions,
    listComponentVersionsResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_targetArn,
    getDeploymentResponse_components,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_iotJobId,
    getDeploymentResponse_iotJobArn,
    getDeploymentResponse_deploymentPolicies,
    getDeploymentResponse_creationTimestamp,
    getDeploymentResponse_iotJobConfiguration,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_isLatestForTarget,
    getDeploymentResponse_revisionId,
    getDeploymentResponse_deploymentName,
    getDeploymentResponse_tags,
    getDeploymentResponse_httpStatus,

    -- ** DescribeComponent
    describeComponent_arn,
    describeComponentResponse_platforms,
    describeComponentResponse_status,
    describeComponentResponse_arn,
    describeComponentResponse_componentVersion,
    describeComponentResponse_creationTimestamp,
    describeComponentResponse_componentName,
    describeComponentResponse_publisher,
    describeComponentResponse_description,
    describeComponentResponse_tags,
    describeComponentResponse_httpStatus,

    -- ** DeleteCoreDevice
    deleteCoreDevice_coreDeviceThingName,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetCoreDevice
    getCoreDevice_coreDeviceThingName,
    getCoreDeviceResponse_status,
    getCoreDeviceResponse_platform,
    getCoreDeviceResponse_architecture,
    getCoreDeviceResponse_coreDeviceThingName,
    getCoreDeviceResponse_tags,
    getCoreDeviceResponse_coreVersion,
    getCoreDeviceResponse_lastStatusUpdateTimestamp,
    getCoreDeviceResponse_httpStatus,

    -- ** GetComponentVersionArtifact
    getComponentVersionArtifact_arn,
    getComponentVersionArtifact_artifactName,
    getComponentVersionArtifactResponse_httpStatus,
    getComponentVersionArtifactResponse_preSignedUrl,

    -- ** CreateDeployment
    createDeployment_components,
    createDeployment_clientToken,
    createDeployment_deploymentPolicies,
    createDeployment_iotJobConfiguration,
    createDeployment_deploymentName,
    createDeployment_tags,
    createDeployment_targetArn,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_iotJobId,
    createDeploymentResponse_iotJobArn,
    createDeploymentResponse_httpStatus,

    -- ** BatchAssociateClientDeviceWithCoreDevice
    batchAssociateClientDeviceWithCoreDevice_entries,
    batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName,
    batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries,
    batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus,

    -- ** ListInstalledComponents
    listInstalledComponents_nextToken,
    listInstalledComponents_maxResults,
    listInstalledComponents_coreDeviceThingName,
    listInstalledComponentsResponse_installedComponents,
    listInstalledComponentsResponse_nextToken,
    listInstalledComponentsResponse_httpStatus,

    -- ** CancelDeployment
    cancelDeployment_deploymentId,
    cancelDeploymentResponse_message,
    cancelDeploymentResponse_httpStatus,

    -- ** BatchDisassociateClientDeviceFromCoreDevice
    batchDisassociateClientDeviceFromCoreDevice_entries,
    batchDisassociateClientDeviceFromCoreDevice_coreDeviceThingName,
    batchDisassociateClientDeviceFromCoreDeviceResponse_errorEntries,
    batchDisassociateClientDeviceFromCoreDeviceResponse_httpStatus,

    -- ** ListCoreDevices
    listCoreDevices_status,
    listCoreDevices_thingGroupArn,
    listCoreDevices_nextToken,
    listCoreDevices_maxResults,
    listCoreDevicesResponse_nextToken,
    listCoreDevicesResponse_coreDevices,
    listCoreDevicesResponse_httpStatus,

    -- ** ResolveComponentCandidates
    resolveComponentCandidates_platform,
    resolveComponentCandidates_componentCandidates,
    resolveComponentCandidatesResponse_resolvedComponentVersions,
    resolveComponentCandidatesResponse_httpStatus,

    -- ** ListEffectiveDeployments
    listEffectiveDeployments_nextToken,
    listEffectiveDeployments_maxResults,
    listEffectiveDeployments_coreDeviceThingName,
    listEffectiveDeploymentsResponse_nextToken,
    listEffectiveDeploymentsResponse_effectiveDeployments,
    listEffectiveDeploymentsResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_targetArn,
    listDeployments_nextToken,
    listDeployments_historyFilter,
    listDeployments_maxResults,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_scope,
    listComponents_maxResults,
    listComponentsResponse_components,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_arn,

    -- ** CreateComponentVersion
    createComponentVersion_clientToken,
    createComponentVersion_inlineRecipe,
    createComponentVersion_lambdaFunction,
    createComponentVersion_tags,
    createComponentVersionResponse_arn,
    createComponentVersionResponse_httpStatus,
    createComponentVersionResponse_componentName,
    createComponentVersionResponse_componentVersion,
    createComponentVersionResponse_creationTimestamp,
    createComponentVersionResponse_status,

    -- ** GetComponent
    getComponent_recipeOutputFormat,
    getComponent_arn,
    getComponentResponse_tags,
    getComponentResponse_httpStatus,
    getComponentResponse_recipeOutputFormat,
    getComponentResponse_recipe,

    -- ** ListClientDevicesAssociatedWithCoreDevice
    listClientDevicesAssociatedWithCoreDevice_nextToken,
    listClientDevicesAssociatedWithCoreDevice_maxResults,
    listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName,
    listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices,
    listClientDevicesAssociatedWithCoreDeviceResponse_nextToken,
    listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus,

    -- * Types

    -- ** AssociateClientDeviceWithCoreDeviceEntry
    associateClientDeviceWithCoreDeviceEntry_thingName,

    -- ** AssociateClientDeviceWithCoreDeviceErrorEntry
    associateClientDeviceWithCoreDeviceErrorEntry_code,
    associateClientDeviceWithCoreDeviceErrorEntry_message,
    associateClientDeviceWithCoreDeviceErrorEntry_thingName,

    -- ** AssociatedClientDevice
    associatedClientDevice_associationTimestamp,
    associatedClientDevice_thingName,

    -- ** CloudComponentStatus
    cloudComponentStatus_componentState,
    cloudComponentStatus_message,
    cloudComponentStatus_errors,

    -- ** Component
    component_arn,
    component_componentName,
    component_latestVersion,

    -- ** ComponentCandidate
    componentCandidate_componentVersion,
    componentCandidate_versionRequirements,
    componentCandidate_componentName,

    -- ** ComponentConfigurationUpdate
    componentConfigurationUpdate_reset,
    componentConfigurationUpdate_merge,

    -- ** ComponentDependencyRequirement
    componentDependencyRequirement_dependencyType,
    componentDependencyRequirement_versionRequirement,

    -- ** ComponentDeploymentSpecification
    componentDeploymentSpecification_componentVersion,
    componentDeploymentSpecification_runWith,
    componentDeploymentSpecification_configurationUpdate,

    -- ** ComponentLatestVersion
    componentLatestVersion_platforms,
    componentLatestVersion_arn,
    componentLatestVersion_componentVersion,
    componentLatestVersion_creationTimestamp,
    componentLatestVersion_publisher,
    componentLatestVersion_description,

    -- ** ComponentPlatform
    componentPlatform_name,
    componentPlatform_attributes,

    -- ** ComponentRunWith
    componentRunWith_posixUser,
    componentRunWith_systemResourceLimits,

    -- ** ComponentVersionListItem
    componentVersionListItem_arn,
    componentVersionListItem_componentVersion,
    componentVersionListItem_componentName,

    -- ** CoreDevice
    coreDevice_status,
    coreDevice_coreDeviceThingName,
    coreDevice_lastStatusUpdateTimestamp,

    -- ** Deployment
    deployment_targetArn,
    deployment_deploymentId,
    deployment_creationTimestamp,
    deployment_deploymentStatus,
    deployment_isLatestForTarget,
    deployment_revisionId,
    deployment_deploymentName,

    -- ** DeploymentComponentUpdatePolicy
    deploymentComponentUpdatePolicy_action,
    deploymentComponentUpdatePolicy_timeoutInSeconds,

    -- ** DeploymentConfigurationValidationPolicy
    deploymentConfigurationValidationPolicy_timeoutInSeconds,

    -- ** DeploymentIoTJobConfiguration
    deploymentIoTJobConfiguration_jobExecutionsRolloutConfig,
    deploymentIoTJobConfiguration_abortConfig,
    deploymentIoTJobConfiguration_timeoutConfig,

    -- ** DeploymentPolicies
    deploymentPolicies_failureHandlingPolicy,
    deploymentPolicies_configurationValidationPolicy,
    deploymentPolicies_componentUpdatePolicy,

    -- ** DisassociateClientDeviceFromCoreDeviceEntry
    disassociateClientDeviceFromCoreDeviceEntry_thingName,

    -- ** DisassociateClientDeviceFromCoreDeviceErrorEntry
    disassociateClientDeviceFromCoreDeviceErrorEntry_code,
    disassociateClientDeviceFromCoreDeviceErrorEntry_message,
    disassociateClientDeviceFromCoreDeviceErrorEntry_thingName,

    -- ** EffectiveDeployment
    effectiveDeployment_iotJobId,
    effectiveDeployment_iotJobArn,
    effectiveDeployment_reason,
    effectiveDeployment_description,
    effectiveDeployment_deploymentId,
    effectiveDeployment_deploymentName,
    effectiveDeployment_targetArn,
    effectiveDeployment_coreDeviceExecutionStatus,
    effectiveDeployment_creationTimestamp,
    effectiveDeployment_modifiedTimestamp,

    -- ** InstalledComponent
    installedComponent_isRoot,
    installedComponent_componentVersion,
    installedComponent_componentName,
    installedComponent_lifecycleStateDetails,
    installedComponent_lifecycleState,

    -- ** IoTJobAbortConfig
    ioTJobAbortConfig_criteriaList,

    -- ** IoTJobAbortCriteria
    ioTJobAbortCriteria_failureType,
    ioTJobAbortCriteria_action,
    ioTJobAbortCriteria_thresholdPercentage,
    ioTJobAbortCriteria_minNumberOfExecutedThings,

    -- ** IoTJobExecutionsRolloutConfig
    ioTJobExecutionsRolloutConfig_exponentialRate,
    ioTJobExecutionsRolloutConfig_maximumPerMinute,

    -- ** IoTJobExponentialRolloutRate
    ioTJobExponentialRolloutRate_baseRatePerMinute,
    ioTJobExponentialRolloutRate_incrementFactor,
    ioTJobExponentialRolloutRate_rateIncreaseCriteria,

    -- ** IoTJobRateIncreaseCriteria
    ioTJobRateIncreaseCriteria_numberOfNotifiedThings,
    ioTJobRateIncreaseCriteria_numberOfSucceededThings,

    -- ** IoTJobTimeoutConfig
    ioTJobTimeoutConfig_inProgressTimeoutInMinutes,

    -- ** LambdaContainerParams
    lambdaContainerParams_mountROSysfs,
    lambdaContainerParams_memorySizeInKB,
    lambdaContainerParams_devices,
    lambdaContainerParams_volumes,

    -- ** LambdaDeviceMount
    lambdaDeviceMount_addGroupOwner,
    lambdaDeviceMount_permission,
    lambdaDeviceMount_path,

    -- ** LambdaEventSource
    lambdaEventSource_topic,
    lambdaEventSource_type,

    -- ** LambdaExecutionParameters
    lambdaExecutionParameters_execArgs,
    lambdaExecutionParameters_maxQueueSize,
    lambdaExecutionParameters_pinned,
    lambdaExecutionParameters_inputPayloadEncodingType,
    lambdaExecutionParameters_maxIdleTimeInSeconds,
    lambdaExecutionParameters_timeoutInSeconds,
    lambdaExecutionParameters_eventSources,
    lambdaExecutionParameters_maxInstancesCount,
    lambdaExecutionParameters_environmentVariables,
    lambdaExecutionParameters_statusTimeoutInSeconds,
    lambdaExecutionParameters_linuxProcessParams,

    -- ** LambdaFunctionRecipeSource
    lambdaFunctionRecipeSource_componentLambdaParameters,
    lambdaFunctionRecipeSource_componentVersion,
    lambdaFunctionRecipeSource_componentName,
    lambdaFunctionRecipeSource_componentPlatforms,
    lambdaFunctionRecipeSource_componentDependencies,
    lambdaFunctionRecipeSource_lambdaArn,

    -- ** LambdaLinuxProcessParams
    lambdaLinuxProcessParams_containerParams,
    lambdaLinuxProcessParams_isolationMode,

    -- ** LambdaVolumeMount
    lambdaVolumeMount_addGroupOwner,
    lambdaVolumeMount_permission,
    lambdaVolumeMount_sourcePath,
    lambdaVolumeMount_destinationPath,

    -- ** ResolvedComponentVersion
    resolvedComponentVersion_arn,
    resolvedComponentVersion_componentVersion,
    resolvedComponentVersion_recipe,
    resolvedComponentVersion_componentName,

    -- ** SystemResourceLimits
    systemResourceLimits_memory,
    systemResourceLimits_cpus,
  )
where

import Network.AWS.GreengrassV2.BatchAssociateClientDeviceWithCoreDevice
import Network.AWS.GreengrassV2.BatchDisassociateClientDeviceFromCoreDevice
import Network.AWS.GreengrassV2.CancelDeployment
import Network.AWS.GreengrassV2.CreateComponentVersion
import Network.AWS.GreengrassV2.CreateDeployment
import Network.AWS.GreengrassV2.DeleteComponent
import Network.AWS.GreengrassV2.DeleteCoreDevice
import Network.AWS.GreengrassV2.DescribeComponent
import Network.AWS.GreengrassV2.GetComponent
import Network.AWS.GreengrassV2.GetComponentVersionArtifact
import Network.AWS.GreengrassV2.GetCoreDevice
import Network.AWS.GreengrassV2.GetDeployment
import Network.AWS.GreengrassV2.ListClientDevicesAssociatedWithCoreDevice
import Network.AWS.GreengrassV2.ListComponentVersions
import Network.AWS.GreengrassV2.ListComponents
import Network.AWS.GreengrassV2.ListCoreDevices
import Network.AWS.GreengrassV2.ListDeployments
import Network.AWS.GreengrassV2.ListEffectiveDeployments
import Network.AWS.GreengrassV2.ListInstalledComponents
import Network.AWS.GreengrassV2.ListTagsForResource
import Network.AWS.GreengrassV2.ResolveComponentCandidates
import Network.AWS.GreengrassV2.TagResource
import Network.AWS.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceEntry
import Network.AWS.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceErrorEntry
import Network.AWS.GreengrassV2.Types.AssociatedClientDevice
import Network.AWS.GreengrassV2.Types.CloudComponentStatus
import Network.AWS.GreengrassV2.Types.Component
import Network.AWS.GreengrassV2.Types.ComponentCandidate
import Network.AWS.GreengrassV2.Types.ComponentConfigurationUpdate
import Network.AWS.GreengrassV2.Types.ComponentDependencyRequirement
import Network.AWS.GreengrassV2.Types.ComponentDeploymentSpecification
import Network.AWS.GreengrassV2.Types.ComponentLatestVersion
import Network.AWS.GreengrassV2.Types.ComponentPlatform
import Network.AWS.GreengrassV2.Types.ComponentRunWith
import Network.AWS.GreengrassV2.Types.ComponentVersionListItem
import Network.AWS.GreengrassV2.Types.CoreDevice
import Network.AWS.GreengrassV2.Types.Deployment
import Network.AWS.GreengrassV2.Types.DeploymentComponentUpdatePolicy
import Network.AWS.GreengrassV2.Types.DeploymentConfigurationValidationPolicy
import Network.AWS.GreengrassV2.Types.DeploymentIoTJobConfiguration
import Network.AWS.GreengrassV2.Types.DeploymentPolicies
import Network.AWS.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceEntry
import Network.AWS.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceErrorEntry
import Network.AWS.GreengrassV2.Types.EffectiveDeployment
import Network.AWS.GreengrassV2.Types.InstalledComponent
import Network.AWS.GreengrassV2.Types.IoTJobAbortConfig
import Network.AWS.GreengrassV2.Types.IoTJobAbortCriteria
import Network.AWS.GreengrassV2.Types.IoTJobExecutionsRolloutConfig
import Network.AWS.GreengrassV2.Types.IoTJobExponentialRolloutRate
import Network.AWS.GreengrassV2.Types.IoTJobRateIncreaseCriteria
import Network.AWS.GreengrassV2.Types.IoTJobTimeoutConfig
import Network.AWS.GreengrassV2.Types.LambdaContainerParams
import Network.AWS.GreengrassV2.Types.LambdaDeviceMount
import Network.AWS.GreengrassV2.Types.LambdaEventSource
import Network.AWS.GreengrassV2.Types.LambdaExecutionParameters
import Network.AWS.GreengrassV2.Types.LambdaFunctionRecipeSource
import Network.AWS.GreengrassV2.Types.LambdaLinuxProcessParams
import Network.AWS.GreengrassV2.Types.LambdaVolumeMount
import Network.AWS.GreengrassV2.Types.ResolvedComponentVersion
import Network.AWS.GreengrassV2.Types.SystemResourceLimits
import Network.AWS.GreengrassV2.UntagResource
