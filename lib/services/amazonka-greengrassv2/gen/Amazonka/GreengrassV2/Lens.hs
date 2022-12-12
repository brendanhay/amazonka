{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GreengrassV2.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Lens
  ( -- * Operations

    -- ** AssociateServiceRoleToAccount
    associateServiceRoleToAccount_roleArn,
    associateServiceRoleToAccountResponse_associatedAt,
    associateServiceRoleToAccountResponse_httpStatus,

    -- ** BatchAssociateClientDeviceWithCoreDevice
    batchAssociateClientDeviceWithCoreDevice_entries,
    batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName,
    batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries,
    batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus,

    -- ** BatchDisassociateClientDeviceFromCoreDevice
    batchDisassociateClientDeviceFromCoreDevice_entries,
    batchDisassociateClientDeviceFromCoreDevice_coreDeviceThingName,
    batchDisassociateClientDeviceFromCoreDeviceResponse_errorEntries,
    batchDisassociateClientDeviceFromCoreDeviceResponse_httpStatus,

    -- ** CancelDeployment
    cancelDeployment_deploymentId,
    cancelDeploymentResponse_message,
    cancelDeploymentResponse_httpStatus,

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

    -- ** CreateDeployment
    createDeployment_clientToken,
    createDeployment_components,
    createDeployment_deploymentName,
    createDeployment_deploymentPolicies,
    createDeployment_iotJobConfiguration,
    createDeployment_parentTargetArn,
    createDeployment_tags,
    createDeployment_targetArn,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_iotJobArn,
    createDeploymentResponse_iotJobId,
    createDeploymentResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_arn,

    -- ** DeleteCoreDevice
    deleteCoreDevice_coreDeviceThingName,

    -- ** DeleteDeployment
    deleteDeployment_deploymentId,

    -- ** DescribeComponent
    describeComponent_arn,
    describeComponentResponse_arn,
    describeComponentResponse_componentName,
    describeComponentResponse_componentVersion,
    describeComponentResponse_creationTimestamp,
    describeComponentResponse_description,
    describeComponentResponse_platforms,
    describeComponentResponse_publisher,
    describeComponentResponse_status,
    describeComponentResponse_tags,
    describeComponentResponse_httpStatus,

    -- ** DisassociateServiceRoleFromAccount
    disassociateServiceRoleFromAccountResponse_disassociatedAt,
    disassociateServiceRoleFromAccountResponse_httpStatus,

    -- ** GetComponent
    getComponent_recipeOutputFormat,
    getComponent_arn,
    getComponentResponse_tags,
    getComponentResponse_httpStatus,
    getComponentResponse_recipeOutputFormat,
    getComponentResponse_recipe,

    -- ** GetComponentVersionArtifact
    getComponentVersionArtifact_arn,
    getComponentVersionArtifact_artifactName,
    getComponentVersionArtifactResponse_httpStatus,
    getComponentVersionArtifactResponse_preSignedUrl,

    -- ** GetConnectivityInfo
    getConnectivityInfo_thingName,
    getConnectivityInfoResponse_connectivityInfo,
    getConnectivityInfoResponse_message,
    getConnectivityInfoResponse_httpStatus,

    -- ** GetCoreDevice
    getCoreDevice_coreDeviceThingName,
    getCoreDeviceResponse_architecture,
    getCoreDeviceResponse_coreDeviceThingName,
    getCoreDeviceResponse_coreVersion,
    getCoreDeviceResponse_lastStatusUpdateTimestamp,
    getCoreDeviceResponse_platform,
    getCoreDeviceResponse_status,
    getCoreDeviceResponse_tags,
    getCoreDeviceResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_components,
    getDeploymentResponse_creationTimestamp,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_deploymentName,
    getDeploymentResponse_deploymentPolicies,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_iotJobArn,
    getDeploymentResponse_iotJobConfiguration,
    getDeploymentResponse_iotJobId,
    getDeploymentResponse_isLatestForTarget,
    getDeploymentResponse_parentTargetArn,
    getDeploymentResponse_revisionId,
    getDeploymentResponse_tags,
    getDeploymentResponse_targetArn,
    getDeploymentResponse_httpStatus,

    -- ** GetServiceRoleForAccount
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_httpStatus,

    -- ** ListClientDevicesAssociatedWithCoreDevice
    listClientDevicesAssociatedWithCoreDevice_maxResults,
    listClientDevicesAssociatedWithCoreDevice_nextToken,
    listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName,
    listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices,
    listClientDevicesAssociatedWithCoreDeviceResponse_nextToken,
    listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus,

    -- ** ListComponentVersions
    listComponentVersions_maxResults,
    listComponentVersions_nextToken,
    listComponentVersions_arn,
    listComponentVersionsResponse_componentVersions,
    listComponentVersionsResponse_nextToken,
    listComponentVersionsResponse_httpStatus,

    -- ** ListComponents
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_scope,
    listComponentsResponse_components,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,

    -- ** ListCoreDevices
    listCoreDevices_maxResults,
    listCoreDevices_nextToken,
    listCoreDevices_status,
    listCoreDevices_thingGroupArn,
    listCoreDevicesResponse_coreDevices,
    listCoreDevicesResponse_nextToken,
    listCoreDevicesResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_historyFilter,
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_parentTargetArn,
    listDeployments_targetArn,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,

    -- ** ListEffectiveDeployments
    listEffectiveDeployments_maxResults,
    listEffectiveDeployments_nextToken,
    listEffectiveDeployments_coreDeviceThingName,
    listEffectiveDeploymentsResponse_effectiveDeployments,
    listEffectiveDeploymentsResponse_nextToken,
    listEffectiveDeploymentsResponse_httpStatus,

    -- ** ListInstalledComponents
    listInstalledComponents_maxResults,
    listInstalledComponents_nextToken,
    listInstalledComponents_topologyFilter,
    listInstalledComponents_coreDeviceThingName,
    listInstalledComponentsResponse_installedComponents,
    listInstalledComponentsResponse_nextToken,
    listInstalledComponentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ResolveComponentCandidates
    resolveComponentCandidates_componentCandidates,
    resolveComponentCandidates_platform,
    resolveComponentCandidatesResponse_resolvedComponentVersions,
    resolveComponentCandidatesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConnectivityInfo
    updateConnectivityInfo_thingName,
    updateConnectivityInfo_connectivityInfo,
    updateConnectivityInfoResponse_message,
    updateConnectivityInfoResponse_version,
    updateConnectivityInfoResponse_httpStatus,

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
    cloudComponentStatus_errors,
    cloudComponentStatus_message,
    cloudComponentStatus_vendorGuidance,
    cloudComponentStatus_vendorGuidanceMessage,

    -- ** Component
    component_arn,
    component_componentName,
    component_latestVersion,

    -- ** ComponentCandidate
    componentCandidate_componentName,
    componentCandidate_componentVersion,
    componentCandidate_versionRequirements,

    -- ** ComponentConfigurationUpdate
    componentConfigurationUpdate_merge,
    componentConfigurationUpdate_reset,

    -- ** ComponentDependencyRequirement
    componentDependencyRequirement_dependencyType,
    componentDependencyRequirement_versionRequirement,

    -- ** ComponentDeploymentSpecification
    componentDeploymentSpecification_componentVersion,
    componentDeploymentSpecification_configurationUpdate,
    componentDeploymentSpecification_runWith,

    -- ** ComponentLatestVersion
    componentLatestVersion_arn,
    componentLatestVersion_componentVersion,
    componentLatestVersion_creationTimestamp,
    componentLatestVersion_description,
    componentLatestVersion_platforms,
    componentLatestVersion_publisher,

    -- ** ComponentPlatform
    componentPlatform_attributes,
    componentPlatform_name,

    -- ** ComponentRunWith
    componentRunWith_posixUser,
    componentRunWith_systemResourceLimits,
    componentRunWith_windowsUser,

    -- ** ComponentVersionListItem
    componentVersionListItem_arn,
    componentVersionListItem_componentName,
    componentVersionListItem_componentVersion,

    -- ** ConnectivityInfo
    connectivityInfo_hostAddress,
    connectivityInfo_id,
    connectivityInfo_metadata,
    connectivityInfo_portNumber,

    -- ** CoreDevice
    coreDevice_coreDeviceThingName,
    coreDevice_lastStatusUpdateTimestamp,
    coreDevice_status,

    -- ** Deployment
    deployment_creationTimestamp,
    deployment_deploymentId,
    deployment_deploymentName,
    deployment_deploymentStatus,
    deployment_isLatestForTarget,
    deployment_parentTargetArn,
    deployment_revisionId,
    deployment_targetArn,

    -- ** DeploymentComponentUpdatePolicy
    deploymentComponentUpdatePolicy_action,
    deploymentComponentUpdatePolicy_timeoutInSeconds,

    -- ** DeploymentConfigurationValidationPolicy
    deploymentConfigurationValidationPolicy_timeoutInSeconds,

    -- ** DeploymentIoTJobConfiguration
    deploymentIoTJobConfiguration_abortConfig,
    deploymentIoTJobConfiguration_jobExecutionsRolloutConfig,
    deploymentIoTJobConfiguration_timeoutConfig,

    -- ** DeploymentPolicies
    deploymentPolicies_componentUpdatePolicy,
    deploymentPolicies_configurationValidationPolicy,
    deploymentPolicies_failureHandlingPolicy,

    -- ** DisassociateClientDeviceFromCoreDeviceEntry
    disassociateClientDeviceFromCoreDeviceEntry_thingName,

    -- ** DisassociateClientDeviceFromCoreDeviceErrorEntry
    disassociateClientDeviceFromCoreDeviceErrorEntry_code,
    disassociateClientDeviceFromCoreDeviceErrorEntry_message,
    disassociateClientDeviceFromCoreDeviceErrorEntry_thingName,

    -- ** EffectiveDeployment
    effectiveDeployment_description,
    effectiveDeployment_iotJobArn,
    effectiveDeployment_iotJobId,
    effectiveDeployment_reason,
    effectiveDeployment_statusDetails,
    effectiveDeployment_deploymentId,
    effectiveDeployment_deploymentName,
    effectiveDeployment_targetArn,
    effectiveDeployment_coreDeviceExecutionStatus,
    effectiveDeployment_creationTimestamp,
    effectiveDeployment_modifiedTimestamp,

    -- ** EffectiveDeploymentStatusDetails
    effectiveDeploymentStatusDetails_errorStack,
    effectiveDeploymentStatusDetails_errorTypes,

    -- ** InstalledComponent
    installedComponent_componentName,
    installedComponent_componentVersion,
    installedComponent_isRoot,
    installedComponent_lastInstallationSource,
    installedComponent_lastReportedTimestamp,
    installedComponent_lastStatusChangeTimestamp,
    installedComponent_lifecycleState,
    installedComponent_lifecycleStateDetails,
    installedComponent_lifecycleStatusCodes,

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
    lambdaContainerParams_devices,
    lambdaContainerParams_memorySizeInKB,
    lambdaContainerParams_mountROSysfs,
    lambdaContainerParams_volumes,

    -- ** LambdaDeviceMount
    lambdaDeviceMount_addGroupOwner,
    lambdaDeviceMount_permission,
    lambdaDeviceMount_path,

    -- ** LambdaEventSource
    lambdaEventSource_topic,
    lambdaEventSource_type,

    -- ** LambdaExecutionParameters
    lambdaExecutionParameters_environmentVariables,
    lambdaExecutionParameters_eventSources,
    lambdaExecutionParameters_execArgs,
    lambdaExecutionParameters_inputPayloadEncodingType,
    lambdaExecutionParameters_linuxProcessParams,
    lambdaExecutionParameters_maxIdleTimeInSeconds,
    lambdaExecutionParameters_maxInstancesCount,
    lambdaExecutionParameters_maxQueueSize,
    lambdaExecutionParameters_pinned,
    lambdaExecutionParameters_statusTimeoutInSeconds,
    lambdaExecutionParameters_timeoutInSeconds,

    -- ** LambdaFunctionRecipeSource
    lambdaFunctionRecipeSource_componentDependencies,
    lambdaFunctionRecipeSource_componentLambdaParameters,
    lambdaFunctionRecipeSource_componentName,
    lambdaFunctionRecipeSource_componentPlatforms,
    lambdaFunctionRecipeSource_componentVersion,
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
    resolvedComponentVersion_componentName,
    resolvedComponentVersion_componentVersion,
    resolvedComponentVersion_message,
    resolvedComponentVersion_recipe,
    resolvedComponentVersion_vendorGuidance,

    -- ** SystemResourceLimits
    systemResourceLimits_cpus,
    systemResourceLimits_memory,
  )
where

import Amazonka.GreengrassV2.AssociateServiceRoleToAccount
import Amazonka.GreengrassV2.BatchAssociateClientDeviceWithCoreDevice
import Amazonka.GreengrassV2.BatchDisassociateClientDeviceFromCoreDevice
import Amazonka.GreengrassV2.CancelDeployment
import Amazonka.GreengrassV2.CreateComponentVersion
import Amazonka.GreengrassV2.CreateDeployment
import Amazonka.GreengrassV2.DeleteComponent
import Amazonka.GreengrassV2.DeleteCoreDevice
import Amazonka.GreengrassV2.DeleteDeployment
import Amazonka.GreengrassV2.DescribeComponent
import Amazonka.GreengrassV2.DisassociateServiceRoleFromAccount
import Amazonka.GreengrassV2.GetComponent
import Amazonka.GreengrassV2.GetComponentVersionArtifact
import Amazonka.GreengrassV2.GetConnectivityInfo
import Amazonka.GreengrassV2.GetCoreDevice
import Amazonka.GreengrassV2.GetDeployment
import Amazonka.GreengrassV2.GetServiceRoleForAccount
import Amazonka.GreengrassV2.ListClientDevicesAssociatedWithCoreDevice
import Amazonka.GreengrassV2.ListComponentVersions
import Amazonka.GreengrassV2.ListComponents
import Amazonka.GreengrassV2.ListCoreDevices
import Amazonka.GreengrassV2.ListDeployments
import Amazonka.GreengrassV2.ListEffectiveDeployments
import Amazonka.GreengrassV2.ListInstalledComponents
import Amazonka.GreengrassV2.ListTagsForResource
import Amazonka.GreengrassV2.ResolveComponentCandidates
import Amazonka.GreengrassV2.TagResource
import Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceEntry
import Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceErrorEntry
import Amazonka.GreengrassV2.Types.AssociatedClientDevice
import Amazonka.GreengrassV2.Types.CloudComponentStatus
import Amazonka.GreengrassV2.Types.Component
import Amazonka.GreengrassV2.Types.ComponentCandidate
import Amazonka.GreengrassV2.Types.ComponentConfigurationUpdate
import Amazonka.GreengrassV2.Types.ComponentDependencyRequirement
import Amazonka.GreengrassV2.Types.ComponentDeploymentSpecification
import Amazonka.GreengrassV2.Types.ComponentLatestVersion
import Amazonka.GreengrassV2.Types.ComponentPlatform
import Amazonka.GreengrassV2.Types.ComponentRunWith
import Amazonka.GreengrassV2.Types.ComponentVersionListItem
import Amazonka.GreengrassV2.Types.ConnectivityInfo
import Amazonka.GreengrassV2.Types.CoreDevice
import Amazonka.GreengrassV2.Types.Deployment
import Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicy
import Amazonka.GreengrassV2.Types.DeploymentConfigurationValidationPolicy
import Amazonka.GreengrassV2.Types.DeploymentIoTJobConfiguration
import Amazonka.GreengrassV2.Types.DeploymentPolicies
import Amazonka.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceEntry
import Amazonka.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceErrorEntry
import Amazonka.GreengrassV2.Types.EffectiveDeployment
import Amazonka.GreengrassV2.Types.EffectiveDeploymentStatusDetails
import Amazonka.GreengrassV2.Types.InstalledComponent
import Amazonka.GreengrassV2.Types.IoTJobAbortConfig
import Amazonka.GreengrassV2.Types.IoTJobAbortCriteria
import Amazonka.GreengrassV2.Types.IoTJobExecutionsRolloutConfig
import Amazonka.GreengrassV2.Types.IoTJobExponentialRolloutRate
import Amazonka.GreengrassV2.Types.IoTJobRateIncreaseCriteria
import Amazonka.GreengrassV2.Types.IoTJobTimeoutConfig
import Amazonka.GreengrassV2.Types.LambdaContainerParams
import Amazonka.GreengrassV2.Types.LambdaDeviceMount
import Amazonka.GreengrassV2.Types.LambdaEventSource
import Amazonka.GreengrassV2.Types.LambdaExecutionParameters
import Amazonka.GreengrassV2.Types.LambdaFunctionRecipeSource
import Amazonka.GreengrassV2.Types.LambdaLinuxProcessParams
import Amazonka.GreengrassV2.Types.LambdaVolumeMount
import Amazonka.GreengrassV2.Types.ResolvedComponentVersion
import Amazonka.GreengrassV2.Types.SystemResourceLimits
import Amazonka.GreengrassV2.UntagResource
import Amazonka.GreengrassV2.UpdateConnectivityInfo
