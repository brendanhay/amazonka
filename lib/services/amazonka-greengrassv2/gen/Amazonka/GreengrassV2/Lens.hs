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
    createComponentVersion_inlineRecipe,
    createComponentVersion_tags,
    createComponentVersion_clientToken,
    createComponentVersion_lambdaFunction,
    createComponentVersionResponse_arn,
    createComponentVersionResponse_httpStatus,
    createComponentVersionResponse_componentName,
    createComponentVersionResponse_componentVersion,
    createComponentVersionResponse_creationTimestamp,
    createComponentVersionResponse_status,

    -- ** CreateDeployment
    createDeployment_tags,
    createDeployment_clientToken,
    createDeployment_iotJobConfiguration,
    createDeployment_deploymentName,
    createDeployment_parentTargetArn,
    createDeployment_deploymentPolicies,
    createDeployment_components,
    createDeployment_targetArn,
    createDeploymentResponse_iotJobArn,
    createDeploymentResponse_iotJobId,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_arn,

    -- ** DeleteCoreDevice
    deleteCoreDevice_coreDeviceThingName,

    -- ** DeleteDeployment
    deleteDeployment_deploymentId,

    -- ** DescribeComponent
    describeComponent_arn,
    describeComponentResponse_tags,
    describeComponentResponse_componentVersion,
    describeComponentResponse_componentName,
    describeComponentResponse_arn,
    describeComponentResponse_status,
    describeComponentResponse_description,
    describeComponentResponse_creationTimestamp,
    describeComponentResponse_platforms,
    describeComponentResponse_publisher,
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
    getConnectivityInfoResponse_message,
    getConnectivityInfoResponse_connectivityInfo,
    getConnectivityInfoResponse_httpStatus,

    -- ** GetCoreDevice
    getCoreDevice_coreDeviceThingName,
    getCoreDeviceResponse_tags,
    getCoreDeviceResponse_coreDeviceThingName,
    getCoreDeviceResponse_status,
    getCoreDeviceResponse_platform,
    getCoreDeviceResponse_coreVersion,
    getCoreDeviceResponse_lastStatusUpdateTimestamp,
    getCoreDeviceResponse_architecture,
    getCoreDeviceResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_tags,
    getDeploymentResponse_iotJobConfiguration,
    getDeploymentResponse_iotJobArn,
    getDeploymentResponse_iotJobId,
    getDeploymentResponse_deploymentName,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_targetArn,
    getDeploymentResponse_isLatestForTarget,
    getDeploymentResponse_creationTimestamp,
    getDeploymentResponse_parentTargetArn,
    getDeploymentResponse_deploymentPolicies,
    getDeploymentResponse_components,
    getDeploymentResponse_revisionId,
    getDeploymentResponse_httpStatus,

    -- ** GetServiceRoleForAccount
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_httpStatus,

    -- ** ListClientDevicesAssociatedWithCoreDevice
    listClientDevicesAssociatedWithCoreDevice_nextToken,
    listClientDevicesAssociatedWithCoreDevice_maxResults,
    listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName,
    listClientDevicesAssociatedWithCoreDeviceResponse_nextToken,
    listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices,
    listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus,

    -- ** ListComponentVersions
    listComponentVersions_nextToken,
    listComponentVersions_maxResults,
    listComponentVersions_arn,
    listComponentVersionsResponse_nextToken,
    listComponentVersionsResponse_componentVersions,
    listComponentVersionsResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_maxResults,
    listComponents_scope,
    listComponentsResponse_nextToken,
    listComponentsResponse_components,
    listComponentsResponse_httpStatus,

    -- ** ListCoreDevices
    listCoreDevices_nextToken,
    listCoreDevices_status,
    listCoreDevices_maxResults,
    listCoreDevices_thingGroupArn,
    listCoreDevicesResponse_nextToken,
    listCoreDevicesResponse_coreDevices,
    listCoreDevicesResponse_httpStatus,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_targetArn,
    listDeployments_parentTargetArn,
    listDeployments_maxResults,
    listDeployments_historyFilter,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,

    -- ** ListEffectiveDeployments
    listEffectiveDeployments_nextToken,
    listEffectiveDeployments_maxResults,
    listEffectiveDeployments_coreDeviceThingName,
    listEffectiveDeploymentsResponse_effectiveDeployments,
    listEffectiveDeploymentsResponse_nextToken,
    listEffectiveDeploymentsResponse_httpStatus,

    -- ** ListInstalledComponents
    listInstalledComponents_nextToken,
    listInstalledComponents_topologyFilter,
    listInstalledComponents_maxResults,
    listInstalledComponents_coreDeviceThingName,
    listInstalledComponentsResponse_nextToken,
    listInstalledComponentsResponse_installedComponents,
    listInstalledComponentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ResolveComponentCandidates
    resolveComponentCandidates_platform,
    resolveComponentCandidates_componentCandidates,
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
    associateClientDeviceWithCoreDeviceErrorEntry_message,
    associateClientDeviceWithCoreDeviceErrorEntry_thingName,
    associateClientDeviceWithCoreDeviceErrorEntry_code,

    -- ** AssociatedClientDevice
    associatedClientDevice_thingName,
    associatedClientDevice_associationTimestamp,

    -- ** CloudComponentStatus
    cloudComponentStatus_message,
    cloudComponentStatus_vendorGuidance,
    cloudComponentStatus_vendorGuidanceMessage,
    cloudComponentStatus_errors,
    cloudComponentStatus_componentState,

    -- ** Component
    component_componentName,
    component_arn,
    component_latestVersion,

    -- ** ComponentCandidate
    componentCandidate_componentVersion,
    componentCandidate_componentName,
    componentCandidate_versionRequirements,

    -- ** ComponentConfigurationUpdate
    componentConfigurationUpdate_merge,
    componentConfigurationUpdate_reset,

    -- ** ComponentDependencyRequirement
    componentDependencyRequirement_versionRequirement,
    componentDependencyRequirement_dependencyType,

    -- ** ComponentDeploymentSpecification
    componentDeploymentSpecification_componentVersion,
    componentDeploymentSpecification_runWith,
    componentDeploymentSpecification_configurationUpdate,

    -- ** ComponentLatestVersion
    componentLatestVersion_componentVersion,
    componentLatestVersion_arn,
    componentLatestVersion_description,
    componentLatestVersion_creationTimestamp,
    componentLatestVersion_platforms,
    componentLatestVersion_publisher,

    -- ** ComponentPlatform
    componentPlatform_name,
    componentPlatform_attributes,

    -- ** ComponentRunWith
    componentRunWith_systemResourceLimits,
    componentRunWith_posixUser,
    componentRunWith_windowsUser,

    -- ** ComponentVersionListItem
    componentVersionListItem_componentVersion,
    componentVersionListItem_componentName,
    componentVersionListItem_arn,

    -- ** ConnectivityInfo
    connectivityInfo_portNumber,
    connectivityInfo_metadata,
    connectivityInfo_id,
    connectivityInfo_hostAddress,

    -- ** CoreDevice
    coreDevice_coreDeviceThingName,
    coreDevice_status,
    coreDevice_lastStatusUpdateTimestamp,

    -- ** Deployment
    deployment_deploymentStatus,
    deployment_deploymentName,
    deployment_deploymentId,
    deployment_targetArn,
    deployment_isLatestForTarget,
    deployment_creationTimestamp,
    deployment_parentTargetArn,
    deployment_revisionId,

    -- ** DeploymentComponentUpdatePolicy
    deploymentComponentUpdatePolicy_timeoutInSeconds,
    deploymentComponentUpdatePolicy_action,

    -- ** DeploymentConfigurationValidationPolicy
    deploymentConfigurationValidationPolicy_timeoutInSeconds,

    -- ** DeploymentIoTJobConfiguration
    deploymentIoTJobConfiguration_jobExecutionsRolloutConfig,
    deploymentIoTJobConfiguration_abortConfig,
    deploymentIoTJobConfiguration_timeoutConfig,

    -- ** DeploymentPolicies
    deploymentPolicies_componentUpdatePolicy,
    deploymentPolicies_configurationValidationPolicy,
    deploymentPolicies_failureHandlingPolicy,

    -- ** DisassociateClientDeviceFromCoreDeviceEntry
    disassociateClientDeviceFromCoreDeviceEntry_thingName,

    -- ** DisassociateClientDeviceFromCoreDeviceErrorEntry
    disassociateClientDeviceFromCoreDeviceErrorEntry_message,
    disassociateClientDeviceFromCoreDeviceErrorEntry_thingName,
    disassociateClientDeviceFromCoreDeviceErrorEntry_code,

    -- ** EffectiveDeployment
    effectiveDeployment_iotJobArn,
    effectiveDeployment_iotJobId,
    effectiveDeployment_statusDetails,
    effectiveDeployment_description,
    effectiveDeployment_reason,
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
    installedComponent_componentVersion,
    installedComponent_componentName,
    installedComponent_lastReportedTimestamp,
    installedComponent_lifecycleStatusCodes,
    installedComponent_lifecycleState,
    installedComponent_lifecycleStateDetails,
    installedComponent_lastInstallationSource,
    installedComponent_lastStatusChangeTimestamp,
    installedComponent_isRoot,

    -- ** IoTJobAbortConfig
    ioTJobAbortConfig_criteriaList,

    -- ** IoTJobAbortCriteria
    ioTJobAbortCriteria_failureType,
    ioTJobAbortCriteria_action,
    ioTJobAbortCriteria_thresholdPercentage,
    ioTJobAbortCriteria_minNumberOfExecutedThings,

    -- ** IoTJobExecutionsRolloutConfig
    ioTJobExecutionsRolloutConfig_maximumPerMinute,
    ioTJobExecutionsRolloutConfig_exponentialRate,

    -- ** IoTJobExponentialRolloutRate
    ioTJobExponentialRolloutRate_baseRatePerMinute,
    ioTJobExponentialRolloutRate_incrementFactor,
    ioTJobExponentialRolloutRate_rateIncreaseCriteria,

    -- ** IoTJobRateIncreaseCriteria
    ioTJobRateIncreaseCriteria_numberOfSucceededThings,
    ioTJobRateIncreaseCriteria_numberOfNotifiedThings,

    -- ** IoTJobTimeoutConfig
    ioTJobTimeoutConfig_inProgressTimeoutInMinutes,

    -- ** LambdaContainerParams
    lambdaContainerParams_devices,
    lambdaContainerParams_memorySizeInKB,
    lambdaContainerParams_volumes,
    lambdaContainerParams_mountROSysfs,

    -- ** LambdaDeviceMount
    lambdaDeviceMount_addGroupOwner,
    lambdaDeviceMount_permission,
    lambdaDeviceMount_path,

    -- ** LambdaEventSource
    lambdaEventSource_topic,
    lambdaEventSource_type,

    -- ** LambdaExecutionParameters
    lambdaExecutionParameters_statusTimeoutInSeconds,
    lambdaExecutionParameters_linuxProcessParams,
    lambdaExecutionParameters_execArgs,
    lambdaExecutionParameters_maxIdleTimeInSeconds,
    lambdaExecutionParameters_eventSources,
    lambdaExecutionParameters_timeoutInSeconds,
    lambdaExecutionParameters_inputPayloadEncodingType,
    lambdaExecutionParameters_environmentVariables,
    lambdaExecutionParameters_maxQueueSize,
    lambdaExecutionParameters_pinned,
    lambdaExecutionParameters_maxInstancesCount,

    -- ** LambdaFunctionRecipeSource
    lambdaFunctionRecipeSource_componentPlatforms,
    lambdaFunctionRecipeSource_componentDependencies,
    lambdaFunctionRecipeSource_componentLambdaParameters,
    lambdaFunctionRecipeSource_componentVersion,
    lambdaFunctionRecipeSource_componentName,
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
    resolvedComponentVersion_message,
    resolvedComponentVersion_vendorGuidance,
    resolvedComponentVersion_componentVersion,
    resolvedComponentVersion_recipe,
    resolvedComponentVersion_componentName,
    resolvedComponentVersion_arn,

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
