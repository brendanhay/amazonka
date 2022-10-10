{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Lens
  ( -- * Operations

    -- ** CancelImageCreation
    cancelImageCreation_imageBuildVersionArn,
    cancelImageCreation_clientToken,
    cancelImageCreationResponse_clientToken,
    cancelImageCreationResponse_requestId,
    cancelImageCreationResponse_imageBuildVersionArn,
    cancelImageCreationResponse_httpStatus,

    -- ** CreateComponent
    createComponent_tags,
    createComponent_changeDescription,
    createComponent_description,
    createComponent_uri,
    createComponent_kmsKeyId,
    createComponent_data,
    createComponent_supportedOsVersions,
    createComponent_name,
    createComponent_semanticVersion,
    createComponent_platform,
    createComponent_clientToken,
    createComponentResponse_clientToken,
    createComponentResponse_requestId,
    createComponentResponse_componentBuildVersionArn,
    createComponentResponse_httpStatus,

    -- ** CreateContainerRecipe
    createContainerRecipe_tags,
    createContainerRecipe_instanceConfiguration,
    createContainerRecipe_description,
    createContainerRecipe_platformOverride,
    createContainerRecipe_kmsKeyId,
    createContainerRecipe_imageOsVersionOverride,
    createContainerRecipe_dockerfileTemplateData,
    createContainerRecipe_dockerfileTemplateUri,
    createContainerRecipe_workingDirectory,
    createContainerRecipe_containerType,
    createContainerRecipe_name,
    createContainerRecipe_semanticVersion,
    createContainerRecipe_components,
    createContainerRecipe_parentImage,
    createContainerRecipe_targetRepository,
    createContainerRecipe_clientToken,
    createContainerRecipeResponse_clientToken,
    createContainerRecipeResponse_requestId,
    createContainerRecipeResponse_containerRecipeArn,
    createContainerRecipeResponse_httpStatus,

    -- ** CreateDistributionConfiguration
    createDistributionConfiguration_tags,
    createDistributionConfiguration_description,
    createDistributionConfiguration_name,
    createDistributionConfiguration_distributions,
    createDistributionConfiguration_clientToken,
    createDistributionConfigurationResponse_clientToken,
    createDistributionConfigurationResponse_requestId,
    createDistributionConfigurationResponse_distributionConfigurationArn,
    createDistributionConfigurationResponse_httpStatus,

    -- ** CreateImage
    createImage_enhancedImageMetadataEnabled,
    createImage_tags,
    createImage_imageTestsConfiguration,
    createImage_imageRecipeArn,
    createImage_containerRecipeArn,
    createImage_distributionConfigurationArn,
    createImage_infrastructureConfigurationArn,
    createImage_clientToken,
    createImageResponse_clientToken,
    createImageResponse_requestId,
    createImageResponse_imageBuildVersionArn,
    createImageResponse_httpStatus,

    -- ** CreateImagePipeline
    createImagePipeline_enhancedImageMetadataEnabled,
    createImagePipeline_schedule,
    createImagePipeline_tags,
    createImagePipeline_imageTestsConfiguration,
    createImagePipeline_imageRecipeArn,
    createImagePipeline_status,
    createImagePipeline_description,
    createImagePipeline_containerRecipeArn,
    createImagePipeline_distributionConfigurationArn,
    createImagePipeline_name,
    createImagePipeline_infrastructureConfigurationArn,
    createImagePipeline_clientToken,
    createImagePipelineResponse_clientToken,
    createImagePipelineResponse_requestId,
    createImagePipelineResponse_imagePipelineArn,
    createImagePipelineResponse_httpStatus,

    -- ** CreateImageRecipe
    createImageRecipe_tags,
    createImageRecipe_blockDeviceMappings,
    createImageRecipe_description,
    createImageRecipe_additionalInstanceConfiguration,
    createImageRecipe_workingDirectory,
    createImageRecipe_name,
    createImageRecipe_semanticVersion,
    createImageRecipe_components,
    createImageRecipe_parentImage,
    createImageRecipe_clientToken,
    createImageRecipeResponse_clientToken,
    createImageRecipeResponse_requestId,
    createImageRecipeResponse_imageRecipeArn,
    createImageRecipeResponse_httpStatus,

    -- ** CreateInfrastructureConfiguration
    createInfrastructureConfiguration_tags,
    createInfrastructureConfiguration_instanceTypes,
    createInfrastructureConfiguration_securityGroupIds,
    createInfrastructureConfiguration_subnetId,
    createInfrastructureConfiguration_description,
    createInfrastructureConfiguration_resourceTags,
    createInfrastructureConfiguration_keyPair,
    createInfrastructureConfiguration_logging,
    createInfrastructureConfiguration_snsTopicArn,
    createInfrastructureConfiguration_instanceMetadataOptions,
    createInfrastructureConfiguration_terminateInstanceOnFailure,
    createInfrastructureConfiguration_name,
    createInfrastructureConfiguration_instanceProfileName,
    createInfrastructureConfiguration_clientToken,
    createInfrastructureConfigurationResponse_clientToken,
    createInfrastructureConfigurationResponse_requestId,
    createInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    createInfrastructureConfigurationResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_componentBuildVersionArn,
    deleteComponentResponse_requestId,
    deleteComponentResponse_componentBuildVersionArn,
    deleteComponentResponse_httpStatus,

    -- ** DeleteContainerRecipe
    deleteContainerRecipe_containerRecipeArn,
    deleteContainerRecipeResponse_requestId,
    deleteContainerRecipeResponse_containerRecipeArn,
    deleteContainerRecipeResponse_httpStatus,

    -- ** DeleteDistributionConfiguration
    deleteDistributionConfiguration_distributionConfigurationArn,
    deleteDistributionConfigurationResponse_requestId,
    deleteDistributionConfigurationResponse_distributionConfigurationArn,
    deleteDistributionConfigurationResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_imageBuildVersionArn,
    deleteImageResponse_requestId,
    deleteImageResponse_imageBuildVersionArn,
    deleteImageResponse_httpStatus,

    -- ** DeleteImagePipeline
    deleteImagePipeline_imagePipelineArn,
    deleteImagePipelineResponse_requestId,
    deleteImagePipelineResponse_imagePipelineArn,
    deleteImagePipelineResponse_httpStatus,

    -- ** DeleteImageRecipe
    deleteImageRecipe_imageRecipeArn,
    deleteImageRecipeResponse_requestId,
    deleteImageRecipeResponse_imageRecipeArn,
    deleteImageRecipeResponse_httpStatus,

    -- ** DeleteInfrastructureConfiguration
    deleteInfrastructureConfiguration_infrastructureConfigurationArn,
    deleteInfrastructureConfigurationResponse_requestId,
    deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    deleteInfrastructureConfigurationResponse_httpStatus,

    -- ** GetComponent
    getComponent_componentBuildVersionArn,
    getComponentResponse_requestId,
    getComponentResponse_component,
    getComponentResponse_httpStatus,

    -- ** GetComponentPolicy
    getComponentPolicy_componentArn,
    getComponentPolicyResponse_policy,
    getComponentPolicyResponse_requestId,
    getComponentPolicyResponse_httpStatus,

    -- ** GetContainerRecipe
    getContainerRecipe_containerRecipeArn,
    getContainerRecipeResponse_requestId,
    getContainerRecipeResponse_containerRecipe,
    getContainerRecipeResponse_httpStatus,

    -- ** GetContainerRecipePolicy
    getContainerRecipePolicy_containerRecipeArn,
    getContainerRecipePolicyResponse_policy,
    getContainerRecipePolicyResponse_requestId,
    getContainerRecipePolicyResponse_httpStatus,

    -- ** GetDistributionConfiguration
    getDistributionConfiguration_distributionConfigurationArn,
    getDistributionConfigurationResponse_requestId,
    getDistributionConfigurationResponse_distributionConfiguration,
    getDistributionConfigurationResponse_httpStatus,

    -- ** GetImage
    getImage_imageBuildVersionArn,
    getImageResponse_requestId,
    getImageResponse_image,
    getImageResponse_httpStatus,

    -- ** GetImagePipeline
    getImagePipeline_imagePipelineArn,
    getImagePipelineResponse_requestId,
    getImagePipelineResponse_imagePipeline,
    getImagePipelineResponse_httpStatus,

    -- ** GetImagePolicy
    getImagePolicy_imageArn,
    getImagePolicyResponse_policy,
    getImagePolicyResponse_requestId,
    getImagePolicyResponse_httpStatus,

    -- ** GetImageRecipe
    getImageRecipe_imageRecipeArn,
    getImageRecipeResponse_requestId,
    getImageRecipeResponse_imageRecipe,
    getImageRecipeResponse_httpStatus,

    -- ** GetImageRecipePolicy
    getImageRecipePolicy_imageRecipeArn,
    getImageRecipePolicyResponse_policy,
    getImageRecipePolicyResponse_requestId,
    getImageRecipePolicyResponse_httpStatus,

    -- ** GetInfrastructureConfiguration
    getInfrastructureConfiguration_infrastructureConfigurationArn,
    getInfrastructureConfigurationResponse_requestId,
    getInfrastructureConfigurationResponse_infrastructureConfiguration,
    getInfrastructureConfigurationResponse_httpStatus,

    -- ** ImportComponent
    importComponent_tags,
    importComponent_changeDescription,
    importComponent_description,
    importComponent_uri,
    importComponent_kmsKeyId,
    importComponent_data,
    importComponent_name,
    importComponent_semanticVersion,
    importComponent_type,
    importComponent_format,
    importComponent_platform,
    importComponent_clientToken,
    importComponentResponse_clientToken,
    importComponentResponse_requestId,
    importComponentResponse_componentBuildVersionArn,
    importComponentResponse_httpStatus,

    -- ** ImportVmImage
    importVmImage_tags,
    importVmImage_osVersion,
    importVmImage_description,
    importVmImage_name,
    importVmImage_semanticVersion,
    importVmImage_platform,
    importVmImage_vmImportTaskId,
    importVmImage_clientToken,
    importVmImageResponse_clientToken,
    importVmImageResponse_requestId,
    importVmImageResponse_imageArn,
    importVmImageResponse_httpStatus,

    -- ** ListComponentBuildVersions
    listComponentBuildVersions_nextToken,
    listComponentBuildVersions_maxResults,
    listComponentBuildVersions_componentVersionArn,
    listComponentBuildVersionsResponse_componentSummaryList,
    listComponentBuildVersionsResponse_nextToken,
    listComponentBuildVersionsResponse_requestId,
    listComponentBuildVersionsResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_byName,
    listComponents_filters,
    listComponents_owner,
    listComponents_maxResults,
    listComponentsResponse_nextToken,
    listComponentsResponse_componentVersionList,
    listComponentsResponse_requestId,
    listComponentsResponse_httpStatus,

    -- ** ListContainerRecipes
    listContainerRecipes_nextToken,
    listContainerRecipes_filters,
    listContainerRecipes_owner,
    listContainerRecipes_maxResults,
    listContainerRecipesResponse_nextToken,
    listContainerRecipesResponse_requestId,
    listContainerRecipesResponse_containerRecipeSummaryList,
    listContainerRecipesResponse_httpStatus,

    -- ** ListDistributionConfigurations
    listDistributionConfigurations_nextToken,
    listDistributionConfigurations_filters,
    listDistributionConfigurations_maxResults,
    listDistributionConfigurationsResponse_nextToken,
    listDistributionConfigurationsResponse_distributionConfigurationSummaryList,
    listDistributionConfigurationsResponse_requestId,
    listDistributionConfigurationsResponse_httpStatus,

    -- ** ListImageBuildVersions
    listImageBuildVersions_nextToken,
    listImageBuildVersions_filters,
    listImageBuildVersions_maxResults,
    listImageBuildVersions_imageVersionArn,
    listImageBuildVersionsResponse_nextToken,
    listImageBuildVersionsResponse_requestId,
    listImageBuildVersionsResponse_imageSummaryList,
    listImageBuildVersionsResponse_httpStatus,

    -- ** ListImagePackages
    listImagePackages_nextToken,
    listImagePackages_maxResults,
    listImagePackages_imageBuildVersionArn,
    listImagePackagesResponse_nextToken,
    listImagePackagesResponse_requestId,
    listImagePackagesResponse_imagePackageList,
    listImagePackagesResponse_httpStatus,

    -- ** ListImagePipelineImages
    listImagePipelineImages_nextToken,
    listImagePipelineImages_filters,
    listImagePipelineImages_maxResults,
    listImagePipelineImages_imagePipelineArn,
    listImagePipelineImagesResponse_nextToken,
    listImagePipelineImagesResponse_requestId,
    listImagePipelineImagesResponse_imageSummaryList,
    listImagePipelineImagesResponse_httpStatus,

    -- ** ListImagePipelines
    listImagePipelines_nextToken,
    listImagePipelines_filters,
    listImagePipelines_maxResults,
    listImagePipelinesResponse_nextToken,
    listImagePipelinesResponse_requestId,
    listImagePipelinesResponse_imagePipelineList,
    listImagePipelinesResponse_httpStatus,

    -- ** ListImageRecipes
    listImageRecipes_nextToken,
    listImageRecipes_filters,
    listImageRecipes_owner,
    listImageRecipes_maxResults,
    listImageRecipesResponse_nextToken,
    listImageRecipesResponse_requestId,
    listImageRecipesResponse_imageRecipeSummaryList,
    listImageRecipesResponse_httpStatus,

    -- ** ListImages
    listImages_nextToken,
    listImages_byName,
    listImages_filters,
    listImages_owner,
    listImages_maxResults,
    listImages_includeDeprecated,
    listImagesResponse_nextToken,
    listImagesResponse_imageVersionList,
    listImagesResponse_requestId,
    listImagesResponse_httpStatus,

    -- ** ListInfrastructureConfigurations
    listInfrastructureConfigurations_nextToken,
    listInfrastructureConfigurations_filters,
    listInfrastructureConfigurations_maxResults,
    listInfrastructureConfigurationsResponse_nextToken,
    listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList,
    listInfrastructureConfigurationsResponse_requestId,
    listInfrastructureConfigurationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutComponentPolicy
    putComponentPolicy_componentArn,
    putComponentPolicy_policy,
    putComponentPolicyResponse_componentArn,
    putComponentPolicyResponse_requestId,
    putComponentPolicyResponse_httpStatus,

    -- ** PutContainerRecipePolicy
    putContainerRecipePolicy_containerRecipeArn,
    putContainerRecipePolicy_policy,
    putContainerRecipePolicyResponse_requestId,
    putContainerRecipePolicyResponse_containerRecipeArn,
    putContainerRecipePolicyResponse_httpStatus,

    -- ** PutImagePolicy
    putImagePolicy_imageArn,
    putImagePolicy_policy,
    putImagePolicyResponse_requestId,
    putImagePolicyResponse_imageArn,
    putImagePolicyResponse_httpStatus,

    -- ** PutImageRecipePolicy
    putImageRecipePolicy_imageRecipeArn,
    putImageRecipePolicy_policy,
    putImageRecipePolicyResponse_requestId,
    putImageRecipePolicyResponse_imageRecipeArn,
    putImageRecipePolicyResponse_httpStatus,

    -- ** StartImagePipelineExecution
    startImagePipelineExecution_imagePipelineArn,
    startImagePipelineExecution_clientToken,
    startImagePipelineExecutionResponse_clientToken,
    startImagePipelineExecutionResponse_requestId,
    startImagePipelineExecutionResponse_imageBuildVersionArn,
    startImagePipelineExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDistributionConfiguration
    updateDistributionConfiguration_description,
    updateDistributionConfiguration_distributionConfigurationArn,
    updateDistributionConfiguration_distributions,
    updateDistributionConfiguration_clientToken,
    updateDistributionConfigurationResponse_clientToken,
    updateDistributionConfigurationResponse_requestId,
    updateDistributionConfigurationResponse_distributionConfigurationArn,
    updateDistributionConfigurationResponse_httpStatus,

    -- ** UpdateImagePipeline
    updateImagePipeline_enhancedImageMetadataEnabled,
    updateImagePipeline_schedule,
    updateImagePipeline_imageTestsConfiguration,
    updateImagePipeline_imageRecipeArn,
    updateImagePipeline_status,
    updateImagePipeline_description,
    updateImagePipeline_containerRecipeArn,
    updateImagePipeline_distributionConfigurationArn,
    updateImagePipeline_imagePipelineArn,
    updateImagePipeline_infrastructureConfigurationArn,
    updateImagePipeline_clientToken,
    updateImagePipelineResponse_clientToken,
    updateImagePipelineResponse_requestId,
    updateImagePipelineResponse_imagePipelineArn,
    updateImagePipelineResponse_httpStatus,

    -- ** UpdateInfrastructureConfiguration
    updateInfrastructureConfiguration_instanceTypes,
    updateInfrastructureConfiguration_securityGroupIds,
    updateInfrastructureConfiguration_subnetId,
    updateInfrastructureConfiguration_description,
    updateInfrastructureConfiguration_resourceTags,
    updateInfrastructureConfiguration_keyPair,
    updateInfrastructureConfiguration_logging,
    updateInfrastructureConfiguration_snsTopicArn,
    updateInfrastructureConfiguration_instanceMetadataOptions,
    updateInfrastructureConfiguration_terminateInstanceOnFailure,
    updateInfrastructureConfiguration_infrastructureConfigurationArn,
    updateInfrastructureConfiguration_instanceProfileName,
    updateInfrastructureConfiguration_clientToken,
    updateInfrastructureConfigurationResponse_clientToken,
    updateInfrastructureConfigurationResponse_requestId,
    updateInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    updateInfrastructureConfigurationResponse_httpStatus,

    -- * Types

    -- ** AdditionalInstanceConfiguration
    additionalInstanceConfiguration_userDataOverride,
    additionalInstanceConfiguration_systemsManagerAgent,

    -- ** Ami
    ami_name,
    ami_state,
    ami_description,
    ami_region,
    ami_accountId,
    ami_image,

    -- ** AmiDistributionConfiguration
    amiDistributionConfiguration_launchPermission,
    amiDistributionConfiguration_name,
    amiDistributionConfiguration_description,
    amiDistributionConfiguration_targetAccountIds,
    amiDistributionConfiguration_amiTags,
    amiDistributionConfiguration_kmsKeyId,

    -- ** Component
    component_tags,
    component_name,
    component_type,
    component_changeDescription,
    component_arn,
    component_state,
    component_owner,
    component_description,
    component_platform,
    component_encrypted,
    component_kmsKeyId,
    component_dateCreated,
    component_data,
    component_supportedOsVersions,
    component_version,
    component_parameters,

    -- ** ComponentConfiguration
    componentConfiguration_parameters,
    componentConfiguration_componentArn,

    -- ** ComponentParameter
    componentParameter_name,
    componentParameter_value,

    -- ** ComponentParameterDetail
    componentParameterDetail_defaultValue,
    componentParameterDetail_description,
    componentParameterDetail_name,
    componentParameterDetail_type,

    -- ** ComponentState
    componentState_status,
    componentState_reason,

    -- ** ComponentSummary
    componentSummary_tags,
    componentSummary_name,
    componentSummary_type,
    componentSummary_changeDescription,
    componentSummary_arn,
    componentSummary_state,
    componentSummary_owner,
    componentSummary_description,
    componentSummary_platform,
    componentSummary_dateCreated,
    componentSummary_supportedOsVersions,
    componentSummary_version,

    -- ** ComponentVersion
    componentVersion_name,
    componentVersion_type,
    componentVersion_arn,
    componentVersion_owner,
    componentVersion_description,
    componentVersion_platform,
    componentVersion_dateCreated,
    componentVersion_supportedOsVersions,
    componentVersion_version,

    -- ** Container
    container_region,
    container_imageUris,

    -- ** ContainerDistributionConfiguration
    containerDistributionConfiguration_containerTags,
    containerDistributionConfiguration_description,
    containerDistributionConfiguration_targetRepository,

    -- ** ContainerRecipe
    containerRecipe_tags,
    containerRecipe_name,
    containerRecipe_targetRepository,
    containerRecipe_containerType,
    containerRecipe_instanceConfiguration,
    containerRecipe_parentImage,
    containerRecipe_arn,
    containerRecipe_owner,
    containerRecipe_description,
    containerRecipe_platform,
    containerRecipe_components,
    containerRecipe_encrypted,
    containerRecipe_kmsKeyId,
    containerRecipe_dateCreated,
    containerRecipe_dockerfileTemplateData,
    containerRecipe_version,
    containerRecipe_workingDirectory,

    -- ** ContainerRecipeSummary
    containerRecipeSummary_tags,
    containerRecipeSummary_name,
    containerRecipeSummary_containerType,
    containerRecipeSummary_parentImage,
    containerRecipeSummary_arn,
    containerRecipeSummary_owner,
    containerRecipeSummary_platform,
    containerRecipeSummary_dateCreated,

    -- ** Distribution
    distribution_amiDistributionConfiguration,
    distribution_licenseConfigurationArns,
    distribution_s3ExportConfiguration,
    distribution_fastLaunchConfigurations,
    distribution_launchTemplateConfigurations,
    distribution_containerDistributionConfiguration,
    distribution_region,

    -- ** DistributionConfiguration
    distributionConfiguration_tags,
    distributionConfiguration_name,
    distributionConfiguration_arn,
    distributionConfiguration_description,
    distributionConfiguration_dateUpdated,
    distributionConfiguration_dateCreated,
    distributionConfiguration_distributions,
    distributionConfiguration_timeoutMinutes,

    -- ** DistributionConfigurationSummary
    distributionConfigurationSummary_tags,
    distributionConfigurationSummary_name,
    distributionConfigurationSummary_regions,
    distributionConfigurationSummary_arn,
    distributionConfigurationSummary_description,
    distributionConfigurationSummary_dateUpdated,
    distributionConfigurationSummary_dateCreated,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_snapshotId,
    ebsInstanceBlockDeviceSpecification_volumeType,
    ebsInstanceBlockDeviceSpecification_volumeSize,
    ebsInstanceBlockDeviceSpecification_encrypted,
    ebsInstanceBlockDeviceSpecification_kmsKeyId,
    ebsInstanceBlockDeviceSpecification_throughput,
    ebsInstanceBlockDeviceSpecification_iops,

    -- ** FastLaunchConfiguration
    fastLaunchConfiguration_launchTemplate,
    fastLaunchConfiguration_snapshotConfiguration,
    fastLaunchConfiguration_accountId,
    fastLaunchConfiguration_maxParallelLaunches,
    fastLaunchConfiguration_enabled,

    -- ** FastLaunchLaunchTemplateSpecification
    fastLaunchLaunchTemplateSpecification_launchTemplateVersion,
    fastLaunchLaunchTemplateSpecification_launchTemplateId,
    fastLaunchLaunchTemplateSpecification_launchTemplateName,

    -- ** FastLaunchSnapshotConfiguration
    fastLaunchSnapshotConfiguration_targetResourceCount,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** Image
    image_enhancedImageMetadataEnabled,
    image_tags,
    image_name,
    image_type,
    image_imageTestsConfiguration,
    image_sourcePipelineArn,
    image_arn,
    image_state,
    image_osVersion,
    image_containerRecipe,
    image_platform,
    image_distributionConfiguration,
    image_imageRecipe,
    image_outputResources,
    image_sourcePipelineName,
    image_dateCreated,
    image_infrastructureConfiguration,
    image_buildType,
    image_version,

    -- ** ImagePackage
    imagePackage_packageName,
    imagePackage_packageVersion,

    -- ** ImagePipeline
    imagePipeline_enhancedImageMetadataEnabled,
    imagePipeline_schedule,
    imagePipeline_tags,
    imagePipeline_name,
    imagePipeline_imageTestsConfiguration,
    imagePipeline_arn,
    imagePipeline_imageRecipeArn,
    imagePipeline_status,
    imagePipeline_description,
    imagePipeline_platform,
    imagePipeline_containerRecipeArn,
    imagePipeline_dateLastRun,
    imagePipeline_dateNextRun,
    imagePipeline_infrastructureConfigurationArn,
    imagePipeline_dateUpdated,
    imagePipeline_dateCreated,
    imagePipeline_distributionConfigurationArn,

    -- ** ImageRecipe
    imageRecipe_tags,
    imageRecipe_name,
    imageRecipe_type,
    imageRecipe_parentImage,
    imageRecipe_blockDeviceMappings,
    imageRecipe_arn,
    imageRecipe_owner,
    imageRecipe_description,
    imageRecipe_platform,
    imageRecipe_additionalInstanceConfiguration,
    imageRecipe_components,
    imageRecipe_dateCreated,
    imageRecipe_version,
    imageRecipe_workingDirectory,

    -- ** ImageRecipeSummary
    imageRecipeSummary_tags,
    imageRecipeSummary_name,
    imageRecipeSummary_parentImage,
    imageRecipeSummary_arn,
    imageRecipeSummary_owner,
    imageRecipeSummary_platform,
    imageRecipeSummary_dateCreated,

    -- ** ImageState
    imageState_status,
    imageState_reason,

    -- ** ImageSummary
    imageSummary_tags,
    imageSummary_name,
    imageSummary_type,
    imageSummary_arn,
    imageSummary_state,
    imageSummary_osVersion,
    imageSummary_owner,
    imageSummary_platform,
    imageSummary_outputResources,
    imageSummary_dateCreated,
    imageSummary_buildType,
    imageSummary_version,

    -- ** ImageTestsConfiguration
    imageTestsConfiguration_imageTestsEnabled,
    imageTestsConfiguration_timeoutMinutes,

    -- ** ImageVersion
    imageVersion_name,
    imageVersion_type,
    imageVersion_arn,
    imageVersion_osVersion,
    imageVersion_owner,
    imageVersion_platform,
    imageVersion_dateCreated,
    imageVersion_buildType,
    imageVersion_version,

    -- ** InfrastructureConfiguration
    infrastructureConfiguration_tags,
    infrastructureConfiguration_name,
    infrastructureConfiguration_instanceProfileName,
    infrastructureConfiguration_instanceTypes,
    infrastructureConfiguration_securityGroupIds,
    infrastructureConfiguration_subnetId,
    infrastructureConfiguration_arn,
    infrastructureConfiguration_description,
    infrastructureConfiguration_resourceTags,
    infrastructureConfiguration_keyPair,
    infrastructureConfiguration_logging,
    infrastructureConfiguration_snsTopicArn,
    infrastructureConfiguration_instanceMetadataOptions,
    infrastructureConfiguration_dateUpdated,
    infrastructureConfiguration_dateCreated,
    infrastructureConfiguration_terminateInstanceOnFailure,

    -- ** InfrastructureConfigurationSummary
    infrastructureConfigurationSummary_tags,
    infrastructureConfigurationSummary_name,
    infrastructureConfigurationSummary_instanceProfileName,
    infrastructureConfigurationSummary_instanceTypes,
    infrastructureConfigurationSummary_arn,
    infrastructureConfigurationSummary_description,
    infrastructureConfigurationSummary_resourceTags,
    infrastructureConfigurationSummary_dateUpdated,
    infrastructureConfigurationSummary_dateCreated,

    -- ** InstanceBlockDeviceMapping
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_noDevice,
    instanceBlockDeviceMapping_virtualName,

    -- ** InstanceConfiguration
    instanceConfiguration_blockDeviceMappings,
    instanceConfiguration_image,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- ** LaunchPermissionConfiguration
    launchPermissionConfiguration_userGroups,
    launchPermissionConfiguration_organizationArns,
    launchPermissionConfiguration_userIds,
    launchPermissionConfiguration_organizationalUnitArns,

    -- ** LaunchTemplateConfiguration
    launchTemplateConfiguration_accountId,
    launchTemplateConfiguration_setDefaultVersion,
    launchTemplateConfiguration_launchTemplateId,

    -- ** Logging
    logging_s3Logs,

    -- ** OutputResources
    outputResources_containers,
    outputResources_amis,

    -- ** S3ExportConfiguration
    s3ExportConfiguration_s3Prefix,
    s3ExportConfiguration_roleName,
    s3ExportConfiguration_diskImageFormat,
    s3ExportConfiguration_s3Bucket,

    -- ** S3Logs
    s3Logs_s3KeyPrefix,
    s3Logs_s3BucketName,

    -- ** Schedule
    schedule_timezone,
    schedule_scheduleExpression,
    schedule_pipelineExecutionStartCondition,

    -- ** SystemsManagerAgent
    systemsManagerAgent_uninstallAfterBuild,

    -- ** TargetContainerRepository
    targetContainerRepository_service,
    targetContainerRepository_repositoryName,
  )
where

import Amazonka.ImageBuilder.CancelImageCreation
import Amazonka.ImageBuilder.CreateComponent
import Amazonka.ImageBuilder.CreateContainerRecipe
import Amazonka.ImageBuilder.CreateDistributionConfiguration
import Amazonka.ImageBuilder.CreateImage
import Amazonka.ImageBuilder.CreateImagePipeline
import Amazonka.ImageBuilder.CreateImageRecipe
import Amazonka.ImageBuilder.CreateInfrastructureConfiguration
import Amazonka.ImageBuilder.DeleteComponent
import Amazonka.ImageBuilder.DeleteContainerRecipe
import Amazonka.ImageBuilder.DeleteDistributionConfiguration
import Amazonka.ImageBuilder.DeleteImage
import Amazonka.ImageBuilder.DeleteImagePipeline
import Amazonka.ImageBuilder.DeleteImageRecipe
import Amazonka.ImageBuilder.DeleteInfrastructureConfiguration
import Amazonka.ImageBuilder.GetComponent
import Amazonka.ImageBuilder.GetComponentPolicy
import Amazonka.ImageBuilder.GetContainerRecipe
import Amazonka.ImageBuilder.GetContainerRecipePolicy
import Amazonka.ImageBuilder.GetDistributionConfiguration
import Amazonka.ImageBuilder.GetImage
import Amazonka.ImageBuilder.GetImagePipeline
import Amazonka.ImageBuilder.GetImagePolicy
import Amazonka.ImageBuilder.GetImageRecipe
import Amazonka.ImageBuilder.GetImageRecipePolicy
import Amazonka.ImageBuilder.GetInfrastructureConfiguration
import Amazonka.ImageBuilder.ImportComponent
import Amazonka.ImageBuilder.ImportVmImage
import Amazonka.ImageBuilder.ListComponentBuildVersions
import Amazonka.ImageBuilder.ListComponents
import Amazonka.ImageBuilder.ListContainerRecipes
import Amazonka.ImageBuilder.ListDistributionConfigurations
import Amazonka.ImageBuilder.ListImageBuildVersions
import Amazonka.ImageBuilder.ListImagePackages
import Amazonka.ImageBuilder.ListImagePipelineImages
import Amazonka.ImageBuilder.ListImagePipelines
import Amazonka.ImageBuilder.ListImageRecipes
import Amazonka.ImageBuilder.ListImages
import Amazonka.ImageBuilder.ListInfrastructureConfigurations
import Amazonka.ImageBuilder.ListTagsForResource
import Amazonka.ImageBuilder.PutComponentPolicy
import Amazonka.ImageBuilder.PutContainerRecipePolicy
import Amazonka.ImageBuilder.PutImagePolicy
import Amazonka.ImageBuilder.PutImageRecipePolicy
import Amazonka.ImageBuilder.StartImagePipelineExecution
import Amazonka.ImageBuilder.TagResource
import Amazonka.ImageBuilder.Types.AdditionalInstanceConfiguration
import Amazonka.ImageBuilder.Types.Ami
import Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
import Amazonka.ImageBuilder.Types.Component
import Amazonka.ImageBuilder.Types.ComponentConfiguration
import Amazonka.ImageBuilder.Types.ComponentParameter
import Amazonka.ImageBuilder.Types.ComponentParameterDetail
import Amazonka.ImageBuilder.Types.ComponentState
import Amazonka.ImageBuilder.Types.ComponentSummary
import Amazonka.ImageBuilder.Types.ComponentVersion
import Amazonka.ImageBuilder.Types.Container
import Amazonka.ImageBuilder.Types.ContainerDistributionConfiguration
import Amazonka.ImageBuilder.Types.ContainerRecipe
import Amazonka.ImageBuilder.Types.ContainerRecipeSummary
import Amazonka.ImageBuilder.Types.Distribution
import Amazonka.ImageBuilder.Types.DistributionConfiguration
import Amazonka.ImageBuilder.Types.DistributionConfigurationSummary
import Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification
import Amazonka.ImageBuilder.Types.FastLaunchConfiguration
import Amazonka.ImageBuilder.Types.FastLaunchLaunchTemplateSpecification
import Amazonka.ImageBuilder.Types.FastLaunchSnapshotConfiguration
import Amazonka.ImageBuilder.Types.Filter
import Amazonka.ImageBuilder.Types.Image
import Amazonka.ImageBuilder.Types.ImagePackage
import Amazonka.ImageBuilder.Types.ImagePipeline
import Amazonka.ImageBuilder.Types.ImageRecipe
import Amazonka.ImageBuilder.Types.ImageRecipeSummary
import Amazonka.ImageBuilder.Types.ImageState
import Amazonka.ImageBuilder.Types.ImageSummary
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.ImageVersion
import Amazonka.ImageBuilder.Types.InfrastructureConfiguration
import Amazonka.ImageBuilder.Types.InfrastructureConfigurationSummary
import Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping
import Amazonka.ImageBuilder.Types.InstanceConfiguration
import Amazonka.ImageBuilder.Types.InstanceMetadataOptions
import Amazonka.ImageBuilder.Types.LaunchPermissionConfiguration
import Amazonka.ImageBuilder.Types.LaunchTemplateConfiguration
import Amazonka.ImageBuilder.Types.Logging
import Amazonka.ImageBuilder.Types.OutputResources
import Amazonka.ImageBuilder.Types.S3ExportConfiguration
import Amazonka.ImageBuilder.Types.S3Logs
import Amazonka.ImageBuilder.Types.Schedule
import Amazonka.ImageBuilder.Types.SystemsManagerAgent
import Amazonka.ImageBuilder.Types.TargetContainerRepository
import Amazonka.ImageBuilder.UntagResource
import Amazonka.ImageBuilder.UpdateDistributionConfiguration
import Amazonka.ImageBuilder.UpdateImagePipeline
import Amazonka.ImageBuilder.UpdateInfrastructureConfiguration
