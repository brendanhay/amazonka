{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Lens
  ( -- * Operations

    -- ** CancelImageCreation
    cancelImageCreation_imageBuildVersionArn,
    cancelImageCreation_clientToken,
    cancelImageCreationResponse_clientToken,
    cancelImageCreationResponse_imageBuildVersionArn,
    cancelImageCreationResponse_requestId,
    cancelImageCreationResponse_httpStatus,

    -- ** CreateComponent
    createComponent_changeDescription,
    createComponent_data,
    createComponent_description,
    createComponent_kmsKeyId,
    createComponent_supportedOsVersions,
    createComponent_tags,
    createComponent_uri,
    createComponent_name,
    createComponent_semanticVersion,
    createComponent_platform,
    createComponent_clientToken,
    createComponentResponse_clientToken,
    createComponentResponse_componentBuildVersionArn,
    createComponentResponse_requestId,
    createComponentResponse_httpStatus,

    -- ** CreateContainerRecipe
    createContainerRecipe_description,
    createContainerRecipe_dockerfileTemplateData,
    createContainerRecipe_dockerfileTemplateUri,
    createContainerRecipe_imageOsVersionOverride,
    createContainerRecipe_instanceConfiguration,
    createContainerRecipe_kmsKeyId,
    createContainerRecipe_platformOverride,
    createContainerRecipe_tags,
    createContainerRecipe_workingDirectory,
    createContainerRecipe_containerType,
    createContainerRecipe_name,
    createContainerRecipe_semanticVersion,
    createContainerRecipe_components,
    createContainerRecipe_parentImage,
    createContainerRecipe_targetRepository,
    createContainerRecipe_clientToken,
    createContainerRecipeResponse_clientToken,
    createContainerRecipeResponse_containerRecipeArn,
    createContainerRecipeResponse_requestId,
    createContainerRecipeResponse_httpStatus,

    -- ** CreateDistributionConfiguration
    createDistributionConfiguration_description,
    createDistributionConfiguration_tags,
    createDistributionConfiguration_name,
    createDistributionConfiguration_distributions,
    createDistributionConfiguration_clientToken,
    createDistributionConfigurationResponse_clientToken,
    createDistributionConfigurationResponse_distributionConfigurationArn,
    createDistributionConfigurationResponse_requestId,
    createDistributionConfigurationResponse_httpStatus,

    -- ** CreateImage
    createImage_containerRecipeArn,
    createImage_distributionConfigurationArn,
    createImage_enhancedImageMetadataEnabled,
    createImage_imageRecipeArn,
    createImage_imageTestsConfiguration,
    createImage_tags,
    createImage_infrastructureConfigurationArn,
    createImage_clientToken,
    createImageResponse_clientToken,
    createImageResponse_imageBuildVersionArn,
    createImageResponse_requestId,
    createImageResponse_httpStatus,

    -- ** CreateImagePipeline
    createImagePipeline_containerRecipeArn,
    createImagePipeline_description,
    createImagePipeline_distributionConfigurationArn,
    createImagePipeline_enhancedImageMetadataEnabled,
    createImagePipeline_imageRecipeArn,
    createImagePipeline_imageTestsConfiguration,
    createImagePipeline_schedule,
    createImagePipeline_status,
    createImagePipeline_tags,
    createImagePipeline_name,
    createImagePipeline_infrastructureConfigurationArn,
    createImagePipeline_clientToken,
    createImagePipelineResponse_clientToken,
    createImagePipelineResponse_imagePipelineArn,
    createImagePipelineResponse_requestId,
    createImagePipelineResponse_httpStatus,

    -- ** CreateImageRecipe
    createImageRecipe_additionalInstanceConfiguration,
    createImageRecipe_blockDeviceMappings,
    createImageRecipe_description,
    createImageRecipe_tags,
    createImageRecipe_workingDirectory,
    createImageRecipe_name,
    createImageRecipe_semanticVersion,
    createImageRecipe_components,
    createImageRecipe_parentImage,
    createImageRecipe_clientToken,
    createImageRecipeResponse_clientToken,
    createImageRecipeResponse_imageRecipeArn,
    createImageRecipeResponse_requestId,
    createImageRecipeResponse_httpStatus,

    -- ** CreateInfrastructureConfiguration
    createInfrastructureConfiguration_description,
    createInfrastructureConfiguration_instanceMetadataOptions,
    createInfrastructureConfiguration_instanceTypes,
    createInfrastructureConfiguration_keyPair,
    createInfrastructureConfiguration_logging,
    createInfrastructureConfiguration_resourceTags,
    createInfrastructureConfiguration_securityGroupIds,
    createInfrastructureConfiguration_snsTopicArn,
    createInfrastructureConfiguration_subnetId,
    createInfrastructureConfiguration_tags,
    createInfrastructureConfiguration_terminateInstanceOnFailure,
    createInfrastructureConfiguration_name,
    createInfrastructureConfiguration_instanceProfileName,
    createInfrastructureConfiguration_clientToken,
    createInfrastructureConfigurationResponse_clientToken,
    createInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    createInfrastructureConfigurationResponse_requestId,
    createInfrastructureConfigurationResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_componentBuildVersionArn,
    deleteComponentResponse_componentBuildVersionArn,
    deleteComponentResponse_requestId,
    deleteComponentResponse_httpStatus,

    -- ** DeleteContainerRecipe
    deleteContainerRecipe_containerRecipeArn,
    deleteContainerRecipeResponse_containerRecipeArn,
    deleteContainerRecipeResponse_requestId,
    deleteContainerRecipeResponse_httpStatus,

    -- ** DeleteDistributionConfiguration
    deleteDistributionConfiguration_distributionConfigurationArn,
    deleteDistributionConfigurationResponse_distributionConfigurationArn,
    deleteDistributionConfigurationResponse_requestId,
    deleteDistributionConfigurationResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_imageBuildVersionArn,
    deleteImageResponse_imageBuildVersionArn,
    deleteImageResponse_requestId,
    deleteImageResponse_httpStatus,

    -- ** DeleteImagePipeline
    deleteImagePipeline_imagePipelineArn,
    deleteImagePipelineResponse_imagePipelineArn,
    deleteImagePipelineResponse_requestId,
    deleteImagePipelineResponse_httpStatus,

    -- ** DeleteImageRecipe
    deleteImageRecipe_imageRecipeArn,
    deleteImageRecipeResponse_imageRecipeArn,
    deleteImageRecipeResponse_requestId,
    deleteImageRecipeResponse_httpStatus,

    -- ** DeleteInfrastructureConfiguration
    deleteInfrastructureConfiguration_infrastructureConfigurationArn,
    deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    deleteInfrastructureConfigurationResponse_requestId,
    deleteInfrastructureConfigurationResponse_httpStatus,

    -- ** GetComponent
    getComponent_componentBuildVersionArn,
    getComponentResponse_component,
    getComponentResponse_requestId,
    getComponentResponse_httpStatus,

    -- ** GetComponentPolicy
    getComponentPolicy_componentArn,
    getComponentPolicyResponse_policy,
    getComponentPolicyResponse_requestId,
    getComponentPolicyResponse_httpStatus,

    -- ** GetContainerRecipe
    getContainerRecipe_containerRecipeArn,
    getContainerRecipeResponse_containerRecipe,
    getContainerRecipeResponse_requestId,
    getContainerRecipeResponse_httpStatus,

    -- ** GetContainerRecipePolicy
    getContainerRecipePolicy_containerRecipeArn,
    getContainerRecipePolicyResponse_policy,
    getContainerRecipePolicyResponse_requestId,
    getContainerRecipePolicyResponse_httpStatus,

    -- ** GetDistributionConfiguration
    getDistributionConfiguration_distributionConfigurationArn,
    getDistributionConfigurationResponse_distributionConfiguration,
    getDistributionConfigurationResponse_requestId,
    getDistributionConfigurationResponse_httpStatus,

    -- ** GetImage
    getImage_imageBuildVersionArn,
    getImageResponse_image,
    getImageResponse_requestId,
    getImageResponse_httpStatus,

    -- ** GetImagePipeline
    getImagePipeline_imagePipelineArn,
    getImagePipelineResponse_imagePipeline,
    getImagePipelineResponse_requestId,
    getImagePipelineResponse_httpStatus,

    -- ** GetImagePolicy
    getImagePolicy_imageArn,
    getImagePolicyResponse_policy,
    getImagePolicyResponse_requestId,
    getImagePolicyResponse_httpStatus,

    -- ** GetImageRecipe
    getImageRecipe_imageRecipeArn,
    getImageRecipeResponse_imageRecipe,
    getImageRecipeResponse_requestId,
    getImageRecipeResponse_httpStatus,

    -- ** GetImageRecipePolicy
    getImageRecipePolicy_imageRecipeArn,
    getImageRecipePolicyResponse_policy,
    getImageRecipePolicyResponse_requestId,
    getImageRecipePolicyResponse_httpStatus,

    -- ** GetInfrastructureConfiguration
    getInfrastructureConfiguration_infrastructureConfigurationArn,
    getInfrastructureConfigurationResponse_infrastructureConfiguration,
    getInfrastructureConfigurationResponse_requestId,
    getInfrastructureConfigurationResponse_httpStatus,

    -- ** ImportComponent
    importComponent_changeDescription,
    importComponent_data,
    importComponent_description,
    importComponent_kmsKeyId,
    importComponent_tags,
    importComponent_uri,
    importComponent_name,
    importComponent_semanticVersion,
    importComponent_type,
    importComponent_format,
    importComponent_platform,
    importComponent_clientToken,
    importComponentResponse_clientToken,
    importComponentResponse_componentBuildVersionArn,
    importComponentResponse_requestId,
    importComponentResponse_httpStatus,

    -- ** ImportVmImage
    importVmImage_description,
    importVmImage_osVersion,
    importVmImage_tags,
    importVmImage_name,
    importVmImage_semanticVersion,
    importVmImage_platform,
    importVmImage_vmImportTaskId,
    importVmImage_clientToken,
    importVmImageResponse_clientToken,
    importVmImageResponse_imageArn,
    importVmImageResponse_requestId,
    importVmImageResponse_httpStatus,

    -- ** ListComponentBuildVersions
    listComponentBuildVersions_maxResults,
    listComponentBuildVersions_nextToken,
    listComponentBuildVersions_componentVersionArn,
    listComponentBuildVersionsResponse_componentSummaryList,
    listComponentBuildVersionsResponse_nextToken,
    listComponentBuildVersionsResponse_requestId,
    listComponentBuildVersionsResponse_httpStatus,

    -- ** ListComponents
    listComponents_byName,
    listComponents_filters,
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_owner,
    listComponentsResponse_componentVersionList,
    listComponentsResponse_nextToken,
    listComponentsResponse_requestId,
    listComponentsResponse_httpStatus,

    -- ** ListContainerRecipes
    listContainerRecipes_filters,
    listContainerRecipes_maxResults,
    listContainerRecipes_nextToken,
    listContainerRecipes_owner,
    listContainerRecipesResponse_containerRecipeSummaryList,
    listContainerRecipesResponse_nextToken,
    listContainerRecipesResponse_requestId,
    listContainerRecipesResponse_httpStatus,

    -- ** ListDistributionConfigurations
    listDistributionConfigurations_filters,
    listDistributionConfigurations_maxResults,
    listDistributionConfigurations_nextToken,
    listDistributionConfigurationsResponse_distributionConfigurationSummaryList,
    listDistributionConfigurationsResponse_nextToken,
    listDistributionConfigurationsResponse_requestId,
    listDistributionConfigurationsResponse_httpStatus,

    -- ** ListImageBuildVersions
    listImageBuildVersions_filters,
    listImageBuildVersions_maxResults,
    listImageBuildVersions_nextToken,
    listImageBuildVersions_imageVersionArn,
    listImageBuildVersionsResponse_imageSummaryList,
    listImageBuildVersionsResponse_nextToken,
    listImageBuildVersionsResponse_requestId,
    listImageBuildVersionsResponse_httpStatus,

    -- ** ListImagePackages
    listImagePackages_maxResults,
    listImagePackages_nextToken,
    listImagePackages_imageBuildVersionArn,
    listImagePackagesResponse_imagePackageList,
    listImagePackagesResponse_nextToken,
    listImagePackagesResponse_requestId,
    listImagePackagesResponse_httpStatus,

    -- ** ListImagePipelineImages
    listImagePipelineImages_filters,
    listImagePipelineImages_maxResults,
    listImagePipelineImages_nextToken,
    listImagePipelineImages_imagePipelineArn,
    listImagePipelineImagesResponse_imageSummaryList,
    listImagePipelineImagesResponse_nextToken,
    listImagePipelineImagesResponse_requestId,
    listImagePipelineImagesResponse_httpStatus,

    -- ** ListImagePipelines
    listImagePipelines_filters,
    listImagePipelines_maxResults,
    listImagePipelines_nextToken,
    listImagePipelinesResponse_imagePipelineList,
    listImagePipelinesResponse_nextToken,
    listImagePipelinesResponse_requestId,
    listImagePipelinesResponse_httpStatus,

    -- ** ListImageRecipes
    listImageRecipes_filters,
    listImageRecipes_maxResults,
    listImageRecipes_nextToken,
    listImageRecipes_owner,
    listImageRecipesResponse_imageRecipeSummaryList,
    listImageRecipesResponse_nextToken,
    listImageRecipesResponse_requestId,
    listImageRecipesResponse_httpStatus,

    -- ** ListImages
    listImages_byName,
    listImages_filters,
    listImages_includeDeprecated,
    listImages_maxResults,
    listImages_nextToken,
    listImages_owner,
    listImagesResponse_imageVersionList,
    listImagesResponse_nextToken,
    listImagesResponse_requestId,
    listImagesResponse_httpStatus,

    -- ** ListInfrastructureConfigurations
    listInfrastructureConfigurations_filters,
    listInfrastructureConfigurations_maxResults,
    listInfrastructureConfigurations_nextToken,
    listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList,
    listInfrastructureConfigurationsResponse_nextToken,
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
    putContainerRecipePolicyResponse_containerRecipeArn,
    putContainerRecipePolicyResponse_requestId,
    putContainerRecipePolicyResponse_httpStatus,

    -- ** PutImagePolicy
    putImagePolicy_imageArn,
    putImagePolicy_policy,
    putImagePolicyResponse_imageArn,
    putImagePolicyResponse_requestId,
    putImagePolicyResponse_httpStatus,

    -- ** PutImageRecipePolicy
    putImageRecipePolicy_imageRecipeArn,
    putImageRecipePolicy_policy,
    putImageRecipePolicyResponse_imageRecipeArn,
    putImageRecipePolicyResponse_requestId,
    putImageRecipePolicyResponse_httpStatus,

    -- ** StartImagePipelineExecution
    startImagePipelineExecution_imagePipelineArn,
    startImagePipelineExecution_clientToken,
    startImagePipelineExecutionResponse_clientToken,
    startImagePipelineExecutionResponse_imageBuildVersionArn,
    startImagePipelineExecutionResponse_requestId,
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
    updateDistributionConfigurationResponse_distributionConfigurationArn,
    updateDistributionConfigurationResponse_requestId,
    updateDistributionConfigurationResponse_httpStatus,

    -- ** UpdateImagePipeline
    updateImagePipeline_containerRecipeArn,
    updateImagePipeline_description,
    updateImagePipeline_distributionConfigurationArn,
    updateImagePipeline_enhancedImageMetadataEnabled,
    updateImagePipeline_imageRecipeArn,
    updateImagePipeline_imageTestsConfiguration,
    updateImagePipeline_schedule,
    updateImagePipeline_status,
    updateImagePipeline_imagePipelineArn,
    updateImagePipeline_infrastructureConfigurationArn,
    updateImagePipeline_clientToken,
    updateImagePipelineResponse_clientToken,
    updateImagePipelineResponse_imagePipelineArn,
    updateImagePipelineResponse_requestId,
    updateImagePipelineResponse_httpStatus,

    -- ** UpdateInfrastructureConfiguration
    updateInfrastructureConfiguration_description,
    updateInfrastructureConfiguration_instanceMetadataOptions,
    updateInfrastructureConfiguration_instanceTypes,
    updateInfrastructureConfiguration_keyPair,
    updateInfrastructureConfiguration_logging,
    updateInfrastructureConfiguration_resourceTags,
    updateInfrastructureConfiguration_securityGroupIds,
    updateInfrastructureConfiguration_snsTopicArn,
    updateInfrastructureConfiguration_subnetId,
    updateInfrastructureConfiguration_terminateInstanceOnFailure,
    updateInfrastructureConfiguration_infrastructureConfigurationArn,
    updateInfrastructureConfiguration_instanceProfileName,
    updateInfrastructureConfiguration_clientToken,
    updateInfrastructureConfigurationResponse_clientToken,
    updateInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    updateInfrastructureConfigurationResponse_requestId,
    updateInfrastructureConfigurationResponse_httpStatus,

    -- * Types

    -- ** AdditionalInstanceConfiguration
    additionalInstanceConfiguration_systemsManagerAgent,
    additionalInstanceConfiguration_userDataOverride,

    -- ** Ami
    ami_accountId,
    ami_description,
    ami_image,
    ami_name,
    ami_region,
    ami_state,

    -- ** AmiDistributionConfiguration
    amiDistributionConfiguration_amiTags,
    amiDistributionConfiguration_description,
    amiDistributionConfiguration_kmsKeyId,
    amiDistributionConfiguration_launchPermission,
    amiDistributionConfiguration_name,
    amiDistributionConfiguration_targetAccountIds,

    -- ** Component
    component_arn,
    component_changeDescription,
    component_data,
    component_dateCreated,
    component_description,
    component_encrypted,
    component_kmsKeyId,
    component_name,
    component_owner,
    component_parameters,
    component_platform,
    component_state,
    component_supportedOsVersions,
    component_tags,
    component_type,
    component_version,

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
    componentState_reason,
    componentState_status,

    -- ** ComponentSummary
    componentSummary_arn,
    componentSummary_changeDescription,
    componentSummary_dateCreated,
    componentSummary_description,
    componentSummary_name,
    componentSummary_owner,
    componentSummary_platform,
    componentSummary_state,
    componentSummary_supportedOsVersions,
    componentSummary_tags,
    componentSummary_type,
    componentSummary_version,

    -- ** ComponentVersion
    componentVersion_arn,
    componentVersion_dateCreated,
    componentVersion_description,
    componentVersion_name,
    componentVersion_owner,
    componentVersion_platform,
    componentVersion_supportedOsVersions,
    componentVersion_type,
    componentVersion_version,

    -- ** Container
    container_imageUris,
    container_region,

    -- ** ContainerDistributionConfiguration
    containerDistributionConfiguration_containerTags,
    containerDistributionConfiguration_description,
    containerDistributionConfiguration_targetRepository,

    -- ** ContainerRecipe
    containerRecipe_arn,
    containerRecipe_components,
    containerRecipe_containerType,
    containerRecipe_dateCreated,
    containerRecipe_description,
    containerRecipe_dockerfileTemplateData,
    containerRecipe_encrypted,
    containerRecipe_instanceConfiguration,
    containerRecipe_kmsKeyId,
    containerRecipe_name,
    containerRecipe_owner,
    containerRecipe_parentImage,
    containerRecipe_platform,
    containerRecipe_tags,
    containerRecipe_targetRepository,
    containerRecipe_version,
    containerRecipe_workingDirectory,

    -- ** ContainerRecipeSummary
    containerRecipeSummary_arn,
    containerRecipeSummary_containerType,
    containerRecipeSummary_dateCreated,
    containerRecipeSummary_name,
    containerRecipeSummary_owner,
    containerRecipeSummary_parentImage,
    containerRecipeSummary_platform,
    containerRecipeSummary_tags,

    -- ** Distribution
    distribution_amiDistributionConfiguration,
    distribution_containerDistributionConfiguration,
    distribution_fastLaunchConfigurations,
    distribution_launchTemplateConfigurations,
    distribution_licenseConfigurationArns,
    distribution_s3ExportConfiguration,
    distribution_region,

    -- ** DistributionConfiguration
    distributionConfiguration_arn,
    distributionConfiguration_dateCreated,
    distributionConfiguration_dateUpdated,
    distributionConfiguration_description,
    distributionConfiguration_distributions,
    distributionConfiguration_name,
    distributionConfiguration_tags,
    distributionConfiguration_timeoutMinutes,

    -- ** DistributionConfigurationSummary
    distributionConfigurationSummary_arn,
    distributionConfigurationSummary_dateCreated,
    distributionConfigurationSummary_dateUpdated,
    distributionConfigurationSummary_description,
    distributionConfigurationSummary_name,
    distributionConfigurationSummary_regions,
    distributionConfigurationSummary_tags,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_encrypted,
    ebsInstanceBlockDeviceSpecification_iops,
    ebsInstanceBlockDeviceSpecification_kmsKeyId,
    ebsInstanceBlockDeviceSpecification_snapshotId,
    ebsInstanceBlockDeviceSpecification_throughput,
    ebsInstanceBlockDeviceSpecification_volumeSize,
    ebsInstanceBlockDeviceSpecification_volumeType,

    -- ** FastLaunchConfiguration
    fastLaunchConfiguration_accountId,
    fastLaunchConfiguration_launchTemplate,
    fastLaunchConfiguration_maxParallelLaunches,
    fastLaunchConfiguration_snapshotConfiguration,
    fastLaunchConfiguration_enabled,

    -- ** FastLaunchLaunchTemplateSpecification
    fastLaunchLaunchTemplateSpecification_launchTemplateId,
    fastLaunchLaunchTemplateSpecification_launchTemplateName,
    fastLaunchLaunchTemplateSpecification_launchTemplateVersion,

    -- ** FastLaunchSnapshotConfiguration
    fastLaunchSnapshotConfiguration_targetResourceCount,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** Image
    image_arn,
    image_buildType,
    image_containerRecipe,
    image_dateCreated,
    image_distributionConfiguration,
    image_enhancedImageMetadataEnabled,
    image_imageRecipe,
    image_imageTestsConfiguration,
    image_infrastructureConfiguration,
    image_name,
    image_osVersion,
    image_outputResources,
    image_platform,
    image_sourcePipelineArn,
    image_sourcePipelineName,
    image_state,
    image_tags,
    image_type,
    image_version,

    -- ** ImagePackage
    imagePackage_packageName,
    imagePackage_packageVersion,

    -- ** ImagePipeline
    imagePipeline_arn,
    imagePipeline_containerRecipeArn,
    imagePipeline_dateCreated,
    imagePipeline_dateLastRun,
    imagePipeline_dateNextRun,
    imagePipeline_dateUpdated,
    imagePipeline_description,
    imagePipeline_distributionConfigurationArn,
    imagePipeline_enhancedImageMetadataEnabled,
    imagePipeline_imageRecipeArn,
    imagePipeline_imageTestsConfiguration,
    imagePipeline_infrastructureConfigurationArn,
    imagePipeline_name,
    imagePipeline_platform,
    imagePipeline_schedule,
    imagePipeline_status,
    imagePipeline_tags,

    -- ** ImageRecipe
    imageRecipe_additionalInstanceConfiguration,
    imageRecipe_arn,
    imageRecipe_blockDeviceMappings,
    imageRecipe_components,
    imageRecipe_dateCreated,
    imageRecipe_description,
    imageRecipe_name,
    imageRecipe_owner,
    imageRecipe_parentImage,
    imageRecipe_platform,
    imageRecipe_tags,
    imageRecipe_type,
    imageRecipe_version,
    imageRecipe_workingDirectory,

    -- ** ImageRecipeSummary
    imageRecipeSummary_arn,
    imageRecipeSummary_dateCreated,
    imageRecipeSummary_name,
    imageRecipeSummary_owner,
    imageRecipeSummary_parentImage,
    imageRecipeSummary_platform,
    imageRecipeSummary_tags,

    -- ** ImageState
    imageState_reason,
    imageState_status,

    -- ** ImageSummary
    imageSummary_arn,
    imageSummary_buildType,
    imageSummary_dateCreated,
    imageSummary_name,
    imageSummary_osVersion,
    imageSummary_outputResources,
    imageSummary_owner,
    imageSummary_platform,
    imageSummary_state,
    imageSummary_tags,
    imageSummary_type,
    imageSummary_version,

    -- ** ImageTestsConfiguration
    imageTestsConfiguration_imageTestsEnabled,
    imageTestsConfiguration_timeoutMinutes,

    -- ** ImageVersion
    imageVersion_arn,
    imageVersion_buildType,
    imageVersion_dateCreated,
    imageVersion_name,
    imageVersion_osVersion,
    imageVersion_owner,
    imageVersion_platform,
    imageVersion_type,
    imageVersion_version,

    -- ** InfrastructureConfiguration
    infrastructureConfiguration_arn,
    infrastructureConfiguration_dateCreated,
    infrastructureConfiguration_dateUpdated,
    infrastructureConfiguration_description,
    infrastructureConfiguration_instanceMetadataOptions,
    infrastructureConfiguration_instanceProfileName,
    infrastructureConfiguration_instanceTypes,
    infrastructureConfiguration_keyPair,
    infrastructureConfiguration_logging,
    infrastructureConfiguration_name,
    infrastructureConfiguration_resourceTags,
    infrastructureConfiguration_securityGroupIds,
    infrastructureConfiguration_snsTopicArn,
    infrastructureConfiguration_subnetId,
    infrastructureConfiguration_tags,
    infrastructureConfiguration_terminateInstanceOnFailure,

    -- ** InfrastructureConfigurationSummary
    infrastructureConfigurationSummary_arn,
    infrastructureConfigurationSummary_dateCreated,
    infrastructureConfigurationSummary_dateUpdated,
    infrastructureConfigurationSummary_description,
    infrastructureConfigurationSummary_instanceProfileName,
    infrastructureConfigurationSummary_instanceTypes,
    infrastructureConfigurationSummary_name,
    infrastructureConfigurationSummary_resourceTags,
    infrastructureConfigurationSummary_tags,

    -- ** InstanceBlockDeviceMapping
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_noDevice,
    instanceBlockDeviceMapping_virtualName,

    -- ** InstanceConfiguration
    instanceConfiguration_blockDeviceMappings,
    instanceConfiguration_image,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- ** LaunchPermissionConfiguration
    launchPermissionConfiguration_organizationArns,
    launchPermissionConfiguration_organizationalUnitArns,
    launchPermissionConfiguration_userGroups,
    launchPermissionConfiguration_userIds,

    -- ** LaunchTemplateConfiguration
    launchTemplateConfiguration_accountId,
    launchTemplateConfiguration_setDefaultVersion,
    launchTemplateConfiguration_launchTemplateId,

    -- ** Logging
    logging_s3Logs,

    -- ** OutputResources
    outputResources_amis,
    outputResources_containers,

    -- ** S3ExportConfiguration
    s3ExportConfiguration_s3Prefix,
    s3ExportConfiguration_roleName,
    s3ExportConfiguration_diskImageFormat,
    s3ExportConfiguration_s3Bucket,

    -- ** S3Logs
    s3Logs_s3BucketName,
    s3Logs_s3KeyPrefix,

    -- ** Schedule
    schedule_pipelineExecutionStartCondition,
    schedule_scheduleExpression,
    schedule_timezone,

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
