{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _ClientException,
    _ResourceDependencyException,
    _ServiceQuotaExceededException,
    _InvalidVersionNumberException,
    _InvalidPaginationTokenException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _InvalidParameterCombinationException,
    _CallRateLimitExceededException,
    _ResourceInUseException,
    _ForbiddenException,
    _ServiceException,
    _InvalidRequestException,
    _InvalidParameterValueException,
    _IdempotentParameterMismatchException,
    _InvalidParameterException,

    -- * BuildType
    BuildType (..),

    -- * ComponentFormat
    ComponentFormat (..),

    -- * ComponentStatus
    ComponentStatus (..),

    -- * ComponentType
    ComponentType (..),

    -- * ContainerRepositoryService
    ContainerRepositoryService (..),

    -- * ContainerType
    ContainerType (..),

    -- * DiskImageFormat
    DiskImageFormat (..),

    -- * EbsVolumeType
    EbsVolumeType (..),

    -- * ImageStatus
    ImageStatus (..),

    -- * ImageType
    ImageType (..),

    -- * Ownership
    Ownership (..),

    -- * PipelineExecutionStartCondition
    PipelineExecutionStartCondition (..),

    -- * PipelineStatus
    PipelineStatus (..),

    -- * Platform
    Platform (..),

    -- * AdditionalInstanceConfiguration
    AdditionalInstanceConfiguration (..),
    newAdditionalInstanceConfiguration,
    additionalInstanceConfiguration_userDataOverride,
    additionalInstanceConfiguration_systemsManagerAgent,

    -- * Ami
    Ami (..),
    newAmi,
    ami_name,
    ami_state,
    ami_description,
    ami_region,
    ami_accountId,
    ami_image,

    -- * AmiDistributionConfiguration
    AmiDistributionConfiguration (..),
    newAmiDistributionConfiguration,
    amiDistributionConfiguration_launchPermission,
    amiDistributionConfiguration_name,
    amiDistributionConfiguration_description,
    amiDistributionConfiguration_targetAccountIds,
    amiDistributionConfiguration_amiTags,
    amiDistributionConfiguration_kmsKeyId,

    -- * Component
    Component (..),
    newComponent,
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

    -- * ComponentConfiguration
    ComponentConfiguration (..),
    newComponentConfiguration,
    componentConfiguration_parameters,
    componentConfiguration_componentArn,

    -- * ComponentParameter
    ComponentParameter (..),
    newComponentParameter,
    componentParameter_name,
    componentParameter_value,

    -- * ComponentParameterDetail
    ComponentParameterDetail (..),
    newComponentParameterDetail,
    componentParameterDetail_defaultValue,
    componentParameterDetail_description,
    componentParameterDetail_name,
    componentParameterDetail_type,

    -- * ComponentState
    ComponentState (..),
    newComponentState,
    componentState_status,
    componentState_reason,

    -- * ComponentSummary
    ComponentSummary (..),
    newComponentSummary,
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

    -- * ComponentVersion
    ComponentVersion (..),
    newComponentVersion,
    componentVersion_name,
    componentVersion_type,
    componentVersion_arn,
    componentVersion_owner,
    componentVersion_description,
    componentVersion_platform,
    componentVersion_dateCreated,
    componentVersion_supportedOsVersions,
    componentVersion_version,

    -- * Container
    Container (..),
    newContainer,
    container_region,
    container_imageUris,

    -- * ContainerDistributionConfiguration
    ContainerDistributionConfiguration (..),
    newContainerDistributionConfiguration,
    containerDistributionConfiguration_containerTags,
    containerDistributionConfiguration_description,
    containerDistributionConfiguration_targetRepository,

    -- * ContainerRecipe
    ContainerRecipe (..),
    newContainerRecipe,
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

    -- * ContainerRecipeSummary
    ContainerRecipeSummary (..),
    newContainerRecipeSummary,
    containerRecipeSummary_tags,
    containerRecipeSummary_name,
    containerRecipeSummary_containerType,
    containerRecipeSummary_parentImage,
    containerRecipeSummary_arn,
    containerRecipeSummary_owner,
    containerRecipeSummary_platform,
    containerRecipeSummary_dateCreated,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_amiDistributionConfiguration,
    distribution_licenseConfigurationArns,
    distribution_s3ExportConfiguration,
    distribution_fastLaunchConfigurations,
    distribution_launchTemplateConfigurations,
    distribution_containerDistributionConfiguration,
    distribution_region,

    -- * DistributionConfiguration
    DistributionConfiguration (..),
    newDistributionConfiguration,
    distributionConfiguration_tags,
    distributionConfiguration_name,
    distributionConfiguration_arn,
    distributionConfiguration_description,
    distributionConfiguration_dateUpdated,
    distributionConfiguration_dateCreated,
    distributionConfiguration_distributions,
    distributionConfiguration_timeoutMinutes,

    -- * DistributionConfigurationSummary
    DistributionConfigurationSummary (..),
    newDistributionConfigurationSummary,
    distributionConfigurationSummary_tags,
    distributionConfigurationSummary_name,
    distributionConfigurationSummary_regions,
    distributionConfigurationSummary_arn,
    distributionConfigurationSummary_description,
    distributionConfigurationSummary_dateUpdated,
    distributionConfigurationSummary_dateCreated,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_snapshotId,
    ebsInstanceBlockDeviceSpecification_volumeType,
    ebsInstanceBlockDeviceSpecification_volumeSize,
    ebsInstanceBlockDeviceSpecification_encrypted,
    ebsInstanceBlockDeviceSpecification_kmsKeyId,
    ebsInstanceBlockDeviceSpecification_throughput,
    ebsInstanceBlockDeviceSpecification_iops,

    -- * FastLaunchConfiguration
    FastLaunchConfiguration (..),
    newFastLaunchConfiguration,
    fastLaunchConfiguration_launchTemplate,
    fastLaunchConfiguration_snapshotConfiguration,
    fastLaunchConfiguration_accountId,
    fastLaunchConfiguration_maxParallelLaunches,
    fastLaunchConfiguration_enabled,

    -- * FastLaunchLaunchTemplateSpecification
    FastLaunchLaunchTemplateSpecification (..),
    newFastLaunchLaunchTemplateSpecification,
    fastLaunchLaunchTemplateSpecification_launchTemplateVersion,
    fastLaunchLaunchTemplateSpecification_launchTemplateId,
    fastLaunchLaunchTemplateSpecification_launchTemplateName,

    -- * FastLaunchSnapshotConfiguration
    FastLaunchSnapshotConfiguration (..),
    newFastLaunchSnapshotConfiguration,
    fastLaunchSnapshotConfiguration_targetResourceCount,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * Image
    Image (..),
    newImage,
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

    -- * ImagePackage
    ImagePackage (..),
    newImagePackage,
    imagePackage_packageName,
    imagePackage_packageVersion,

    -- * ImagePipeline
    ImagePipeline (..),
    newImagePipeline,
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

    -- * ImageRecipe
    ImageRecipe (..),
    newImageRecipe,
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

    -- * ImageRecipeSummary
    ImageRecipeSummary (..),
    newImageRecipeSummary,
    imageRecipeSummary_tags,
    imageRecipeSummary_name,
    imageRecipeSummary_parentImage,
    imageRecipeSummary_arn,
    imageRecipeSummary_owner,
    imageRecipeSummary_platform,
    imageRecipeSummary_dateCreated,

    -- * ImageState
    ImageState (..),
    newImageState,
    imageState_status,
    imageState_reason,

    -- * ImageSummary
    ImageSummary (..),
    newImageSummary,
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

    -- * ImageTestsConfiguration
    ImageTestsConfiguration (..),
    newImageTestsConfiguration,
    imageTestsConfiguration_imageTestsEnabled,
    imageTestsConfiguration_timeoutMinutes,

    -- * ImageVersion
    ImageVersion (..),
    newImageVersion,
    imageVersion_name,
    imageVersion_type,
    imageVersion_arn,
    imageVersion_osVersion,
    imageVersion_owner,
    imageVersion_platform,
    imageVersion_dateCreated,
    imageVersion_buildType,
    imageVersion_version,

    -- * InfrastructureConfiguration
    InfrastructureConfiguration (..),
    newInfrastructureConfiguration,
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

    -- * InfrastructureConfigurationSummary
    InfrastructureConfigurationSummary (..),
    newInfrastructureConfigurationSummary,
    infrastructureConfigurationSummary_tags,
    infrastructureConfigurationSummary_name,
    infrastructureConfigurationSummary_instanceProfileName,
    infrastructureConfigurationSummary_instanceTypes,
    infrastructureConfigurationSummary_arn,
    infrastructureConfigurationSummary_description,
    infrastructureConfigurationSummary_resourceTags,
    infrastructureConfigurationSummary_dateUpdated,
    infrastructureConfigurationSummary_dateCreated,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    newInstanceBlockDeviceMapping,
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_noDevice,
    instanceBlockDeviceMapping_virtualName,

    -- * InstanceConfiguration
    InstanceConfiguration (..),
    newInstanceConfiguration,
    instanceConfiguration_blockDeviceMappings,
    instanceConfiguration_image,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    newInstanceMetadataOptions,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- * LaunchPermissionConfiguration
    LaunchPermissionConfiguration (..),
    newLaunchPermissionConfiguration,
    launchPermissionConfiguration_userGroups,
    launchPermissionConfiguration_organizationArns,
    launchPermissionConfiguration_userIds,
    launchPermissionConfiguration_organizationalUnitArns,

    -- * LaunchTemplateConfiguration
    LaunchTemplateConfiguration (..),
    newLaunchTemplateConfiguration,
    launchTemplateConfiguration_accountId,
    launchTemplateConfiguration_setDefaultVersion,
    launchTemplateConfiguration_launchTemplateId,

    -- * Logging
    Logging (..),
    newLogging,
    logging_s3Logs,

    -- * OutputResources
    OutputResources (..),
    newOutputResources,
    outputResources_containers,
    outputResources_amis,

    -- * S3ExportConfiguration
    S3ExportConfiguration (..),
    newS3ExportConfiguration,
    s3ExportConfiguration_s3Prefix,
    s3ExportConfiguration_roleName,
    s3ExportConfiguration_diskImageFormat,
    s3ExportConfiguration_s3Bucket,

    -- * S3Logs
    S3Logs (..),
    newS3Logs,
    s3Logs_s3KeyPrefix,
    s3Logs_s3BucketName,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_timezone,
    schedule_scheduleExpression,
    schedule_pipelineExecutionStartCondition,

    -- * SystemsManagerAgent
    SystemsManagerAgent (..),
    newSystemsManagerAgent,
    systemsManagerAgent_uninstallAfterBuild,

    -- * TargetContainerRepository
    TargetContainerRepository (..),
    newTargetContainerRepository,
    targetContainerRepository_service,
    targetContainerRepository_repositoryName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types.AdditionalInstanceConfiguration
import Amazonka.ImageBuilder.Types.Ami
import Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
import Amazonka.ImageBuilder.Types.BuildType
import Amazonka.ImageBuilder.Types.Component
import Amazonka.ImageBuilder.Types.ComponentConfiguration
import Amazonka.ImageBuilder.Types.ComponentFormat
import Amazonka.ImageBuilder.Types.ComponentParameter
import Amazonka.ImageBuilder.Types.ComponentParameterDetail
import Amazonka.ImageBuilder.Types.ComponentState
import Amazonka.ImageBuilder.Types.ComponentStatus
import Amazonka.ImageBuilder.Types.ComponentSummary
import Amazonka.ImageBuilder.Types.ComponentType
import Amazonka.ImageBuilder.Types.ComponentVersion
import Amazonka.ImageBuilder.Types.Container
import Amazonka.ImageBuilder.Types.ContainerDistributionConfiguration
import Amazonka.ImageBuilder.Types.ContainerRecipe
import Amazonka.ImageBuilder.Types.ContainerRecipeSummary
import Amazonka.ImageBuilder.Types.ContainerRepositoryService
import Amazonka.ImageBuilder.Types.ContainerType
import Amazonka.ImageBuilder.Types.DiskImageFormat
import Amazonka.ImageBuilder.Types.Distribution
import Amazonka.ImageBuilder.Types.DistributionConfiguration
import Amazonka.ImageBuilder.Types.DistributionConfigurationSummary
import Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification
import Amazonka.ImageBuilder.Types.EbsVolumeType
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
import Amazonka.ImageBuilder.Types.ImageStatus
import Amazonka.ImageBuilder.Types.ImageSummary
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.ImageType
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
import Amazonka.ImageBuilder.Types.Ownership
import Amazonka.ImageBuilder.Types.PipelineExecutionStartCondition
import Amazonka.ImageBuilder.Types.PipelineStatus
import Amazonka.ImageBuilder.Types.Platform
import Amazonka.ImageBuilder.Types.S3ExportConfiguration
import Amazonka.ImageBuilder.Types.S3Logs
import Amazonka.ImageBuilder.Types.Schedule
import Amazonka.ImageBuilder.Types.SystemsManagerAgent
import Amazonka.ImageBuilder.Types.TargetContainerRepository
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon EC2 Image Builder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ImageBuilder",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "imagebuilder",
      Core.signingName = "imagebuilder",
      Core.version = "2019-12-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ImageBuilder",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource that you are trying to create already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | These errors are usually caused by a client action, such as using an
-- action or resource on behalf of a user that doesn\'t have permissions to
-- use the action or resource, or specifying an invalid resource
-- identifier.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"
    Prelude.. Core.hasStatus 400

-- | You have attempted to mutate or delete a resource with a dependency that
-- prohibits this action. See the error message for more details.
_ResourceDependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDependencyException =
  Core._MatchServiceError
    defaultService
    "ResourceDependencyException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the number of permitted resources or operations for
-- this service. For service quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/imagebuilder.html#limits_imagebuilder EC2 Image Builder endpoints and quotas>.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Your version number is out of bounds or does not follow the required
-- syntax.
_InvalidVersionNumberException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVersionNumberException =
  Core._MatchServiceError
    defaultService
    "InvalidVersionNumberException"
    Prelude.. Core.hasStatus 400

-- | You have provided an invalid pagination token in your request.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | The service is unable to process your request at this time.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | At least one of the resources referenced by your request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have specified two or more mutually exclusive parameters. Review the
-- error message for details.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the permitted request rate for the specific operation.
_CallRateLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CallRateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CallRateLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The resource that you are trying to operate on is currently in use.
-- Review the message details and retry later.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400

-- | You are not authorized to perform the requested operation.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | This exception is thrown when the service encounters an unrecoverable
-- exception.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | You have made a request for an action that is not supported by the
-- service.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The value that you provided for the specified parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | You have specified a client token for an operation using parameter
-- values that differ from a previous request that used the same client
-- token.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400
