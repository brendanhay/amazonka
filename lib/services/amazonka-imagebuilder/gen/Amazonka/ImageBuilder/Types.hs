{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CallRateLimitExceededException,
    _ClientException,
    _ForbiddenException,
    _IdempotentParameterMismatchException,
    _InvalidPaginationTokenException,
    _InvalidParameterCombinationException,
    _InvalidParameterException,
    _InvalidParameterValueException,
    _InvalidRequestException,
    _InvalidVersionNumberException,
    _ResourceAlreadyExistsException,
    _ResourceDependencyException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ServiceException,
    _ServiceQuotaExceededException,
    _ServiceUnavailableException,

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
    additionalInstanceConfiguration_systemsManagerAgent,
    additionalInstanceConfiguration_userDataOverride,

    -- * Ami
    Ami (..),
    newAmi,
    ami_accountId,
    ami_description,
    ami_image,
    ami_name,
    ami_region,
    ami_state,

    -- * AmiDistributionConfiguration
    AmiDistributionConfiguration (..),
    newAmiDistributionConfiguration,
    amiDistributionConfiguration_amiTags,
    amiDistributionConfiguration_description,
    amiDistributionConfiguration_kmsKeyId,
    amiDistributionConfiguration_launchPermission,
    amiDistributionConfiguration_name,
    amiDistributionConfiguration_targetAccountIds,

    -- * Component
    Component (..),
    newComponent,
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
    componentState_reason,
    componentState_status,

    -- * ComponentSummary
    ComponentSummary (..),
    newComponentSummary,
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

    -- * ComponentVersion
    ComponentVersion (..),
    newComponentVersion,
    componentVersion_arn,
    componentVersion_dateCreated,
    componentVersion_description,
    componentVersion_name,
    componentVersion_owner,
    componentVersion_platform,
    componentVersion_supportedOsVersions,
    componentVersion_type,
    componentVersion_version,

    -- * Container
    Container (..),
    newContainer,
    container_imageUris,
    container_region,

    -- * ContainerDistributionConfiguration
    ContainerDistributionConfiguration (..),
    newContainerDistributionConfiguration,
    containerDistributionConfiguration_containerTags,
    containerDistributionConfiguration_description,
    containerDistributionConfiguration_targetRepository,

    -- * ContainerRecipe
    ContainerRecipe (..),
    newContainerRecipe,
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

    -- * ContainerRecipeSummary
    ContainerRecipeSummary (..),
    newContainerRecipeSummary,
    containerRecipeSummary_arn,
    containerRecipeSummary_containerType,
    containerRecipeSummary_dateCreated,
    containerRecipeSummary_name,
    containerRecipeSummary_owner,
    containerRecipeSummary_parentImage,
    containerRecipeSummary_platform,
    containerRecipeSummary_tags,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_amiDistributionConfiguration,
    distribution_containerDistributionConfiguration,
    distribution_fastLaunchConfigurations,
    distribution_launchTemplateConfigurations,
    distribution_licenseConfigurationArns,
    distribution_s3ExportConfiguration,
    distribution_region,

    -- * DistributionConfiguration
    DistributionConfiguration (..),
    newDistributionConfiguration,
    distributionConfiguration_arn,
    distributionConfiguration_dateCreated,
    distributionConfiguration_dateUpdated,
    distributionConfiguration_description,
    distributionConfiguration_distributions,
    distributionConfiguration_name,
    distributionConfiguration_tags,
    distributionConfiguration_timeoutMinutes,

    -- * DistributionConfigurationSummary
    DistributionConfigurationSummary (..),
    newDistributionConfigurationSummary,
    distributionConfigurationSummary_arn,
    distributionConfigurationSummary_dateCreated,
    distributionConfigurationSummary_dateUpdated,
    distributionConfigurationSummary_description,
    distributionConfigurationSummary_name,
    distributionConfigurationSummary_regions,
    distributionConfigurationSummary_tags,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_encrypted,
    ebsInstanceBlockDeviceSpecification_iops,
    ebsInstanceBlockDeviceSpecification_kmsKeyId,
    ebsInstanceBlockDeviceSpecification_snapshotId,
    ebsInstanceBlockDeviceSpecification_throughput,
    ebsInstanceBlockDeviceSpecification_volumeSize,
    ebsInstanceBlockDeviceSpecification_volumeType,

    -- * FastLaunchConfiguration
    FastLaunchConfiguration (..),
    newFastLaunchConfiguration,
    fastLaunchConfiguration_accountId,
    fastLaunchConfiguration_launchTemplate,
    fastLaunchConfiguration_maxParallelLaunches,
    fastLaunchConfiguration_snapshotConfiguration,
    fastLaunchConfiguration_enabled,

    -- * FastLaunchLaunchTemplateSpecification
    FastLaunchLaunchTemplateSpecification (..),
    newFastLaunchLaunchTemplateSpecification,
    fastLaunchLaunchTemplateSpecification_launchTemplateId,
    fastLaunchLaunchTemplateSpecification_launchTemplateName,
    fastLaunchLaunchTemplateSpecification_launchTemplateVersion,

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

    -- * ImagePackage
    ImagePackage (..),
    newImagePackage,
    imagePackage_packageName,
    imagePackage_packageVersion,

    -- * ImagePipeline
    ImagePipeline (..),
    newImagePipeline,
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

    -- * ImageRecipe
    ImageRecipe (..),
    newImageRecipe,
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

    -- * ImageRecipeSummary
    ImageRecipeSummary (..),
    newImageRecipeSummary,
    imageRecipeSummary_arn,
    imageRecipeSummary_dateCreated,
    imageRecipeSummary_name,
    imageRecipeSummary_owner,
    imageRecipeSummary_parentImage,
    imageRecipeSummary_platform,
    imageRecipeSummary_tags,

    -- * ImageState
    ImageState (..),
    newImageState,
    imageState_reason,
    imageState_status,

    -- * ImageSummary
    ImageSummary (..),
    newImageSummary,
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

    -- * ImageTestsConfiguration
    ImageTestsConfiguration (..),
    newImageTestsConfiguration,
    imageTestsConfiguration_imageTestsEnabled,
    imageTestsConfiguration_timeoutMinutes,

    -- * ImageVersion
    ImageVersion (..),
    newImageVersion,
    imageVersion_arn,
    imageVersion_buildType,
    imageVersion_dateCreated,
    imageVersion_name,
    imageVersion_osVersion,
    imageVersion_owner,
    imageVersion_platform,
    imageVersion_type,
    imageVersion_version,

    -- * InfrastructureConfiguration
    InfrastructureConfiguration (..),
    newInfrastructureConfiguration,
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

    -- * InfrastructureConfigurationSummary
    InfrastructureConfigurationSummary (..),
    newInfrastructureConfigurationSummary,
    infrastructureConfigurationSummary_arn,
    infrastructureConfigurationSummary_dateCreated,
    infrastructureConfigurationSummary_dateUpdated,
    infrastructureConfigurationSummary_description,
    infrastructureConfigurationSummary_instanceProfileName,
    infrastructureConfigurationSummary_instanceTypes,
    infrastructureConfigurationSummary_name,
    infrastructureConfigurationSummary_resourceTags,
    infrastructureConfigurationSummary_tags,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    newInstanceBlockDeviceMapping,
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_ebs,
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
    launchPermissionConfiguration_organizationArns,
    launchPermissionConfiguration_organizationalUnitArns,
    launchPermissionConfiguration_userGroups,
    launchPermissionConfiguration_userIds,

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
    outputResources_amis,
    outputResources_containers,

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
    s3Logs_s3BucketName,
    s3Logs_s3KeyPrefix,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_pipelineExecutionStartCondition,
    schedule_scheduleExpression,
    schedule_timezone,

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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You have exceeded the permitted request rate for the specific operation.
_CallRateLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CallRateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CallRateLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | These errors are usually caused by a client action, such as using an
-- action or resource on behalf of a user that doesn\'t have permissions to
-- use the action or resource, or specifying an invalid resource
-- identifier.
_ClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"
    Prelude.. Core.hasStatus 400

-- | You are not authorized to perform the requested operation.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | You have specified a client token for an operation using parameter
-- values that differ from a previous request that used the same client
-- token.
_IdempotentParameterMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | You have provided an invalid pagination token in your request.
_InvalidPaginationTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | You have specified two or more mutually exclusive parameters. Review the
-- error message for details.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"
    Prelude.. Core.hasStatus 400

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The value that you provided for the specified parameter is invalid.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | You have made a request for an action that is not supported by the
-- service.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Your version number is out of bounds or does not follow the required
-- syntax.
_InvalidVersionNumberException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVersionNumberException =
  Core._MatchServiceError
    defaultService
    "InvalidVersionNumberException"
    Prelude.. Core.hasStatus 400

-- | The resource that you are trying to create already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | You have attempted to mutate or delete a resource with a dependency that
-- prohibits this action. See the error message for more details.
_ResourceDependencyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceDependencyException =
  Core._MatchServiceError
    defaultService
    "ResourceDependencyException"
    Prelude.. Core.hasStatus 400

-- | The resource that you are trying to operate on is currently in use.
-- Review the message details and retry later.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400

-- | At least one of the resources referenced by your request does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | This exception is thrown when the service encounters an unrecoverable
-- exception.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | You have exceeded the number of permitted resources or operations for
-- this service. For service quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/imagebuilder.html#limits_imagebuilder EC2 Image Builder endpoints and quotas>.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The service is unable to process your request at this time.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503
