{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidParameterException,
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _CallRateLimitExceededException,
    _ForbiddenException,
    _InvalidParameterValueException,
    _InvalidVersionNumberException,
    _ServiceQuotaExceededException,
    _ServiceException,
    _ResourceDependencyException,
    _ServiceUnavailableException,
    _IdempotentParameterMismatchException,
    _ClientException,
    _ResourceNotFoundException,
    _InvalidParameterCombinationException,
    _InvalidPaginationTokenException,
    _ResourceInUseException,

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
    ami_image,
    ami_state,
    ami_accountId,
    ami_name,
    ami_region,
    ami_description,

    -- * AmiDistributionConfiguration
    AmiDistributionConfiguration (..),
    newAmiDistributionConfiguration,
    amiDistributionConfiguration_launchPermission,
    amiDistributionConfiguration_targetAccountIds,
    amiDistributionConfiguration_amiTags,
    amiDistributionConfiguration_name,
    amiDistributionConfiguration_kmsKeyId,
    amiDistributionConfiguration_description,

    -- * Component
    Component (..),
    newComponent,
    component_state,
    component_platform,
    component_arn,
    component_data,
    component_supportedOsVersions,
    component_encrypted,
    component_owner,
    component_dateCreated,
    component_name,
    component_kmsKeyId,
    component_version,
    component_parameters,
    component_changeDescription,
    component_type,
    component_description,
    component_tags,

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
    componentSummary_state,
    componentSummary_platform,
    componentSummary_arn,
    componentSummary_supportedOsVersions,
    componentSummary_owner,
    componentSummary_dateCreated,
    componentSummary_name,
    componentSummary_version,
    componentSummary_changeDescription,
    componentSummary_type,
    componentSummary_description,
    componentSummary_tags,

    -- * ComponentVersion
    ComponentVersion (..),
    newComponentVersion,
    componentVersion_platform,
    componentVersion_arn,
    componentVersion_supportedOsVersions,
    componentVersion_owner,
    componentVersion_dateCreated,
    componentVersion_name,
    componentVersion_version,
    componentVersion_type,
    componentVersion_description,

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
    containerRecipe_components,
    containerRecipe_containerType,
    containerRecipe_platform,
    containerRecipe_dockerfileTemplateData,
    containerRecipe_arn,
    containerRecipe_workingDirectory,
    containerRecipe_parentImage,
    containerRecipe_encrypted,
    containerRecipe_owner,
    containerRecipe_dateCreated,
    containerRecipe_name,
    containerRecipe_kmsKeyId,
    containerRecipe_version,
    containerRecipe_targetRepository,
    containerRecipe_description,
    containerRecipe_tags,
    containerRecipe_instanceConfiguration,

    -- * ContainerRecipeSummary
    ContainerRecipeSummary (..),
    newContainerRecipeSummary,
    containerRecipeSummary_containerType,
    containerRecipeSummary_platform,
    containerRecipeSummary_arn,
    containerRecipeSummary_parentImage,
    containerRecipeSummary_owner,
    containerRecipeSummary_dateCreated,
    containerRecipeSummary_name,
    containerRecipeSummary_tags,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_amiDistributionConfiguration,
    distribution_launchTemplateConfigurations,
    distribution_licenseConfigurationArns,
    distribution_containerDistributionConfiguration,
    distribution_region,

    -- * DistributionConfiguration
    DistributionConfiguration (..),
    newDistributionConfiguration,
    distributionConfiguration_arn,
    distributionConfiguration_dateUpdated,
    distributionConfiguration_dateCreated,
    distributionConfiguration_name,
    distributionConfiguration_description,
    distributionConfiguration_distributions,
    distributionConfiguration_tags,
    distributionConfiguration_timeoutMinutes,

    -- * DistributionConfigurationSummary
    DistributionConfigurationSummary (..),
    newDistributionConfigurationSummary,
    distributionConfigurationSummary_arn,
    distributionConfigurationSummary_regions,
    distributionConfigurationSummary_dateUpdated,
    distributionConfigurationSummary_dateCreated,
    distributionConfigurationSummary_name,
    distributionConfigurationSummary_description,
    distributionConfigurationSummary_tags,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_throughput,
    ebsInstanceBlockDeviceSpecification_volumeSize,
    ebsInstanceBlockDeviceSpecification_iops,
    ebsInstanceBlockDeviceSpecification_encrypted,
    ebsInstanceBlockDeviceSpecification_kmsKeyId,
    ebsInstanceBlockDeviceSpecification_volumeType,
    ebsInstanceBlockDeviceSpecification_snapshotId,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * Image
    Image (..),
    newImage,
    image_imageRecipe,
    image_state,
    image_platform,
    image_imageTestsConfiguration,
    image_arn,
    image_enhancedImageMetadataEnabled,
    image_dateCreated,
    image_containerRecipe,
    image_name,
    image_infrastructureConfiguration,
    image_version,
    image_distributionConfiguration,
    image_sourcePipelineArn,
    image_type,
    image_sourcePipelineName,
    image_outputResources,
    image_osVersion,
    image_tags,

    -- * ImagePackage
    ImagePackage (..),
    newImagePackage,
    imagePackage_packageName,
    imagePackage_packageVersion,

    -- * ImagePipeline
    ImagePipeline (..),
    newImagePipeline,
    imagePipeline_status,
    imagePipeline_dateLastRun,
    imagePipeline_platform,
    imagePipeline_infrastructureConfigurationArn,
    imagePipeline_containerRecipeArn,
    imagePipeline_imageTestsConfiguration,
    imagePipeline_dateNextRun,
    imagePipeline_arn,
    imagePipeline_schedule,
    imagePipeline_enhancedImageMetadataEnabled,
    imagePipeline_dateUpdated,
    imagePipeline_dateCreated,
    imagePipeline_name,
    imagePipeline_distributionConfigurationArn,
    imagePipeline_imageRecipeArn,
    imagePipeline_description,
    imagePipeline_tags,

    -- * ImageRecipe
    ImageRecipe (..),
    newImageRecipe,
    imageRecipe_components,
    imageRecipe_platform,
    imageRecipe_arn,
    imageRecipe_additionalInstanceConfiguration,
    imageRecipe_workingDirectory,
    imageRecipe_parentImage,
    imageRecipe_owner,
    imageRecipe_dateCreated,
    imageRecipe_name,
    imageRecipe_version,
    imageRecipe_type,
    imageRecipe_blockDeviceMappings,
    imageRecipe_description,
    imageRecipe_tags,

    -- * ImageRecipeSummary
    ImageRecipeSummary (..),
    newImageRecipeSummary,
    imageRecipeSummary_platform,
    imageRecipeSummary_arn,
    imageRecipeSummary_parentImage,
    imageRecipeSummary_owner,
    imageRecipeSummary_dateCreated,
    imageRecipeSummary_name,
    imageRecipeSummary_tags,

    -- * ImageState
    ImageState (..),
    newImageState,
    imageState_status,
    imageState_reason,

    -- * ImageSummary
    ImageSummary (..),
    newImageSummary,
    imageSummary_state,
    imageSummary_platform,
    imageSummary_arn,
    imageSummary_owner,
    imageSummary_dateCreated,
    imageSummary_name,
    imageSummary_version,
    imageSummary_type,
    imageSummary_outputResources,
    imageSummary_osVersion,
    imageSummary_tags,

    -- * ImageTestsConfiguration
    ImageTestsConfiguration (..),
    newImageTestsConfiguration,
    imageTestsConfiguration_timeoutMinutes,
    imageTestsConfiguration_imageTestsEnabled,

    -- * ImageVersion
    ImageVersion (..),
    newImageVersion,
    imageVersion_platform,
    imageVersion_arn,
    imageVersion_owner,
    imageVersion_dateCreated,
    imageVersion_name,
    imageVersion_version,
    imageVersion_type,
    imageVersion_osVersion,

    -- * InfrastructureConfiguration
    InfrastructureConfiguration (..),
    newInfrastructureConfiguration,
    infrastructureConfiguration_securityGroupIds,
    infrastructureConfiguration_snsTopicArn,
    infrastructureConfiguration_instanceTypes,
    infrastructureConfiguration_arn,
    infrastructureConfiguration_keyPair,
    infrastructureConfiguration_resourceTags,
    infrastructureConfiguration_subnetId,
    infrastructureConfiguration_dateUpdated,
    infrastructureConfiguration_dateCreated,
    infrastructureConfiguration_instanceMetadataOptions,
    infrastructureConfiguration_name,
    infrastructureConfiguration_instanceProfileName,
    infrastructureConfiguration_logging,
    infrastructureConfiguration_description,
    infrastructureConfiguration_tags,
    infrastructureConfiguration_terminateInstanceOnFailure,

    -- * InfrastructureConfigurationSummary
    InfrastructureConfigurationSummary (..),
    newInfrastructureConfigurationSummary,
    infrastructureConfigurationSummary_instanceTypes,
    infrastructureConfigurationSummary_arn,
    infrastructureConfigurationSummary_resourceTags,
    infrastructureConfigurationSummary_dateUpdated,
    infrastructureConfigurationSummary_dateCreated,
    infrastructureConfigurationSummary_name,
    infrastructureConfigurationSummary_instanceProfileName,
    infrastructureConfigurationSummary_description,
    infrastructureConfigurationSummary_tags,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    newInstanceBlockDeviceMapping,
    instanceBlockDeviceMapping_virtualName,
    instanceBlockDeviceMapping_noDevice,
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,

    -- * InstanceConfiguration
    InstanceConfiguration (..),
    newInstanceConfiguration,
    instanceConfiguration_image,
    instanceConfiguration_blockDeviceMappings,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    newInstanceMetadataOptions,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- * LaunchPermissionConfiguration
    LaunchPermissionConfiguration (..),
    newLaunchPermissionConfiguration,
    launchPermissionConfiguration_userIds,
    launchPermissionConfiguration_userGroups,

    -- * LaunchTemplateConfiguration
    LaunchTemplateConfiguration (..),
    newLaunchTemplateConfiguration,
    launchTemplateConfiguration_setDefaultVersion,
    launchTemplateConfiguration_accountId,
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

    -- * S3Logs
    S3Logs (..),
    newS3Logs,
    s3Logs_s3KeyPrefix,
    s3Logs_s3BucketName,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_scheduleExpression,
    schedule_pipelineExecutionStartCondition,
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
import Amazonka.ImageBuilder.Types.AdditionalInstanceConfiguration
import Amazonka.ImageBuilder.Types.Ami
import Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
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
import Amazonka.ImageBuilder.Types.Distribution
import Amazonka.ImageBuilder.Types.DistributionConfiguration
import Amazonka.ImageBuilder.Types.DistributionConfigurationSummary
import Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification
import Amazonka.ImageBuilder.Types.EbsVolumeType
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
import Amazonka.ImageBuilder.Types.S3Logs
import Amazonka.ImageBuilder.Types.Schedule
import Amazonka.ImageBuilder.Types.SystemsManagerAgent
import Amazonka.ImageBuilder.Types.TargetContainerRepository
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon EC2 Image Builder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ImageBuilder",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "imagebuilder",
      Core._serviceSigningName = "imagebuilder",
      Core._serviceVersion = "2019-12-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ImageBuilder",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | You have made a request for an action that is not supported by the
-- service.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The resource that you are trying to create already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the permitted request rate for the specific operation.
_CallRateLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CallRateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CallRateLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | You are not authorized to perform the requested operation.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The value that you provided for the specified parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | Your version number is out of bounds or does not follow the required
-- syntax.
_InvalidVersionNumberException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVersionNumberException =
  Core._MatchServiceError
    defaultService
    "InvalidVersionNumberException"
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

-- | This exception is thrown when the service encounters an unrecoverable
-- exception.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | You have attempted to mutate or delete a resource with a dependency that
-- prohibits this action. See the error message for more details.
_ResourceDependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDependencyException =
  Core._MatchServiceError
    defaultService
    "ResourceDependencyException"
    Prelude.. Core.hasStatus 400

-- | The service is unable to process your request at this time.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | You have specified a client token for an operation using parameter
-- values that differ from a previous request that used the same client
-- token.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
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

-- | You have provided an invalid pagination token in your request.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | The resource that you are trying to operate on is currently in use.
-- Review the message details and retry later.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400
