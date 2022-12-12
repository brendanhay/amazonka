{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ImageBuilder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- EC2 Image Builder is a fully managed Amazon Web Services service that
-- makes it easier to automate the creation, management, and deployment of
-- customized, secure, and up-to-date \"golden\" server images that are
-- pre-installed and pre-configured with software and settings to meet
-- specific IT standards.
module Amazonka.ImageBuilder
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CallRateLimitExceededException
    _CallRateLimitExceededException,

    -- ** ClientException
    _ClientException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidVersionNumberException
    _InvalidVersionNumberException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceDependencyException
    _ResourceDependencyException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceException
    _ServiceException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelImageCreation
    CancelImageCreation (CancelImageCreation'),
    newCancelImageCreation,
    CancelImageCreationResponse (CancelImageCreationResponse'),
    newCancelImageCreationResponse,

    -- ** CreateComponent
    CreateComponent (CreateComponent'),
    newCreateComponent,
    CreateComponentResponse (CreateComponentResponse'),
    newCreateComponentResponse,

    -- ** CreateContainerRecipe
    CreateContainerRecipe (CreateContainerRecipe'),
    newCreateContainerRecipe,
    CreateContainerRecipeResponse (CreateContainerRecipeResponse'),
    newCreateContainerRecipeResponse,

    -- ** CreateDistributionConfiguration
    CreateDistributionConfiguration (CreateDistributionConfiguration'),
    newCreateDistributionConfiguration,
    CreateDistributionConfigurationResponse (CreateDistributionConfigurationResponse'),
    newCreateDistributionConfigurationResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** CreateImagePipeline
    CreateImagePipeline (CreateImagePipeline'),
    newCreateImagePipeline,
    CreateImagePipelineResponse (CreateImagePipelineResponse'),
    newCreateImagePipelineResponse,

    -- ** CreateImageRecipe
    CreateImageRecipe (CreateImageRecipe'),
    newCreateImageRecipe,
    CreateImageRecipeResponse (CreateImageRecipeResponse'),
    newCreateImageRecipeResponse,

    -- ** CreateInfrastructureConfiguration
    CreateInfrastructureConfiguration (CreateInfrastructureConfiguration'),
    newCreateInfrastructureConfiguration,
    CreateInfrastructureConfigurationResponse (CreateInfrastructureConfigurationResponse'),
    newCreateInfrastructureConfigurationResponse,

    -- ** DeleteComponent
    DeleteComponent (DeleteComponent'),
    newDeleteComponent,
    DeleteComponentResponse (DeleteComponentResponse'),
    newDeleteComponentResponse,

    -- ** DeleteContainerRecipe
    DeleteContainerRecipe (DeleteContainerRecipe'),
    newDeleteContainerRecipe,
    DeleteContainerRecipeResponse (DeleteContainerRecipeResponse'),
    newDeleteContainerRecipeResponse,

    -- ** DeleteDistributionConfiguration
    DeleteDistributionConfiguration (DeleteDistributionConfiguration'),
    newDeleteDistributionConfiguration,
    DeleteDistributionConfigurationResponse (DeleteDistributionConfigurationResponse'),
    newDeleteDistributionConfigurationResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteImagePipeline
    DeleteImagePipeline (DeleteImagePipeline'),
    newDeleteImagePipeline,
    DeleteImagePipelineResponse (DeleteImagePipelineResponse'),
    newDeleteImagePipelineResponse,

    -- ** DeleteImageRecipe
    DeleteImageRecipe (DeleteImageRecipe'),
    newDeleteImageRecipe,
    DeleteImageRecipeResponse (DeleteImageRecipeResponse'),
    newDeleteImageRecipeResponse,

    -- ** DeleteInfrastructureConfiguration
    DeleteInfrastructureConfiguration (DeleteInfrastructureConfiguration'),
    newDeleteInfrastructureConfiguration,
    DeleteInfrastructureConfigurationResponse (DeleteInfrastructureConfigurationResponse'),
    newDeleteInfrastructureConfigurationResponse,

    -- ** GetComponent
    GetComponent (GetComponent'),
    newGetComponent,
    GetComponentResponse (GetComponentResponse'),
    newGetComponentResponse,

    -- ** GetComponentPolicy
    GetComponentPolicy (GetComponentPolicy'),
    newGetComponentPolicy,
    GetComponentPolicyResponse (GetComponentPolicyResponse'),
    newGetComponentPolicyResponse,

    -- ** GetContainerRecipe
    GetContainerRecipe (GetContainerRecipe'),
    newGetContainerRecipe,
    GetContainerRecipeResponse (GetContainerRecipeResponse'),
    newGetContainerRecipeResponse,

    -- ** GetContainerRecipePolicy
    GetContainerRecipePolicy (GetContainerRecipePolicy'),
    newGetContainerRecipePolicy,
    GetContainerRecipePolicyResponse (GetContainerRecipePolicyResponse'),
    newGetContainerRecipePolicyResponse,

    -- ** GetDistributionConfiguration
    GetDistributionConfiguration (GetDistributionConfiguration'),
    newGetDistributionConfiguration,
    GetDistributionConfigurationResponse (GetDistributionConfigurationResponse'),
    newGetDistributionConfigurationResponse,

    -- ** GetImage
    GetImage (GetImage'),
    newGetImage,
    GetImageResponse (GetImageResponse'),
    newGetImageResponse,

    -- ** GetImagePipeline
    GetImagePipeline (GetImagePipeline'),
    newGetImagePipeline,
    GetImagePipelineResponse (GetImagePipelineResponse'),
    newGetImagePipelineResponse,

    -- ** GetImagePolicy
    GetImagePolicy (GetImagePolicy'),
    newGetImagePolicy,
    GetImagePolicyResponse (GetImagePolicyResponse'),
    newGetImagePolicyResponse,

    -- ** GetImageRecipe
    GetImageRecipe (GetImageRecipe'),
    newGetImageRecipe,
    GetImageRecipeResponse (GetImageRecipeResponse'),
    newGetImageRecipeResponse,

    -- ** GetImageRecipePolicy
    GetImageRecipePolicy (GetImageRecipePolicy'),
    newGetImageRecipePolicy,
    GetImageRecipePolicyResponse (GetImageRecipePolicyResponse'),
    newGetImageRecipePolicyResponse,

    -- ** GetInfrastructureConfiguration
    GetInfrastructureConfiguration (GetInfrastructureConfiguration'),
    newGetInfrastructureConfiguration,
    GetInfrastructureConfigurationResponse (GetInfrastructureConfigurationResponse'),
    newGetInfrastructureConfigurationResponse,

    -- ** ImportComponent
    ImportComponent (ImportComponent'),
    newImportComponent,
    ImportComponentResponse (ImportComponentResponse'),
    newImportComponentResponse,

    -- ** ImportVmImage
    ImportVmImage (ImportVmImage'),
    newImportVmImage,
    ImportVmImageResponse (ImportVmImageResponse'),
    newImportVmImageResponse,

    -- ** ListComponentBuildVersions
    ListComponentBuildVersions (ListComponentBuildVersions'),
    newListComponentBuildVersions,
    ListComponentBuildVersionsResponse (ListComponentBuildVersionsResponse'),
    newListComponentBuildVersionsResponse,

    -- ** ListComponents
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** ListContainerRecipes
    ListContainerRecipes (ListContainerRecipes'),
    newListContainerRecipes,
    ListContainerRecipesResponse (ListContainerRecipesResponse'),
    newListContainerRecipesResponse,

    -- ** ListDistributionConfigurations
    ListDistributionConfigurations (ListDistributionConfigurations'),
    newListDistributionConfigurations,
    ListDistributionConfigurationsResponse (ListDistributionConfigurationsResponse'),
    newListDistributionConfigurationsResponse,

    -- ** ListImageBuildVersions
    ListImageBuildVersions (ListImageBuildVersions'),
    newListImageBuildVersions,
    ListImageBuildVersionsResponse (ListImageBuildVersionsResponse'),
    newListImageBuildVersionsResponse,

    -- ** ListImagePackages
    ListImagePackages (ListImagePackages'),
    newListImagePackages,
    ListImagePackagesResponse (ListImagePackagesResponse'),
    newListImagePackagesResponse,

    -- ** ListImagePipelineImages
    ListImagePipelineImages (ListImagePipelineImages'),
    newListImagePipelineImages,
    ListImagePipelineImagesResponse (ListImagePipelineImagesResponse'),
    newListImagePipelineImagesResponse,

    -- ** ListImagePipelines
    ListImagePipelines (ListImagePipelines'),
    newListImagePipelines,
    ListImagePipelinesResponse (ListImagePipelinesResponse'),
    newListImagePipelinesResponse,

    -- ** ListImageRecipes
    ListImageRecipes (ListImageRecipes'),
    newListImageRecipes,
    ListImageRecipesResponse (ListImageRecipesResponse'),
    newListImageRecipesResponse,

    -- ** ListImages
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** ListInfrastructureConfigurations
    ListInfrastructureConfigurations (ListInfrastructureConfigurations'),
    newListInfrastructureConfigurations,
    ListInfrastructureConfigurationsResponse (ListInfrastructureConfigurationsResponse'),
    newListInfrastructureConfigurationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutComponentPolicy
    PutComponentPolicy (PutComponentPolicy'),
    newPutComponentPolicy,
    PutComponentPolicyResponse (PutComponentPolicyResponse'),
    newPutComponentPolicyResponse,

    -- ** PutContainerRecipePolicy
    PutContainerRecipePolicy (PutContainerRecipePolicy'),
    newPutContainerRecipePolicy,
    PutContainerRecipePolicyResponse (PutContainerRecipePolicyResponse'),
    newPutContainerRecipePolicyResponse,

    -- ** PutImagePolicy
    PutImagePolicy (PutImagePolicy'),
    newPutImagePolicy,
    PutImagePolicyResponse (PutImagePolicyResponse'),
    newPutImagePolicyResponse,

    -- ** PutImageRecipePolicy
    PutImageRecipePolicy (PutImageRecipePolicy'),
    newPutImageRecipePolicy,
    PutImageRecipePolicyResponse (PutImageRecipePolicyResponse'),
    newPutImageRecipePolicyResponse,

    -- ** StartImagePipelineExecution
    StartImagePipelineExecution (StartImagePipelineExecution'),
    newStartImagePipelineExecution,
    StartImagePipelineExecutionResponse (StartImagePipelineExecutionResponse'),
    newStartImagePipelineExecutionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDistributionConfiguration
    UpdateDistributionConfiguration (UpdateDistributionConfiguration'),
    newUpdateDistributionConfiguration,
    UpdateDistributionConfigurationResponse (UpdateDistributionConfigurationResponse'),
    newUpdateDistributionConfigurationResponse,

    -- ** UpdateImagePipeline
    UpdateImagePipeline (UpdateImagePipeline'),
    newUpdateImagePipeline,
    UpdateImagePipelineResponse (UpdateImagePipelineResponse'),
    newUpdateImagePipelineResponse,

    -- ** UpdateInfrastructureConfiguration
    UpdateInfrastructureConfiguration (UpdateInfrastructureConfiguration'),
    newUpdateInfrastructureConfiguration,
    UpdateInfrastructureConfigurationResponse (UpdateInfrastructureConfigurationResponse'),
    newUpdateInfrastructureConfigurationResponse,

    -- * Types

    -- ** BuildType
    BuildType (..),

    -- ** ComponentFormat
    ComponentFormat (..),

    -- ** ComponentStatus
    ComponentStatus (..),

    -- ** ComponentType
    ComponentType (..),

    -- ** ContainerRepositoryService
    ContainerRepositoryService (..),

    -- ** ContainerType
    ContainerType (..),

    -- ** DiskImageFormat
    DiskImageFormat (..),

    -- ** EbsVolumeType
    EbsVolumeType (..),

    -- ** ImageStatus
    ImageStatus (..),

    -- ** ImageType
    ImageType (..),

    -- ** Ownership
    Ownership (..),

    -- ** PipelineExecutionStartCondition
    PipelineExecutionStartCondition (..),

    -- ** PipelineStatus
    PipelineStatus (..),

    -- ** Platform
    Platform (..),

    -- ** AdditionalInstanceConfiguration
    AdditionalInstanceConfiguration (AdditionalInstanceConfiguration'),
    newAdditionalInstanceConfiguration,

    -- ** Ami
    Ami (Ami'),
    newAmi,

    -- ** AmiDistributionConfiguration
    AmiDistributionConfiguration (AmiDistributionConfiguration'),
    newAmiDistributionConfiguration,

    -- ** Component
    Component (Component'),
    newComponent,

    -- ** ComponentConfiguration
    ComponentConfiguration (ComponentConfiguration'),
    newComponentConfiguration,

    -- ** ComponentParameter
    ComponentParameter (ComponentParameter'),
    newComponentParameter,

    -- ** ComponentParameterDetail
    ComponentParameterDetail (ComponentParameterDetail'),
    newComponentParameterDetail,

    -- ** ComponentState
    ComponentState (ComponentState'),
    newComponentState,

    -- ** ComponentSummary
    ComponentSummary (ComponentSummary'),
    newComponentSummary,

    -- ** ComponentVersion
    ComponentVersion (ComponentVersion'),
    newComponentVersion,

    -- ** Container
    Container (Container'),
    newContainer,

    -- ** ContainerDistributionConfiguration
    ContainerDistributionConfiguration (ContainerDistributionConfiguration'),
    newContainerDistributionConfiguration,

    -- ** ContainerRecipe
    ContainerRecipe (ContainerRecipe'),
    newContainerRecipe,

    -- ** ContainerRecipeSummary
    ContainerRecipeSummary (ContainerRecipeSummary'),
    newContainerRecipeSummary,

    -- ** Distribution
    Distribution (Distribution'),
    newDistribution,

    -- ** DistributionConfiguration
    DistributionConfiguration (DistributionConfiguration'),
    newDistributionConfiguration,

    -- ** DistributionConfigurationSummary
    DistributionConfigurationSummary (DistributionConfigurationSummary'),
    newDistributionConfigurationSummary,

    -- ** EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (EbsInstanceBlockDeviceSpecification'),
    newEbsInstanceBlockDeviceSpecification,

    -- ** FastLaunchConfiguration
    FastLaunchConfiguration (FastLaunchConfiguration'),
    newFastLaunchConfiguration,

    -- ** FastLaunchLaunchTemplateSpecification
    FastLaunchLaunchTemplateSpecification (FastLaunchLaunchTemplateSpecification'),
    newFastLaunchLaunchTemplateSpecification,

    -- ** FastLaunchSnapshotConfiguration
    FastLaunchSnapshotConfiguration (FastLaunchSnapshotConfiguration'),
    newFastLaunchSnapshotConfiguration,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImagePackage
    ImagePackage (ImagePackage'),
    newImagePackage,

    -- ** ImagePipeline
    ImagePipeline (ImagePipeline'),
    newImagePipeline,

    -- ** ImageRecipe
    ImageRecipe (ImageRecipe'),
    newImageRecipe,

    -- ** ImageRecipeSummary
    ImageRecipeSummary (ImageRecipeSummary'),
    newImageRecipeSummary,

    -- ** ImageState
    ImageState (ImageState'),
    newImageState,

    -- ** ImageSummary
    ImageSummary (ImageSummary'),
    newImageSummary,

    -- ** ImageTestsConfiguration
    ImageTestsConfiguration (ImageTestsConfiguration'),
    newImageTestsConfiguration,

    -- ** ImageVersion
    ImageVersion (ImageVersion'),
    newImageVersion,

    -- ** InfrastructureConfiguration
    InfrastructureConfiguration (InfrastructureConfiguration'),
    newInfrastructureConfiguration,

    -- ** InfrastructureConfigurationSummary
    InfrastructureConfigurationSummary (InfrastructureConfigurationSummary'),
    newInfrastructureConfigurationSummary,

    -- ** InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (InstanceBlockDeviceMapping'),
    newInstanceBlockDeviceMapping,

    -- ** InstanceConfiguration
    InstanceConfiguration (InstanceConfiguration'),
    newInstanceConfiguration,

    -- ** InstanceMetadataOptions
    InstanceMetadataOptions (InstanceMetadataOptions'),
    newInstanceMetadataOptions,

    -- ** LaunchPermissionConfiguration
    LaunchPermissionConfiguration (LaunchPermissionConfiguration'),
    newLaunchPermissionConfiguration,

    -- ** LaunchTemplateConfiguration
    LaunchTemplateConfiguration (LaunchTemplateConfiguration'),
    newLaunchTemplateConfiguration,

    -- ** Logging
    Logging (Logging'),
    newLogging,

    -- ** OutputResources
    OutputResources (OutputResources'),
    newOutputResources,

    -- ** S3ExportConfiguration
    S3ExportConfiguration (S3ExportConfiguration'),
    newS3ExportConfiguration,

    -- ** S3Logs
    S3Logs (S3Logs'),
    newS3Logs,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** SystemsManagerAgent
    SystemsManagerAgent (SystemsManagerAgent'),
    newSystemsManagerAgent,

    -- ** TargetContainerRepository
    TargetContainerRepository (TargetContainerRepository'),
    newTargetContainerRepository,
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
import Amazonka.ImageBuilder.Lens
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
import Amazonka.ImageBuilder.Types
import Amazonka.ImageBuilder.UntagResource
import Amazonka.ImageBuilder.UpdateDistributionConfiguration
import Amazonka.ImageBuilder.UpdateImagePipeline
import Amazonka.ImageBuilder.UpdateInfrastructureConfiguration
import Amazonka.ImageBuilder.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ImageBuilder'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
