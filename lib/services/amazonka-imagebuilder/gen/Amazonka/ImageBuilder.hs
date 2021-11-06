{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** CallRateLimitExceededException
    _CallRateLimitExceededException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidVersionNumberException
    _InvalidVersionNumberException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ServiceException
    _ServiceException,

    -- ** ResourceDependencyException
    _ResourceDependencyException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** ClientException
    _ClientException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListImagePackages
    ListImagePackages (ListImagePackages'),
    newListImagePackages,
    ListImagePackagesResponse (ListImagePackagesResponse'),
    newListImagePackagesResponse,

    -- ** GetDistributionConfiguration
    GetDistributionConfiguration (GetDistributionConfiguration'),
    newGetDistributionConfiguration,
    GetDistributionConfigurationResponse (GetDistributionConfigurationResponse'),
    newGetDistributionConfigurationResponse,

    -- ** ImportComponent
    ImportComponent (ImportComponent'),
    newImportComponent,
    ImportComponentResponse (ImportComponentResponse'),
    newImportComponentResponse,

    -- ** ListComponentBuildVersions
    ListComponentBuildVersions (ListComponentBuildVersions'),
    newListComponentBuildVersions,
    ListComponentBuildVersionsResponse (ListComponentBuildVersionsResponse'),
    newListComponentBuildVersionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListImageBuildVersions
    ListImageBuildVersions (ListImageBuildVersions'),
    newListImageBuildVersions,
    ListImageBuildVersionsResponse (ListImageBuildVersionsResponse'),
    newListImageBuildVersionsResponse,

    -- ** CancelImageCreation
    CancelImageCreation (CancelImageCreation'),
    newCancelImageCreation,
    CancelImageCreationResponse (CancelImageCreationResponse'),
    newCancelImageCreationResponse,

    -- ** GetImagePipeline
    GetImagePipeline (GetImagePipeline'),
    newGetImagePipeline,
    GetImagePipelineResponse (GetImagePipelineResponse'),
    newGetImagePipelineResponse,

    -- ** CreateInfrastructureConfiguration
    CreateInfrastructureConfiguration (CreateInfrastructureConfiguration'),
    newCreateInfrastructureConfiguration,
    CreateInfrastructureConfigurationResponse (CreateInfrastructureConfigurationResponse'),
    newCreateInfrastructureConfigurationResponse,

    -- ** ListDistributionConfigurations
    ListDistributionConfigurations (ListDistributionConfigurations'),
    newListDistributionConfigurations,
    ListDistributionConfigurationsResponse (ListDistributionConfigurationsResponse'),
    newListDistributionConfigurationsResponse,

    -- ** CreateContainerRecipe
    CreateContainerRecipe (CreateContainerRecipe'),
    newCreateContainerRecipe,
    CreateContainerRecipeResponse (CreateContainerRecipeResponse'),
    newCreateContainerRecipeResponse,

    -- ** ListInfrastructureConfigurations
    ListInfrastructureConfigurations (ListInfrastructureConfigurations'),
    newListInfrastructureConfigurations,
    ListInfrastructureConfigurationsResponse (ListInfrastructureConfigurationsResponse'),
    newListInfrastructureConfigurationsResponse,

    -- ** PutImageRecipePolicy
    PutImageRecipePolicy (PutImageRecipePolicy'),
    newPutImageRecipePolicy,
    PutImageRecipePolicyResponse (PutImageRecipePolicyResponse'),
    newPutImageRecipePolicyResponse,

    -- ** ListContainerRecipes
    ListContainerRecipes (ListContainerRecipes'),
    newListContainerRecipes,
    ListContainerRecipesResponse (ListContainerRecipesResponse'),
    newListContainerRecipesResponse,

    -- ** PutImagePolicy
    PutImagePolicy (PutImagePolicy'),
    newPutImagePolicy,
    PutImagePolicyResponse (PutImagePolicyResponse'),
    newPutImagePolicyResponse,

    -- ** DeleteContainerRecipe
    DeleteContainerRecipe (DeleteContainerRecipe'),
    newDeleteContainerRecipe,
    DeleteContainerRecipeResponse (DeleteContainerRecipeResponse'),
    newDeleteContainerRecipeResponse,

    -- ** PutComponentPolicy
    PutComponentPolicy (PutComponentPolicy'),
    newPutComponentPolicy,
    PutComponentPolicyResponse (PutComponentPolicyResponse'),
    newPutComponentPolicyResponse,

    -- ** DeleteInfrastructureConfiguration
    DeleteInfrastructureConfiguration (DeleteInfrastructureConfiguration'),
    newDeleteInfrastructureConfiguration,
    DeleteInfrastructureConfigurationResponse (DeleteInfrastructureConfigurationResponse'),
    newDeleteInfrastructureConfigurationResponse,

    -- ** UpdateInfrastructureConfiguration
    UpdateInfrastructureConfiguration (UpdateInfrastructureConfiguration'),
    newUpdateInfrastructureConfiguration,
    UpdateInfrastructureConfigurationResponse (UpdateInfrastructureConfigurationResponse'),
    newUpdateInfrastructureConfigurationResponse,

    -- ** CreateImagePipeline
    CreateImagePipeline (CreateImagePipeline'),
    newCreateImagePipeline,
    CreateImagePipelineResponse (CreateImagePipelineResponse'),
    newCreateImagePipelineResponse,

    -- ** GetContainerRecipe
    GetContainerRecipe (GetContainerRecipe'),
    newGetContainerRecipe,
    GetContainerRecipeResponse (GetContainerRecipeResponse'),
    newGetContainerRecipeResponse,

    -- ** GetInfrastructureConfiguration
    GetInfrastructureConfiguration (GetInfrastructureConfiguration'),
    newGetInfrastructureConfiguration,
    GetInfrastructureConfigurationResponse (GetInfrastructureConfigurationResponse'),
    newGetInfrastructureConfigurationResponse,

    -- ** GetImagePolicy
    GetImagePolicy (GetImagePolicy'),
    newGetImagePolicy,
    GetImagePolicyResponse (GetImagePolicyResponse'),
    newGetImagePolicyResponse,

    -- ** GetImageRecipePolicy
    GetImageRecipePolicy (GetImageRecipePolicy'),
    newGetImageRecipePolicy,
    GetImageRecipePolicyResponse (GetImageRecipePolicyResponse'),
    newGetImageRecipePolicyResponse,

    -- ** GetComponentPolicy
    GetComponentPolicy (GetComponentPolicy'),
    newGetComponentPolicy,
    GetComponentPolicyResponse (GetComponentPolicyResponse'),
    newGetComponentPolicyResponse,

    -- ** DeleteImagePipeline
    DeleteImagePipeline (DeleteImagePipeline'),
    newDeleteImagePipeline,
    DeleteImagePipelineResponse (DeleteImagePipelineResponse'),
    newDeleteImagePipelineResponse,

    -- ** UpdateImagePipeline
    UpdateImagePipeline (UpdateImagePipeline'),
    newUpdateImagePipeline,
    UpdateImagePipelineResponse (UpdateImagePipelineResponse'),
    newUpdateImagePipelineResponse,

    -- ** ListImagePipelines
    ListImagePipelines (ListImagePipelines'),
    newListImagePipelines,
    ListImagePipelinesResponse (ListImagePipelinesResponse'),
    newListImagePipelinesResponse,

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

    -- ** CreateImageRecipe
    CreateImageRecipe (CreateImageRecipe'),
    newCreateImageRecipe,
    CreateImageRecipeResponse (CreateImageRecipeResponse'),
    newCreateImageRecipeResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateComponent
    CreateComponent (CreateComponent'),
    newCreateComponent,
    CreateComponentResponse (CreateComponentResponse'),
    newCreateComponentResponse,

    -- ** DeleteDistributionConfiguration
    DeleteDistributionConfiguration (DeleteDistributionConfiguration'),
    newDeleteDistributionConfiguration,
    DeleteDistributionConfigurationResponse (DeleteDistributionConfigurationResponse'),
    newDeleteDistributionConfigurationResponse,

    -- ** UpdateDistributionConfiguration
    UpdateDistributionConfiguration (UpdateDistributionConfiguration'),
    newUpdateDistributionConfiguration,
    UpdateDistributionConfigurationResponse (UpdateDistributionConfigurationResponse'),
    newUpdateDistributionConfigurationResponse,

    -- ** ListImagePipelineImages
    ListImagePipelineImages (ListImagePipelineImages'),
    newListImagePipelineImages,
    ListImagePipelineImagesResponse (ListImagePipelineImagesResponse'),
    newListImagePipelineImagesResponse,

    -- ** DeleteImageRecipe
    DeleteImageRecipe (DeleteImageRecipe'),
    newDeleteImageRecipe,
    DeleteImageRecipeResponse (DeleteImageRecipeResponse'),
    newDeleteImageRecipeResponse,

    -- ** ListComponents
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** PutContainerRecipePolicy
    PutContainerRecipePolicy (PutContainerRecipePolicy'),
    newPutContainerRecipePolicy,
    PutContainerRecipePolicyResponse (PutContainerRecipePolicyResponse'),
    newPutContainerRecipePolicyResponse,

    -- ** ListImages
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** CreateDistributionConfiguration
    CreateDistributionConfiguration (CreateDistributionConfiguration'),
    newCreateDistributionConfiguration,
    CreateDistributionConfigurationResponse (CreateDistributionConfigurationResponse'),
    newCreateDistributionConfigurationResponse,

    -- ** ListImageRecipes
    ListImageRecipes (ListImageRecipes'),
    newListImageRecipes,
    ListImageRecipesResponse (ListImageRecipesResponse'),
    newListImageRecipesResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteComponent
    DeleteComponent (DeleteComponent'),
    newDeleteComponent,
    DeleteComponentResponse (DeleteComponentResponse'),
    newDeleteComponentResponse,

    -- ** GetImage
    GetImage (GetImage'),
    newGetImage,
    GetImageResponse (GetImageResponse'),
    newGetImageResponse,

    -- ** GetContainerRecipePolicy
    GetContainerRecipePolicy (GetContainerRecipePolicy'),
    newGetContainerRecipePolicy,
    GetContainerRecipePolicyResponse (GetContainerRecipePolicyResponse'),
    newGetContainerRecipePolicyResponse,

    -- ** GetImageRecipe
    GetImageRecipe (GetImageRecipe'),
    newGetImageRecipe,
    GetImageRecipeResponse (GetImageRecipeResponse'),
    newGetImageRecipeResponse,

    -- ** GetComponent
    GetComponent (GetComponent'),
    newGetComponent,
    GetComponentResponse (GetComponentResponse'),
    newGetComponentResponse,

    -- * Types

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
