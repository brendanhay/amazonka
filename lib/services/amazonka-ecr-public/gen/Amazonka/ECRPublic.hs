{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ECRPublic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-10-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic Container Registry Public
--
-- Amazon Elastic Container Registry (Amazon ECR) is a managed container
-- image registry service. Amazon ECR provides both public and private
-- registries to host your container images. You can use the familiar
-- Docker CLI, or their preferred client, to push, pull, and manage images.
-- Amazon ECR provides a secure, scalable, and reliable registry for your
-- Docker or Open Container Initiative (OCI) images. Amazon ECR supports
-- public repositories with this API. For information about the Amazon ECR
-- API for private repositories, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/APIReference/Welcome.html Amazon Elastic Container Registry API Reference>.
module Amazonka.ECRPublic
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** EmptyUploadException
    _EmptyUploadException,

    -- ** ImageAlreadyExistsException
    _ImageAlreadyExistsException,

    -- ** ImageDigestDoesNotMatchException
    _ImageDigestDoesNotMatchException,

    -- ** ImageNotFoundException
    _ImageNotFoundException,

    -- ** ImageTagAlreadyExistsException
    _ImageTagAlreadyExistsException,

    -- ** InvalidLayerException
    _InvalidLayerException,

    -- ** InvalidLayerPartException
    _InvalidLayerPartException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** LayerAlreadyExistsException
    _LayerAlreadyExistsException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** LayersNotFoundException
    _LayersNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ReferencedImagesNotFoundException
    _ReferencedImagesNotFoundException,

    -- ** RegistryNotFoundException
    _RegistryNotFoundException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** RepositoryNotFoundException
    _RepositoryNotFoundException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** ServerException
    _ServerException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedCommandException
    _UnsupportedCommandException,

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCheckLayerAvailability
    BatchCheckLayerAvailability (BatchCheckLayerAvailability'),
    newBatchCheckLayerAvailability,
    BatchCheckLayerAvailabilityResponse (BatchCheckLayerAvailabilityResponse'),
    newBatchCheckLayerAvailabilityResponse,

    -- ** BatchDeleteImage
    BatchDeleteImage (BatchDeleteImage'),
    newBatchDeleteImage,
    BatchDeleteImageResponse (BatchDeleteImageResponse'),
    newBatchDeleteImageResponse,

    -- ** CompleteLayerUpload
    CompleteLayerUpload (CompleteLayerUpload'),
    newCompleteLayerUpload,
    CompleteLayerUploadResponse (CompleteLayerUploadResponse'),
    newCompleteLayerUploadResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** DeleteRepository
    DeleteRepository (DeleteRepository'),
    newDeleteRepository,
    DeleteRepositoryResponse (DeleteRepositoryResponse'),
    newDeleteRepositoryResponse,

    -- ** DeleteRepositoryPolicy
    DeleteRepositoryPolicy (DeleteRepositoryPolicy'),
    newDeleteRepositoryPolicy,
    DeleteRepositoryPolicyResponse (DeleteRepositoryPolicyResponse'),
    newDeleteRepositoryPolicyResponse,

    -- ** DescribeImageTags (Paginated)
    DescribeImageTags (DescribeImageTags'),
    newDescribeImageTags,
    DescribeImageTagsResponse (DescribeImageTagsResponse'),
    newDescribeImageTagsResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DescribeRegistries (Paginated)
    DescribeRegistries (DescribeRegistries'),
    newDescribeRegistries,
    DescribeRegistriesResponse (DescribeRegistriesResponse'),
    newDescribeRegistriesResponse,

    -- ** DescribeRepositories (Paginated)
    DescribeRepositories (DescribeRepositories'),
    newDescribeRepositories,
    DescribeRepositoriesResponse (DescribeRepositoriesResponse'),
    newDescribeRepositoriesResponse,

    -- ** GetAuthorizationToken
    GetAuthorizationToken (GetAuthorizationToken'),
    newGetAuthorizationToken,
    GetAuthorizationTokenResponse (GetAuthorizationTokenResponse'),
    newGetAuthorizationTokenResponse,

    -- ** GetRegistryCatalogData
    GetRegistryCatalogData (GetRegistryCatalogData'),
    newGetRegistryCatalogData,
    GetRegistryCatalogDataResponse (GetRegistryCatalogDataResponse'),
    newGetRegistryCatalogDataResponse,

    -- ** GetRepositoryCatalogData
    GetRepositoryCatalogData (GetRepositoryCatalogData'),
    newGetRepositoryCatalogData,
    GetRepositoryCatalogDataResponse (GetRepositoryCatalogDataResponse'),
    newGetRepositoryCatalogDataResponse,

    -- ** GetRepositoryPolicy
    GetRepositoryPolicy (GetRepositoryPolicy'),
    newGetRepositoryPolicy,
    GetRepositoryPolicyResponse (GetRepositoryPolicyResponse'),
    newGetRepositoryPolicyResponse,

    -- ** InitiateLayerUpload
    InitiateLayerUpload (InitiateLayerUpload'),
    newInitiateLayerUpload,
    InitiateLayerUploadResponse (InitiateLayerUploadResponse'),
    newInitiateLayerUploadResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutImage
    PutImage (PutImage'),
    newPutImage,
    PutImageResponse (PutImageResponse'),
    newPutImageResponse,

    -- ** PutRegistryCatalogData
    PutRegistryCatalogData (PutRegistryCatalogData'),
    newPutRegistryCatalogData,
    PutRegistryCatalogDataResponse (PutRegistryCatalogDataResponse'),
    newPutRegistryCatalogDataResponse,

    -- ** PutRepositoryCatalogData
    PutRepositoryCatalogData (PutRepositoryCatalogData'),
    newPutRepositoryCatalogData,
    PutRepositoryCatalogDataResponse (PutRepositoryCatalogDataResponse'),
    newPutRepositoryCatalogDataResponse,

    -- ** SetRepositoryPolicy
    SetRepositoryPolicy (SetRepositoryPolicy'),
    newSetRepositoryPolicy,
    SetRepositoryPolicyResponse (SetRepositoryPolicyResponse'),
    newSetRepositoryPolicyResponse,

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

    -- ** UploadLayerPart
    UploadLayerPart (UploadLayerPart'),
    newUploadLayerPart,
    UploadLayerPartResponse (UploadLayerPartResponse'),
    newUploadLayerPartResponse,

    -- * Types

    -- ** ImageFailureCode
    ImageFailureCode (..),

    -- ** LayerAvailability
    LayerAvailability (..),

    -- ** LayerFailureCode
    LayerFailureCode (..),

    -- ** RegistryAliasStatus
    RegistryAliasStatus (..),

    -- ** AuthorizationData
    AuthorizationData (AuthorizationData'),
    newAuthorizationData,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageDetail
    ImageDetail (ImageDetail'),
    newImageDetail,

    -- ** ImageFailure
    ImageFailure (ImageFailure'),
    newImageFailure,

    -- ** ImageIdentifier
    ImageIdentifier (ImageIdentifier'),
    newImageIdentifier,

    -- ** ImageTagDetail
    ImageTagDetail (ImageTagDetail'),
    newImageTagDetail,

    -- ** Layer
    Layer (Layer'),
    newLayer,

    -- ** LayerFailure
    LayerFailure (LayerFailure'),
    newLayerFailure,

    -- ** ReferencedImageDetail
    ReferencedImageDetail (ReferencedImageDetail'),
    newReferencedImageDetail,

    -- ** Registry
    Registry (Registry'),
    newRegistry,

    -- ** RegistryAlias
    RegistryAlias (RegistryAlias'),
    newRegistryAlias,

    -- ** RegistryCatalogData
    RegistryCatalogData (RegistryCatalogData'),
    newRegistryCatalogData,

    -- ** Repository
    Repository (Repository'),
    newRepository,

    -- ** RepositoryCatalogData
    RepositoryCatalogData (RepositoryCatalogData'),
    newRepositoryCatalogData,

    -- ** RepositoryCatalogDataInput
    RepositoryCatalogDataInput (RepositoryCatalogDataInput'),
    newRepositoryCatalogDataInput,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.ECRPublic.BatchCheckLayerAvailability
import Amazonka.ECRPublic.BatchDeleteImage
import Amazonka.ECRPublic.CompleteLayerUpload
import Amazonka.ECRPublic.CreateRepository
import Amazonka.ECRPublic.DeleteRepository
import Amazonka.ECRPublic.DeleteRepositoryPolicy
import Amazonka.ECRPublic.DescribeImageTags
import Amazonka.ECRPublic.DescribeImages
import Amazonka.ECRPublic.DescribeRegistries
import Amazonka.ECRPublic.DescribeRepositories
import Amazonka.ECRPublic.GetAuthorizationToken
import Amazonka.ECRPublic.GetRegistryCatalogData
import Amazonka.ECRPublic.GetRepositoryCatalogData
import Amazonka.ECRPublic.GetRepositoryPolicy
import Amazonka.ECRPublic.InitiateLayerUpload
import Amazonka.ECRPublic.Lens
import Amazonka.ECRPublic.ListTagsForResource
import Amazonka.ECRPublic.PutImage
import Amazonka.ECRPublic.PutRegistryCatalogData
import Amazonka.ECRPublic.PutRepositoryCatalogData
import Amazonka.ECRPublic.SetRepositoryPolicy
import Amazonka.ECRPublic.TagResource
import Amazonka.ECRPublic.Types
import Amazonka.ECRPublic.UntagResource
import Amazonka.ECRPublic.UploadLayerPart
import Amazonka.ECRPublic.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ECRPublic'.

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
