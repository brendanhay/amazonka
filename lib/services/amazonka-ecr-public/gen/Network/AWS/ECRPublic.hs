{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ECRPublic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.ECRPublic
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ImageTagAlreadyExistsException
    _ImageTagAlreadyExistsException,

    -- ** LayersNotFoundException
    _LayersNotFoundException,

    -- ** ReferencedImagesNotFoundException
    _ReferencedImagesNotFoundException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LayerAlreadyExistsException
    _LayerAlreadyExistsException,

    -- ** ServerException
    _ServerException,

    -- ** UnsupportedCommandException
    _UnsupportedCommandException,

    -- ** InvalidLayerException
    _InvalidLayerException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** ImageDigestDoesNotMatchException
    _ImageDigestDoesNotMatchException,

    -- ** ImageNotFoundException
    _ImageNotFoundException,

    -- ** ImageAlreadyExistsException
    _ImageAlreadyExistsException,

    -- ** RepositoryNotFoundException
    _RepositoryNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- ** InvalidLayerPartException
    _InvalidLayerPartException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** EmptyUploadException
    _EmptyUploadException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** RegistryNotFoundException
    _RegistryNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetRepositoryPolicy
    GetRepositoryPolicy (GetRepositoryPolicy'),
    newGetRepositoryPolicy,
    GetRepositoryPolicyResponse (GetRepositoryPolicyResponse'),
    newGetRepositoryPolicyResponse,

    -- ** PutRegistryCatalogData
    PutRegistryCatalogData (PutRegistryCatalogData'),
    newPutRegistryCatalogData,
    PutRegistryCatalogDataResponse (PutRegistryCatalogDataResponse'),
    newPutRegistryCatalogDataResponse,

    -- ** BatchDeleteImage
    BatchDeleteImage (BatchDeleteImage'),
    newBatchDeleteImage,
    BatchDeleteImageResponse (BatchDeleteImageResponse'),
    newBatchDeleteImageResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** BatchCheckLayerAvailability
    BatchCheckLayerAvailability (BatchCheckLayerAvailability'),
    newBatchCheckLayerAvailability,
    BatchCheckLayerAvailabilityResponse (BatchCheckLayerAvailabilityResponse'),
    newBatchCheckLayerAvailabilityResponse,

    -- ** PutRepositoryCatalogData
    PutRepositoryCatalogData (PutRepositoryCatalogData'),
    newPutRepositoryCatalogData,
    PutRepositoryCatalogDataResponse (PutRepositoryCatalogDataResponse'),
    newPutRepositoryCatalogDataResponse,

    -- ** DeleteRepositoryPolicy
    DeleteRepositoryPolicy (DeleteRepositoryPolicy'),
    newDeleteRepositoryPolicy,
    DeleteRepositoryPolicyResponse (DeleteRepositoryPolicyResponse'),
    newDeleteRepositoryPolicyResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** DescribeRegistries (Paginated)
    DescribeRegistries (DescribeRegistries'),
    newDescribeRegistries,
    DescribeRegistriesResponse (DescribeRegistriesResponse'),
    newDescribeRegistriesResponse,

    -- ** CompleteLayerUpload
    CompleteLayerUpload (CompleteLayerUpload'),
    newCompleteLayerUpload,
    CompleteLayerUploadResponse (CompleteLayerUploadResponse'),
    newCompleteLayerUploadResponse,

    -- ** DescribeRepositories (Paginated)
    DescribeRepositories (DescribeRepositories'),
    newDescribeRepositories,
    DescribeRepositoriesResponse (DescribeRepositoriesResponse'),
    newDescribeRepositoriesResponse,

    -- ** UploadLayerPart
    UploadLayerPart (UploadLayerPart'),
    newUploadLayerPart,
    UploadLayerPartResponse (UploadLayerPartResponse'),
    newUploadLayerPartResponse,

    -- ** GetRepositoryCatalogData
    GetRepositoryCatalogData (GetRepositoryCatalogData'),
    newGetRepositoryCatalogData,
    GetRepositoryCatalogDataResponse (GetRepositoryCatalogDataResponse'),
    newGetRepositoryCatalogDataResponse,

    -- ** GetRegistryCatalogData
    GetRegistryCatalogData (GetRegistryCatalogData'),
    newGetRegistryCatalogData,
    GetRegistryCatalogDataResponse (GetRegistryCatalogDataResponse'),
    newGetRegistryCatalogDataResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** SetRepositoryPolicy
    SetRepositoryPolicy (SetRepositoryPolicy'),
    newSetRepositoryPolicy,
    SetRepositoryPolicyResponse (SetRepositoryPolicyResponse'),
    newSetRepositoryPolicyResponse,

    -- ** InitiateLayerUpload
    InitiateLayerUpload (InitiateLayerUpload'),
    newInitiateLayerUpload,
    InitiateLayerUploadResponse (InitiateLayerUploadResponse'),
    newInitiateLayerUploadResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeImageTags (Paginated)
    DescribeImageTags (DescribeImageTags'),
    newDescribeImageTags,
    DescribeImageTagsResponse (DescribeImageTagsResponse'),
    newDescribeImageTagsResponse,

    -- ** DeleteRepository
    DeleteRepository (DeleteRepository'),
    newDeleteRepository,
    DeleteRepositoryResponse (DeleteRepositoryResponse'),
    newDeleteRepositoryResponse,

    -- ** PutImage
    PutImage (PutImage'),
    newPutImage,
    PutImageResponse (PutImageResponse'),
    newPutImageResponse,

    -- ** GetAuthorizationToken
    GetAuthorizationToken (GetAuthorizationToken'),
    newGetAuthorizationToken,
    GetAuthorizationTokenResponse (GetAuthorizationTokenResponse'),
    newGetAuthorizationTokenResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

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

import Network.AWS.ECRPublic.BatchCheckLayerAvailability
import Network.AWS.ECRPublic.BatchDeleteImage
import Network.AWS.ECRPublic.CompleteLayerUpload
import Network.AWS.ECRPublic.CreateRepository
import Network.AWS.ECRPublic.DeleteRepository
import Network.AWS.ECRPublic.DeleteRepositoryPolicy
import Network.AWS.ECRPublic.DescribeImageTags
import Network.AWS.ECRPublic.DescribeImages
import Network.AWS.ECRPublic.DescribeRegistries
import Network.AWS.ECRPublic.DescribeRepositories
import Network.AWS.ECRPublic.GetAuthorizationToken
import Network.AWS.ECRPublic.GetRegistryCatalogData
import Network.AWS.ECRPublic.GetRepositoryCatalogData
import Network.AWS.ECRPublic.GetRepositoryPolicy
import Network.AWS.ECRPublic.InitiateLayerUpload
import Network.AWS.ECRPublic.Lens
import Network.AWS.ECRPublic.ListTagsForResource
import Network.AWS.ECRPublic.PutImage
import Network.AWS.ECRPublic.PutRegistryCatalogData
import Network.AWS.ECRPublic.PutRepositoryCatalogData
import Network.AWS.ECRPublic.SetRepositoryPolicy
import Network.AWS.ECRPublic.TagResource
import Network.AWS.ECRPublic.Types
import Network.AWS.ECRPublic.UntagResource
import Network.AWS.ECRPublic.UploadLayerPart
import Network.AWS.ECRPublic.Waiters

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
