{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ECR
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-09-21@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic Container Registry
--
-- Amazon Elastic Container Registry (Amazon ECR) is a managed container
-- image registry service. Customers can use the familiar Docker CLI, or
-- their preferred client, to push, pull, and manage images. Amazon ECR
-- provides a secure, scalable, and reliable registry for your Docker or
-- Open Container Initiative (OCI) images. Amazon ECR supports private
-- repositories with resource-based permissions using IAM so that specific
-- users or Amazon EC2 instances can access repositories and images.
--
-- Amazon ECR has service endpoints in each supported Region. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/ecr.html Amazon ECR endpoints>
-- in the /Amazon Web Services General Reference/.
module Network.AWS.ECR
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ImageTagAlreadyExistsException
    _ImageTagAlreadyExistsException,

    -- ** ValidationException
    _ValidationException,

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

    -- ** LayerInaccessibleException
    _LayerInaccessibleException,

    -- ** InvalidLayerException
    _InvalidLayerException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** LifecyclePolicyPreviewNotFoundException
    _LifecyclePolicyPreviewNotFoundException,

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

    -- ** RegistryPolicyNotFoundException
    _RegistryPolicyNotFoundException,

    -- ** LifecyclePolicyPreviewInProgressException
    _LifecyclePolicyPreviewInProgressException,

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- ** LifecyclePolicyNotFoundException
    _LifecyclePolicyNotFoundException,

    -- ** KmsException
    _KmsException,

    -- ** InvalidLayerPartException
    _InvalidLayerPartException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** UnsupportedImageTypeException
    _UnsupportedImageTypeException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** ScanNotFoundException
    _ScanNotFoundException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** EmptyUploadException
    _EmptyUploadException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** LifecyclePolicyPreviewComplete
    newLifecyclePolicyPreviewComplete,

    -- ** ImageScanComplete
    newImageScanComplete,

    -- * Operations
    -- $operations

    -- ** GetRepositoryPolicy
    GetRepositoryPolicy (GetRepositoryPolicy'),
    newGetRepositoryPolicy,
    GetRepositoryPolicyResponse (GetRepositoryPolicyResponse'),
    newGetRepositoryPolicyResponse,

    -- ** PutImageScanningConfiguration
    PutImageScanningConfiguration (PutImageScanningConfiguration'),
    newPutImageScanningConfiguration,
    PutImageScanningConfigurationResponse (PutImageScanningConfigurationResponse'),
    newPutImageScanningConfigurationResponse,

    -- ** PutLifecyclePolicy
    PutLifecyclePolicy (PutLifecyclePolicy'),
    newPutLifecyclePolicy,
    PutLifecyclePolicyResponse (PutLifecyclePolicyResponse'),
    newPutLifecyclePolicyResponse,

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** PutImageTagMutability
    PutImageTagMutability (PutImageTagMutability'),
    newPutImageTagMutability,
    PutImageTagMutabilityResponse (PutImageTagMutabilityResponse'),
    newPutImageTagMutabilityResponse,

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

    -- ** GetLifecyclePolicyPreview (Paginated)
    GetLifecyclePolicyPreview (GetLifecyclePolicyPreview'),
    newGetLifecyclePolicyPreview,
    GetLifecyclePolicyPreviewResponse (GetLifecyclePolicyPreviewResponse'),
    newGetLifecyclePolicyPreviewResponse,

    -- ** BatchCheckLayerAvailability
    BatchCheckLayerAvailability (BatchCheckLayerAvailability'),
    newBatchCheckLayerAvailability,
    BatchCheckLayerAvailabilityResponse (BatchCheckLayerAvailabilityResponse'),
    newBatchCheckLayerAvailabilityResponse,

    -- ** DescribeRegistry
    DescribeRegistry (DescribeRegistry'),
    newDescribeRegistry,
    DescribeRegistryResponse (DescribeRegistryResponse'),
    newDescribeRegistryResponse,

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

    -- ** StartLifecyclePolicyPreview
    StartLifecyclePolicyPreview (StartLifecyclePolicyPreview'),
    newStartLifecyclePolicyPreview,
    StartLifecyclePolicyPreviewResponse (StartLifecyclePolicyPreviewResponse'),
    newStartLifecyclePolicyPreviewResponse,

    -- ** DeleteRegistryPolicy
    DeleteRegistryPolicy (DeleteRegistryPolicy'),
    newDeleteRegistryPolicy,
    DeleteRegistryPolicyResponse (DeleteRegistryPolicyResponse'),
    newDeleteRegistryPolicyResponse,

    -- ** PutRegistryPolicy
    PutRegistryPolicy (PutRegistryPolicy'),
    newPutRegistryPolicy,
    PutRegistryPolicyResponse (PutRegistryPolicyResponse'),
    newPutRegistryPolicyResponse,

    -- ** UploadLayerPart
    UploadLayerPart (UploadLayerPart'),
    newUploadLayerPart,
    UploadLayerPartResponse (UploadLayerPartResponse'),
    newUploadLayerPartResponse,

    -- ** DescribeImageReplicationStatus
    DescribeImageReplicationStatus (DescribeImageReplicationStatus'),
    newDescribeImageReplicationStatus,
    DescribeImageReplicationStatusResponse (DescribeImageReplicationStatusResponse'),
    newDescribeImageReplicationStatusResponse,

    -- ** BatchGetImage
    BatchGetImage (BatchGetImage'),
    newBatchGetImage,
    BatchGetImageResponse (BatchGetImageResponse'),
    newBatchGetImageResponse,

    -- ** PutReplicationConfiguration
    PutReplicationConfiguration (PutReplicationConfiguration'),
    newPutReplicationConfiguration,
    PutReplicationConfigurationResponse (PutReplicationConfigurationResponse'),
    newPutReplicationConfigurationResponse,

    -- ** StartImageScan
    StartImageScan (StartImageScan'),
    newStartImageScan,
    StartImageScanResponse (StartImageScanResponse'),
    newStartImageScanResponse,

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

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

    -- ** DescribeImageScanFindings (Paginated)
    DescribeImageScanFindings (DescribeImageScanFindings'),
    newDescribeImageScanFindings,
    DescribeImageScanFindingsResponse (DescribeImageScanFindingsResponse'),
    newDescribeImageScanFindingsResponse,

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

    -- ** DeleteRepository
    DeleteRepository (DeleteRepository'),
    newDeleteRepository,
    DeleteRepositoryResponse (DeleteRepositoryResponse'),
    newDeleteRepositoryResponse,

    -- ** GetRegistryPolicy
    GetRegistryPolicy (GetRegistryPolicy'),
    newGetRegistryPolicy,
    GetRegistryPolicyResponse (GetRegistryPolicyResponse'),
    newGetRegistryPolicyResponse,

    -- ** PutImage
    PutImage (PutImage'),
    newPutImage,
    PutImageResponse (PutImageResponse'),
    newPutImageResponse,

    -- ** ListImages (Paginated)
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** GetAuthorizationToken
    GetAuthorizationToken (GetAuthorizationToken'),
    newGetAuthorizationToken,
    GetAuthorizationTokenResponse (GetAuthorizationTokenResponse'),
    newGetAuthorizationTokenResponse,

    -- ** GetDownloadUrlForLayer
    GetDownloadUrlForLayer (GetDownloadUrlForLayer'),
    newGetDownloadUrlForLayer,
    GetDownloadUrlForLayerResponse (GetDownloadUrlForLayerResponse'),
    newGetDownloadUrlForLayerResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- * Types

    -- ** EncryptionType
    EncryptionType (..),

    -- ** FindingSeverity
    FindingSeverity (..),

    -- ** ImageActionType
    ImageActionType (..),

    -- ** ImageFailureCode
    ImageFailureCode (..),

    -- ** ImageTagMutability
    ImageTagMutability (..),

    -- ** LayerAvailability
    LayerAvailability (..),

    -- ** LayerFailureCode
    LayerFailureCode (..),

    -- ** LifecyclePolicyPreviewStatus
    LifecyclePolicyPreviewStatus (..),

    -- ** ReplicationStatus
    ReplicationStatus (..),

    -- ** RepositoryFilterType
    RepositoryFilterType (..),

    -- ** ScanStatus
    ScanStatus (..),

    -- ** TagStatus
    TagStatus (..),

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** AuthorizationData
    AuthorizationData (AuthorizationData'),
    newAuthorizationData,

    -- ** DescribeImagesFilter
    DescribeImagesFilter (DescribeImagesFilter'),
    newDescribeImagesFilter,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

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

    -- ** ImageReplicationStatus
    ImageReplicationStatus (ImageReplicationStatus'),
    newImageReplicationStatus,

    -- ** ImageScanFinding
    ImageScanFinding (ImageScanFinding'),
    newImageScanFinding,

    -- ** ImageScanFindings
    ImageScanFindings (ImageScanFindings'),
    newImageScanFindings,

    -- ** ImageScanFindingsSummary
    ImageScanFindingsSummary (ImageScanFindingsSummary'),
    newImageScanFindingsSummary,

    -- ** ImageScanStatus
    ImageScanStatus (ImageScanStatus'),
    newImageScanStatus,

    -- ** ImageScanningConfiguration
    ImageScanningConfiguration (ImageScanningConfiguration'),
    newImageScanningConfiguration,

    -- ** Layer
    Layer (Layer'),
    newLayer,

    -- ** LayerFailure
    LayerFailure (LayerFailure'),
    newLayerFailure,

    -- ** LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter (LifecyclePolicyPreviewFilter'),
    newLifecyclePolicyPreviewFilter,

    -- ** LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult (LifecyclePolicyPreviewResult'),
    newLifecyclePolicyPreviewResult,

    -- ** LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary (LifecyclePolicyPreviewSummary'),
    newLifecyclePolicyPreviewSummary,

    -- ** LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction (LifecyclePolicyRuleAction'),
    newLifecyclePolicyRuleAction,

    -- ** ListImagesFilter
    ListImagesFilter (ListImagesFilter'),
    newListImagesFilter,

    -- ** ReplicationConfiguration
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** ReplicationDestination
    ReplicationDestination (ReplicationDestination'),
    newReplicationDestination,

    -- ** ReplicationRule
    ReplicationRule (ReplicationRule'),
    newReplicationRule,

    -- ** Repository
    Repository (Repository'),
    newRepository,

    -- ** RepositoryFilter
    RepositoryFilter (RepositoryFilter'),
    newRepositoryFilter,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.ECR.BatchCheckLayerAvailability
import Network.AWS.ECR.BatchDeleteImage
import Network.AWS.ECR.BatchGetImage
import Network.AWS.ECR.CompleteLayerUpload
import Network.AWS.ECR.CreateRepository
import Network.AWS.ECR.DeleteLifecyclePolicy
import Network.AWS.ECR.DeleteRegistryPolicy
import Network.AWS.ECR.DeleteRepository
import Network.AWS.ECR.DeleteRepositoryPolicy
import Network.AWS.ECR.DescribeImageReplicationStatus
import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.DescribeRegistry
import Network.AWS.ECR.DescribeRepositories
import Network.AWS.ECR.GetAuthorizationToken
import Network.AWS.ECR.GetDownloadUrlForLayer
import Network.AWS.ECR.GetLifecyclePolicy
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.GetRegistryPolicy
import Network.AWS.ECR.GetRepositoryPolicy
import Network.AWS.ECR.InitiateLayerUpload
import Network.AWS.ECR.Lens
import Network.AWS.ECR.ListImages
import Network.AWS.ECR.ListTagsForResource
import Network.AWS.ECR.PutImage
import Network.AWS.ECR.PutImageScanningConfiguration
import Network.AWS.ECR.PutImageTagMutability
import Network.AWS.ECR.PutLifecyclePolicy
import Network.AWS.ECR.PutRegistryPolicy
import Network.AWS.ECR.PutReplicationConfiguration
import Network.AWS.ECR.SetRepositoryPolicy
import Network.AWS.ECR.StartImageScan
import Network.AWS.ECR.StartLifecyclePolicyPreview
import Network.AWS.ECR.TagResource
import Network.AWS.ECR.Types
import Network.AWS.ECR.UntagResource
import Network.AWS.ECR.UploadLayerPart
import Network.AWS.ECR.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ECR'.

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
