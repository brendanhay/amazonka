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

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- ** ImageTagAlreadyExistsException
    _ImageTagAlreadyExistsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** RepositoryNotFoundException
    _RepositoryNotFoundException,

    -- ** ImageAlreadyExistsException
    _ImageAlreadyExistsException,

    -- ** ScanNotFoundException
    _ScanNotFoundException,

    -- ** ImageDigestDoesNotMatchException
    _ImageDigestDoesNotMatchException,

    -- ** ImageNotFoundException
    _ImageNotFoundException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** InvalidLayerException
    _InvalidLayerException,

    -- ** InvalidLayerPartException
    _InvalidLayerPartException,

    -- ** ReferencedImagesNotFoundException
    _ReferencedImagesNotFoundException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LayersNotFoundException
    _LayersNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- ** EmptyUploadException
    _EmptyUploadException,

    -- ** RegistryPolicyNotFoundException
    _RegistryPolicyNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** LifecyclePolicyPreviewInProgressException
    _LifecyclePolicyPreviewInProgressException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** LifecyclePolicyPreviewNotFoundException
    _LifecyclePolicyPreviewNotFoundException,

    -- ** LayerInaccessibleException
    _LayerInaccessibleException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** UnsupportedImageTypeException
    _UnsupportedImageTypeException,

    -- ** LifecyclePolicyNotFoundException
    _LifecyclePolicyNotFoundException,

    -- ** LayerAlreadyExistsException
    _LayerAlreadyExistsException,

    -- ** ServerException
    _ServerException,

    -- ** KmsException
    _KmsException,

    -- * Waiters
    -- $waiters

    -- ** ImageScanComplete
    newImageScanComplete,

    -- ** LifecyclePolicyPreviewComplete
    newLifecyclePolicyPreviewComplete,

    -- * Operations
    -- $operations

    -- ** UploadLayerPart
    UploadLayerPart (UploadLayerPart'),
    newUploadLayerPart,
    UploadLayerPartResponse (UploadLayerPartResponse'),
    newUploadLayerPartResponse,

    -- ** PutLifecyclePolicy
    PutLifecyclePolicy (PutLifecyclePolicy'),
    newPutLifecyclePolicy,
    PutLifecyclePolicyResponse (PutLifecyclePolicyResponse'),
    newPutLifecyclePolicyResponse,

    -- ** PutRegistryPolicy
    PutRegistryPolicy (PutRegistryPolicy'),
    newPutRegistryPolicy,
    PutRegistryPolicyResponse (PutRegistryPolicyResponse'),
    newPutRegistryPolicyResponse,

    -- ** StartLifecyclePolicyPreview
    StartLifecyclePolicyPreview (StartLifecyclePolicyPreview'),
    newStartLifecyclePolicyPreview,
    StartLifecyclePolicyPreviewResponse (StartLifecyclePolicyPreviewResponse'),
    newStartLifecyclePolicyPreviewResponse,

    -- ** DescribeRepositories (Paginated)
    DescribeRepositories (DescribeRepositories'),
    newDescribeRepositories,
    DescribeRepositoriesResponse (DescribeRepositoriesResponse'),
    newDescribeRepositoriesResponse,

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

    -- ** GetRegistryPolicy
    GetRegistryPolicy (GetRegistryPolicy'),
    newGetRegistryPolicy,
    GetRegistryPolicyResponse (GetRegistryPolicyResponse'),
    newGetRegistryPolicyResponse,

    -- ** InitiateLayerUpload
    InitiateLayerUpload (InitiateLayerUpload'),
    newInitiateLayerUpload,
    InitiateLayerUploadResponse (InitiateLayerUploadResponse'),
    newInitiateLayerUploadResponse,

    -- ** DescribeImageScanFindings (Paginated)
    DescribeImageScanFindings (DescribeImageScanFindings'),
    newDescribeImageScanFindings,
    DescribeImageScanFindingsResponse (DescribeImageScanFindingsResponse'),
    newDescribeImageScanFindingsResponse,

    -- ** DeleteRepositoryPolicy
    DeleteRepositoryPolicy (DeleteRepositoryPolicy'),
    newDeleteRepositoryPolicy,
    DeleteRepositoryPolicyResponse (DeleteRepositoryPolicyResponse'),
    newDeleteRepositoryPolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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

    -- ** DescribeRegistry
    DescribeRegistry (DescribeRegistry'),
    newDescribeRegistry,
    DescribeRegistryResponse (DescribeRegistryResponse'),
    newDescribeRegistryResponse,

    -- ** BatchDeleteImage
    BatchDeleteImage (BatchDeleteImage'),
    newBatchDeleteImage,
    BatchDeleteImageResponse (BatchDeleteImageResponse'),
    newBatchDeleteImageResponse,

    -- ** PutImageScanningConfiguration
    PutImageScanningConfiguration (PutImageScanningConfiguration'),
    newPutImageScanningConfiguration,
    PutImageScanningConfigurationResponse (PutImageScanningConfigurationResponse'),
    newPutImageScanningConfigurationResponse,

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** DeleteRegistryPolicy
    DeleteRegistryPolicy (DeleteRegistryPolicy'),
    newDeleteRegistryPolicy,
    DeleteRegistryPolicyResponse (DeleteRegistryPolicyResponse'),
    newDeleteRegistryPolicyResponse,

    -- ** GetRepositoryPolicy
    GetRepositoryPolicy (GetRepositoryPolicy'),
    newGetRepositoryPolicy,
    GetRepositoryPolicyResponse (GetRepositoryPolicyResponse'),
    newGetRepositoryPolicyResponse,

    -- ** CompleteLayerUpload
    CompleteLayerUpload (CompleteLayerUpload'),
    newCompleteLayerUpload,
    CompleteLayerUploadResponse (CompleteLayerUploadResponse'),
    newCompleteLayerUploadResponse,

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

    -- ** GetAuthorizationToken
    GetAuthorizationToken (GetAuthorizationToken'),
    newGetAuthorizationToken,
    GetAuthorizationTokenResponse (GetAuthorizationTokenResponse'),
    newGetAuthorizationTokenResponse,

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

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

    -- ** StartImageScan
    StartImageScan (StartImageScan'),
    newStartImageScan,
    StartImageScanResponse (StartImageScanResponse'),
    newStartImageScanResponse,

    -- ** BatchCheckLayerAvailability
    BatchCheckLayerAvailability (BatchCheckLayerAvailability'),
    newBatchCheckLayerAvailability,
    BatchCheckLayerAvailabilityResponse (BatchCheckLayerAvailabilityResponse'),
    newBatchCheckLayerAvailabilityResponse,

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

    -- ** PutImageTagMutability
    PutImageTagMutability (PutImageTagMutability'),
    newPutImageTagMutability,
    PutImageTagMutabilityResponse (PutImageTagMutabilityResponse'),
    newPutImageTagMutabilityResponse,

    -- ** DescribeImageReplicationStatus
    DescribeImageReplicationStatus (DescribeImageReplicationStatus'),
    newDescribeImageReplicationStatus,
    DescribeImageReplicationStatusResponse (DescribeImageReplicationStatusResponse'),
    newDescribeImageReplicationStatusResponse,

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
