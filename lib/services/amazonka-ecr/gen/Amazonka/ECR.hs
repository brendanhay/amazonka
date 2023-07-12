{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ECR
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ECR
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

    -- ** KmsException
    _KmsException,

    -- ** LayerAlreadyExistsException
    _LayerAlreadyExistsException,

    -- ** LayerInaccessibleException
    _LayerInaccessibleException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** LayersNotFoundException
    _LayersNotFoundException,

    -- ** LifecyclePolicyNotFoundException
    _LifecyclePolicyNotFoundException,

    -- ** LifecyclePolicyPreviewInProgressException
    _LifecyclePolicyPreviewInProgressException,

    -- ** LifecyclePolicyPreviewNotFoundException
    _LifecyclePolicyPreviewNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** PullThroughCacheRuleAlreadyExistsException
    _PullThroughCacheRuleAlreadyExistsException,

    -- ** PullThroughCacheRuleNotFoundException
    _PullThroughCacheRuleNotFoundException,

    -- ** ReferencedImagesNotFoundException
    _ReferencedImagesNotFoundException,

    -- ** RegistryPolicyNotFoundException
    _RegistryPolicyNotFoundException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** RepositoryNotFoundException
    _RepositoryNotFoundException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** ScanNotFoundException
    _ScanNotFoundException,

    -- ** ServerException
    _ServerException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedImageTypeException
    _UnsupportedImageTypeException,

    -- ** UnsupportedUpstreamRegistryException
    _UnsupportedUpstreamRegistryException,

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** ImageScanComplete
    newImageScanComplete,

    -- ** LifecyclePolicyPreviewComplete
    newLifecyclePolicyPreviewComplete,

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

    -- ** BatchGetImage
    BatchGetImage (BatchGetImage'),
    newBatchGetImage,
    BatchGetImageResponse (BatchGetImageResponse'),
    newBatchGetImageResponse,

    -- ** BatchGetRepositoryScanningConfiguration
    BatchGetRepositoryScanningConfiguration (BatchGetRepositoryScanningConfiguration'),
    newBatchGetRepositoryScanningConfiguration,
    BatchGetRepositoryScanningConfigurationResponse (BatchGetRepositoryScanningConfigurationResponse'),
    newBatchGetRepositoryScanningConfigurationResponse,

    -- ** CompleteLayerUpload
    CompleteLayerUpload (CompleteLayerUpload'),
    newCompleteLayerUpload,
    CompleteLayerUploadResponse (CompleteLayerUploadResponse'),
    newCompleteLayerUploadResponse,

    -- ** CreatePullThroughCacheRule
    CreatePullThroughCacheRule (CreatePullThroughCacheRule'),
    newCreatePullThroughCacheRule,
    CreatePullThroughCacheRuleResponse (CreatePullThroughCacheRuleResponse'),
    newCreatePullThroughCacheRuleResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** DeletePullThroughCacheRule
    DeletePullThroughCacheRule (DeletePullThroughCacheRule'),
    newDeletePullThroughCacheRule,
    DeletePullThroughCacheRuleResponse (DeletePullThroughCacheRuleResponse'),
    newDeletePullThroughCacheRuleResponse,

    -- ** DeleteRegistryPolicy
    DeleteRegistryPolicy (DeleteRegistryPolicy'),
    newDeleteRegistryPolicy,
    DeleteRegistryPolicyResponse (DeleteRegistryPolicyResponse'),
    newDeleteRegistryPolicyResponse,

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

    -- ** DescribeImageReplicationStatus
    DescribeImageReplicationStatus (DescribeImageReplicationStatus'),
    newDescribeImageReplicationStatus,
    DescribeImageReplicationStatusResponse (DescribeImageReplicationStatusResponse'),
    newDescribeImageReplicationStatusResponse,

    -- ** DescribeImageScanFindings (Paginated)
    DescribeImageScanFindings (DescribeImageScanFindings'),
    newDescribeImageScanFindings,
    DescribeImageScanFindingsResponse (DescribeImageScanFindingsResponse'),
    newDescribeImageScanFindingsResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DescribePullThroughCacheRules (Paginated)
    DescribePullThroughCacheRules (DescribePullThroughCacheRules'),
    newDescribePullThroughCacheRules,
    DescribePullThroughCacheRulesResponse (DescribePullThroughCacheRulesResponse'),
    newDescribePullThroughCacheRulesResponse,

    -- ** DescribeRegistry
    DescribeRegistry (DescribeRegistry'),
    newDescribeRegistry,
    DescribeRegistryResponse (DescribeRegistryResponse'),
    newDescribeRegistryResponse,

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

    -- ** GetDownloadUrlForLayer
    GetDownloadUrlForLayer (GetDownloadUrlForLayer'),
    newGetDownloadUrlForLayer,
    GetDownloadUrlForLayerResponse (GetDownloadUrlForLayerResponse'),
    newGetDownloadUrlForLayerResponse,

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

    -- ** GetLifecyclePolicyPreview (Paginated)
    GetLifecyclePolicyPreview (GetLifecyclePolicyPreview'),
    newGetLifecyclePolicyPreview,
    GetLifecyclePolicyPreviewResponse (GetLifecyclePolicyPreviewResponse'),
    newGetLifecyclePolicyPreviewResponse,

    -- ** GetRegistryPolicy
    GetRegistryPolicy (GetRegistryPolicy'),
    newGetRegistryPolicy,
    GetRegistryPolicyResponse (GetRegistryPolicyResponse'),
    newGetRegistryPolicyResponse,

    -- ** GetRegistryScanningConfiguration
    GetRegistryScanningConfiguration (GetRegistryScanningConfiguration'),
    newGetRegistryScanningConfiguration,
    GetRegistryScanningConfigurationResponse (GetRegistryScanningConfigurationResponse'),
    newGetRegistryScanningConfigurationResponse,

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

    -- ** ListImages (Paginated)
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

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

    -- ** PutImageScanningConfiguration
    PutImageScanningConfiguration (PutImageScanningConfiguration'),
    newPutImageScanningConfiguration,
    PutImageScanningConfigurationResponse (PutImageScanningConfigurationResponse'),
    newPutImageScanningConfigurationResponse,

    -- ** PutImageTagMutability
    PutImageTagMutability (PutImageTagMutability'),
    newPutImageTagMutability,
    PutImageTagMutabilityResponse (PutImageTagMutabilityResponse'),
    newPutImageTagMutabilityResponse,

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

    -- ** PutRegistryScanningConfiguration
    PutRegistryScanningConfiguration (PutRegistryScanningConfiguration'),
    newPutRegistryScanningConfiguration,
    PutRegistryScanningConfigurationResponse (PutRegistryScanningConfigurationResponse'),
    newPutRegistryScanningConfigurationResponse,

    -- ** PutReplicationConfiguration
    PutReplicationConfiguration (PutReplicationConfiguration'),
    newPutReplicationConfiguration,
    PutReplicationConfigurationResponse (PutReplicationConfigurationResponse'),
    newPutReplicationConfigurationResponse,

    -- ** SetRepositoryPolicy
    SetRepositoryPolicy (SetRepositoryPolicy'),
    newSetRepositoryPolicy,
    SetRepositoryPolicyResponse (SetRepositoryPolicyResponse'),
    newSetRepositoryPolicyResponse,

    -- ** StartImageScan
    StartImageScan (StartImageScan'),
    newStartImageScan,
    StartImageScanResponse (StartImageScanResponse'),
    newStartImageScanResponse,

    -- ** StartLifecyclePolicyPreview
    StartLifecyclePolicyPreview (StartLifecyclePolicyPreview'),
    newStartLifecyclePolicyPreview,
    StartLifecyclePolicyPreviewResponse (StartLifecyclePolicyPreviewResponse'),
    newStartLifecyclePolicyPreviewResponse,

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

    -- ** ScanFrequency
    ScanFrequency (..),

    -- ** ScanStatus
    ScanStatus (..),

    -- ** ScanType
    ScanType (..),

    -- ** ScanningConfigurationFailureCode
    ScanningConfigurationFailureCode (..),

    -- ** ScanningRepositoryFilterType
    ScanningRepositoryFilterType (..),

    -- ** TagStatus
    TagStatus (..),

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** AuthorizationData
    AuthorizationData (AuthorizationData'),
    newAuthorizationData,

    -- ** AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (AwsEcrContainerImageDetails'),
    newAwsEcrContainerImageDetails,

    -- ** CvssScore
    CvssScore (CvssScore'),
    newCvssScore,

    -- ** CvssScoreAdjustment
    CvssScoreAdjustment (CvssScoreAdjustment'),
    newCvssScoreAdjustment,

    -- ** CvssScoreDetails
    CvssScoreDetails (CvssScoreDetails'),
    newCvssScoreDetails,

    -- ** DescribeImagesFilter
    DescribeImagesFilter (DescribeImagesFilter'),
    newDescribeImagesFilter,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** EnhancedImageScanFinding
    EnhancedImageScanFinding (EnhancedImageScanFinding'),
    newEnhancedImageScanFinding,

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

    -- ** PackageVulnerabilityDetails
    PackageVulnerabilityDetails (PackageVulnerabilityDetails'),
    newPackageVulnerabilityDetails,

    -- ** PullThroughCacheRule
    PullThroughCacheRule (PullThroughCacheRule'),
    newPullThroughCacheRule,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** RegistryScanningConfiguration
    RegistryScanningConfiguration (RegistryScanningConfiguration'),
    newRegistryScanningConfiguration,

    -- ** RegistryScanningRule
    RegistryScanningRule (RegistryScanningRule'),
    newRegistryScanningRule,

    -- ** Remediation
    Remediation (Remediation'),
    newRemediation,

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

    -- ** RepositoryScanningConfiguration
    RepositoryScanningConfiguration (RepositoryScanningConfiguration'),
    newRepositoryScanningConfiguration,

    -- ** RepositoryScanningConfigurationFailure
    RepositoryScanningConfigurationFailure (RepositoryScanningConfigurationFailure'),
    newRepositoryScanningConfigurationFailure,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** ScanningRepositoryFilter
    ScanningRepositoryFilter (ScanningRepositoryFilter'),
    newScanningRepositoryFilter,

    -- ** ScoreDetails
    ScoreDetails (ScoreDetails'),
    newScoreDetails,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VulnerablePackage
    VulnerablePackage (VulnerablePackage'),
    newVulnerablePackage,
  )
where

import Amazonka.ECR.BatchCheckLayerAvailability
import Amazonka.ECR.BatchDeleteImage
import Amazonka.ECR.BatchGetImage
import Amazonka.ECR.BatchGetRepositoryScanningConfiguration
import Amazonka.ECR.CompleteLayerUpload
import Amazonka.ECR.CreatePullThroughCacheRule
import Amazonka.ECR.CreateRepository
import Amazonka.ECR.DeleteLifecyclePolicy
import Amazonka.ECR.DeletePullThroughCacheRule
import Amazonka.ECR.DeleteRegistryPolicy
import Amazonka.ECR.DeleteRepository
import Amazonka.ECR.DeleteRepositoryPolicy
import Amazonka.ECR.DescribeImageReplicationStatus
import Amazonka.ECR.DescribeImageScanFindings
import Amazonka.ECR.DescribeImages
import Amazonka.ECR.DescribePullThroughCacheRules
import Amazonka.ECR.DescribeRegistry
import Amazonka.ECR.DescribeRepositories
import Amazonka.ECR.GetAuthorizationToken
import Amazonka.ECR.GetDownloadUrlForLayer
import Amazonka.ECR.GetLifecyclePolicy
import Amazonka.ECR.GetLifecyclePolicyPreview
import Amazonka.ECR.GetRegistryPolicy
import Amazonka.ECR.GetRegistryScanningConfiguration
import Amazonka.ECR.GetRepositoryPolicy
import Amazonka.ECR.InitiateLayerUpload
import Amazonka.ECR.Lens
import Amazonka.ECR.ListImages
import Amazonka.ECR.ListTagsForResource
import Amazonka.ECR.PutImage
import Amazonka.ECR.PutImageScanningConfiguration
import Amazonka.ECR.PutImageTagMutability
import Amazonka.ECR.PutLifecyclePolicy
import Amazonka.ECR.PutRegistryPolicy
import Amazonka.ECR.PutRegistryScanningConfiguration
import Amazonka.ECR.PutReplicationConfiguration
import Amazonka.ECR.SetRepositoryPolicy
import Amazonka.ECR.StartImageScan
import Amazonka.ECR.StartLifecyclePolicyPreview
import Amazonka.ECR.TagResource
import Amazonka.ECR.Types
import Amazonka.ECR.UntagResource
import Amazonka.ECR.UploadLayerPart
import Amazonka.ECR.Waiters

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
