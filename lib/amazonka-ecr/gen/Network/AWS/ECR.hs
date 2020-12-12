{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic Container Registry__
--
-- Amazon Elastic Container Registry (Amazon ECR) is a managed container image registry service. Customers can use the familiar Docker CLI, or their preferred client, to push, pull, and manage images. Amazon ECR provides a secure, scalable, and reliable registry for your Docker or Open Container Initiative (OCI) images. Amazon ECR supports private repositories with resource-based permissions using IAM so that specific users or Amazon EC2 instances can access repositories and images.
module Network.AWS.ECR
  ( -- * Service configuration
    ecrService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** LifecyclePolicyPreviewComplete
    mkLifecyclePolicyPreviewComplete,

    -- ** ImageScanComplete
    mkImageScanComplete,

    -- * Operations
    -- $operations

    -- ** GetRepositoryPolicy
    module Network.AWS.ECR.GetRepositoryPolicy,

    -- ** PutImageScanningConfiguration
    module Network.AWS.ECR.PutImageScanningConfiguration,

    -- ** PutLifecyclePolicy
    module Network.AWS.ECR.PutLifecyclePolicy,

    -- ** DeleteLifecyclePolicy
    module Network.AWS.ECR.DeleteLifecyclePolicy,

    -- ** PutImageTagMutability
    module Network.AWS.ECR.PutImageTagMutability,

    -- ** BatchDeleteImage
    module Network.AWS.ECR.BatchDeleteImage,

    -- ** ListTagsForResource
    module Network.AWS.ECR.ListTagsForResource,

    -- ** GetLifecyclePolicyPreview (Paginated)
    module Network.AWS.ECR.GetLifecyclePolicyPreview,

    -- ** BatchCheckLayerAvailability
    module Network.AWS.ECR.BatchCheckLayerAvailability,

    -- ** DeleteRepositoryPolicy
    module Network.AWS.ECR.DeleteRepositoryPolicy,

    -- ** CreateRepository
    module Network.AWS.ECR.CreateRepository,

    -- ** CompleteLayerUpload
    module Network.AWS.ECR.CompleteLayerUpload,

    -- ** DescribeRepositories (Paginated)
    module Network.AWS.ECR.DescribeRepositories,

    -- ** StartLifecyclePolicyPreview
    module Network.AWS.ECR.StartLifecyclePolicyPreview,

    -- ** UploadLayerPart
    module Network.AWS.ECR.UploadLayerPart,

    -- ** BatchGetImage
    module Network.AWS.ECR.BatchGetImage,

    -- ** StartImageScan
    module Network.AWS.ECR.StartImageScan,

    -- ** GetLifecyclePolicy
    module Network.AWS.ECR.GetLifecyclePolicy,

    -- ** TagResource
    module Network.AWS.ECR.TagResource,

    -- ** SetRepositoryPolicy
    module Network.AWS.ECR.SetRepositoryPolicy,

    -- ** DescribeImageScanFindings (Paginated)
    module Network.AWS.ECR.DescribeImageScanFindings,

    -- ** InitiateLayerUpload
    module Network.AWS.ECR.InitiateLayerUpload,

    -- ** UntagResource
    module Network.AWS.ECR.UntagResource,

    -- ** DeleteRepository
    module Network.AWS.ECR.DeleteRepository,

    -- ** PutImage
    module Network.AWS.ECR.PutImage,

    -- ** ListImages (Paginated)
    module Network.AWS.ECR.ListImages,

    -- ** GetAuthorizationToken
    module Network.AWS.ECR.GetAuthorizationToken,

    -- ** GetDownloadURLForLayer
    module Network.AWS.ECR.GetDownloadURLForLayer,

    -- ** DescribeImages (Paginated)
    module Network.AWS.ECR.DescribeImages,

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

    -- ** ScanStatus
    ScanStatus (..),

    -- ** TagStatus
    TagStatus (..),

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aValue,
    aKey,

    -- ** AuthorizationData
    AuthorizationData (..),
    mkAuthorizationData,
    adExpiresAt,
    adProxyEndpoint,
    adAuthorizationToken,

    -- ** DescribeImagesFilter
    DescribeImagesFilter (..),
    mkDescribeImagesFilter,
    difTagStatus,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecKmsKey,
    ecEncryptionType,

    -- ** Image
    Image (..),
    mkImage,
    iRegistryId,
    iImageManifestMediaType,
    iImageId,
    iRepositoryName,
    iImageManifest,

    -- ** ImageDetail
    ImageDetail (..),
    mkImageDetail,
    idRegistryId,
    idImageTags,
    idImageScanStatus,
    idImageManifestMediaType,
    idImageSizeInBytes,
    idImageDigest,
    idImageScanFindingsSummary,
    idArtifactMediaType,
    idImagePushedAt,
    idRepositoryName,

    -- ** ImageFailure
    ImageFailure (..),
    mkImageFailure,
    ifFailureReason,
    ifFailureCode,
    ifImageId,

    -- ** ImageIdentifier
    ImageIdentifier (..),
    mkImageIdentifier,
    iiImageDigest,
    iiImageTag,

    -- ** ImageScanFinding
    ImageScanFinding (..),
    mkImageScanFinding,
    isfSeverity,
    isfUri,
    isfName,
    isfAttributes,
    isfDescription,

    -- ** ImageScanFindings
    ImageScanFindings (..),
    mkImageScanFindings,
    isfImageScanCompletedAt,
    isfFindings,
    isfFindingSeverityCounts,
    isfVulnerabilitySourceUpdatedAt,

    -- ** ImageScanFindingsSummary
    ImageScanFindingsSummary (..),
    mkImageScanFindingsSummary,
    isfsImageScanCompletedAt,
    isfsFindingSeverityCounts,
    isfsVulnerabilitySourceUpdatedAt,

    -- ** ImageScanStatus
    ImageScanStatus (..),
    mkImageScanStatus,
    issStatus,
    issDescription,

    -- ** ImageScanningConfiguration
    ImageScanningConfiguration (..),
    mkImageScanningConfiguration,
    iscScanOnPush,

    -- ** Layer
    Layer (..),
    mkLayer,
    lMediaType,
    lLayerDigest,
    lLayerSize,
    lLayerAvailability,

    -- ** LayerFailure
    LayerFailure (..),
    mkLayerFailure,
    lfFailureReason,
    lfFailureCode,
    lfLayerDigest,

    -- ** LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter (..),
    mkLifecyclePolicyPreviewFilter,
    lppfTagStatus,

    -- ** LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult (..),
    mkLifecyclePolicyPreviewResult,
    lpprImageTags,
    lpprAction,
    lpprImageDigest,
    lpprImagePushedAt,
    lpprAppliedRulePriority,

    -- ** LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary (..),
    mkLifecyclePolicyPreviewSummary,
    lppsExpiringImageTotalCount,

    -- ** LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction (..),
    mkLifecyclePolicyRuleAction,
    lpraType,

    -- ** ListImagesFilter
    ListImagesFilter (..),
    mkListImagesFilter,
    lifTagStatus,

    -- ** Repository
    Repository (..),
    mkRepository,
    rRepositoryARN,
    rCreatedAt,
    rRegistryId,
    rImageScanningConfiguration,
    rRepositoryURI,
    rEncryptionConfiguration,
    rRepositoryName,
    rImageTagMutability,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.ECR.BatchCheckLayerAvailability
import Network.AWS.ECR.BatchDeleteImage
import Network.AWS.ECR.BatchGetImage
import Network.AWS.ECR.CompleteLayerUpload
import Network.AWS.ECR.CreateRepository
import Network.AWS.ECR.DeleteLifecyclePolicy
import Network.AWS.ECR.DeleteRepository
import Network.AWS.ECR.DeleteRepositoryPolicy
import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.DescribeRepositories
import Network.AWS.ECR.GetAuthorizationToken
import Network.AWS.ECR.GetDownloadURLForLayer
import Network.AWS.ECR.GetLifecyclePolicy
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.GetRepositoryPolicy
import Network.AWS.ECR.InitiateLayerUpload
import Network.AWS.ECR.ListImages
import Network.AWS.ECR.ListTagsForResource
import Network.AWS.ECR.PutImage
import Network.AWS.ECR.PutImageScanningConfiguration
import Network.AWS.ECR.PutImageTagMutability
import Network.AWS.ECR.PutLifecyclePolicy
import Network.AWS.ECR.SetRepositoryPolicy
import Network.AWS.ECR.StartImageScan
import Network.AWS.ECR.StartLifecyclePolicyPreview
import Network.AWS.ECR.TagResource
import Network.AWS.ECR.Types
import Network.AWS.ECR.UntagResource
import Network.AWS.ECR.UploadLayerPart
import Network.AWS.ECR.Waiters
import qualified Network.AWS.Prelude as Lude

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
