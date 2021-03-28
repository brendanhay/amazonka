-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ImageTagAlreadyExistsException
    , _LayersNotFoundException
    , _ReferencedImagesNotFoundException
    , _InvalidParameterException
    , _LayerAlreadyExistsException
    , _ServerException
    , _LayerInaccessibleException
    , _InvalidLayerException
    , _LayerPartTooSmallException
    , _LifecyclePolicyPreviewNotFoundException
    , _ImageDigestDoesNotMatchException
    , _ImageNotFoundException
    , _ImageAlreadyExistsException
    , _RepositoryNotFoundException
    , _TooManyTagsException
    , _LifecyclePolicyPreviewInProgressException
    , _UploadNotFoundException
    , _LifecyclePolicyNotFoundException
    , _KmsException
    , _InvalidLayerPartException
    , _InvalidTagParameterException
    , _RepositoryNotEmptyException
    , _UnsupportedImageTypeException
    , _RepositoryAlreadyExistsException
    , _ScanNotFoundException
    , _RepositoryPolicyNotFoundException
    , _EmptyUploadException
    , _LimitExceededException

    -- * LifecyclePolicyPreviewFilter
    , LifecyclePolicyPreviewFilter (..)
    , mkLifecyclePolicyPreviewFilter
    , lppfTagStatus

    -- * Attribute
    , Attribute (..)
    , mkAttribute
    , aKey
    , aValue

    -- * ImageIdentifier
    , ImageIdentifier (..)
    , mkImageIdentifier
    , iiImageDigest
    , iiImageTag

    -- * MediaType
    , MediaType (..)

    -- * EncryptionType
    , EncryptionType (..)

    -- * ImageFailure
    , ImageFailure (..)
    , mkImageFailure
    , ifFailureCode
    , ifFailureReason
    , ifImageId

    -- * Image
    , Image (..)
    , mkImage
    , iImageId
    , iImageManifest
    , iImageManifestMediaType
    , iRegistryId
    , iRepositoryName

    -- * LayerFailureReason
    , LayerFailureReason (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * Repository
    , Repository (..)
    , mkRepository
    , rCreatedAt
    , rEncryptionConfiguration
    , rImageScanningConfiguration
    , rImageTagMutability
    , rRegistryId
    , rRepositoryArn
    , rRepositoryName
    , rRepositoryUri

    -- * KmsKey
    , KmsKey (..)

    -- * Arn
    , Arn (..)

    -- * ScanStatusDescription
    , ScanStatusDescription (..)

    -- * RegistryId
    , RegistryId (..)

    -- * AuthorizationData
    , AuthorizationData (..)
    , mkAuthorizationData
    , adAuthorizationToken
    , adExpiresAt
    , adProxyEndpoint

    -- * LayerDigest
    , LayerDigest (..)

    -- * Url
    , Url (..)

    -- * BatchedOperationLayerDigest
    , BatchedOperationLayerDigest (..)

    -- * LifecyclePolicyText
    , LifecyclePolicyText (..)

    -- * ImageDetail
    , ImageDetail (..)
    , mkImageDetail
    , idArtifactMediaType
    , idImageDigest
    , idImageManifestMediaType
    , idImagePushedAt
    , idImageScanFindingsSummary
    , idImageScanStatus
    , idImageSizeInBytes
    , idImageTags
    , idRegistryId
    , idRepositoryName

    -- * ImageScanFindings
    , ImageScanFindings (..)
    , mkImageScanFindings
    , isfFindingSeverityCounts
    , isfFindings
    , isfImageScanCompletedAt
    , isfVulnerabilitySourceUpdatedAt

    -- * ImageScanningConfiguration
    , ImageScanningConfiguration (..)
    , mkImageScanningConfiguration
    , iscScanOnPush

    -- * ListImagesFilter
    , ListImagesFilter (..)
    , mkListImagesFilter
    , lifTagStatus

    -- * ImageActionType
    , ImageActionType (..)

    -- * DescribeImagesFilter
    , DescribeImagesFilter (..)
    , mkDescribeImagesFilter
    , difTagStatus

    -- * ImageScanStatus
    , ImageScanStatus (..)
    , mkImageScanStatus
    , issDescription
    , issStatus

    -- * NextToken
    , NextToken (..)

    -- * ImageDigest
    , ImageDigest (..)

    -- * ProxyEndpoint
    , ProxyEndpoint (..)

    -- * RepositoryPolicyText
    , RepositoryPolicyText (..)

    -- * EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecEncryptionType
    , ecKmsKey

    -- * LayerAvailability
    , LayerAvailability (..)

    -- * ImageFailureCode
    , ImageFailureCode (..)

    -- * FindingName
    , FindingName (..)

    -- * ImageScanFindingsSummary
    , ImageScanFindingsSummary (..)
    , mkImageScanFindingsSummary
    , isfsFindingSeverityCounts
    , isfsImageScanCompletedAt
    , isfsVulnerabilitySourceUpdatedAt

    -- * LifecyclePolicyPreviewResult
    , LifecyclePolicyPreviewResult (..)
    , mkLifecyclePolicyPreviewResult
    , lpprAction
    , lpprAppliedRulePriority
    , lpprImageDigest
    , lpprImagePushedAt
    , lpprImageTags

    -- * ImageScanFinding
    , ImageScanFinding (..)
    , mkImageScanFinding
    , isfAttributes
    , isfDescription
    , isfName
    , isfSeverity
    , isfUri

    -- * RepositoryName
    , RepositoryName (..)

    -- * ImageTag
    , ImageTag (..)

    -- * TagKey
    , TagKey (..)

    -- * ImageManifest
    , ImageManifest (..)

    -- * LifecyclePolicyPreviewSummary
    , LifecyclePolicyPreviewSummary (..)
    , mkLifecyclePolicyPreviewSummary
    , lppsExpiringImageTotalCount

    -- * LifecyclePolicyPreviewStatus
    , LifecyclePolicyPreviewStatus (..)

    -- * ImageTagMutability
    , ImageTagMutability (..)

    -- * Layer
    , Layer (..)
    , mkLayer
    , lLayerAvailability
    , lLayerDigest
    , lLayerSize
    , lMediaType

    -- * LayerFailure
    , LayerFailure (..)
    , mkLayerFailure
    , lfFailureCode
    , lfFailureReason
    , lfLayerDigest

    -- * ScanStatus
    , ScanStatus (..)

    -- * TagStatus
    , TagStatus (..)

    -- * LifecyclePolicyRuleAction
    , LifecyclePolicyRuleAction (..)
    , mkLifecyclePolicyRuleAction
    , lpraType

    -- * UploadId
    , UploadId (..)

    -- * LayerFailureCode
    , LayerFailureCode (..)

    -- * FindingSeverity
    , FindingSeverity (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * FailureReason
    , FailureReason (..)

    -- * PolicyText
    , PolicyText (..)

    -- * RepositoryArn
    , RepositoryArn (..)

    -- * RepositoryUri
    , RepositoryUri (..)

    -- * AuthorizationToken
    , AuthorizationToken (..)

    -- * Description
    , Description (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
  
  
import Network.AWS.ECR.Types.Attribute
  
import Network.AWS.ECR.Types.ImageIdentifier
  
import Network.AWS.ECR.Types.MediaType
  
  
import Network.AWS.ECR.Types.EncryptionType
  
  
import Network.AWS.ECR.Types.ImageFailure
  
  
import Network.AWS.ECR.Types.Image
  
import Network.AWS.ECR.Types.LayerFailureReason
  
import Network.AWS.ECR.Types.Tag
  
  
import Network.AWS.ECR.Types.Repository
  
  
import Network.AWS.ECR.Types.KmsKey
  
import Network.AWS.ECR.Types.Arn
  
  
import Network.AWS.ECR.Types.ScanStatusDescription
  
  
import Network.AWS.ECR.Types.RegistryId
  
  
  
import Network.AWS.ECR.Types.AuthorizationData
  
  
  
import Network.AWS.ECR.Types.LayerDigest
  
import Network.AWS.ECR.Types.Url
  
import Network.AWS.ECR.Types.BatchedOperationLayerDigest
  
import Network.AWS.ECR.Types.LifecyclePolicyText
  
  
import Network.AWS.ECR.Types.ImageDetail
  
  
import Network.AWS.ECR.Types.ImageScanFindings
  
import Network.AWS.ECR.Types.ImageScanningConfiguration
  
import Network.AWS.ECR.Types.ListImagesFilter
  
import Network.AWS.ECR.Types.ImageActionType
  
import Network.AWS.ECR.Types.DescribeImagesFilter
  
  
  
import Network.AWS.ECR.Types.ImageScanStatus
  
import Network.AWS.ECR.Types.NextToken
  
import Network.AWS.ECR.Types.ImageDigest
  
import Network.AWS.ECR.Types.ProxyEndpoint
  
import Network.AWS.ECR.Types.RepositoryPolicyText
  
  
import Network.AWS.ECR.Types.EncryptionConfiguration
  
import Network.AWS.ECR.Types.LayerAvailability
  
  
import Network.AWS.ECR.Types.ImageFailureCode
  
import Network.AWS.ECR.Types.FindingName
  
  
import Network.AWS.ECR.Types.ImageScanFindingsSummary
  
  
  
  
import Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
  
import Network.AWS.ECR.Types.ImageScanFinding
  
import Network.AWS.ECR.Types.RepositoryName
  
import Network.AWS.ECR.Types.ImageTag
  
  
import Network.AWS.ECR.Types.TagKey
  
import Network.AWS.ECR.Types.ImageManifest
  
import Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
  
import Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
  
import Network.AWS.ECR.Types.ImageTagMutability
  
import Network.AWS.ECR.Types.Layer
  
import Network.AWS.ECR.Types.LayerFailure
  
  
import Network.AWS.ECR.Types.ScanStatus
  
  
import Network.AWS.ECR.Types.TagStatus
  
import Network.AWS.ECR.Types.LifecyclePolicyRuleAction
  
import Network.AWS.ECR.Types.UploadId
  
import Network.AWS.ECR.Types.LayerFailureCode
  
  
import Network.AWS.ECR.Types.FindingSeverity
  
  
  
import Network.AWS.ECR.Types.Key
  
import Network.AWS.ECR.Types.Value
  
import Network.AWS.ECR.Types.FailureReason
  
import Network.AWS.ECR.Types.PolicyText
  
import Network.AWS.ECR.Types.RepositoryArn
  
import Network.AWS.ECR.Types.RepositoryUri
  
import Network.AWS.ECR.Types.AuthorizationToken
  
import Network.AWS.ECR.Types.Description
  

-- | API version @2015-09-21@ of the Amazon EC2 Container Registry SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ECR", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "api.ecr", Core._svcVersion = "2015-09-21",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ECR",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The specified image is tagged with a tag that already exists. The repository is configured for tag immutability.
_ImageTagAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ImageTagAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ImageTagAlreadyExistsException"
{-# INLINEABLE _ImageTagAlreadyExistsException #-}
{-# DEPRECATED _ImageTagAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified layers could not be found, or the specified layer is not valid for this repository.
_LayersNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LayersNotFoundException
  = Core._MatchServiceError mkServiceConfig "LayersNotFoundException"
{-# INLINEABLE _LayersNotFoundException #-}
{-# DEPRECATED _LayersNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The manifest list is referencing an image that does not exist.
_ReferencedImagesNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReferencedImagesNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ReferencedImagesNotFoundException"
{-# INLINEABLE _ReferencedImagesNotFoundException #-}
{-# DEPRECATED _ReferencedImagesNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified parameter is invalid. Review the available parameters for the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LayerAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "LayerAlreadyExistsException"
{-# INLINEABLE _LayerAlreadyExistsException #-}
{-# DEPRECATED _LayerAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | These errors are usually caused by a server-side issue.
_ServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServerException
  = Core._MatchServiceError mkServiceConfig "ServerException"
{-# INLINEABLE _ServerException #-}
{-# DEPRECATED _ServerException "Use generic-lens or generic-optics instead"  #-}

-- | The specified layer is not available because it is not associated with an image. Unassociated image layers may be cleaned up at any time.
_LayerInaccessibleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LayerInaccessibleException
  = Core._MatchServiceError mkServiceConfig
      "LayerInaccessibleException"
{-# INLINEABLE _LayerInaccessibleException #-}
{-# DEPRECATED _LayerInaccessibleException "Use generic-lens or generic-optics instead"  #-}

-- | The layer digest calculation performed by Amazon ECR upon receipt of the image layer does not match the digest specified.
_InvalidLayerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLayerException
  = Core._MatchServiceError mkServiceConfig "InvalidLayerException"
{-# INLINEABLE _InvalidLayerException #-}
{-# DEPRECATED _InvalidLayerException "Use generic-lens or generic-optics instead"  #-}

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LayerPartTooSmallException
  = Core._MatchServiceError mkServiceConfig
      "LayerPartTooSmallException"
{-# INLINEABLE _LayerPartTooSmallException #-}
{-# DEPRECATED _LayerPartTooSmallException "Use generic-lens or generic-optics instead"  #-}

-- | There is no dry run for this repository.
_LifecyclePolicyPreviewNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LifecyclePolicyPreviewNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "LifecyclePolicyPreviewNotFoundException"
{-# INLINEABLE _LifecyclePolicyPreviewNotFoundException #-}
{-# DEPRECATED _LifecyclePolicyPreviewNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified image digest does not match the digest that Amazon ECR calculated for the image.
_ImageDigestDoesNotMatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ImageDigestDoesNotMatchException
  = Core._MatchServiceError mkServiceConfig
      "ImageDigestDoesNotMatchException"
{-# INLINEABLE _ImageDigestDoesNotMatchException #-}
{-# DEPRECATED _ImageDigestDoesNotMatchException "Use generic-lens or generic-optics instead"  #-}

-- | The image requested does not exist in the specified repository.
_ImageNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ImageNotFoundException
  = Core._MatchServiceError mkServiceConfig "ImageNotFoundException"
{-# INLINEABLE _ImageNotFoundException #-}
{-# DEPRECATED _ImageNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified image has already been pushed, and there were no changes to the manifest or image tag after the last push.
_ImageAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ImageAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ImageAlreadyExistsException"
{-# INLINEABLE _ImageAlreadyExistsException #-}
{-# DEPRECATED _ImageAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified repository could not be found. Check the spelling of the specified repository and ensure that you are performing operations on the correct registry.
_RepositoryNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "RepositoryNotFoundException"
{-# INLINEABLE _RepositoryNotFoundException #-}
{-# DEPRECATED _RepositoryNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The list of tags on the repository is over the limit. The maximum number of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | The previous lifecycle policy preview request has not completed. Wait and try again.
_LifecyclePolicyPreviewInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LifecyclePolicyPreviewInProgressException
  = Core._MatchServiceError mkServiceConfig
      "LifecyclePolicyPreviewInProgressException"
{-# INLINEABLE _LifecyclePolicyPreviewInProgressException #-}
{-# DEPRECATED _LifecyclePolicyPreviewInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | The upload could not be found, or the specified upload ID is not valid for this repository.
_UploadNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UploadNotFoundException
  = Core._MatchServiceError mkServiceConfig "UploadNotFoundException"
{-# INLINEABLE _UploadNotFoundException #-}
{-# DEPRECATED _UploadNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The lifecycle policy could not be found, and no policy is set to the repository.
_LifecyclePolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LifecyclePolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "LifecyclePolicyNotFoundException"
{-# INLINEABLE _LifecyclePolicyNotFoundException #-}
{-# DEPRECATED _LifecyclePolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The operation failed due to a KMS exception.
_KmsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KmsException
  = Core._MatchServiceError mkServiceConfig "KmsException"
{-# INLINEABLE _KmsException #-}
{-# DEPRECATED _KmsException "Use generic-lens or generic-optics instead"  #-}

-- | The layer part size is not valid, or the first byte specified is not consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLayerPartException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLayerPartException"
{-# INLINEABLE _InvalidLayerPartException #-}
{-# DEPRECATED _InvalidLayerPartException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid parameter has been specified. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
_InvalidTagParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTagParameterException"
{-# INLINEABLE _InvalidTagParameterException #-}
{-# DEPRECATED _InvalidTagParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The specified repository contains images. To delete a repository that contains images, you must force the deletion with the @force@ parameter.
_RepositoryNotEmptyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNotEmptyException
  = Core._MatchServiceError mkServiceConfig
      "RepositoryNotEmptyException"
{-# INLINEABLE _RepositoryNotEmptyException #-}
{-# DEPRECATED _RepositoryNotEmptyException "Use generic-lens or generic-optics instead"  #-}

-- | The image is of a type that cannot be scanned.
_UnsupportedImageTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedImageTypeException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedImageTypeException"
{-# INLINEABLE _UnsupportedImageTypeException #-}
{-# DEPRECATED _UnsupportedImageTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "RepositoryAlreadyExistsException"
{-# INLINEABLE _RepositoryAlreadyExistsException #-}
{-# DEPRECATED _RepositoryAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified image scan could not be found. Ensure that image scanning is enabled on the repository and try again.
_ScanNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScanNotFoundException
  = Core._MatchServiceError mkServiceConfig "ScanNotFoundException"
{-# INLINEABLE _ScanNotFoundException #-}
{-# DEPRECATED _ScanNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified repository and registry combination does not have an associated repository policy.
_RepositoryPolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryPolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "RepositoryPolicyNotFoundException"
{-# INLINEABLE _RepositoryPolicyNotFoundException #-}
{-# DEPRECATED _RepositoryPolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EmptyUploadException
  = Core._MatchServiceError mkServiceConfig "EmptyUploadException"
{-# INLINEABLE _EmptyUploadException #-}
{-# DEPRECATED _EmptyUploadException "Use generic-lens or generic-optics instead"  #-}

-- | The operation did not succeed because it would have exceeded a service limit for your account. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/service-quotas.html Amazon ECR Service Quotas> in the Amazon Elastic Container Registry User Guide.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
