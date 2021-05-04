{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UploadNotFoundException,
    _ImageTagAlreadyExistsException,
    _TooManyTagsException,
    _RepositoryNotFoundException,
    _ImageAlreadyExistsException,
    _ScanNotFoundException,
    _ImageDigestDoesNotMatchException,
    _ImageNotFoundException,
    _LayerPartTooSmallException,
    _InvalidLayerException,
    _InvalidTagParameterException,
    _InvalidLayerPartException,
    _ReferencedImagesNotFoundException,
    _InvalidParameterException,
    _ValidationException,
    _LayersNotFoundException,
    _LimitExceededException,
    _RegistryPolicyNotFoundException,
    _LifecyclePolicyPreviewInProgressException,
    _EmptyUploadException,
    _RepositoryPolicyNotFoundException,
    _RepositoryAlreadyExistsException,
    _LifecyclePolicyPreviewNotFoundException,
    _UnsupportedImageTypeException,
    _RepositoryNotEmptyException,
    _LayerInaccessibleException,
    _LifecyclePolicyNotFoundException,
    _KmsException,
    _ServerException,
    _LayerAlreadyExistsException,

    -- * EncryptionType
    EncryptionType (..),

    -- * FindingSeverity
    FindingSeverity (..),

    -- * ImageActionType
    ImageActionType (..),

    -- * ImageFailureCode
    ImageFailureCode (..),

    -- * ImageTagMutability
    ImageTagMutability (..),

    -- * LayerAvailability
    LayerAvailability (..),

    -- * LayerFailureCode
    LayerFailureCode (..),

    -- * LifecyclePolicyPreviewStatus
    LifecyclePolicyPreviewStatus (..),

    -- * ScanStatus
    ScanStatus (..),

    -- * TagStatus
    TagStatus (..),

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_value,
    attribute_key,

    -- * AuthorizationData
    AuthorizationData (..),
    newAuthorizationData,
    authorizationData_proxyEndpoint,
    authorizationData_expiresAt,
    authorizationData_authorizationToken,

    -- * DescribeImagesFilter
    DescribeImagesFilter (..),
    newDescribeImagesFilter,
    describeImagesFilter_tagStatus,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- * Image
    Image (..),
    newImage,
    image_imageManifest,
    image_registryId,
    image_repositoryName,
    image_imageId,
    image_imageManifestMediaType,

    -- * ImageDetail
    ImageDetail (..),
    newImageDetail,
    imageDetail_imageDigest,
    imageDetail_imageScanStatus,
    imageDetail_imageTags,
    imageDetail_registryId,
    imageDetail_repositoryName,
    imageDetail_artifactMediaType,
    imageDetail_imageSizeInBytes,
    imageDetail_imageManifestMediaType,
    imageDetail_imagePushedAt,
    imageDetail_imageScanFindingsSummary,

    -- * ImageFailure
    ImageFailure (..),
    newImageFailure,
    imageFailure_failureCode,
    imageFailure_imageId,
    imageFailure_failureReason,

    -- * ImageIdentifier
    ImageIdentifier (..),
    newImageIdentifier,
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- * ImageScanFinding
    ImageScanFinding (..),
    newImageScanFinding,
    imageScanFinding_uri,
    imageScanFinding_severity,
    imageScanFinding_name,
    imageScanFinding_attributes,
    imageScanFinding_description,

    -- * ImageScanFindings
    ImageScanFindings (..),
    newImageScanFindings,
    imageScanFindings_findings,
    imageScanFindings_imageScanCompletedAt,
    imageScanFindings_vulnerabilitySourceUpdatedAt,
    imageScanFindings_findingSeverityCounts,

    -- * ImageScanFindingsSummary
    ImageScanFindingsSummary (..),
    newImageScanFindingsSummary,
    imageScanFindingsSummary_imageScanCompletedAt,
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,
    imageScanFindingsSummary_findingSeverityCounts,

    -- * ImageScanStatus
    ImageScanStatus (..),
    newImageScanStatus,
    imageScanStatus_status,
    imageScanStatus_description,

    -- * ImageScanningConfiguration
    ImageScanningConfiguration (..),
    newImageScanningConfiguration,
    imageScanningConfiguration_scanOnPush,

    -- * Layer
    Layer (..),
    newLayer,
    layer_layerSize,
    layer_layerAvailability,
    layer_mediaType,
    layer_layerDigest,

    -- * LayerFailure
    LayerFailure (..),
    newLayerFailure,
    layerFailure_failureCode,
    layerFailure_failureReason,
    layerFailure_layerDigest,

    -- * LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter (..),
    newLifecyclePolicyPreviewFilter,
    lifecyclePolicyPreviewFilter_tagStatus,

    -- * LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult (..),
    newLifecyclePolicyPreviewResult,
    lifecyclePolicyPreviewResult_imageDigest,
    lifecyclePolicyPreviewResult_appliedRulePriority,
    lifecyclePolicyPreviewResult_imageTags,
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_imagePushedAt,

    -- * LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary (..),
    newLifecyclePolicyPreviewSummary,
    lifecyclePolicyPreviewSummary_expiringImageTotalCount,

    -- * LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction (..),
    newLifecyclePolicyRuleAction,
    lifecyclePolicyRuleAction_type,

    -- * ListImagesFilter
    ListImagesFilter (..),
    newListImagesFilter,
    listImagesFilter_tagStatus,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    newReplicationConfiguration,
    replicationConfiguration_rules,

    -- * ReplicationDestination
    ReplicationDestination (..),
    newReplicationDestination,
    replicationDestination_region,
    replicationDestination_registryId,

    -- * ReplicationRule
    ReplicationRule (..),
    newReplicationRule,
    replicationRule_destinations,

    -- * Repository
    Repository (..),
    newRepository,
    repository_encryptionConfiguration,
    repository_repositoryUri,
    repository_registryId,
    repository_createdAt,
    repository_repositoryName,
    repository_repositoryArn,
    repository_imageScanningConfiguration,
    repository_imageTagMutability,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.AuthorizationData
import Network.AWS.ECR.Types.DescribeImagesFilter
import Network.AWS.ECR.Types.EncryptionConfiguration
import Network.AWS.ECR.Types.EncryptionType
import Network.AWS.ECR.Types.FindingSeverity
import Network.AWS.ECR.Types.Image
import Network.AWS.ECR.Types.ImageActionType
import Network.AWS.ECR.Types.ImageDetail
import Network.AWS.ECR.Types.ImageFailure
import Network.AWS.ECR.Types.ImageFailureCode
import Network.AWS.ECR.Types.ImageIdentifier
import Network.AWS.ECR.Types.ImageScanFinding
import Network.AWS.ECR.Types.ImageScanFindings
import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.ImageTagMutability
import Network.AWS.ECR.Types.Layer
import Network.AWS.ECR.Types.LayerAvailability
import Network.AWS.ECR.Types.LayerFailure
import Network.AWS.ECR.Types.LayerFailureCode
import Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
import Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
import Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
import Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
import Network.AWS.ECR.Types.LifecyclePolicyRuleAction
import Network.AWS.ECR.Types.ListImagesFilter
import Network.AWS.ECR.Types.ReplicationConfiguration
import Network.AWS.ECR.Types.ReplicationDestination
import Network.AWS.ECR.Types.ReplicationRule
import Network.AWS.ECR.Types.Repository
import Network.AWS.ECR.Types.ScanStatus
import Network.AWS.ECR.Types.Tag
import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-09-21@ of the Amazon EC2 Container Registry SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "ECR",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "api.ecr",
      Prelude._svcSigningName = "ecr",
      Prelude._svcVersion = "2015-09-21",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "ECR",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The upload could not be found, or the specified upload ID is not valid
-- for this repository.
_UploadNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UploadNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "UploadNotFoundException"

-- | The specified image is tagged with a tag that already exists. The
-- repository is configured for tag immutability.
_ImageTagAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ImageTagAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ImageTagAlreadyExistsException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The specified repository could not be found. Check the spelling of the
-- specified repository and ensure that you are performing operations on
-- the correct registry.
_RepositoryNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNotFoundException"

-- | The specified image has already been pushed, and there were no changes
-- to the manifest or image tag after the last push.
_ImageAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ImageAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ImageAlreadyExistsException"

-- | The specified image scan could not be found. Ensure that image scanning
-- is enabled on the repository and try again.
_ScanNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScanNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ScanNotFoundException"

-- | The specified image digest does not match the digest that Amazon ECR
-- calculated for the image.
_ImageDigestDoesNotMatchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ImageDigestDoesNotMatchException =
  Prelude._MatchServiceError
    defaultService
    "ImageDigestDoesNotMatchException"

-- | The image requested does not exist in the specified repository.
_ImageNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ImageNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ImageNotFoundException"

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LayerPartTooSmallException =
  Prelude._MatchServiceError
    defaultService
    "LayerPartTooSmallException"

-- | The layer digest calculation performed by Amazon ECR upon receipt of the
-- image layer does not match the digest specified.
_InvalidLayerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLayerException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLayerException"

-- | An invalid parameter has been specified. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
_InvalidTagParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | The layer part size is not valid, or the first byte specified is not
-- consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLayerPartException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLayerPartException"

-- | The manifest list is referencing an image that does not exist.
_ReferencedImagesNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReferencedImagesNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ReferencedImagesNotFoundException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | There was an exception validating this request.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"

-- | The specified layers could not be found, or the specified layer is not
-- valid for this repository.
_LayersNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LayersNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LayersNotFoundException"

-- | The operation did not succeed because it would have exceeded a service
-- limit for your account. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/service-quotas.html Amazon ECR Service Quotas>
-- in the Amazon Elastic Container Registry User Guide.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The registry doesn\'t have an associated registry policy.
_RegistryPolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RegistryPolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "RegistryPolicyNotFoundException"

-- | The previous lifecycle policy preview request has not completed. Wait
-- and try again.
_LifecyclePolicyPreviewInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LifecyclePolicyPreviewInProgressException =
  Prelude._MatchServiceError
    defaultService
    "LifecyclePolicyPreviewInProgressException"

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EmptyUploadException =
  Prelude._MatchServiceError
    defaultService
    "EmptyUploadException"

-- | The specified repository and registry combination does not have an
-- associated repository policy.
_RepositoryPolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryPolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryPolicyNotFoundException"

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryAlreadyExistsException"

-- | There is no dry run for this repository.
_LifecyclePolicyPreviewNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LifecyclePolicyPreviewNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LifecyclePolicyPreviewNotFoundException"

-- | The image is of a type that cannot be scanned.
_UnsupportedImageTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedImageTypeException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedImageTypeException"

-- | The specified repository contains images. To delete a repository that
-- contains images, you must force the deletion with the @force@ parameter.
_RepositoryNotEmptyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNotEmptyException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNotEmptyException"

-- | The specified layer is not available because it is not associated with
-- an image. Unassociated image layers may be cleaned up at any time.
_LayerInaccessibleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LayerInaccessibleException =
  Prelude._MatchServiceError
    defaultService
    "LayerInaccessibleException"

-- | The lifecycle policy could not be found, and no policy is set to the
-- repository.
_LifecyclePolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LifecyclePolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LifecyclePolicyNotFoundException"

-- | The operation failed due to a KMS exception.
_KmsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KmsException =
  Prelude._MatchServiceError
    defaultService
    "KmsException"

-- | These errors are usually caused by a server-side issue.
_ServerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServerException =
  Prelude._MatchServiceError
    defaultService
    "ServerException"

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LayerAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "LayerAlreadyExistsException"
