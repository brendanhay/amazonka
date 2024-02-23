{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECR.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _EmptyUploadException,
    _ImageAlreadyExistsException,
    _ImageDigestDoesNotMatchException,
    _ImageNotFoundException,
    _ImageTagAlreadyExistsException,
    _InvalidLayerException,
    _InvalidLayerPartException,
    _InvalidParameterException,
    _InvalidTagParameterException,
    _KmsException,
    _LayerAlreadyExistsException,
    _LayerInaccessibleException,
    _LayerPartTooSmallException,
    _LayersNotFoundException,
    _LifecyclePolicyNotFoundException,
    _LifecyclePolicyPreviewInProgressException,
    _LifecyclePolicyPreviewNotFoundException,
    _LimitExceededException,
    _PullThroughCacheRuleAlreadyExistsException,
    _PullThroughCacheRuleNotFoundException,
    _ReferencedImagesNotFoundException,
    _RegistryPolicyNotFoundException,
    _RepositoryAlreadyExistsException,
    _RepositoryNotEmptyException,
    _RepositoryNotFoundException,
    _RepositoryPolicyNotFoundException,
    _ScanNotFoundException,
    _ServerException,
    _TooManyTagsException,
    _UnsupportedImageTypeException,
    _UnsupportedUpstreamRegistryException,
    _UploadNotFoundException,
    _ValidationException,

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

    -- * ReplicationStatus
    ReplicationStatus (..),

    -- * RepositoryFilterType
    RepositoryFilterType (..),

    -- * ScanFrequency
    ScanFrequency (..),

    -- * ScanStatus
    ScanStatus (..),

    -- * ScanType
    ScanType (..),

    -- * ScanningConfigurationFailureCode
    ScanningConfigurationFailureCode (..),

    -- * ScanningRepositoryFilterType
    ScanningRepositoryFilterType (..),

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
    authorizationData_authorizationToken,
    authorizationData_expiresAt,
    authorizationData_proxyEndpoint,

    -- * AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (..),
    newAwsEcrContainerImageDetails,
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_author,
    awsEcrContainerImageDetails_imageHash,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_platform,
    awsEcrContainerImageDetails_pushedAt,
    awsEcrContainerImageDetails_registry,
    awsEcrContainerImageDetails_repositoryName,

    -- * CvssScore
    CvssScore (..),
    newCvssScore,
    cvssScore_baseScore,
    cvssScore_scoringVector,
    cvssScore_source,
    cvssScore_version,

    -- * CvssScoreAdjustment
    CvssScoreAdjustment (..),
    newCvssScoreAdjustment,
    cvssScoreAdjustment_metric,
    cvssScoreAdjustment_reason,

    -- * CvssScoreDetails
    CvssScoreDetails (..),
    newCvssScoreDetails,
    cvssScoreDetails_adjustments,
    cvssScoreDetails_score,
    cvssScoreDetails_scoreSource,
    cvssScoreDetails_scoringVector,
    cvssScoreDetails_version,

    -- * DescribeImagesFilter
    DescribeImagesFilter (..),
    newDescribeImagesFilter,
    describeImagesFilter_tagStatus,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionType,

    -- * EnhancedImageScanFinding
    EnhancedImageScanFinding (..),
    newEnhancedImageScanFinding,
    enhancedImageScanFinding_awsAccountId,
    enhancedImageScanFinding_description,
    enhancedImageScanFinding_findingArn,
    enhancedImageScanFinding_firstObservedAt,
    enhancedImageScanFinding_lastObservedAt,
    enhancedImageScanFinding_packageVulnerabilityDetails,
    enhancedImageScanFinding_remediation,
    enhancedImageScanFinding_resources,
    enhancedImageScanFinding_score,
    enhancedImageScanFinding_scoreDetails,
    enhancedImageScanFinding_severity,
    enhancedImageScanFinding_status,
    enhancedImageScanFinding_title,
    enhancedImageScanFinding_type,
    enhancedImageScanFinding_updatedAt,

    -- * Image
    Image (..),
    newImage,
    image_imageId,
    image_imageManifest,
    image_imageManifestMediaType,
    image_registryId,
    image_repositoryName,

    -- * ImageDetail
    ImageDetail (..),
    newImageDetail,
    imageDetail_artifactMediaType,
    imageDetail_imageDigest,
    imageDetail_imageManifestMediaType,
    imageDetail_imagePushedAt,
    imageDetail_imageScanFindingsSummary,
    imageDetail_imageScanStatus,
    imageDetail_imageSizeInBytes,
    imageDetail_imageTags,
    imageDetail_lastRecordedPullTime,
    imageDetail_registryId,
    imageDetail_repositoryName,

    -- * ImageFailure
    ImageFailure (..),
    newImageFailure,
    imageFailure_failureCode,
    imageFailure_failureReason,
    imageFailure_imageId,

    -- * ImageIdentifier
    ImageIdentifier (..),
    newImageIdentifier,
    imageIdentifier_imageDigest,
    imageIdentifier_imageTag,

    -- * ImageReplicationStatus
    ImageReplicationStatus (..),
    newImageReplicationStatus,
    imageReplicationStatus_failureCode,
    imageReplicationStatus_region,
    imageReplicationStatus_registryId,
    imageReplicationStatus_status,

    -- * ImageScanFinding
    ImageScanFinding (..),
    newImageScanFinding,
    imageScanFinding_attributes,
    imageScanFinding_description,
    imageScanFinding_name,
    imageScanFinding_severity,
    imageScanFinding_uri,

    -- * ImageScanFindings
    ImageScanFindings (..),
    newImageScanFindings,
    imageScanFindings_enhancedFindings,
    imageScanFindings_findingSeverityCounts,
    imageScanFindings_findings,
    imageScanFindings_imageScanCompletedAt,
    imageScanFindings_vulnerabilitySourceUpdatedAt,

    -- * ImageScanFindingsSummary
    ImageScanFindingsSummary (..),
    newImageScanFindingsSummary,
    imageScanFindingsSummary_findingSeverityCounts,
    imageScanFindingsSummary_imageScanCompletedAt,
    imageScanFindingsSummary_vulnerabilitySourceUpdatedAt,

    -- * ImageScanStatus
    ImageScanStatus (..),
    newImageScanStatus,
    imageScanStatus_description,
    imageScanStatus_status,

    -- * ImageScanningConfiguration
    ImageScanningConfiguration (..),
    newImageScanningConfiguration,
    imageScanningConfiguration_scanOnPush,

    -- * Layer
    Layer (..),
    newLayer,
    layer_layerAvailability,
    layer_layerDigest,
    layer_layerSize,
    layer_mediaType,

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
    lifecyclePolicyPreviewResult_action,
    lifecyclePolicyPreviewResult_appliedRulePriority,
    lifecyclePolicyPreviewResult_imageDigest,
    lifecyclePolicyPreviewResult_imagePushedAt,
    lifecyclePolicyPreviewResult_imageTags,

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

    -- * PackageVulnerabilityDetails
    PackageVulnerabilityDetails (..),
    newPackageVulnerabilityDetails,
    packageVulnerabilityDetails_cvss,
    packageVulnerabilityDetails_referenceUrls,
    packageVulnerabilityDetails_relatedVulnerabilities,
    packageVulnerabilityDetails_source,
    packageVulnerabilityDetails_sourceUrl,
    packageVulnerabilityDetails_vendorCreatedAt,
    packageVulnerabilityDetails_vendorSeverity,
    packageVulnerabilityDetails_vendorUpdatedAt,
    packageVulnerabilityDetails_vulnerabilityId,
    packageVulnerabilityDetails_vulnerablePackages,

    -- * PullThroughCacheRule
    PullThroughCacheRule (..),
    newPullThroughCacheRule,
    pullThroughCacheRule_createdAt,
    pullThroughCacheRule_ecrRepositoryPrefix,
    pullThroughCacheRule_registryId,
    pullThroughCacheRule_upstreamRegistryUrl,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_text,
    recommendation_url,

    -- * RegistryScanningConfiguration
    RegistryScanningConfiguration (..),
    newRegistryScanningConfiguration,
    registryScanningConfiguration_rules,
    registryScanningConfiguration_scanType,

    -- * RegistryScanningRule
    RegistryScanningRule (..),
    newRegistryScanningRule,
    registryScanningRule_scanFrequency,
    registryScanningRule_repositoryFilters,

    -- * Remediation
    Remediation (..),
    newRemediation,
    remediation_recommendation,

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
    replicationRule_repositoryFilters,
    replicationRule_destinations,

    -- * Repository
    Repository (..),
    newRepository,
    repository_createdAt,
    repository_encryptionConfiguration,
    repository_imageScanningConfiguration,
    repository_imageTagMutability,
    repository_registryId,
    repository_repositoryArn,
    repository_repositoryName,
    repository_repositoryUri,

    -- * RepositoryFilter
    RepositoryFilter (..),
    newRepositoryFilter,
    repositoryFilter_filter,
    repositoryFilter_filterType,

    -- * RepositoryScanningConfiguration
    RepositoryScanningConfiguration (..),
    newRepositoryScanningConfiguration,
    repositoryScanningConfiguration_appliedScanFilters,
    repositoryScanningConfiguration_repositoryArn,
    repositoryScanningConfiguration_repositoryName,
    repositoryScanningConfiguration_scanFrequency,
    repositoryScanningConfiguration_scanOnPush,

    -- * RepositoryScanningConfigurationFailure
    RepositoryScanningConfigurationFailure (..),
    newRepositoryScanningConfigurationFailure,
    repositoryScanningConfigurationFailure_failureCode,
    repositoryScanningConfigurationFailure_failureReason,
    repositoryScanningConfigurationFailure_repositoryName,

    -- * Resource
    Resource (..),
    newResource,
    resource_details,
    resource_id,
    resource_tags,
    resource_type,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_awsEcrContainerImage,

    -- * ScanningRepositoryFilter
    ScanningRepositoryFilter (..),
    newScanningRepositoryFilter,
    scanningRepositoryFilter_filter,
    scanningRepositoryFilter_filterType,

    -- * ScoreDetails
    ScoreDetails (..),
    newScoreDetails,
    scoreDetails_cvss,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VulnerablePackage
    VulnerablePackage (..),
    newVulnerablePackage,
    vulnerablePackage_arch,
    vulnerablePackage_epoch,
    vulnerablePackage_filePath,
    vulnerablePackage_name,
    vulnerablePackage_packageManager,
    vulnerablePackage_release,
    vulnerablePackage_sourceLayerHash,
    vulnerablePackage_version,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.Attribute
import Amazonka.ECR.Types.AuthorizationData
import Amazonka.ECR.Types.AwsEcrContainerImageDetails
import Amazonka.ECR.Types.CvssScore
import Amazonka.ECR.Types.CvssScoreAdjustment
import Amazonka.ECR.Types.CvssScoreDetails
import Amazonka.ECR.Types.DescribeImagesFilter
import Amazonka.ECR.Types.EncryptionConfiguration
import Amazonka.ECR.Types.EncryptionType
import Amazonka.ECR.Types.EnhancedImageScanFinding
import Amazonka.ECR.Types.FindingSeverity
import Amazonka.ECR.Types.Image
import Amazonka.ECR.Types.ImageActionType
import Amazonka.ECR.Types.ImageDetail
import Amazonka.ECR.Types.ImageFailure
import Amazonka.ECR.Types.ImageFailureCode
import Amazonka.ECR.Types.ImageIdentifier
import Amazonka.ECR.Types.ImageReplicationStatus
import Amazonka.ECR.Types.ImageScanFinding
import Amazonka.ECR.Types.ImageScanFindings
import Amazonka.ECR.Types.ImageScanFindingsSummary
import Amazonka.ECR.Types.ImageScanStatus
import Amazonka.ECR.Types.ImageScanningConfiguration
import Amazonka.ECR.Types.ImageTagMutability
import Amazonka.ECR.Types.Layer
import Amazonka.ECR.Types.LayerAvailability
import Amazonka.ECR.Types.LayerFailure
import Amazonka.ECR.Types.LayerFailureCode
import Amazonka.ECR.Types.LifecyclePolicyPreviewFilter
import Amazonka.ECR.Types.LifecyclePolicyPreviewResult
import Amazonka.ECR.Types.LifecyclePolicyPreviewStatus
import Amazonka.ECR.Types.LifecyclePolicyPreviewSummary
import Amazonka.ECR.Types.LifecyclePolicyRuleAction
import Amazonka.ECR.Types.ListImagesFilter
import Amazonka.ECR.Types.PackageVulnerabilityDetails
import Amazonka.ECR.Types.PullThroughCacheRule
import Amazonka.ECR.Types.Recommendation
import Amazonka.ECR.Types.RegistryScanningConfiguration
import Amazonka.ECR.Types.RegistryScanningRule
import Amazonka.ECR.Types.Remediation
import Amazonka.ECR.Types.ReplicationConfiguration
import Amazonka.ECR.Types.ReplicationDestination
import Amazonka.ECR.Types.ReplicationRule
import Amazonka.ECR.Types.ReplicationStatus
import Amazonka.ECR.Types.Repository
import Amazonka.ECR.Types.RepositoryFilter
import Amazonka.ECR.Types.RepositoryFilterType
import Amazonka.ECR.Types.RepositoryScanningConfiguration
import Amazonka.ECR.Types.RepositoryScanningConfigurationFailure
import Amazonka.ECR.Types.Resource
import Amazonka.ECR.Types.ResourceDetails
import Amazonka.ECR.Types.ScanFrequency
import Amazonka.ECR.Types.ScanStatus
import Amazonka.ECR.Types.ScanType
import Amazonka.ECR.Types.ScanningConfigurationFailureCode
import Amazonka.ECR.Types.ScanningRepositoryFilter
import Amazonka.ECR.Types.ScanningRepositoryFilterType
import Amazonka.ECR.Types.ScoreDetails
import Amazonka.ECR.Types.Tag
import Amazonka.ECR.Types.TagStatus
import Amazonka.ECR.Types.VulnerablePackage
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-09-21@ of the Amazon EC2 Container Registry SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ECR",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.ecr",
      Core.signingName = "ecr",
      Core.version = "2015-09-21",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ECR",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified layer upload does not contain any layer parts.
_EmptyUploadException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EmptyUploadException =
  Core._MatchServiceError
    defaultService
    "EmptyUploadException"

-- | The specified image has already been pushed, and there were no changes
-- to the manifest or image tag after the last push.
_ImageAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ImageAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ImageAlreadyExistsException"

-- | The specified image digest does not match the digest that Amazon ECR
-- calculated for the image.
_ImageDigestDoesNotMatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ImageDigestDoesNotMatchException =
  Core._MatchServiceError
    defaultService
    "ImageDigestDoesNotMatchException"

-- | The image requested does not exist in the specified repository.
_ImageNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ImageNotFoundException =
  Core._MatchServiceError
    defaultService
    "ImageNotFoundException"

-- | The specified image is tagged with a tag that already exists. The
-- repository is configured for tag immutability.
_ImageTagAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ImageTagAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ImageTagAlreadyExistsException"

-- | The layer digest calculation performed by Amazon ECR upon receipt of the
-- image layer does not match the digest specified.
_InvalidLayerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLayerException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerException"

-- | The layer part size is not valid, or the first byte specified is not
-- consecutive to the last byte of a previous layer part upload.
_InvalidLayerPartException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLayerPartException =
  Core._MatchServiceError
    defaultService
    "InvalidLayerPartException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | An invalid parameter has been specified. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
_InvalidTagParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | The operation failed due to a KMS exception.
_KmsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KmsException =
  Core._MatchServiceError
    defaultService
    "KmsException"

-- | The image layer already exists in the associated repository.
_LayerAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LayerAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "LayerAlreadyExistsException"

-- | The specified layer is not available because it is not associated with
-- an image. Unassociated image layers may be cleaned up at any time.
_LayerInaccessibleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LayerInaccessibleException =
  Core._MatchServiceError
    defaultService
    "LayerInaccessibleException"

-- | Layer parts must be at least 5 MiB in size.
_LayerPartTooSmallException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LayerPartTooSmallException =
  Core._MatchServiceError
    defaultService
    "LayerPartTooSmallException"

-- | The specified layers could not be found, or the specified layer is not
-- valid for this repository.
_LayersNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LayersNotFoundException =
  Core._MatchServiceError
    defaultService
    "LayersNotFoundException"

-- | The lifecycle policy could not be found, and no policy is set to the
-- repository.
_LifecyclePolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LifecyclePolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "LifecyclePolicyNotFoundException"

-- | The previous lifecycle policy preview request has not completed. Wait
-- and try again.
_LifecyclePolicyPreviewInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LifecyclePolicyPreviewInProgressException =
  Core._MatchServiceError
    defaultService
    "LifecyclePolicyPreviewInProgressException"

-- | There is no dry run for this repository.
_LifecyclePolicyPreviewNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LifecyclePolicyPreviewNotFoundException =
  Core._MatchServiceError
    defaultService
    "LifecyclePolicyPreviewNotFoundException"

-- | The operation did not succeed because it would have exceeded a service
-- limit for your account. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/service-quotas.html Amazon ECR service quotas>
-- in the Amazon Elastic Container Registry User Guide.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A pull through cache rule with these settings already exists for the
-- private registry.
_PullThroughCacheRuleAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PullThroughCacheRuleAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "PullThroughCacheRuleAlreadyExistsException"

-- | The pull through cache rule was not found. Specify a valid pull through
-- cache rule and try again.
_PullThroughCacheRuleNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PullThroughCacheRuleNotFoundException =
  Core._MatchServiceError
    defaultService
    "PullThroughCacheRuleNotFoundException"

-- | The manifest list is referencing an image that does not exist.
_ReferencedImagesNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReferencedImagesNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReferencedImagesNotFoundException"

-- | The registry doesn\'t have an associated registry policy.
_RegistryPolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RegistryPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "RegistryPolicyNotFoundException"

-- | The specified repository already exists in the specified registry.
_RepositoryAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RepositoryAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "RepositoryAlreadyExistsException"

-- | The specified repository contains images. To delete a repository that
-- contains images, you must force the deletion with the @force@ parameter.
_RepositoryNotEmptyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RepositoryNotEmptyException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotEmptyException"

-- | The specified repository could not be found. Check the spelling of the
-- specified repository and ensure that you are performing operations on
-- the correct registry.
_RepositoryNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RepositoryNotFoundException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotFoundException"

-- | The specified repository and registry combination does not have an
-- associated repository policy.
_RepositoryPolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RepositoryPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "RepositoryPolicyNotFoundException"

-- | The specified image scan could not be found. Ensure that image scanning
-- is enabled on the repository and try again.
_ScanNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScanNotFoundException =
  Core._MatchServiceError
    defaultService
    "ScanNotFoundException"

-- | These errors are usually caused by a server-side issue.
_ServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The image is of a type that cannot be scanned.
_UnsupportedImageTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedImageTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedImageTypeException"

-- | The specified upstream registry isn\'t supported.
_UnsupportedUpstreamRegistryException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedUpstreamRegistryException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUpstreamRegistryException"

-- | The upload could not be found, or the specified upload ID is not valid
-- for this repository.
_UploadNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UploadNotFoundException =
  Core._MatchServiceError
    defaultService
    "UploadNotFoundException"

-- | There was an exception validating this request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
