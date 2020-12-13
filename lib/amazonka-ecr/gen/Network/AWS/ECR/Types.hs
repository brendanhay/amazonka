-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types
  ( -- * Service configuration
    ecrService,

    -- * Errors

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
    mkAttribute,
    aValue,
    aKey,

    -- * AuthorizationData
    AuthorizationData (..),
    mkAuthorizationData,
    adExpiresAt,
    adProxyEndpoint,
    adAuthorizationToken,

    -- * DescribeImagesFilter
    DescribeImagesFilter (..),
    mkDescribeImagesFilter,
    difTagStatus,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecEncryptionType,
    ecKmsKey,

    -- * Image
    Image (..),
    mkImage,
    iRegistryId,
    iImageManifestMediaType,
    iImageId,
    iRepositoryName,
    iImageManifest,

    -- * ImageDetail
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

    -- * ImageFailure
    ImageFailure (..),
    mkImageFailure,
    ifFailureReason,
    ifFailureCode,
    ifImageId,

    -- * ImageIdentifier
    ImageIdentifier (..),
    mkImageIdentifier,
    iiImageDigest,
    iiImageTag,

    -- * ImageScanFinding
    ImageScanFinding (..),
    mkImageScanFinding,
    isfSeverity,
    isfUri,
    isfName,
    isfAttributes,
    isfDescription,

    -- * ImageScanFindings
    ImageScanFindings (..),
    mkImageScanFindings,
    isfImageScanCompletedAt,
    isfFindings,
    isfFindingSeverityCounts,
    isfVulnerabilitySourceUpdatedAt,

    -- * ImageScanFindingsSummary
    ImageScanFindingsSummary (..),
    mkImageScanFindingsSummary,
    isfsImageScanCompletedAt,
    isfsFindingSeverityCounts,
    isfsVulnerabilitySourceUpdatedAt,

    -- * ImageScanStatus
    ImageScanStatus (..),
    mkImageScanStatus,
    issStatus,
    issDescription,

    -- * ImageScanningConfiguration
    ImageScanningConfiguration (..),
    mkImageScanningConfiguration,
    iscScanOnPush,

    -- * Layer
    Layer (..),
    mkLayer,
    lMediaType,
    lLayerDigest,
    lLayerSize,
    lLayerAvailability,

    -- * LayerFailure
    LayerFailure (..),
    mkLayerFailure,
    lfFailureReason,
    lfFailureCode,
    lfLayerDigest,

    -- * LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter (..),
    mkLifecyclePolicyPreviewFilter,
    lppfTagStatus,

    -- * LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult (..),
    mkLifecyclePolicyPreviewResult,
    lpprImageTags,
    lpprAction,
    lpprImageDigest,
    lpprImagePushedAt,
    lpprAppliedRulePriority,

    -- * LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary (..),
    mkLifecyclePolicyPreviewSummary,
    lppsExpiringImageTotalCount,

    -- * LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction (..),
    mkLifecyclePolicyRuleAction,
    lpraType,

    -- * ListImagesFilter
    ListImagesFilter (..),
    mkListImagesFilter,
    lifTagStatus,

    -- * Repository
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

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import Network.AWS.ECR.Types.Repository
import Network.AWS.ECR.Types.ScanStatus
import Network.AWS.ECR.Types.Tag
import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-09-21@ of the Amazon EC2 Container Registry SDK configuration.
ecrService :: Lude.Service
ecrService =
  Lude.Service
    { Lude._svcAbbrev = "ECR",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "api.ecr",
      Lude._svcVersion = "2015-09-21",
      Lude._svcEndpoint = Lude.defaultEndpoint ecrService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ECR",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
