{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types
  ( -- * Service Configuration
    ecr,

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
    Attribute,
    attribute,
    aValue,
    aKey,

    -- * AuthorizationData
    AuthorizationData,
    authorizationData,
    adExpiresAt,
    adProxyEndpoint,
    adAuthorizationToken,

    -- * DescribeImagesFilter
    DescribeImagesFilter,
    describeImagesFilter,
    difTagStatus,

    -- * EncryptionConfiguration
    EncryptionConfiguration,
    encryptionConfiguration,
    ecKmsKey,
    ecEncryptionType,

    -- * Image
    Image,
    image,
    iRegistryId,
    iImageManifestMediaType,
    iImageId,
    iRepositoryName,
    iImageManifest,

    -- * ImageDetail
    ImageDetail,
    imageDetail,
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
    ImageFailure,
    imageFailure,
    ifFailureReason,
    ifFailureCode,
    ifImageId,

    -- * ImageIdentifier
    ImageIdentifier,
    imageIdentifier,
    iiImageDigest,
    iiImageTag,

    -- * ImageScanFinding
    ImageScanFinding,
    imageScanFinding,
    isfSeverity,
    isfUri,
    isfName,
    isfAttributes,
    isfDescription,

    -- * ImageScanFindings
    ImageScanFindings,
    imageScanFindings,
    isfImageScanCompletedAt,
    isfFindings,
    isfFindingSeverityCounts,
    isfVulnerabilitySourceUpdatedAt,

    -- * ImageScanFindingsSummary
    ImageScanFindingsSummary,
    imageScanFindingsSummary,
    isfsImageScanCompletedAt,
    isfsFindingSeverityCounts,
    isfsVulnerabilitySourceUpdatedAt,

    -- * ImageScanStatus
    ImageScanStatus,
    imageScanStatus,
    issStatus,
    issDescription,

    -- * ImageScanningConfiguration
    ImageScanningConfiguration,
    imageScanningConfiguration,
    iscScanOnPush,

    -- * Layer
    Layer,
    layer,
    lMediaType,
    lLayerDigest,
    lLayerSize,
    lLayerAvailability,

    -- * LayerFailure
    LayerFailure,
    layerFailure,
    lfFailureReason,
    lfFailureCode,
    lfLayerDigest,

    -- * LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter,
    lifecyclePolicyPreviewFilter,
    lppfTagStatus,

    -- * LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult,
    lifecyclePolicyPreviewResult,
    lpprImageTags,
    lpprAction,
    lpprImageDigest,
    lpprImagePushedAt,
    lpprAppliedRulePriority,

    -- * LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary,
    lifecyclePolicyPreviewSummary,
    lppsExpiringImageTotalCount,

    -- * LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction,
    lifecyclePolicyRuleAction,
    lpraType,

    -- * ListImagesFilter
    ListImagesFilter,
    listImagesFilter,
    lifTagStatus,

    -- * Repository
    Repository,
    repository,
    rRepositoryARN,
    rCreatedAt,
    rRegistryId,
    rImageScanningConfiguration,
    rRepositoryURI,
    rEncryptionConfiguration,
    rRepositoryName,
    rImageTagMutability,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-09-21@ of the Amazon EC2 Container Registry SDK configuration.
ecr :: Service
ecr =
  Service
    { _svcAbbrev = "ECR",
      _svcSigner = v4,
      _svcPrefix = "api.ecr",
      _svcVersion = "2015-09-21",
      _svcEndpoint = defaultEndpoint ecr,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ECR",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
