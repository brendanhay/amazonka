{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackageVOD.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackageVOD.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnprocessableEntityException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ServiceUnavailableException,

    -- * AdMarkers
    AdMarkers (..),

    -- * EncryptionMethod
    EncryptionMethod (..),

    -- * ManifestLayout
    ManifestLayout (..),

    -- * PeriodTriggersElement
    PeriodTriggersElement (..),

    -- * Profile
    Profile (..),

    -- * SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- * StreamOrder
    StreamOrder (..),

    -- * AssetShallow
    AssetShallow (..),
    newAssetShallow,
    assetShallow_resourceId,
    assetShallow_arn,
    assetShallow_createdAt,
    assetShallow_packagingGroupId,
    assetShallow_sourceArn,
    assetShallow_sourceRoleArn,
    assetShallow_id,
    assetShallow_tags,

    -- * Authorization
    Authorization (..),
    newAuthorization,
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- * CmafEncryption
    CmafEncryption (..),
    newCmafEncryption,
    cmafEncryption_constantInitializationVector,
    cmafEncryption_spekeKeyProvider,

    -- * CmafPackage
    CmafPackage (..),
    newCmafPackage,
    cmafPackage_includeEncoderConfigurationInSegments,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_encryption,
    cmafPackage_hlsManifests,

    -- * DashEncryption
    DashEncryption (..),
    newDashEncryption,
    dashEncryption_spekeKeyProvider,

    -- * DashManifest
    DashManifest (..),
    newDashManifest,
    dashManifest_minBufferTimeSeconds,
    dashManifest_manifestName,
    dashManifest_profile,
    dashManifest_streamSelection,
    dashManifest_manifestLayout,

    -- * DashPackage
    DashPackage (..),
    newDashPackage,
    dashPackage_includeEncoderConfigurationInSegments,
    dashPackage_segmentTemplateFormat,
    dashPackage_segmentDurationSeconds,
    dashPackage_encryption,
    dashPackage_periodTriggers,
    dashPackage_dashManifests,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    newEgressAccessLogs,
    egressAccessLogs_logGroupName,

    -- * EgressEndpoint
    EgressEndpoint (..),
    newEgressEndpoint,
    egressEndpoint_status,
    egressEndpoint_url,
    egressEndpoint_packagingConfigurationId,

    -- * HlsEncryption
    HlsEncryption (..),
    newHlsEncryption,
    hlsEncryption_encryptionMethod,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_spekeKeyProvider,

    -- * HlsManifest
    HlsManifest (..),
    newHlsManifest,
    hlsManifest_manifestName,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_streamSelection,
    hlsManifest_adMarkers,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_repeatExtXKey,

    -- * HlsPackage
    HlsPackage (..),
    newHlsPackage,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_encryption,
    hlsPackage_hlsManifests,

    -- * MssEncryption
    MssEncryption (..),
    newMssEncryption,
    mssEncryption_spekeKeyProvider,

    -- * MssManifest
    MssManifest (..),
    newMssManifest,
    mssManifest_manifestName,
    mssManifest_streamSelection,

    -- * MssPackage
    MssPackage (..),
    newMssPackage,
    mssPackage_segmentDurationSeconds,
    mssPackage_encryption,
    mssPackage_mssManifests,

    -- * PackagingConfiguration
    PackagingConfiguration (..),
    newPackagingConfiguration,
    packagingConfiguration_hlsPackage,
    packagingConfiguration_arn,
    packagingConfiguration_packagingGroupId,
    packagingConfiguration_dashPackage,
    packagingConfiguration_mssPackage,
    packagingConfiguration_id,
    packagingConfiguration_cmafPackage,
    packagingConfiguration_tags,

    -- * PackagingGroup
    PackagingGroup (..),
    newPackagingGroup,
    packagingGroup_arn,
    packagingGroup_authorization,
    packagingGroup_domainName,
    packagingGroup_id,
    packagingGroup_egressAccessLogs,
    packagingGroup_tags,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- * StreamSelection
    StreamSelection (..),
    newStreamSelection,
    streamSelection_streamOrder,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackageVOD.Types.AdMarkers
import Network.AWS.MediaPackageVOD.Types.AssetShallow
import Network.AWS.MediaPackageVOD.Types.Authorization
import Network.AWS.MediaPackageVOD.Types.CmafEncryption
import Network.AWS.MediaPackageVOD.Types.CmafPackage
import Network.AWS.MediaPackageVOD.Types.DashEncryption
import Network.AWS.MediaPackageVOD.Types.DashManifest
import Network.AWS.MediaPackageVOD.Types.DashPackage
import Network.AWS.MediaPackageVOD.Types.EgressAccessLogs
import Network.AWS.MediaPackageVOD.Types.EgressEndpoint
import Network.AWS.MediaPackageVOD.Types.EncryptionMethod
import Network.AWS.MediaPackageVOD.Types.HlsEncryption
import Network.AWS.MediaPackageVOD.Types.HlsManifest
import Network.AWS.MediaPackageVOD.Types.HlsPackage
import Network.AWS.MediaPackageVOD.Types.ManifestLayout
import Network.AWS.MediaPackageVOD.Types.MssEncryption
import Network.AWS.MediaPackageVOD.Types.MssManifest
import Network.AWS.MediaPackageVOD.Types.MssPackage
import Network.AWS.MediaPackageVOD.Types.PackagingConfiguration
import Network.AWS.MediaPackageVOD.Types.PackagingGroup
import Network.AWS.MediaPackageVOD.Types.PeriodTriggersElement
import Network.AWS.MediaPackageVOD.Types.Profile
import Network.AWS.MediaPackageVOD.Types.SegmentTemplateFormat
import Network.AWS.MediaPackageVOD.Types.SpekeKeyProvider
import Network.AWS.MediaPackageVOD.Types.StreamOrder
import Network.AWS.MediaPackageVOD.Types.StreamSelection
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-07@ of the Amazon Elemental MediaPackage VOD SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MediaPackageVOD",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mediapackage-vod",
      Core._serviceSigningName = "mediapackage-vod",
      Core._serviceVersion = "2018-11-07",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaPackageVOD",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The parameters sent in the request are not valid.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | The client is not authorized to access the requested resource.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The client has exceeded their resource or throttling limits.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | An unexpected error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | An unexpected error occurred.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503
