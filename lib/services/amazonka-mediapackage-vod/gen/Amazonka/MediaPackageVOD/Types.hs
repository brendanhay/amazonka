{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageVOD.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _ServiceUnavailableException,
    _InternalServerErrorException,
    _UnprocessableEntityException,
    _ForbiddenException,
    _TooManyRequestsException,

    -- * AdMarkers
    AdMarkers (..),

    -- * EncryptionMethod
    EncryptionMethod (..),

    -- * ManifestLayout
    ManifestLayout (..),

    -- * PeriodTriggersElement
    PeriodTriggersElement (..),

    -- * PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- * PresetSpeke20Video
    PresetSpeke20Video (..),

    -- * Profile
    Profile (..),

    -- * ScteMarkersSource
    ScteMarkersSource (..),

    -- * SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- * StreamOrder
    StreamOrder (..),

    -- * AssetShallow
    AssetShallow (..),
    newAssetShallow,
    assetShallow_resourceId,
    assetShallow_tags,
    assetShallow_sourceRoleArn,
    assetShallow_sourceArn,
    assetShallow_packagingGroupId,
    assetShallow_arn,
    assetShallow_id,
    assetShallow_createdAt,

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
    cmafPackage_segmentDurationSeconds,
    cmafPackage_encryption,
    cmafPackage_includeEncoderConfigurationInSegments,
    cmafPackage_hlsManifests,

    -- * DashEncryption
    DashEncryption (..),
    newDashEncryption,
    dashEncryption_spekeKeyProvider,

    -- * DashManifest
    DashManifest (..),
    newDashManifest,
    dashManifest_profile,
    dashManifest_streamSelection,
    dashManifest_manifestName,
    dashManifest_manifestLayout,
    dashManifest_scteMarkersSource,
    dashManifest_minBufferTimeSeconds,

    -- * DashPackage
    DashPackage (..),
    newDashPackage,
    dashPackage_segmentTemplateFormat,
    dashPackage_includeIframeOnlyStream,
    dashPackage_segmentDurationSeconds,
    dashPackage_periodTriggers,
    dashPackage_encryption,
    dashPackage_includeEncoderConfigurationInSegments,
    dashPackage_dashManifests,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    newEgressAccessLogs,
    egressAccessLogs_logGroupName,

    -- * EgressEndpoint
    EgressEndpoint (..),
    newEgressEndpoint,
    egressEndpoint_url,
    egressEndpoint_status,
    egressEndpoint_packagingConfigurationId,

    -- * EncryptionContractConfiguration
    EncryptionContractConfiguration (..),
    newEncryptionContractConfiguration,
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- * HlsEncryption
    HlsEncryption (..),
    newHlsEncryption,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_encryptionMethod,
    hlsEncryption_spekeKeyProvider,

    -- * HlsManifest
    HlsManifest (..),
    newHlsManifest,
    hlsManifest_streamSelection,
    hlsManifest_repeatExtXKey,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_adMarkers,
    hlsManifest_manifestName,

    -- * HlsPackage
    HlsPackage (..),
    newHlsPackage,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_encryption,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_hlsManifests,

    -- * MssEncryption
    MssEncryption (..),
    newMssEncryption,
    mssEncryption_spekeKeyProvider,

    -- * MssManifest
    MssManifest (..),
    newMssManifest,
    mssManifest_streamSelection,
    mssManifest_manifestName,

    -- * MssPackage
    MssPackage (..),
    newMssPackage,
    mssPackage_segmentDurationSeconds,
    mssPackage_encryption,
    mssPackage_mssManifests,

    -- * PackagingConfiguration
    PackagingConfiguration (..),
    newPackagingConfiguration,
    packagingConfiguration_tags,
    packagingConfiguration_mssPackage,
    packagingConfiguration_packagingGroupId,
    packagingConfiguration_arn,
    packagingConfiguration_id,
    packagingConfiguration_dashPackage,
    packagingConfiguration_cmafPackage,
    packagingConfiguration_hlsPackage,

    -- * PackagingGroup
    PackagingGroup (..),
    newPackagingGroup,
    packagingGroup_tags,
    packagingGroup_domainName,
    packagingGroup_arn,
    packagingGroup_id,
    packagingGroup_authorization,
    packagingGroup_egressAccessLogs,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_encryptionContractConfiguration,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types.AdMarkers
import Amazonka.MediaPackageVOD.Types.AssetShallow
import Amazonka.MediaPackageVOD.Types.Authorization
import Amazonka.MediaPackageVOD.Types.CmafEncryption
import Amazonka.MediaPackageVOD.Types.CmafPackage
import Amazonka.MediaPackageVOD.Types.DashEncryption
import Amazonka.MediaPackageVOD.Types.DashManifest
import Amazonka.MediaPackageVOD.Types.DashPackage
import Amazonka.MediaPackageVOD.Types.EgressAccessLogs
import Amazonka.MediaPackageVOD.Types.EgressEndpoint
import Amazonka.MediaPackageVOD.Types.EncryptionContractConfiguration
import Amazonka.MediaPackageVOD.Types.EncryptionMethod
import Amazonka.MediaPackageVOD.Types.HlsEncryption
import Amazonka.MediaPackageVOD.Types.HlsManifest
import Amazonka.MediaPackageVOD.Types.HlsPackage
import Amazonka.MediaPackageVOD.Types.ManifestLayout
import Amazonka.MediaPackageVOD.Types.MssEncryption
import Amazonka.MediaPackageVOD.Types.MssManifest
import Amazonka.MediaPackageVOD.Types.MssPackage
import Amazonka.MediaPackageVOD.Types.PackagingConfiguration
import Amazonka.MediaPackageVOD.Types.PackagingGroup
import Amazonka.MediaPackageVOD.Types.PeriodTriggersElement
import Amazonka.MediaPackageVOD.Types.PresetSpeke20Audio
import Amazonka.MediaPackageVOD.Types.PresetSpeke20Video
import Amazonka.MediaPackageVOD.Types.Profile
import Amazonka.MediaPackageVOD.Types.ScteMarkersSource
import Amazonka.MediaPackageVOD.Types.SegmentTemplateFormat
import Amazonka.MediaPackageVOD.Types.SpekeKeyProvider
import Amazonka.MediaPackageVOD.Types.StreamOrder
import Amazonka.MediaPackageVOD.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-07@ of the Amazon Elemental MediaPackage VOD SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaPackageVOD",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mediapackage-vod",
      Core.signingName = "mediapackage-vod",
      Core.version = "2018-11-07",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaPackageVOD",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | An unexpected error occurred.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | An unexpected error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

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

-- | The client has exceeded their resource or throttling limits.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
