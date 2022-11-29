{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackage.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types
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

    -- * AdTriggersElement
    AdTriggersElement (..),

    -- * AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

    -- * CmafEncryptionMethod
    CmafEncryptionMethod (..),

    -- * EncryptionMethod
    EncryptionMethod (..),

    -- * ManifestLayout
    ManifestLayout (..),

    -- * Origination
    Origination (..),

    -- * PeriodTriggersElement
    PeriodTriggersElement (..),

    -- * PlaylistType
    PlaylistType (..),

    -- * PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- * PresetSpeke20Video
    PresetSpeke20Video (..),

    -- * Profile
    Profile (..),

    -- * SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- * Status
    Status (..),

    -- * StreamOrder
    StreamOrder (..),

    -- * UtcTiming
    UtcTiming (..),

    -- * Authorization
    Authorization (..),
    newAuthorization,
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- * Channel
    Channel (..),
    newChannel,
    channel_tags,
    channel_ingressAccessLogs,
    channel_arn,
    channel_id,
    channel_description,
    channel_egressAccessLogs,
    channel_hlsIngest,

    -- * CmafEncryption
    CmafEncryption (..),
    newCmafEncryption,
    cmafEncryption_constantInitializationVector,
    cmafEncryption_encryptionMethod,
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_spekeKeyProvider,

    -- * CmafPackage
    CmafPackage (..),
    newCmafPackage,
    cmafPackage_streamSelection,
    cmafPackage_hlsManifests,
    cmafPackage_segmentPrefix,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_encryption,

    -- * CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (..),
    newCmafPackageCreateOrUpdateParameters,
    cmafPackageCreateOrUpdateParameters_streamSelection,
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,
    cmafPackageCreateOrUpdateParameters_encryption,

    -- * DashEncryption
    DashEncryption (..),
    newDashEncryption,
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- * DashPackage
    DashPackage (..),
    newDashPackage,
    dashPackage_profile,
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_segmentTemplateFormat,
    dashPackage_streamSelection,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_includeIframeOnlyStream,
    dashPackage_manifestWindowSeconds,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_adTriggers,
    dashPackage_segmentDurationSeconds,
    dashPackage_periodTriggers,
    dashPackage_utcTiming,
    dashPackage_encryption,
    dashPackage_manifestLayout,
    dashPackage_minBufferTimeSeconds,
    dashPackage_utcTimingUri,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    newEgressAccessLogs,
    egressAccessLogs_logGroupName,

    -- * EncryptionContractConfiguration
    EncryptionContractConfiguration (..),
    newEncryptionContractConfiguration,
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- * HarvestJob
    HarvestJob (..),
    newHarvestJob,
    harvestJob_arn,
    harvestJob_status,
    harvestJob_endTime,
    harvestJob_id,
    harvestJob_channelId,
    harvestJob_s3Destination,
    harvestJob_originEndpointId,
    harvestJob_createdAt,
    harvestJob_startTime,

    -- * HlsEncryption
    HlsEncryption (..),
    newHlsEncryption,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_repeatExtXKey,
    hlsEncryption_encryptionMethod,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_spekeKeyProvider,

    -- * HlsIngest
    HlsIngest (..),
    newHlsIngest,
    hlsIngest_ingestEndpoints,

    -- * HlsManifest
    HlsManifest (..),
    newHlsManifest,
    hlsManifest_playlistWindowSeconds,
    hlsManifest_adsOnDeliveryRestrictions,
    hlsManifest_url,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_adMarkers,
    hlsManifest_adTriggers,
    hlsManifest_manifestName,
    hlsManifest_playlistType,
    hlsManifest_id,

    -- * HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters (..),
    newHlsManifestCreateOrUpdateParameters,
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_id,

    -- * HlsPackage
    HlsPackage (..),
    newHlsPackage,
    hlsPackage_playlistWindowSeconds,
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_streamSelection,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_adMarkers,
    hlsPackage_adTriggers,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_encryption,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_playlistType,

    -- * IngestEndpoint
    IngestEndpoint (..),
    newIngestEndpoint,
    ingestEndpoint_password,
    ingestEndpoint_username,
    ingestEndpoint_url,
    ingestEndpoint_id,

    -- * IngressAccessLogs
    IngressAccessLogs (..),
    newIngressAccessLogs,
    ingressAccessLogs_logGroupName,

    -- * MssEncryption
    MssEncryption (..),
    newMssEncryption,
    mssEncryption_spekeKeyProvider,

    -- * MssPackage
    MssPackage (..),
    newMssPackage,
    mssPackage_streamSelection,
    mssPackage_manifestWindowSeconds,
    mssPackage_segmentDurationSeconds,
    mssPackage_encryption,

    -- * OriginEndpoint
    OriginEndpoint (..),
    newOriginEndpoint,
    originEndpoint_tags,
    originEndpoint_timeDelaySeconds,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_mssPackage,
    originEndpoint_arn,
    originEndpoint_whitelist,
    originEndpoint_url,
    originEndpoint_id,
    originEndpoint_description,
    originEndpoint_manifestName,
    originEndpoint_channelId,
    originEndpoint_authorization,
    originEndpoint_dashPackage,
    originEndpoint_cmafPackage,
    originEndpoint_hlsPackage,
    originEndpoint_origination,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_manifestKey,
    s3Destination_bucketName,
    s3Destination_roleArn,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_resourceId,
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
import Amazonka.MediaPackage.Types.AdMarkers
import Amazonka.MediaPackage.Types.AdTriggersElement
import Amazonka.MediaPackage.Types.AdsOnDeliveryRestrictions
import Amazonka.MediaPackage.Types.Authorization
import Amazonka.MediaPackage.Types.Channel
import Amazonka.MediaPackage.Types.CmafEncryption
import Amazonka.MediaPackage.Types.CmafEncryptionMethod
import Amazonka.MediaPackage.Types.CmafPackage
import Amazonka.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
import Amazonka.MediaPackage.Types.DashEncryption
import Amazonka.MediaPackage.Types.DashPackage
import Amazonka.MediaPackage.Types.EgressAccessLogs
import Amazonka.MediaPackage.Types.EncryptionContractConfiguration
import Amazonka.MediaPackage.Types.EncryptionMethod
import Amazonka.MediaPackage.Types.HarvestJob
import Amazonka.MediaPackage.Types.HlsEncryption
import Amazonka.MediaPackage.Types.HlsIngest
import Amazonka.MediaPackage.Types.HlsManifest
import Amazonka.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Amazonka.MediaPackage.Types.HlsPackage
import Amazonka.MediaPackage.Types.IngestEndpoint
import Amazonka.MediaPackage.Types.IngressAccessLogs
import Amazonka.MediaPackage.Types.ManifestLayout
import Amazonka.MediaPackage.Types.MssEncryption
import Amazonka.MediaPackage.Types.MssPackage
import Amazonka.MediaPackage.Types.OriginEndpoint
import Amazonka.MediaPackage.Types.Origination
import Amazonka.MediaPackage.Types.PeriodTriggersElement
import Amazonka.MediaPackage.Types.PlaylistType
import Amazonka.MediaPackage.Types.PresetSpeke20Audio
import Amazonka.MediaPackage.Types.PresetSpeke20Video
import Amazonka.MediaPackage.Types.Profile
import Amazonka.MediaPackage.Types.S3Destination
import Amazonka.MediaPackage.Types.SegmentTemplateFormat
import Amazonka.MediaPackage.Types.SpekeKeyProvider
import Amazonka.MediaPackage.Types.Status
import Amazonka.MediaPackage.Types.StreamOrder
import Amazonka.MediaPackage.Types.StreamSelection
import Amazonka.MediaPackage.Types.UtcTiming
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaPackage",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mediapackage",
      Core.signingName = "mediapackage",
      Core.version = "2017-10-12",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaPackage",
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
