{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _ServiceUnavailableException,
    _InternalServerErrorException,
    _ForbiddenException,
    _UnprocessableEntityException,
    _TooManyRequestsException,

    -- * AdMarkers
    AdMarkers (..),

    -- * AdTriggersElement
    AdTriggersElement (..),

    -- * AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

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
    channel_egressAccessLogs,
    channel_hlsIngest,
    channel_arn,
    channel_id,
    channel_ingressAccessLogs,
    channel_tags,
    channel_description,

    -- * CmafEncryption
    CmafEncryption (..),
    newCmafEncryption,
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_spekeKeyProvider,

    -- * CmafPackage
    CmafPackage (..),
    newCmafPackage,
    cmafPackage_streamSelection,
    cmafPackage_hlsManifests,
    cmafPackage_segmentPrefix,
    cmafPackage_encryption,
    cmafPackage_segmentDurationSeconds,

    -- * CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (..),
    newCmafPackageCreateOrUpdateParameters,
    cmafPackageCreateOrUpdateParameters_streamSelection,
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,
    cmafPackageCreateOrUpdateParameters_encryption,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,

    -- * DashEncryption
    DashEncryption (..),
    newDashEncryption,
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- * DashPackage
    DashPackage (..),
    newDashPackage,
    dashPackage_minBufferTimeSeconds,
    dashPackage_streamSelection,
    dashPackage_periodTriggers,
    dashPackage_adTriggers,
    dashPackage_manifestWindowSeconds,
    dashPackage_manifestLayout,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_encryption,
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_utcTimingUri,
    dashPackage_segmentDurationSeconds,
    dashPackage_profile,
    dashPackage_segmentTemplateFormat,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_utcTiming,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    newEgressAccessLogs,
    egressAccessLogs_logGroupName,

    -- * HarvestJob
    HarvestJob (..),
    newHarvestJob,
    harvestJob_status,
    harvestJob_s3Destination,
    harvestJob_channelId,
    harvestJob_startTime,
    harvestJob_arn,
    harvestJob_id,
    harvestJob_createdAt,
    harvestJob_originEndpointId,
    harvestJob_endTime,

    -- * HlsEncryption
    HlsEncryption (..),
    newHlsEncryption,
    hlsEncryption_repeatExtXKey,
    hlsEncryption_encryptionMethod,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_spekeKeyProvider,

    -- * HlsIngest
    HlsIngest (..),
    newHlsIngest,
    hlsIngest_ingestEndpoints,

    -- * HlsManifest
    HlsManifest (..),
    newHlsManifest,
    hlsManifest_adMarkers,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_playlistWindowSeconds,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_manifestName,
    hlsManifest_playlistType,
    hlsManifest_url,
    hlsManifest_id,

    -- * HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters (..),
    newHlsManifestCreateOrUpdateParameters,
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_id,

    -- * HlsPackage
    HlsPackage (..),
    newHlsPackage,
    hlsPackage_adMarkers,
    hlsPackage_streamSelection,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_playlistWindowSeconds,
    hlsPackage_adTriggers,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_encryption,
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_playlistType,

    -- * IngestEndpoint
    IngestEndpoint (..),
    newIngestEndpoint,
    ingestEndpoint_id,
    ingestEndpoint_password,
    ingestEndpoint_username,
    ingestEndpoint_url,

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
    mssPackage_encryption,
    mssPackage_segmentDurationSeconds,

    -- * OriginEndpoint
    OriginEndpoint (..),
    newOriginEndpoint,
    originEndpoint_dashPackage,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_origination,
    originEndpoint_channelId,
    originEndpoint_cmafPackage,
    originEndpoint_manifestName,
    originEndpoint_arn,
    originEndpoint_id,
    originEndpoint_whitelist,
    originEndpoint_mssPackage,
    originEndpoint_tags,
    originEndpoint_description,
    originEndpoint_timeDelaySeconds,
    originEndpoint_authorization,
    originEndpoint_url,
    originEndpoint_hlsPackage,

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
    spekeKeyProvider_resourceId,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- * StreamSelection
    StreamSelection (..),
    newStreamSelection,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
    streamSelection_streamOrder,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.Authorization
import Network.AWS.MediaPackage.Types.Channel
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.CmafPackage
import Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.DashEncryption
import Network.AWS.MediaPackage.Types.DashPackage
import Network.AWS.MediaPackage.Types.EgressAccessLogs
import Network.AWS.MediaPackage.Types.EncryptionMethod
import Network.AWS.MediaPackage.Types.HarvestJob
import Network.AWS.MediaPackage.Types.HlsEncryption
import Network.AWS.MediaPackage.Types.HlsIngest
import Network.AWS.MediaPackage.Types.HlsManifest
import Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.HlsPackage
import Network.AWS.MediaPackage.Types.IngestEndpoint
import Network.AWS.MediaPackage.Types.IngressAccessLogs
import Network.AWS.MediaPackage.Types.ManifestLayout
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.MssPackage
import Network.AWS.MediaPackage.Types.OriginEndpoint
import Network.AWS.MediaPackage.Types.Origination
import Network.AWS.MediaPackage.Types.PeriodTriggersElement
import Network.AWS.MediaPackage.Types.PlaylistType
import Network.AWS.MediaPackage.Types.Profile
import Network.AWS.MediaPackage.Types.S3Destination
import Network.AWS.MediaPackage.Types.SegmentTemplateFormat
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import Network.AWS.MediaPackage.Types.Status
import Network.AWS.MediaPackage.Types.StreamOrder
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.MediaPackage.Types.UtcTiming
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MediaPackage",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mediapackage",
      Core._serviceSigningName = "mediapackage",
      Core._serviceVersion = "2017-10-12",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaPackage",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | An unexpected error occurred.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Core.. Core.hasStatus 503

-- | An unexpected error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Core.. Core.hasStatus 500

-- | The client is not authorized to access the requested resource.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Core.. Core.hasStatus 403

-- | The parameters sent in the request are not valid.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Core.. Core.hasStatus 422

-- | The client has exceeded their resource or throttling limits.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429
