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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "MediaPackage",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "mediapackage",
      Prelude._svcVersion = "2017-10-12",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "MediaPackage",
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

-- | The requested resource does not exist.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | An unexpected error occurred.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 503

-- | An unexpected error occurred.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Prelude.hasStatus 500

-- | The client is not authorized to access the requested resource.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Prelude.hasStatus 403

-- | The parameters sent in the request are not valid.
_UnprocessableEntityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnprocessableEntityException =
  Prelude._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Prelude.hasStatus 422

-- | The client has exceeded their resource or throttling limits.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
