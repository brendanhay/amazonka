-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _UnprocessableEntityException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ServiceUnavailableException,

    -- * AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

    -- * Status
    Status (..),

    -- * IngressAccessLogs
    IngressAccessLogs (..),
    mkIngressAccessLogs,
    ialLogGroupName,

    -- * UtcTiming
    UtcTiming (..),

    -- * MssEncryption
    MssEncryption (..),
    mkMssEncryption,
    meSpekeKeyProvider,

    -- * StreamOrder
    StreamOrder (..),

    -- * HlsIngest
    HlsIngest (..),
    mkHlsIngest,
    hiIngestEndpoints,

    -- * HlsPackage
    HlsPackage (..),
    mkHlsPackage,
    hpAdMarkers,
    hpAdTriggers,
    hpAdsOnDeliveryRestrictions,
    hpEncryption,
    hpIncludeIframeOnlyStream,
    hpPlaylistType,
    hpPlaylistWindowSeconds,
    hpProgramDateTimeIntervalSeconds,
    hpSegmentDurationSeconds,
    hpStreamSelection,
    hpUseAudioRenditionGroup,

    -- * EncryptionMethod
    EncryptionMethod (..),

    -- * SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- * Authorization
    Authorization (..),
    mkAuthorization,
    aSecretsRoleArn,
    aCdnIdentifierSecret,

    -- * Channel
    Channel (..),
    mkChannel,
    cArn,
    cDescription,
    cEgressAccessLogs,
    cHlsIngest,
    cId,
    cIngressAccessLogs,
    cTags,

    -- * Profile
    Profile (..),

    -- * PlaylistType
    PlaylistType (..),

    -- * StreamSelection
    StreamSelection (..),
    mkStreamSelection,
    ssMaxVideoBitsPerSecond,
    ssMinVideoBitsPerSecond,
    ssStreamOrder,

    -- * AdMarkers
    AdMarkers (..),

    -- * S3Destination
    S3Destination (..),
    mkS3Destination,
    sdManifestKey,
    sdBucketName,
    sdRoleArn,

    -- * DashPackage
    DashPackage (..),
    mkDashPackage,
    dpAdTriggers,
    dpAdsOnDeliveryRestrictions,
    dpEncryption,
    dpManifestLayout,
    dpManifestWindowSeconds,
    dpMinBufferTimeSeconds,
    dpMinUpdatePeriodSeconds,
    dpPeriodTriggers,
    dpProfile,
    dpSegmentDurationSeconds,
    dpSegmentTemplateFormat,
    dpStreamSelection,
    dpSuggestedPresentationDelaySeconds,
    dpUtcTiming,
    dpUtcTimingUri,

    -- * MssPackage
    MssPackage (..),
    mkMssPackage,
    mpEncryption,
    mpManifestWindowSeconds,
    mpSegmentDurationSeconds,
    mpStreamSelection,

    -- * OriginEndpoint
    OriginEndpoint (..),
    mkOriginEndpoint,
    oeArn,
    oeAuthorization,
    oeChannelId,
    oeCmafPackage,
    oeDashPackage,
    oeDescription,
    oeHlsPackage,
    oeId,
    oeManifestName,
    oeMssPackage,
    oeOrigination,
    oeStartoverWindowSeconds,
    oeTags,
    oeTimeDelaySeconds,
    oeUrl,
    oeWhitelist,

    -- * HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters (..),
    mkHlsManifestCreateOrUpdateParameters,
    hmcoupId,
    hmcoupAdMarkers,
    hmcoupAdTriggers,
    hmcoupAdsOnDeliveryRestrictions,
    hmcoupIncludeIframeOnlyStream,
    hmcoupManifestName,
    hmcoupPlaylistType,
    hmcoupPlaylistWindowSeconds,
    hmcoupProgramDateTimeIntervalSeconds,

    -- * ManifestLayout
    ManifestLayout (..),

    -- * AdTriggersElement
    AdTriggersElement (..),

    -- * HlsManifest
    HlsManifest (..),
    mkHlsManifest,
    hmId,
    hmAdMarkers,
    hmIncludeIframeOnlyStream,
    hmManifestName,
    hmPlaylistType,
    hmPlaylistWindowSeconds,
    hmProgramDateTimeIntervalSeconds,
    hmUrl,

    -- * HlsEncryption
    HlsEncryption (..),
    mkHlsEncryption,
    heSpekeKeyProvider,
    heConstantInitializationVector,
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heRepeatExtXKey,

    -- * HarvestJob
    HarvestJob (..),
    mkHarvestJob,
    hjArn,
    hjChannelId,
    hjCreatedAt,
    hjEndTime,
    hjId,
    hjOriginEndpointId,
    hjS3Destination,
    hjStartTime,
    hjStatus,

    -- * PeriodTriggersElement
    PeriodTriggersElement (..),

    -- * CmafPackage
    CmafPackage (..),
    mkCmafPackage,
    cpEncryption,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpSegmentPrefix,
    cpStreamSelection,

    -- * CmafEncryption
    CmafEncryption (..),
    mkCmafEncryption,
    ceSpekeKeyProvider,
    ceKeyRotationIntervalSeconds,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    mkSpekeKeyProvider,
    skpResourceId,
    skpSystemIds,
    skpUrl,
    skpRoleArn,
    skpCertificateArn,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    mkEgressAccessLogs,
    ealLogGroupName,

    -- * CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (..),
    mkCmafPackageCreateOrUpdateParameters,
    cpcoupEncryption,
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupSegmentPrefix,
    cpcoupStreamSelection,

    -- * Origination
    Origination (..),

    -- * IngestEndpoint
    IngestEndpoint (..),
    mkIngestEndpoint,
    ieId,
    iePassword,
    ieUrl,
    ieUsername,

    -- * DashEncryption
    DashEncryption (..),
    mkDashEncryption,
    deSpekeKeyProvider,
    deKeyRotationIntervalSeconds,
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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MediaPackage",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "mediapackage",
      Core._svcVersion = "2017-10-12",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MediaPackage",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The parameters sent in the request are not valid.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    mkServiceConfig
    "UnprocessableEntityException"
    Core.. Core.hasStatues 422
{-# DEPRECATED _UnprocessableEntityException "Use generic-lens or generic-optics instead." #-}

-- | The client is not authorized to access the requested resource.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError mkServiceConfig "ForbiddenException"
    Core.. Core.hasStatues 403
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource does not exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The client has exceeded their resource or throttling limits.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | An unexpected error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | An unexpected error occurred.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
    Core.. Core.hasStatues 503
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}
