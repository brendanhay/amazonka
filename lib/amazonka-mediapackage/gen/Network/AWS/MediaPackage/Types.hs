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
    mediaPackageService,

    -- * Errors

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
    mkAuthorization,
    aCdnIdentifierSecret,
    aSecretsRoleARN,

    -- * Channel
    Channel (..),
    mkChannel,
    cIngressAccessLogs,
    cHlsIngest,
    cARN,
    cId,
    cDescription,
    cEgressAccessLogs,
    cTags,

    -- * CmafEncryption
    CmafEncryption (..),
    mkCmafEncryption,
    ceKeyRotationIntervalSeconds,
    ceSpekeKeyProvider,

    -- * CmafPackage
    CmafPackage (..),
    mkCmafPackage,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpStreamSelection,
    cpEncryption,
    cpSegmentPrefix,

    -- * CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (..),
    mkCmafPackageCreateOrUpdateParameters,
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupStreamSelection,
    cpcoupEncryption,
    cpcoupSegmentPrefix,

    -- * DashEncryption
    DashEncryption (..),
    mkDashEncryption,
    deKeyRotationIntervalSeconds,
    deSpekeKeyProvider,

    -- * DashPackage
    DashPackage (..),
    mkDashPackage,
    dpAdsOnDeliveryRestrictions,
    dpMinBufferTimeSeconds,
    dpUtcTiming,
    dpSegmentTemplateFormat,
    dpProfile,
    dpSegmentDurationSeconds,
    dpUtcTimingURI,
    dpStreamSelection,
    dpEncryption,
    dpMinUpdatePeriodSeconds,
    dpManifestLayout,
    dpSuggestedPresentationDelaySeconds,
    dpManifestWindowSeconds,
    dpAdTriggers,
    dpPeriodTriggers,

    -- * EgressAccessLogs
    EgressAccessLogs (..),
    mkEgressAccessLogs,
    ealLogGroupName,

    -- * HarvestJob
    HarvestJob (..),
    mkHarvestJob,
    hjStatus,
    hjOriginEndpointId,
    hjStartTime,
    hjARN,
    hjCreatedAt,
    hjChannelId,
    hjS3Destination,
    hjEndTime,
    hjId,

    -- * HlsEncryption
    HlsEncryption (..),
    mkHlsEncryption,
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heConstantInitializationVector,
    heSpekeKeyProvider,
    heRepeatExtXKey,

    -- * HlsIngest
    HlsIngest (..),
    mkHlsIngest,
    hiIngestEndpoints,

    -- * HlsManifest
    HlsManifest (..),
    mkHlsManifest,
    hmManifestName,
    hmURL,
    hmPlaylistType,
    hmProgramDateTimeIntervalSeconds,
    hmAdMarkers,
    hmId,
    hmIncludeIframeOnlyStream,
    hmPlaylistWindowSeconds,

    -- * HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters (..),
    mkHlsManifestCreateOrUpdateParameters,
    hmcoupAdsOnDeliveryRestrictions,
    hmcoupManifestName,
    hmcoupPlaylistType,
    hmcoupProgramDateTimeIntervalSeconds,
    hmcoupAdMarkers,
    hmcoupId,
    hmcoupIncludeIframeOnlyStream,
    hmcoupAdTriggers,
    hmcoupPlaylistWindowSeconds,

    -- * HlsPackage
    HlsPackage (..),
    mkHlsPackage,
    hpAdsOnDeliveryRestrictions,
    hpUseAudioRenditionGroup,
    hpPlaylistType,
    hpSegmentDurationSeconds,
    hpProgramDateTimeIntervalSeconds,
    hpStreamSelection,
    hpAdMarkers,
    hpEncryption,
    hpIncludeIframeOnlyStream,
    hpAdTriggers,
    hpPlaylistWindowSeconds,

    -- * IngestEndpoint
    IngestEndpoint (..),
    mkIngestEndpoint,
    ieURL,
    ieUsername,
    iePassword,
    ieId,

    -- * IngressAccessLogs
    IngressAccessLogs (..),
    mkIngressAccessLogs,
    ialLogGroupName,

    -- * MssEncryption
    MssEncryption (..),
    mkMssEncryption,
    meSpekeKeyProvider,

    -- * MssPackage
    MssPackage (..),
    mkMssPackage,
    mpSegmentDurationSeconds,
    mpStreamSelection,
    mpEncryption,
    mpManifestWindowSeconds,

    -- * OriginEndpoint
    OriginEndpoint (..),
    mkOriginEndpoint,
    oeWhitelist,
    oeHlsPackage,
    oeARN,
    oeManifestName,
    oeURL,
    oeAuthorization,
    oeChannelId,
    oeStartoverWindowSeconds,
    oeDashPackage,
    oeMssPackage,
    oeId,
    oeTimeDelaySeconds,
    oeCmafPackage,
    oeDescription,
    oeTags,
    oeOrigination,

    -- * S3Destination
    S3Destination (..),
    mkS3Destination,
    sdBucketName,
    sdManifestKey,
    sdRoleARN,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    mkSpekeKeyProvider,
    skpResourceId,
    skpCertificateARN,
    skpURL,
    skpSystemIds,
    skpRoleARN,

    -- * StreamSelection
    StreamSelection (..),
    mkStreamSelection,
    ssStreamOrder,
    ssMinVideoBitsPerSecond,
    ssMaxVideoBitsPerSecond,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
mediaPackageService :: Lude.Service
mediaPackageService =
  Lude.Service
    { Lude._svcAbbrev = "MediaPackage",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "mediapackage",
      Lude._svcVersion = "2017-10-12",
      Lude._svcEndpoint = Lude.defaultEndpoint mediaPackageService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "MediaPackage",
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
