{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types
  ( -- * Service Configuration
    mediaPackage,

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
    Authorization,
    authorization,
    aSecretsRoleARN,
    aCdnIdentifierSecret,

    -- * Channel
    Channel,
    channel,
    cIngressAccessLogs,
    cHlsIngest,
    cARN,
    cId,
    cDescription,
    cEgressAccessLogs,
    cTags,

    -- * CmafEncryption
    CmafEncryption,
    cmafEncryption,
    ceKeyRotationIntervalSeconds,
    ceSpekeKeyProvider,

    -- * CmafPackage
    CmafPackage,
    cmafPackage,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpStreamSelection,
    cpEncryption,
    cpSegmentPrefix,

    -- * CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters,
    cmafPackageCreateOrUpdateParameters,
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupStreamSelection,
    cpcoupEncryption,
    cpcoupSegmentPrefix,

    -- * DashEncryption
    DashEncryption,
    dashEncryption,
    deKeyRotationIntervalSeconds,
    deSpekeKeyProvider,

    -- * DashPackage
    DashPackage,
    dashPackage,
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
    EgressAccessLogs,
    egressAccessLogs,
    ealLogGroupName,

    -- * HarvestJob
    HarvestJob,
    harvestJob,
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
    HlsEncryption,
    hlsEncryption,
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heConstantInitializationVector,
    heRepeatExtXKey,
    heSpekeKeyProvider,

    -- * HlsIngest
    HlsIngest,
    hlsIngest,
    hiIngestEndpoints,

    -- * HlsManifest
    HlsManifest,
    hlsManifest,
    hmManifestName,
    hmURL,
    hmPlaylistType,
    hmProgramDateTimeIntervalSeconds,
    hmAdMarkers,
    hmIncludeIframeOnlyStream,
    hmPlaylistWindowSeconds,
    hmId,

    -- * HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters,
    hlsManifestCreateOrUpdateParameters,
    hmcoupAdsOnDeliveryRestrictions,
    hmcoupManifestName,
    hmcoupPlaylistType,
    hmcoupProgramDateTimeIntervalSeconds,
    hmcoupAdMarkers,
    hmcoupIncludeIframeOnlyStream,
    hmcoupAdTriggers,
    hmcoupPlaylistWindowSeconds,
    hmcoupId,

    -- * HlsPackage
    HlsPackage,
    hlsPackage,
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
    IngestEndpoint,
    ingestEndpoint,
    ieURL,
    ieUsername,
    iePassword,
    ieId,

    -- * IngressAccessLogs
    IngressAccessLogs,
    ingressAccessLogs,
    ialLogGroupName,

    -- * MssEncryption
    MssEncryption,
    mssEncryption,
    meSpekeKeyProvider,

    -- * MssPackage
    MssPackage,
    mssPackage,
    mpSegmentDurationSeconds,
    mpStreamSelection,
    mpEncryption,
    mpManifestWindowSeconds,

    -- * OriginEndpoint
    OriginEndpoint,
    originEndpoint,
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
    S3Destination,
    s3Destination,
    sdManifestKey,
    sdBucketName,
    sdRoleARN,

    -- * SpekeKeyProvider
    SpekeKeyProvider,
    spekeKeyProvider,
    skpCertificateARN,
    skpResourceId,
    skpSystemIds,
    skpURL,
    skpRoleARN,

    -- * StreamSelection
    StreamSelection,
    streamSelection,
    ssStreamOrder,
    ssMinVideoBitsPerSecond,
    ssMaxVideoBitsPerSecond,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-12@ of the Amazon Elemental MediaPackage SDK configuration.
mediaPackage :: Service
mediaPackage =
  Service
    { _svcAbbrev = "MediaPackage",
      _svcSigner = v4,
      _svcPrefix = "mediapackage",
      _svcVersion = "2017-10-12",
      _svcEndpoint = defaultEndpoint mediaPackage,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "MediaPackage",
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
