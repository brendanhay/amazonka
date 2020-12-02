{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elemental MediaPackage
module Network.AWS.MediaPackage
  ( -- * Service Configuration
    mediaPackage,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateHarvestJob
    module Network.AWS.MediaPackage.CreateHarvestJob,

    -- ** ConfigureLogs
    module Network.AWS.MediaPackage.ConfigureLogs,

    -- ** DescribeOriginEndpoint
    module Network.AWS.MediaPackage.DescribeOriginEndpoint,

    -- ** ListChannels (Paginated)
    module Network.AWS.MediaPackage.ListChannels,

    -- ** ListTagsForResource
    module Network.AWS.MediaPackage.ListTagsForResource,

    -- ** DeleteChannel
    module Network.AWS.MediaPackage.DeleteChannel,

    -- ** UpdateChannel
    module Network.AWS.MediaPackage.UpdateChannel,

    -- ** DescribeHarvestJob
    module Network.AWS.MediaPackage.DescribeHarvestJob,

    -- ** RotateIngestEndpointCredentials
    module Network.AWS.MediaPackage.RotateIngestEndpointCredentials,

    -- ** CreateOriginEndpoint
    module Network.AWS.MediaPackage.CreateOriginEndpoint,

    -- ** ListOriginEndpoints (Paginated)
    module Network.AWS.MediaPackage.ListOriginEndpoints,

    -- ** ListHarvestJobs (Paginated)
    module Network.AWS.MediaPackage.ListHarvestJobs,

    -- ** CreateChannel
    module Network.AWS.MediaPackage.CreateChannel,

    -- ** TagResource
    module Network.AWS.MediaPackage.TagResource,

    -- ** UntagResource
    module Network.AWS.MediaPackage.UntagResource,

    -- ** DescribeChannel
    module Network.AWS.MediaPackage.DescribeChannel,

    -- ** DeleteOriginEndpoint
    module Network.AWS.MediaPackage.DeleteOriginEndpoint,

    -- ** UpdateOriginEndpoint
    module Network.AWS.MediaPackage.UpdateOriginEndpoint,

    -- * Types

    -- ** AdMarkers
    AdMarkers (..),

    -- ** AdTriggersElement
    AdTriggersElement (..),

    -- ** AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

    -- ** EncryptionMethod
    EncryptionMethod (..),

    -- ** ManifestLayout
    ManifestLayout (..),

    -- ** Origination
    Origination (..),

    -- ** PeriodTriggersElement
    PeriodTriggersElement (..),

    -- ** PlaylistType
    PlaylistType (..),

    -- ** Profile
    Profile (..),

    -- ** SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- ** Status
    Status (..),

    -- ** StreamOrder
    StreamOrder (..),

    -- ** UtcTiming
    UtcTiming (..),

    -- ** Authorization
    Authorization,
    authorization,
    aSecretsRoleARN,
    aCdnIdentifierSecret,

    -- ** Channel
    Channel,
    channel,
    cIngressAccessLogs,
    cHlsIngest,
    cARN,
    cId,
    cDescription,
    cEgressAccessLogs,
    cTags,

    -- ** CmafEncryption
    CmafEncryption,
    cmafEncryption,
    ceKeyRotationIntervalSeconds,
    ceSpekeKeyProvider,

    -- ** CmafPackage
    CmafPackage,
    cmafPackage,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpStreamSelection,
    cpEncryption,
    cpSegmentPrefix,

    -- ** CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters,
    cmafPackageCreateOrUpdateParameters,
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupStreamSelection,
    cpcoupEncryption,
    cpcoupSegmentPrefix,

    -- ** DashEncryption
    DashEncryption,
    dashEncryption,
    deKeyRotationIntervalSeconds,
    deSpekeKeyProvider,

    -- ** DashPackage
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

    -- ** EgressAccessLogs
    EgressAccessLogs,
    egressAccessLogs,
    ealLogGroupName,

    -- ** HarvestJob
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

    -- ** HlsEncryption
    HlsEncryption,
    hlsEncryption,
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heConstantInitializationVector,
    heRepeatExtXKey,
    heSpekeKeyProvider,

    -- ** HlsIngest
    HlsIngest,
    hlsIngest,
    hiIngestEndpoints,

    -- ** HlsManifest
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

    -- ** HlsManifestCreateOrUpdateParameters
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

    -- ** HlsPackage
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

    -- ** IngestEndpoint
    IngestEndpoint,
    ingestEndpoint,
    ieURL,
    ieUsername,
    iePassword,
    ieId,

    -- ** IngressAccessLogs
    IngressAccessLogs,
    ingressAccessLogs,
    ialLogGroupName,

    -- ** MssEncryption
    MssEncryption,
    mssEncryption,
    meSpekeKeyProvider,

    -- ** MssPackage
    MssPackage,
    mssPackage,
    mpSegmentDurationSeconds,
    mpStreamSelection,
    mpEncryption,
    mpManifestWindowSeconds,

    -- ** OriginEndpoint
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

    -- ** S3Destination
    S3Destination,
    s3Destination,
    sdManifestKey,
    sdBucketName,
    sdRoleARN,

    -- ** SpekeKeyProvider
    SpekeKeyProvider,
    spekeKeyProvider,
    skpCertificateARN,
    skpResourceId,
    skpSystemIds,
    skpURL,
    skpRoleARN,

    -- ** StreamSelection
    StreamSelection,
    streamSelection,
    ssStreamOrder,
    ssMinVideoBitsPerSecond,
    ssMaxVideoBitsPerSecond,
  )
where

import Network.AWS.MediaPackage.ConfigureLogs
import Network.AWS.MediaPackage.CreateChannel
import Network.AWS.MediaPackage.CreateHarvestJob
import Network.AWS.MediaPackage.CreateOriginEndpoint
import Network.AWS.MediaPackage.DeleteChannel
import Network.AWS.MediaPackage.DeleteOriginEndpoint
import Network.AWS.MediaPackage.DescribeChannel
import Network.AWS.MediaPackage.DescribeHarvestJob
import Network.AWS.MediaPackage.DescribeOriginEndpoint
import Network.AWS.MediaPackage.ListChannels
import Network.AWS.MediaPackage.ListHarvestJobs
import Network.AWS.MediaPackage.ListOriginEndpoints
import Network.AWS.MediaPackage.ListTagsForResource
import Network.AWS.MediaPackage.RotateIngestEndpointCredentials
import Network.AWS.MediaPackage.TagResource
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.UntagResource
import Network.AWS.MediaPackage.UpdateChannel
import Network.AWS.MediaPackage.UpdateOriginEndpoint
import Network.AWS.MediaPackage.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaPackage'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
