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
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

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

    -- ** AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

    -- ** Status
    Status (..),

    -- ** IngressAccessLogs
    IngressAccessLogs (..),
    mkIngressAccessLogs,
    ialLogGroupName,

    -- ** UtcTiming
    UtcTiming (..),

    -- ** MssEncryption
    MssEncryption (..),
    mkMssEncryption,
    meSpekeKeyProvider,

    -- ** StreamOrder
    StreamOrder (..),

    -- ** HlsIngest
    HlsIngest (..),
    mkHlsIngest,
    hiIngestEndpoints,

    -- ** HlsPackage
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

    -- ** EncryptionMethod
    EncryptionMethod (..),

    -- ** SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- ** Authorization
    Authorization (..),
    mkAuthorization,
    aSecretsRoleArn,
    aCdnIdentifierSecret,

    -- ** Channel
    Channel (..),
    mkChannel,
    cArn,
    cDescription,
    cEgressAccessLogs,
    cHlsIngest,
    cId,
    cIngressAccessLogs,
    cTags,

    -- ** Profile
    Profile (..),

    -- ** PlaylistType
    PlaylistType (..),

    -- ** StreamSelection
    StreamSelection (..),
    mkStreamSelection,
    ssMaxVideoBitsPerSecond,
    ssMinVideoBitsPerSecond,
    ssStreamOrder,

    -- ** AdMarkers
    AdMarkers (..),

    -- ** S3Destination
    S3Destination (..),
    mkS3Destination,
    sdManifestKey,
    sdBucketName,
    sdRoleArn,

    -- ** DashPackage
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

    -- ** MssPackage
    MssPackage (..),
    mkMssPackage,
    mpEncryption,
    mpManifestWindowSeconds,
    mpSegmentDurationSeconds,
    mpStreamSelection,

    -- ** OriginEndpoint
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

    -- ** HlsManifestCreateOrUpdateParameters
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

    -- ** ManifestLayout
    ManifestLayout (..),

    -- ** AdTriggersElement
    AdTriggersElement (..),

    -- ** HlsManifest
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

    -- ** HlsEncryption
    HlsEncryption (..),
    mkHlsEncryption,
    heSpekeKeyProvider,
    heConstantInitializationVector,
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heRepeatExtXKey,

    -- ** HarvestJob
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

    -- ** PeriodTriggersElement
    PeriodTriggersElement (..),

    -- ** CmafPackage
    CmafPackage (..),
    mkCmafPackage,
    cpEncryption,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpSegmentPrefix,
    cpStreamSelection,

    -- ** CmafEncryption
    CmafEncryption (..),
    mkCmafEncryption,
    ceSpekeKeyProvider,
    ceKeyRotationIntervalSeconds,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (..),
    mkSpekeKeyProvider,
    skpResourceId,
    skpSystemIds,
    skpUrl,
    skpRoleArn,
    skpCertificateArn,

    -- ** EgressAccessLogs
    EgressAccessLogs (..),
    mkEgressAccessLogs,
    ealLogGroupName,

    -- ** CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (..),
    mkCmafPackageCreateOrUpdateParameters,
    cpcoupEncryption,
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupSegmentPrefix,
    cpcoupStreamSelection,

    -- ** Origination
    Origination (..),

    -- ** IngestEndpoint
    IngestEndpoint (..),
    mkIngestEndpoint,
    ieId,
    iePassword,
    ieUrl,
    ieUsername,

    -- ** DashEncryption
    DashEncryption (..),
    mkDashEncryption,
    deSpekeKeyProvider,
    deKeyRotationIntervalSeconds,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
