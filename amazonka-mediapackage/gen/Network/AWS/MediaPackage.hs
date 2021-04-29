{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elemental MediaPackage
module Network.AWS.MediaPackage
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateHarvestJob
    CreateHarvestJob (CreateHarvestJob'),
    newCreateHarvestJob,
    CreateHarvestJobResponse (CreateHarvestJobResponse'),
    newCreateHarvestJobResponse,

    -- ** UpdateOriginEndpoint
    UpdateOriginEndpoint (UpdateOriginEndpoint'),
    newUpdateOriginEndpoint,
    UpdateOriginEndpointResponse (UpdateOriginEndpointResponse'),
    newUpdateOriginEndpointResponse,

    -- ** DeleteOriginEndpoint
    DeleteOriginEndpoint (DeleteOriginEndpoint'),
    newDeleteOriginEndpoint,
    DeleteOriginEndpointResponse (DeleteOriginEndpointResponse'),
    newDeleteOriginEndpointResponse,

    -- ** ListOriginEndpoints (Paginated)
    ListOriginEndpoints (ListOriginEndpoints'),
    newListOriginEndpoints,
    ListOriginEndpointsResponse (ListOriginEndpointsResponse'),
    newListOriginEndpointsResponse,

    -- ** CreateOriginEndpoint
    CreateOriginEndpoint (CreateOriginEndpoint'),
    newCreateOriginEndpoint,
    CreateOriginEndpointResponse (CreateOriginEndpointResponse'),
    newCreateOriginEndpointResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeOriginEndpoint
    DescribeOriginEndpoint (DescribeOriginEndpoint'),
    newDescribeOriginEndpoint,
    DescribeOriginEndpointResponse (DescribeOriginEndpointResponse'),
    newDescribeOriginEndpointResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ConfigureLogs
    ConfigureLogs (ConfigureLogs'),
    newConfigureLogs,
    ConfigureLogsResponse (ConfigureLogsResponse'),
    newConfigureLogsResponse,

    -- ** ListHarvestJobs (Paginated)
    ListHarvestJobs (ListHarvestJobs'),
    newListHarvestJobs,
    ListHarvestJobsResponse (ListHarvestJobsResponse'),
    newListHarvestJobsResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** RotateIngestEndpointCredentials
    RotateIngestEndpointCredentials (RotateIngestEndpointCredentials'),
    newRotateIngestEndpointCredentials,
    RotateIngestEndpointCredentialsResponse (RotateIngestEndpointCredentialsResponse'),
    newRotateIngestEndpointCredentialsResponse,

    -- ** DescribeHarvestJob
    DescribeHarvestJob (DescribeHarvestJob'),
    newDescribeHarvestJob,
    DescribeHarvestJobResponse (DescribeHarvestJobResponse'),
    newDescribeHarvestJobResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

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
    Authorization (Authorization'),
    newAuthorization,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** CmafEncryption
    CmafEncryption (CmafEncryption'),
    newCmafEncryption,

    -- ** CmafPackage
    CmafPackage (CmafPackage'),
    newCmafPackage,

    -- ** CmafPackageCreateOrUpdateParameters
    CmafPackageCreateOrUpdateParameters (CmafPackageCreateOrUpdateParameters'),
    newCmafPackageCreateOrUpdateParameters,

    -- ** DashEncryption
    DashEncryption (DashEncryption'),
    newDashEncryption,

    -- ** DashPackage
    DashPackage (DashPackage'),
    newDashPackage,

    -- ** EgressAccessLogs
    EgressAccessLogs (EgressAccessLogs'),
    newEgressAccessLogs,

    -- ** HarvestJob
    HarvestJob (HarvestJob'),
    newHarvestJob,

    -- ** HlsEncryption
    HlsEncryption (HlsEncryption'),
    newHlsEncryption,

    -- ** HlsIngest
    HlsIngest (HlsIngest'),
    newHlsIngest,

    -- ** HlsManifest
    HlsManifest (HlsManifest'),
    newHlsManifest,

    -- ** HlsManifestCreateOrUpdateParameters
    HlsManifestCreateOrUpdateParameters (HlsManifestCreateOrUpdateParameters'),
    newHlsManifestCreateOrUpdateParameters,

    -- ** HlsPackage
    HlsPackage (HlsPackage'),
    newHlsPackage,

    -- ** IngestEndpoint
    IngestEndpoint (IngestEndpoint'),
    newIngestEndpoint,

    -- ** IngressAccessLogs
    IngressAccessLogs (IngressAccessLogs'),
    newIngressAccessLogs,

    -- ** MssEncryption
    MssEncryption (MssEncryption'),
    newMssEncryption,

    -- ** MssPackage
    MssPackage (MssPackage'),
    newMssPackage,

    -- ** OriginEndpoint
    OriginEndpoint (OriginEndpoint'),
    newOriginEndpoint,

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (SpekeKeyProvider'),
    newSpekeKeyProvider,

    -- ** StreamSelection
    StreamSelection (StreamSelection'),
    newStreamSelection,
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
import Network.AWS.MediaPackage.Lens
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
