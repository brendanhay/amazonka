{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elemental MediaPackage
module Amazonka.MediaPackage
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ConfigureLogs
    ConfigureLogs (ConfigureLogs'),
    newConfigureLogs,
    ConfigureLogsResponse (ConfigureLogsResponse'),
    newConfigureLogsResponse,

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

    -- ** CreateOriginEndpoint
    CreateOriginEndpoint (CreateOriginEndpoint'),
    newCreateOriginEndpoint,
    CreateOriginEndpointResponse (CreateOriginEndpointResponse'),
    newCreateOriginEndpointResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteOriginEndpoint
    DeleteOriginEndpoint (DeleteOriginEndpoint'),
    newDeleteOriginEndpoint,
    DeleteOriginEndpointResponse (DeleteOriginEndpointResponse'),
    newDeleteOriginEndpointResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeHarvestJob
    DescribeHarvestJob (DescribeHarvestJob'),
    newDescribeHarvestJob,
    DescribeHarvestJobResponse (DescribeHarvestJobResponse'),
    newDescribeHarvestJobResponse,

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

    -- ** ListHarvestJobs (Paginated)
    ListHarvestJobs (ListHarvestJobs'),
    newListHarvestJobs,
    ListHarvestJobsResponse (ListHarvestJobsResponse'),
    newListHarvestJobsResponse,

    -- ** ListOriginEndpoints (Paginated)
    ListOriginEndpoints (ListOriginEndpoints'),
    newListOriginEndpoints,
    ListOriginEndpointsResponse (ListOriginEndpointsResponse'),
    newListOriginEndpointsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RotateIngestEndpointCredentials
    RotateIngestEndpointCredentials (RotateIngestEndpointCredentials'),
    newRotateIngestEndpointCredentials,
    RotateIngestEndpointCredentialsResponse (RotateIngestEndpointCredentialsResponse'),
    newRotateIngestEndpointCredentialsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** UpdateOriginEndpoint
    UpdateOriginEndpoint (UpdateOriginEndpoint'),
    newUpdateOriginEndpoint,
    UpdateOriginEndpointResponse (UpdateOriginEndpointResponse'),
    newUpdateOriginEndpointResponse,

    -- * Types

    -- ** AdMarkers
    AdMarkers (..),

    -- ** AdTriggersElement
    AdTriggersElement (..),

    -- ** AdsOnDeliveryRestrictions
    AdsOnDeliveryRestrictions (..),

    -- ** CmafEncryptionMethod
    CmafEncryptionMethod (..),

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

    -- ** PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- ** PresetSpeke20Video
    PresetSpeke20Video (..),

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

    -- ** EncryptionContractConfiguration
    EncryptionContractConfiguration (EncryptionContractConfiguration'),
    newEncryptionContractConfiguration,

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

import Amazonka.MediaPackage.ConfigureLogs
import Amazonka.MediaPackage.CreateChannel
import Amazonka.MediaPackage.CreateHarvestJob
import Amazonka.MediaPackage.CreateOriginEndpoint
import Amazonka.MediaPackage.DeleteChannel
import Amazonka.MediaPackage.DeleteOriginEndpoint
import Amazonka.MediaPackage.DescribeChannel
import Amazonka.MediaPackage.DescribeHarvestJob
import Amazonka.MediaPackage.DescribeOriginEndpoint
import Amazonka.MediaPackage.Lens
import Amazonka.MediaPackage.ListChannels
import Amazonka.MediaPackage.ListHarvestJobs
import Amazonka.MediaPackage.ListOriginEndpoints
import Amazonka.MediaPackage.ListTagsForResource
import Amazonka.MediaPackage.RotateIngestEndpointCredentials
import Amazonka.MediaPackage.TagResource
import Amazonka.MediaPackage.Types
import Amazonka.MediaPackage.UntagResource
import Amazonka.MediaPackage.UpdateChannel
import Amazonka.MediaPackage.UpdateOriginEndpoint
import Amazonka.MediaPackage.Waiters

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
