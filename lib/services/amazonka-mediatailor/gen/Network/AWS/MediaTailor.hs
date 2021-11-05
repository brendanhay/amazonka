{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.MediaTailor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-04-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use the AWS Elemental MediaTailor SDKs and CLI to configure scalable ad
-- insertion and linear channels. With MediaTailor, you can assemble
-- existing content into a linear stream and serve targeted ads to viewers
-- while maintaining broadcast quality in over-the-top (OTT) video
-- applications. For information about using the service, including
-- detailed information about the settings covered in this guide, see the
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ AWS Elemental MediaTailor User Guide>.
--
-- Through the SDKs and the CLI you manage AWS Elemental MediaTailor
-- configurations and channels the same as you do through the console. For
-- example, you specify ad insertion behavior and mapping information for
-- the origin server and the ad decision server (ADS).
module Network.AWS.MediaTailor
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSourceLocation
    CreateSourceLocation (CreateSourceLocation'),
    newCreateSourceLocation,
    CreateSourceLocationResponse (CreateSourceLocationResponse'),
    newCreateSourceLocationResponse,

    -- ** ListPrefetchSchedules (Paginated)
    ListPrefetchSchedules (ListPrefetchSchedules'),
    newListPrefetchSchedules,
    ListPrefetchSchedulesResponse (ListPrefetchSchedulesResponse'),
    newListPrefetchSchedulesResponse,

    -- ** DeletePrefetchSchedule
    DeletePrefetchSchedule (DeletePrefetchSchedule'),
    newDeletePrefetchSchedule,
    DeletePrefetchScheduleResponse (DeletePrefetchScheduleResponse'),
    newDeletePrefetchScheduleResponse,

    -- ** ListAlerts (Paginated)
    ListAlerts (ListAlerts'),
    newListAlerts,
    ListAlertsResponse (ListAlertsResponse'),
    newListAlertsResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** CreatePrefetchSchedule
    CreatePrefetchSchedule (CreatePrefetchSchedule'),
    newCreatePrefetchSchedule,
    CreatePrefetchScheduleResponse (CreatePrefetchScheduleResponse'),
    newCreatePrefetchScheduleResponse,

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

    -- ** GetChannelPolicy
    GetChannelPolicy (GetChannelPolicy'),
    newGetChannelPolicy,
    GetChannelPolicyResponse (GetChannelPolicyResponse'),
    newGetChannelPolicyResponse,

    -- ** DescribeVodSource
    DescribeVodSource (DescribeVodSource'),
    newDescribeVodSource,
    DescribeVodSourceResponse (DescribeVodSourceResponse'),
    newDescribeVodSourceResponse,

    -- ** DescribeSourceLocation
    DescribeSourceLocation (DescribeSourceLocation'),
    newDescribeSourceLocation,
    DescribeSourceLocationResponse (DescribeSourceLocationResponse'),
    newDescribeSourceLocationResponse,

    -- ** GetPrefetchSchedule
    GetPrefetchSchedule (GetPrefetchSchedule'),
    newGetPrefetchSchedule,
    GetPrefetchScheduleResponse (GetPrefetchScheduleResponse'),
    newGetPrefetchScheduleResponse,

    -- ** CreateProgram
    CreateProgram (CreateProgram'),
    newCreateProgram,
    CreateProgramResponse (CreateProgramResponse'),
    newCreateProgramResponse,

    -- ** StartChannel
    StartChannel (StartChannel'),
    newStartChannel,
    StartChannelResponse (StartChannelResponse'),
    newStartChannelResponse,

    -- ** ListPlaybackConfigurations (Paginated)
    ListPlaybackConfigurations (ListPlaybackConfigurations'),
    newListPlaybackConfigurations,
    ListPlaybackConfigurationsResponse (ListPlaybackConfigurationsResponse'),
    newListPlaybackConfigurationsResponse,

    -- ** DeletePlaybackConfiguration
    DeletePlaybackConfiguration (DeletePlaybackConfiguration'),
    newDeletePlaybackConfiguration,
    DeletePlaybackConfigurationResponse (DeletePlaybackConfigurationResponse'),
    newDeletePlaybackConfigurationResponse,

    -- ** PutPlaybackConfiguration
    PutPlaybackConfiguration (PutPlaybackConfiguration'),
    newPutPlaybackConfiguration,
    PutPlaybackConfigurationResponse (PutPlaybackConfigurationResponse'),
    newPutPlaybackConfigurationResponse,

    -- ** ListSourceLocations (Paginated)
    ListSourceLocations (ListSourceLocations'),
    newListSourceLocations,
    ListSourceLocationsResponse (ListSourceLocationsResponse'),
    newListSourceLocationsResponse,

    -- ** UpdateSourceLocation
    UpdateSourceLocation (UpdateSourceLocation'),
    newUpdateSourceLocation,
    UpdateSourceLocationResponse (UpdateSourceLocationResponse'),
    newUpdateSourceLocationResponse,

    -- ** DeleteSourceLocation
    DeleteSourceLocation (DeleteSourceLocation'),
    newDeleteSourceLocation,
    DeleteSourceLocationResponse (DeleteSourceLocationResponse'),
    newDeleteSourceLocationResponse,

    -- ** GetPlaybackConfiguration
    GetPlaybackConfiguration (GetPlaybackConfiguration'),
    newGetPlaybackConfiguration,
    GetPlaybackConfigurationResponse (GetPlaybackConfigurationResponse'),
    newGetPlaybackConfigurationResponse,

    -- ** DeleteVodSource
    DeleteVodSource (DeleteVodSource'),
    newDeleteVodSource,
    DeleteVodSourceResponse (DeleteVodSourceResponse'),
    newDeleteVodSourceResponse,

    -- ** UpdateVodSource
    UpdateVodSource (UpdateVodSource'),
    newUpdateVodSource,
    UpdateVodSourceResponse (UpdateVodSourceResponse'),
    newUpdateVodSourceResponse,

    -- ** CreateVodSource
    CreateVodSource (CreateVodSource'),
    newCreateVodSource,
    CreateVodSourceResponse (CreateVodSourceResponse'),
    newCreateVodSourceResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** DeleteChannelPolicy
    DeleteChannelPolicy (DeleteChannelPolicy'),
    newDeleteChannelPolicy,
    DeleteChannelPolicyResponse (DeleteChannelPolicyResponse'),
    newDeleteChannelPolicyResponse,

    -- ** PutChannelPolicy
    PutChannelPolicy (PutChannelPolicy'),
    newPutChannelPolicy,
    PutChannelPolicyResponse (PutChannelPolicyResponse'),
    newPutChannelPolicyResponse,

    -- ** DeleteProgram
    DeleteProgram (DeleteProgram'),
    newDeleteProgram,
    DeleteProgramResponse (DeleteProgramResponse'),
    newDeleteProgramResponse,

    -- ** GetChannelSchedule (Paginated)
    GetChannelSchedule (GetChannelSchedule'),
    newGetChannelSchedule,
    GetChannelScheduleResponse (GetChannelScheduleResponse'),
    newGetChannelScheduleResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ConfigureLogsForPlaybackConfiguration
    ConfigureLogsForPlaybackConfiguration (ConfigureLogsForPlaybackConfiguration'),
    newConfigureLogsForPlaybackConfiguration,
    ConfigureLogsForPlaybackConfigurationResponse (ConfigureLogsForPlaybackConfigurationResponse'),
    newConfigureLogsForPlaybackConfigurationResponse,

    -- ** StopChannel
    StopChannel (StopChannel'),
    newStopChannel,
    StopChannelResponse (StopChannelResponse'),
    newStopChannelResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** ListVodSources (Paginated)
    ListVodSources (ListVodSources'),
    newListVodSources,
    ListVodSourcesResponse (ListVodSourcesResponse'),
    newListVodSourcesResponse,

    -- ** DescribeProgram
    DescribeProgram (DescribeProgram'),
    newDescribeProgram,
    DescribeProgramResponse (DescribeProgramResponse'),
    newDescribeProgramResponse,

    -- * Types

    -- ** AccessType
    AccessType (..),

    -- ** ChannelState
    ChannelState (..),

    -- ** MessageType
    MessageType (..),

    -- ** Mode
    Mode (..),

    -- ** Operator
    Operator (..),

    -- ** OriginManifestType
    OriginManifestType (..),

    -- ** PlaybackMode
    PlaybackMode (..),

    -- ** RelativePosition
    RelativePosition (..),

    -- ** ScheduleEntryType
    ScheduleEntryType (..),

    -- ** Type
    Type (..),

    -- ** AccessConfiguration
    AccessConfiguration (AccessConfiguration'),
    newAccessConfiguration,

    -- ** AdBreak
    AdBreak (AdBreak'),
    newAdBreak,

    -- ** AdMarkerPassthrough
    AdMarkerPassthrough (AdMarkerPassthrough'),
    newAdMarkerPassthrough,

    -- ** Alert
    Alert (Alert'),
    newAlert,

    -- ** AvailMatchingCriteria
    AvailMatchingCriteria (AvailMatchingCriteria'),
    newAvailMatchingCriteria,

    -- ** AvailSuppression
    AvailSuppression (AvailSuppression'),
    newAvailSuppression,

    -- ** Bumper
    Bumper (Bumper'),
    newBumper,

    -- ** CdnConfiguration
    CdnConfiguration (CdnConfiguration'),
    newCdnConfiguration,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** DashConfiguration
    DashConfiguration (DashConfiguration'),
    newDashConfiguration,

    -- ** DashConfigurationForPut
    DashConfigurationForPut (DashConfigurationForPut'),
    newDashConfigurationForPut,

    -- ** DashPlaylistSettings
    DashPlaylistSettings (DashPlaylistSettings'),
    newDashPlaylistSettings,

    -- ** DefaultSegmentDeliveryConfiguration
    DefaultSegmentDeliveryConfiguration (DefaultSegmentDeliveryConfiguration'),
    newDefaultSegmentDeliveryConfiguration,

    -- ** HlsConfiguration
    HlsConfiguration (HlsConfiguration'),
    newHlsConfiguration,

    -- ** HlsPlaylistSettings
    HlsPlaylistSettings (HlsPlaylistSettings'),
    newHlsPlaylistSettings,

    -- ** HttpConfiguration
    HttpConfiguration (HttpConfiguration'),
    newHttpConfiguration,

    -- ** HttpPackageConfiguration
    HttpPackageConfiguration (HttpPackageConfiguration'),
    newHttpPackageConfiguration,

    -- ** LivePreRollConfiguration
    LivePreRollConfiguration (LivePreRollConfiguration'),
    newLivePreRollConfiguration,

    -- ** LogConfiguration
    LogConfiguration (LogConfiguration'),
    newLogConfiguration,

    -- ** ManifestProcessingRules
    ManifestProcessingRules (ManifestProcessingRules'),
    newManifestProcessingRules,

    -- ** PlaybackConfiguration
    PlaybackConfiguration (PlaybackConfiguration'),
    newPlaybackConfiguration,

    -- ** PrefetchConsumption
    PrefetchConsumption (PrefetchConsumption'),
    newPrefetchConsumption,

    -- ** PrefetchRetrieval
    PrefetchRetrieval (PrefetchRetrieval'),
    newPrefetchRetrieval,

    -- ** PrefetchSchedule
    PrefetchSchedule (PrefetchSchedule'),
    newPrefetchSchedule,

    -- ** RequestOutputItem
    RequestOutputItem (RequestOutputItem'),
    newRequestOutputItem,

    -- ** ResponseOutputItem
    ResponseOutputItem (ResponseOutputItem'),
    newResponseOutputItem,

    -- ** ScheduleAdBreak
    ScheduleAdBreak (ScheduleAdBreak'),
    newScheduleAdBreak,

    -- ** ScheduleConfiguration
    ScheduleConfiguration (ScheduleConfiguration'),
    newScheduleConfiguration,

    -- ** ScheduleEntry
    ScheduleEntry (ScheduleEntry'),
    newScheduleEntry,

    -- ** SecretsManagerAccessTokenConfiguration
    SecretsManagerAccessTokenConfiguration (SecretsManagerAccessTokenConfiguration'),
    newSecretsManagerAccessTokenConfiguration,

    -- ** SlateSource
    SlateSource (SlateSource'),
    newSlateSource,

    -- ** SourceLocation
    SourceLocation (SourceLocation'),
    newSourceLocation,

    -- ** SpliceInsertMessage
    SpliceInsertMessage (SpliceInsertMessage'),
    newSpliceInsertMessage,

    -- ** Transition
    Transition (Transition'),
    newTransition,

    -- ** VodSource
    VodSource (VodSource'),
    newVodSource,
  )
where

import Network.AWS.MediaTailor.ConfigureLogsForPlaybackConfiguration
import Network.AWS.MediaTailor.CreateChannel
import Network.AWS.MediaTailor.CreatePrefetchSchedule
import Network.AWS.MediaTailor.CreateProgram
import Network.AWS.MediaTailor.CreateSourceLocation
import Network.AWS.MediaTailor.CreateVodSource
import Network.AWS.MediaTailor.DeleteChannel
import Network.AWS.MediaTailor.DeleteChannelPolicy
import Network.AWS.MediaTailor.DeletePlaybackConfiguration
import Network.AWS.MediaTailor.DeletePrefetchSchedule
import Network.AWS.MediaTailor.DeleteProgram
import Network.AWS.MediaTailor.DeleteSourceLocation
import Network.AWS.MediaTailor.DeleteVodSource
import Network.AWS.MediaTailor.DescribeChannel
import Network.AWS.MediaTailor.DescribeProgram
import Network.AWS.MediaTailor.DescribeSourceLocation
import Network.AWS.MediaTailor.DescribeVodSource
import Network.AWS.MediaTailor.GetChannelPolicy
import Network.AWS.MediaTailor.GetChannelSchedule
import Network.AWS.MediaTailor.GetPlaybackConfiguration
import Network.AWS.MediaTailor.GetPrefetchSchedule
import Network.AWS.MediaTailor.Lens
import Network.AWS.MediaTailor.ListAlerts
import Network.AWS.MediaTailor.ListChannels
import Network.AWS.MediaTailor.ListPlaybackConfigurations
import Network.AWS.MediaTailor.ListPrefetchSchedules
import Network.AWS.MediaTailor.ListSourceLocations
import Network.AWS.MediaTailor.ListTagsForResource
import Network.AWS.MediaTailor.ListVodSources
import Network.AWS.MediaTailor.PutChannelPolicy
import Network.AWS.MediaTailor.PutPlaybackConfiguration
import Network.AWS.MediaTailor.StartChannel
import Network.AWS.MediaTailor.StopChannel
import Network.AWS.MediaTailor.TagResource
import Network.AWS.MediaTailor.Types
import Network.AWS.MediaTailor.UntagResource
import Network.AWS.MediaTailor.UpdateChannel
import Network.AWS.MediaTailor.UpdateSourceLocation
import Network.AWS.MediaTailor.UpdateVodSource
import Network.AWS.MediaTailor.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaTailor'.

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
