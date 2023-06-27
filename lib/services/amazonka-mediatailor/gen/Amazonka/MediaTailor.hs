{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaTailor
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MediaTailor
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

    -- ** ConfigureLogsForChannel
    ConfigureLogsForChannel (ConfigureLogsForChannel'),
    newConfigureLogsForChannel,
    ConfigureLogsForChannelResponse (ConfigureLogsForChannelResponse'),
    newConfigureLogsForChannelResponse,

    -- ** ConfigureLogsForPlaybackConfiguration
    ConfigureLogsForPlaybackConfiguration (ConfigureLogsForPlaybackConfiguration'),
    newConfigureLogsForPlaybackConfiguration,
    ConfigureLogsForPlaybackConfigurationResponse (ConfigureLogsForPlaybackConfigurationResponse'),
    newConfigureLogsForPlaybackConfigurationResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateLiveSource
    CreateLiveSource (CreateLiveSource'),
    newCreateLiveSource,
    CreateLiveSourceResponse (CreateLiveSourceResponse'),
    newCreateLiveSourceResponse,

    -- ** CreatePrefetchSchedule
    CreatePrefetchSchedule (CreatePrefetchSchedule'),
    newCreatePrefetchSchedule,
    CreatePrefetchScheduleResponse (CreatePrefetchScheduleResponse'),
    newCreatePrefetchScheduleResponse,

    -- ** CreateProgram
    CreateProgram (CreateProgram'),
    newCreateProgram,
    CreateProgramResponse (CreateProgramResponse'),
    newCreateProgramResponse,

    -- ** CreateSourceLocation
    CreateSourceLocation (CreateSourceLocation'),
    newCreateSourceLocation,
    CreateSourceLocationResponse (CreateSourceLocationResponse'),
    newCreateSourceLocationResponse,

    -- ** CreateVodSource
    CreateVodSource (CreateVodSource'),
    newCreateVodSource,
    CreateVodSourceResponse (CreateVodSourceResponse'),
    newCreateVodSourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteChannelPolicy
    DeleteChannelPolicy (DeleteChannelPolicy'),
    newDeleteChannelPolicy,
    DeleteChannelPolicyResponse (DeleteChannelPolicyResponse'),
    newDeleteChannelPolicyResponse,

    -- ** DeleteLiveSource
    DeleteLiveSource (DeleteLiveSource'),
    newDeleteLiveSource,
    DeleteLiveSourceResponse (DeleteLiveSourceResponse'),
    newDeleteLiveSourceResponse,

    -- ** DeletePlaybackConfiguration
    DeletePlaybackConfiguration (DeletePlaybackConfiguration'),
    newDeletePlaybackConfiguration,
    DeletePlaybackConfigurationResponse (DeletePlaybackConfigurationResponse'),
    newDeletePlaybackConfigurationResponse,

    -- ** DeletePrefetchSchedule
    DeletePrefetchSchedule (DeletePrefetchSchedule'),
    newDeletePrefetchSchedule,
    DeletePrefetchScheduleResponse (DeletePrefetchScheduleResponse'),
    newDeletePrefetchScheduleResponse,

    -- ** DeleteProgram
    DeleteProgram (DeleteProgram'),
    newDeleteProgram,
    DeleteProgramResponse (DeleteProgramResponse'),
    newDeleteProgramResponse,

    -- ** DeleteSourceLocation
    DeleteSourceLocation (DeleteSourceLocation'),
    newDeleteSourceLocation,
    DeleteSourceLocationResponse (DeleteSourceLocationResponse'),
    newDeleteSourceLocationResponse,

    -- ** DeleteVodSource
    DeleteVodSource (DeleteVodSource'),
    newDeleteVodSource,
    DeleteVodSourceResponse (DeleteVodSourceResponse'),
    newDeleteVodSourceResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeLiveSource
    DescribeLiveSource (DescribeLiveSource'),
    newDescribeLiveSource,
    DescribeLiveSourceResponse (DescribeLiveSourceResponse'),
    newDescribeLiveSourceResponse,

    -- ** DescribeProgram
    DescribeProgram (DescribeProgram'),
    newDescribeProgram,
    DescribeProgramResponse (DescribeProgramResponse'),
    newDescribeProgramResponse,

    -- ** DescribeSourceLocation
    DescribeSourceLocation (DescribeSourceLocation'),
    newDescribeSourceLocation,
    DescribeSourceLocationResponse (DescribeSourceLocationResponse'),
    newDescribeSourceLocationResponse,

    -- ** DescribeVodSource
    DescribeVodSource (DescribeVodSource'),
    newDescribeVodSource,
    DescribeVodSourceResponse (DescribeVodSourceResponse'),
    newDescribeVodSourceResponse,

    -- ** GetChannelPolicy
    GetChannelPolicy (GetChannelPolicy'),
    newGetChannelPolicy,
    GetChannelPolicyResponse (GetChannelPolicyResponse'),
    newGetChannelPolicyResponse,

    -- ** GetChannelSchedule (Paginated)
    GetChannelSchedule (GetChannelSchedule'),
    newGetChannelSchedule,
    GetChannelScheduleResponse (GetChannelScheduleResponse'),
    newGetChannelScheduleResponse,

    -- ** GetPlaybackConfiguration
    GetPlaybackConfiguration (GetPlaybackConfiguration'),
    newGetPlaybackConfiguration,
    GetPlaybackConfigurationResponse (GetPlaybackConfigurationResponse'),
    newGetPlaybackConfigurationResponse,

    -- ** GetPrefetchSchedule
    GetPrefetchSchedule (GetPrefetchSchedule'),
    newGetPrefetchSchedule,
    GetPrefetchScheduleResponse (GetPrefetchScheduleResponse'),
    newGetPrefetchScheduleResponse,

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

    -- ** ListLiveSources (Paginated)
    ListLiveSources (ListLiveSources'),
    newListLiveSources,
    ListLiveSourcesResponse (ListLiveSourcesResponse'),
    newListLiveSourcesResponse,

    -- ** ListPlaybackConfigurations (Paginated)
    ListPlaybackConfigurations (ListPlaybackConfigurations'),
    newListPlaybackConfigurations,
    ListPlaybackConfigurationsResponse (ListPlaybackConfigurationsResponse'),
    newListPlaybackConfigurationsResponse,

    -- ** ListPrefetchSchedules (Paginated)
    ListPrefetchSchedules (ListPrefetchSchedules'),
    newListPrefetchSchedules,
    ListPrefetchSchedulesResponse (ListPrefetchSchedulesResponse'),
    newListPrefetchSchedulesResponse,

    -- ** ListSourceLocations (Paginated)
    ListSourceLocations (ListSourceLocations'),
    newListSourceLocations,
    ListSourceLocationsResponse (ListSourceLocationsResponse'),
    newListSourceLocationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVodSources (Paginated)
    ListVodSources (ListVodSources'),
    newListVodSources,
    ListVodSourcesResponse (ListVodSourcesResponse'),
    newListVodSourcesResponse,

    -- ** PutChannelPolicy
    PutChannelPolicy (PutChannelPolicy'),
    newPutChannelPolicy,
    PutChannelPolicyResponse (PutChannelPolicyResponse'),
    newPutChannelPolicyResponse,

    -- ** PutPlaybackConfiguration
    PutPlaybackConfiguration (PutPlaybackConfiguration'),
    newPutPlaybackConfiguration,
    PutPlaybackConfigurationResponse (PutPlaybackConfigurationResponse'),
    newPutPlaybackConfigurationResponse,

    -- ** StartChannel
    StartChannel (StartChannel'),
    newStartChannel,
    StartChannelResponse (StartChannelResponse'),
    newStartChannelResponse,

    -- ** StopChannel
    StopChannel (StopChannel'),
    newStopChannel,
    StopChannelResponse (StopChannelResponse'),
    newStopChannelResponse,

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

    -- ** UpdateLiveSource
    UpdateLiveSource (UpdateLiveSource'),
    newUpdateLiveSource,
    UpdateLiveSourceResponse (UpdateLiveSourceResponse'),
    newUpdateLiveSourceResponse,

    -- ** UpdateProgram
    UpdateProgram (UpdateProgram'),
    newUpdateProgram,
    UpdateProgramResponse (UpdateProgramResponse'),
    newUpdateProgramResponse,

    -- ** UpdateSourceLocation
    UpdateSourceLocation (UpdateSourceLocation'),
    newUpdateSourceLocation,
    UpdateSourceLocationResponse (UpdateSourceLocationResponse'),
    newUpdateSourceLocationResponse,

    -- ** UpdateVodSource
    UpdateVodSource (UpdateVodSource'),
    newUpdateVodSource,
    UpdateVodSourceResponse (UpdateVodSourceResponse'),
    newUpdateVodSourceResponse,

    -- * Types

    -- ** AccessType
    AccessType (..),

    -- ** ChannelState
    ChannelState (..),

    -- ** FillPolicy
    FillPolicy (..),

    -- ** LogType
    LogType (..),

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

    -- ** Tier
    Tier (..),

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

    -- ** ClipRange
    ClipRange (ClipRange'),
    newClipRange,

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

    -- ** LiveSource
    LiveSource (LiveSource'),
    newLiveSource,

    -- ** LogConfiguration
    LogConfiguration (LogConfiguration'),
    newLogConfiguration,

    -- ** LogConfigurationForChannel
    LogConfigurationForChannel (LogConfigurationForChannel'),
    newLogConfigurationForChannel,

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

    -- ** SegmentDeliveryConfiguration
    SegmentDeliveryConfiguration (SegmentDeliveryConfiguration'),
    newSegmentDeliveryConfiguration,

    -- ** SegmentationDescriptor
    SegmentationDescriptor (SegmentationDescriptor'),
    newSegmentationDescriptor,

    -- ** SlateSource
    SlateSource (SlateSource'),
    newSlateSource,

    -- ** SourceLocation
    SourceLocation (SourceLocation'),
    newSourceLocation,

    -- ** SpliceInsertMessage
    SpliceInsertMessage (SpliceInsertMessage'),
    newSpliceInsertMessage,

    -- ** TimeSignalMessage
    TimeSignalMessage (TimeSignalMessage'),
    newTimeSignalMessage,

    -- ** Transition
    Transition (Transition'),
    newTransition,

    -- ** UpdateProgramScheduleConfiguration
    UpdateProgramScheduleConfiguration (UpdateProgramScheduleConfiguration'),
    newUpdateProgramScheduleConfiguration,

    -- ** UpdateProgramTransition
    UpdateProgramTransition (UpdateProgramTransition'),
    newUpdateProgramTransition,

    -- ** VodSource
    VodSource (VodSource'),
    newVodSource,
  )
where

import Amazonka.MediaTailor.ConfigureLogsForChannel
import Amazonka.MediaTailor.ConfigureLogsForPlaybackConfiguration
import Amazonka.MediaTailor.CreateChannel
import Amazonka.MediaTailor.CreateLiveSource
import Amazonka.MediaTailor.CreatePrefetchSchedule
import Amazonka.MediaTailor.CreateProgram
import Amazonka.MediaTailor.CreateSourceLocation
import Amazonka.MediaTailor.CreateVodSource
import Amazonka.MediaTailor.DeleteChannel
import Amazonka.MediaTailor.DeleteChannelPolicy
import Amazonka.MediaTailor.DeleteLiveSource
import Amazonka.MediaTailor.DeletePlaybackConfiguration
import Amazonka.MediaTailor.DeletePrefetchSchedule
import Amazonka.MediaTailor.DeleteProgram
import Amazonka.MediaTailor.DeleteSourceLocation
import Amazonka.MediaTailor.DeleteVodSource
import Amazonka.MediaTailor.DescribeChannel
import Amazonka.MediaTailor.DescribeLiveSource
import Amazonka.MediaTailor.DescribeProgram
import Amazonka.MediaTailor.DescribeSourceLocation
import Amazonka.MediaTailor.DescribeVodSource
import Amazonka.MediaTailor.GetChannelPolicy
import Amazonka.MediaTailor.GetChannelSchedule
import Amazonka.MediaTailor.GetPlaybackConfiguration
import Amazonka.MediaTailor.GetPrefetchSchedule
import Amazonka.MediaTailor.Lens
import Amazonka.MediaTailor.ListAlerts
import Amazonka.MediaTailor.ListChannels
import Amazonka.MediaTailor.ListLiveSources
import Amazonka.MediaTailor.ListPlaybackConfigurations
import Amazonka.MediaTailor.ListPrefetchSchedules
import Amazonka.MediaTailor.ListSourceLocations
import Amazonka.MediaTailor.ListTagsForResource
import Amazonka.MediaTailor.ListVodSources
import Amazonka.MediaTailor.PutChannelPolicy
import Amazonka.MediaTailor.PutPlaybackConfiguration
import Amazonka.MediaTailor.StartChannel
import Amazonka.MediaTailor.StopChannel
import Amazonka.MediaTailor.TagResource
import Amazonka.MediaTailor.Types
import Amazonka.MediaTailor.UntagResource
import Amazonka.MediaTailor.UpdateChannel
import Amazonka.MediaTailor.UpdateLiveSource
import Amazonka.MediaTailor.UpdateProgram
import Amazonka.MediaTailor.UpdateSourceLocation
import Amazonka.MediaTailor.UpdateVodSource
import Amazonka.MediaTailor.Waiters

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
