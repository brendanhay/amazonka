{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Lens
  ( -- * Operations

    -- ** ConfigureLogsForPlaybackConfiguration
    configureLogsForPlaybackConfiguration_percentEnabled,
    configureLogsForPlaybackConfiguration_playbackConfigurationName,
    configureLogsForPlaybackConfigurationResponse_playbackConfigurationName,
    configureLogsForPlaybackConfigurationResponse_httpStatus,
    configureLogsForPlaybackConfigurationResponse_percentEnabled,

    -- ** CreateChannel
    createChannel_tags,
    createChannel_fillerSlate,
    createChannel_tier,
    createChannel_channelName,
    createChannel_outputs,
    createChannel_playbackMode,
    createChannelResponse_tags,
    createChannelResponse_channelName,
    createChannelResponse_fillerSlate,
    createChannelResponse_arn,
    createChannelResponse_lastModifiedTime,
    createChannelResponse_tier,
    createChannelResponse_outputs,
    createChannelResponse_creationTime,
    createChannelResponse_playbackMode,
    createChannelResponse_channelState,
    createChannelResponse_httpStatus,

    -- ** CreateLiveSource
    createLiveSource_tags,
    createLiveSource_httpPackageConfigurations,
    createLiveSource_liveSourceName,
    createLiveSource_sourceLocationName,
    createLiveSourceResponse_tags,
    createLiveSourceResponse_liveSourceName,
    createLiveSourceResponse_arn,
    createLiveSourceResponse_lastModifiedTime,
    createLiveSourceResponse_creationTime,
    createLiveSourceResponse_sourceLocationName,
    createLiveSourceResponse_httpPackageConfigurations,
    createLiveSourceResponse_httpStatus,

    -- ** CreatePrefetchSchedule
    createPrefetchSchedule_streamId,
    createPrefetchSchedule_consumption,
    createPrefetchSchedule_name,
    createPrefetchSchedule_playbackConfigurationName,
    createPrefetchSchedule_retrieval,
    createPrefetchScheduleResponse_name,
    createPrefetchScheduleResponse_arn,
    createPrefetchScheduleResponse_streamId,
    createPrefetchScheduleResponse_retrieval,
    createPrefetchScheduleResponse_playbackConfigurationName,
    createPrefetchScheduleResponse_consumption,
    createPrefetchScheduleResponse_httpStatus,

    -- ** CreateProgram
    createProgram_liveSourceName,
    createProgram_vodSourceName,
    createProgram_adBreaks,
    createProgram_channelName,
    createProgram_programName,
    createProgram_scheduleConfiguration,
    createProgram_sourceLocationName,
    createProgramResponse_scheduledStartTime,
    createProgramResponse_liveSourceName,
    createProgramResponse_programName,
    createProgramResponse_channelName,
    createProgramResponse_vodSourceName,
    createProgramResponse_arn,
    createProgramResponse_adBreaks,
    createProgramResponse_creationTime,
    createProgramResponse_sourceLocationName,
    createProgramResponse_httpStatus,

    -- ** CreateSourceLocation
    createSourceLocation_tags,
    createSourceLocation_segmentDeliveryConfigurations,
    createSourceLocation_accessConfiguration,
    createSourceLocation_defaultSegmentDeliveryConfiguration,
    createSourceLocation_httpConfiguration,
    createSourceLocation_sourceLocationName,
    createSourceLocationResponse_tags,
    createSourceLocationResponse_segmentDeliveryConfigurations,
    createSourceLocationResponse_arn,
    createSourceLocationResponse_accessConfiguration,
    createSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    createSourceLocationResponse_lastModifiedTime,
    createSourceLocationResponse_creationTime,
    createSourceLocationResponse_sourceLocationName,
    createSourceLocationResponse_httpConfiguration,
    createSourceLocationResponse_httpStatus,

    -- ** CreateVodSource
    createVodSource_tags,
    createVodSource_httpPackageConfigurations,
    createVodSource_sourceLocationName,
    createVodSource_vodSourceName,
    createVodSourceResponse_tags,
    createVodSourceResponse_vodSourceName,
    createVodSourceResponse_arn,
    createVodSourceResponse_lastModifiedTime,
    createVodSourceResponse_creationTime,
    createVodSourceResponse_sourceLocationName,
    createVodSourceResponse_httpPackageConfigurations,
    createVodSourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelName,
    deleteChannelResponse_httpStatus,

    -- ** DeleteChannelPolicy
    deleteChannelPolicy_channelName,
    deleteChannelPolicyResponse_httpStatus,

    -- ** DeleteLiveSource
    deleteLiveSource_liveSourceName,
    deleteLiveSource_sourceLocationName,
    deleteLiveSourceResponse_httpStatus,

    -- ** DeletePlaybackConfiguration
    deletePlaybackConfiguration_name,
    deletePlaybackConfigurationResponse_httpStatus,

    -- ** DeletePrefetchSchedule
    deletePrefetchSchedule_name,
    deletePrefetchSchedule_playbackConfigurationName,
    deletePrefetchScheduleResponse_httpStatus,

    -- ** DeleteProgram
    deleteProgram_channelName,
    deleteProgram_programName,
    deleteProgramResponse_httpStatus,

    -- ** DeleteSourceLocation
    deleteSourceLocation_sourceLocationName,
    deleteSourceLocationResponse_httpStatus,

    -- ** DeleteVodSource
    deleteVodSource_sourceLocationName,
    deleteVodSource_vodSourceName,
    deleteVodSourceResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_channelName,
    describeChannelResponse_tags,
    describeChannelResponse_channelName,
    describeChannelResponse_fillerSlate,
    describeChannelResponse_arn,
    describeChannelResponse_lastModifiedTime,
    describeChannelResponse_tier,
    describeChannelResponse_outputs,
    describeChannelResponse_creationTime,
    describeChannelResponse_playbackMode,
    describeChannelResponse_channelState,
    describeChannelResponse_httpStatus,

    -- ** DescribeLiveSource
    describeLiveSource_liveSourceName,
    describeLiveSource_sourceLocationName,
    describeLiveSourceResponse_tags,
    describeLiveSourceResponse_liveSourceName,
    describeLiveSourceResponse_arn,
    describeLiveSourceResponse_lastModifiedTime,
    describeLiveSourceResponse_creationTime,
    describeLiveSourceResponse_sourceLocationName,
    describeLiveSourceResponse_httpPackageConfigurations,
    describeLiveSourceResponse_httpStatus,

    -- ** DescribeProgram
    describeProgram_channelName,
    describeProgram_programName,
    describeProgramResponse_scheduledStartTime,
    describeProgramResponse_liveSourceName,
    describeProgramResponse_programName,
    describeProgramResponse_channelName,
    describeProgramResponse_vodSourceName,
    describeProgramResponse_arn,
    describeProgramResponse_adBreaks,
    describeProgramResponse_creationTime,
    describeProgramResponse_sourceLocationName,
    describeProgramResponse_httpStatus,

    -- ** DescribeSourceLocation
    describeSourceLocation_sourceLocationName,
    describeSourceLocationResponse_tags,
    describeSourceLocationResponse_segmentDeliveryConfigurations,
    describeSourceLocationResponse_arn,
    describeSourceLocationResponse_accessConfiguration,
    describeSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    describeSourceLocationResponse_lastModifiedTime,
    describeSourceLocationResponse_creationTime,
    describeSourceLocationResponse_sourceLocationName,
    describeSourceLocationResponse_httpConfiguration,
    describeSourceLocationResponse_httpStatus,

    -- ** DescribeVodSource
    describeVodSource_sourceLocationName,
    describeVodSource_vodSourceName,
    describeVodSourceResponse_tags,
    describeVodSourceResponse_vodSourceName,
    describeVodSourceResponse_arn,
    describeVodSourceResponse_lastModifiedTime,
    describeVodSourceResponse_creationTime,
    describeVodSourceResponse_sourceLocationName,
    describeVodSourceResponse_httpPackageConfigurations,
    describeVodSourceResponse_httpStatus,

    -- ** GetChannelPolicy
    getChannelPolicy_channelName,
    getChannelPolicyResponse_policy,
    getChannelPolicyResponse_httpStatus,

    -- ** GetChannelSchedule
    getChannelSchedule_nextToken,
    getChannelSchedule_maxResults,
    getChannelSchedule_durationMinutes,
    getChannelSchedule_channelName,
    getChannelScheduleResponse_items,
    getChannelScheduleResponse_nextToken,
    getChannelScheduleResponse_httpStatus,

    -- ** GetPlaybackConfiguration
    getPlaybackConfiguration_name,
    getPlaybackConfigurationResponse_tags,
    getPlaybackConfigurationResponse_name,
    getPlaybackConfigurationResponse_configurationAliases,
    getPlaybackConfigurationResponse_logConfiguration,
    getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    getPlaybackConfigurationResponse_cdnConfiguration,
    getPlaybackConfigurationResponse_bumper,
    getPlaybackConfigurationResponse_manifestProcessingRules,
    getPlaybackConfigurationResponse_livePreRollConfiguration,
    getPlaybackConfigurationResponse_dashConfiguration,
    getPlaybackConfigurationResponse_adDecisionServerUrl,
    getPlaybackConfigurationResponse_transcodeProfileName,
    getPlaybackConfigurationResponse_videoContentSourceUrl,
    getPlaybackConfigurationResponse_playbackConfigurationArn,
    getPlaybackConfigurationResponse_hlsConfiguration,
    getPlaybackConfigurationResponse_slateAdUrl,
    getPlaybackConfigurationResponse_availSuppression,
    getPlaybackConfigurationResponse_playbackEndpointPrefix,
    getPlaybackConfigurationResponse_personalizationThresholdSeconds,
    getPlaybackConfigurationResponse_httpStatus,

    -- ** GetPrefetchSchedule
    getPrefetchSchedule_name,
    getPrefetchSchedule_playbackConfigurationName,
    getPrefetchScheduleResponse_name,
    getPrefetchScheduleResponse_arn,
    getPrefetchScheduleResponse_streamId,
    getPrefetchScheduleResponse_retrieval,
    getPrefetchScheduleResponse_playbackConfigurationName,
    getPrefetchScheduleResponse_consumption,
    getPrefetchScheduleResponse_httpStatus,

    -- ** ListAlerts
    listAlerts_nextToken,
    listAlerts_maxResults,
    listAlerts_resourceArn,
    listAlertsResponse_items,
    listAlertsResponse_nextToken,
    listAlertsResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_items,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListLiveSources
    listLiveSources_nextToken,
    listLiveSources_maxResults,
    listLiveSources_sourceLocationName,
    listLiveSourcesResponse_items,
    listLiveSourcesResponse_nextToken,
    listLiveSourcesResponse_httpStatus,

    -- ** ListPlaybackConfigurations
    listPlaybackConfigurations_nextToken,
    listPlaybackConfigurations_maxResults,
    listPlaybackConfigurationsResponse_items,
    listPlaybackConfigurationsResponse_nextToken,
    listPlaybackConfigurationsResponse_httpStatus,

    -- ** ListPrefetchSchedules
    listPrefetchSchedules_nextToken,
    listPrefetchSchedules_streamId,
    listPrefetchSchedules_maxResults,
    listPrefetchSchedules_playbackConfigurationName,
    listPrefetchSchedulesResponse_items,
    listPrefetchSchedulesResponse_nextToken,
    listPrefetchSchedulesResponse_httpStatus,

    -- ** ListSourceLocations
    listSourceLocations_nextToken,
    listSourceLocations_maxResults,
    listSourceLocationsResponse_items,
    listSourceLocationsResponse_nextToken,
    listSourceLocationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVodSources
    listVodSources_nextToken,
    listVodSources_maxResults,
    listVodSources_sourceLocationName,
    listVodSourcesResponse_items,
    listVodSourcesResponse_nextToken,
    listVodSourcesResponse_httpStatus,

    -- ** PutChannelPolicy
    putChannelPolicy_channelName,
    putChannelPolicy_policy,
    putChannelPolicyResponse_httpStatus,

    -- ** PutPlaybackConfiguration
    putPlaybackConfiguration_tags,
    putPlaybackConfiguration_configurationAliases,
    putPlaybackConfiguration_cdnConfiguration,
    putPlaybackConfiguration_bumper,
    putPlaybackConfiguration_manifestProcessingRules,
    putPlaybackConfiguration_livePreRollConfiguration,
    putPlaybackConfiguration_dashConfiguration,
    putPlaybackConfiguration_adDecisionServerUrl,
    putPlaybackConfiguration_transcodeProfileName,
    putPlaybackConfiguration_videoContentSourceUrl,
    putPlaybackConfiguration_slateAdUrl,
    putPlaybackConfiguration_availSuppression,
    putPlaybackConfiguration_personalizationThresholdSeconds,
    putPlaybackConfiguration_name,
    putPlaybackConfigurationResponse_tags,
    putPlaybackConfigurationResponse_name,
    putPlaybackConfigurationResponse_configurationAliases,
    putPlaybackConfigurationResponse_logConfiguration,
    putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    putPlaybackConfigurationResponse_cdnConfiguration,
    putPlaybackConfigurationResponse_bumper,
    putPlaybackConfigurationResponse_manifestProcessingRules,
    putPlaybackConfigurationResponse_livePreRollConfiguration,
    putPlaybackConfigurationResponse_dashConfiguration,
    putPlaybackConfigurationResponse_adDecisionServerUrl,
    putPlaybackConfigurationResponse_transcodeProfileName,
    putPlaybackConfigurationResponse_videoContentSourceUrl,
    putPlaybackConfigurationResponse_playbackConfigurationArn,
    putPlaybackConfigurationResponse_hlsConfiguration,
    putPlaybackConfigurationResponse_slateAdUrl,
    putPlaybackConfigurationResponse_availSuppression,
    putPlaybackConfigurationResponse_playbackEndpointPrefix,
    putPlaybackConfigurationResponse_personalizationThresholdSeconds,
    putPlaybackConfigurationResponse_httpStatus,

    -- ** StartChannel
    startChannel_channelName,
    startChannelResponse_httpStatus,

    -- ** StopChannel
    stopChannel_channelName,
    stopChannelResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateChannel
    updateChannel_fillerSlate,
    updateChannel_channelName,
    updateChannel_outputs,
    updateChannelResponse_tags,
    updateChannelResponse_channelName,
    updateChannelResponse_fillerSlate,
    updateChannelResponse_arn,
    updateChannelResponse_lastModifiedTime,
    updateChannelResponse_tier,
    updateChannelResponse_outputs,
    updateChannelResponse_creationTime,
    updateChannelResponse_playbackMode,
    updateChannelResponse_channelState,
    updateChannelResponse_httpStatus,

    -- ** UpdateLiveSource
    updateLiveSource_httpPackageConfigurations,
    updateLiveSource_liveSourceName,
    updateLiveSource_sourceLocationName,
    updateLiveSourceResponse_tags,
    updateLiveSourceResponse_liveSourceName,
    updateLiveSourceResponse_arn,
    updateLiveSourceResponse_lastModifiedTime,
    updateLiveSourceResponse_creationTime,
    updateLiveSourceResponse_sourceLocationName,
    updateLiveSourceResponse_httpPackageConfigurations,
    updateLiveSourceResponse_httpStatus,

    -- ** UpdateSourceLocation
    updateSourceLocation_segmentDeliveryConfigurations,
    updateSourceLocation_accessConfiguration,
    updateSourceLocation_defaultSegmentDeliveryConfiguration,
    updateSourceLocation_httpConfiguration,
    updateSourceLocation_sourceLocationName,
    updateSourceLocationResponse_tags,
    updateSourceLocationResponse_segmentDeliveryConfigurations,
    updateSourceLocationResponse_arn,
    updateSourceLocationResponse_accessConfiguration,
    updateSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    updateSourceLocationResponse_lastModifiedTime,
    updateSourceLocationResponse_creationTime,
    updateSourceLocationResponse_sourceLocationName,
    updateSourceLocationResponse_httpConfiguration,
    updateSourceLocationResponse_httpStatus,

    -- ** UpdateVodSource
    updateVodSource_httpPackageConfigurations,
    updateVodSource_sourceLocationName,
    updateVodSource_vodSourceName,
    updateVodSourceResponse_tags,
    updateVodSourceResponse_vodSourceName,
    updateVodSourceResponse_arn,
    updateVodSourceResponse_lastModifiedTime,
    updateVodSourceResponse_creationTime,
    updateVodSourceResponse_sourceLocationName,
    updateVodSourceResponse_httpPackageConfigurations,
    updateVodSourceResponse_httpStatus,

    -- * Types

    -- ** AccessConfiguration
    accessConfiguration_secretsManagerAccessTokenConfiguration,
    accessConfiguration_accessType,

    -- ** AdBreak
    adBreak_messageType,
    adBreak_offsetMillis,
    adBreak_spliceInsertMessage,
    adBreak_slate,
    adBreak_timeSignalMessage,

    -- ** AdMarkerPassthrough
    adMarkerPassthrough_enabled,

    -- ** Alert
    alert_alertCode,
    alert_alertMessage,
    alert_lastModifiedTime,
    alert_relatedResourceArns,
    alert_resourceArn,

    -- ** AvailMatchingCriteria
    availMatchingCriteria_dynamicVariable,
    availMatchingCriteria_operator,

    -- ** AvailSuppression
    availSuppression_mode,
    availSuppression_value,

    -- ** Bumper
    bumper_startUrl,
    bumper_endUrl,

    -- ** CdnConfiguration
    cdnConfiguration_adSegmentUrlPrefix,
    cdnConfiguration_contentSegmentUrlPrefix,

    -- ** Channel
    channel_tags,
    channel_fillerSlate,
    channel_lastModifiedTime,
    channel_creationTime,
    channel_arn,
    channel_channelName,
    channel_channelState,
    channel_outputs,
    channel_playbackMode,
    channel_tier,

    -- ** DashConfiguration
    dashConfiguration_manifestEndpointPrefix,
    dashConfiguration_mpdLocation,
    dashConfiguration_originManifestType,

    -- ** DashConfigurationForPut
    dashConfigurationForPut_mpdLocation,
    dashConfigurationForPut_originManifestType,

    -- ** DashPlaylistSettings
    dashPlaylistSettings_suggestedPresentationDelaySeconds,
    dashPlaylistSettings_manifestWindowSeconds,
    dashPlaylistSettings_minUpdatePeriodSeconds,
    dashPlaylistSettings_minBufferTimeSeconds,

    -- ** DefaultSegmentDeliveryConfiguration
    defaultSegmentDeliveryConfiguration_baseUrl,

    -- ** HlsConfiguration
    hlsConfiguration_manifestEndpointPrefix,

    -- ** HlsPlaylistSettings
    hlsPlaylistSettings_manifestWindowSeconds,

    -- ** HttpConfiguration
    httpConfiguration_baseUrl,

    -- ** HttpPackageConfiguration
    httpPackageConfiguration_path,
    httpPackageConfiguration_sourceGroup,
    httpPackageConfiguration_type,

    -- ** LivePreRollConfiguration
    livePreRollConfiguration_adDecisionServerUrl,
    livePreRollConfiguration_maxDurationSeconds,

    -- ** LiveSource
    liveSource_tags,
    liveSource_lastModifiedTime,
    liveSource_creationTime,
    liveSource_arn,
    liveSource_httpPackageConfigurations,
    liveSource_liveSourceName,
    liveSource_sourceLocationName,

    -- ** LogConfiguration
    logConfiguration_percentEnabled,

    -- ** ManifestProcessingRules
    manifestProcessingRules_adMarkerPassthrough,

    -- ** PlaybackConfiguration
    playbackConfiguration_tags,
    playbackConfiguration_name,
    playbackConfiguration_configurationAliases,
    playbackConfiguration_logConfiguration,
    playbackConfiguration_sessionInitializationEndpointPrefix,
    playbackConfiguration_cdnConfiguration,
    playbackConfiguration_bumper,
    playbackConfiguration_manifestProcessingRules,
    playbackConfiguration_livePreRollConfiguration,
    playbackConfiguration_dashConfiguration,
    playbackConfiguration_adDecisionServerUrl,
    playbackConfiguration_transcodeProfileName,
    playbackConfiguration_videoContentSourceUrl,
    playbackConfiguration_playbackConfigurationArn,
    playbackConfiguration_hlsConfiguration,
    playbackConfiguration_slateAdUrl,
    playbackConfiguration_availSuppression,
    playbackConfiguration_playbackEndpointPrefix,
    playbackConfiguration_personalizationThresholdSeconds,

    -- ** PrefetchConsumption
    prefetchConsumption_availMatchingCriteria,
    prefetchConsumption_startTime,
    prefetchConsumption_endTime,

    -- ** PrefetchRetrieval
    prefetchRetrieval_dynamicVariables,
    prefetchRetrieval_startTime,
    prefetchRetrieval_endTime,

    -- ** PrefetchSchedule
    prefetchSchedule_streamId,
    prefetchSchedule_arn,
    prefetchSchedule_consumption,
    prefetchSchedule_name,
    prefetchSchedule_playbackConfigurationName,
    prefetchSchedule_retrieval,

    -- ** RequestOutputItem
    requestOutputItem_hlsPlaylistSettings,
    requestOutputItem_dashPlaylistSettings,
    requestOutputItem_manifestName,
    requestOutputItem_sourceGroup,

    -- ** ResponseOutputItem
    responseOutputItem_hlsPlaylistSettings,
    responseOutputItem_dashPlaylistSettings,
    responseOutputItem_manifestName,
    responseOutputItem_playbackUrl,
    responseOutputItem_sourceGroup,

    -- ** ScheduleAdBreak
    scheduleAdBreak_approximateStartTime,
    scheduleAdBreak_vodSourceName,
    scheduleAdBreak_approximateDurationSeconds,
    scheduleAdBreak_sourceLocationName,

    -- ** ScheduleConfiguration
    scheduleConfiguration_transition,

    -- ** ScheduleEntry
    scheduleEntry_approximateStartTime,
    scheduleEntry_liveSourceName,
    scheduleEntry_vodSourceName,
    scheduleEntry_approximateDurationSeconds,
    scheduleEntry_scheduleEntryType,
    scheduleEntry_scheduleAdBreaks,
    scheduleEntry_arn,
    scheduleEntry_channelName,
    scheduleEntry_programName,
    scheduleEntry_sourceLocationName,

    -- ** SecretsManagerAccessTokenConfiguration
    secretsManagerAccessTokenConfiguration_headerName,
    secretsManagerAccessTokenConfiguration_secretArn,
    secretsManagerAccessTokenConfiguration_secretStringKey,

    -- ** SegmentDeliveryConfiguration
    segmentDeliveryConfiguration_baseUrl,
    segmentDeliveryConfiguration_name,

    -- ** SegmentationDescriptor
    segmentationDescriptor_subSegmentsExpected,
    segmentationDescriptor_segmentationTypeId,
    segmentationDescriptor_segmentationUpidType,
    segmentationDescriptor_segmentNum,
    segmentationDescriptor_segmentationUpid,
    segmentationDescriptor_subSegmentNum,
    segmentationDescriptor_segmentsExpected,
    segmentationDescriptor_segmentationEventId,

    -- ** SlateSource
    slateSource_vodSourceName,
    slateSource_sourceLocationName,

    -- ** SourceLocation
    sourceLocation_tags,
    sourceLocation_segmentDeliveryConfigurations,
    sourceLocation_accessConfiguration,
    sourceLocation_defaultSegmentDeliveryConfiguration,
    sourceLocation_lastModifiedTime,
    sourceLocation_creationTime,
    sourceLocation_arn,
    sourceLocation_httpConfiguration,
    sourceLocation_sourceLocationName,

    -- ** SpliceInsertMessage
    spliceInsertMessage_availsExpected,
    spliceInsertMessage_spliceEventId,
    spliceInsertMessage_availNum,
    spliceInsertMessage_uniqueProgramId,

    -- ** TimeSignalMessage
    timeSignalMessage_segmentationDescriptors,

    -- ** Transition
    transition_relativeProgram,
    transition_scheduledStartTimeMillis,
    transition_durationMillis,
    transition_relativePosition,
    transition_type,

    -- ** VodSource
    vodSource_tags,
    vodSource_lastModifiedTime,
    vodSource_creationTime,
    vodSource_arn,
    vodSource_httpPackageConfigurations,
    vodSource_sourceLocationName,
    vodSource_vodSourceName,
  )
where

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
import Amazonka.MediaTailor.Types.AccessConfiguration
import Amazonka.MediaTailor.Types.AdBreak
import Amazonka.MediaTailor.Types.AdMarkerPassthrough
import Amazonka.MediaTailor.Types.Alert
import Amazonka.MediaTailor.Types.AvailMatchingCriteria
import Amazonka.MediaTailor.Types.AvailSuppression
import Amazonka.MediaTailor.Types.Bumper
import Amazonka.MediaTailor.Types.CdnConfiguration
import Amazonka.MediaTailor.Types.Channel
import Amazonka.MediaTailor.Types.DashConfiguration
import Amazonka.MediaTailor.Types.DashConfigurationForPut
import Amazonka.MediaTailor.Types.DashPlaylistSettings
import Amazonka.MediaTailor.Types.DefaultSegmentDeliveryConfiguration
import Amazonka.MediaTailor.Types.HlsConfiguration
import Amazonka.MediaTailor.Types.HlsPlaylistSettings
import Amazonka.MediaTailor.Types.HttpConfiguration
import Amazonka.MediaTailor.Types.HttpPackageConfiguration
import Amazonka.MediaTailor.Types.LivePreRollConfiguration
import Amazonka.MediaTailor.Types.LiveSource
import Amazonka.MediaTailor.Types.LogConfiguration
import Amazonka.MediaTailor.Types.ManifestProcessingRules
import Amazonka.MediaTailor.Types.PlaybackConfiguration
import Amazonka.MediaTailor.Types.PrefetchConsumption
import Amazonka.MediaTailor.Types.PrefetchRetrieval
import Amazonka.MediaTailor.Types.PrefetchSchedule
import Amazonka.MediaTailor.Types.RequestOutputItem
import Amazonka.MediaTailor.Types.ResponseOutputItem
import Amazonka.MediaTailor.Types.ScheduleAdBreak
import Amazonka.MediaTailor.Types.ScheduleConfiguration
import Amazonka.MediaTailor.Types.ScheduleEntry
import Amazonka.MediaTailor.Types.SecretsManagerAccessTokenConfiguration
import Amazonka.MediaTailor.Types.SegmentDeliveryConfiguration
import Amazonka.MediaTailor.Types.SegmentationDescriptor
import Amazonka.MediaTailor.Types.SlateSource
import Amazonka.MediaTailor.Types.SourceLocation
import Amazonka.MediaTailor.Types.SpliceInsertMessage
import Amazonka.MediaTailor.Types.TimeSignalMessage
import Amazonka.MediaTailor.Types.Transition
import Amazonka.MediaTailor.Types.VodSource
import Amazonka.MediaTailor.UntagResource
import Amazonka.MediaTailor.UpdateChannel
import Amazonka.MediaTailor.UpdateLiveSource
import Amazonka.MediaTailor.UpdateSourceLocation
import Amazonka.MediaTailor.UpdateVodSource
