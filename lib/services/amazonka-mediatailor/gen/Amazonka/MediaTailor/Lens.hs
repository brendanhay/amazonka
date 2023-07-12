{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createChannel_fillerSlate,
    createChannel_tags,
    createChannel_tier,
    createChannel_channelName,
    createChannel_outputs,
    createChannel_playbackMode,
    createChannelResponse_arn,
    createChannelResponse_channelName,
    createChannelResponse_channelState,
    createChannelResponse_creationTime,
    createChannelResponse_fillerSlate,
    createChannelResponse_lastModifiedTime,
    createChannelResponse_outputs,
    createChannelResponse_playbackMode,
    createChannelResponse_tags,
    createChannelResponse_tier,
    createChannelResponse_httpStatus,

    -- ** CreateLiveSource
    createLiveSource_tags,
    createLiveSource_httpPackageConfigurations,
    createLiveSource_liveSourceName,
    createLiveSource_sourceLocationName,
    createLiveSourceResponse_arn,
    createLiveSourceResponse_creationTime,
    createLiveSourceResponse_httpPackageConfigurations,
    createLiveSourceResponse_lastModifiedTime,
    createLiveSourceResponse_liveSourceName,
    createLiveSourceResponse_sourceLocationName,
    createLiveSourceResponse_tags,
    createLiveSourceResponse_httpStatus,

    -- ** CreatePrefetchSchedule
    createPrefetchSchedule_streamId,
    createPrefetchSchedule_consumption,
    createPrefetchSchedule_name,
    createPrefetchSchedule_playbackConfigurationName,
    createPrefetchSchedule_retrieval,
    createPrefetchScheduleResponse_arn,
    createPrefetchScheduleResponse_consumption,
    createPrefetchScheduleResponse_name,
    createPrefetchScheduleResponse_playbackConfigurationName,
    createPrefetchScheduleResponse_retrieval,
    createPrefetchScheduleResponse_streamId,
    createPrefetchScheduleResponse_httpStatus,

    -- ** CreateProgram
    createProgram_adBreaks,
    createProgram_liveSourceName,
    createProgram_vodSourceName,
    createProgram_channelName,
    createProgram_programName,
    createProgram_scheduleConfiguration,
    createProgram_sourceLocationName,
    createProgramResponse_adBreaks,
    createProgramResponse_arn,
    createProgramResponse_channelName,
    createProgramResponse_creationTime,
    createProgramResponse_liveSourceName,
    createProgramResponse_programName,
    createProgramResponse_scheduledStartTime,
    createProgramResponse_sourceLocationName,
    createProgramResponse_vodSourceName,
    createProgramResponse_httpStatus,

    -- ** CreateSourceLocation
    createSourceLocation_accessConfiguration,
    createSourceLocation_defaultSegmentDeliveryConfiguration,
    createSourceLocation_segmentDeliveryConfigurations,
    createSourceLocation_tags,
    createSourceLocation_httpConfiguration,
    createSourceLocation_sourceLocationName,
    createSourceLocationResponse_accessConfiguration,
    createSourceLocationResponse_arn,
    createSourceLocationResponse_creationTime,
    createSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    createSourceLocationResponse_httpConfiguration,
    createSourceLocationResponse_lastModifiedTime,
    createSourceLocationResponse_segmentDeliveryConfigurations,
    createSourceLocationResponse_sourceLocationName,
    createSourceLocationResponse_tags,
    createSourceLocationResponse_httpStatus,

    -- ** CreateVodSource
    createVodSource_tags,
    createVodSource_httpPackageConfigurations,
    createVodSource_sourceLocationName,
    createVodSource_vodSourceName,
    createVodSourceResponse_arn,
    createVodSourceResponse_creationTime,
    createVodSourceResponse_httpPackageConfigurations,
    createVodSourceResponse_lastModifiedTime,
    createVodSourceResponse_sourceLocationName,
    createVodSourceResponse_tags,
    createVodSourceResponse_vodSourceName,
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
    describeChannelResponse_arn,
    describeChannelResponse_channelName,
    describeChannelResponse_channelState,
    describeChannelResponse_creationTime,
    describeChannelResponse_fillerSlate,
    describeChannelResponse_lastModifiedTime,
    describeChannelResponse_outputs,
    describeChannelResponse_playbackMode,
    describeChannelResponse_tags,
    describeChannelResponse_tier,
    describeChannelResponse_httpStatus,

    -- ** DescribeLiveSource
    describeLiveSource_liveSourceName,
    describeLiveSource_sourceLocationName,
    describeLiveSourceResponse_arn,
    describeLiveSourceResponse_creationTime,
    describeLiveSourceResponse_httpPackageConfigurations,
    describeLiveSourceResponse_lastModifiedTime,
    describeLiveSourceResponse_liveSourceName,
    describeLiveSourceResponse_sourceLocationName,
    describeLiveSourceResponse_tags,
    describeLiveSourceResponse_httpStatus,

    -- ** DescribeProgram
    describeProgram_channelName,
    describeProgram_programName,
    describeProgramResponse_adBreaks,
    describeProgramResponse_arn,
    describeProgramResponse_channelName,
    describeProgramResponse_creationTime,
    describeProgramResponse_liveSourceName,
    describeProgramResponse_programName,
    describeProgramResponse_scheduledStartTime,
    describeProgramResponse_sourceLocationName,
    describeProgramResponse_vodSourceName,
    describeProgramResponse_httpStatus,

    -- ** DescribeSourceLocation
    describeSourceLocation_sourceLocationName,
    describeSourceLocationResponse_accessConfiguration,
    describeSourceLocationResponse_arn,
    describeSourceLocationResponse_creationTime,
    describeSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    describeSourceLocationResponse_httpConfiguration,
    describeSourceLocationResponse_lastModifiedTime,
    describeSourceLocationResponse_segmentDeliveryConfigurations,
    describeSourceLocationResponse_sourceLocationName,
    describeSourceLocationResponse_tags,
    describeSourceLocationResponse_httpStatus,

    -- ** DescribeVodSource
    describeVodSource_sourceLocationName,
    describeVodSource_vodSourceName,
    describeVodSourceResponse_arn,
    describeVodSourceResponse_creationTime,
    describeVodSourceResponse_httpPackageConfigurations,
    describeVodSourceResponse_lastModifiedTime,
    describeVodSourceResponse_sourceLocationName,
    describeVodSourceResponse_tags,
    describeVodSourceResponse_vodSourceName,
    describeVodSourceResponse_httpStatus,

    -- ** GetChannelPolicy
    getChannelPolicy_channelName,
    getChannelPolicyResponse_policy,
    getChannelPolicyResponse_httpStatus,

    -- ** GetChannelSchedule
    getChannelSchedule_durationMinutes,
    getChannelSchedule_maxResults,
    getChannelSchedule_nextToken,
    getChannelSchedule_channelName,
    getChannelScheduleResponse_items,
    getChannelScheduleResponse_nextToken,
    getChannelScheduleResponse_httpStatus,

    -- ** GetPlaybackConfiguration
    getPlaybackConfiguration_name,
    getPlaybackConfigurationResponse_adDecisionServerUrl,
    getPlaybackConfigurationResponse_availSuppression,
    getPlaybackConfigurationResponse_bumper,
    getPlaybackConfigurationResponse_cdnConfiguration,
    getPlaybackConfigurationResponse_configurationAliases,
    getPlaybackConfigurationResponse_dashConfiguration,
    getPlaybackConfigurationResponse_hlsConfiguration,
    getPlaybackConfigurationResponse_livePreRollConfiguration,
    getPlaybackConfigurationResponse_logConfiguration,
    getPlaybackConfigurationResponse_manifestProcessingRules,
    getPlaybackConfigurationResponse_name,
    getPlaybackConfigurationResponse_personalizationThresholdSeconds,
    getPlaybackConfigurationResponse_playbackConfigurationArn,
    getPlaybackConfigurationResponse_playbackEndpointPrefix,
    getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    getPlaybackConfigurationResponse_slateAdUrl,
    getPlaybackConfigurationResponse_tags,
    getPlaybackConfigurationResponse_transcodeProfileName,
    getPlaybackConfigurationResponse_videoContentSourceUrl,
    getPlaybackConfigurationResponse_httpStatus,

    -- ** GetPrefetchSchedule
    getPrefetchSchedule_name,
    getPrefetchSchedule_playbackConfigurationName,
    getPrefetchScheduleResponse_arn,
    getPrefetchScheduleResponse_consumption,
    getPrefetchScheduleResponse_name,
    getPrefetchScheduleResponse_playbackConfigurationName,
    getPrefetchScheduleResponse_retrieval,
    getPrefetchScheduleResponse_streamId,
    getPrefetchScheduleResponse_httpStatus,

    -- ** ListAlerts
    listAlerts_maxResults,
    listAlerts_nextToken,
    listAlerts_resourceArn,
    listAlertsResponse_items,
    listAlertsResponse_nextToken,
    listAlertsResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_items,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListLiveSources
    listLiveSources_maxResults,
    listLiveSources_nextToken,
    listLiveSources_sourceLocationName,
    listLiveSourcesResponse_items,
    listLiveSourcesResponse_nextToken,
    listLiveSourcesResponse_httpStatus,

    -- ** ListPlaybackConfigurations
    listPlaybackConfigurations_maxResults,
    listPlaybackConfigurations_nextToken,
    listPlaybackConfigurationsResponse_items,
    listPlaybackConfigurationsResponse_nextToken,
    listPlaybackConfigurationsResponse_httpStatus,

    -- ** ListPrefetchSchedules
    listPrefetchSchedules_maxResults,
    listPrefetchSchedules_nextToken,
    listPrefetchSchedules_streamId,
    listPrefetchSchedules_playbackConfigurationName,
    listPrefetchSchedulesResponse_items,
    listPrefetchSchedulesResponse_nextToken,
    listPrefetchSchedulesResponse_httpStatus,

    -- ** ListSourceLocations
    listSourceLocations_maxResults,
    listSourceLocations_nextToken,
    listSourceLocationsResponse_items,
    listSourceLocationsResponse_nextToken,
    listSourceLocationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVodSources
    listVodSources_maxResults,
    listVodSources_nextToken,
    listVodSources_sourceLocationName,
    listVodSourcesResponse_items,
    listVodSourcesResponse_nextToken,
    listVodSourcesResponse_httpStatus,

    -- ** PutChannelPolicy
    putChannelPolicy_channelName,
    putChannelPolicy_policy,
    putChannelPolicyResponse_httpStatus,

    -- ** PutPlaybackConfiguration
    putPlaybackConfiguration_adDecisionServerUrl,
    putPlaybackConfiguration_availSuppression,
    putPlaybackConfiguration_bumper,
    putPlaybackConfiguration_cdnConfiguration,
    putPlaybackConfiguration_configurationAliases,
    putPlaybackConfiguration_dashConfiguration,
    putPlaybackConfiguration_livePreRollConfiguration,
    putPlaybackConfiguration_manifestProcessingRules,
    putPlaybackConfiguration_personalizationThresholdSeconds,
    putPlaybackConfiguration_slateAdUrl,
    putPlaybackConfiguration_tags,
    putPlaybackConfiguration_transcodeProfileName,
    putPlaybackConfiguration_videoContentSourceUrl,
    putPlaybackConfiguration_name,
    putPlaybackConfigurationResponse_adDecisionServerUrl,
    putPlaybackConfigurationResponse_availSuppression,
    putPlaybackConfigurationResponse_bumper,
    putPlaybackConfigurationResponse_cdnConfiguration,
    putPlaybackConfigurationResponse_configurationAliases,
    putPlaybackConfigurationResponse_dashConfiguration,
    putPlaybackConfigurationResponse_hlsConfiguration,
    putPlaybackConfigurationResponse_livePreRollConfiguration,
    putPlaybackConfigurationResponse_logConfiguration,
    putPlaybackConfigurationResponse_manifestProcessingRules,
    putPlaybackConfigurationResponse_name,
    putPlaybackConfigurationResponse_personalizationThresholdSeconds,
    putPlaybackConfigurationResponse_playbackConfigurationArn,
    putPlaybackConfigurationResponse_playbackEndpointPrefix,
    putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    putPlaybackConfigurationResponse_slateAdUrl,
    putPlaybackConfigurationResponse_tags,
    putPlaybackConfigurationResponse_transcodeProfileName,
    putPlaybackConfigurationResponse_videoContentSourceUrl,
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
    updateChannelResponse_arn,
    updateChannelResponse_channelName,
    updateChannelResponse_channelState,
    updateChannelResponse_creationTime,
    updateChannelResponse_fillerSlate,
    updateChannelResponse_lastModifiedTime,
    updateChannelResponse_outputs,
    updateChannelResponse_playbackMode,
    updateChannelResponse_tags,
    updateChannelResponse_tier,
    updateChannelResponse_httpStatus,

    -- ** UpdateLiveSource
    updateLiveSource_httpPackageConfigurations,
    updateLiveSource_liveSourceName,
    updateLiveSource_sourceLocationName,
    updateLiveSourceResponse_arn,
    updateLiveSourceResponse_creationTime,
    updateLiveSourceResponse_httpPackageConfigurations,
    updateLiveSourceResponse_lastModifiedTime,
    updateLiveSourceResponse_liveSourceName,
    updateLiveSourceResponse_sourceLocationName,
    updateLiveSourceResponse_tags,
    updateLiveSourceResponse_httpStatus,

    -- ** UpdateSourceLocation
    updateSourceLocation_accessConfiguration,
    updateSourceLocation_defaultSegmentDeliveryConfiguration,
    updateSourceLocation_segmentDeliveryConfigurations,
    updateSourceLocation_httpConfiguration,
    updateSourceLocation_sourceLocationName,
    updateSourceLocationResponse_accessConfiguration,
    updateSourceLocationResponse_arn,
    updateSourceLocationResponse_creationTime,
    updateSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    updateSourceLocationResponse_httpConfiguration,
    updateSourceLocationResponse_lastModifiedTime,
    updateSourceLocationResponse_segmentDeliveryConfigurations,
    updateSourceLocationResponse_sourceLocationName,
    updateSourceLocationResponse_tags,
    updateSourceLocationResponse_httpStatus,

    -- ** UpdateVodSource
    updateVodSource_httpPackageConfigurations,
    updateVodSource_sourceLocationName,
    updateVodSource_vodSourceName,
    updateVodSourceResponse_arn,
    updateVodSourceResponse_creationTime,
    updateVodSourceResponse_httpPackageConfigurations,
    updateVodSourceResponse_lastModifiedTime,
    updateVodSourceResponse_sourceLocationName,
    updateVodSourceResponse_tags,
    updateVodSourceResponse_vodSourceName,
    updateVodSourceResponse_httpStatus,

    -- * Types

    -- ** AccessConfiguration
    accessConfiguration_accessType,
    accessConfiguration_secretsManagerAccessTokenConfiguration,

    -- ** AdBreak
    adBreak_messageType,
    adBreak_offsetMillis,
    adBreak_slate,
    adBreak_spliceInsertMessage,
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
    bumper_endUrl,
    bumper_startUrl,

    -- ** CdnConfiguration
    cdnConfiguration_adSegmentUrlPrefix,
    cdnConfiguration_contentSegmentUrlPrefix,

    -- ** Channel
    channel_creationTime,
    channel_fillerSlate,
    channel_lastModifiedTime,
    channel_tags,
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
    dashPlaylistSettings_manifestWindowSeconds,
    dashPlaylistSettings_minBufferTimeSeconds,
    dashPlaylistSettings_minUpdatePeriodSeconds,
    dashPlaylistSettings_suggestedPresentationDelaySeconds,

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
    liveSource_creationTime,
    liveSource_lastModifiedTime,
    liveSource_tags,
    liveSource_arn,
    liveSource_httpPackageConfigurations,
    liveSource_liveSourceName,
    liveSource_sourceLocationName,

    -- ** LogConfiguration
    logConfiguration_percentEnabled,

    -- ** ManifestProcessingRules
    manifestProcessingRules_adMarkerPassthrough,

    -- ** PlaybackConfiguration
    playbackConfiguration_adDecisionServerUrl,
    playbackConfiguration_availSuppression,
    playbackConfiguration_bumper,
    playbackConfiguration_cdnConfiguration,
    playbackConfiguration_configurationAliases,
    playbackConfiguration_dashConfiguration,
    playbackConfiguration_hlsConfiguration,
    playbackConfiguration_livePreRollConfiguration,
    playbackConfiguration_logConfiguration,
    playbackConfiguration_manifestProcessingRules,
    playbackConfiguration_name,
    playbackConfiguration_personalizationThresholdSeconds,
    playbackConfiguration_playbackConfigurationArn,
    playbackConfiguration_playbackEndpointPrefix,
    playbackConfiguration_sessionInitializationEndpointPrefix,
    playbackConfiguration_slateAdUrl,
    playbackConfiguration_tags,
    playbackConfiguration_transcodeProfileName,
    playbackConfiguration_videoContentSourceUrl,

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
    requestOutputItem_dashPlaylistSettings,
    requestOutputItem_hlsPlaylistSettings,
    requestOutputItem_manifestName,
    requestOutputItem_sourceGroup,

    -- ** ResponseOutputItem
    responseOutputItem_dashPlaylistSettings,
    responseOutputItem_hlsPlaylistSettings,
    responseOutputItem_manifestName,
    responseOutputItem_playbackUrl,
    responseOutputItem_sourceGroup,

    -- ** ScheduleAdBreak
    scheduleAdBreak_approximateDurationSeconds,
    scheduleAdBreak_approximateStartTime,
    scheduleAdBreak_sourceLocationName,
    scheduleAdBreak_vodSourceName,

    -- ** ScheduleConfiguration
    scheduleConfiguration_transition,

    -- ** ScheduleEntry
    scheduleEntry_approximateDurationSeconds,
    scheduleEntry_approximateStartTime,
    scheduleEntry_liveSourceName,
    scheduleEntry_scheduleAdBreaks,
    scheduleEntry_scheduleEntryType,
    scheduleEntry_vodSourceName,
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
    segmentationDescriptor_segmentNum,
    segmentationDescriptor_segmentationEventId,
    segmentationDescriptor_segmentationTypeId,
    segmentationDescriptor_segmentationUpid,
    segmentationDescriptor_segmentationUpidType,
    segmentationDescriptor_segmentsExpected,
    segmentationDescriptor_subSegmentNum,
    segmentationDescriptor_subSegmentsExpected,

    -- ** SlateSource
    slateSource_sourceLocationName,
    slateSource_vodSourceName,

    -- ** SourceLocation
    sourceLocation_accessConfiguration,
    sourceLocation_creationTime,
    sourceLocation_defaultSegmentDeliveryConfiguration,
    sourceLocation_lastModifiedTime,
    sourceLocation_segmentDeliveryConfigurations,
    sourceLocation_tags,
    sourceLocation_arn,
    sourceLocation_httpConfiguration,
    sourceLocation_sourceLocationName,

    -- ** SpliceInsertMessage
    spliceInsertMessage_availNum,
    spliceInsertMessage_availsExpected,
    spliceInsertMessage_spliceEventId,
    spliceInsertMessage_uniqueProgramId,

    -- ** TimeSignalMessage
    timeSignalMessage_segmentationDescriptors,

    -- ** Transition
    transition_durationMillis,
    transition_relativeProgram,
    transition_scheduledStartTimeMillis,
    transition_relativePosition,
    transition_type,

    -- ** VodSource
    vodSource_creationTime,
    vodSource_lastModifiedTime,
    vodSource_tags,
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
