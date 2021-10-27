{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaTailor.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaTailor.Lens
  ( -- * Operations

    -- ** CreateSourceLocation
    createSourceLocation_accessConfiguration,
    createSourceLocation_defaultSegmentDeliveryConfiguration,
    createSourceLocation_tags,
    createSourceLocation_sourceLocationName,
    createSourceLocation_httpConfiguration,
    createSourceLocationResponse_creationTime,
    createSourceLocationResponse_sourceLocationName,
    createSourceLocationResponse_arn,
    createSourceLocationResponse_httpConfiguration,
    createSourceLocationResponse_lastModifiedTime,
    createSourceLocationResponse_accessConfiguration,
    createSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    createSourceLocationResponse_tags,
    createSourceLocationResponse_httpStatus,

    -- ** ListPrefetchSchedules
    listPrefetchSchedules_nextToken,
    listPrefetchSchedules_maxResults,
    listPrefetchSchedules_streamId,
    listPrefetchSchedules_playbackConfigurationName,
    listPrefetchSchedulesResponse_items,
    listPrefetchSchedulesResponse_nextToken,
    listPrefetchSchedulesResponse_httpStatus,

    -- ** DeletePrefetchSchedule
    deletePrefetchSchedule_name,
    deletePrefetchSchedule_playbackConfigurationName,
    deletePrefetchScheduleResponse_httpStatus,

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

    -- ** CreatePrefetchSchedule
    createPrefetchSchedule_streamId,
    createPrefetchSchedule_name,
    createPrefetchSchedule_playbackConfigurationName,
    createPrefetchSchedule_consumption,
    createPrefetchSchedule_retrieval,
    createPrefetchScheduleResponse_arn,
    createPrefetchScheduleResponse_playbackConfigurationName,
    createPrefetchScheduleResponse_retrieval,
    createPrefetchScheduleResponse_name,
    createPrefetchScheduleResponse_consumption,
    createPrefetchScheduleResponse_streamId,
    createPrefetchScheduleResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelName,
    deleteChannelResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_channelName,
    updateChannel_outputs,
    updateChannelResponse_creationTime,
    updateChannelResponse_arn,
    updateChannelResponse_lastModifiedTime,
    updateChannelResponse_playbackMode,
    updateChannelResponse_channelName,
    updateChannelResponse_outputs,
    updateChannelResponse_channelState,
    updateChannelResponse_fillerSlate,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,

    -- ** GetChannelPolicy
    getChannelPolicy_channelName,
    getChannelPolicyResponse_policy,
    getChannelPolicyResponse_httpStatus,

    -- ** DescribeVodSource
    describeVodSource_sourceLocationName,
    describeVodSource_vodSourceName,
    describeVodSourceResponse_creationTime,
    describeVodSourceResponse_sourceLocationName,
    describeVodSourceResponse_arn,
    describeVodSourceResponse_lastModifiedTime,
    describeVodSourceResponse_httpPackageConfigurations,
    describeVodSourceResponse_vodSourceName,
    describeVodSourceResponse_tags,
    describeVodSourceResponse_httpStatus,

    -- ** DescribeSourceLocation
    describeSourceLocation_sourceLocationName,
    describeSourceLocationResponse_creationTime,
    describeSourceLocationResponse_sourceLocationName,
    describeSourceLocationResponse_arn,
    describeSourceLocationResponse_httpConfiguration,
    describeSourceLocationResponse_lastModifiedTime,
    describeSourceLocationResponse_accessConfiguration,
    describeSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    describeSourceLocationResponse_tags,
    describeSourceLocationResponse_httpStatus,

    -- ** GetPrefetchSchedule
    getPrefetchSchedule_name,
    getPrefetchSchedule_playbackConfigurationName,
    getPrefetchScheduleResponse_arn,
    getPrefetchScheduleResponse_playbackConfigurationName,
    getPrefetchScheduleResponse_retrieval,
    getPrefetchScheduleResponse_name,
    getPrefetchScheduleResponse_consumption,
    getPrefetchScheduleResponse_streamId,
    getPrefetchScheduleResponse_httpStatus,

    -- ** CreateProgram
    createProgram_adBreaks,
    createProgram_channelName,
    createProgram_programName,
    createProgram_vodSourceName,
    createProgram_scheduleConfiguration,
    createProgram_sourceLocationName,
    createProgramResponse_creationTime,
    createProgramResponse_sourceLocationName,
    createProgramResponse_arn,
    createProgramResponse_programName,
    createProgramResponse_adBreaks,
    createProgramResponse_channelName,
    createProgramResponse_scheduledStartTime,
    createProgramResponse_vodSourceName,
    createProgramResponse_httpStatus,

    -- ** StartChannel
    startChannel_channelName,
    startChannelResponse_httpStatus,

    -- ** ListPlaybackConfigurations
    listPlaybackConfigurations_nextToken,
    listPlaybackConfigurations_maxResults,
    listPlaybackConfigurationsResponse_items,
    listPlaybackConfigurationsResponse_nextToken,
    listPlaybackConfigurationsResponse_httpStatus,

    -- ** DeletePlaybackConfiguration
    deletePlaybackConfiguration_name,
    deletePlaybackConfigurationResponse_httpStatus,

    -- ** PutPlaybackConfiguration
    putPlaybackConfiguration_personalizationThresholdSeconds,
    putPlaybackConfiguration_availSuppression,
    putPlaybackConfiguration_bumper,
    putPlaybackConfiguration_adDecisionServerUrl,
    putPlaybackConfiguration_videoContentSourceUrl,
    putPlaybackConfiguration_dashConfiguration,
    putPlaybackConfiguration_manifestProcessingRules,
    putPlaybackConfiguration_livePreRollConfiguration,
    putPlaybackConfiguration_name,
    putPlaybackConfiguration_configurationAliases,
    putPlaybackConfiguration_transcodeProfileName,
    putPlaybackConfiguration_tags,
    putPlaybackConfiguration_slateAdUrl,
    putPlaybackConfiguration_cdnConfiguration,
    putPlaybackConfigurationResponse_playbackEndpointPrefix,
    putPlaybackConfigurationResponse_hlsConfiguration,
    putPlaybackConfigurationResponse_personalizationThresholdSeconds,
    putPlaybackConfigurationResponse_playbackConfigurationArn,
    putPlaybackConfigurationResponse_availSuppression,
    putPlaybackConfigurationResponse_bumper,
    putPlaybackConfigurationResponse_adDecisionServerUrl,
    putPlaybackConfigurationResponse_videoContentSourceUrl,
    putPlaybackConfigurationResponse_dashConfiguration,
    putPlaybackConfigurationResponse_manifestProcessingRules,
    putPlaybackConfigurationResponse_logConfiguration,
    putPlaybackConfigurationResponse_livePreRollConfiguration,
    putPlaybackConfigurationResponse_name,
    putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    putPlaybackConfigurationResponse_configurationAliases,
    putPlaybackConfigurationResponse_transcodeProfileName,
    putPlaybackConfigurationResponse_tags,
    putPlaybackConfigurationResponse_slateAdUrl,
    putPlaybackConfigurationResponse_cdnConfiguration,
    putPlaybackConfigurationResponse_httpStatus,

    -- ** ListSourceLocations
    listSourceLocations_nextToken,
    listSourceLocations_maxResults,
    listSourceLocationsResponse_items,
    listSourceLocationsResponse_nextToken,
    listSourceLocationsResponse_httpStatus,

    -- ** UpdateSourceLocation
    updateSourceLocation_accessConfiguration,
    updateSourceLocation_defaultSegmentDeliveryConfiguration,
    updateSourceLocation_sourceLocationName,
    updateSourceLocation_httpConfiguration,
    updateSourceLocationResponse_creationTime,
    updateSourceLocationResponse_sourceLocationName,
    updateSourceLocationResponse_arn,
    updateSourceLocationResponse_httpConfiguration,
    updateSourceLocationResponse_lastModifiedTime,
    updateSourceLocationResponse_accessConfiguration,
    updateSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    updateSourceLocationResponse_tags,
    updateSourceLocationResponse_httpStatus,

    -- ** DeleteSourceLocation
    deleteSourceLocation_sourceLocationName,
    deleteSourceLocationResponse_httpStatus,

    -- ** GetPlaybackConfiguration
    getPlaybackConfiguration_name,
    getPlaybackConfigurationResponse_playbackEndpointPrefix,
    getPlaybackConfigurationResponse_hlsConfiguration,
    getPlaybackConfigurationResponse_personalizationThresholdSeconds,
    getPlaybackConfigurationResponse_playbackConfigurationArn,
    getPlaybackConfigurationResponse_availSuppression,
    getPlaybackConfigurationResponse_bumper,
    getPlaybackConfigurationResponse_adDecisionServerUrl,
    getPlaybackConfigurationResponse_videoContentSourceUrl,
    getPlaybackConfigurationResponse_dashConfiguration,
    getPlaybackConfigurationResponse_manifestProcessingRules,
    getPlaybackConfigurationResponse_logConfiguration,
    getPlaybackConfigurationResponse_livePreRollConfiguration,
    getPlaybackConfigurationResponse_name,
    getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    getPlaybackConfigurationResponse_configurationAliases,
    getPlaybackConfigurationResponse_transcodeProfileName,
    getPlaybackConfigurationResponse_tags,
    getPlaybackConfigurationResponse_slateAdUrl,
    getPlaybackConfigurationResponse_cdnConfiguration,
    getPlaybackConfigurationResponse_httpStatus,

    -- ** DeleteVodSource
    deleteVodSource_sourceLocationName,
    deleteVodSource_vodSourceName,
    deleteVodSourceResponse_httpStatus,

    -- ** UpdateVodSource
    updateVodSource_sourceLocationName,
    updateVodSource_vodSourceName,
    updateVodSource_httpPackageConfigurations,
    updateVodSourceResponse_creationTime,
    updateVodSourceResponse_sourceLocationName,
    updateVodSourceResponse_arn,
    updateVodSourceResponse_lastModifiedTime,
    updateVodSourceResponse_httpPackageConfigurations,
    updateVodSourceResponse_vodSourceName,
    updateVodSourceResponse_tags,
    updateVodSourceResponse_httpStatus,

    -- ** CreateVodSource
    createVodSource_tags,
    createVodSource_sourceLocationName,
    createVodSource_vodSourceName,
    createVodSource_httpPackageConfigurations,
    createVodSourceResponse_creationTime,
    createVodSourceResponse_sourceLocationName,
    createVodSourceResponse_arn,
    createVodSourceResponse_lastModifiedTime,
    createVodSourceResponse_httpPackageConfigurations,
    createVodSourceResponse_vodSourceName,
    createVodSourceResponse_tags,
    createVodSourceResponse_httpStatus,

    -- ** CreateChannel
    createChannel_fillerSlate,
    createChannel_tags,
    createChannel_channelName,
    createChannel_outputs,
    createChannel_playbackMode,
    createChannelResponse_creationTime,
    createChannelResponse_arn,
    createChannelResponse_lastModifiedTime,
    createChannelResponse_playbackMode,
    createChannelResponse_channelName,
    createChannelResponse_outputs,
    createChannelResponse_channelState,
    createChannelResponse_fillerSlate,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,

    -- ** DeleteChannelPolicy
    deleteChannelPolicy_channelName,
    deleteChannelPolicyResponse_httpStatus,

    -- ** PutChannelPolicy
    putChannelPolicy_channelName,
    putChannelPolicy_policy,
    putChannelPolicyResponse_httpStatus,

    -- ** DeleteProgram
    deleteProgram_channelName,
    deleteProgram_programName,
    deleteProgramResponse_httpStatus,

    -- ** GetChannelSchedule
    getChannelSchedule_nextToken,
    getChannelSchedule_durationMinutes,
    getChannelSchedule_maxResults,
    getChannelSchedule_channelName,
    getChannelScheduleResponse_items,
    getChannelScheduleResponse_nextToken,
    getChannelScheduleResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** ConfigureLogsForPlaybackConfiguration
    configureLogsForPlaybackConfiguration_percentEnabled,
    configureLogsForPlaybackConfiguration_playbackConfigurationName,
    configureLogsForPlaybackConfigurationResponse_playbackConfigurationName,
    configureLogsForPlaybackConfigurationResponse_percentEnabled,
    configureLogsForPlaybackConfigurationResponse_httpStatus,

    -- ** StopChannel
    stopChannel_channelName,
    stopChannelResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DescribeChannel
    describeChannel_channelName,
    describeChannelResponse_creationTime,
    describeChannelResponse_arn,
    describeChannelResponse_lastModifiedTime,
    describeChannelResponse_playbackMode,
    describeChannelResponse_channelName,
    describeChannelResponse_outputs,
    describeChannelResponse_channelState,
    describeChannelResponse_fillerSlate,
    describeChannelResponse_tags,
    describeChannelResponse_httpStatus,

    -- ** ListVodSources
    listVodSources_nextToken,
    listVodSources_maxResults,
    listVodSources_sourceLocationName,
    listVodSourcesResponse_items,
    listVodSourcesResponse_nextToken,
    listVodSourcesResponse_httpStatus,

    -- ** DescribeProgram
    describeProgram_channelName,
    describeProgram_programName,
    describeProgramResponse_creationTime,
    describeProgramResponse_sourceLocationName,
    describeProgramResponse_arn,
    describeProgramResponse_programName,
    describeProgramResponse_adBreaks,
    describeProgramResponse_channelName,
    describeProgramResponse_scheduledStartTime,
    describeProgramResponse_vodSourceName,
    describeProgramResponse_httpStatus,

    -- * Types

    -- ** AccessConfiguration
    accessConfiguration_accessType,
    accessConfiguration_secretsManagerAccessTokenConfiguration,

    -- ** AdBreak
    adBreak_spliceInsertMessage,
    adBreak_messageType,
    adBreak_slate,
    adBreak_offsetMillis,

    -- ** AdMarkerPassthrough
    adMarkerPassthrough_enabled,

    -- ** Alert
    alert_resourceArn,
    alert_alertCode,
    alert_lastModifiedTime,
    alert_relatedResourceArns,
    alert_alertMessage,

    -- ** AvailMatchingCriteria
    availMatchingCriteria_operator,
    availMatchingCriteria_dynamicVariable,

    -- ** AvailSuppression
    availSuppression_value,
    availSuppression_mode,

    -- ** Bumper
    bumper_endUrl,
    bumper_startUrl,

    -- ** CdnConfiguration
    cdnConfiguration_adSegmentUrlPrefix,
    cdnConfiguration_contentSegmentUrlPrefix,

    -- ** Channel
    channel_creationTime,
    channel_lastModifiedTime,
    channel_fillerSlate,
    channel_tags,
    channel_channelState,
    channel_channelName,
    channel_outputs,
    channel_arn,
    channel_playbackMode,

    -- ** DashConfiguration
    dashConfiguration_manifestEndpointPrefix,
    dashConfiguration_originManifestType,
    dashConfiguration_mpdLocation,

    -- ** DashConfigurationForPut
    dashConfigurationForPut_originManifestType,
    dashConfigurationForPut_mpdLocation,

    -- ** DashPlaylistSettings
    dashPlaylistSettings_minBufferTimeSeconds,
    dashPlaylistSettings_minUpdatePeriodSeconds,
    dashPlaylistSettings_suggestedPresentationDelaySeconds,
    dashPlaylistSettings_manifestWindowSeconds,

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
    httpPackageConfiguration_type,
    httpPackageConfiguration_sourceGroup,

    -- ** LivePreRollConfiguration
    livePreRollConfiguration_adDecisionServerUrl,
    livePreRollConfiguration_maxDurationSeconds,

    -- ** LogConfiguration
    logConfiguration_percentEnabled,

    -- ** ManifestProcessingRules
    manifestProcessingRules_adMarkerPassthrough,

    -- ** PlaybackConfiguration
    playbackConfiguration_playbackEndpointPrefix,
    playbackConfiguration_hlsConfiguration,
    playbackConfiguration_personalizationThresholdSeconds,
    playbackConfiguration_playbackConfigurationArn,
    playbackConfiguration_availSuppression,
    playbackConfiguration_bumper,
    playbackConfiguration_adDecisionServerUrl,
    playbackConfiguration_videoContentSourceUrl,
    playbackConfiguration_dashConfiguration,
    playbackConfiguration_manifestProcessingRules,
    playbackConfiguration_logConfiguration,
    playbackConfiguration_livePreRollConfiguration,
    playbackConfiguration_name,
    playbackConfiguration_sessionInitializationEndpointPrefix,
    playbackConfiguration_configurationAliases,
    playbackConfiguration_transcodeProfileName,
    playbackConfiguration_tags,
    playbackConfiguration_slateAdUrl,
    playbackConfiguration_cdnConfiguration,

    -- ** PrefetchConsumption
    prefetchConsumption_startTime,
    prefetchConsumption_availMatchingCriteria,
    prefetchConsumption_endTime,

    -- ** PrefetchRetrieval
    prefetchRetrieval_startTime,
    prefetchRetrieval_dynamicVariables,
    prefetchRetrieval_endTime,

    -- ** PrefetchSchedule
    prefetchSchedule_streamId,
    prefetchSchedule_retrieval,
    prefetchSchedule_consumption,
    prefetchSchedule_arn,
    prefetchSchedule_playbackConfigurationName,
    prefetchSchedule_name,

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
    scheduleAdBreak_sourceLocationName,
    scheduleAdBreak_approximateDurationSeconds,
    scheduleAdBreak_vodSourceName,
    scheduleAdBreak_approximateStartTime,

    -- ** ScheduleConfiguration
    scheduleConfiguration_transition,

    -- ** ScheduleEntry
    scheduleEntry_scheduleAdBreaks,
    scheduleEntry_approximateDurationSeconds,
    scheduleEntry_approximateStartTime,
    scheduleEntry_scheduleEntryType,
    scheduleEntry_vodSourceName,
    scheduleEntry_channelName,
    scheduleEntry_sourceLocationName,
    scheduleEntry_arn,
    scheduleEntry_programName,

    -- ** SecretsManagerAccessTokenConfiguration
    secretsManagerAccessTokenConfiguration_headerName,
    secretsManagerAccessTokenConfiguration_secretStringKey,
    secretsManagerAccessTokenConfiguration_secretArn,

    -- ** SlateSource
    slateSource_sourceLocationName,
    slateSource_vodSourceName,

    -- ** SourceLocation
    sourceLocation_creationTime,
    sourceLocation_lastModifiedTime,
    sourceLocation_accessConfiguration,
    sourceLocation_defaultSegmentDeliveryConfiguration,
    sourceLocation_tags,
    sourceLocation_sourceLocationName,
    sourceLocation_httpConfiguration,
    sourceLocation_arn,

    -- ** SpliceInsertMessage
    spliceInsertMessage_availNum,
    spliceInsertMessage_uniqueProgramId,
    spliceInsertMessage_availsExpected,
    spliceInsertMessage_spliceEventId,

    -- ** Transition
    transition_scheduledStartTimeMillis,
    transition_relativeProgram,
    transition_type,
    transition_relativePosition,

    -- ** VodSource
    vodSource_creationTime,
    vodSource_lastModifiedTime,
    vodSource_tags,
    vodSource_vodSourceName,
    vodSource_sourceLocationName,
    vodSource_httpPackageConfigurations,
    vodSource_arn,
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
import Network.AWS.MediaTailor.Types.AccessConfiguration
import Network.AWS.MediaTailor.Types.AdBreak
import Network.AWS.MediaTailor.Types.AdMarkerPassthrough
import Network.AWS.MediaTailor.Types.Alert
import Network.AWS.MediaTailor.Types.AvailMatchingCriteria
import Network.AWS.MediaTailor.Types.AvailSuppression
import Network.AWS.MediaTailor.Types.Bumper
import Network.AWS.MediaTailor.Types.CdnConfiguration
import Network.AWS.MediaTailor.Types.Channel
import Network.AWS.MediaTailor.Types.DashConfiguration
import Network.AWS.MediaTailor.Types.DashConfigurationForPut
import Network.AWS.MediaTailor.Types.DashPlaylistSettings
import Network.AWS.MediaTailor.Types.DefaultSegmentDeliveryConfiguration
import Network.AWS.MediaTailor.Types.HlsConfiguration
import Network.AWS.MediaTailor.Types.HlsPlaylistSettings
import Network.AWS.MediaTailor.Types.HttpConfiguration
import Network.AWS.MediaTailor.Types.HttpPackageConfiguration
import Network.AWS.MediaTailor.Types.LivePreRollConfiguration
import Network.AWS.MediaTailor.Types.LogConfiguration
import Network.AWS.MediaTailor.Types.ManifestProcessingRules
import Network.AWS.MediaTailor.Types.PlaybackConfiguration
import Network.AWS.MediaTailor.Types.PrefetchConsumption
import Network.AWS.MediaTailor.Types.PrefetchRetrieval
import Network.AWS.MediaTailor.Types.PrefetchSchedule
import Network.AWS.MediaTailor.Types.RequestOutputItem
import Network.AWS.MediaTailor.Types.ResponseOutputItem
import Network.AWS.MediaTailor.Types.ScheduleAdBreak
import Network.AWS.MediaTailor.Types.ScheduleConfiguration
import Network.AWS.MediaTailor.Types.ScheduleEntry
import Network.AWS.MediaTailor.Types.SecretsManagerAccessTokenConfiguration
import Network.AWS.MediaTailor.Types.SlateSource
import Network.AWS.MediaTailor.Types.SourceLocation
import Network.AWS.MediaTailor.Types.SpliceInsertMessage
import Network.AWS.MediaTailor.Types.Transition
import Network.AWS.MediaTailor.Types.VodSource
import Network.AWS.MediaTailor.UntagResource
import Network.AWS.MediaTailor.UpdateChannel
import Network.AWS.MediaTailor.UpdateSourceLocation
import Network.AWS.MediaTailor.UpdateVodSource
