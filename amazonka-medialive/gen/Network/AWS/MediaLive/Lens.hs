{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Lens
  ( -- * Operations

    -- ** DescribeInputDeviceThumbnail
    describeInputDeviceThumbnail_inputDeviceId,
    describeInputDeviceThumbnail_accept,
    describeInputDeviceThumbnailResponse_eTag,
    describeInputDeviceThumbnailResponse_contentType,
    describeInputDeviceThumbnailResponse_contentLength,
    describeInputDeviceThumbnailResponse_lastModified,
    describeInputDeviceThumbnailResponse_httpStatus,
    describeInputDeviceThumbnailResponse_body,

    -- ** UpdateInputDevice
    updateInputDevice'_uhdDeviceSettings,
    updateInputDevice'_hdDeviceSettings,
    updateInputDevice'_name,
    updateInputDevice'_inputDeviceId,
    updateInputDeviceResponse_uhdDeviceSettings,
    updateInputDeviceResponse_hdDeviceSettings,
    updateInputDeviceResponse_macAddress,
    updateInputDeviceResponse_connectionState,
    updateInputDeviceResponse_networkSettings,
    updateInputDeviceResponse_arn,
    updateInputDeviceResponse_id,
    updateInputDeviceResponse_deviceUpdateStatus,
    updateInputDeviceResponse_name,
    updateInputDeviceResponse_serialNumber,
    updateInputDeviceResponse_type,
    updateInputDeviceResponse_deviceSettingsSyncState,
    updateInputDeviceResponse_httpStatus,

    -- ** ListInputs
    listInputs_nextToken,
    listInputs_maxResults,
    listInputsResponse_nextToken,
    listInputsResponse_inputs,
    listInputsResponse_httpStatus,

    -- ** CreateChannel
    createChannel'_encoderSettings,
    createChannel'_roleArn,
    createChannel'_inputSpecification,
    createChannel'_channelClass,
    createChannel'_logLevel,
    createChannel'_destinations,
    createChannel'_name,
    createChannel'_reserved,
    createChannel'_requestId,
    createChannel'_inputAttachments,
    createChannel'_tags,
    createChannel'_vpc,
    createChannel'_cdiInputSpecification,
    createChannelResponse_channel,
    createChannelResponse_httpStatus,

    -- ** DeleteInput
    deleteInput_inputId,
    deleteInputResponse_httpStatus,

    -- ** ListInputDevices
    listInputDevices_nextToken,
    listInputDevices_maxResults,
    listInputDevicesResponse_nextToken,
    listInputDevicesResponse_inputDevices,
    listInputDevicesResponse_httpStatus,

    -- ** UpdateInput
    updateInput'_inputSecurityGroups,
    updateInput'_roleArn,
    updateInput'_sources,
    updateInput'_mediaConnectFlows,
    updateInput'_destinations,
    updateInput'_name,
    updateInput'_inputDevices,
    updateInput'_inputId,
    updateInputResponse_input,
    updateInputResponse_httpStatus,

    -- ** ListInputDeviceTransfers
    listInputDeviceTransfers_nextToken,
    listInputDeviceTransfers_maxResults,
    listInputDeviceTransfers_transferType,
    listInputDeviceTransfersResponse_nextToken,
    listInputDeviceTransfersResponse_inputDeviceTransfers,
    listInputDeviceTransfersResponse_httpStatus,

    -- ** BatchStop
    batchStop'_multiplexIds,
    batchStop'_channelIds,
    batchStopResponse_successful,
    batchStopResponse_failed,
    batchStopResponse_httpStatus,

    -- ** UpdateChannelClass
    updateChannelClass'_destinations,
    updateChannelClass'_channelId,
    updateChannelClass'_channelClass,
    updateChannelClassResponse_channel,
    updateChannelClassResponse_httpStatus,

    -- ** BatchStart
    batchStart'_multiplexIds,
    batchStart'_channelIds,
    batchStartResponse_successful,
    batchStartResponse_failed,
    batchStartResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_maximumFramerate,
    listOfferings_nextToken,
    listOfferings_videoQuality,
    listOfferings_duration,
    listOfferings_maxResults,
    listOfferings_codec,
    listOfferings_channelConfiguration,
    listOfferings_maximumBitrate,
    listOfferings_specialFeature,
    listOfferings_channelClass,
    listOfferings_resourceType,
    listOfferings_resolution,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** UpdateMultiplex
    updateMultiplex'_name,
    updateMultiplex'_multiplexSettings,
    updateMultiplex'_multiplexId,
    updateMultiplexResponse_multiplex,
    updateMultiplexResponse_httpStatus,

    -- ** DeleteMultiplex
    deleteMultiplex_multiplexId,
    deleteMultiplexResponse_availabilityZones,
    deleteMultiplexResponse_arn,
    deleteMultiplexResponse_id,
    deleteMultiplexResponse_pipelinesRunningCount,
    deleteMultiplexResponse_programCount,
    deleteMultiplexResponse_destinations,
    deleteMultiplexResponse_state,
    deleteMultiplexResponse_name,
    deleteMultiplexResponse_tags,
    deleteMultiplexResponse_multiplexSettings,
    deleteMultiplexResponse_httpStatus,

    -- ** DeleteInputSecurityGroup
    deleteInputSecurityGroup_inputSecurityGroupId,
    deleteInputSecurityGroupResponse_httpStatus,

    -- ** UpdateInputSecurityGroup
    updateInputSecurityGroup_tags,
    updateInputSecurityGroup_whitelistRules,
    updateInputSecurityGroup_inputSecurityGroupId,
    updateInputSecurityGroupResponse_securityGroup,
    updateInputSecurityGroupResponse_httpStatus,

    -- ** ListInputSecurityGroups
    listInputSecurityGroups_nextToken,
    listInputSecurityGroups_maxResults,
    listInputSecurityGroupsResponse_nextToken,
    listInputSecurityGroupsResponse_inputSecurityGroups,
    listInputSecurityGroupsResponse_httpStatus,

    -- ** DescribeInput
    describeInput_inputId,
    describeInputResponse_roleArn,
    describeInputResponse_sources,
    describeInputResponse_inputPartnerIds,
    describeInputResponse_inputSourceType,
    describeInputResponse_mediaConnectFlows,
    describeInputResponse_arn,
    describeInputResponse_id,
    describeInputResponse_securityGroups,
    describeInputResponse_destinations,
    describeInputResponse_state,
    describeInputResponse_name,
    describeInputResponse_tags,
    describeInputResponse_type,
    describeInputResponse_inputClass,
    describeInputResponse_inputDevices,
    describeInputResponse_attachedChannels,
    describeInputResponse_httpStatus,

    -- ** CreateInputSecurityGroup
    createInputSecurityGroup_tags,
    createInputSecurityGroup_whitelistRules,
    createInputSecurityGroupResponse_securityGroup,
    createInputSecurityGroupResponse_httpStatus,

    -- ** StartChannel
    startChannel_channelId,
    startChannelResponse_encoderSettings,
    startChannelResponse_roleArn,
    startChannelResponse_inputSpecification,
    startChannelResponse_arn,
    startChannelResponse_id,
    startChannelResponse_pipelinesRunningCount,
    startChannelResponse_channelClass,
    startChannelResponse_logLevel,
    startChannelResponse_destinations,
    startChannelResponse_state,
    startChannelResponse_name,
    startChannelResponse_inputAttachments,
    startChannelResponse_tags,
    startChannelResponse_pipelineDetails,
    startChannelResponse_egressEndpoints,
    startChannelResponse_vpc,
    startChannelResponse_cdiInputSpecification,
    startChannelResponse_httpStatus,

    -- ** DescribeInputDevice
    describeInputDevice_inputDeviceId,
    describeInputDeviceResponse_uhdDeviceSettings,
    describeInputDeviceResponse_hdDeviceSettings,
    describeInputDeviceResponse_macAddress,
    describeInputDeviceResponse_connectionState,
    describeInputDeviceResponse_networkSettings,
    describeInputDeviceResponse_arn,
    describeInputDeviceResponse_id,
    describeInputDeviceResponse_deviceUpdateStatus,
    describeInputDeviceResponse_name,
    describeInputDeviceResponse_serialNumber,
    describeInputDeviceResponse_type,
    describeInputDeviceResponse_deviceSettingsSyncState,
    describeInputDeviceResponse_httpStatus,

    -- ** StopChannel
    stopChannel_channelId,
    stopChannelResponse_encoderSettings,
    stopChannelResponse_roleArn,
    stopChannelResponse_inputSpecification,
    stopChannelResponse_arn,
    stopChannelResponse_id,
    stopChannelResponse_pipelinesRunningCount,
    stopChannelResponse_channelClass,
    stopChannelResponse_logLevel,
    stopChannelResponse_destinations,
    stopChannelResponse_state,
    stopChannelResponse_name,
    stopChannelResponse_inputAttachments,
    stopChannelResponse_tags,
    stopChannelResponse_pipelineDetails,
    stopChannelResponse_egressEndpoints,
    stopChannelResponse_vpc,
    stopChannelResponse_cdiInputSpecification,
    stopChannelResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** BatchUpdateSchedule
    batchUpdateSchedule_deletes,
    batchUpdateSchedule_creates,
    batchUpdateSchedule_channelId,
    batchUpdateScheduleResponse_deletes,
    batchUpdateScheduleResponse_creates,
    batchUpdateScheduleResponse_httpStatus,

    -- ** DescribeOffering
    describeOffering_offeringId,
    describeOfferingResponse_duration,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_arn,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_region,
    describeOfferingResponse_httpStatus,

    -- ** AcceptInputDeviceTransfer
    acceptInputDeviceTransfer_inputDeviceId,
    acceptInputDeviceTransferResponse_httpStatus,

    -- ** DeleteMultiplexProgram
    deleteMultiplexProgram_multiplexId,
    deleteMultiplexProgram_programName,
    deleteMultiplexProgramResponse_packetIdentifiersMap,
    deleteMultiplexProgramResponse_multiplexProgramSettings,
    deleteMultiplexProgramResponse_channelId,
    deleteMultiplexProgramResponse_programName,
    deleteMultiplexProgramResponse_pipelineDetails,
    deleteMultiplexProgramResponse_httpStatus,

    -- ** UpdateMultiplexProgram
    updateMultiplexProgram'_multiplexProgramSettings,
    updateMultiplexProgram'_multiplexId,
    updateMultiplexProgram'_programName,
    updateMultiplexProgramResponse_multiplexProgram,
    updateMultiplexProgramResponse_httpStatus,

    -- ** DescribeReservation
    describeReservation_reservationId,
    describeReservationResponse_end,
    describeReservationResponse_duration,
    describeReservationResponse_durationUnits,
    describeReservationResponse_arn,
    describeReservationResponse_offeringId,
    describeReservationResponse_currencyCode,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_state,
    describeReservationResponse_name,
    describeReservationResponse_tags,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_count,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_usagePrice,
    describeReservationResponse_offeringType,
    describeReservationResponse_region,
    describeReservationResponse_start,
    describeReservationResponse_reservationId,
    describeReservationResponse_httpStatus,

    -- ** DescribeInputSecurityGroup
    describeInputSecurityGroup_inputSecurityGroupId,
    describeInputSecurityGroupResponse_arn,
    describeInputSecurityGroupResponse_id,
    describeInputSecurityGroupResponse_state,
    describeInputSecurityGroupResponse_tags,
    describeInputSecurityGroupResponse_whitelistRules,
    describeInputSecurityGroupResponse_inputs,
    describeInputSecurityGroupResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_httpStatus,

    -- ** RejectInputDeviceTransfer
    rejectInputDeviceTransfer_inputDeviceId,
    rejectInputDeviceTransferResponse_httpStatus,

    -- ** CreateMultiplex
    createMultiplex'_tags,
    createMultiplex'_requestId,
    createMultiplex'_multiplexSettings,
    createMultiplex'_availabilityZones,
    createMultiplex'_name,
    createMultiplexResponse_multiplex,
    createMultiplexResponse_httpStatus,

    -- ** CreatePartnerInput
    createPartnerInput'_requestId,
    createPartnerInput'_tags,
    createPartnerInput'_inputId,
    createPartnerInputResponse_input,
    createPartnerInputResponse_httpStatus,

    -- ** DeleteSchedule
    deleteSchedule_channelId,
    deleteScheduleResponse_httpStatus,

    -- ** ListMultiplexes
    listMultiplexes_nextToken,
    listMultiplexes_maxResults,
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_httpStatus,

    -- ** UpdateReservation
    updateReservation'_name,
    updateReservation'_reservationId,
    updateReservationResponse_reservation,
    updateReservationResponse_httpStatus,

    -- ** DeleteReservation
    deleteReservation_reservationId,
    deleteReservationResponse_end,
    deleteReservationResponse_duration,
    deleteReservationResponse_durationUnits,
    deleteReservationResponse_arn,
    deleteReservationResponse_offeringId,
    deleteReservationResponse_currencyCode,
    deleteReservationResponse_resourceSpecification,
    deleteReservationResponse_state,
    deleteReservationResponse_name,
    deleteReservationResponse_tags,
    deleteReservationResponse_offeringDescription,
    deleteReservationResponse_count,
    deleteReservationResponse_fixedPrice,
    deleteReservationResponse_usagePrice,
    deleteReservationResponse_offeringType,
    deleteReservationResponse_region,
    deleteReservationResponse_start,
    deleteReservationResponse_reservationId,
    deleteReservationResponse_httpStatus,

    -- ** DescribeMultiplexProgram
    describeMultiplexProgram_multiplexId,
    describeMultiplexProgram_programName,
    describeMultiplexProgramResponse_packetIdentifiersMap,
    describeMultiplexProgramResponse_multiplexProgramSettings,
    describeMultiplexProgramResponse_channelId,
    describeMultiplexProgramResponse_programName,
    describeMultiplexProgramResponse_pipelineDetails,
    describeMultiplexProgramResponse_httpStatus,

    -- ** ListReservations
    listReservations_maximumFramerate,
    listReservations_nextToken,
    listReservations_videoQuality,
    listReservations_maxResults,
    listReservations_codec,
    listReservations_maximumBitrate,
    listReservations_specialFeature,
    listReservations_channelClass,
    listReservations_resourceType,
    listReservations_resolution,
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_channelId,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_roleArn,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_channelClass,
    describeChannelResponse_logLevel,
    describeChannelResponse_destinations,
    describeChannelResponse_state,
    describeChannelResponse_name,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_tags,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_vpc,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_httpStatus,

    -- ** CancelInputDeviceTransfer
    cancelInputDeviceTransfer_inputDeviceId,
    cancelInputDeviceTransferResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering'_name,
    purchaseOffering'_requestId,
    purchaseOffering'_tags,
    purchaseOffering'_start,
    purchaseOffering'_offeringId,
    purchaseOffering'_count,
    purchaseOfferingResponse_reservation,
    purchaseOfferingResponse_httpStatus,

    -- ** StartMultiplex
    startMultiplex_multiplexId,
    startMultiplexResponse_availabilityZones,
    startMultiplexResponse_arn,
    startMultiplexResponse_id,
    startMultiplexResponse_pipelinesRunningCount,
    startMultiplexResponse_programCount,
    startMultiplexResponse_destinations,
    startMultiplexResponse_state,
    startMultiplexResponse_name,
    startMultiplexResponse_tags,
    startMultiplexResponse_multiplexSettings,
    startMultiplexResponse_httpStatus,

    -- ** StopMultiplex
    stopMultiplex_multiplexId,
    stopMultiplexResponse_availabilityZones,
    stopMultiplexResponse_arn,
    stopMultiplexResponse_id,
    stopMultiplexResponse_pipelinesRunningCount,
    stopMultiplexResponse_programCount,
    stopMultiplexResponse_destinations,
    stopMultiplexResponse_state,
    stopMultiplexResponse_name,
    stopMultiplexResponse_tags,
    stopMultiplexResponse_multiplexSettings,
    stopMultiplexResponse_httpStatus,

    -- ** DescribeSchedule
    describeSchedule_nextToken,
    describeSchedule_maxResults,
    describeSchedule_channelId,
    describeScheduleResponse_nextToken,
    describeScheduleResponse_scheduleActions,
    describeScheduleResponse_httpStatus,

    -- ** CreateMultiplexProgram
    createMultiplexProgram'_multiplexId,
    createMultiplexProgram'_requestId,
    createMultiplexProgram'_multiplexProgramSettings,
    createMultiplexProgram'_programName,
    createMultiplexProgramResponse_multiplexProgram,
    createMultiplexProgramResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** TransferInputDevice
    transferInputDevice'_transferMessage,
    transferInputDevice'_targetCustomerId,
    transferInputDevice'_inputDeviceId,
    transferInputDeviceResponse_httpStatus,

    -- ** ListMultiplexPrograms
    listMultiplexPrograms_nextToken,
    listMultiplexPrograms_maxResults,
    listMultiplexPrograms_multiplexId,
    listMultiplexProgramsResponse_multiplexPrograms,
    listMultiplexProgramsResponse_nextToken,
    listMultiplexProgramsResponse_httpStatus,

    -- ** DescribeMultiplex
    describeMultiplex_multiplexId,
    describeMultiplexResponse_availabilityZones,
    describeMultiplexResponse_arn,
    describeMultiplexResponse_id,
    describeMultiplexResponse_pipelinesRunningCount,
    describeMultiplexResponse_programCount,
    describeMultiplexResponse_destinations,
    describeMultiplexResponse_state,
    describeMultiplexResponse_name,
    describeMultiplexResponse_tags,
    describeMultiplexResponse_multiplexSettings,
    describeMultiplexResponse_httpStatus,

    -- ** BatchDelete
    batchDelete'_inputSecurityGroupIds,
    batchDelete'_multiplexIds,
    batchDelete'_inputIds,
    batchDelete'_channelIds,
    batchDeleteResponse_successful,
    batchDeleteResponse_failed,
    batchDeleteResponse_httpStatus,

    -- ** CreateInput
    createInput'_inputSecurityGroups,
    createInput'_roleArn,
    createInput'_sources,
    createInput'_mediaConnectFlows,
    createInput'_destinations,
    createInput'_name,
    createInput'_requestId,
    createInput'_tags,
    createInput'_type,
    createInput'_vpc,
    createInput'_inputDevices,
    createInputResponse_input,
    createInputResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelId,
    deleteChannelResponse_encoderSettings,
    deleteChannelResponse_roleArn,
    deleteChannelResponse_inputSpecification,
    deleteChannelResponse_arn,
    deleteChannelResponse_id,
    deleteChannelResponse_pipelinesRunningCount,
    deleteChannelResponse_channelClass,
    deleteChannelResponse_logLevel,
    deleteChannelResponse_destinations,
    deleteChannelResponse_state,
    deleteChannelResponse_name,
    deleteChannelResponse_inputAttachments,
    deleteChannelResponse_tags,
    deleteChannelResponse_pipelineDetails,
    deleteChannelResponse_egressEndpoints,
    deleteChannelResponse_vpc,
    deleteChannelResponse_cdiInputSpecification,
    deleteChannelResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel'_encoderSettings,
    updateChannel'_roleArn,
    updateChannel'_inputSpecification,
    updateChannel'_logLevel,
    updateChannel'_destinations,
    updateChannel'_name,
    updateChannel'_inputAttachments,
    updateChannel'_cdiInputSpecification,
    updateChannel'_channelId,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_rateControlMode,
    aacSettings_codingMode,
    aacSettings_spec,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_inputType,
    aacSettings_profile,
    aacSettings_vbrQuality,
    aacSettings_bitrate,

    -- ** Ac3Settings
    ac3Settings_dialnorm,
    ac3Settings_drcProfile,
    ac3Settings_codingMode,
    ac3Settings_lfeFilter,
    ac3Settings_bitstreamMode,
    ac3Settings_bitrate,
    ac3Settings_metadataControl,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** ArchiveContainerSettings
    archiveContainerSettings_rawSettings,
    archiveContainerSettings_m2tsSettings,

    -- ** ArchiveGroupSettings
    archiveGroupSettings_rolloverInterval,
    archiveGroupSettings_destination,

    -- ** ArchiveOutputSettings
    archiveOutputSettings_extension,
    archiveOutputSettings_nameModifier,
    archiveOutputSettings_containerSettings,

    -- ** AribDestinationSettings

    -- ** AribSourceSettings

    -- ** AudioChannelMapping
    audioChannelMapping_outputChannel,
    audioChannelMapping_inputChannelLevels,

    -- ** AudioCodecSettings
    audioCodecSettings_ac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_passThroughSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_wavSettings,

    -- ** AudioDescription
    audioDescription_languageCode,
    audioDescription_audioType,
    audioDescription_codecSettings,
    audioDescription_languageCodeControl,
    audioDescription_audioTypeControl,
    audioDescription_remixSettings,
    audioDescription_audioNormalizationSettings,
    audioDescription_streamName,
    audioDescription_audioSelectorName,
    audioDescription_name,

    -- ** AudioLanguageSelection
    audioLanguageSelection_languageSelectionPolicy,
    audioLanguageSelection_languageCode,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithmControl,

    -- ** AudioOnlyHlsSettings
    audioOnlyHlsSettings_audioGroupId,
    audioOnlyHlsSettings_audioTrackType,
    audioOnlyHlsSettings_segmentType,
    audioOnlyHlsSettings_audioOnlyImage,

    -- ** AudioPidSelection
    audioPidSelection_pid,

    -- ** AudioSelector
    audioSelector_selectorSettings,
    audioSelector_name,

    -- ** AudioSelectorSettings
    audioSelectorSettings_audioLanguageSelection,
    audioSelectorSettings_audioPidSelection,
    audioSelectorSettings_audioTrackSelection,

    -- ** AudioSilenceFailoverSettings
    audioSilenceFailoverSettings_audioSilenceThresholdMsec,
    audioSilenceFailoverSettings_audioSelectorName,

    -- ** AudioTrack
    audioTrack_track,

    -- ** AudioTrackSelection
    audioTrackSelection_tracks,

    -- ** AutomaticInputFailoverSettings
    automaticInputFailoverSettings_failoverConditions,
    automaticInputFailoverSettings_errorClearTimeMsec,
    automaticInputFailoverSettings_inputPreference,
    automaticInputFailoverSettings_secondaryInputId,

    -- ** AvailBlanking
    availBlanking_state,
    availBlanking_availBlankingImage,

    -- ** AvailConfiguration
    availConfiguration_availSettings,

    -- ** AvailSettings
    availSettings_scte35TimeSignalApos,
    availSettings_scte35SpliceInsert,

    -- ** BatchFailedResultModel
    batchFailedResultModel_message,
    batchFailedResultModel_arn,
    batchFailedResultModel_id,
    batchFailedResultModel_code,

    -- ** BatchScheduleActionCreateRequest
    batchScheduleActionCreateRequest_scheduleActions,

    -- ** BatchScheduleActionCreateResult
    batchScheduleActionCreateResult_scheduleActions,

    -- ** BatchScheduleActionDeleteRequest
    batchScheduleActionDeleteRequest_actionNames,

    -- ** BatchScheduleActionDeleteResult
    batchScheduleActionDeleteResult_scheduleActions,

    -- ** BatchSuccessfulResultModel
    batchSuccessfulResultModel_arn,
    batchSuccessfulResultModel_id,
    batchSuccessfulResultModel_state,

    -- ** BlackoutSlate
    blackoutSlate_blackoutSlateImage,
    blackoutSlate_networkEndBlackout,
    blackoutSlate_state,
    blackoutSlate_networkEndBlackoutImage,
    blackoutSlate_networkId,

    -- ** BurnInDestinationSettings
    burnInDestinationSettings_alignment,
    burnInDestinationSettings_shadowOpacity,
    burnInDestinationSettings_shadowColor,
    burnInDestinationSettings_outlineColor,
    burnInDestinationSettings_teletextGridControl,
    burnInDestinationSettings_backgroundOpacity,
    burnInDestinationSettings_xPosition,
    burnInDestinationSettings_fontColor,
    burnInDestinationSettings_fontSize,
    burnInDestinationSettings_backgroundColor,
    burnInDestinationSettings_shadowXOffset,
    burnInDestinationSettings_font,
    burnInDestinationSettings_yPosition,
    burnInDestinationSettings_fontResolution,
    burnInDestinationSettings_outlineSize,
    burnInDestinationSettings_fontOpacity,
    burnInDestinationSettings_shadowYOffset,

    -- ** CaptionDescription
    captionDescription_languageCode,
    captionDescription_languageDescription,
    captionDescription_destinationSettings,
    captionDescription_captionSelectorName,
    captionDescription_name,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_webvttDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_aribDestinationSettings,
    captionDestinationSettings_scte20PlusEmbeddedDestinationSettings,
    captionDestinationSettings_embeddedPlusScte20DestinationSettings,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_scte27DestinationSettings,
    captionDestinationSettings_rtmpCaptionInfoDestinationSettings,
    captionDestinationSettings_ebuTtDDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_smpteTtDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_burnInDestinationSettings,

    -- ** CaptionLanguageMapping
    captionLanguageMapping_languageCode,
    captionLanguageMapping_languageDescription,
    captionLanguageMapping_captionChannel,

    -- ** CaptionSelector
    captionSelector_languageCode,
    captionSelector_selectorSettings,
    captionSelector_name,

    -- ** CaptionSelectorSettings
    captionSelectorSettings_ancillarySourceSettings,
    captionSelectorSettings_embeddedSourceSettings,
    captionSelectorSettings_aribSourceSettings,
    captionSelectorSettings_scte27SourceSettings,
    captionSelectorSettings_dvbSubSourceSettings,
    captionSelectorSettings_scte20SourceSettings,
    captionSelectorSettings_teletextSourceSettings,

    -- ** CdiInputSpecification
    cdiInputSpecification_resolution,

    -- ** Channel
    channel_encoderSettings,
    channel_roleArn,
    channel_inputSpecification,
    channel_arn,
    channel_id,
    channel_pipelinesRunningCount,
    channel_channelClass,
    channel_logLevel,
    channel_destinations,
    channel_state,
    channel_name,
    channel_inputAttachments,
    channel_tags,
    channel_pipelineDetails,
    channel_egressEndpoints,
    channel_vpc,
    channel_cdiInputSpecification,

    -- ** ChannelEgressEndpoint
    channelEgressEndpoint_sourceIp,

    -- ** ChannelSummary
    channelSummary_roleArn,
    channelSummary_inputSpecification,
    channelSummary_arn,
    channelSummary_id,
    channelSummary_pipelinesRunningCount,
    channelSummary_channelClass,
    channelSummary_logLevel,
    channelSummary_destinations,
    channelSummary_state,
    channelSummary_name,
    channelSummary_inputAttachments,
    channelSummary_tags,
    channelSummary_egressEndpoints,
    channelSummary_vpc,
    channelSummary_cdiInputSpecification,

    -- ** ColorSpacePassthroughSettings

    -- ** DvbNitSettings
    dvbNitSettings_repInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- ** DvbSdtSettings
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_repInterval,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_teletextGridControl,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_font,
    dvbSubDestinationSettings_yPosition,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_shadowYOffset,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_repInterval,

    -- ** Eac3Settings
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_dialnorm,
    eac3Settings_codingMode,
    eac3Settings_lfeControl,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_lfeFilter,
    eac3Settings_dcFilter,
    eac3Settings_stereoDownmix,
    eac3Settings_drcRf,
    eac3Settings_drcLine,
    eac3Settings_bitstreamMode,
    eac3Settings_surroundExMode,
    eac3Settings_phaseControl,
    eac3Settings_passthroughControl,
    eac3Settings_bitrate,
    eac3Settings_attenuationControl,
    eac3Settings_surroundMode,
    eac3Settings_metadataControl,

    -- ** EbuTtDDestinationSettings
    ebuTtDDestinationSettings_fillLineGap,
    ebuTtDDestinationSettings_styleControl,
    ebuTtDDestinationSettings_fontFamily,

    -- ** EmbeddedDestinationSettings

    -- ** EmbeddedPlusScte20DestinationSettings

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_scte20Detection,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- ** EncoderSettings
    encoderSettings_globalConfiguration,
    encoderSettings_featureActivations,
    encoderSettings_availConfiguration,
    encoderSettings_availBlanking,
    encoderSettings_nielsenConfiguration,
    encoderSettings_blackoutSlate,
    encoderSettings_captionDescriptions,
    encoderSettings_videoDescriptions,
    encoderSettings_audioDescriptions,
    encoderSettings_outputGroups,
    encoderSettings_timecodeConfig,

    -- ** FailoverCondition
    failoverCondition_failoverConditionSettings,

    -- ** FailoverConditionSettings
    failoverConditionSettings_videoBlackSettings,
    failoverConditionSettings_inputLossSettings,
    failoverConditionSettings_audioSilenceSettings,

    -- ** FeatureActivations
    featureActivations_inputPrepareScheduleActions,

    -- ** FecOutputSettings
    fecOutputSettings_rowLength,
    fecOutputSettings_columnDepth,
    fecOutputSettings_includeFec,

    -- ** FixedModeScheduleActionStartSettings
    fixedModeScheduleActionStartSettings_time,

    -- ** Fmp4HlsSettings
    fmp4HlsSettings_audioRenditionSets,
    fmp4HlsSettings_nielsenId3Behavior,
    fmp4HlsSettings_timedMetadataBehavior,

    -- ** FollowModeScheduleActionStartSettings
    followModeScheduleActionStartSettings_referenceActionName,
    followModeScheduleActionStartSettings_followPoint,

    -- ** FrameCaptureGroupSettings
    frameCaptureGroupSettings_destination,

    -- ** FrameCaptureHlsSettings

    -- ** FrameCaptureOutputSettings
    frameCaptureOutputSettings_nameModifier,

    -- ** FrameCaptureSettings
    frameCaptureSettings_captureInterval,
    frameCaptureSettings_captureIntervalUnits,

    -- ** GlobalConfiguration
    globalConfiguration_initialAudioGain,
    globalConfiguration_outputLockingMode,
    globalConfiguration_inputEndAction,
    globalConfiguration_inputLossBehavior,
    globalConfiguration_supportLowFramerateInputs,
    globalConfiguration_outputTimingSource,

    -- ** H264ColorSpaceSettings
    h264ColorSpaceSettings_rec601Settings,
    h264ColorSpaceSettings_rec709Settings,
    h264ColorSpaceSettings_colorSpacePassthroughSettings,

    -- ** H264FilterSettings
    h264FilterSettings_temporalFilterSettings,

    -- ** H264Settings
    h264Settings_scanType,
    h264Settings_temporalAq,
    h264Settings_bufSize,
    h264Settings_flickerAq,
    h264Settings_gopBReference,
    h264Settings_framerateNumerator,
    h264Settings_rateControlMode,
    h264Settings_slices,
    h264Settings_qualityLevel,
    h264Settings_colorMetadata,
    h264Settings_subgopLength,
    h264Settings_entropyEncoding,
    h264Settings_gopSizeUnits,
    h264Settings_gopSize,
    h264Settings_framerateDenominator,
    h264Settings_fixedAfd,
    h264Settings_softness,
    h264Settings_filterSettings,
    h264Settings_parNumerator,
    h264Settings_spatialAq,
    h264Settings_gopNumBFrames,
    h264Settings_sceneChangeDetect,
    h264Settings_timecodeInsertion,
    h264Settings_colorSpaceSettings,
    h264Settings_minIInterval,
    h264Settings_qvbrQualityLevel,
    h264Settings_parControl,
    h264Settings_bufFillPct,
    h264Settings_gopClosedCadence,
    h264Settings_parDenominator,
    h264Settings_maxBitrate,
    h264Settings_syntax,
    h264Settings_numRefFrames,
    h264Settings_level,
    h264Settings_profile,
    h264Settings_adaptiveQuantization,
    h264Settings_lookAheadRateControl,
    h264Settings_framerateControl,
    h264Settings_forceFieldPictures,
    h264Settings_bitrate,
    h264Settings_afdSignaling,

    -- ** H265ColorSpaceSettings
    h265ColorSpaceSettings_rec601Settings,
    h265ColorSpaceSettings_rec709Settings,
    h265ColorSpaceSettings_colorSpacePassthroughSettings,
    h265ColorSpaceSettings_hdr10Settings,

    -- ** H265FilterSettings
    h265FilterSettings_temporalFilterSettings,

    -- ** H265Settings
    h265Settings_scanType,
    h265Settings_bufSize,
    h265Settings_flickerAq,
    h265Settings_alternativeTransferFunction,
    h265Settings_rateControlMode,
    h265Settings_slices,
    h265Settings_colorMetadata,
    h265Settings_gopSizeUnits,
    h265Settings_gopSize,
    h265Settings_fixedAfd,
    h265Settings_filterSettings,
    h265Settings_parNumerator,
    h265Settings_sceneChangeDetect,
    h265Settings_timecodeInsertion,
    h265Settings_colorSpaceSettings,
    h265Settings_minIInterval,
    h265Settings_qvbrQualityLevel,
    h265Settings_gopClosedCadence,
    h265Settings_parDenominator,
    h265Settings_maxBitrate,
    h265Settings_level,
    h265Settings_profile,
    h265Settings_adaptiveQuantization,
    h265Settings_lookAheadRateControl,
    h265Settings_bitrate,
    h265Settings_afdSignaling,
    h265Settings_tier,
    h265Settings_framerateNumerator,
    h265Settings_framerateDenominator,

    -- ** Hdr10Settings
    hdr10Settings_maxCll,
    hdr10Settings_maxFall,

    -- ** HlsAkamaiSettings
    hlsAkamaiSettings_filecacheDuration,
    hlsAkamaiSettings_numRetries,
    hlsAkamaiSettings_httpTransferMode,
    hlsAkamaiSettings_connectionRetryInterval,
    hlsAkamaiSettings_token,
    hlsAkamaiSettings_restartDelay,
    hlsAkamaiSettings_salt,

    -- ** HlsBasicPutSettings
    hlsBasicPutSettings_filecacheDuration,
    hlsBasicPutSettings_numRetries,
    hlsBasicPutSettings_connectionRetryInterval,
    hlsBasicPutSettings_restartDelay,

    -- ** HlsCdnSettings
    hlsCdnSettings_hlsBasicPutSettings,
    hlsCdnSettings_hlsWebdavSettings,
    hlsCdnSettings_hlsAkamaiSettings,
    hlsCdnSettings_hlsMediaStoreSettings,

    -- ** HlsGroupSettings
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_ivInManifest,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_encryptionType,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_keyFormat,
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_constantIv,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_mode,
    hlsGroupSettings_hlsCdnSettings,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_redundantManifest,
    hlsGroupSettings_indexNSegments,
    hlsGroupSettings_iFrameOnlyPlaylists,
    hlsGroupSettings_segmentationMode,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_tsFileMode,
    hlsGroupSettings_discontinuityTags,
    hlsGroupSettings_baseUrlContent,
    hlsGroupSettings_hlsId3SegmentTagging,
    hlsGroupSettings_incompleteSegmentBehavior,
    hlsGroupSettings_baseUrlManifest,
    hlsGroupSettings_baseUrlContent1,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_inputLossAction,
    hlsGroupSettings_keyProviderSettings,
    hlsGroupSettings_keepSegments,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_baseUrlManifest1,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_keyFormatVersions,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_ivSource,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_destination,

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    hlsId3SegmentTaggingScheduleActionSettings_tag,

    -- ** HlsInputSettings
    hlsInputSettings_retryInterval,
    hlsInputSettings_bandwidth,
    hlsInputSettings_retries,
    hlsInputSettings_bufferSegments,

    -- ** HlsMediaStoreSettings
    hlsMediaStoreSettings_filecacheDuration,
    hlsMediaStoreSettings_numRetries,
    hlsMediaStoreSettings_mediaStoreStorageClass,
    hlsMediaStoreSettings_connectionRetryInterval,
    hlsMediaStoreSettings_restartDelay,

    -- ** HlsOutputSettings
    hlsOutputSettings_segmentModifier,
    hlsOutputSettings_h265PackagingType,
    hlsOutputSettings_nameModifier,
    hlsOutputSettings_hlsSettings,

    -- ** HlsSettings
    hlsSettings_standardHlsSettings,
    hlsSettings_frameCaptureHlsSettings,
    hlsSettings_audioOnlyHlsSettings,
    hlsSettings_fmp4HlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    hlsTimedMetadataScheduleActionSettings_id3,

    -- ** HlsWebdavSettings
    hlsWebdavSettings_filecacheDuration,
    hlsWebdavSettings_numRetries,
    hlsWebdavSettings_httpTransferMode,
    hlsWebdavSettings_connectionRetryInterval,
    hlsWebdavSettings_restartDelay,

    -- ** ImmediateModeScheduleActionStartSettings

    -- ** Input
    input_roleArn,
    input_sources,
    input_inputPartnerIds,
    input_inputSourceType,
    input_mediaConnectFlows,
    input_arn,
    input_id,
    input_securityGroups,
    input_destinations,
    input_state,
    input_name,
    input_tags,
    input_type,
    input_inputClass,
    input_inputDevices,
    input_attachedChannels,

    -- ** InputAttachment
    inputAttachment_inputSettings,
    inputAttachment_inputId,
    inputAttachment_inputAttachmentName,
    inputAttachment_automaticInputFailoverSettings,

    -- ** InputChannelLevel
    inputChannelLevel_inputChannel,
    inputChannelLevel_gain,

    -- ** InputClippingSettings
    inputClippingSettings_startTimecode,
    inputClippingSettings_stopTimecode,
    inputClippingSettings_inputTimecodeSource,

    -- ** InputDestination
    inputDestination_ip,
    inputDestination_port,
    inputDestination_url,
    inputDestination_vpc,

    -- ** InputDestinationRequest
    inputDestinationRequest_streamName,

    -- ** InputDestinationVpc
    inputDestinationVpc_availabilityZone,
    inputDestinationVpc_networkInterfaceId,

    -- ** InputDeviceConfigurableSettings
    inputDeviceConfigurableSettings_configuredInput,
    inputDeviceConfigurableSettings_maxBitrate,

    -- ** InputDeviceHdSettings
    inputDeviceHdSettings_height,
    inputDeviceHdSettings_scanType,
    inputDeviceHdSettings_width,
    inputDeviceHdSettings_configuredInput,
    inputDeviceHdSettings_framerate,
    inputDeviceHdSettings_deviceState,
    inputDeviceHdSettings_maxBitrate,
    inputDeviceHdSettings_activeInput,

    -- ** InputDeviceNetworkSettings
    inputDeviceNetworkSettings_dnsAddresses,
    inputDeviceNetworkSettings_ipAddress,
    inputDeviceNetworkSettings_subnetMask,
    inputDeviceNetworkSettings_ipScheme,
    inputDeviceNetworkSettings_gateway,

    -- ** InputDeviceRequest
    inputDeviceRequest_id,

    -- ** InputDeviceSettings
    inputDeviceSettings_id,

    -- ** InputDeviceSummary
    inputDeviceSummary_uhdDeviceSettings,
    inputDeviceSummary_hdDeviceSettings,
    inputDeviceSummary_macAddress,
    inputDeviceSummary_connectionState,
    inputDeviceSummary_networkSettings,
    inputDeviceSummary_arn,
    inputDeviceSummary_id,
    inputDeviceSummary_deviceUpdateStatus,
    inputDeviceSummary_name,
    inputDeviceSummary_serialNumber,
    inputDeviceSummary_type,
    inputDeviceSummary_deviceSettingsSyncState,

    -- ** InputDeviceUhdSettings
    inputDeviceUhdSettings_height,
    inputDeviceUhdSettings_scanType,
    inputDeviceUhdSettings_width,
    inputDeviceUhdSettings_configuredInput,
    inputDeviceUhdSettings_framerate,
    inputDeviceUhdSettings_deviceState,
    inputDeviceUhdSettings_maxBitrate,
    inputDeviceUhdSettings_activeInput,

    -- ** InputLocation
    inputLocation_passwordParam,
    inputLocation_username,
    inputLocation_uri,

    -- ** InputLossBehavior
    inputLossBehavior_blackFrameMsec,
    inputLossBehavior_inputLossImageColor,
    inputLossBehavior_inputLossImageSlate,
    inputLossBehavior_repeatFrameMsec,
    inputLossBehavior_inputLossImageType,

    -- ** InputLossFailoverSettings
    inputLossFailoverSettings_inputLossThresholdMsec,

    -- ** InputPrepareScheduleActionSettings
    inputPrepareScheduleActionSettings_inputAttachmentNameReference,
    inputPrepareScheduleActionSettings_urlPath,
    inputPrepareScheduleActionSettings_inputClippingSettings,

    -- ** InputSecurityGroup
    inputSecurityGroup_arn,
    inputSecurityGroup_id,
    inputSecurityGroup_state,
    inputSecurityGroup_tags,
    inputSecurityGroup_whitelistRules,
    inputSecurityGroup_inputs,

    -- ** InputSettings
    inputSettings_denoiseFilter,
    inputSettings_audioSelectors,
    inputSettings_filterStrength,
    inputSettings_smpte2038DataPreference,
    inputSettings_videoSelector,
    inputSettings_sourceEndBehavior,
    inputSettings_inputFilter,
    inputSettings_deblockFilter,
    inputSettings_captionSelectors,
    inputSettings_networkInputSettings,

    -- ** InputSource
    inputSource_passwordParam,
    inputSource_username,
    inputSource_url,

    -- ** InputSourceRequest
    inputSourceRequest_passwordParam,
    inputSourceRequest_username,
    inputSourceRequest_url,

    -- ** InputSpecification
    inputSpecification_codec,
    inputSpecification_maximumBitrate,
    inputSpecification_resolution,

    -- ** InputSwitchScheduleActionSettings
    inputSwitchScheduleActionSettings_urlPath,
    inputSwitchScheduleActionSettings_inputClippingSettings,
    inputSwitchScheduleActionSettings_inputAttachmentNameReference,

    -- ** InputVpcRequest
    inputVpcRequest_securityGroupIds,
    inputVpcRequest_subnetIds,

    -- ** InputWhitelistRule
    inputWhitelistRule_cidr,

    -- ** InputWhitelistRuleCidr
    inputWhitelistRuleCidr_cidr,

    -- ** KeyProviderSettings
    keyProviderSettings_staticKeySettings,

    -- ** M2tsSettings
    m2tsSettings_segmentationMarkers,
    m2tsSettings_klvDataPids,
    m2tsSettings_etvSignalPid,
    m2tsSettings_pcrPeriod,
    m2tsSettings_pmtPid,
    m2tsSettings_videoPid,
    m2tsSettings_nielsenId3Behavior,
    m2tsSettings_audioBufferModel,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_segmentationStyle,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_pcrControl,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_ebpPlacement,
    m2tsSettings_pmtInterval,
    m2tsSettings_ccDescriptor,
    m2tsSettings_scte35Control,
    m2tsSettings_aribCaptionsPid,
    m2tsSettings_audioPids,
    m2tsSettings_etvPlatformPid,
    m2tsSettings_patInterval,
    m2tsSettings_programNum,
    m2tsSettings_audioStreamType,
    m2tsSettings_ebpLookaheadMs,
    m2tsSettings_bufferModel,
    m2tsSettings_aribCaptionsPidControl,
    m2tsSettings_pcrPid,
    m2tsSettings_klv,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_rateMode,
    m2tsSettings_dvbTdtSettings,
    m2tsSettings_ecmPid,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_absentInputAudioBehavior,
    m2tsSettings_segmentationTime,
    m2tsSettings_ebif,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_timedMetadataBehavior,
    m2tsSettings_arib,
    m2tsSettings_bitrate,
    m2tsSettings_fragmentTime,
    m2tsSettings_esRateInPes,
    m2tsSettings_scte27Pids,
    m2tsSettings_transportStreamId,
    m2tsSettings_dvbSubPids,
    m2tsSettings_scte35Pid,

    -- ** M3u8Settings
    m3u8Settings_pcrPeriod,
    m3u8Settings_pmtPid,
    m3u8Settings_videoPid,
    m3u8Settings_nielsenId3Behavior,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_pcrControl,
    m3u8Settings_pmtInterval,
    m3u8Settings_audioPids,
    m3u8Settings_patInterval,
    m3u8Settings_programNum,
    m3u8Settings_pcrPid,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_ecmPid,
    m3u8Settings_scte35Behavior,
    m3u8Settings_timedMetadataBehavior,
    m3u8Settings_transportStreamId,
    m3u8Settings_scte35Pid,

    -- ** MediaConnectFlow
    mediaConnectFlow_flowArn,

    -- ** MediaConnectFlowRequest
    mediaConnectFlowRequest_flowArn,

    -- ** MediaPackageGroupSettings
    mediaPackageGroupSettings_destination,

    -- ** MediaPackageOutputDestinationSettings
    mediaPackageOutputDestinationSettings_channelId,

    -- ** MediaPackageOutputSettings

    -- ** Mp2Settings
    mp2Settings_codingMode,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- ** Mpeg2FilterSettings
    mpeg2FilterSettings_temporalFilterSettings,

    -- ** Mpeg2Settings
    mpeg2Settings_scanType,
    mpeg2Settings_displayAspectRatio,
    mpeg2Settings_colorMetadata,
    mpeg2Settings_subgopLength,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_gopSize,
    mpeg2Settings_fixedAfd,
    mpeg2Settings_filterSettings,
    mpeg2Settings_gopNumBFrames,
    mpeg2Settings_timecodeInsertion,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_colorSpace,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_afdSignaling,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_framerateDenominator,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_streamManifestBehavior,
    msSmoothGroupSettings_filecacheDuration,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_eventId,
    msSmoothGroupSettings_certificateMode,
    msSmoothGroupSettings_numRetries,
    msSmoothGroupSettings_acquisitionPointId,
    msSmoothGroupSettings_audioOnlyTimecodeControl,
    msSmoothGroupSettings_segmentationMode,
    msSmoothGroupSettings_eventIdMode,
    msSmoothGroupSettings_sendDelayMs,
    msSmoothGroupSettings_connectionRetryInterval,
    msSmoothGroupSettings_sparseTrackType,
    msSmoothGroupSettings_inputLossAction,
    msSmoothGroupSettings_timestampOffset,
    msSmoothGroupSettings_eventStopBehavior,
    msSmoothGroupSettings_timestampOffsetMode,
    msSmoothGroupSettings_restartDelay,
    msSmoothGroupSettings_destination,

    -- ** MsSmoothOutputSettings
    msSmoothOutputSettings_h265PackagingType,
    msSmoothOutputSettings_nameModifier,

    -- ** Multiplex
    multiplex_availabilityZones,
    multiplex_arn,
    multiplex_id,
    multiplex_pipelinesRunningCount,
    multiplex_programCount,
    multiplex_destinations,
    multiplex_state,
    multiplex_name,
    multiplex_tags,
    multiplex_multiplexSettings,

    -- ** MultiplexGroupSettings

    -- ** MultiplexMediaConnectOutputDestinationSettings
    multiplexMediaConnectOutputDestinationSettings_entitlementArn,

    -- ** MultiplexOutputDestination
    multiplexOutputDestination_mediaConnectSettings,

    -- ** MultiplexOutputSettings
    multiplexOutputSettings_destination,

    -- ** MultiplexProgram
    multiplexProgram_packetIdentifiersMap,
    multiplexProgram_multiplexProgramSettings,
    multiplexProgram_channelId,
    multiplexProgram_programName,
    multiplexProgram_pipelineDetails,

    -- ** MultiplexProgramChannelDestinationSettings
    multiplexProgramChannelDestinationSettings_multiplexId,
    multiplexProgramChannelDestinationSettings_programName,

    -- ** MultiplexProgramPacketIdentifiersMap
    multiplexProgramPacketIdentifiersMap_klvDataPids,
    multiplexProgramPacketIdentifiersMap_etvSignalPid,
    multiplexProgramPacketIdentifiersMap_pmtPid,
    multiplexProgramPacketIdentifiersMap_videoPid,
    multiplexProgramPacketIdentifiersMap_timedMetadataPid,
    multiplexProgramPacketIdentifiersMap_audioPids,
    multiplexProgramPacketIdentifiersMap_etvPlatformPid,
    multiplexProgramPacketIdentifiersMap_pcrPid,
    multiplexProgramPacketIdentifiersMap_dvbTeletextPid,
    multiplexProgramPacketIdentifiersMap_privateMetadataPid,
    multiplexProgramPacketIdentifiersMap_scte27Pids,
    multiplexProgramPacketIdentifiersMap_dvbSubPids,
    multiplexProgramPacketIdentifiersMap_scte35Pid,

    -- ** MultiplexProgramPipelineDetail
    multiplexProgramPipelineDetail_pipelineId,
    multiplexProgramPipelineDetail_activeChannelPipeline,

    -- ** MultiplexProgramServiceDescriptor
    multiplexProgramServiceDescriptor_providerName,
    multiplexProgramServiceDescriptor_serviceName,

    -- ** MultiplexProgramSettings
    multiplexProgramSettings_preferredChannelPipeline,
    multiplexProgramSettings_serviceDescriptor,
    multiplexProgramSettings_videoSettings,
    multiplexProgramSettings_programNumber,

    -- ** MultiplexProgramSummary
    multiplexProgramSummary_channelId,
    multiplexProgramSummary_programName,

    -- ** MultiplexSettings
    multiplexSettings_transportStreamReservedBitrate,
    multiplexSettings_maximumVideoBufferDelayMilliseconds,
    multiplexSettings_transportStreamBitrate,
    multiplexSettings_transportStreamId,

    -- ** MultiplexSettingsSummary
    multiplexSettingsSummary_transportStreamBitrate,

    -- ** MultiplexStatmuxVideoSettings
    multiplexStatmuxVideoSettings_minimumBitrate,
    multiplexStatmuxVideoSettings_maximumBitrate,
    multiplexStatmuxVideoSettings_priority,

    -- ** MultiplexSummary
    multiplexSummary_availabilityZones,
    multiplexSummary_arn,
    multiplexSummary_id,
    multiplexSummary_pipelinesRunningCount,
    multiplexSummary_programCount,
    multiplexSummary_state,
    multiplexSummary_name,
    multiplexSummary_tags,
    multiplexSummary_multiplexSettings,

    -- ** MultiplexVideoSettings
    multiplexVideoSettings_constantBitrate,
    multiplexVideoSettings_statmuxSettings,

    -- ** NetworkInputSettings
    networkInputSettings_hlsInputSettings,
    networkInputSettings_serverValidation,

    -- ** NielsenConfiguration
    nielsenConfiguration_nielsenPcmToId3Tagging,
    nielsenConfiguration_distributorId,

    -- ** Offering
    offering_duration,
    offering_durationUnits,
    offering_arn,
    offering_offeringId,
    offering_currencyCode,
    offering_resourceSpecification,
    offering_offeringDescription,
    offering_fixedPrice,
    offering_usagePrice,
    offering_offeringType,
    offering_region,

    -- ** Output
    output_audioDescriptionNames,
    output_outputName,
    output_videoDescriptionName,
    output_captionDescriptionNames,
    output_outputSettings,

    -- ** OutputDestination
    outputDestination_mediaPackageSettings,
    outputDestination_id,
    outputDestination_multiplexSettings,
    outputDestination_settings,

    -- ** OutputDestinationSettings
    outputDestinationSettings_passwordParam,
    outputDestinationSettings_username,
    outputDestinationSettings_streamName,
    outputDestinationSettings_url,

    -- ** OutputGroup
    outputGroup_name,
    outputGroup_outputs,
    outputGroup_outputGroupSettings,

    -- ** OutputGroupSettings
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_frameCaptureGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_mediaPackageGroupSettings,
    outputGroupSettings_rtmpGroupSettings,
    outputGroupSettings_udpGroupSettings,
    outputGroupSettings_archiveGroupSettings,
    outputGroupSettings_multiplexGroupSettings,

    -- ** OutputLocationRef
    outputLocationRef_destinationRefId,

    -- ** OutputSettings
    outputSettings_rtmpOutputSettings,
    outputSettings_msSmoothOutputSettings,
    outputSettings_udpOutputSettings,
    outputSettings_mediaPackageOutputSettings,
    outputSettings_frameCaptureOutputSettings,
    outputSettings_archiveOutputSettings,
    outputSettings_hlsOutputSettings,
    outputSettings_multiplexOutputSettings,

    -- ** PassThroughSettings

    -- ** PauseStateScheduleActionSettings
    pauseStateScheduleActionSettings_pipelines,

    -- ** PipelineDetail
    pipelineDetail_pipelineId,
    pipelineDetail_activeInputAttachmentName,
    pipelineDetail_activeInputSwitchActionName,

    -- ** PipelinePauseStateSettings
    pipelinePauseStateSettings_pipelineId,

    -- ** RawSettings

    -- ** Rec601Settings

    -- ** Rec709Settings

    -- ** RemixSettings
    remixSettings_channelsIn,
    remixSettings_channelsOut,
    remixSettings_channelMappings,

    -- ** Reservation
    reservation_end,
    reservation_duration,
    reservation_durationUnits,
    reservation_arn,
    reservation_offeringId,
    reservation_currencyCode,
    reservation_resourceSpecification,
    reservation_state,
    reservation_name,
    reservation_tags,
    reservation_offeringDescription,
    reservation_count,
    reservation_fixedPrice,
    reservation_usagePrice,
    reservation_offeringType,
    reservation_region,
    reservation_start,
    reservation_reservationId,

    -- ** ReservationResourceSpecification
    reservationResourceSpecification_maximumFramerate,
    reservationResourceSpecification_videoQuality,
    reservationResourceSpecification_codec,
    reservationResourceSpecification_maximumBitrate,
    reservationResourceSpecification_specialFeature,
    reservationResourceSpecification_channelClass,
    reservationResourceSpecification_resourceType,
    reservationResourceSpecification_resolution,

    -- ** RtmpCaptionInfoDestinationSettings

    -- ** RtmpGroupSettings
    rtmpGroupSettings_adMarkers,
    rtmpGroupSettings_captionData,
    rtmpGroupSettings_cacheFullBehavior,
    rtmpGroupSettings_cacheLength,
    rtmpGroupSettings_authenticationScheme,
    rtmpGroupSettings_inputLossAction,
    rtmpGroupSettings_restartDelay,

    -- ** RtmpOutputSettings
    rtmpOutputSettings_certificateMode,
    rtmpOutputSettings_numRetries,
    rtmpOutputSettings_connectionRetryInterval,
    rtmpOutputSettings_destination,

    -- ** ScheduleAction
    scheduleAction_actionName,
    scheduleAction_scheduleActionStartSettings,
    scheduleAction_scheduleActionSettings,

    -- ** ScheduleActionSettings
    scheduleActionSettings_inputSwitchSettings,
    scheduleActionSettings_scte35TimeSignalSettings,
    scheduleActionSettings_hlsTimedMetadataSettings,
    scheduleActionSettings_staticImageActivateSettings,
    scheduleActionSettings_pauseStateSettings,
    scheduleActionSettings_scte35SpliceInsertSettings,
    scheduleActionSettings_scte35ReturnToNetworkSettings,
    scheduleActionSettings_hlsId3SegmentTaggingSettings,
    scheduleActionSettings_staticImageDeactivateSettings,
    scheduleActionSettings_inputPrepareSettings,

    -- ** ScheduleActionStartSettings
    scheduleActionStartSettings_followModeScheduleActionStartSettings,
    scheduleActionStartSettings_immediateModeScheduleActionStartSettings,
    scheduleActionStartSettings_fixedModeScheduleActionStartSettings,

    -- ** Scte20PlusEmbeddedDestinationSettings

    -- ** Scte20SourceSettings
    scte20SourceSettings_convert608To708,
    scte20SourceSettings_source608ChannelNumber,

    -- ** Scte27DestinationSettings

    -- ** Scte27SourceSettings
    scte27SourceSettings_pid,

    -- ** Scte35DeliveryRestrictions
    scte35DeliveryRestrictions_deviceRestrictions,
    scte35DeliveryRestrictions_archiveAllowedFlag,
    scte35DeliveryRestrictions_webDeliveryAllowedFlag,
    scte35DeliveryRestrictions_noRegionalBlackoutFlag,

    -- ** Scte35Descriptor
    scte35Descriptor_scte35DescriptorSettings,

    -- ** Scte35DescriptorSettings
    scte35DescriptorSettings_segmentationDescriptorScte35DescriptorSettings,

    -- ** Scte35ReturnToNetworkScheduleActionSettings
    scte35ReturnToNetworkScheduleActionSettings_spliceEventId,

    -- ** Scte35SegmentationDescriptor
    scte35SegmentationDescriptor_subSegmentNum,
    scte35SegmentationDescriptor_segmentationUpid,
    scte35SegmentationDescriptor_segmentNum,
    scte35SegmentationDescriptor_subSegmentsExpected,
    scte35SegmentationDescriptor_segmentationUpidType,
    scte35SegmentationDescriptor_segmentsExpected,
    scte35SegmentationDescriptor_segmentationTypeId,
    scte35SegmentationDescriptor_deliveryRestrictions,
    scte35SegmentationDescriptor_segmentationDuration,
    scte35SegmentationDescriptor_segmentationEventId,
    scte35SegmentationDescriptor_segmentationCancelIndicator,

    -- ** Scte35SpliceInsert
    scte35SpliceInsert_adAvailOffset,
    scte35SpliceInsert_noRegionalBlackoutFlag,
    scte35SpliceInsert_webDeliveryAllowedFlag,

    -- ** Scte35SpliceInsertScheduleActionSettings
    scte35SpliceInsertScheduleActionSettings_duration,
    scte35SpliceInsertScheduleActionSettings_spliceEventId,

    -- ** Scte35TimeSignalApos
    scte35TimeSignalApos_adAvailOffset,
    scte35TimeSignalApos_noRegionalBlackoutFlag,
    scte35TimeSignalApos_webDeliveryAllowedFlag,

    -- ** Scte35TimeSignalScheduleActionSettings
    scte35TimeSignalScheduleActionSettings_scte35Descriptors,

    -- ** SmpteTtDestinationSettings

    -- ** StandardHlsSettings
    standardHlsSettings_audioRenditionSets,
    standardHlsSettings_m3u8Settings,

    -- ** StartTimecode
    startTimecode_timecode,

    -- ** StaticImageActivateScheduleActionSettings
    staticImageActivateScheduleActionSettings_height,
    staticImageActivateScheduleActionSettings_imageX,
    staticImageActivateScheduleActionSettings_imageY,
    staticImageActivateScheduleActionSettings_duration,
    staticImageActivateScheduleActionSettings_width,
    staticImageActivateScheduleActionSettings_layer,
    staticImageActivateScheduleActionSettings_opacity,
    staticImageActivateScheduleActionSettings_fadeIn,
    staticImageActivateScheduleActionSettings_fadeOut,
    staticImageActivateScheduleActionSettings_image,

    -- ** StaticImageDeactivateScheduleActionSettings
    staticImageDeactivateScheduleActionSettings_layer,
    staticImageDeactivateScheduleActionSettings_fadeOut,

    -- ** StaticKeySettings
    staticKeySettings_keyProviderServer,
    staticKeySettings_staticKeyValue,

    -- ** StopTimecode
    stopTimecode_timecode,
    stopTimecode_lastFrameClippingBehavior,

    -- ** TeletextDestinationSettings

    -- ** TeletextSourceSettings
    teletextSourceSettings_pageNumber,

    -- ** TemporalFilterSettings
    temporalFilterSettings_postFilterSharpening,
    temporalFilterSettings_strength,

    -- ** TimecodeConfig
    timecodeConfig_syncThreshold,
    timecodeConfig_source,

    -- ** TransferringInputDeviceSummary
    transferringInputDeviceSummary_transferType,
    transferringInputDeviceSummary_message,
    transferringInputDeviceSummary_id,
    transferringInputDeviceSummary_targetCustomerId,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_styleControl,

    -- ** UdpContainerSettings
    udpContainerSettings_m2tsSettings,

    -- ** UdpGroupSettings
    udpGroupSettings_timedMetadataId3Period,
    udpGroupSettings_timedMetadataId3Frame,
    udpGroupSettings_inputLossAction,

    -- ** UdpOutputSettings
    udpOutputSettings_bufferMsec,
    udpOutputSettings_fecOutputSettings,
    udpOutputSettings_destination,
    udpOutputSettings_containerSettings,

    -- ** VideoBlackFailoverSettings
    videoBlackFailoverSettings_blackDetectThreshold,
    videoBlackFailoverSettings_videoBlackThresholdMsec,

    -- ** VideoCodecSettings
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_h265Settings,

    -- ** VideoDescription
    videoDescription_height,
    videoDescription_respondToAfd,
    videoDescription_width,
    videoDescription_codecSettings,
    videoDescription_scalingBehavior,
    videoDescription_sharpness,
    videoDescription_name,

    -- ** VideoSelector
    videoSelector_colorSpaceUsage,
    videoSelector_selectorSettings,
    videoSelector_colorSpace,

    -- ** VideoSelectorPid
    videoSelectorPid_pid,

    -- ** VideoSelectorProgramId
    videoSelectorProgramId_programId,

    -- ** VideoSelectorSettings
    videoSelectorSettings_videoSelectorPid,
    videoSelectorSettings_videoSelectorProgramId,

    -- ** VpcOutputSettings
    vpcOutputSettings_securityGroupIds,
    vpcOutputSettings_publicAddressAllocationIds,
    vpcOutputSettings_subnetIds,

    -- ** WavSettings
    wavSettings_codingMode,
    wavSettings_bitDepth,
    wavSettings_sampleRate,

    -- ** WebvttDestinationSettings
  )
where

import Network.AWS.MediaLive.AcceptInputDeviceTransfer
import Network.AWS.MediaLive.BatchDelete
import Network.AWS.MediaLive.BatchStart
import Network.AWS.MediaLive.BatchStop
import Network.AWS.MediaLive.BatchUpdateSchedule
import Network.AWS.MediaLive.CancelInputDeviceTransfer
import Network.AWS.MediaLive.CreateChannel
import Network.AWS.MediaLive.CreateInput
import Network.AWS.MediaLive.CreateInputSecurityGroup
import Network.AWS.MediaLive.CreateMultiplex
import Network.AWS.MediaLive.CreateMultiplexProgram
import Network.AWS.MediaLive.CreatePartnerInput
import Network.AWS.MediaLive.CreateTags
import Network.AWS.MediaLive.DeleteChannel
import Network.AWS.MediaLive.DeleteInput
import Network.AWS.MediaLive.DeleteInputSecurityGroup
import Network.AWS.MediaLive.DeleteMultiplex
import Network.AWS.MediaLive.DeleteMultiplexProgram
import Network.AWS.MediaLive.DeleteReservation
import Network.AWS.MediaLive.DeleteSchedule
import Network.AWS.MediaLive.DeleteTags
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeInputDevice
import Network.AWS.MediaLive.DescribeInputDeviceThumbnail
import Network.AWS.MediaLive.DescribeInputSecurityGroup
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.DescribeMultiplexProgram
import Network.AWS.MediaLive.DescribeOffering
import Network.AWS.MediaLive.DescribeReservation
import Network.AWS.MediaLive.DescribeSchedule
import Network.AWS.MediaLive.ListChannels
import Network.AWS.MediaLive.ListInputDeviceTransfers
import Network.AWS.MediaLive.ListInputDevices
import Network.AWS.MediaLive.ListInputSecurityGroups
import Network.AWS.MediaLive.ListInputs
import Network.AWS.MediaLive.ListMultiplexPrograms
import Network.AWS.MediaLive.ListMultiplexes
import Network.AWS.MediaLive.ListOfferings
import Network.AWS.MediaLive.ListReservations
import Network.AWS.MediaLive.ListTagsForResource
import Network.AWS.MediaLive.PurchaseOffering
import Network.AWS.MediaLive.RejectInputDeviceTransfer
import Network.AWS.MediaLive.StartChannel
import Network.AWS.MediaLive.StartMultiplex
import Network.AWS.MediaLive.StopChannel
import Network.AWS.MediaLive.StopMultiplex
import Network.AWS.MediaLive.TransferInputDevice
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.AudioChannelMapping
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescription
import Network.AWS.MediaLive.Types.AudioLanguageSelection
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
import Network.AWS.MediaLive.Types.AudioPidSelection
import Network.AWS.MediaLive.Types.AudioSelector
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings
import Network.AWS.MediaLive.Types.AudioTrack
import Network.AWS.MediaLive.Types.AudioTrackSelection
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.AvailBlanking
import Network.AWS.MediaLive.Types.AvailConfiguration
import Network.AWS.MediaLive.Types.AvailSettings
import Network.AWS.MediaLive.Types.BatchFailedResultModel
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
import Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
import Network.AWS.MediaLive.Types.BlackoutSlate
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.CaptionDescription
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
import Network.AWS.MediaLive.Types.CaptionLanguageMapping
import Network.AWS.MediaLive.Types.CaptionSelector
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.Channel
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelSummary
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.DvbNitSettings
import Network.AWS.MediaLive.Types.DvbSdtSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.DvbTdtSettings
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.EncoderSettings
import Network.AWS.MediaLive.Types.FailoverCondition
import Network.AWS.MediaLive.Types.FailoverConditionSettings
import Network.AWS.MediaLive.Types.FeatureActivations
import Network.AWS.MediaLive.Types.FecOutputSettings
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Fmp4HlsSettings
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureHlsSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureSettings
import Network.AWS.MediaLive.Types.GlobalConfiguration
import Network.AWS.MediaLive.Types.H264ColorSpaceSettings
import Network.AWS.MediaLive.Types.H264FilterSettings
import Network.AWS.MediaLive.Types.H264Settings
import Network.AWS.MediaLive.Types.H265ColorSpaceSettings
import Network.AWS.MediaLive.Types.H265FilterSettings
import Network.AWS.MediaLive.Types.H265Settings
import Network.AWS.MediaLive.Types.Hdr10Settings
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
import Network.AWS.MediaLive.Types.HlsCdnSettings
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsInputSettings
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.HlsSettings
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Input
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputChannelLevel
import Network.AWS.MediaLive.Types.InputClippingSettings
import Network.AWS.MediaLive.Types.InputDestination
import Network.AWS.MediaLive.Types.InputDestinationRequest
import Network.AWS.MediaLive.Types.InputDestinationVpc
import Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceRequest
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputDeviceSummary
import Network.AWS.MediaLive.Types.InputDeviceUhdSettings
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.MediaLive.Types.InputLossBehavior
import Network.AWS.MediaLive.Types.InputLossFailoverSettings
import Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
import Network.AWS.MediaLive.Types.InputSecurityGroup
import Network.AWS.MediaLive.Types.InputSettings
import Network.AWS.MediaLive.Types.InputSource
import Network.AWS.MediaLive.Types.InputSourceRequest
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
import Network.AWS.MediaLive.Types.InputVpcRequest
import Network.AWS.MediaLive.Types.InputWhitelistRule
import Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
import Network.AWS.MediaLive.Types.KeyProviderSettings
import Network.AWS.MediaLive.Types.M2tsSettings
import Network.AWS.MediaLive.Types.M3u8Settings
import Network.AWS.MediaLive.Types.MediaConnectFlow
import Network.AWS.MediaLive.Types.MediaConnectFlowRequest
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.Mpeg2FilterSettings
import Network.AWS.MediaLive.Types.Mpeg2Settings
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
import Network.AWS.MediaLive.Types.Multiplex
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
import Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import Network.AWS.MediaLive.Types.MultiplexOutputDestination
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
import Network.AWS.MediaLive.Types.MultiplexProgram
import Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
import Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
import Network.AWS.MediaLive.Types.MultiplexProgramSummary
import Network.AWS.MediaLive.Types.MultiplexSettings
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
import Network.AWS.MediaLive.Types.MultiplexSummary
import Network.AWS.MediaLive.Types.MultiplexVideoSettings
import Network.AWS.MediaLive.Types.NetworkInputSettings
import Network.AWS.MediaLive.Types.NielsenConfiguration
import Network.AWS.MediaLive.Types.Offering
import Network.AWS.MediaLive.Types.Output
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.MediaLive.Types.OutputDestinationSettings
import Network.AWS.MediaLive.Types.OutputGroup
import Network.AWS.MediaLive.Types.OutputGroupSettings
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.OutputSettings
import Network.AWS.MediaLive.Types.PassThroughSettings
import Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
import Network.AWS.MediaLive.Types.PipelineDetail
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
import Network.AWS.MediaLive.Types.RawSettings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import Network.AWS.MediaLive.Types.RemixSettings
import Network.AWS.MediaLive.Types.Reservation
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.ScheduleAction
import Network.AWS.MediaLive.Types.ScheduleActionSettings
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
import Network.AWS.MediaLive.Types.Scte35Descriptor
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
import Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
import Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
import Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.StandardHlsSettings
import Network.AWS.MediaLive.Types.StartTimecode
import Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticKeySettings
import Network.AWS.MediaLive.Types.StopTimecode
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import Network.AWS.MediaLive.Types.TimecodeConfig
import Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.UdpContainerSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings
import Network.AWS.MediaLive.Types.VideoBlackFailoverSettings
import Network.AWS.MediaLive.Types.VideoCodecSettings
import Network.AWS.MediaLive.Types.VideoDescription
import Network.AWS.MediaLive.Types.VideoSelector
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
import Network.AWS.MediaLive.Types.VideoSelectorSettings
import Network.AWS.MediaLive.Types.VpcOutputSettings
import Network.AWS.MediaLive.Types.WavSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import Network.AWS.MediaLive.UpdateChannel
import Network.AWS.MediaLive.UpdateChannelClass
import Network.AWS.MediaLive.UpdateInput
import Network.AWS.MediaLive.UpdateInputDevice
import Network.AWS.MediaLive.UpdateInputSecurityGroup
import Network.AWS.MediaLive.UpdateMultiplex
import Network.AWS.MediaLive.UpdateMultiplexProgram
import Network.AWS.MediaLive.UpdateReservation
