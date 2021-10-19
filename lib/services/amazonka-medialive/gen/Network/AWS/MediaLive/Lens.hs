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

    -- ** UpdateChannelClass
    updateChannelClass'_destinations,
    updateChannelClass'_channelId,
    updateChannelClass'_channelClass,
    updateChannelClassResponse_channel,
    updateChannelClassResponse_httpStatus,

    -- ** ListMultiplexes
    listMultiplexes_nextToken,
    listMultiplexes_maxResults,
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_httpStatus,

    -- ** BatchStart
    batchStart'_channelIds,
    batchStart'_multiplexIds,
    batchStartResponse_successful,
    batchStartResponse_failed,
    batchStartResponse_httpStatus,

    -- ** CreateMultiplex
    createMultiplex'_tags,
    createMultiplex'_requestId,
    createMultiplex'_multiplexSettings,
    createMultiplex'_availabilityZones,
    createMultiplex'_name,
    createMultiplexResponse_multiplex,
    createMultiplexResponse_httpStatus,

    -- ** ListInputDeviceTransfers
    listInputDeviceTransfers_nextToken,
    listInputDeviceTransfers_maxResults,
    listInputDeviceTransfers_transferType,
    listInputDeviceTransfersResponse_nextToken,
    listInputDeviceTransfersResponse_inputDeviceTransfers,
    listInputDeviceTransfersResponse_httpStatus,

    -- ** ListInputDevices
    listInputDevices_nextToken,
    listInputDevices_maxResults,
    listInputDevicesResponse_inputDevices,
    listInputDevicesResponse_nextToken,
    listInputDevicesResponse_httpStatus,

    -- ** ListInputs
    listInputs_nextToken,
    listInputs_maxResults,
    listInputsResponse_inputs,
    listInputsResponse_nextToken,
    listInputsResponse_httpStatus,

    -- ** DescribeInputDeviceThumbnail
    describeInputDeviceThumbnail_inputDeviceId,
    describeInputDeviceThumbnail_accept,
    describeInputDeviceThumbnailResponse_eTag,
    describeInputDeviceThumbnailResponse_contentLength,
    describeInputDeviceThumbnailResponse_lastModified,
    describeInputDeviceThumbnailResponse_contentType,
    describeInputDeviceThumbnailResponse_httpStatus,
    describeInputDeviceThumbnailResponse_body,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** DescribeInputSecurityGroup
    describeInputSecurityGroup_inputSecurityGroupId,
    describeInputSecurityGroupResponse_state,
    describeInputSecurityGroupResponse_arn,
    describeInputSecurityGroupResponse_inputs,
    describeInputSecurityGroupResponse_id,
    describeInputSecurityGroupResponse_whitelistRules,
    describeInputSecurityGroupResponse_tags,
    describeInputSecurityGroupResponse_httpStatus,

    -- ** CreateInput
    createInput'_requestId,
    createInput'_inputDevices,
    createInput'_sources,
    createInput'_inputSecurityGroups,
    createInput'_destinations,
    createInput'_name,
    createInput'_vpc,
    createInput'_type,
    createInput'_mediaConnectFlows,
    createInput'_tags,
    createInput'_roleArn,
    createInputResponse_input,
    createInputResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelId,
    deleteChannelResponse_state,
    deleteChannelResponse_logLevel,
    deleteChannelResponse_arn,
    deleteChannelResponse_pipelinesRunningCount,
    deleteChannelResponse_pipelineDetails,
    deleteChannelResponse_inputSpecification,
    deleteChannelResponse_inputAttachments,
    deleteChannelResponse_destinations,
    deleteChannelResponse_name,
    deleteChannelResponse_cdiInputSpecification,
    deleteChannelResponse_id,
    deleteChannelResponse_channelClass,
    deleteChannelResponse_vpc,
    deleteChannelResponse_egressEndpoints,
    deleteChannelResponse_tags,
    deleteChannelResponse_encoderSettings,
    deleteChannelResponse_roleArn,
    deleteChannelResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel'_logLevel,
    updateChannel'_inputSpecification,
    updateChannel'_inputAttachments,
    updateChannel'_destinations,
    updateChannel'_name,
    updateChannel'_cdiInputSpecification,
    updateChannel'_encoderSettings,
    updateChannel'_roleArn,
    updateChannel'_channelId,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- ** AcceptInputDeviceTransfer
    acceptInputDeviceTransfer_inputDeviceId,
    acceptInputDeviceTransferResponse_httpStatus,

    -- ** DescribeReservation
    describeReservation_reservationId,
    describeReservationResponse_state,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_currencyCode,
    describeReservationResponse_arn,
    describeReservationResponse_start,
    describeReservationResponse_count,
    describeReservationResponse_end,
    describeReservationResponse_name,
    describeReservationResponse_reservationId,
    describeReservationResponse_offeringId,
    describeReservationResponse_region,
    describeReservationResponse_offeringType,
    describeReservationResponse_usagePrice,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_durationUnits,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_duration,
    describeReservationResponse_tags,
    describeReservationResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** StopMultiplex
    stopMultiplex_multiplexId,
    stopMultiplexResponse_state,
    stopMultiplexResponse_arn,
    stopMultiplexResponse_pipelinesRunningCount,
    stopMultiplexResponse_availabilityZones,
    stopMultiplexResponse_programCount,
    stopMultiplexResponse_destinations,
    stopMultiplexResponse_name,
    stopMultiplexResponse_id,
    stopMultiplexResponse_multiplexSettings,
    stopMultiplexResponse_tags,
    stopMultiplexResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** CreateInputSecurityGroup
    createInputSecurityGroup_whitelistRules,
    createInputSecurityGroup_tags,
    createInputSecurityGroupResponse_securityGroup,
    createInputSecurityGroupResponse_httpStatus,

    -- ** StartChannel
    startChannel_channelId,
    startChannelResponse_state,
    startChannelResponse_logLevel,
    startChannelResponse_arn,
    startChannelResponse_pipelinesRunningCount,
    startChannelResponse_pipelineDetails,
    startChannelResponse_inputSpecification,
    startChannelResponse_inputAttachments,
    startChannelResponse_destinations,
    startChannelResponse_name,
    startChannelResponse_cdiInputSpecification,
    startChannelResponse_id,
    startChannelResponse_channelClass,
    startChannelResponse_vpc,
    startChannelResponse_egressEndpoints,
    startChannelResponse_tags,
    startChannelResponse_encoderSettings,
    startChannelResponse_roleArn,
    startChannelResponse_httpStatus,

    -- ** CancelInputDeviceTransfer
    cancelInputDeviceTransfer_inputDeviceId,
    cancelInputDeviceTransferResponse_httpStatus,

    -- ** ListInputSecurityGroups
    listInputSecurityGroups_nextToken,
    listInputSecurityGroups_maxResults,
    listInputSecurityGroupsResponse_nextToken,
    listInputSecurityGroupsResponse_inputSecurityGroups,
    listInputSecurityGroupsResponse_httpStatus,

    -- ** DeleteReservation
    deleteReservation_reservationId,
    deleteReservationResponse_state,
    deleteReservationResponse_resourceSpecification,
    deleteReservationResponse_currencyCode,
    deleteReservationResponse_arn,
    deleteReservationResponse_start,
    deleteReservationResponse_count,
    deleteReservationResponse_end,
    deleteReservationResponse_name,
    deleteReservationResponse_reservationId,
    deleteReservationResponse_offeringId,
    deleteReservationResponse_region,
    deleteReservationResponse_offeringType,
    deleteReservationResponse_usagePrice,
    deleteReservationResponse_fixedPrice,
    deleteReservationResponse_durationUnits,
    deleteReservationResponse_offeringDescription,
    deleteReservationResponse_duration,
    deleteReservationResponse_tags,
    deleteReservationResponse_httpStatus,

    -- ** UpdateReservation
    updateReservation'_name,
    updateReservation'_reservationId,
    updateReservationResponse_reservation,
    updateReservationResponse_httpStatus,

    -- ** BatchStop
    batchStop'_channelIds,
    batchStop'_multiplexIds,
    batchStopResponse_successful,
    batchStopResponse_failed,
    batchStopResponse_httpStatus,

    -- ** DeleteSchedule
    deleteSchedule_channelId,
    deleteScheduleResponse_httpStatus,

    -- ** CreatePartnerInput
    createPartnerInput'_requestId,
    createPartnerInput'_tags,
    createPartnerInput'_inputId,
    createPartnerInputResponse_input,
    createPartnerInputResponse_httpStatus,

    -- ** CreateChannel
    createChannel'_requestId,
    createChannel'_logLevel,
    createChannel'_inputSpecification,
    createChannel'_inputAttachments,
    createChannel'_reserved,
    createChannel'_destinations,
    createChannel'_name,
    createChannel'_cdiInputSpecification,
    createChannel'_channelClass,
    createChannel'_vpc,
    createChannel'_tags,
    createChannel'_encoderSettings,
    createChannel'_roleArn,
    createChannelResponse_channel,
    createChannelResponse_httpStatus,

    -- ** DeleteInput
    deleteInput_inputId,
    deleteInputResponse_httpStatus,

    -- ** UpdateInput
    updateInput'_inputDevices,
    updateInput'_sources,
    updateInput'_inputSecurityGroups,
    updateInput'_destinations,
    updateInput'_name,
    updateInput'_mediaConnectFlows,
    updateInput'_roleArn,
    updateInput'_inputId,
    updateInputResponse_input,
    updateInputResponse_httpStatus,

    -- ** UpdateInputDevice
    updateInputDevice'_hdDeviceSettings,
    updateInputDevice'_uhdDeviceSettings,
    updateInputDevice'_name,
    updateInputDevice'_inputDeviceId,
    updateInputDeviceResponse_arn,
    updateInputDeviceResponse_macAddress,
    updateInputDeviceResponse_hdDeviceSettings,
    updateInputDeviceResponse_uhdDeviceSettings,
    updateInputDeviceResponse_name,
    updateInputDeviceResponse_id,
    updateInputDeviceResponse_deviceUpdateStatus,
    updateInputDeviceResponse_deviceSettingsSyncState,
    updateInputDeviceResponse_type,
    updateInputDeviceResponse_serialNumber,
    updateInputDeviceResponse_networkSettings,
    updateInputDeviceResponse_connectionState,
    updateInputDeviceResponse_httpStatus,

    -- ** RejectInputDeviceTransfer
    rejectInputDeviceTransfer_inputDeviceId,
    rejectInputDeviceTransferResponse_httpStatus,

    -- ** ClaimDevice
    claimDevice_id,
    claimDeviceResponse_httpStatus,

    -- ** DescribeOffering
    describeOffering_offeringId,
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_arn,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_region,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_duration,
    describeOfferingResponse_httpStatus,

    -- ** TransferInputDevice
    transferInputDevice'_targetRegion,
    transferInputDevice'_transferMessage,
    transferInputDevice'_targetCustomerId,
    transferInputDevice'_inputDeviceId,
    transferInputDeviceResponse_httpStatus,

    -- ** DeleteMultiplexProgram
    deleteMultiplexProgram_multiplexId,
    deleteMultiplexProgram_programName,
    deleteMultiplexProgramResponse_packetIdentifiersMap,
    deleteMultiplexProgramResponse_pipelineDetails,
    deleteMultiplexProgramResponse_programName,
    deleteMultiplexProgramResponse_channelId,
    deleteMultiplexProgramResponse_multiplexProgramSettings,
    deleteMultiplexProgramResponse_httpStatus,

    -- ** UpdateMultiplexProgram
    updateMultiplexProgram'_multiplexProgramSettings,
    updateMultiplexProgram'_multiplexId,
    updateMultiplexProgram'_programName,
    updateMultiplexProgramResponse_multiplexProgram,
    updateMultiplexProgramResponse_httpStatus,

    -- ** BatchDelete
    batchDelete'_channelIds,
    batchDelete'_inputIds,
    batchDelete'_multiplexIds,
    batchDelete'_inputSecurityGroupIds,
    batchDeleteResponse_successful,
    batchDeleteResponse_failed,
    batchDeleteResponse_httpStatus,

    -- ** ListMultiplexPrograms
    listMultiplexPrograms_nextToken,
    listMultiplexPrograms_maxResults,
    listMultiplexPrograms_multiplexId,
    listMultiplexProgramsResponse_nextToken,
    listMultiplexProgramsResponse_multiplexPrograms,
    listMultiplexProgramsResponse_httpStatus,

    -- ** DescribeMultiplex
    describeMultiplex_multiplexId,
    describeMultiplexResponse_state,
    describeMultiplexResponse_arn,
    describeMultiplexResponse_pipelinesRunningCount,
    describeMultiplexResponse_availabilityZones,
    describeMultiplexResponse_programCount,
    describeMultiplexResponse_destinations,
    describeMultiplexResponse_name,
    describeMultiplexResponse_id,
    describeMultiplexResponse_multiplexSettings,
    describeMultiplexResponse_tags,
    describeMultiplexResponse_httpStatus,

    -- ** BatchUpdateSchedule
    batchUpdateSchedule_creates,
    batchUpdateSchedule_deletes,
    batchUpdateSchedule_channelId,
    batchUpdateScheduleResponse_creates,
    batchUpdateScheduleResponse_deletes,
    batchUpdateScheduleResponse_httpStatus,

    -- ** CreateMultiplexProgram
    createMultiplexProgram'_multiplexId,
    createMultiplexProgram'_requestId,
    createMultiplexProgram'_multiplexProgramSettings,
    createMultiplexProgram'_programName,
    createMultiplexProgramResponse_multiplexProgram,
    createMultiplexProgramResponse_httpStatus,

    -- ** DescribeSchedule
    describeSchedule_nextToken,
    describeSchedule_maxResults,
    describeSchedule_channelId,
    describeScheduleResponse_nextToken,
    describeScheduleResponse_scheduleActions,
    describeScheduleResponse_httpStatus,

    -- ** StartMultiplex
    startMultiplex_multiplexId,
    startMultiplexResponse_state,
    startMultiplexResponse_arn,
    startMultiplexResponse_pipelinesRunningCount,
    startMultiplexResponse_availabilityZones,
    startMultiplexResponse_programCount,
    startMultiplexResponse_destinations,
    startMultiplexResponse_name,
    startMultiplexResponse_id,
    startMultiplexResponse_multiplexSettings,
    startMultiplexResponse_tags,
    startMultiplexResponse_httpStatus,

    -- ** StopChannel
    stopChannel_channelId,
    stopChannelResponse_state,
    stopChannelResponse_logLevel,
    stopChannelResponse_arn,
    stopChannelResponse_pipelinesRunningCount,
    stopChannelResponse_pipelineDetails,
    stopChannelResponse_inputSpecification,
    stopChannelResponse_inputAttachments,
    stopChannelResponse_destinations,
    stopChannelResponse_name,
    stopChannelResponse_cdiInputSpecification,
    stopChannelResponse_id,
    stopChannelResponse_channelClass,
    stopChannelResponse_vpc,
    stopChannelResponse_egressEndpoints,
    stopChannelResponse_tags,
    stopChannelResponse_encoderSettings,
    stopChannelResponse_roleArn,
    stopChannelResponse_httpStatus,

    -- ** DescribeInput
    describeInput_inputId,
    describeInputResponse_state,
    describeInputResponse_securityGroups,
    describeInputResponse_arn,
    describeInputResponse_inputDevices,
    describeInputResponse_inputPartnerIds,
    describeInputResponse_sources,
    describeInputResponse_destinations,
    describeInputResponse_name,
    describeInputResponse_attachedChannels,
    describeInputResponse_id,
    describeInputResponse_inputClass,
    describeInputResponse_type,
    describeInputResponse_mediaConnectFlows,
    describeInputResponse_inputSourceType,
    describeInputResponse_tags,
    describeInputResponse_roleArn,
    describeInputResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering'_requestId,
    purchaseOffering'_start,
    purchaseOffering'_name,
    purchaseOffering'_tags,
    purchaseOffering'_offeringId,
    purchaseOffering'_count,
    purchaseOfferingResponse_reservation,
    purchaseOfferingResponse_httpStatus,

    -- ** DescribeInputDevice
    describeInputDevice_inputDeviceId,
    describeInputDeviceResponse_arn,
    describeInputDeviceResponse_macAddress,
    describeInputDeviceResponse_hdDeviceSettings,
    describeInputDeviceResponse_uhdDeviceSettings,
    describeInputDeviceResponse_name,
    describeInputDeviceResponse_id,
    describeInputDeviceResponse_deviceUpdateStatus,
    describeInputDeviceResponse_deviceSettingsSyncState,
    describeInputDeviceResponse_type,
    describeInputDeviceResponse_serialNumber,
    describeInputDeviceResponse_networkSettings,
    describeInputDeviceResponse_connectionState,
    describeInputDeviceResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_channelId,
    describeChannelResponse_state,
    describeChannelResponse_logLevel,
    describeChannelResponse_arn,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_destinations,
    describeChannelResponse_name,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_id,
    describeChannelResponse_channelClass,
    describeChannelResponse_vpc,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_tags,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_roleArn,
    describeChannelResponse_httpStatus,

    -- ** UpdateInputSecurityGroup
    updateInputSecurityGroup_whitelistRules,
    updateInputSecurityGroup_tags,
    updateInputSecurityGroup_inputSecurityGroupId,
    updateInputSecurityGroupResponse_securityGroup,
    updateInputSecurityGroupResponse_httpStatus,

    -- ** DeleteInputSecurityGroup
    deleteInputSecurityGroup_inputSecurityGroupId,
    deleteInputSecurityGroupResponse_httpStatus,

    -- ** ListReservations
    listReservations_videoQuality,
    listReservations_maximumFramerate,
    listReservations_resourceType,
    listReservations_resolution,
    listReservations_codec,
    listReservations_nextToken,
    listReservations_specialFeature,
    listReservations_channelClass,
    listReservations_maximumBitrate,
    listReservations_maxResults,
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,

    -- ** DeleteMultiplex
    deleteMultiplex_multiplexId,
    deleteMultiplexResponse_state,
    deleteMultiplexResponse_arn,
    deleteMultiplexResponse_pipelinesRunningCount,
    deleteMultiplexResponse_availabilityZones,
    deleteMultiplexResponse_programCount,
    deleteMultiplexResponse_destinations,
    deleteMultiplexResponse_name,
    deleteMultiplexResponse_id,
    deleteMultiplexResponse_multiplexSettings,
    deleteMultiplexResponse_tags,
    deleteMultiplexResponse_httpStatus,

    -- ** UpdateMultiplex
    updateMultiplex'_name,
    updateMultiplex'_multiplexSettings,
    updateMultiplex'_multiplexId,
    updateMultiplexResponse_multiplex,
    updateMultiplexResponse_httpStatus,

    -- ** DescribeMultiplexProgram
    describeMultiplexProgram_multiplexId,
    describeMultiplexProgram_programName,
    describeMultiplexProgramResponse_packetIdentifiersMap,
    describeMultiplexProgramResponse_pipelineDetails,
    describeMultiplexProgramResponse_programName,
    describeMultiplexProgramResponse_channelId,
    describeMultiplexProgramResponse_multiplexProgramSettings,
    describeMultiplexProgramResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_videoQuality,
    listOfferings_maximumFramerate,
    listOfferings_resourceType,
    listOfferings_channelConfiguration,
    listOfferings_resolution,
    listOfferings_codec,
    listOfferings_nextToken,
    listOfferings_specialFeature,
    listOfferings_channelClass,
    listOfferings_maximumBitrate,
    listOfferings_duration,
    listOfferings_maxResults,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_rawFormat,
    aacSettings_codingMode,
    aacSettings_profile,
    aacSettings_rateControlMode,
    aacSettings_sampleRate,
    aacSettings_spec,
    aacSettings_bitrate,
    aacSettings_vbrQuality,
    aacSettings_inputType,

    -- ** Ac3Settings
    ac3Settings_lfeFilter,
    ac3Settings_metadataControl,
    ac3Settings_bitstreamMode,
    ac3Settings_codingMode,
    ac3Settings_bitrate,
    ac3Settings_dialnorm,
    ac3Settings_drcProfile,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** ArchiveCdnSettings
    archiveCdnSettings_archiveS3Settings,

    -- ** ArchiveContainerSettings
    archiveContainerSettings_m2tsSettings,
    archiveContainerSettings_rawSettings,

    -- ** ArchiveGroupSettings
    archiveGroupSettings_rolloverInterval,
    archiveGroupSettings_archiveCdnSettings,
    archiveGroupSettings_destination,

    -- ** ArchiveOutputSettings
    archiveOutputSettings_extension,
    archiveOutputSettings_nameModifier,
    archiveOutputSettings_containerSettings,

    -- ** ArchiveS3Settings
    archiveS3Settings_cannedAcl,

    -- ** AribDestinationSettings

    -- ** AribSourceSettings

    -- ** AudioChannelMapping
    audioChannelMapping_outputChannel,
    audioChannelMapping_inputChannelLevels,

    -- ** AudioCodecSettings
    audioCodecSettings_passThroughSettings,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_wavSettings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_eac3Settings,

    -- ** AudioDescription
    audioDescription_languageCode,
    audioDescription_audioType,
    audioDescription_audioNormalizationSettings,
    audioDescription_languageCodeControl,
    audioDescription_codecSettings,
    audioDescription_audioWatermarkingSettings,
    audioDescription_streamName,
    audioDescription_remixSettings,
    audioDescription_audioTypeControl,
    audioDescription_audioSelectorName,
    audioDescription_name,

    -- ** AudioHlsRenditionSelection
    audioHlsRenditionSelection_name,
    audioHlsRenditionSelection_groupId,

    -- ** AudioLanguageSelection
    audioLanguageSelection_languageSelectionPolicy,
    audioLanguageSelection_languageCode,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithm,

    -- ** AudioOnlyHlsSettings
    audioOnlyHlsSettings_audioOnlyImage,
    audioOnlyHlsSettings_segmentType,
    audioOnlyHlsSettings_audioGroupId,
    audioOnlyHlsSettings_audioTrackType,

    -- ** AudioPidSelection
    audioPidSelection_pid,

    -- ** AudioSelector
    audioSelector_selectorSettings,
    audioSelector_name,

    -- ** AudioSelectorSettings
    audioSelectorSettings_audioLanguageSelection,
    audioSelectorSettings_audioTrackSelection,
    audioSelectorSettings_audioHlsRenditionSelection,
    audioSelectorSettings_audioPidSelection,

    -- ** AudioSilenceFailoverSettings
    audioSilenceFailoverSettings_audioSilenceThresholdMsec,
    audioSilenceFailoverSettings_audioSelectorName,

    -- ** AudioTrack
    audioTrack_track,

    -- ** AudioTrackSelection
    audioTrackSelection_tracks,

    -- ** AudioWatermarkSettings
    audioWatermarkSettings_nielsenWatermarksSettings,

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
    availSettings_scte35SpliceInsert,
    availSettings_scte35TimeSignalApos,

    -- ** BatchFailedResultModel
    batchFailedResultModel_arn,
    batchFailedResultModel_id,
    batchFailedResultModel_code,
    batchFailedResultModel_message,

    -- ** BatchScheduleActionCreateRequest
    batchScheduleActionCreateRequest_scheduleActions,

    -- ** BatchScheduleActionCreateResult
    batchScheduleActionCreateResult_scheduleActions,

    -- ** BatchScheduleActionDeleteRequest
    batchScheduleActionDeleteRequest_actionNames,

    -- ** BatchScheduleActionDeleteResult
    batchScheduleActionDeleteResult_scheduleActions,

    -- ** BatchSuccessfulResultModel
    batchSuccessfulResultModel_state,
    batchSuccessfulResultModel_arn,
    batchSuccessfulResultModel_id,

    -- ** BlackoutSlate
    blackoutSlate_networkEndBlackoutImage,
    blackoutSlate_state,
    blackoutSlate_networkEndBlackout,
    blackoutSlate_networkId,
    blackoutSlate_blackoutSlateImage,

    -- ** BurnInDestinationSettings
    burnInDestinationSettings_backgroundOpacity,
    burnInDestinationSettings_fontOpacity,
    burnInDestinationSettings_shadowYOffset,
    burnInDestinationSettings_fontResolution,
    burnInDestinationSettings_yPosition,
    burnInDestinationSettings_backgroundColor,
    burnInDestinationSettings_shadowXOffset,
    burnInDestinationSettings_fontSize,
    burnInDestinationSettings_xPosition,
    burnInDestinationSettings_alignment,
    burnInDestinationSettings_shadowOpacity,
    burnInDestinationSettings_teletextGridControl,
    burnInDestinationSettings_outlineColor,
    burnInDestinationSettings_outlineSize,
    burnInDestinationSettings_font,
    burnInDestinationSettings_shadowColor,
    burnInDestinationSettings_fontColor,

    -- ** CaptionDescription
    captionDescription_languageCode,
    captionDescription_destinationSettings,
    captionDescription_languageDescription,
    captionDescription_captionSelectorName,
    captionDescription_name,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_ebuTtDDestinationSettings,
    captionDestinationSettings_rtmpCaptionInfoDestinationSettings,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_scte27DestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_scte20PlusEmbeddedDestinationSettings,
    captionDestinationSettings_embeddedPlusScte20DestinationSettings,
    captionDestinationSettings_smpteTtDestinationSettings,
    captionDestinationSettings_webvttDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_burnInDestinationSettings,
    captionDestinationSettings_aribDestinationSettings,

    -- ** CaptionLanguageMapping
    captionLanguageMapping_languageCode,
    captionLanguageMapping_languageDescription,
    captionLanguageMapping_captionChannel,

    -- ** CaptionRectangle
    captionRectangle_topOffset,
    captionRectangle_height,
    captionRectangle_width,
    captionRectangle_leftOffset,

    -- ** CaptionSelector
    captionSelector_languageCode,
    captionSelector_selectorSettings,
    captionSelector_name,

    -- ** CaptionSelectorSettings
    captionSelectorSettings_teletextSourceSettings,
    captionSelectorSettings_aribSourceSettings,
    captionSelectorSettings_scte27SourceSettings,
    captionSelectorSettings_dvbSubSourceSettings,
    captionSelectorSettings_ancillarySourceSettings,
    captionSelectorSettings_scte20SourceSettings,
    captionSelectorSettings_embeddedSourceSettings,

    -- ** CdiInputSpecification
    cdiInputSpecification_resolution,

    -- ** Channel
    channel_state,
    channel_logLevel,
    channel_arn,
    channel_pipelinesRunningCount,
    channel_pipelineDetails,
    channel_inputSpecification,
    channel_inputAttachments,
    channel_destinations,
    channel_name,
    channel_cdiInputSpecification,
    channel_id,
    channel_channelClass,
    channel_vpc,
    channel_egressEndpoints,
    channel_tags,
    channel_encoderSettings,
    channel_roleArn,

    -- ** ChannelEgressEndpoint
    channelEgressEndpoint_sourceIp,

    -- ** ChannelSummary
    channelSummary_state,
    channelSummary_logLevel,
    channelSummary_arn,
    channelSummary_pipelinesRunningCount,
    channelSummary_inputSpecification,
    channelSummary_inputAttachments,
    channelSummary_destinations,
    channelSummary_name,
    channelSummary_cdiInputSpecification,
    channelSummary_id,
    channelSummary_channelClass,
    channelSummary_vpc,
    channelSummary_egressEndpoints,
    channelSummary_tags,
    channelSummary_roleArn,

    -- ** ColorSpacePassthroughSettings

    -- ** DvbNitSettings
    dvbNitSettings_repInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- ** DvbSdtSettings
    dvbSdtSettings_repInterval,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_shadowYOffset,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_yPosition,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_teletextGridControl,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_font,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_fontColor,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_ocrLanguage,
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_repInterval,

    -- ** Eac3Settings
    eac3Settings_stereoDownmix,
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_lfeFilter,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_metadataControl,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_surroundMode,
    eac3Settings_attenuationControl,
    eac3Settings_passthroughControl,
    eac3Settings_bitstreamMode,
    eac3Settings_lfeControl,
    eac3Settings_codingMode,
    eac3Settings_drcLine,
    eac3Settings_drcRf,
    eac3Settings_dcFilter,
    eac3Settings_bitrate,
    eac3Settings_phaseControl,
    eac3Settings_surroundExMode,
    eac3Settings_dialnorm,

    -- ** EbuTtDDestinationSettings
    ebuTtDDestinationSettings_fillLineGap,
    ebuTtDDestinationSettings_copyrightHolder,
    ebuTtDDestinationSettings_fontFamily,
    ebuTtDDestinationSettings_styleControl,

    -- ** EmbeddedDestinationSettings

    -- ** EmbeddedPlusScte20DestinationSettings

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_scte20Detection,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- ** EncoderSettings
    encoderSettings_captionDescriptions,
    encoderSettings_availConfiguration,
    encoderSettings_featureActivations,
    encoderSettings_nielsenConfiguration,
    encoderSettings_availBlanking,
    encoderSettings_globalConfiguration,
    encoderSettings_motionGraphicsConfiguration,
    encoderSettings_blackoutSlate,
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
    fecOutputSettings_includeFec,
    fecOutputSettings_columnDepth,

    -- ** FixedModeScheduleActionStartSettings
    fixedModeScheduleActionStartSettings_time,

    -- ** Fmp4HlsSettings
    fmp4HlsSettings_nielsenId3Behavior,
    fmp4HlsSettings_audioRenditionSets,
    fmp4HlsSettings_timedMetadataBehavior,

    -- ** FollowModeScheduleActionStartSettings
    followModeScheduleActionStartSettings_referenceActionName,
    followModeScheduleActionStartSettings_followPoint,

    -- ** FrameCaptureCdnSettings
    frameCaptureCdnSettings_frameCaptureS3Settings,

    -- ** FrameCaptureGroupSettings
    frameCaptureGroupSettings_frameCaptureCdnSettings,
    frameCaptureGroupSettings_destination,

    -- ** FrameCaptureHlsSettings

    -- ** FrameCaptureOutputSettings
    frameCaptureOutputSettings_nameModifier,

    -- ** FrameCaptureS3Settings
    frameCaptureS3Settings_cannedAcl,

    -- ** FrameCaptureSettings
    frameCaptureSettings_captureIntervalUnits,
    frameCaptureSettings_captureInterval,

    -- ** GlobalConfiguration
    globalConfiguration_outputLockingMode,
    globalConfiguration_inputLossBehavior,
    globalConfiguration_initialAudioGain,
    globalConfiguration_supportLowFramerateInputs,
    globalConfiguration_inputEndAction,
    globalConfiguration_outputTimingSource,

    -- ** H264ColorSpaceSettings
    h264ColorSpaceSettings_rec709Settings,
    h264ColorSpaceSettings_rec601Settings,
    h264ColorSpaceSettings_colorSpacePassthroughSettings,

    -- ** H264FilterSettings
    h264FilterSettings_temporalFilterSettings,

    -- ** H264Settings
    h264Settings_temporalAq,
    h264Settings_sceneChangeDetect,
    h264Settings_scanType,
    h264Settings_timecodeInsertion,
    h264Settings_parNumerator,
    h264Settings_afdSignaling,
    h264Settings_gopSize,
    h264Settings_gopSizeUnits,
    h264Settings_subgopLength,
    h264Settings_qualityLevel,
    h264Settings_slices,
    h264Settings_profile,
    h264Settings_rateControlMode,
    h264Settings_minIInterval,
    h264Settings_qvbrQualityLevel,
    h264Settings_colorSpaceSettings,
    h264Settings_parControl,
    h264Settings_flickerAq,
    h264Settings_bufSize,
    h264Settings_spatialAq,
    h264Settings_gopNumBFrames,
    h264Settings_fixedAfd,
    h264Settings_softness,
    h264Settings_filterSettings,
    h264Settings_bitrate,
    h264Settings_framerateDenominator,
    h264Settings_forceFieldPictures,
    h264Settings_entropyEncoding,
    h264Settings_framerateControl,
    h264Settings_colorMetadata,
    h264Settings_lookAheadRateControl,
    h264Settings_adaptiveQuantization,
    h264Settings_framerateNumerator,
    h264Settings_level,
    h264Settings_gopBReference,
    h264Settings_maxBitrate,
    h264Settings_syntax,
    h264Settings_bufFillPct,
    h264Settings_gopClosedCadence,
    h264Settings_numRefFrames,
    h264Settings_parDenominator,

    -- ** H265ColorSpaceSettings
    h265ColorSpaceSettings_hdr10Settings,
    h265ColorSpaceSettings_rec709Settings,
    h265ColorSpaceSettings_rec601Settings,
    h265ColorSpaceSettings_colorSpacePassthroughSettings,

    -- ** H265FilterSettings
    h265FilterSettings_temporalFilterSettings,

    -- ** H265Settings
    h265Settings_sceneChangeDetect,
    h265Settings_scanType,
    h265Settings_timecodeInsertion,
    h265Settings_parNumerator,
    h265Settings_afdSignaling,
    h265Settings_gopSize,
    h265Settings_gopSizeUnits,
    h265Settings_slices,
    h265Settings_profile,
    h265Settings_alternativeTransferFunction,
    h265Settings_rateControlMode,
    h265Settings_minIInterval,
    h265Settings_qvbrQualityLevel,
    h265Settings_colorSpaceSettings,
    h265Settings_flickerAq,
    h265Settings_bufSize,
    h265Settings_tier,
    h265Settings_fixedAfd,
    h265Settings_filterSettings,
    h265Settings_bitrate,
    h265Settings_colorMetadata,
    h265Settings_lookAheadRateControl,
    h265Settings_adaptiveQuantization,
    h265Settings_level,
    h265Settings_maxBitrate,
    h265Settings_gopClosedCadence,
    h265Settings_parDenominator,
    h265Settings_framerateNumerator,
    h265Settings_framerateDenominator,

    -- ** Hdr10Settings
    hdr10Settings_maxFall,
    hdr10Settings_maxCll,

    -- ** HlsAkamaiSettings
    hlsAkamaiSettings_httpTransferMode,
    hlsAkamaiSettings_numRetries,
    hlsAkamaiSettings_token,
    hlsAkamaiSettings_connectionRetryInterval,
    hlsAkamaiSettings_filecacheDuration,
    hlsAkamaiSettings_restartDelay,
    hlsAkamaiSettings_salt,

    -- ** HlsBasicPutSettings
    hlsBasicPutSettings_numRetries,
    hlsBasicPutSettings_connectionRetryInterval,
    hlsBasicPutSettings_filecacheDuration,
    hlsBasicPutSettings_restartDelay,

    -- ** HlsCdnSettings
    hlsCdnSettings_hlsAkamaiSettings,
    hlsCdnSettings_hlsMediaStoreSettings,
    hlsCdnSettings_hlsS3Settings,
    hlsCdnSettings_hlsBasicPutSettings,
    hlsCdnSettings_hlsWebdavSettings,

    -- ** HlsGroupSettings
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_encryptionType,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_ivInManifest,
    hlsGroupSettings_discontinuityTags,
    hlsGroupSettings_tsFileMode,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_iFrameOnlyPlaylists,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_indexNSegments,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_hlsCdnSettings,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_inputLossAction,
    hlsGroupSettings_mode,
    hlsGroupSettings_keyProviderSettings,
    hlsGroupSettings_incompleteSegmentBehavior,
    hlsGroupSettings_constantIv,
    hlsGroupSettings_baseUrlManifest,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_keyFormat,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_hlsId3SegmentTagging,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_baseUrlContent,
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_ivSource,
    hlsGroupSettings_segmentationMode,
    hlsGroupSettings_keyFormatVersions,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_baseUrlManifest1,
    hlsGroupSettings_redundantManifest,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_keepSegments,
    hlsGroupSettings_baseUrlContent1,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_destination,

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    hlsId3SegmentTaggingScheduleActionSettings_tag,

    -- ** HlsInputSettings
    hlsInputSettings_bufferSegments,
    hlsInputSettings_retries,
    hlsInputSettings_retryInterval,
    hlsInputSettings_bandwidth,
    hlsInputSettings_scte35Source,

    -- ** HlsMediaStoreSettings
    hlsMediaStoreSettings_numRetries,
    hlsMediaStoreSettings_connectionRetryInterval,
    hlsMediaStoreSettings_filecacheDuration,
    hlsMediaStoreSettings_mediaStoreStorageClass,
    hlsMediaStoreSettings_restartDelay,

    -- ** HlsOutputSettings
    hlsOutputSettings_h265PackagingType,
    hlsOutputSettings_segmentModifier,
    hlsOutputSettings_nameModifier,
    hlsOutputSettings_hlsSettings,

    -- ** HlsS3Settings
    hlsS3Settings_cannedAcl,

    -- ** HlsSettings
    hlsSettings_fmp4HlsSettings,
    hlsSettings_audioOnlyHlsSettings,
    hlsSettings_frameCaptureHlsSettings,
    hlsSettings_standardHlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    hlsTimedMetadataScheduleActionSettings_id3,

    -- ** HlsWebdavSettings
    hlsWebdavSettings_httpTransferMode,
    hlsWebdavSettings_numRetries,
    hlsWebdavSettings_connectionRetryInterval,
    hlsWebdavSettings_filecacheDuration,
    hlsWebdavSettings_restartDelay,

    -- ** HtmlMotionGraphicsSettings

    -- ** ImmediateModeScheduleActionStartSettings

    -- ** Input
    input_state,
    input_securityGroups,
    input_arn,
    input_inputDevices,
    input_inputPartnerIds,
    input_sources,
    input_destinations,
    input_name,
    input_attachedChannels,
    input_id,
    input_inputClass,
    input_type,
    input_mediaConnectFlows,
    input_inputSourceType,
    input_tags,
    input_roleArn,

    -- ** InputAttachment
    inputAttachment_inputAttachmentName,
    inputAttachment_inputId,
    inputAttachment_automaticInputFailoverSettings,
    inputAttachment_inputSettings,

    -- ** InputChannelLevel
    inputChannelLevel_inputChannel,
    inputChannelLevel_gain,

    -- ** InputClippingSettings
    inputClippingSettings_stopTimecode,
    inputClippingSettings_startTimecode,
    inputClippingSettings_inputTimecodeSource,

    -- ** InputDestination
    inputDestination_url,
    inputDestination_ip,
    inputDestination_vpc,
    inputDestination_port,

    -- ** InputDestinationRequest
    inputDestinationRequest_streamName,

    -- ** InputDestinationVpc
    inputDestinationVpc_networkInterfaceId,
    inputDestinationVpc_availabilityZone,

    -- ** InputDeviceConfigurableSettings
    inputDeviceConfigurableSettings_configuredInput,
    inputDeviceConfigurableSettings_maxBitrate,

    -- ** InputDeviceHdSettings
    inputDeviceHdSettings_framerate,
    inputDeviceHdSettings_scanType,
    inputDeviceHdSettings_deviceState,
    inputDeviceHdSettings_height,
    inputDeviceHdSettings_activeInput,
    inputDeviceHdSettings_width,
    inputDeviceHdSettings_configuredInput,
    inputDeviceHdSettings_maxBitrate,

    -- ** InputDeviceNetworkSettings
    inputDeviceNetworkSettings_ipAddress,
    inputDeviceNetworkSettings_gateway,
    inputDeviceNetworkSettings_dnsAddresses,
    inputDeviceNetworkSettings_ipScheme,
    inputDeviceNetworkSettings_subnetMask,

    -- ** InputDeviceRequest
    inputDeviceRequest_id,

    -- ** InputDeviceSettings
    inputDeviceSettings_id,

    -- ** InputDeviceSummary
    inputDeviceSummary_arn,
    inputDeviceSummary_macAddress,
    inputDeviceSummary_hdDeviceSettings,
    inputDeviceSummary_uhdDeviceSettings,
    inputDeviceSummary_name,
    inputDeviceSummary_id,
    inputDeviceSummary_deviceUpdateStatus,
    inputDeviceSummary_deviceSettingsSyncState,
    inputDeviceSummary_type,
    inputDeviceSummary_serialNumber,
    inputDeviceSummary_networkSettings,
    inputDeviceSummary_connectionState,

    -- ** InputDeviceUhdSettings
    inputDeviceUhdSettings_framerate,
    inputDeviceUhdSettings_scanType,
    inputDeviceUhdSettings_deviceState,
    inputDeviceUhdSettings_height,
    inputDeviceUhdSettings_activeInput,
    inputDeviceUhdSettings_width,
    inputDeviceUhdSettings_configuredInput,
    inputDeviceUhdSettings_maxBitrate,

    -- ** InputLocation
    inputLocation_username,
    inputLocation_passwordParam,
    inputLocation_uri,

    -- ** InputLossBehavior
    inputLossBehavior_inputLossImageColor,
    inputLossBehavior_blackFrameMsec,
    inputLossBehavior_repeatFrameMsec,
    inputLossBehavior_inputLossImageType,
    inputLossBehavior_inputLossImageSlate,

    -- ** InputLossFailoverSettings
    inputLossFailoverSettings_inputLossThresholdMsec,

    -- ** InputPrepareScheduleActionSettings
    inputPrepareScheduleActionSettings_inputAttachmentNameReference,
    inputPrepareScheduleActionSettings_inputClippingSettings,
    inputPrepareScheduleActionSettings_urlPath,

    -- ** InputSecurityGroup
    inputSecurityGroup_state,
    inputSecurityGroup_arn,
    inputSecurityGroup_inputs,
    inputSecurityGroup_id,
    inputSecurityGroup_whitelistRules,
    inputSecurityGroup_tags,

    -- ** InputSettings
    inputSettings_videoSelector,
    inputSettings_smpte2038DataPreference,
    inputSettings_networkInputSettings,
    inputSettings_audioSelectors,
    inputSettings_deblockFilter,
    inputSettings_denoiseFilter,
    inputSettings_filterStrength,
    inputSettings_captionSelectors,
    inputSettings_inputFilter,
    inputSettings_sourceEndBehavior,

    -- ** InputSource
    inputSource_url,
    inputSource_username,
    inputSource_passwordParam,

    -- ** InputSourceRequest
    inputSourceRequest_url,
    inputSourceRequest_username,
    inputSourceRequest_passwordParam,

    -- ** InputSpecification
    inputSpecification_resolution,
    inputSpecification_codec,
    inputSpecification_maximumBitrate,

    -- ** InputSwitchScheduleActionSettings
    inputSwitchScheduleActionSettings_inputClippingSettings,
    inputSwitchScheduleActionSettings_urlPath,
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
    m2tsSettings_pmtPid,
    m2tsSettings_etvSignalPid,
    m2tsSettings_videoPid,
    m2tsSettings_nielsenId3Behavior,
    m2tsSettings_bufferModel,
    m2tsSettings_scte35Pid,
    m2tsSettings_transportStreamId,
    m2tsSettings_programNum,
    m2tsSettings_fragmentTime,
    m2tsSettings_timedMetadataBehavior,
    m2tsSettings_ccDescriptor,
    m2tsSettings_pmtInterval,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_ecmPid,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_audioBufferModel,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_klv,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_pcrPeriod,
    m2tsSettings_pcrPid,
    m2tsSettings_segmentationMarkers,
    m2tsSettings_aribCaptionsPidControl,
    m2tsSettings_klvDataPids,
    m2tsSettings_ebpLookaheadMs,
    m2tsSettings_dvbSubPids,
    m2tsSettings_scte27Pids,
    m2tsSettings_patInterval,
    m2tsSettings_audioStreamType,
    m2tsSettings_esRateInPes,
    m2tsSettings_etvPlatformPid,
    m2tsSettings_bitrate,
    m2tsSettings_scte35Control,
    m2tsSettings_audioPids,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_ebif,
    m2tsSettings_arib,
    m2tsSettings_aribCaptionsPid,
    m2tsSettings_absentInputAudioBehavior,
    m2tsSettings_segmentationTime,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_pcrControl,
    m2tsSettings_ebpPlacement,
    m2tsSettings_rateMode,
    m2tsSettings_segmentationStyle,
    m2tsSettings_dvbTdtSettings,

    -- ** M3u8Settings
    m3u8Settings_pmtPid,
    m3u8Settings_videoPid,
    m3u8Settings_nielsenId3Behavior,
    m3u8Settings_scte35Pid,
    m3u8Settings_transportStreamId,
    m3u8Settings_programNum,
    m3u8Settings_timedMetadataBehavior,
    m3u8Settings_pmtInterval,
    m3u8Settings_ecmPid,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_pcrPeriod,
    m3u8Settings_pcrPid,
    m3u8Settings_patInterval,
    m3u8Settings_audioPids,
    m3u8Settings_scte35Behavior,
    m3u8Settings_pcrControl,

    -- ** MediaConnectFlow
    mediaConnectFlow_flowArn,

    -- ** MediaConnectFlowRequest
    mediaConnectFlowRequest_flowArn,

    -- ** MediaPackageGroupSettings
    mediaPackageGroupSettings_destination,

    -- ** MediaPackageOutputDestinationSettings
    mediaPackageOutputDestinationSettings_channelId,

    -- ** MediaPackageOutputSettings

    -- ** MotionGraphicsActivateScheduleActionSettings
    motionGraphicsActivateScheduleActionSettings_url,
    motionGraphicsActivateScheduleActionSettings_username,
    motionGraphicsActivateScheduleActionSettings_passwordParam,
    motionGraphicsActivateScheduleActionSettings_duration,

    -- ** MotionGraphicsConfiguration
    motionGraphicsConfiguration_motionGraphicsInsertion,
    motionGraphicsConfiguration_motionGraphicsSettings,

    -- ** MotionGraphicsDeactivateScheduleActionSettings

    -- ** MotionGraphicsSettings
    motionGraphicsSettings_htmlMotionGraphicsSettings,

    -- ** Mp2Settings
    mp2Settings_codingMode,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- ** Mpeg2FilterSettings
    mpeg2FilterSettings_temporalFilterSettings,

    -- ** Mpeg2Settings
    mpeg2Settings_scanType,
    mpeg2Settings_timecodeInsertion,
    mpeg2Settings_afdSignaling,
    mpeg2Settings_gopSize,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_subgopLength,
    mpeg2Settings_displayAspectRatio,
    mpeg2Settings_gopNumBFrames,
    mpeg2Settings_fixedAfd,
    mpeg2Settings_filterSettings,
    mpeg2Settings_colorMetadata,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_colorSpace,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_framerateDenominator,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_streamManifestBehavior,
    msSmoothGroupSettings_sendDelayMs,
    msSmoothGroupSettings_eventStopBehavior,
    msSmoothGroupSettings_timestampOffsetMode,
    msSmoothGroupSettings_numRetries,
    msSmoothGroupSettings_acquisitionPointId,
    msSmoothGroupSettings_inputLossAction,
    msSmoothGroupSettings_timestampOffset,
    msSmoothGroupSettings_certificateMode,
    msSmoothGroupSettings_sparseTrackType,
    msSmoothGroupSettings_connectionRetryInterval,
    msSmoothGroupSettings_filecacheDuration,
    msSmoothGroupSettings_restartDelay,
    msSmoothGroupSettings_eventIdMode,
    msSmoothGroupSettings_audioOnlyTimecodeControl,
    msSmoothGroupSettings_segmentationMode,
    msSmoothGroupSettings_eventId,
    msSmoothGroupSettings_destination,

    -- ** MsSmoothOutputSettings
    msSmoothOutputSettings_h265PackagingType,
    msSmoothOutputSettings_nameModifier,

    -- ** Multiplex
    multiplex_state,
    multiplex_arn,
    multiplex_pipelinesRunningCount,
    multiplex_availabilityZones,
    multiplex_programCount,
    multiplex_destinations,
    multiplex_name,
    multiplex_id,
    multiplex_multiplexSettings,
    multiplex_tags,

    -- ** MultiplexGroupSettings

    -- ** MultiplexMediaConnectOutputDestinationSettings
    multiplexMediaConnectOutputDestinationSettings_entitlementArn,

    -- ** MultiplexOutputDestination
    multiplexOutputDestination_mediaConnectSettings,

    -- ** MultiplexOutputSettings
    multiplexOutputSettings_destination,

    -- ** MultiplexProgram
    multiplexProgram_packetIdentifiersMap,
    multiplexProgram_pipelineDetails,
    multiplexProgram_programName,
    multiplexProgram_channelId,
    multiplexProgram_multiplexProgramSettings,

    -- ** MultiplexProgramChannelDestinationSettings
    multiplexProgramChannelDestinationSettings_multiplexId,
    multiplexProgramChannelDestinationSettings_programName,

    -- ** MultiplexProgramPacketIdentifiersMap
    multiplexProgramPacketIdentifiersMap_pmtPid,
    multiplexProgramPacketIdentifiersMap_etvSignalPid,
    multiplexProgramPacketIdentifiersMap_videoPid,
    multiplexProgramPacketIdentifiersMap_scte35Pid,
    multiplexProgramPacketIdentifiersMap_privateMetadataPid,
    multiplexProgramPacketIdentifiersMap_timedMetadataPid,
    multiplexProgramPacketIdentifiersMap_pcrPid,
    multiplexProgramPacketIdentifiersMap_klvDataPids,
    multiplexProgramPacketIdentifiersMap_dvbSubPids,
    multiplexProgramPacketIdentifiersMap_scte27Pids,
    multiplexProgramPacketIdentifiersMap_etvPlatformPid,
    multiplexProgramPacketIdentifiersMap_audioPids,
    multiplexProgramPacketIdentifiersMap_dvbTeletextPid,

    -- ** MultiplexProgramPipelineDetail
    multiplexProgramPipelineDetail_pipelineId,
    multiplexProgramPipelineDetail_activeChannelPipeline,

    -- ** MultiplexProgramServiceDescriptor
    multiplexProgramServiceDescriptor_providerName,
    multiplexProgramServiceDescriptor_serviceName,

    -- ** MultiplexProgramSettings
    multiplexProgramSettings_preferredChannelPipeline,
    multiplexProgramSettings_videoSettings,
    multiplexProgramSettings_serviceDescriptor,
    multiplexProgramSettings_programNumber,

    -- ** MultiplexProgramSummary
    multiplexProgramSummary_programName,
    multiplexProgramSummary_channelId,

    -- ** MultiplexSettings
    multiplexSettings_maximumVideoBufferDelayMilliseconds,
    multiplexSettings_transportStreamReservedBitrate,
    multiplexSettings_transportStreamBitrate,
    multiplexSettings_transportStreamId,

    -- ** MultiplexSettingsSummary
    multiplexSettingsSummary_transportStreamBitrate,

    -- ** MultiplexStatmuxVideoSettings
    multiplexStatmuxVideoSettings_priority,
    multiplexStatmuxVideoSettings_minimumBitrate,
    multiplexStatmuxVideoSettings_maximumBitrate,

    -- ** MultiplexSummary
    multiplexSummary_state,
    multiplexSummary_arn,
    multiplexSummary_pipelinesRunningCount,
    multiplexSummary_availabilityZones,
    multiplexSummary_programCount,
    multiplexSummary_name,
    multiplexSummary_id,
    multiplexSummary_multiplexSettings,
    multiplexSummary_tags,

    -- ** MultiplexVideoSettings
    multiplexVideoSettings_statmuxSettings,
    multiplexVideoSettings_constantBitrate,

    -- ** NetworkInputSettings
    networkInputSettings_hlsInputSettings,
    networkInputSettings_serverValidation,

    -- ** NielsenCBET
    nielsenCBET_cbetCheckDigitString,
    nielsenCBET_cbetStepaside,
    nielsenCBET_csid,

    -- ** NielsenConfiguration
    nielsenConfiguration_distributorId,
    nielsenConfiguration_nielsenPcmToId3Tagging,

    -- ** NielsenNaesIiNw
    nielsenNaesIiNw_checkDigitString,
    nielsenNaesIiNw_sid,

    -- ** NielsenWatermarksSettings
    nielsenWatermarksSettings_nielsenCbetSettings,
    nielsenWatermarksSettings_nielsenNaesIiNwSettings,
    nielsenWatermarksSettings_nielsenDistributionType,

    -- ** Offering
    offering_resourceSpecification,
    offering_currencyCode,
    offering_arn,
    offering_offeringId,
    offering_region,
    offering_offeringType,
    offering_usagePrice,
    offering_fixedPrice,
    offering_durationUnits,
    offering_offeringDescription,
    offering_duration,

    -- ** Output
    output_captionDescriptionNames,
    output_videoDescriptionName,
    output_outputName,
    output_audioDescriptionNames,
    output_outputSettings,

    -- ** OutputDestination
    outputDestination_settings,
    outputDestination_mediaPackageSettings,
    outputDestination_id,
    outputDestination_multiplexSettings,

    -- ** OutputDestinationSettings
    outputDestinationSettings_url,
    outputDestinationSettings_username,
    outputDestinationSettings_passwordParam,
    outputDestinationSettings_streamName,

    -- ** OutputGroup
    outputGroup_name,
    outputGroup_outputs,
    outputGroup_outputGroupSettings,

    -- ** OutputGroupSettings
    outputGroupSettings_mediaPackageGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_rtmpGroupSettings,
    outputGroupSettings_multiplexGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_archiveGroupSettings,
    outputGroupSettings_udpGroupSettings,
    outputGroupSettings_frameCaptureGroupSettings,

    -- ** OutputLocationRef
    outputLocationRef_destinationRefId,

    -- ** OutputSettings
    outputSettings_multiplexOutputSettings,
    outputSettings_archiveOutputSettings,
    outputSettings_rtmpOutputSettings,
    outputSettings_mediaPackageOutputSettings,
    outputSettings_hlsOutputSettings,
    outputSettings_frameCaptureOutputSettings,
    outputSettings_udpOutputSettings,
    outputSettings_msSmoothOutputSettings,

    -- ** PassThroughSettings

    -- ** PauseStateScheduleActionSettings
    pauseStateScheduleActionSettings_pipelines,

    -- ** PipelineDetail
    pipelineDetail_pipelineId,
    pipelineDetail_activeInputSwitchActionName,
    pipelineDetail_activeMotionGraphicsUri,
    pipelineDetail_activeInputAttachmentName,
    pipelineDetail_activeMotionGraphicsActionName,

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
    reservation_state,
    reservation_resourceSpecification,
    reservation_currencyCode,
    reservation_arn,
    reservation_start,
    reservation_count,
    reservation_end,
    reservation_name,
    reservation_reservationId,
    reservation_offeringId,
    reservation_region,
    reservation_offeringType,
    reservation_usagePrice,
    reservation_fixedPrice,
    reservation_durationUnits,
    reservation_offeringDescription,
    reservation_duration,
    reservation_tags,

    -- ** ReservationResourceSpecification
    reservationResourceSpecification_videoQuality,
    reservationResourceSpecification_maximumFramerate,
    reservationResourceSpecification_resourceType,
    reservationResourceSpecification_resolution,
    reservationResourceSpecification_codec,
    reservationResourceSpecification_specialFeature,
    reservationResourceSpecification_channelClass,
    reservationResourceSpecification_maximumBitrate,

    -- ** RtmpCaptionInfoDestinationSettings

    -- ** RtmpGroupSettings
    rtmpGroupSettings_inputLossAction,
    rtmpGroupSettings_captionData,
    rtmpGroupSettings_adMarkers,
    rtmpGroupSettings_restartDelay,
    rtmpGroupSettings_authenticationScheme,
    rtmpGroupSettings_cacheLength,
    rtmpGroupSettings_cacheFullBehavior,

    -- ** RtmpOutputSettings
    rtmpOutputSettings_numRetries,
    rtmpOutputSettings_certificateMode,
    rtmpOutputSettings_connectionRetryInterval,
    rtmpOutputSettings_destination,

    -- ** ScheduleAction
    scheduleAction_actionName,
    scheduleAction_scheduleActionStartSettings,
    scheduleAction_scheduleActionSettings,

    -- ** ScheduleActionSettings
    scheduleActionSettings_staticImageDeactivateSettings,
    scheduleActionSettings_scte35SpliceInsertSettings,
    scheduleActionSettings_motionGraphicsImageActivateSettings,
    scheduleActionSettings_staticImageActivateSettings,
    scheduleActionSettings_motionGraphicsImageDeactivateSettings,
    scheduleActionSettings_scte35TimeSignalSettings,
    scheduleActionSettings_inputPrepareSettings,
    scheduleActionSettings_hlsId3SegmentTaggingSettings,
    scheduleActionSettings_scte35ReturnToNetworkSettings,
    scheduleActionSettings_pauseStateSettings,
    scheduleActionSettings_hlsTimedMetadataSettings,
    scheduleActionSettings_inputSwitchSettings,

    -- ** ScheduleActionStartSettings
    scheduleActionStartSettings_immediateModeScheduleActionStartSettings,
    scheduleActionStartSettings_followModeScheduleActionStartSettings,
    scheduleActionStartSettings_fixedModeScheduleActionStartSettings,

    -- ** Scte20PlusEmbeddedDestinationSettings

    -- ** Scte20SourceSettings
    scte20SourceSettings_convert608To708,
    scte20SourceSettings_source608ChannelNumber,

    -- ** Scte27DestinationSettings

    -- ** Scte27SourceSettings
    scte27SourceSettings_ocrLanguage,
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
    scte35SegmentationDescriptor_segmentationUpidType,
    scte35SegmentationDescriptor_segmentsExpected,
    scte35SegmentationDescriptor_subSegmentsExpected,
    scte35SegmentationDescriptor_segmentNum,
    scte35SegmentationDescriptor_segmentationDuration,
    scte35SegmentationDescriptor_segmentationTypeId,
    scte35SegmentationDescriptor_deliveryRestrictions,
    scte35SegmentationDescriptor_segmentationUpid,
    scte35SegmentationDescriptor_subSegmentNum,
    scte35SegmentationDescriptor_segmentationEventId,
    scte35SegmentationDescriptor_segmentationCancelIndicator,

    -- ** Scte35SpliceInsert
    scte35SpliceInsert_webDeliveryAllowedFlag,
    scte35SpliceInsert_adAvailOffset,
    scte35SpliceInsert_noRegionalBlackoutFlag,

    -- ** Scte35SpliceInsertScheduleActionSettings
    scte35SpliceInsertScheduleActionSettings_duration,
    scte35SpliceInsertScheduleActionSettings_spliceEventId,

    -- ** Scte35TimeSignalApos
    scte35TimeSignalApos_webDeliveryAllowedFlag,
    scte35TimeSignalApos_adAvailOffset,
    scte35TimeSignalApos_noRegionalBlackoutFlag,

    -- ** Scte35TimeSignalScheduleActionSettings
    scte35TimeSignalScheduleActionSettings_scte35Descriptors,

    -- ** SmpteTtDestinationSettings

    -- ** StandardHlsSettings
    standardHlsSettings_audioRenditionSets,
    standardHlsSettings_m3u8Settings,

    -- ** StartTimecode
    startTimecode_timecode,

    -- ** StaticImageActivateScheduleActionSettings
    staticImageActivateScheduleActionSettings_imageX,
    staticImageActivateScheduleActionSettings_height,
    staticImageActivateScheduleActionSettings_fadeOut,
    staticImageActivateScheduleActionSettings_width,
    staticImageActivateScheduleActionSettings_opacity,
    staticImageActivateScheduleActionSettings_layer,
    staticImageActivateScheduleActionSettings_duration,
    staticImageActivateScheduleActionSettings_imageY,
    staticImageActivateScheduleActionSettings_fadeIn,
    staticImageActivateScheduleActionSettings_image,

    -- ** StaticImageDeactivateScheduleActionSettings
    staticImageDeactivateScheduleActionSettings_fadeOut,
    staticImageDeactivateScheduleActionSettings_layer,

    -- ** StaticKeySettings
    staticKeySettings_keyProviderServer,
    staticKeySettings_staticKeyValue,

    -- ** StopTimecode
    stopTimecode_lastFrameClippingBehavior,
    stopTimecode_timecode,

    -- ** TeletextDestinationSettings

    -- ** TeletextSourceSettings
    teletextSourceSettings_outputRectangle,
    teletextSourceSettings_pageNumber,

    -- ** TemporalFilterSettings
    temporalFilterSettings_strength,
    temporalFilterSettings_postFilterSharpening,

    -- ** TimecodeConfig
    timecodeConfig_syncThreshold,
    timecodeConfig_source,

    -- ** TransferringInputDeviceSummary
    transferringInputDeviceSummary_transferType,
    transferringInputDeviceSummary_id,
    transferringInputDeviceSummary_targetCustomerId,
    transferringInputDeviceSummary_message,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_styleControl,

    -- ** UdpContainerSettings
    udpContainerSettings_m2tsSettings,

    -- ** UdpGroupSettings
    udpGroupSettings_timedMetadataId3Period,
    udpGroupSettings_inputLossAction,
    udpGroupSettings_timedMetadataId3Frame,

    -- ** UdpOutputSettings
    udpOutputSettings_fecOutputSettings,
    udpOutputSettings_bufferMsec,
    udpOutputSettings_destination,
    udpOutputSettings_containerSettings,

    -- ** VideoBlackFailoverSettings
    videoBlackFailoverSettings_videoBlackThresholdMsec,
    videoBlackFailoverSettings_blackDetectThreshold,

    -- ** VideoCodecSettings
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_mpeg2Settings,

    -- ** VideoDescription
    videoDescription_height,
    videoDescription_sharpness,
    videoDescription_width,
    videoDescription_scalingBehavior,
    videoDescription_respondToAfd,
    videoDescription_codecSettings,
    videoDescription_name,

    -- ** VideoSelector
    videoSelector_selectorSettings,
    videoSelector_colorSpaceUsage,
    videoSelector_colorSpaceSettings,
    videoSelector_colorSpace,

    -- ** VideoSelectorColorSpaceSettings
    videoSelectorColorSpaceSettings_hdr10Settings,

    -- ** VideoSelectorPid
    videoSelectorPid_pid,

    -- ** VideoSelectorProgramId
    videoSelectorProgramId_programId,

    -- ** VideoSelectorSettings
    videoSelectorSettings_videoSelectorProgramId,
    videoSelectorSettings_videoSelectorPid,

    -- ** VpcOutputSettings
    vpcOutputSettings_securityGroupIds,
    vpcOutputSettings_publicAddressAllocationIds,
    vpcOutputSettings_subnetIds,

    -- ** VpcOutputSettingsDescription
    vpcOutputSettingsDescription_securityGroupIds,
    vpcOutputSettingsDescription_subnetIds,
    vpcOutputSettingsDescription_networkInterfaceIds,
    vpcOutputSettingsDescription_availabilityZones,

    -- ** WavSettings
    wavSettings_bitDepth,
    wavSettings_codingMode,
    wavSettings_sampleRate,

    -- ** WebvttDestinationSettings
    webvttDestinationSettings_styleControl,
  )
where

import Network.AWS.MediaLive.AcceptInputDeviceTransfer
import Network.AWS.MediaLive.BatchDelete
import Network.AWS.MediaLive.BatchStart
import Network.AWS.MediaLive.BatchStop
import Network.AWS.MediaLive.BatchUpdateSchedule
import Network.AWS.MediaLive.CancelInputDeviceTransfer
import Network.AWS.MediaLive.ClaimDevice
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
import Network.AWS.MediaLive.Types.ArchiveCdnSettings
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.ArchiveS3Settings
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.AudioChannelMapping
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescription
import Network.AWS.MediaLive.Types.AudioHlsRenditionSelection
import Network.AWS.MediaLive.Types.AudioLanguageSelection
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
import Network.AWS.MediaLive.Types.AudioPidSelection
import Network.AWS.MediaLive.Types.AudioSelector
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings
import Network.AWS.MediaLive.Types.AudioTrack
import Network.AWS.MediaLive.Types.AudioTrackSelection
import Network.AWS.MediaLive.Types.AudioWatermarkSettings
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
import Network.AWS.MediaLive.Types.CaptionRectangle
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
import Network.AWS.MediaLive.Types.FrameCaptureCdnSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureHlsSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureS3Settings
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
import Network.AWS.MediaLive.Types.HlsS3Settings
import Network.AWS.MediaLive.Types.HlsSettings
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import Network.AWS.MediaLive.Types.HtmlMotionGraphicsSettings
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
import Network.AWS.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings
import Network.AWS.MediaLive.Types.MotionGraphicsConfiguration
import Network.AWS.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings
import Network.AWS.MediaLive.Types.MotionGraphicsSettings
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
import Network.AWS.MediaLive.Types.NielsenCBET
import Network.AWS.MediaLive.Types.NielsenConfiguration
import Network.AWS.MediaLive.Types.NielsenNaesIiNw
import Network.AWS.MediaLive.Types.NielsenWatermarksSettings
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
import Network.AWS.MediaLive.Types.VideoSelectorColorSpaceSettings
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
import Network.AWS.MediaLive.Types.VideoSelectorSettings
import Network.AWS.MediaLive.Types.VpcOutputSettings
import Network.AWS.MediaLive.Types.VpcOutputSettingsDescription
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
