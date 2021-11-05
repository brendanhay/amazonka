{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Lens
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

import Amazonka.MediaLive.AcceptInputDeviceTransfer
import Amazonka.MediaLive.BatchDelete
import Amazonka.MediaLive.BatchStart
import Amazonka.MediaLive.BatchStop
import Amazonka.MediaLive.BatchUpdateSchedule
import Amazonka.MediaLive.CancelInputDeviceTransfer
import Amazonka.MediaLive.ClaimDevice
import Amazonka.MediaLive.CreateChannel
import Amazonka.MediaLive.CreateInput
import Amazonka.MediaLive.CreateInputSecurityGroup
import Amazonka.MediaLive.CreateMultiplex
import Amazonka.MediaLive.CreateMultiplexProgram
import Amazonka.MediaLive.CreatePartnerInput
import Amazonka.MediaLive.CreateTags
import Amazonka.MediaLive.DeleteChannel
import Amazonka.MediaLive.DeleteInput
import Amazonka.MediaLive.DeleteInputSecurityGroup
import Amazonka.MediaLive.DeleteMultiplex
import Amazonka.MediaLive.DeleteMultiplexProgram
import Amazonka.MediaLive.DeleteReservation
import Amazonka.MediaLive.DeleteSchedule
import Amazonka.MediaLive.DeleteTags
import Amazonka.MediaLive.DescribeChannel
import Amazonka.MediaLive.DescribeInput
import Amazonka.MediaLive.DescribeInputDevice
import Amazonka.MediaLive.DescribeInputDeviceThumbnail
import Amazonka.MediaLive.DescribeInputSecurityGroup
import Amazonka.MediaLive.DescribeMultiplex
import Amazonka.MediaLive.DescribeMultiplexProgram
import Amazonka.MediaLive.DescribeOffering
import Amazonka.MediaLive.DescribeReservation
import Amazonka.MediaLive.DescribeSchedule
import Amazonka.MediaLive.ListChannels
import Amazonka.MediaLive.ListInputDeviceTransfers
import Amazonka.MediaLive.ListInputDevices
import Amazonka.MediaLive.ListInputSecurityGroups
import Amazonka.MediaLive.ListInputs
import Amazonka.MediaLive.ListMultiplexPrograms
import Amazonka.MediaLive.ListMultiplexes
import Amazonka.MediaLive.ListOfferings
import Amazonka.MediaLive.ListReservations
import Amazonka.MediaLive.ListTagsForResource
import Amazonka.MediaLive.PurchaseOffering
import Amazonka.MediaLive.RejectInputDeviceTransfer
import Amazonka.MediaLive.StartChannel
import Amazonka.MediaLive.StartMultiplex
import Amazonka.MediaLive.StopChannel
import Amazonka.MediaLive.StopMultiplex
import Amazonka.MediaLive.TransferInputDevice
import Amazonka.MediaLive.Types.AacSettings
import Amazonka.MediaLive.Types.Ac3Settings
import Amazonka.MediaLive.Types.AncillarySourceSettings
import Amazonka.MediaLive.Types.ArchiveCdnSettings
import Amazonka.MediaLive.Types.ArchiveContainerSettings
import Amazonka.MediaLive.Types.ArchiveGroupSettings
import Amazonka.MediaLive.Types.ArchiveOutputSettings
import Amazonka.MediaLive.Types.ArchiveS3Settings
import Amazonka.MediaLive.Types.AribDestinationSettings
import Amazonka.MediaLive.Types.AribSourceSettings
import Amazonka.MediaLive.Types.AudioChannelMapping
import Amazonka.MediaLive.Types.AudioCodecSettings
import Amazonka.MediaLive.Types.AudioDescription
import Amazonka.MediaLive.Types.AudioHlsRenditionSelection
import Amazonka.MediaLive.Types.AudioLanguageSelection
import Amazonka.MediaLive.Types.AudioNormalizationSettings
import Amazonka.MediaLive.Types.AudioOnlyHlsSettings
import Amazonka.MediaLive.Types.AudioPidSelection
import Amazonka.MediaLive.Types.AudioSelector
import Amazonka.MediaLive.Types.AudioSelectorSettings
import Amazonka.MediaLive.Types.AudioSilenceFailoverSettings
import Amazonka.MediaLive.Types.AudioTrack
import Amazonka.MediaLive.Types.AudioTrackSelection
import Amazonka.MediaLive.Types.AudioWatermarkSettings
import Amazonka.MediaLive.Types.AutomaticInputFailoverSettings
import Amazonka.MediaLive.Types.AvailBlanking
import Amazonka.MediaLive.Types.AvailConfiguration
import Amazonka.MediaLive.Types.AvailSettings
import Amazonka.MediaLive.Types.BatchFailedResultModel
import Amazonka.MediaLive.Types.BatchScheduleActionCreateRequest
import Amazonka.MediaLive.Types.BatchScheduleActionCreateResult
import Amazonka.MediaLive.Types.BatchScheduleActionDeleteRequest
import Amazonka.MediaLive.Types.BatchScheduleActionDeleteResult
import Amazonka.MediaLive.Types.BatchSuccessfulResultModel
import Amazonka.MediaLive.Types.BlackoutSlate
import Amazonka.MediaLive.Types.BurnInDestinationSettings
import Amazonka.MediaLive.Types.CaptionDescription
import Amazonka.MediaLive.Types.CaptionDestinationSettings
import Amazonka.MediaLive.Types.CaptionLanguageMapping
import Amazonka.MediaLive.Types.CaptionRectangle
import Amazonka.MediaLive.Types.CaptionSelector
import Amazonka.MediaLive.Types.CaptionSelectorSettings
import Amazonka.MediaLive.Types.CdiInputSpecification
import Amazonka.MediaLive.Types.Channel
import Amazonka.MediaLive.Types.ChannelEgressEndpoint
import Amazonka.MediaLive.Types.ChannelSummary
import Amazonka.MediaLive.Types.ColorSpacePassthroughSettings
import Amazonka.MediaLive.Types.DvbNitSettings
import Amazonka.MediaLive.Types.DvbSdtSettings
import Amazonka.MediaLive.Types.DvbSubDestinationSettings
import Amazonka.MediaLive.Types.DvbSubSourceSettings
import Amazonka.MediaLive.Types.DvbTdtSettings
import Amazonka.MediaLive.Types.Eac3Settings
import Amazonka.MediaLive.Types.EbuTtDDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Amazonka.MediaLive.Types.EmbeddedSourceSettings
import Amazonka.MediaLive.Types.EncoderSettings
import Amazonka.MediaLive.Types.FailoverCondition
import Amazonka.MediaLive.Types.FailoverConditionSettings
import Amazonka.MediaLive.Types.FeatureActivations
import Amazonka.MediaLive.Types.FecOutputSettings
import Amazonka.MediaLive.Types.FixedModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.Fmp4HlsSettings
import Amazonka.MediaLive.Types.FollowModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.FrameCaptureCdnSettings
import Amazonka.MediaLive.Types.FrameCaptureGroupSettings
import Amazonka.MediaLive.Types.FrameCaptureHlsSettings
import Amazonka.MediaLive.Types.FrameCaptureOutputSettings
import Amazonka.MediaLive.Types.FrameCaptureS3Settings
import Amazonka.MediaLive.Types.FrameCaptureSettings
import Amazonka.MediaLive.Types.GlobalConfiguration
import Amazonka.MediaLive.Types.H264ColorSpaceSettings
import Amazonka.MediaLive.Types.H264FilterSettings
import Amazonka.MediaLive.Types.H264Settings
import Amazonka.MediaLive.Types.H265ColorSpaceSettings
import Amazonka.MediaLive.Types.H265FilterSettings
import Amazonka.MediaLive.Types.H265Settings
import Amazonka.MediaLive.Types.Hdr10Settings
import Amazonka.MediaLive.Types.HlsAkamaiSettings
import Amazonka.MediaLive.Types.HlsBasicPutSettings
import Amazonka.MediaLive.Types.HlsCdnSettings
import Amazonka.MediaLive.Types.HlsGroupSettings
import Amazonka.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Amazonka.MediaLive.Types.HlsInputSettings
import Amazonka.MediaLive.Types.HlsMediaStoreSettings
import Amazonka.MediaLive.Types.HlsOutputSettings
import Amazonka.MediaLive.Types.HlsS3Settings
import Amazonka.MediaLive.Types.HlsSettings
import Amazonka.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Amazonka.MediaLive.Types.HlsWebdavSettings
import Amazonka.MediaLive.Types.HtmlMotionGraphicsSettings
import Amazonka.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.Input
import Amazonka.MediaLive.Types.InputAttachment
import Amazonka.MediaLive.Types.InputChannelLevel
import Amazonka.MediaLive.Types.InputClippingSettings
import Amazonka.MediaLive.Types.InputDestination
import Amazonka.MediaLive.Types.InputDestinationRequest
import Amazonka.MediaLive.Types.InputDestinationVpc
import Amazonka.MediaLive.Types.InputDeviceConfigurableSettings
import Amazonka.MediaLive.Types.InputDeviceHdSettings
import Amazonka.MediaLive.Types.InputDeviceNetworkSettings
import Amazonka.MediaLive.Types.InputDeviceRequest
import Amazonka.MediaLive.Types.InputDeviceSettings
import Amazonka.MediaLive.Types.InputDeviceSummary
import Amazonka.MediaLive.Types.InputDeviceUhdSettings
import Amazonka.MediaLive.Types.InputLocation
import Amazonka.MediaLive.Types.InputLossBehavior
import Amazonka.MediaLive.Types.InputLossFailoverSettings
import Amazonka.MediaLive.Types.InputPrepareScheduleActionSettings
import Amazonka.MediaLive.Types.InputSecurityGroup
import Amazonka.MediaLive.Types.InputSettings
import Amazonka.MediaLive.Types.InputSource
import Amazonka.MediaLive.Types.InputSourceRequest
import Amazonka.MediaLive.Types.InputSpecification
import Amazonka.MediaLive.Types.InputSwitchScheduleActionSettings
import Amazonka.MediaLive.Types.InputVpcRequest
import Amazonka.MediaLive.Types.InputWhitelistRule
import Amazonka.MediaLive.Types.InputWhitelistRuleCidr
import Amazonka.MediaLive.Types.KeyProviderSettings
import Amazonka.MediaLive.Types.M2tsSettings
import Amazonka.MediaLive.Types.M3u8Settings
import Amazonka.MediaLive.Types.MediaConnectFlow
import Amazonka.MediaLive.Types.MediaConnectFlowRequest
import Amazonka.MediaLive.Types.MediaPackageGroupSettings
import Amazonka.MediaLive.Types.MediaPackageOutputDestinationSettings
import Amazonka.MediaLive.Types.MediaPackageOutputSettings
import Amazonka.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsConfiguration
import Amazonka.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsSettings
import Amazonka.MediaLive.Types.Mp2Settings
import Amazonka.MediaLive.Types.Mpeg2FilterSettings
import Amazonka.MediaLive.Types.Mpeg2Settings
import Amazonka.MediaLive.Types.MsSmoothGroupSettings
import Amazonka.MediaLive.Types.MsSmoothOutputSettings
import Amazonka.MediaLive.Types.Multiplex
import Amazonka.MediaLive.Types.MultiplexGroupSettings
import Amazonka.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import Amazonka.MediaLive.Types.MultiplexOutputDestination
import Amazonka.MediaLive.Types.MultiplexOutputSettings
import Amazonka.MediaLive.Types.MultiplexProgram
import Amazonka.MediaLive.Types.MultiplexProgramChannelDestinationSettings
import Amazonka.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Amazonka.MediaLive.Types.MultiplexProgramPipelineDetail
import Amazonka.MediaLive.Types.MultiplexProgramServiceDescriptor
import Amazonka.MediaLive.Types.MultiplexProgramSettings
import Amazonka.MediaLive.Types.MultiplexProgramSummary
import Amazonka.MediaLive.Types.MultiplexSettings
import Amazonka.MediaLive.Types.MultiplexSettingsSummary
import Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings
import Amazonka.MediaLive.Types.MultiplexSummary
import Amazonka.MediaLive.Types.MultiplexVideoSettings
import Amazonka.MediaLive.Types.NetworkInputSettings
import Amazonka.MediaLive.Types.NielsenCBET
import Amazonka.MediaLive.Types.NielsenConfiguration
import Amazonka.MediaLive.Types.NielsenNaesIiNw
import Amazonka.MediaLive.Types.NielsenWatermarksSettings
import Amazonka.MediaLive.Types.Offering
import Amazonka.MediaLive.Types.Output
import Amazonka.MediaLive.Types.OutputDestination
import Amazonka.MediaLive.Types.OutputDestinationSettings
import Amazonka.MediaLive.Types.OutputGroup
import Amazonka.MediaLive.Types.OutputGroupSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import Amazonka.MediaLive.Types.OutputSettings
import Amazonka.MediaLive.Types.PassThroughSettings
import Amazonka.MediaLive.Types.PauseStateScheduleActionSettings
import Amazonka.MediaLive.Types.PipelineDetail
import Amazonka.MediaLive.Types.PipelinePauseStateSettings
import Amazonka.MediaLive.Types.RawSettings
import Amazonka.MediaLive.Types.Rec601Settings
import Amazonka.MediaLive.Types.Rec709Settings
import Amazonka.MediaLive.Types.RemixSettings
import Amazonka.MediaLive.Types.Reservation
import Amazonka.MediaLive.Types.ReservationResourceSpecification
import Amazonka.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Amazonka.MediaLive.Types.RtmpGroupSettings
import Amazonka.MediaLive.Types.RtmpOutputSettings
import Amazonka.MediaLive.Types.ScheduleAction
import Amazonka.MediaLive.Types.ScheduleActionSettings
import Amazonka.MediaLive.Types.ScheduleActionStartSettings
import Amazonka.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Amazonka.MediaLive.Types.Scte20SourceSettings
import Amazonka.MediaLive.Types.Scte27DestinationSettings
import Amazonka.MediaLive.Types.Scte27SourceSettings
import Amazonka.MediaLive.Types.Scte35DeliveryRestrictions
import Amazonka.MediaLive.Types.Scte35Descriptor
import Amazonka.MediaLive.Types.Scte35DescriptorSettings
import Amazonka.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35SegmentationDescriptor
import Amazonka.MediaLive.Types.Scte35SpliceInsert
import Amazonka.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35TimeSignalApos
import Amazonka.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Amazonka.MediaLive.Types.SmpteTtDestinationSettings
import Amazonka.MediaLive.Types.StandardHlsSettings
import Amazonka.MediaLive.Types.StartTimecode
import Amazonka.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Amazonka.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import Amazonka.MediaLive.Types.StaticKeySettings
import Amazonka.MediaLive.Types.StopTimecode
import Amazonka.MediaLive.Types.TeletextDestinationSettings
import Amazonka.MediaLive.Types.TeletextSourceSettings
import Amazonka.MediaLive.Types.TemporalFilterSettings
import Amazonka.MediaLive.Types.TimecodeConfig
import Amazonka.MediaLive.Types.TransferringInputDeviceSummary
import Amazonka.MediaLive.Types.TtmlDestinationSettings
import Amazonka.MediaLive.Types.UdpContainerSettings
import Amazonka.MediaLive.Types.UdpGroupSettings
import Amazonka.MediaLive.Types.UdpOutputSettings
import Amazonka.MediaLive.Types.VideoBlackFailoverSettings
import Amazonka.MediaLive.Types.VideoCodecSettings
import Amazonka.MediaLive.Types.VideoDescription
import Amazonka.MediaLive.Types.VideoSelector
import Amazonka.MediaLive.Types.VideoSelectorColorSpaceSettings
import Amazonka.MediaLive.Types.VideoSelectorPid
import Amazonka.MediaLive.Types.VideoSelectorProgramId
import Amazonka.MediaLive.Types.VideoSelectorSettings
import Amazonka.MediaLive.Types.VpcOutputSettings
import Amazonka.MediaLive.Types.VpcOutputSettingsDescription
import Amazonka.MediaLive.Types.WavSettings
import Amazonka.MediaLive.Types.WebvttDestinationSettings
import Amazonka.MediaLive.UpdateChannel
import Amazonka.MediaLive.UpdateChannelClass
import Amazonka.MediaLive.UpdateInput
import Amazonka.MediaLive.UpdateInputDevice
import Amazonka.MediaLive.UpdateInputSecurityGroup
import Amazonka.MediaLive.UpdateMultiplex
import Amazonka.MediaLive.UpdateMultiplexProgram
import Amazonka.MediaLive.UpdateReservation
