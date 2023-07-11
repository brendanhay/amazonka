{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Lens
  ( -- * Operations

    -- ** AcceptInputDeviceTransfer
    acceptInputDeviceTransfer_inputDeviceId,
    acceptInputDeviceTransferResponse_httpStatus,

    -- ** BatchDelete
    batchDelete'_channelIds,
    batchDelete'_inputIds,
    batchDelete'_inputSecurityGroupIds,
    batchDelete'_multiplexIds,
    batchDeleteResponse_failed,
    batchDeleteResponse_successful,
    batchDeleteResponse_httpStatus,

    -- ** BatchStart
    batchStart'_channelIds,
    batchStart'_multiplexIds,
    batchStartResponse_failed,
    batchStartResponse_successful,
    batchStartResponse_httpStatus,

    -- ** BatchStop
    batchStop'_channelIds,
    batchStop'_multiplexIds,
    batchStopResponse_failed,
    batchStopResponse_successful,
    batchStopResponse_httpStatus,

    -- ** BatchUpdateSchedule
    batchUpdateSchedule_creates,
    batchUpdateSchedule_deletes,
    batchUpdateSchedule_channelId,
    batchUpdateScheduleResponse_creates,
    batchUpdateScheduleResponse_deletes,
    batchUpdateScheduleResponse_httpStatus,

    -- ** CancelInputDeviceTransfer
    cancelInputDeviceTransfer_inputDeviceId,
    cancelInputDeviceTransferResponse_httpStatus,

    -- ** ClaimDevice
    claimDevice_id,
    claimDeviceResponse_httpStatus,

    -- ** CreateChannel
    createChannel'_cdiInputSpecification,
    createChannel'_channelClass,
    createChannel'_destinations,
    createChannel'_encoderSettings,
    createChannel'_inputAttachments,
    createChannel'_inputSpecification,
    createChannel'_logLevel,
    createChannel'_maintenance,
    createChannel'_name,
    createChannel'_requestId,
    createChannel'_reserved,
    createChannel'_roleArn,
    createChannel'_tags,
    createChannel'_vpc,
    createChannelResponse_channel,
    createChannelResponse_httpStatus,

    -- ** CreateInput
    createInput'_destinations,
    createInput'_inputDevices,
    createInput'_inputSecurityGroups,
    createInput'_mediaConnectFlows,
    createInput'_name,
    createInput'_requestId,
    createInput'_roleArn,
    createInput'_sources,
    createInput'_tags,
    createInput'_type,
    createInput'_vpc,
    createInputResponse_input,
    createInputResponse_httpStatus,

    -- ** CreateInputSecurityGroup
    createInputSecurityGroup_tags,
    createInputSecurityGroup_whitelistRules,
    createInputSecurityGroupResponse_securityGroup,
    createInputSecurityGroupResponse_httpStatus,

    -- ** CreateMultiplex
    createMultiplex'_tags,
    createMultiplex'_requestId,
    createMultiplex'_multiplexSettings,
    createMultiplex'_availabilityZones,
    createMultiplex'_name,
    createMultiplexResponse_multiplex,
    createMultiplexResponse_httpStatus,

    -- ** CreateMultiplexProgram
    createMultiplexProgram'_multiplexId,
    createMultiplexProgram'_requestId,
    createMultiplexProgram'_multiplexProgramSettings,
    createMultiplexProgram'_programName,
    createMultiplexProgramResponse_multiplexProgram,
    createMultiplexProgramResponse_httpStatus,

    -- ** CreatePartnerInput
    createPartnerInput'_requestId,
    createPartnerInput'_tags,
    createPartnerInput'_inputId,
    createPartnerInputResponse_input,
    createPartnerInputResponse_httpStatus,

    -- ** CreateTags
    createTags_tags,
    createTags_resourceArn,

    -- ** DeleteChannel
    deleteChannel_channelId,
    deleteChannelResponse_arn,
    deleteChannelResponse_cdiInputSpecification,
    deleteChannelResponse_channelClass,
    deleteChannelResponse_destinations,
    deleteChannelResponse_egressEndpoints,
    deleteChannelResponse_encoderSettings,
    deleteChannelResponse_id,
    deleteChannelResponse_inputAttachments,
    deleteChannelResponse_inputSpecification,
    deleteChannelResponse_logLevel,
    deleteChannelResponse_maintenance,
    deleteChannelResponse_name,
    deleteChannelResponse_pipelineDetails,
    deleteChannelResponse_pipelinesRunningCount,
    deleteChannelResponse_roleArn,
    deleteChannelResponse_state,
    deleteChannelResponse_tags,
    deleteChannelResponse_vpc,
    deleteChannelResponse_httpStatus,

    -- ** DeleteInput
    deleteInput_inputId,
    deleteInputResponse_httpStatus,

    -- ** DeleteInputSecurityGroup
    deleteInputSecurityGroup_inputSecurityGroupId,
    deleteInputSecurityGroupResponse_httpStatus,

    -- ** DeleteMultiplex
    deleteMultiplex_multiplexId,
    deleteMultiplexResponse_arn,
    deleteMultiplexResponse_availabilityZones,
    deleteMultiplexResponse_destinations,
    deleteMultiplexResponse_id,
    deleteMultiplexResponse_multiplexSettings,
    deleteMultiplexResponse_name,
    deleteMultiplexResponse_pipelinesRunningCount,
    deleteMultiplexResponse_programCount,
    deleteMultiplexResponse_state,
    deleteMultiplexResponse_tags,
    deleteMultiplexResponse_httpStatus,

    -- ** DeleteMultiplexProgram
    deleteMultiplexProgram_multiplexId,
    deleteMultiplexProgram_programName,
    deleteMultiplexProgramResponse_channelId,
    deleteMultiplexProgramResponse_multiplexProgramSettings,
    deleteMultiplexProgramResponse_packetIdentifiersMap,
    deleteMultiplexProgramResponse_pipelineDetails,
    deleteMultiplexProgramResponse_programName,
    deleteMultiplexProgramResponse_httpStatus,

    -- ** DeleteReservation
    deleteReservation_reservationId,
    deleteReservationResponse_arn,
    deleteReservationResponse_count,
    deleteReservationResponse_currencyCode,
    deleteReservationResponse_duration,
    deleteReservationResponse_durationUnits,
    deleteReservationResponse_end,
    deleteReservationResponse_fixedPrice,
    deleteReservationResponse_name,
    deleteReservationResponse_offeringDescription,
    deleteReservationResponse_offeringId,
    deleteReservationResponse_offeringType,
    deleteReservationResponse_region,
    deleteReservationResponse_renewalSettings,
    deleteReservationResponse_reservationId,
    deleteReservationResponse_resourceSpecification,
    deleteReservationResponse_start,
    deleteReservationResponse_state,
    deleteReservationResponse_tags,
    deleteReservationResponse_usagePrice,
    deleteReservationResponse_httpStatus,

    -- ** DeleteSchedule
    deleteSchedule_channelId,
    deleteScheduleResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- ** DescribeChannel
    describeChannel_channelId,
    describeChannelResponse_arn,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_channelClass,
    describeChannelResponse_destinations,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_id,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_logLevel,
    describeChannelResponse_maintenance,
    describeChannelResponse_name,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_roleArn,
    describeChannelResponse_state,
    describeChannelResponse_tags,
    describeChannelResponse_vpc,
    describeChannelResponse_httpStatus,

    -- ** DescribeInput
    describeInput_inputId,
    describeInputResponse_arn,
    describeInputResponse_attachedChannels,
    describeInputResponse_destinations,
    describeInputResponse_id,
    describeInputResponse_inputClass,
    describeInputResponse_inputDevices,
    describeInputResponse_inputPartnerIds,
    describeInputResponse_inputSourceType,
    describeInputResponse_mediaConnectFlows,
    describeInputResponse_name,
    describeInputResponse_roleArn,
    describeInputResponse_securityGroups,
    describeInputResponse_sources,
    describeInputResponse_state,
    describeInputResponse_tags,
    describeInputResponse_type,
    describeInputResponse_httpStatus,

    -- ** DescribeInputDevice
    describeInputDevice_inputDeviceId,
    describeInputDeviceResponse_arn,
    describeInputDeviceResponse_connectionState,
    describeInputDeviceResponse_deviceSettingsSyncState,
    describeInputDeviceResponse_deviceUpdateStatus,
    describeInputDeviceResponse_hdDeviceSettings,
    describeInputDeviceResponse_id,
    describeInputDeviceResponse_macAddress,
    describeInputDeviceResponse_name,
    describeInputDeviceResponse_networkSettings,
    describeInputDeviceResponse_serialNumber,
    describeInputDeviceResponse_type,
    describeInputDeviceResponse_uhdDeviceSettings,
    describeInputDeviceResponse_httpStatus,

    -- ** DescribeInputDeviceThumbnail
    describeInputDeviceThumbnail_inputDeviceId,
    describeInputDeviceThumbnail_accept,
    describeInputDeviceThumbnailResponse_contentLength,
    describeInputDeviceThumbnailResponse_contentType,
    describeInputDeviceThumbnailResponse_eTag,
    describeInputDeviceThumbnailResponse_lastModified,
    describeInputDeviceThumbnailResponse_httpStatus,
    describeInputDeviceThumbnailResponse_body,

    -- ** DescribeInputSecurityGroup
    describeInputSecurityGroup_inputSecurityGroupId,
    describeInputSecurityGroupResponse_arn,
    describeInputSecurityGroupResponse_id,
    describeInputSecurityGroupResponse_inputs,
    describeInputSecurityGroupResponse_state,
    describeInputSecurityGroupResponse_tags,
    describeInputSecurityGroupResponse_whitelistRules,
    describeInputSecurityGroupResponse_httpStatus,

    -- ** DescribeMultiplex
    describeMultiplex_multiplexId,
    describeMultiplexResponse_arn,
    describeMultiplexResponse_availabilityZones,
    describeMultiplexResponse_destinations,
    describeMultiplexResponse_id,
    describeMultiplexResponse_multiplexSettings,
    describeMultiplexResponse_name,
    describeMultiplexResponse_pipelinesRunningCount,
    describeMultiplexResponse_programCount,
    describeMultiplexResponse_state,
    describeMultiplexResponse_tags,
    describeMultiplexResponse_httpStatus,

    -- ** DescribeMultiplexProgram
    describeMultiplexProgram_multiplexId,
    describeMultiplexProgram_programName,
    describeMultiplexProgramResponse_channelId,
    describeMultiplexProgramResponse_multiplexProgramSettings,
    describeMultiplexProgramResponse_packetIdentifiersMap,
    describeMultiplexProgramResponse_pipelineDetails,
    describeMultiplexProgramResponse_programName,
    describeMultiplexProgramResponse_httpStatus,

    -- ** DescribeOffering
    describeOffering_offeringId,
    describeOfferingResponse_arn,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_duration,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_region,
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_httpStatus,

    -- ** DescribeReservation
    describeReservation_reservationId,
    describeReservationResponse_arn,
    describeReservationResponse_count,
    describeReservationResponse_currencyCode,
    describeReservationResponse_duration,
    describeReservationResponse_durationUnits,
    describeReservationResponse_end,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_name,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_offeringId,
    describeReservationResponse_offeringType,
    describeReservationResponse_region,
    describeReservationResponse_renewalSettings,
    describeReservationResponse_reservationId,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_start,
    describeReservationResponse_state,
    describeReservationResponse_tags,
    describeReservationResponse_usagePrice,
    describeReservationResponse_httpStatus,

    -- ** DescribeSchedule
    describeSchedule_maxResults,
    describeSchedule_nextToken,
    describeSchedule_channelId,
    describeScheduleResponse_nextToken,
    describeScheduleResponse_scheduleActions,
    describeScheduleResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListInputDeviceTransfers
    listInputDeviceTransfers_maxResults,
    listInputDeviceTransfers_nextToken,
    listInputDeviceTransfers_transferType,
    listInputDeviceTransfersResponse_inputDeviceTransfers,
    listInputDeviceTransfersResponse_nextToken,
    listInputDeviceTransfersResponse_httpStatus,

    -- ** ListInputDevices
    listInputDevices_maxResults,
    listInputDevices_nextToken,
    listInputDevicesResponse_inputDevices,
    listInputDevicesResponse_nextToken,
    listInputDevicesResponse_httpStatus,

    -- ** ListInputSecurityGroups
    listInputSecurityGroups_maxResults,
    listInputSecurityGroups_nextToken,
    listInputSecurityGroupsResponse_inputSecurityGroups,
    listInputSecurityGroupsResponse_nextToken,
    listInputSecurityGroupsResponse_httpStatus,

    -- ** ListInputs
    listInputs_maxResults,
    listInputs_nextToken,
    listInputsResponse_inputs,
    listInputsResponse_nextToken,
    listInputsResponse_httpStatus,

    -- ** ListMultiplexPrograms
    listMultiplexPrograms_maxResults,
    listMultiplexPrograms_nextToken,
    listMultiplexPrograms_multiplexId,
    listMultiplexProgramsResponse_multiplexPrograms,
    listMultiplexProgramsResponse_nextToken,
    listMultiplexProgramsResponse_httpStatus,

    -- ** ListMultiplexes
    listMultiplexes_maxResults,
    listMultiplexes_nextToken,
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_channelClass,
    listOfferings_channelConfiguration,
    listOfferings_codec,
    listOfferings_duration,
    listOfferings_maxResults,
    listOfferings_maximumBitrate,
    listOfferings_maximumFramerate,
    listOfferings_nextToken,
    listOfferings_resolution,
    listOfferings_resourceType,
    listOfferings_specialFeature,
    listOfferings_videoQuality,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** ListReservations
    listReservations_channelClass,
    listReservations_codec,
    listReservations_maxResults,
    listReservations_maximumBitrate,
    listReservations_maximumFramerate,
    listReservations_nextToken,
    listReservations_resolution,
    listReservations_resourceType,
    listReservations_specialFeature,
    listReservations_videoQuality,
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering'_name,
    purchaseOffering'_renewalSettings,
    purchaseOffering'_requestId,
    purchaseOffering'_start,
    purchaseOffering'_tags,
    purchaseOffering'_offeringId,
    purchaseOffering'_count,
    purchaseOfferingResponse_reservation,
    purchaseOfferingResponse_httpStatus,

    -- ** RebootInputDevice
    rebootInputDevice'_force,
    rebootInputDevice'_inputDeviceId,
    rebootInputDeviceResponse_httpStatus,

    -- ** RejectInputDeviceTransfer
    rejectInputDeviceTransfer_inputDeviceId,
    rejectInputDeviceTransferResponse_httpStatus,

    -- ** StartChannel
    startChannel_channelId,
    startChannelResponse_arn,
    startChannelResponse_cdiInputSpecification,
    startChannelResponse_channelClass,
    startChannelResponse_destinations,
    startChannelResponse_egressEndpoints,
    startChannelResponse_encoderSettings,
    startChannelResponse_id,
    startChannelResponse_inputAttachments,
    startChannelResponse_inputSpecification,
    startChannelResponse_logLevel,
    startChannelResponse_maintenance,
    startChannelResponse_name,
    startChannelResponse_pipelineDetails,
    startChannelResponse_pipelinesRunningCount,
    startChannelResponse_roleArn,
    startChannelResponse_state,
    startChannelResponse_tags,
    startChannelResponse_vpc,
    startChannelResponse_httpStatus,

    -- ** StartInputDeviceMaintenanceWindow
    startInputDeviceMaintenanceWindow_inputDeviceId,
    startInputDeviceMaintenanceWindowResponse_httpStatus,

    -- ** StartMultiplex
    startMultiplex_multiplexId,
    startMultiplexResponse_arn,
    startMultiplexResponse_availabilityZones,
    startMultiplexResponse_destinations,
    startMultiplexResponse_id,
    startMultiplexResponse_multiplexSettings,
    startMultiplexResponse_name,
    startMultiplexResponse_pipelinesRunningCount,
    startMultiplexResponse_programCount,
    startMultiplexResponse_state,
    startMultiplexResponse_tags,
    startMultiplexResponse_httpStatus,

    -- ** StopChannel
    stopChannel_channelId,
    stopChannelResponse_arn,
    stopChannelResponse_cdiInputSpecification,
    stopChannelResponse_channelClass,
    stopChannelResponse_destinations,
    stopChannelResponse_egressEndpoints,
    stopChannelResponse_encoderSettings,
    stopChannelResponse_id,
    stopChannelResponse_inputAttachments,
    stopChannelResponse_inputSpecification,
    stopChannelResponse_logLevel,
    stopChannelResponse_maintenance,
    stopChannelResponse_name,
    stopChannelResponse_pipelineDetails,
    stopChannelResponse_pipelinesRunningCount,
    stopChannelResponse_roleArn,
    stopChannelResponse_state,
    stopChannelResponse_tags,
    stopChannelResponse_vpc,
    stopChannelResponse_httpStatus,

    -- ** StopMultiplex
    stopMultiplex_multiplexId,
    stopMultiplexResponse_arn,
    stopMultiplexResponse_availabilityZones,
    stopMultiplexResponse_destinations,
    stopMultiplexResponse_id,
    stopMultiplexResponse_multiplexSettings,
    stopMultiplexResponse_name,
    stopMultiplexResponse_pipelinesRunningCount,
    stopMultiplexResponse_programCount,
    stopMultiplexResponse_state,
    stopMultiplexResponse_tags,
    stopMultiplexResponse_httpStatus,

    -- ** TransferInputDevice
    transferInputDevice'_targetCustomerId,
    transferInputDevice'_targetRegion,
    transferInputDevice'_transferMessage,
    transferInputDevice'_inputDeviceId,
    transferInputDeviceResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel'_cdiInputSpecification,
    updateChannel'_destinations,
    updateChannel'_encoderSettings,
    updateChannel'_inputAttachments,
    updateChannel'_inputSpecification,
    updateChannel'_logLevel,
    updateChannel'_maintenance,
    updateChannel'_name,
    updateChannel'_roleArn,
    updateChannel'_channelId,
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,

    -- ** UpdateChannelClass
    updateChannelClass'_destinations,
    updateChannelClass'_channelId,
    updateChannelClass'_channelClass,
    updateChannelClassResponse_channel,
    updateChannelClassResponse_httpStatus,

    -- ** UpdateInput
    updateInput'_destinations,
    updateInput'_inputDevices,
    updateInput'_inputSecurityGroups,
    updateInput'_mediaConnectFlows,
    updateInput'_name,
    updateInput'_roleArn,
    updateInput'_sources,
    updateInput'_inputId,
    updateInputResponse_input,
    updateInputResponse_httpStatus,

    -- ** UpdateInputDevice
    updateInputDevice'_hdDeviceSettings,
    updateInputDevice'_name,
    updateInputDevice'_uhdDeviceSettings,
    updateInputDevice'_inputDeviceId,
    updateInputDeviceResponse_arn,
    updateInputDeviceResponse_connectionState,
    updateInputDeviceResponse_deviceSettingsSyncState,
    updateInputDeviceResponse_deviceUpdateStatus,
    updateInputDeviceResponse_hdDeviceSettings,
    updateInputDeviceResponse_id,
    updateInputDeviceResponse_macAddress,
    updateInputDeviceResponse_name,
    updateInputDeviceResponse_networkSettings,
    updateInputDeviceResponse_serialNumber,
    updateInputDeviceResponse_type,
    updateInputDeviceResponse_uhdDeviceSettings,
    updateInputDeviceResponse_httpStatus,

    -- ** UpdateInputSecurityGroup
    updateInputSecurityGroup_tags,
    updateInputSecurityGroup_whitelistRules,
    updateInputSecurityGroup_inputSecurityGroupId,
    updateInputSecurityGroupResponse_securityGroup,
    updateInputSecurityGroupResponse_httpStatus,

    -- ** UpdateMultiplex
    updateMultiplex'_multiplexSettings,
    updateMultiplex'_name,
    updateMultiplex'_multiplexId,
    updateMultiplexResponse_multiplex,
    updateMultiplexResponse_httpStatus,

    -- ** UpdateMultiplexProgram
    updateMultiplexProgram'_multiplexProgramSettings,
    updateMultiplexProgram'_multiplexId,
    updateMultiplexProgram'_programName,
    updateMultiplexProgramResponse_multiplexProgram,
    updateMultiplexProgramResponse_httpStatus,

    -- ** UpdateReservation
    updateReservation'_name,
    updateReservation'_renewalSettings,
    updateReservation'_reservationId,
    updateReservationResponse_reservation,
    updateReservationResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_bitrate,
    aacSettings_codingMode,
    aacSettings_inputType,
    aacSettings_profile,
    aacSettings_rateControlMode,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_spec,
    aacSettings_vbrQuality,

    -- ** Ac3Settings
    ac3Settings_bitrate,
    ac3Settings_bitstreamMode,
    ac3Settings_codingMode,
    ac3Settings_dialnorm,
    ac3Settings_drcProfile,
    ac3Settings_lfeFilter,
    ac3Settings_metadataControl,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** ArchiveCdnSettings
    archiveCdnSettings_archiveS3Settings,

    -- ** ArchiveContainerSettings
    archiveContainerSettings_m2tsSettings,
    archiveContainerSettings_rawSettings,

    -- ** ArchiveGroupSettings
    archiveGroupSettings_archiveCdnSettings,
    archiveGroupSettings_rolloverInterval,
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
    audioCodecSettings_aacSettings,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_eac3AtmosSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_passThroughSettings,
    audioCodecSettings_wavSettings,

    -- ** AudioDescription
    audioDescription_audioNormalizationSettings,
    audioDescription_audioType,
    audioDescription_audioTypeControl,
    audioDescription_audioWatermarkingSettings,
    audioDescription_codecSettings,
    audioDescription_languageCode,
    audioDescription_languageCodeControl,
    audioDescription_remixSettings,
    audioDescription_streamName,
    audioDescription_audioSelectorName,
    audioDescription_name,

    -- ** AudioDolbyEDecode
    audioDolbyEDecode_programSelection,

    -- ** AudioHlsRenditionSelection
    audioHlsRenditionSelection_name,
    audioHlsRenditionSelection_groupId,

    -- ** AudioLanguageSelection
    audioLanguageSelection_languageSelectionPolicy,
    audioLanguageSelection_languageCode,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_targetLkfs,

    -- ** AudioOnlyHlsSettings
    audioOnlyHlsSettings_audioGroupId,
    audioOnlyHlsSettings_audioOnlyImage,
    audioOnlyHlsSettings_audioTrackType,
    audioOnlyHlsSettings_segmentType,

    -- ** AudioPidSelection
    audioPidSelection_pid,

    -- ** AudioSelector
    audioSelector_selectorSettings,
    audioSelector_name,

    -- ** AudioSelectorSettings
    audioSelectorSettings_audioHlsRenditionSelection,
    audioSelectorSettings_audioLanguageSelection,
    audioSelectorSettings_audioPidSelection,
    audioSelectorSettings_audioTrackSelection,

    -- ** AudioSilenceFailoverSettings
    audioSilenceFailoverSettings_audioSilenceThresholdMsec,
    audioSilenceFailoverSettings_audioSelectorName,

    -- ** AudioTrack
    audioTrack_track,

    -- ** AudioTrackSelection
    audioTrackSelection_dolbyEDecode,
    audioTrackSelection_tracks,

    -- ** AudioWatermarkSettings
    audioWatermarkSettings_nielsenWatermarksSettings,

    -- ** AutomaticInputFailoverSettings
    automaticInputFailoverSettings_errorClearTimeMsec,
    automaticInputFailoverSettings_failoverConditions,
    automaticInputFailoverSettings_inputPreference,
    automaticInputFailoverSettings_secondaryInputId,

    -- ** AvailBlanking
    availBlanking_availBlankingImage,
    availBlanking_state,

    -- ** AvailConfiguration
    availConfiguration_availSettings,

    -- ** AvailSettings
    availSettings_esam,
    availSettings_scte35SpliceInsert,
    availSettings_scte35TimeSignalApos,

    -- ** BatchFailedResultModel
    batchFailedResultModel_arn,
    batchFailedResultModel_code,
    batchFailedResultModel_id,
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
    batchSuccessfulResultModel_arn,
    batchSuccessfulResultModel_id,
    batchSuccessfulResultModel_state,

    -- ** BlackoutSlate
    blackoutSlate_blackoutSlateImage,
    blackoutSlate_networkEndBlackout,
    blackoutSlate_networkEndBlackoutImage,
    blackoutSlate_networkId,
    blackoutSlate_state,

    -- ** BurnInDestinationSettings
    burnInDestinationSettings_alignment,
    burnInDestinationSettings_backgroundColor,
    burnInDestinationSettings_backgroundOpacity,
    burnInDestinationSettings_font,
    burnInDestinationSettings_fontColor,
    burnInDestinationSettings_fontOpacity,
    burnInDestinationSettings_fontResolution,
    burnInDestinationSettings_fontSize,
    burnInDestinationSettings_outlineColor,
    burnInDestinationSettings_outlineSize,
    burnInDestinationSettings_shadowColor,
    burnInDestinationSettings_shadowOpacity,
    burnInDestinationSettings_shadowXOffset,
    burnInDestinationSettings_shadowYOffset,
    burnInDestinationSettings_teletextGridControl,
    burnInDestinationSettings_xPosition,
    burnInDestinationSettings_yPosition,

    -- ** CaptionDescription
    captionDescription_accessibility,
    captionDescription_destinationSettings,
    captionDescription_languageCode,
    captionDescription_languageDescription,
    captionDescription_captionSelectorName,
    captionDescription_name,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_aribDestinationSettings,
    captionDestinationSettings_burnInDestinationSettings,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_ebuTtDDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_embeddedPlusScte20DestinationSettings,
    captionDestinationSettings_rtmpCaptionInfoDestinationSettings,
    captionDestinationSettings_scte20PlusEmbeddedDestinationSettings,
    captionDestinationSettings_scte27DestinationSettings,
    captionDestinationSettings_smpteTtDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_webvttDestinationSettings,

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
    captionSelectorSettings_ancillarySourceSettings,
    captionSelectorSettings_aribSourceSettings,
    captionSelectorSettings_dvbSubSourceSettings,
    captionSelectorSettings_embeddedSourceSettings,
    captionSelectorSettings_scte20SourceSettings,
    captionSelectorSettings_scte27SourceSettings,
    captionSelectorSettings_teletextSourceSettings,

    -- ** CdiInputSpecification
    cdiInputSpecification_resolution,

    -- ** Channel
    channel_arn,
    channel_cdiInputSpecification,
    channel_channelClass,
    channel_destinations,
    channel_egressEndpoints,
    channel_encoderSettings,
    channel_id,
    channel_inputAttachments,
    channel_inputSpecification,
    channel_logLevel,
    channel_maintenance,
    channel_name,
    channel_pipelineDetails,
    channel_pipelinesRunningCount,
    channel_roleArn,
    channel_state,
    channel_tags,
    channel_vpc,

    -- ** ChannelEgressEndpoint
    channelEgressEndpoint_sourceIp,

    -- ** ChannelSummary
    channelSummary_arn,
    channelSummary_cdiInputSpecification,
    channelSummary_channelClass,
    channelSummary_destinations,
    channelSummary_egressEndpoints,
    channelSummary_id,
    channelSummary_inputAttachments,
    channelSummary_inputSpecification,
    channelSummary_logLevel,
    channelSummary_maintenance,
    channelSummary_name,
    channelSummary_pipelinesRunningCount,
    channelSummary_roleArn,
    channelSummary_state,
    channelSummary_tags,
    channelSummary_vpc,

    -- ** ColorSpacePassthroughSettings

    -- ** DolbyVision81Settings

    -- ** DvbNitSettings
    dvbNitSettings_repInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- ** DvbSdtSettings
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_repInterval,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_serviceProviderName,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_font,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_shadowYOffset,
    dvbSubDestinationSettings_teletextGridControl,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_yPosition,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_ocrLanguage,
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_repInterval,

    -- ** Eac3AtmosSettings
    eac3AtmosSettings_bitrate,
    eac3AtmosSettings_codingMode,
    eac3AtmosSettings_dialnorm,
    eac3AtmosSettings_drcLine,
    eac3AtmosSettings_drcRf,
    eac3AtmosSettings_heightTrim,
    eac3AtmosSettings_surroundTrim,

    -- ** Eac3Settings
    eac3Settings_attenuationControl,
    eac3Settings_bitrate,
    eac3Settings_bitstreamMode,
    eac3Settings_codingMode,
    eac3Settings_dcFilter,
    eac3Settings_dialnorm,
    eac3Settings_drcLine,
    eac3Settings_drcRf,
    eac3Settings_lfeControl,
    eac3Settings_lfeFilter,
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_metadataControl,
    eac3Settings_passthroughControl,
    eac3Settings_phaseControl,
    eac3Settings_stereoDownmix,
    eac3Settings_surroundExMode,
    eac3Settings_surroundMode,

    -- ** EbuTtDDestinationSettings
    ebuTtDDestinationSettings_copyrightHolder,
    ebuTtDDestinationSettings_fillLineGap,
    ebuTtDDestinationSettings_fontFamily,
    ebuTtDDestinationSettings_styleControl,

    -- ** EmbeddedDestinationSettings

    -- ** EmbeddedPlusScte20DestinationSettings

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_scte20Detection,
    embeddedSourceSettings_source608ChannelNumber,
    embeddedSourceSettings_source608TrackNumber,

    -- ** EncoderSettings
    encoderSettings_availBlanking,
    encoderSettings_availConfiguration,
    encoderSettings_blackoutSlate,
    encoderSettings_captionDescriptions,
    encoderSettings_featureActivations,
    encoderSettings_globalConfiguration,
    encoderSettings_motionGraphicsConfiguration,
    encoderSettings_nielsenConfiguration,
    encoderSettings_videoDescriptions,
    encoderSettings_audioDescriptions,
    encoderSettings_outputGroups,
    encoderSettings_timecodeConfig,

    -- ** Esam
    esam_adAvailOffset,
    esam_passwordParam,
    esam_username,
    esam_zoneIdentity,
    esam_acquisitionPointId,
    esam_poisEndpoint,

    -- ** FailoverCondition
    failoverCondition_failoverConditionSettings,

    -- ** FailoverConditionSettings
    failoverConditionSettings_audioSilenceSettings,
    failoverConditionSettings_inputLossSettings,
    failoverConditionSettings_videoBlackSettings,

    -- ** FeatureActivations
    featureActivations_inputPrepareScheduleActions,

    -- ** FecOutputSettings
    fecOutputSettings_columnDepth,
    fecOutputSettings_includeFec,
    fecOutputSettings_rowLength,

    -- ** FixedModeScheduleActionStartSettings
    fixedModeScheduleActionStartSettings_time,

    -- ** Fmp4HlsSettings
    fmp4HlsSettings_audioRenditionSets,
    fmp4HlsSettings_nielsenId3Behavior,
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
    frameCaptureSettings_captureInterval,
    frameCaptureSettings_captureIntervalUnits,
    frameCaptureSettings_timecodeBurninSettings,

    -- ** GlobalConfiguration
    globalConfiguration_initialAudioGain,
    globalConfiguration_inputEndAction,
    globalConfiguration_inputLossBehavior,
    globalConfiguration_outputLockingMode,
    globalConfiguration_outputTimingSource,
    globalConfiguration_supportLowFramerateInputs,

    -- ** H264ColorSpaceSettings
    h264ColorSpaceSettings_colorSpacePassthroughSettings,
    h264ColorSpaceSettings_rec601Settings,
    h264ColorSpaceSettings_rec709Settings,

    -- ** H264FilterSettings
    h264FilterSettings_temporalFilterSettings,

    -- ** H264Settings
    h264Settings_adaptiveQuantization,
    h264Settings_afdSignaling,
    h264Settings_bitrate,
    h264Settings_bufFillPct,
    h264Settings_bufSize,
    h264Settings_colorMetadata,
    h264Settings_colorSpaceSettings,
    h264Settings_entropyEncoding,
    h264Settings_filterSettings,
    h264Settings_fixedAfd,
    h264Settings_flickerAq,
    h264Settings_forceFieldPictures,
    h264Settings_framerateControl,
    h264Settings_framerateDenominator,
    h264Settings_framerateNumerator,
    h264Settings_gopBReference,
    h264Settings_gopClosedCadence,
    h264Settings_gopNumBFrames,
    h264Settings_gopSize,
    h264Settings_gopSizeUnits,
    h264Settings_level,
    h264Settings_lookAheadRateControl,
    h264Settings_maxBitrate,
    h264Settings_minIInterval,
    h264Settings_numRefFrames,
    h264Settings_parControl,
    h264Settings_parDenominator,
    h264Settings_parNumerator,
    h264Settings_profile,
    h264Settings_qualityLevel,
    h264Settings_qvbrQualityLevel,
    h264Settings_rateControlMode,
    h264Settings_scanType,
    h264Settings_sceneChangeDetect,
    h264Settings_slices,
    h264Settings_softness,
    h264Settings_spatialAq,
    h264Settings_subgopLength,
    h264Settings_syntax,
    h264Settings_temporalAq,
    h264Settings_timecodeBurninSettings,
    h264Settings_timecodeInsertion,

    -- ** H265ColorSpaceSettings
    h265ColorSpaceSettings_colorSpacePassthroughSettings,
    h265ColorSpaceSettings_dolbyVision81Settings,
    h265ColorSpaceSettings_hdr10Settings,
    h265ColorSpaceSettings_rec601Settings,
    h265ColorSpaceSettings_rec709Settings,

    -- ** H265FilterSettings
    h265FilterSettings_temporalFilterSettings,

    -- ** H265Settings
    h265Settings_adaptiveQuantization,
    h265Settings_afdSignaling,
    h265Settings_alternativeTransferFunction,
    h265Settings_bitrate,
    h265Settings_bufSize,
    h265Settings_colorMetadata,
    h265Settings_colorSpaceSettings,
    h265Settings_filterSettings,
    h265Settings_fixedAfd,
    h265Settings_flickerAq,
    h265Settings_gopClosedCadence,
    h265Settings_gopSize,
    h265Settings_gopSizeUnits,
    h265Settings_level,
    h265Settings_lookAheadRateControl,
    h265Settings_maxBitrate,
    h265Settings_minIInterval,
    h265Settings_parDenominator,
    h265Settings_parNumerator,
    h265Settings_profile,
    h265Settings_qvbrQualityLevel,
    h265Settings_rateControlMode,
    h265Settings_scanType,
    h265Settings_sceneChangeDetect,
    h265Settings_slices,
    h265Settings_tier,
    h265Settings_timecodeBurninSettings,
    h265Settings_timecodeInsertion,
    h265Settings_framerateNumerator,
    h265Settings_framerateDenominator,

    -- ** Hdr10Settings
    hdr10Settings_maxCll,
    hdr10Settings_maxFall,

    -- ** HlsAkamaiSettings
    hlsAkamaiSettings_connectionRetryInterval,
    hlsAkamaiSettings_filecacheDuration,
    hlsAkamaiSettings_httpTransferMode,
    hlsAkamaiSettings_numRetries,
    hlsAkamaiSettings_restartDelay,
    hlsAkamaiSettings_salt,
    hlsAkamaiSettings_token,

    -- ** HlsBasicPutSettings
    hlsBasicPutSettings_connectionRetryInterval,
    hlsBasicPutSettings_filecacheDuration,
    hlsBasicPutSettings_numRetries,
    hlsBasicPutSettings_restartDelay,

    -- ** HlsCdnSettings
    hlsCdnSettings_hlsAkamaiSettings,
    hlsCdnSettings_hlsBasicPutSettings,
    hlsCdnSettings_hlsMediaStoreSettings,
    hlsCdnSettings_hlsS3Settings,
    hlsCdnSettings_hlsWebdavSettings,

    -- ** HlsGroupSettings
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_baseUrlContent,
    hlsGroupSettings_baseUrlContent1,
    hlsGroupSettings_baseUrlManifest,
    hlsGroupSettings_baseUrlManifest1,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_constantIv,
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_discontinuityTags,
    hlsGroupSettings_encryptionType,
    hlsGroupSettings_hlsCdnSettings,
    hlsGroupSettings_hlsId3SegmentTagging,
    hlsGroupSettings_iFrameOnlyPlaylists,
    hlsGroupSettings_incompleteSegmentBehavior,
    hlsGroupSettings_indexNSegments,
    hlsGroupSettings_inputLossAction,
    hlsGroupSettings_ivInManifest,
    hlsGroupSettings_ivSource,
    hlsGroupSettings_keepSegments,
    hlsGroupSettings_keyFormat,
    hlsGroupSettings_keyFormatVersions,
    hlsGroupSettings_keyProviderSettings,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_mode,
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_programDateTimeClock,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_redundantManifest,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_segmentationMode,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_tsFileMode,
    hlsGroupSettings_destination,

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    hlsId3SegmentTaggingScheduleActionSettings_tag,

    -- ** HlsInputSettings
    hlsInputSettings_bandwidth,
    hlsInputSettings_bufferSegments,
    hlsInputSettings_retries,
    hlsInputSettings_retryInterval,
    hlsInputSettings_scte35Source,

    -- ** HlsMediaStoreSettings
    hlsMediaStoreSettings_connectionRetryInterval,
    hlsMediaStoreSettings_filecacheDuration,
    hlsMediaStoreSettings_mediaStoreStorageClass,
    hlsMediaStoreSettings_numRetries,
    hlsMediaStoreSettings_restartDelay,

    -- ** HlsOutputSettings
    hlsOutputSettings_h265PackagingType,
    hlsOutputSettings_nameModifier,
    hlsOutputSettings_segmentModifier,
    hlsOutputSettings_hlsSettings,

    -- ** HlsS3Settings
    hlsS3Settings_cannedAcl,

    -- ** HlsSettings
    hlsSettings_audioOnlyHlsSettings,
    hlsSettings_fmp4HlsSettings,
    hlsSettings_frameCaptureHlsSettings,
    hlsSettings_standardHlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    hlsTimedMetadataScheduleActionSettings_id3,

    -- ** HlsWebdavSettings
    hlsWebdavSettings_connectionRetryInterval,
    hlsWebdavSettings_filecacheDuration,
    hlsWebdavSettings_httpTransferMode,
    hlsWebdavSettings_numRetries,
    hlsWebdavSettings_restartDelay,

    -- ** HtmlMotionGraphicsSettings

    -- ** ImmediateModeScheduleActionStartSettings

    -- ** Input
    input_arn,
    input_attachedChannels,
    input_destinations,
    input_id,
    input_inputClass,
    input_inputDevices,
    input_inputPartnerIds,
    input_inputSourceType,
    input_mediaConnectFlows,
    input_name,
    input_roleArn,
    input_securityGroups,
    input_sources,
    input_state,
    input_tags,
    input_type,

    -- ** InputAttachment
    inputAttachment_automaticInputFailoverSettings,
    inputAttachment_inputAttachmentName,
    inputAttachment_inputId,
    inputAttachment_inputSettings,

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
    inputDeviceConfigurableSettings_latencyMs,
    inputDeviceConfigurableSettings_maxBitrate,

    -- ** InputDeviceHdSettings
    inputDeviceHdSettings_activeInput,
    inputDeviceHdSettings_configuredInput,
    inputDeviceHdSettings_deviceState,
    inputDeviceHdSettings_framerate,
    inputDeviceHdSettings_height,
    inputDeviceHdSettings_latencyMs,
    inputDeviceHdSettings_maxBitrate,
    inputDeviceHdSettings_scanType,
    inputDeviceHdSettings_width,

    -- ** InputDeviceNetworkSettings
    inputDeviceNetworkSettings_dnsAddresses,
    inputDeviceNetworkSettings_gateway,
    inputDeviceNetworkSettings_ipAddress,
    inputDeviceNetworkSettings_ipScheme,
    inputDeviceNetworkSettings_subnetMask,

    -- ** InputDeviceRequest
    inputDeviceRequest_id,

    -- ** InputDeviceSettings
    inputDeviceSettings_id,

    -- ** InputDeviceSummary
    inputDeviceSummary_arn,
    inputDeviceSummary_connectionState,
    inputDeviceSummary_deviceSettingsSyncState,
    inputDeviceSummary_deviceUpdateStatus,
    inputDeviceSummary_hdDeviceSettings,
    inputDeviceSummary_id,
    inputDeviceSummary_macAddress,
    inputDeviceSummary_name,
    inputDeviceSummary_networkSettings,
    inputDeviceSummary_serialNumber,
    inputDeviceSummary_type,
    inputDeviceSummary_uhdDeviceSettings,

    -- ** InputDeviceUhdSettings
    inputDeviceUhdSettings_activeInput,
    inputDeviceUhdSettings_configuredInput,
    inputDeviceUhdSettings_deviceState,
    inputDeviceUhdSettings_framerate,
    inputDeviceUhdSettings_height,
    inputDeviceUhdSettings_latencyMs,
    inputDeviceUhdSettings_maxBitrate,
    inputDeviceUhdSettings_scanType,
    inputDeviceUhdSettings_width,

    -- ** InputLocation
    inputLocation_passwordParam,
    inputLocation_username,
    inputLocation_uri,

    -- ** InputLossBehavior
    inputLossBehavior_blackFrameMsec,
    inputLossBehavior_inputLossImageColor,
    inputLossBehavior_inputLossImageSlate,
    inputLossBehavior_inputLossImageType,
    inputLossBehavior_repeatFrameMsec,

    -- ** InputLossFailoverSettings
    inputLossFailoverSettings_inputLossThresholdMsec,

    -- ** InputPrepareScheduleActionSettings
    inputPrepareScheduleActionSettings_inputAttachmentNameReference,
    inputPrepareScheduleActionSettings_inputClippingSettings,
    inputPrepareScheduleActionSettings_urlPath,

    -- ** InputSecurityGroup
    inputSecurityGroup_arn,
    inputSecurityGroup_id,
    inputSecurityGroup_inputs,
    inputSecurityGroup_state,
    inputSecurityGroup_tags,
    inputSecurityGroup_whitelistRules,

    -- ** InputSettings
    inputSettings_audioSelectors,
    inputSettings_captionSelectors,
    inputSettings_deblockFilter,
    inputSettings_denoiseFilter,
    inputSettings_filterStrength,
    inputSettings_inputFilter,
    inputSettings_networkInputSettings,
    inputSettings_scte35Pid,
    inputSettings_smpte2038DataPreference,
    inputSettings_sourceEndBehavior,
    inputSettings_videoSelector,

    -- ** InputSource
    inputSource_passwordParam,
    inputSource_url,
    inputSource_username,

    -- ** InputSourceRequest
    inputSourceRequest_passwordParam,
    inputSourceRequest_url,
    inputSourceRequest_username,

    -- ** InputSpecification
    inputSpecification_codec,
    inputSpecification_maximumBitrate,
    inputSpecification_resolution,

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
    m2tsSettings_absentInputAudioBehavior,
    m2tsSettings_arib,
    m2tsSettings_aribCaptionsPid,
    m2tsSettings_aribCaptionsPidControl,
    m2tsSettings_audioBufferModel,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_audioPids,
    m2tsSettings_audioStreamType,
    m2tsSettings_bitrate,
    m2tsSettings_bufferModel,
    m2tsSettings_ccDescriptor,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_dvbSubPids,
    m2tsSettings_dvbTdtSettings,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_ebif,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_ebpLookaheadMs,
    m2tsSettings_ebpPlacement,
    m2tsSettings_ecmPid,
    m2tsSettings_esRateInPes,
    m2tsSettings_etvPlatformPid,
    m2tsSettings_etvSignalPid,
    m2tsSettings_fragmentTime,
    m2tsSettings_klv,
    m2tsSettings_klvDataPids,
    m2tsSettings_nielsenId3Behavior,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_patInterval,
    m2tsSettings_pcrControl,
    m2tsSettings_pcrPeriod,
    m2tsSettings_pcrPid,
    m2tsSettings_pmtInterval,
    m2tsSettings_pmtPid,
    m2tsSettings_programNum,
    m2tsSettings_rateMode,
    m2tsSettings_scte27Pids,
    m2tsSettings_scte35Control,
    m2tsSettings_scte35Pid,
    m2tsSettings_segmentationMarkers,
    m2tsSettings_segmentationStyle,
    m2tsSettings_segmentationTime,
    m2tsSettings_timedMetadataBehavior,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_transportStreamId,
    m2tsSettings_videoPid,

    -- ** M3u8Settings
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_audioPids,
    m3u8Settings_ecmPid,
    m3u8Settings_nielsenId3Behavior,
    m3u8Settings_patInterval,
    m3u8Settings_pcrControl,
    m3u8Settings_pcrPeriod,
    m3u8Settings_pcrPid,
    m3u8Settings_pmtInterval,
    m3u8Settings_pmtPid,
    m3u8Settings_programNum,
    m3u8Settings_scte35Behavior,
    m3u8Settings_scte35Pid,
    m3u8Settings_timedMetadataBehavior,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_transportStreamId,
    m3u8Settings_videoPid,

    -- ** MaintenanceCreateSettings
    maintenanceCreateSettings_maintenanceDay,
    maintenanceCreateSettings_maintenanceStartTime,

    -- ** MaintenanceStatus
    maintenanceStatus_maintenanceDay,
    maintenanceStatus_maintenanceDeadline,
    maintenanceStatus_maintenanceScheduledDate,
    maintenanceStatus_maintenanceStartTime,

    -- ** MaintenanceUpdateSettings
    maintenanceUpdateSettings_maintenanceDay,
    maintenanceUpdateSettings_maintenanceScheduledDate,
    maintenanceUpdateSettings_maintenanceStartTime,

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
    motionGraphicsActivateScheduleActionSettings_duration,
    motionGraphicsActivateScheduleActionSettings_passwordParam,
    motionGraphicsActivateScheduleActionSettings_url,
    motionGraphicsActivateScheduleActionSettings_username,

    -- ** MotionGraphicsConfiguration
    motionGraphicsConfiguration_motionGraphicsInsertion,
    motionGraphicsConfiguration_motionGraphicsSettings,

    -- ** MotionGraphicsDeactivateScheduleActionSettings

    -- ** MotionGraphicsSettings
    motionGraphicsSettings_htmlMotionGraphicsSettings,

    -- ** Mp2Settings
    mp2Settings_bitrate,
    mp2Settings_codingMode,
    mp2Settings_sampleRate,

    -- ** Mpeg2FilterSettings
    mpeg2FilterSettings_temporalFilterSettings,

    -- ** Mpeg2Settings
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_afdSignaling,
    mpeg2Settings_colorMetadata,
    mpeg2Settings_colorSpace,
    mpeg2Settings_displayAspectRatio,
    mpeg2Settings_filterSettings,
    mpeg2Settings_fixedAfd,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_gopNumBFrames,
    mpeg2Settings_gopSize,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_scanType,
    mpeg2Settings_subgopLength,
    mpeg2Settings_timecodeBurninSettings,
    mpeg2Settings_timecodeInsertion,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_framerateDenominator,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_acquisitionPointId,
    msSmoothGroupSettings_audioOnlyTimecodeControl,
    msSmoothGroupSettings_certificateMode,
    msSmoothGroupSettings_connectionRetryInterval,
    msSmoothGroupSettings_eventId,
    msSmoothGroupSettings_eventIdMode,
    msSmoothGroupSettings_eventStopBehavior,
    msSmoothGroupSettings_filecacheDuration,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_inputLossAction,
    msSmoothGroupSettings_numRetries,
    msSmoothGroupSettings_restartDelay,
    msSmoothGroupSettings_segmentationMode,
    msSmoothGroupSettings_sendDelayMs,
    msSmoothGroupSettings_sparseTrackType,
    msSmoothGroupSettings_streamManifestBehavior,
    msSmoothGroupSettings_timestampOffset,
    msSmoothGroupSettings_timestampOffsetMode,
    msSmoothGroupSettings_destination,

    -- ** MsSmoothOutputSettings
    msSmoothOutputSettings_h265PackagingType,
    msSmoothOutputSettings_nameModifier,

    -- ** Multiplex
    multiplex_arn,
    multiplex_availabilityZones,
    multiplex_destinations,
    multiplex_id,
    multiplex_multiplexSettings,
    multiplex_name,
    multiplex_pipelinesRunningCount,
    multiplex_programCount,
    multiplex_state,
    multiplex_tags,

    -- ** MultiplexGroupSettings

    -- ** MultiplexMediaConnectOutputDestinationSettings
    multiplexMediaConnectOutputDestinationSettings_entitlementArn,

    -- ** MultiplexOutputDestination
    multiplexOutputDestination_mediaConnectSettings,

    -- ** MultiplexOutputSettings
    multiplexOutputSettings_destination,

    -- ** MultiplexProgram
    multiplexProgram_channelId,
    multiplexProgram_multiplexProgramSettings,
    multiplexProgram_packetIdentifiersMap,
    multiplexProgram_pipelineDetails,
    multiplexProgram_programName,

    -- ** MultiplexProgramChannelDestinationSettings
    multiplexProgramChannelDestinationSettings_multiplexId,
    multiplexProgramChannelDestinationSettings_programName,

    -- ** MultiplexProgramPacketIdentifiersMap
    multiplexProgramPacketIdentifiersMap_audioPids,
    multiplexProgramPacketIdentifiersMap_dvbSubPids,
    multiplexProgramPacketIdentifiersMap_dvbTeletextPid,
    multiplexProgramPacketIdentifiersMap_etvPlatformPid,
    multiplexProgramPacketIdentifiersMap_etvSignalPid,
    multiplexProgramPacketIdentifiersMap_klvDataPids,
    multiplexProgramPacketIdentifiersMap_pcrPid,
    multiplexProgramPacketIdentifiersMap_pmtPid,
    multiplexProgramPacketIdentifiersMap_privateMetadataPid,
    multiplexProgramPacketIdentifiersMap_scte27Pids,
    multiplexProgramPacketIdentifiersMap_scte35Pid,
    multiplexProgramPacketIdentifiersMap_timedMetadataPid,
    multiplexProgramPacketIdentifiersMap_videoPid,

    -- ** MultiplexProgramPipelineDetail
    multiplexProgramPipelineDetail_activeChannelPipeline,
    multiplexProgramPipelineDetail_pipelineId,

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
    multiplexSettings_maximumVideoBufferDelayMilliseconds,
    multiplexSettings_transportStreamReservedBitrate,
    multiplexSettings_transportStreamBitrate,
    multiplexSettings_transportStreamId,

    -- ** MultiplexSettingsSummary
    multiplexSettingsSummary_transportStreamBitrate,

    -- ** MultiplexStatmuxVideoSettings
    multiplexStatmuxVideoSettings_maximumBitrate,
    multiplexStatmuxVideoSettings_minimumBitrate,
    multiplexStatmuxVideoSettings_priority,

    -- ** MultiplexSummary
    multiplexSummary_arn,
    multiplexSummary_availabilityZones,
    multiplexSummary_id,
    multiplexSummary_multiplexSettings,
    multiplexSummary_name,
    multiplexSummary_pipelinesRunningCount,
    multiplexSummary_programCount,
    multiplexSummary_state,
    multiplexSummary_tags,

    -- ** MultiplexVideoSettings
    multiplexVideoSettings_constantBitrate,
    multiplexVideoSettings_statmuxSettings,

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
    nielsenWatermarksSettings_nielsenDistributionType,
    nielsenWatermarksSettings_nielsenNaesIiNwSettings,

    -- ** Offering
    offering_arn,
    offering_currencyCode,
    offering_duration,
    offering_durationUnits,
    offering_fixedPrice,
    offering_offeringDescription,
    offering_offeringId,
    offering_offeringType,
    offering_region,
    offering_resourceSpecification,
    offering_usagePrice,

    -- ** Output
    output_audioDescriptionNames,
    output_captionDescriptionNames,
    output_outputName,
    output_videoDescriptionName,
    output_outputSettings,

    -- ** OutputDestination
    outputDestination_id,
    outputDestination_mediaPackageSettings,
    outputDestination_multiplexSettings,
    outputDestination_settings,

    -- ** OutputDestinationSettings
    outputDestinationSettings_passwordParam,
    outputDestinationSettings_streamName,
    outputDestinationSettings_url,
    outputDestinationSettings_username,

    -- ** OutputGroup
    outputGroup_name,
    outputGroup_outputs,
    outputGroup_outputGroupSettings,

    -- ** OutputGroupSettings
    outputGroupSettings_archiveGroupSettings,
    outputGroupSettings_frameCaptureGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_mediaPackageGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_multiplexGroupSettings,
    outputGroupSettings_rtmpGroupSettings,
    outputGroupSettings_udpGroupSettings,

    -- ** OutputLocationRef
    outputLocationRef_destinationRefId,

    -- ** OutputSettings
    outputSettings_archiveOutputSettings,
    outputSettings_frameCaptureOutputSettings,
    outputSettings_hlsOutputSettings,
    outputSettings_mediaPackageOutputSettings,
    outputSettings_msSmoothOutputSettings,
    outputSettings_multiplexOutputSettings,
    outputSettings_rtmpOutputSettings,
    outputSettings_udpOutputSettings,

    -- ** PassThroughSettings

    -- ** PauseStateScheduleActionSettings
    pauseStateScheduleActionSettings_pipelines,

    -- ** PipelineDetail
    pipelineDetail_activeInputAttachmentName,
    pipelineDetail_activeInputSwitchActionName,
    pipelineDetail_activeMotionGraphicsActionName,
    pipelineDetail_activeMotionGraphicsUri,
    pipelineDetail_pipelineId,

    -- ** PipelinePauseStateSettings
    pipelinePauseStateSettings_pipelineId,

    -- ** RawSettings

    -- ** Rec601Settings

    -- ** Rec709Settings

    -- ** RemixSettings
    remixSettings_channelsIn,
    remixSettings_channelsOut,
    remixSettings_channelMappings,

    -- ** RenewalSettings
    renewalSettings_automaticRenewal,
    renewalSettings_renewalCount,

    -- ** Reservation
    reservation_arn,
    reservation_count,
    reservation_currencyCode,
    reservation_duration,
    reservation_durationUnits,
    reservation_end,
    reservation_fixedPrice,
    reservation_name,
    reservation_offeringDescription,
    reservation_offeringId,
    reservation_offeringType,
    reservation_region,
    reservation_renewalSettings,
    reservation_reservationId,
    reservation_resourceSpecification,
    reservation_start,
    reservation_state,
    reservation_tags,
    reservation_usagePrice,

    -- ** ReservationResourceSpecification
    reservationResourceSpecification_channelClass,
    reservationResourceSpecification_codec,
    reservationResourceSpecification_maximumBitrate,
    reservationResourceSpecification_maximumFramerate,
    reservationResourceSpecification_resolution,
    reservationResourceSpecification_resourceType,
    reservationResourceSpecification_specialFeature,
    reservationResourceSpecification_videoQuality,

    -- ** RtmpCaptionInfoDestinationSettings

    -- ** RtmpGroupSettings
    rtmpGroupSettings_adMarkers,
    rtmpGroupSettings_authenticationScheme,
    rtmpGroupSettings_cacheFullBehavior,
    rtmpGroupSettings_cacheLength,
    rtmpGroupSettings_captionData,
    rtmpGroupSettings_inputLossAction,
    rtmpGroupSettings_restartDelay,

    -- ** RtmpOutputSettings
    rtmpOutputSettings_certificateMode,
    rtmpOutputSettings_connectionRetryInterval,
    rtmpOutputSettings_numRetries,
    rtmpOutputSettings_destination,

    -- ** ScheduleAction
    scheduleAction_actionName,
    scheduleAction_scheduleActionStartSettings,
    scheduleAction_scheduleActionSettings,

    -- ** ScheduleActionSettings
    scheduleActionSettings_hlsId3SegmentTaggingSettings,
    scheduleActionSettings_hlsTimedMetadataSettings,
    scheduleActionSettings_inputPrepareSettings,
    scheduleActionSettings_inputSwitchSettings,
    scheduleActionSettings_motionGraphicsImageActivateSettings,
    scheduleActionSettings_motionGraphicsImageDeactivateSettings,
    scheduleActionSettings_pauseStateSettings,
    scheduleActionSettings_scte35InputSettings,
    scheduleActionSettings_scte35ReturnToNetworkSettings,
    scheduleActionSettings_scte35SpliceInsertSettings,
    scheduleActionSettings_scte35TimeSignalSettings,
    scheduleActionSettings_staticImageActivateSettings,
    scheduleActionSettings_staticImageDeactivateSettings,

    -- ** ScheduleActionStartSettings
    scheduleActionStartSettings_fixedModeScheduleActionStartSettings,
    scheduleActionStartSettings_followModeScheduleActionStartSettings,
    scheduleActionStartSettings_immediateModeScheduleActionStartSettings,

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

    -- ** Scte35InputScheduleActionSettings
    scte35InputScheduleActionSettings_inputAttachmentNameReference,
    scte35InputScheduleActionSettings_mode,

    -- ** Scte35ReturnToNetworkScheduleActionSettings
    scte35ReturnToNetworkScheduleActionSettings_spliceEventId,

    -- ** Scte35SegmentationDescriptor
    scte35SegmentationDescriptor_deliveryRestrictions,
    scte35SegmentationDescriptor_segmentNum,
    scte35SegmentationDescriptor_segmentationDuration,
    scte35SegmentationDescriptor_segmentationTypeId,
    scte35SegmentationDescriptor_segmentationUpid,
    scte35SegmentationDescriptor_segmentationUpidType,
    scte35SegmentationDescriptor_segmentsExpected,
    scte35SegmentationDescriptor_subSegmentNum,
    scte35SegmentationDescriptor_subSegmentsExpected,
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
    staticImageActivateScheduleActionSettings_duration,
    staticImageActivateScheduleActionSettings_fadeIn,
    staticImageActivateScheduleActionSettings_fadeOut,
    staticImageActivateScheduleActionSettings_height,
    staticImageActivateScheduleActionSettings_imageX,
    staticImageActivateScheduleActionSettings_imageY,
    staticImageActivateScheduleActionSettings_layer,
    staticImageActivateScheduleActionSettings_opacity,
    staticImageActivateScheduleActionSettings_width,
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
    temporalFilterSettings_postFilterSharpening,
    temporalFilterSettings_strength,

    -- ** TimecodeBurninSettings
    timecodeBurninSettings_prefix,
    timecodeBurninSettings_position,
    timecodeBurninSettings_fontSize,

    -- ** TimecodeConfig
    timecodeConfig_syncThreshold,
    timecodeConfig_source,

    -- ** TransferringInputDeviceSummary
    transferringInputDeviceSummary_id,
    transferringInputDeviceSummary_message,
    transferringInputDeviceSummary_targetCustomerId,
    transferringInputDeviceSummary_transferType,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_styleControl,

    -- ** UdpContainerSettings
    udpContainerSettings_m2tsSettings,

    -- ** UdpGroupSettings
    udpGroupSettings_inputLossAction,
    udpGroupSettings_timedMetadataId3Frame,
    udpGroupSettings_timedMetadataId3Period,

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
    videoCodecSettings_h264Settings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_mpeg2Settings,

    -- ** VideoDescription
    videoDescription_codecSettings,
    videoDescription_height,
    videoDescription_respondToAfd,
    videoDescription_scalingBehavior,
    videoDescription_sharpness,
    videoDescription_width,
    videoDescription_name,

    -- ** VideoSelector
    videoSelector_colorSpace,
    videoSelector_colorSpaceSettings,
    videoSelector_colorSpaceUsage,
    videoSelector_selectorSettings,

    -- ** VideoSelectorColorSpaceSettings
    videoSelectorColorSpaceSettings_hdr10Settings,

    -- ** VideoSelectorPid
    videoSelectorPid_pid,

    -- ** VideoSelectorProgramId
    videoSelectorProgramId_programId,

    -- ** VideoSelectorSettings
    videoSelectorSettings_videoSelectorPid,
    videoSelectorSettings_videoSelectorProgramId,

    -- ** VpcOutputSettings
    vpcOutputSettings_publicAddressAllocationIds,
    vpcOutputSettings_securityGroupIds,
    vpcOutputSettings_subnetIds,

    -- ** VpcOutputSettingsDescription
    vpcOutputSettingsDescription_availabilityZones,
    vpcOutputSettingsDescription_networkInterfaceIds,
    vpcOutputSettingsDescription_securityGroupIds,
    vpcOutputSettingsDescription_subnetIds,

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
import Amazonka.MediaLive.RebootInputDevice
import Amazonka.MediaLive.RejectInputDeviceTransfer
import Amazonka.MediaLive.StartChannel
import Amazonka.MediaLive.StartInputDeviceMaintenanceWindow
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
import Amazonka.MediaLive.Types.AudioDolbyEDecode
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
import Amazonka.MediaLive.Types.DolbyVision81Settings
import Amazonka.MediaLive.Types.DvbNitSettings
import Amazonka.MediaLive.Types.DvbSdtSettings
import Amazonka.MediaLive.Types.DvbSubDestinationSettings
import Amazonka.MediaLive.Types.DvbSubSourceSettings
import Amazonka.MediaLive.Types.DvbTdtSettings
import Amazonka.MediaLive.Types.Eac3AtmosSettings
import Amazonka.MediaLive.Types.Eac3Settings
import Amazonka.MediaLive.Types.EbuTtDDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Amazonka.MediaLive.Types.EmbeddedSourceSettings
import Amazonka.MediaLive.Types.EncoderSettings
import Amazonka.MediaLive.Types.Esam
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
import Amazonka.MediaLive.Types.MaintenanceCreateSettings
import Amazonka.MediaLive.Types.MaintenanceStatus
import Amazonka.MediaLive.Types.MaintenanceUpdateSettings
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
import Amazonka.MediaLive.Types.RenewalSettings
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
import Amazonka.MediaLive.Types.Scte35InputScheduleActionSettings
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
import Amazonka.MediaLive.Types.TimecodeBurninSettings
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
