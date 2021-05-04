{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Lens
  ( -- * Operations

    -- ** DisassociateCertificate
    disassociateCertificate_arn,
    disassociateCertificateResponse_httpStatus,

    -- ** UpdatePreset
    updatePreset_category,
    updatePreset_description,
    updatePreset_settings,
    updatePreset_name,
    updatePresetResponse_preset,
    updatePresetResponse_httpStatus,

    -- ** ListPresets
    listPresets_nextToken,
    listPresets_listBy,
    listPresets_maxResults,
    listPresets_category,
    listPresets_order,
    listPresetsResponse_nextToken,
    listPresetsResponse_presets,
    listPresetsResponse_httpStatus,

    -- ** DeletePreset
    deletePreset_name,
    deletePresetResponse_httpStatus,

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- ** CreatePreset
    createPreset_category,
    createPreset_tags,
    createPreset_description,
    createPreset_settings,
    createPreset_name,
    createPresetResponse_preset,
    createPresetResponse_httpStatus,

    -- ** GetJob
    getJob_id,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_nextToken,
    listJobTemplates_listBy,
    listJobTemplates_maxResults,
    listJobTemplates_category,
    listJobTemplates_order,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_arn,
    untagResourceResponse_httpStatus,

    -- ** CreateJobTemplate
    createJobTemplate_accelerationSettings,
    createJobTemplate_category,
    createJobTemplate_priority,
    createJobTemplate_statusUpdateInterval,
    createJobTemplate_tags,
    createJobTemplate_queue,
    createJobTemplate_description,
    createJobTemplate_hopDestinations,
    createJobTemplate_settings,
    createJobTemplate_name,
    createJobTemplateResponse_jobTemplate,
    createJobTemplateResponse_httpStatus,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_listBy,
    listQueues_maxResults,
    listQueues_order,
    listQueuesResponse_nextToken,
    listQueuesResponse_queues,
    listQueuesResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetQueue
    getQueue_name,
    getQueueResponse_queue,
    getQueueResponse_httpStatus,

    -- ** GetJobTemplate
    getJobTemplate_name,
    getJobTemplateResponse_jobTemplate,
    getJobTemplateResponse_httpStatus,

    -- ** AssociateCertificate
    associateCertificate_arn,
    associateCertificateResponse_httpStatus,

    -- ** ListJobs
    listJobs_status,
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_queue,
    listJobs_order,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** UpdateJobTemplate
    updateJobTemplate_accelerationSettings,
    updateJobTemplate_category,
    updateJobTemplate_priority,
    updateJobTemplate_statusUpdateInterval,
    updateJobTemplate_queue,
    updateJobTemplate_description,
    updateJobTemplate_hopDestinations,
    updateJobTemplate_settings,
    updateJobTemplate_name,
    updateJobTemplateResponse_jobTemplate,
    updateJobTemplateResponse_httpStatus,

    -- ** DeleteJobTemplate
    deleteJobTemplate_name,
    deleteJobTemplateResponse_httpStatus,

    -- ** CreateJob
    createJob_accelerationSettings,
    createJob_billingTagsSource,
    createJob_priority,
    createJob_statusUpdateInterval,
    createJob_jobTemplate,
    createJob_userMetadata,
    createJob_tags,
    createJob_queue,
    createJob_simulateReservedQueue,
    createJob_clientRequestToken,
    createJob_hopDestinations,
    createJob_role,
    createJob_settings,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** GetPreset
    getPreset_name,
    getPresetResponse_preset,
    getPresetResponse_httpStatus,

    -- ** UpdateQueue
    updateQueue_status,
    updateQueue_reservationPlanSettings,
    updateQueue_description,
    updateQueue_name,
    updateQueueResponse_queue,
    updateQueueResponse_httpStatus,

    -- ** DeleteQueue
    deleteQueue_name,
    deleteQueueResponse_httpStatus,

    -- ** CreateQueue
    createQueue_status,
    createQueue_tags,
    createQueue_reservationPlanSettings,
    createQueue_description,
    createQueue_pricingPlan,
    createQueue_name,
    createQueueResponse_queue,
    createQueueResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_nextToken,
    describeEndpoints_maxResults,
    describeEndpoints_mode,
    describeEndpointsResponse_nextToken,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_audioDescriptionBroadcasterMix,
    aacSettings_rateControlMode,
    aacSettings_codingMode,
    aacSettings_codecProfile,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_vbrQuality,
    aacSettings_bitrate,
    aacSettings_specification,

    -- ** Ac3Settings
    ac3Settings_dialnorm,
    ac3Settings_codingMode,
    ac3Settings_lfeFilter,
    ac3Settings_dynamicRangeCompressionProfile,
    ac3Settings_sampleRate,
    ac3Settings_bitstreamMode,
    ac3Settings_bitrate,
    ac3Settings_metadataControl,

    -- ** AccelerationSettings
    accelerationSettings_mode,

    -- ** AiffSettings
    aiffSettings_channels,
    aiffSettings_bitDepth,
    aiffSettings_sampleRate,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_terminateCaptions,
    ancillarySourceSettings_convert608To708,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** AudioChannelTaggingSettings
    audioChannelTaggingSettings_channelTag,

    -- ** AudioCodecSettings
    audioCodecSettings_ac3Settings,
    audioCodecSettings_vorbisSettings,
    audioCodecSettings_codec,
    audioCodecSettings_mp3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_opusSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_wavSettings,
    audioCodecSettings_aiffSettings,
    audioCodecSettings_eac3AtmosSettings,

    -- ** AudioDescription
    audioDescription_languageCode,
    audioDescription_customLanguageCode,
    audioDescription_audioType,
    audioDescription_codecSettings,
    audioDescription_languageCodeControl,
    audioDescription_audioChannelTaggingSettings,
    audioDescription_audioSourceName,
    audioDescription_audioTypeControl,
    audioDescription_remixSettings,
    audioDescription_audioNormalizationSettings,
    audioDescription_streamName,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_correctionGateLevel,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_peakCalculation,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_loudnessLogging,

    -- ** AudioSelector
    audioSelector_languageCode,
    audioSelector_programSelection,
    audioSelector_customLanguageCode,
    audioSelector_tracks,
    audioSelector_defaultSelection,
    audioSelector_selectorType,
    audioSelector_remixSettings,
    audioSelector_pids,
    audioSelector_externalAudioFileInput,
    audioSelector_offset,

    -- ** AudioSelectorGroup
    audioSelectorGroup_audioSelectorNames,

    -- ** AutomatedAbrSettings
    automatedAbrSettings_minAbrBitrate,
    automatedAbrSettings_maxRenditions,
    automatedAbrSettings_maxAbrBitrate,

    -- ** AutomatedEncodingSettings
    automatedEncodingSettings_abrSettings,

    -- ** Av1QvbrSettings
    av1QvbrSettings_qvbrQualityLevelFineTune,
    av1QvbrSettings_qvbrQualityLevel,

    -- ** Av1Settings
    av1Settings_spatialAdaptiveQuantization,
    av1Settings_framerateNumerator,
    av1Settings_rateControlMode,
    av1Settings_slices,
    av1Settings_gopSize,
    av1Settings_framerateDenominator,
    av1Settings_maxBitrate,
    av1Settings_adaptiveQuantization,
    av1Settings_framerateControl,
    av1Settings_numberBFramesBetweenReferenceFrames,
    av1Settings_framerateConversionAlgorithm,
    av1Settings_qvbrSettings,

    -- ** AvailBlanking
    availBlanking_availBlankingImage,

    -- ** AvcIntraSettings
    avcIntraSettings_interlaceMode,
    avcIntraSettings_telecine,
    avcIntraSettings_framerateNumerator,
    avcIntraSettings_framerateDenominator,
    avcIntraSettings_avcIntraClass,
    avcIntraSettings_scanTypeConversionMode,
    avcIntraSettings_framerateControl,
    avcIntraSettings_avcIntraUhdSettings,
    avcIntraSettings_framerateConversionAlgorithm,
    avcIntraSettings_slowPal,

    -- ** AvcIntraUhdSettings
    avcIntraUhdSettings_qualityTuningLevel,

    -- ** BurninDestinationSettings
    burninDestinationSettings_alignment,
    burninDestinationSettings_shadowOpacity,
    burninDestinationSettings_teletextSpacing,
    burninDestinationSettings_shadowColor,
    burninDestinationSettings_outlineColor,
    burninDestinationSettings_backgroundOpacity,
    burninDestinationSettings_fontScript,
    burninDestinationSettings_xPosition,
    burninDestinationSettings_fontColor,
    burninDestinationSettings_fontSize,
    burninDestinationSettings_backgroundColor,
    burninDestinationSettings_shadowXOffset,
    burninDestinationSettings_yPosition,
    burninDestinationSettings_fontResolution,
    burninDestinationSettings_outlineSize,
    burninDestinationSettings_fontOpacity,
    burninDestinationSettings_shadowYOffset,

    -- ** CaptionDescription
    captionDescription_languageCode,
    captionDescription_languageDescription,
    captionDescription_customLanguageCode,
    captionDescription_captionSelectorName,
    captionDescription_destinationSettings,

    -- ** CaptionDescriptionPreset
    captionDescriptionPreset_languageCode,
    captionDescriptionPreset_languageDescription,
    captionDescriptionPreset_customLanguageCode,
    captionDescriptionPreset_destinationSettings,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_destinationType,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_burninDestinationSettings,
    captionDestinationSettings_imscDestinationSettings,
    captionDestinationSettings_sccDestinationSettings,

    -- ** CaptionSelector
    captionSelector_languageCode,
    captionSelector_customLanguageCode,
    captionSelector_sourceSettings,

    -- ** CaptionSourceFramerate
    captionSourceFramerate_framerateNumerator,
    captionSourceFramerate_framerateDenominator,

    -- ** CaptionSourceSettings
    captionSourceSettings_ancillarySourceSettings,
    captionSourceSettings_trackSourceSettings,
    captionSourceSettings_embeddedSourceSettings,
    captionSourceSettings_dvbSubSourceSettings,
    captionSourceSettings_fileSourceSettings,
    captionSourceSettings_teletextSourceSettings,
    captionSourceSettings_sourceType,

    -- ** ChannelMapping
    channelMapping_outputChannels,

    -- ** CmafAdditionalManifest
    cmafAdditionalManifest_manifestNameModifier,
    cmafAdditionalManifest_selectedOutputs,

    -- ** CmafEncryptionSettings
    cmafEncryptionSettings_spekeKeyProvider,
    cmafEncryptionSettings_encryptionMethod,
    cmafEncryptionSettings_constantInitializationVector,
    cmafEncryptionSettings_initializationVectorInManifest,
    cmafEncryptionSettings_staticKeyProvider,
    cmafEncryptionSettings_type,

    -- ** CmafGroupSettings
    cmafGroupSettings_segmentLength,
    cmafGroupSettings_segmentControl,
    cmafGroupSettings_writeDashManifest,
    cmafGroupSettings_fragmentLength,
    cmafGroupSettings_manifestCompression,
    cmafGroupSettings_baseUrl,
    cmafGroupSettings_streamInfResolution,
    cmafGroupSettings_codecSpecification,
    cmafGroupSettings_additionalManifests,
    cmafGroupSettings_mpdProfile,
    cmafGroupSettings_encryption,
    cmafGroupSettings_minBufferTime,
    cmafGroupSettings_destination,
    cmafGroupSettings_minFinalSegmentLength,
    cmafGroupSettings_destinationSettings,
    cmafGroupSettings_writeSegmentTimelineInRepresentation,
    cmafGroupSettings_writeHlsManifest,
    cmafGroupSettings_clientCache,
    cmafGroupSettings_manifestDurationFormat,

    -- ** CmfcSettings
    cmfcSettings_iFrameOnlyManifest,
    cmfcSettings_audioDuration,
    cmfcSettings_scte35Esam,
    cmfcSettings_scte35Source,

    -- ** ColorCorrector
    colorCorrector_saturation,
    colorCorrector_colorSpaceConversion,
    colorCorrector_hdr10Metadata,
    colorCorrector_brightness,
    colorCorrector_hue,
    colorCorrector_contrast,

    -- ** ContainerSettings
    containerSettings_container,
    containerSettings_mpdSettings,
    containerSettings_mp4Settings,
    containerSettings_f4vSettings,
    containerSettings_mxfSettings,
    containerSettings_movSettings,
    containerSettings_cmfcSettings,
    containerSettings_m3u8Settings,
    containerSettings_m2tsSettings,

    -- ** DashAdditionalManifest
    dashAdditionalManifest_manifestNameModifier,
    dashAdditionalManifest_selectedOutputs,

    -- ** DashIsoEncryptionSettings
    dashIsoEncryptionSettings_spekeKeyProvider,
    dashIsoEncryptionSettings_playbackDeviceCompatibility,

    -- ** DashIsoGroupSettings
    dashIsoGroupSettings_segmentLength,
    dashIsoGroupSettings_segmentControl,
    dashIsoGroupSettings_fragmentLength,
    dashIsoGroupSettings_baseUrl,
    dashIsoGroupSettings_additionalManifests,
    dashIsoGroupSettings_mpdProfile,
    dashIsoGroupSettings_encryption,
    dashIsoGroupSettings_minBufferTime,
    dashIsoGroupSettings_hbbtvCompliance,
    dashIsoGroupSettings_destination,
    dashIsoGroupSettings_minFinalSegmentLength,
    dashIsoGroupSettings_destinationSettings,
    dashIsoGroupSettings_writeSegmentTimelineInRepresentation,

    -- ** Deinterlacer
    deinterlacer_algorithm,
    deinterlacer_mode,
    deinterlacer_control,

    -- ** DestinationSettings
    destinationSettings_s3Settings,

    -- ** DolbyVision
    dolbyVision_l6Mode,
    dolbyVision_l6Metadata,
    dolbyVision_profile,

    -- ** DolbyVisionLevel6Metadata
    dolbyVisionLevel6Metadata_maxCll,
    dolbyVisionLevel6Metadata_maxFall,

    -- ** DvbNitSettings
    dvbNitSettings_nitInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- ** DvbSdtSettings
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_sdtInterval,
    dvbSdtSettings_serviceProviderName,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_teletextSpacing,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_fontScript,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_subtitlingType,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_yPosition,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_shadowYOffset,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_tdtInterval,

    -- ** Eac3AtmosSettings
    eac3AtmosSettings_loRoCenterMixLevel,
    eac3AtmosSettings_ltRtCenterMixLevel,
    eac3AtmosSettings_speechThreshold,
    eac3AtmosSettings_codingMode,
    eac3AtmosSettings_dialogueIntelligence,
    eac3AtmosSettings_loRoSurroundMixLevel,
    eac3AtmosSettings_ltRtSurroundMixLevel,
    eac3AtmosSettings_sampleRate,
    eac3AtmosSettings_stereoDownmix,
    eac3AtmosSettings_meteringMode,
    eac3AtmosSettings_bitstreamMode,
    eac3AtmosSettings_surroundExMode,
    eac3AtmosSettings_dynamicRangeCompressionRf,
    eac3AtmosSettings_bitrate,
    eac3AtmosSettings_dynamicRangeCompressionLine,

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
    eac3Settings_sampleRate,
    eac3Settings_stereoDownmix,
    eac3Settings_bitstreamMode,
    eac3Settings_surroundExMode,
    eac3Settings_phaseControl,
    eac3Settings_dynamicRangeCompressionRf,
    eac3Settings_passthroughControl,
    eac3Settings_bitrate,
    eac3Settings_attenuationControl,
    eac3Settings_surroundMode,
    eac3Settings_metadataControl,
    eac3Settings_dynamicRangeCompressionLine,

    -- ** EmbeddedDestinationSettings
    embeddedDestinationSettings_destination708ServiceNumber,
    embeddedDestinationSettings_destination608ChannelNumber,

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_terminateCaptions,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- ** Endpoint
    endpoint_url,

    -- ** EsamManifestConfirmConditionNotification
    esamManifestConfirmConditionNotification_mccXml,

    -- ** EsamSettings
    esamSettings_responseSignalPreroll,
    esamSettings_manifestConfirmConditionNotification,
    esamSettings_signalProcessingNotification,

    -- ** EsamSignalProcessingNotification
    esamSignalProcessingNotification_sccXml,

    -- ** F4vSettings
    f4vSettings_moovPlacement,

    -- ** FileGroupSettings
    fileGroupSettings_destination,
    fileGroupSettings_destinationSettings,

    -- ** FileSourceSettings
    fileSourceSettings_convert608To708,
    fileSourceSettings_framerate,
    fileSourceSettings_sourceFile,
    fileSourceSettings_timeDelta,

    -- ** FrameCaptureSettings
    frameCaptureSettings_framerateNumerator,
    frameCaptureSettings_maxCaptures,
    frameCaptureSettings_framerateDenominator,
    frameCaptureSettings_quality,

    -- ** H264QvbrSettings
    h264QvbrSettings_qvbrQualityLevelFineTune,
    h264QvbrSettings_qvbrQualityLevel,
    h264QvbrSettings_maxAverageBitrate,

    -- ** H264Settings
    h264Settings_hrdBufferInitialFillPercentage,
    h264Settings_temporalAdaptiveQuantization,
    h264Settings_flickerAdaptiveQuantization,
    h264Settings_qualityTuningLevel,
    h264Settings_interlaceMode,
    h264Settings_repeatPps,
    h264Settings_fieldEncoding,
    h264Settings_telecine,
    h264Settings_gopBReference,
    h264Settings_spatialAdaptiveQuantization,
    h264Settings_framerateNumerator,
    h264Settings_rateControlMode,
    h264Settings_numberReferenceFrames,
    h264Settings_slices,
    h264Settings_entropyEncoding,
    h264Settings_gopSizeUnits,
    h264Settings_codecProfile,
    h264Settings_gopSize,
    h264Settings_framerateDenominator,
    h264Settings_softness,
    h264Settings_parNumerator,
    h264Settings_sceneChangeDetect,
    h264Settings_minIInterval,
    h264Settings_unregisteredSeiTimecode,
    h264Settings_scanTypeConversionMode,
    h264Settings_parControl,
    h264Settings_gopClosedCadence,
    h264Settings_parDenominator,
    h264Settings_maxBitrate,
    h264Settings_dynamicSubGop,
    h264Settings_syntax,
    h264Settings_hrdBufferSize,
    h264Settings_adaptiveQuantization,
    h264Settings_framerateControl,
    h264Settings_numberBFramesBetweenReferenceFrames,
    h264Settings_framerateConversionAlgorithm,
    h264Settings_codecLevel,
    h264Settings_bitrate,
    h264Settings_qvbrSettings,
    h264Settings_slowPal,

    -- ** H265QvbrSettings
    h265QvbrSettings_qvbrQualityLevelFineTune,
    h265QvbrSettings_qvbrQualityLevel,
    h265QvbrSettings_maxAverageBitrate,

    -- ** H265Settings
    h265Settings_hrdBufferInitialFillPercentage,
    h265Settings_temporalAdaptiveQuantization,
    h265Settings_flickerAdaptiveQuantization,
    h265Settings_qualityTuningLevel,
    h265Settings_interlaceMode,
    h265Settings_telecine,
    h265Settings_gopBReference,
    h265Settings_spatialAdaptiveQuantization,
    h265Settings_framerateNumerator,
    h265Settings_rateControlMode,
    h265Settings_numberReferenceFrames,
    h265Settings_slices,
    h265Settings_writeMp4PackagingType,
    h265Settings_gopSizeUnits,
    h265Settings_codecProfile,
    h265Settings_gopSize,
    h265Settings_framerateDenominator,
    h265Settings_parNumerator,
    h265Settings_tiles,
    h265Settings_sceneChangeDetect,
    h265Settings_minIInterval,
    h265Settings_unregisteredSeiTimecode,
    h265Settings_scanTypeConversionMode,
    h265Settings_parControl,
    h265Settings_gopClosedCadence,
    h265Settings_parDenominator,
    h265Settings_maxBitrate,
    h265Settings_dynamicSubGop,
    h265Settings_alternateTransferFunctionSei,
    h265Settings_hrdBufferSize,
    h265Settings_adaptiveQuantization,
    h265Settings_framerateControl,
    h265Settings_numberBFramesBetweenReferenceFrames,
    h265Settings_framerateConversionAlgorithm,
    h265Settings_codecLevel,
    h265Settings_bitrate,
    h265Settings_temporalIds,
    h265Settings_qvbrSettings,
    h265Settings_sampleAdaptiveOffsetFilterMode,
    h265Settings_slowPal,

    -- ** Hdr10Metadata
    hdr10Metadata_greenPrimaryX,
    hdr10Metadata_maxLuminance,
    hdr10Metadata_greenPrimaryY,
    hdr10Metadata_bluePrimaryY,
    hdr10Metadata_bluePrimaryX,
    hdr10Metadata_redPrimaryX,
    hdr10Metadata_redPrimaryY,
    hdr10Metadata_whitePointX,
    hdr10Metadata_minLuminance,
    hdr10Metadata_maxContentLightLevel,
    hdr10Metadata_maxFrameAverageLightLevel,
    hdr10Metadata_whitePointY,

    -- ** HlsAdditionalManifest
    hlsAdditionalManifest_manifestNameModifier,
    hlsAdditionalManifest_selectedOutputs,

    -- ** HlsCaptionLanguageMapping
    hlsCaptionLanguageMapping_languageCode,
    hlsCaptionLanguageMapping_languageDescription,
    hlsCaptionLanguageMapping_customLanguageCode,
    hlsCaptionLanguageMapping_captionChannel,

    -- ** HlsEncryptionSettings
    hlsEncryptionSettings_offlineEncrypted,
    hlsEncryptionSettings_spekeKeyProvider,
    hlsEncryptionSettings_encryptionMethod,
    hlsEncryptionSettings_constantInitializationVector,
    hlsEncryptionSettings_initializationVectorInManifest,
    hlsEncryptionSettings_staticKeyProvider,
    hlsEncryptionSettings_type,

    -- ** HlsGroupSettings
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_segmentControl,
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_baseUrl,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_additionalManifests,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_encryption,
    hlsGroupSettings_destination,
    hlsGroupSettings_minFinalSegmentLength,
    hlsGroupSettings_destinationSettings,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_audioOnlyHeader,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_captionLanguageSetting,

    -- ** HlsSettings
    hlsSettings_audioRenditionSets,
    hlsSettings_iFrameOnlyManifest,
    hlsSettings_segmentModifier,
    hlsSettings_audioOnlyContainer,
    hlsSettings_audioGroupId,
    hlsSettings_audioTrackType,

    -- ** HopDestination
    hopDestination_priority,
    hopDestination_queue,
    hopDestination_waitMinutes,

    -- ** Id3Insertion
    id3Insertion_id3,
    id3Insertion_timecode,

    -- ** ImageInserter
    imageInserter_insertableImages,

    -- ** ImscDestinationSettings
    imscDestinationSettings_stylePassthrough,

    -- ** Input
    input_imageInserter,
    input_denoiseFilter,
    input_supplementalImps,
    input_inputScanType,
    input_inputClippings,
    input_fileInput,
    input_timecodeStart,
    input_decryptionSettings,
    input_audioSelectors,
    input_filterStrength,
    input_psiControl,
    input_programNumber,
    input_audioSelectorGroups,
    input_videoSelector,
    input_filterEnable,
    input_position,
    input_crop,
    input_deblockFilter,
    input_captionSelectors,
    input_timecodeSource,

    -- ** InputClipping
    inputClipping_startTimecode,
    inputClipping_endTimecode,

    -- ** InputDecryptionSettings
    inputDecryptionSettings_decryptionMode,
    inputDecryptionSettings_encryptedDecryptionKey,
    inputDecryptionSettings_initializationVector,
    inputDecryptionSettings_kmsKeyRegion,

    -- ** InputTemplate
    inputTemplate_imageInserter,
    inputTemplate_denoiseFilter,
    inputTemplate_inputScanType,
    inputTemplate_inputClippings,
    inputTemplate_timecodeStart,
    inputTemplate_audioSelectors,
    inputTemplate_filterStrength,
    inputTemplate_psiControl,
    inputTemplate_programNumber,
    inputTemplate_audioSelectorGroups,
    inputTemplate_videoSelector,
    inputTemplate_filterEnable,
    inputTemplate_position,
    inputTemplate_crop,
    inputTemplate_deblockFilter,
    inputTemplate_captionSelectors,
    inputTemplate_timecodeSource,

    -- ** InsertableImage
    insertableImage_height,
    insertableImage_imageX,
    insertableImage_imageY,
    insertableImage_duration,
    insertableImage_width,
    insertableImage_layer,
    insertableImage_startTime,
    insertableImage_opacity,
    insertableImage_fadeIn,
    insertableImage_imageInserterInput,
    insertableImage_fadeOut,

    -- ** Job
    job_accelerationSettings,
    job_billingTagsSource,
    job_status,
    job_accelerationStatus,
    job_retryCount,
    job_queueTransitions,
    job_arn,
    job_id,
    job_jobPercentComplete,
    job_createdAt,
    job_priority,
    job_statusUpdateInterval,
    job_jobTemplate,
    job_userMetadata,
    job_queue,
    job_currentPhase,
    job_errorMessage,
    job_simulateReservedQueue,
    job_timing,
    job_hopDestinations,
    job_messages,
    job_errorCode,
    job_outputGroupDetails,
    job_role,
    job_settings,

    -- ** JobMessages
    jobMessages_info,
    jobMessages_warning,

    -- ** JobSettings
    jobSettings_adAvailOffset,
    jobSettings_timedMetadataInsertion,
    jobSettings_esam,
    jobSettings_nielsenNonLinearWatermark,
    jobSettings_motionImageInserter,
    jobSettings_availBlanking,
    jobSettings_nielsenConfiguration,
    jobSettings_outputGroups,
    jobSettings_inputs,
    jobSettings_timecodeConfig,

    -- ** JobTemplate
    jobTemplate_accelerationSettings,
    jobTemplate_category,
    jobTemplate_arn,
    jobTemplate_createdAt,
    jobTemplate_priority,
    jobTemplate_statusUpdateInterval,
    jobTemplate_lastUpdated,
    jobTemplate_queue,
    jobTemplate_description,
    jobTemplate_type,
    jobTemplate_hopDestinations,
    jobTemplate_settings,
    jobTemplate_name,

    -- ** JobTemplateSettings
    jobTemplateSettings_adAvailOffset,
    jobTemplateSettings_timedMetadataInsertion,
    jobTemplateSettings_esam,
    jobTemplateSettings_nielsenNonLinearWatermark,
    jobTemplateSettings_motionImageInserter,
    jobTemplateSettings_availBlanking,
    jobTemplateSettings_nielsenConfiguration,
    jobTemplateSettings_outputGroups,
    jobTemplateSettings_inputs,
    jobTemplateSettings_timecodeConfig,

    -- ** M2tsScte35Esam
    m2tsScte35Esam_scte35EsamPid,

    -- ** M2tsSettings
    m2tsSettings_segmentationMarkers,
    m2tsSettings_pmtPid,
    m2tsSettings_videoPid,
    m2tsSettings_audioBufferModel,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_segmentationStyle,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_pcrControl,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_ebpPlacement,
    m2tsSettings_pmtInterval,
    m2tsSettings_audioPids,
    m2tsSettings_patInterval,
    m2tsSettings_minEbpInterval,
    m2tsSettings_maxPcrInterval,
    m2tsSettings_programNumber,
    m2tsSettings_bufferModel,
    m2tsSettings_pcrPid,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_rateMode,
    m2tsSettings_dvbTdtSettings,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_segmentationTime,
    m2tsSettings_audioDuration,
    m2tsSettings_nielsenId3,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_bitrate,
    m2tsSettings_fragmentTime,
    m2tsSettings_esRateInPes,
    m2tsSettings_privateMetadataPid,
    m2tsSettings_scte35Esam,
    m2tsSettings_scte35Source,
    m2tsSettings_forceTsVideoEbpOrder,
    m2tsSettings_transportStreamId,
    m2tsSettings_dvbSubPids,
    m2tsSettings_scte35Pid,

    -- ** M3u8Settings
    m3u8Settings_pmtPid,
    m3u8Settings_timedMetadata,
    m3u8Settings_videoPid,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_pcrControl,
    m3u8Settings_pmtInterval,
    m3u8Settings_audioPids,
    m3u8Settings_patInterval,
    m3u8Settings_programNumber,
    m3u8Settings_pcrPid,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_audioDuration,
    m3u8Settings_nielsenId3,
    m3u8Settings_privateMetadataPid,
    m3u8Settings_scte35Source,
    m3u8Settings_transportStreamId,
    m3u8Settings_scte35Pid,

    -- ** MotionImageInserter
    motionImageInserter_insertionMode,
    motionImageInserter_input,
    motionImageInserter_startTime,
    motionImageInserter_playback,
    motionImageInserter_framerate,
    motionImageInserter_offset,

    -- ** MotionImageInsertionFramerate
    motionImageInsertionFramerate_framerateNumerator,
    motionImageInsertionFramerate_framerateDenominator,

    -- ** MotionImageInsertionOffset
    motionImageInsertionOffset_imageX,
    motionImageInsertionOffset_imageY,

    -- ** MovSettings
    movSettings_paddingControl,
    movSettings_cslgAtom,
    movSettings_mpeg2FourCCControl,
    movSettings_clapAtom,
    movSettings_reference,

    -- ** Mp2Settings
    mp2Settings_channels,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- ** Mp3Settings
    mp3Settings_rateControlMode,
    mp3Settings_channels,
    mp3Settings_sampleRate,
    mp3Settings_vbrQuality,
    mp3Settings_bitrate,

    -- ** Mp4Settings
    mp4Settings_cslgAtom,
    mp4Settings_mp4MajorBrand,
    mp4Settings_audioDuration,
    mp4Settings_freeSpaceBox,
    mp4Settings_moovPlacement,
    mp4Settings_cttsVersion,

    -- ** MpdSettings
    mpdSettings_accessibilityCaptionHints,
    mpdSettings_captionContainerType,
    mpdSettings_audioDuration,
    mpdSettings_scte35Esam,
    mpdSettings_scte35Source,

    -- ** Mpeg2Settings
    mpeg2Settings_hrdBufferInitialFillPercentage,
    mpeg2Settings_temporalAdaptiveQuantization,
    mpeg2Settings_qualityTuningLevel,
    mpeg2Settings_interlaceMode,
    mpeg2Settings_intraDcPrecision,
    mpeg2Settings_telecine,
    mpeg2Settings_spatialAdaptiveQuantization,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_rateControlMode,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_codecProfile,
    mpeg2Settings_gopSize,
    mpeg2Settings_framerateDenominator,
    mpeg2Settings_softness,
    mpeg2Settings_parNumerator,
    mpeg2Settings_sceneChangeDetect,
    mpeg2Settings_minIInterval,
    mpeg2Settings_scanTypeConversionMode,
    mpeg2Settings_parControl,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_parDenominator,
    mpeg2Settings_maxBitrate,
    mpeg2Settings_dynamicSubGop,
    mpeg2Settings_syntax,
    mpeg2Settings_hrdBufferSize,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_framerateControl,
    mpeg2Settings_numberBFramesBetweenReferenceFrames,
    mpeg2Settings_framerateConversionAlgorithm,
    mpeg2Settings_codecLevel,
    mpeg2Settings_bitrate,
    mpeg2Settings_slowPal,

    -- ** MsSmoothAdditionalManifest
    msSmoothAdditionalManifest_manifestNameModifier,
    msSmoothAdditionalManifest_selectedOutputs,

    -- ** MsSmoothEncryptionSettings
    msSmoothEncryptionSettings_spekeKeyProvider,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_manifestEncoding,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_additionalManifests,
    msSmoothGroupSettings_encryption,
    msSmoothGroupSettings_destination,
    msSmoothGroupSettings_destinationSettings,
    msSmoothGroupSettings_audioDeduplication,

    -- ** MxfSettings
    mxfSettings_profile,
    mxfSettings_afdSignaling,

    -- ** NexGuardFileMarkerSettings
    nexGuardFileMarkerSettings_payload,
    nexGuardFileMarkerSettings_license,
    nexGuardFileMarkerSettings_preset,
    nexGuardFileMarkerSettings_strength,

    -- ** NielsenConfiguration
    nielsenConfiguration_breakoutCode,
    nielsenConfiguration_distributorId,

    -- ** NielsenNonLinearWatermarkSettings
    nielsenNonLinearWatermarkSettings_assetName,
    nielsenNonLinearWatermarkSettings_activeWatermarkProcess,
    nielsenNonLinearWatermarkSettings_sourceWatermarkStatus,
    nielsenNonLinearWatermarkSettings_ticServerUrl,
    nielsenNonLinearWatermarkSettings_sourceId,
    nielsenNonLinearWatermarkSettings_cbetSourceId,
    nielsenNonLinearWatermarkSettings_episodeId,
    nielsenNonLinearWatermarkSettings_metadataDestination,
    nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack,
    nielsenNonLinearWatermarkSettings_adiFilename,
    nielsenNonLinearWatermarkSettings_assetId,

    -- ** NoiseReducer
    noiseReducer_spatialFilterSettings,
    noiseReducer_temporalFilterSettings,
    noiseReducer_filterSettings,
    noiseReducer_filter,

    -- ** NoiseReducerFilterSettings
    noiseReducerFilterSettings_strength,

    -- ** NoiseReducerSpatialFilterSettings
    noiseReducerSpatialFilterSettings_speed,
    noiseReducerSpatialFilterSettings_postFilterSharpenStrength,
    noiseReducerSpatialFilterSettings_strength,

    -- ** NoiseReducerTemporalFilterSettings
    noiseReducerTemporalFilterSettings_postTemporalSharpening,
    noiseReducerTemporalFilterSettings_speed,
    noiseReducerTemporalFilterSettings_aggressiveMode,
    noiseReducerTemporalFilterSettings_strength,

    -- ** OpusSettings
    opusSettings_channels,
    opusSettings_sampleRate,
    opusSettings_bitrate,

    -- ** Output
    output_audioDescriptions,
    output_preset,
    output_containerSettings,
    output_videoDescription,
    output_extension,
    output_captionDescriptions,
    output_nameModifier,
    output_outputSettings,

    -- ** OutputChannelMapping
    outputChannelMapping_inputChannels,
    outputChannelMapping_inputChannelsFineTune,

    -- ** OutputDetail
    outputDetail_videoDetails,
    outputDetail_durationInMs,

    -- ** OutputGroup
    outputGroup_outputs,
    outputGroup_automatedEncodingSettings,
    outputGroup_outputGroupSettings,
    outputGroup_name,
    outputGroup_customName,

    -- ** OutputGroupDetail
    outputGroupDetail_outputDetails,

    -- ** OutputGroupSettings
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_fileGroupSettings,
    outputGroupSettings_dashIsoGroupSettings,
    outputGroupSettings_cmafGroupSettings,
    outputGroupSettings_type,

    -- ** OutputSettings
    outputSettings_hlsSettings,

    -- ** PartnerWatermarking
    partnerWatermarking_nexguardFileMarkerSettings,

    -- ** Preset
    preset_category,
    preset_arn,
    preset_createdAt,
    preset_lastUpdated,
    preset_description,
    preset_type,
    preset_settings,
    preset_name,

    -- ** PresetSettings
    presetSettings_audioDescriptions,
    presetSettings_containerSettings,
    presetSettings_videoDescription,
    presetSettings_captionDescriptions,

    -- ** ProresSettings
    proresSettings_interlaceMode,
    proresSettings_telecine,
    proresSettings_framerateNumerator,
    proresSettings_codecProfile,
    proresSettings_framerateDenominator,
    proresSettings_parNumerator,
    proresSettings_scanTypeConversionMode,
    proresSettings_parControl,
    proresSettings_parDenominator,
    proresSettings_framerateControl,
    proresSettings_framerateConversionAlgorithm,
    proresSettings_slowPal,

    -- ** Queue
    queue_status,
    queue_arn,
    queue_createdAt,
    queue_lastUpdated,
    queue_submittedJobsCount,
    queue_description,
    queue_pricingPlan,
    queue_reservationPlan,
    queue_type,
    queue_progressingJobsCount,
    queue_name,

    -- ** QueueTransition
    queueTransition_sourceQueue,
    queueTransition_timestamp,
    queueTransition_destinationQueue,

    -- ** Rectangle
    rectangle_height,
    rectangle_y,
    rectangle_width,
    rectangle_x,

    -- ** RemixSettings
    remixSettings_channelMapping,
    remixSettings_channelsIn,
    remixSettings_channelsOut,

    -- ** ReservationPlan
    reservationPlan_status,
    reservationPlan_reservedSlots,
    reservationPlan_expiresAt,
    reservationPlan_purchasedAt,
    reservationPlan_renewalType,
    reservationPlan_commitment,

    -- ** ReservationPlanSettings
    reservationPlanSettings_commitment,
    reservationPlanSettings_reservedSlots,
    reservationPlanSettings_renewalType,

    -- ** ResourceTags
    resourceTags_arn,
    resourceTags_tags,

    -- ** S3DestinationAccessControl
    s3DestinationAccessControl_cannedAcl,

    -- ** S3DestinationSettings
    s3DestinationSettings_encryption,
    s3DestinationSettings_accessControl,

    -- ** S3EncryptionSettings
    s3EncryptionSettings_encryptionType,
    s3EncryptionSettings_kmsKeyArn,

    -- ** SccDestinationSettings
    sccDestinationSettings_framerate,

    -- ** SpekeKeyProvider
    spekeKeyProvider_resourceId,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,

    -- ** SpekeKeyProviderCmaf
    spekeKeyProviderCmaf_resourceId,
    spekeKeyProviderCmaf_certificateArn,
    spekeKeyProviderCmaf_url,
    spekeKeyProviderCmaf_hlsSignaledSystemIds,
    spekeKeyProviderCmaf_dashSignaledSystemIds,

    -- ** StaticKeyProvider
    staticKeyProvider_keyFormat,
    staticKeyProvider_staticKeyValue,
    staticKeyProvider_url,
    staticKeyProvider_keyFormatVersions,

    -- ** TeletextDestinationSettings
    teletextDestinationSettings_pageTypes,
    teletextDestinationSettings_pageNumber,

    -- ** TeletextSourceSettings
    teletextSourceSettings_pageNumber,

    -- ** TimecodeBurnin
    timecodeBurnin_prefix,
    timecodeBurnin_position,
    timecodeBurnin_fontSize,

    -- ** TimecodeConfig
    timecodeConfig_anchor,
    timecodeConfig_source,
    timecodeConfig_timestampOffset,
    timecodeConfig_start,

    -- ** TimedMetadataInsertion
    timedMetadataInsertion_id3Insertions,

    -- ** Timing
    timing_finishTime,
    timing_startTime,
    timing_submitTime,

    -- ** TrackSourceSettings
    trackSourceSettings_trackNumber,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_stylePassthrough,

    -- ** Vc3Settings
    vc3Settings_interlaceMode,
    vc3Settings_telecine,
    vc3Settings_framerateNumerator,
    vc3Settings_framerateDenominator,
    vc3Settings_scanTypeConversionMode,
    vc3Settings_framerateControl,
    vc3Settings_framerateConversionAlgorithm,
    vc3Settings_vc3Class,
    vc3Settings_slowPal,

    -- ** VideoCodecSettings
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_codec,
    videoCodecSettings_vc3Settings,
    videoCodecSettings_vp8Settings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_vp9Settings,
    videoCodecSettings_proresSettings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_avcIntraSettings,
    videoCodecSettings_av1Settings,

    -- ** VideoDescription
    videoDescription_height,
    videoDescription_antiAlias,
    videoDescription_dropFrameTimecode,
    videoDescription_respondToAfd,
    videoDescription_width,
    videoDescription_codecSettings,
    videoDescription_colorMetadata,
    videoDescription_fixedAfd,
    videoDescription_timecodeInsertion,
    videoDescription_scalingBehavior,
    videoDescription_position,
    videoDescription_crop,
    videoDescription_videoPreprocessors,
    videoDescription_sharpness,
    videoDescription_afdSignaling,

    -- ** VideoDetail
    videoDetail_widthInPx,
    videoDetail_heightInPx,

    -- ** VideoPreprocessor
    videoPreprocessor_imageInserter,
    videoPreprocessor_timecodeBurnin,
    videoPreprocessor_deinterlacer,
    videoPreprocessor_noiseReducer,
    videoPreprocessor_partnerWatermarking,
    videoPreprocessor_colorCorrector,
    videoPreprocessor_dolbyVision,

    -- ** VideoSelector
    videoSelector_colorSpaceUsage,
    videoSelector_hdr10Metadata,
    videoSelector_programNumber,
    videoSelector_rotate,
    videoSelector_colorSpace,
    videoSelector_alphaBehavior,
    videoSelector_pid,

    -- ** VorbisSettings
    vorbisSettings_channels,
    vorbisSettings_sampleRate,
    vorbisSettings_vbrQuality,

    -- ** Vp8Settings
    vp8Settings_qualityTuningLevel,
    vp8Settings_framerateNumerator,
    vp8Settings_rateControlMode,
    vp8Settings_gopSize,
    vp8Settings_framerateDenominator,
    vp8Settings_parNumerator,
    vp8Settings_parControl,
    vp8Settings_parDenominator,
    vp8Settings_maxBitrate,
    vp8Settings_hrdBufferSize,
    vp8Settings_framerateControl,
    vp8Settings_framerateConversionAlgorithm,
    vp8Settings_bitrate,

    -- ** Vp9Settings
    vp9Settings_qualityTuningLevel,
    vp9Settings_framerateNumerator,
    vp9Settings_rateControlMode,
    vp9Settings_gopSize,
    vp9Settings_framerateDenominator,
    vp9Settings_parNumerator,
    vp9Settings_parControl,
    vp9Settings_parDenominator,
    vp9Settings_maxBitrate,
    vp9Settings_hrdBufferSize,
    vp9Settings_framerateControl,
    vp9Settings_framerateConversionAlgorithm,
    vp9Settings_bitrate,

    -- ** WavSettings
    wavSettings_format,
    wavSettings_channels,
    wavSettings_bitDepth,
    wavSettings_sampleRate,
  )
where

import Network.AWS.MediaConvert.AssociateCertificate
import Network.AWS.MediaConvert.CancelJob
import Network.AWS.MediaConvert.CreateJob
import Network.AWS.MediaConvert.CreateJobTemplate
import Network.AWS.MediaConvert.CreatePreset
import Network.AWS.MediaConvert.CreateQueue
import Network.AWS.MediaConvert.DeleteJobTemplate
import Network.AWS.MediaConvert.DeletePreset
import Network.AWS.MediaConvert.DeleteQueue
import Network.AWS.MediaConvert.DescribeEndpoints
import Network.AWS.MediaConvert.DisassociateCertificate
import Network.AWS.MediaConvert.GetJob
import Network.AWS.MediaConvert.GetJobTemplate
import Network.AWS.MediaConvert.GetPreset
import Network.AWS.MediaConvert.GetQueue
import Network.AWS.MediaConvert.ListJobTemplates
import Network.AWS.MediaConvert.ListJobs
import Network.AWS.MediaConvert.ListPresets
import Network.AWS.MediaConvert.ListQueues
import Network.AWS.MediaConvert.ListTagsForResource
import Network.AWS.MediaConvert.TagResource
import Network.AWS.MediaConvert.Types.AacSettings
import Network.AWS.MediaConvert.Types.Ac3Settings
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.AiffSettings
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
import Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
import Network.AWS.MediaConvert.Types.AudioCodecSettings
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.AudioNormalizationSettings
import Network.AWS.MediaConvert.Types.AudioSelector
import Network.AWS.MediaConvert.Types.AudioSelectorGroup
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
import Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
import Network.AWS.MediaConvert.Types.Av1Settings
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.AvcIntraSettings
import Network.AWS.MediaConvert.Types.AvcIntraUhdSettings
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionDescription
import Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionSelector
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
import Network.AWS.MediaConvert.Types.CaptionSourceSettings
import Network.AWS.MediaConvert.Types.ChannelMapping
import Network.AWS.MediaConvert.Types.CmafAdditionalManifest
import Network.AWS.MediaConvert.Types.CmafEncryptionSettings
import Network.AWS.MediaConvert.Types.CmafGroupSettings
import Network.AWS.MediaConvert.Types.CmfcSettings
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.Deinterlacer
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.MediaConvert.Types.DolbyVision
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
import Network.AWS.MediaConvert.Types.DvbNitSettings
import Network.AWS.MediaConvert.Types.DvbSdtSettings
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
import Network.AWS.MediaConvert.Types.DvbTdtSettings
import Network.AWS.MediaConvert.Types.Eac3AtmosSettings
import Network.AWS.MediaConvert.Types.Eac3Settings
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
import Network.AWS.MediaConvert.Types.Endpoint
import Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
import Network.AWS.MediaConvert.Types.F4vSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264Settings
import Network.AWS.MediaConvert.Types.H265QvbrSettings
import Network.AWS.MediaConvert.Types.H265Settings
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.HlsAdditionalManifest
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
import Network.AWS.MediaConvert.Types.HlsEncryptionSettings
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.HlsSettings
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.Id3Insertion
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
import Network.AWS.MediaConvert.Types.Input
import Network.AWS.MediaConvert.Types.InputClipping
import Network.AWS.MediaConvert.Types.InputDecryptionSettings
import Network.AWS.MediaConvert.Types.InputTemplate
import Network.AWS.MediaConvert.Types.InsertableImage
import Network.AWS.MediaConvert.Types.Job
import Network.AWS.MediaConvert.Types.JobMessages
import Network.AWS.MediaConvert.Types.JobSettings
import Network.AWS.MediaConvert.Types.JobTemplate
import Network.AWS.MediaConvert.Types.JobTemplateSettings
import Network.AWS.MediaConvert.Types.M2tsScte35Esam
import Network.AWS.MediaConvert.Types.M2tsSettings
import Network.AWS.MediaConvert.Types.M3u8Settings
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
import Network.AWS.MediaConvert.Types.MovSettings
import Network.AWS.MediaConvert.Types.Mp2Settings
import Network.AWS.MediaConvert.Types.Mp3Settings
import Network.AWS.MediaConvert.Types.Mp4Settings
import Network.AWS.MediaConvert.Types.MpdSettings
import Network.AWS.MediaConvert.Types.Mpeg2Settings
import Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
import Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
import Network.AWS.MediaConvert.Types.MxfSettings
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import Network.AWS.MediaConvert.Types.OpusSettings
import Network.AWS.MediaConvert.Types.Output
import Network.AWS.MediaConvert.Types.OutputChannelMapping
import Network.AWS.MediaConvert.Types.OutputDetail
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.OutputGroupDetail
import Network.AWS.MediaConvert.Types.OutputGroupSettings
import Network.AWS.MediaConvert.Types.OutputSettings
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.Preset
import Network.AWS.MediaConvert.Types.PresetSettings
import Network.AWS.MediaConvert.Types.ProresSettings
import Network.AWS.MediaConvert.Types.Queue
import Network.AWS.MediaConvert.Types.QueueTransition
import Network.AWS.MediaConvert.Types.Rectangle
import Network.AWS.MediaConvert.Types.RemixSettings
import Network.AWS.MediaConvert.Types.ReservationPlan
import Network.AWS.MediaConvert.Types.ReservationPlanSettings
import Network.AWS.MediaConvert.Types.ResourceTags
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
import Network.AWS.MediaConvert.Types.S3DestinationSettings
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
import Network.AWS.MediaConvert.Types.SccDestinationSettings
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
import Network.AWS.MediaConvert.Types.TimecodeBurnin
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import Network.AWS.MediaConvert.Types.Timing
import Network.AWS.MediaConvert.Types.TrackSourceSettings
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings
import Network.AWS.MediaConvert.Types.Vc3Settings
import Network.AWS.MediaConvert.Types.VideoCodecSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import Network.AWS.MediaConvert.Types.VideoDetail
import Network.AWS.MediaConvert.Types.VideoPreprocessor
import Network.AWS.MediaConvert.Types.VideoSelector
import Network.AWS.MediaConvert.Types.VorbisSettings
import Network.AWS.MediaConvert.Types.Vp8Settings
import Network.AWS.MediaConvert.Types.Vp9Settings
import Network.AWS.MediaConvert.Types.WavSettings
import Network.AWS.MediaConvert.UntagResource
import Network.AWS.MediaConvert.UpdateJobTemplate
import Network.AWS.MediaConvert.UpdatePreset
import Network.AWS.MediaConvert.UpdateQueue
