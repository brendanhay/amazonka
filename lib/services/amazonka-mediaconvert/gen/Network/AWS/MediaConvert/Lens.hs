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

    -- ** DeletePreset
    deletePreset_name,
    deletePresetResponse_httpStatus,

    -- ** UpdatePreset
    updatePreset_settings,
    updatePreset_category,
    updatePreset_description,
    updatePreset_name,
    updatePresetResponse_preset,
    updatePresetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListQueues
    listQueues_listBy,
    listQueues_nextToken,
    listQueues_order,
    listQueues_maxResults,
    listQueuesResponse_queues,
    listQueuesResponse_nextToken,
    listQueuesResponse_httpStatus,

    -- ** DeleteQueue
    deleteQueue_name,
    deleteQueueResponse_httpStatus,

    -- ** UpdateQueue
    updateQueue_status,
    updateQueue_description,
    updateQueue_reservationPlanSettings,
    updateQueue_name,
    updateQueueResponse_queue,
    updateQueueResponse_httpStatus,

    -- ** GetPreset
    getPreset_name,
    getPresetResponse_preset,
    getPresetResponse_httpStatus,

    -- ** CreateJob
    createJob_jobTemplate,
    createJob_accelerationSettings,
    createJob_priority,
    createJob_statusUpdateInterval,
    createJob_hopDestinations,
    createJob_simulateReservedQueue,
    createJob_queue,
    createJob_userMetadata,
    createJob_billingTagsSource,
    createJob_clientRequestToken,
    createJob_tags,
    createJob_role,
    createJob_settings,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** ListJobs
    listJobs_status,
    listJobs_queue,
    listJobs_nextToken,
    listJobs_order,
    listJobs_maxResults,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** PutPolicy
    putPolicy_policy,
    putPolicyResponse_policy,
    putPolicyResponse_httpStatus,

    -- ** GetJob
    getJob_id,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicyResponse_httpStatus,

    -- ** CreatePreset
    createPreset_category,
    createPreset_description,
    createPreset_tags,
    createPreset_settings,
    createPreset_name,
    createPresetResponse_preset,
    createPresetResponse_httpStatus,

    -- ** ListPresets
    listPresets_category,
    listPresets_listBy,
    listPresets_nextToken,
    listPresets_order,
    listPresets_maxResults,
    listPresetsResponse_presets,
    listPresetsResponse_nextToken,
    listPresetsResponse_httpStatus,

    -- ** DisassociateCertificate
    disassociateCertificate_arn,
    disassociateCertificateResponse_httpStatus,

    -- ** GetQueue
    getQueue_name,
    getQueueResponse_queue,
    getQueueResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_mode,
    describeEndpoints_nextToken,
    describeEndpoints_maxResults,
    describeEndpointsResponse_nextToken,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,

    -- ** CreateQueue
    createQueue_status,
    createQueue_pricingPlan,
    createQueue_description,
    createQueue_reservationPlanSettings,
    createQueue_tags,
    createQueue_name,
    createQueueResponse_queue,
    createQueueResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateJobTemplate
    createJobTemplate_accelerationSettings,
    createJobTemplate_priority,
    createJobTemplate_statusUpdateInterval,
    createJobTemplate_category,
    createJobTemplate_hopDestinations,
    createJobTemplate_queue,
    createJobTemplate_description,
    createJobTemplate_tags,
    createJobTemplate_settings,
    createJobTemplate_name,
    createJobTemplateResponse_jobTemplate,
    createJobTemplateResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_arn,
    untagResourceResponse_httpStatus,

    -- ** DeleteJobTemplate
    deleteJobTemplate_name,
    deleteJobTemplateResponse_httpStatus,

    -- ** UpdateJobTemplate
    updateJobTemplate_accelerationSettings,
    updateJobTemplate_priority,
    updateJobTemplate_statusUpdateInterval,
    updateJobTemplate_settings,
    updateJobTemplate_category,
    updateJobTemplate_hopDestinations,
    updateJobTemplate_queue,
    updateJobTemplate_description,
    updateJobTemplate_name,
    updateJobTemplateResponse_jobTemplate,
    updateJobTemplateResponse_httpStatus,

    -- ** GetPolicy
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_category,
    listJobTemplates_listBy,
    listJobTemplates_nextToken,
    listJobTemplates_order,
    listJobTemplates_maxResults,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_httpStatus,

    -- ** GetJobTemplate
    getJobTemplate_name,
    getJobTemplateResponse_jobTemplate,
    getJobTemplateResponse_httpStatus,

    -- ** AssociateCertificate
    associateCertificate_arn,
    associateCertificateResponse_httpStatus,

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_audioDescriptionBroadcasterMix,
    aacSettings_rawFormat,
    aacSettings_codingMode,
    aacSettings_rateControlMode,
    aacSettings_sampleRate,
    aacSettings_specification,
    aacSettings_codecProfile,
    aacSettings_bitrate,
    aacSettings_vbrQuality,

    -- ** Ac3Settings
    ac3Settings_lfeFilter,
    ac3Settings_dynamicRangeCompressionLine,
    ac3Settings_metadataControl,
    ac3Settings_bitstreamMode,
    ac3Settings_dynamicRangeCompressionRf,
    ac3Settings_codingMode,
    ac3Settings_sampleRate,
    ac3Settings_dynamicRangeCompressionProfile,
    ac3Settings_bitrate,
    ac3Settings_dialnorm,

    -- ** AccelerationSettings
    accelerationSettings_mode,

    -- ** AiffSettings
    aiffSettings_bitDepth,
    aiffSettings_channels,
    aiffSettings_sampleRate,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_convert608To708,
    ancillarySourceSettings_terminateCaptions,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** AudioChannelTaggingSettings
    audioChannelTaggingSettings_channelTag,

    -- ** AudioCodecSettings
    audioCodecSettings_aiffSettings,
    audioCodecSettings_codec,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_opusSettings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_wavSettings,
    audioCodecSettings_eac3AtmosSettings,
    audioCodecSettings_mp3Settings,
    audioCodecSettings_vorbisSettings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_eac3Settings,

    -- ** AudioDescription
    audioDescription_audioSourceName,
    audioDescription_customLanguageCode,
    audioDescription_languageCode,
    audioDescription_audioChannelTaggingSettings,
    audioDescription_audioType,
    audioDescription_audioNormalizationSettings,
    audioDescription_languageCodeControl,
    audioDescription_codecSettings,
    audioDescription_streamName,
    audioDescription_remixSettings,
    audioDescription_audioTypeControl,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_peakCalculation,
    audioNormalizationSettings_correctionGateLevel,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_loudnessLogging,

    -- ** AudioSelector
    audioSelector_tracks,
    audioSelector_customLanguageCode,
    audioSelector_programSelection,
    audioSelector_languageCode,
    audioSelector_offset,
    audioSelector_defaultSelection,
    audioSelector_pids,
    audioSelector_hlsRenditionGroupSettings,
    audioSelector_selectorType,
    audioSelector_externalAudioFileInput,
    audioSelector_remixSettings,

    -- ** AudioSelectorGroup
    audioSelectorGroup_audioSelectorNames,

    -- ** AutomatedAbrSettings
    automatedAbrSettings_maxRenditions,
    automatedAbrSettings_maxAbrBitrate,
    automatedAbrSettings_minAbrBitrate,

    -- ** AutomatedEncodingSettings
    automatedEncodingSettings_abrSettings,

    -- ** Av1QvbrSettings
    av1QvbrSettings_qvbrQualityLevelFineTune,
    av1QvbrSettings_qvbrQualityLevel,

    -- ** Av1Settings
    av1Settings_gopSize,
    av1Settings_numberBFramesBetweenReferenceFrames,
    av1Settings_slices,
    av1Settings_rateControlMode,
    av1Settings_qvbrSettings,
    av1Settings_framerateDenominator,
    av1Settings_framerateConversionAlgorithm,
    av1Settings_framerateControl,
    av1Settings_adaptiveQuantization,
    av1Settings_framerateNumerator,
    av1Settings_maxBitrate,
    av1Settings_spatialAdaptiveQuantization,

    -- ** AvailBlanking
    availBlanking_availBlankingImage,

    -- ** AvcIntraSettings
    avcIntraSettings_slowPal,
    avcIntraSettings_avcIntraUhdSettings,
    avcIntraSettings_telecine,
    avcIntraSettings_interlaceMode,
    avcIntraSettings_scanTypeConversionMode,
    avcIntraSettings_avcIntraClass,
    avcIntraSettings_framerateDenominator,
    avcIntraSettings_framerateConversionAlgorithm,
    avcIntraSettings_framerateControl,
    avcIntraSettings_framerateNumerator,

    -- ** AvcIntraUhdSettings
    avcIntraUhdSettings_qualityTuningLevel,

    -- ** BurninDestinationSettings
    burninDestinationSettings_backgroundOpacity,
    burninDestinationSettings_fallbackFont,
    burninDestinationSettings_fontOpacity,
    burninDestinationSettings_shadowYOffset,
    burninDestinationSettings_fontResolution,
    burninDestinationSettings_yPosition,
    burninDestinationSettings_backgroundColor,
    burninDestinationSettings_shadowXOffset,
    burninDestinationSettings_fontSize,
    burninDestinationSettings_xPosition,
    burninDestinationSettings_teletextSpacing,
    burninDestinationSettings_fontScript,
    burninDestinationSettings_alignment,
    burninDestinationSettings_shadowOpacity,
    burninDestinationSettings_applyFontColor,
    burninDestinationSettings_stylePassthrough,
    burninDestinationSettings_outlineColor,
    burninDestinationSettings_outlineSize,
    burninDestinationSettings_shadowColor,
    burninDestinationSettings_hexFontColor,
    burninDestinationSettings_fontColor,

    -- ** CaptionDescription
    captionDescription_captionSelectorName,
    captionDescription_customLanguageCode,
    captionDescription_languageCode,
    captionDescription_destinationSettings,
    captionDescription_languageDescription,

    -- ** CaptionDescriptionPreset
    captionDescriptionPreset_customLanguageCode,
    captionDescriptionPreset_languageCode,
    captionDescriptionPreset_destinationSettings,
    captionDescriptionPreset_languageDescription,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_srtDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_destinationType,
    captionDestinationSettings_webvttDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_sccDestinationSettings,
    captionDestinationSettings_burninDestinationSettings,
    captionDestinationSettings_imscDestinationSettings,

    -- ** CaptionSelector
    captionSelector_customLanguageCode,
    captionSelector_languageCode,
    captionSelector_sourceSettings,

    -- ** CaptionSourceFramerate
    captionSourceFramerate_framerateDenominator,
    captionSourceFramerate_framerateNumerator,

    -- ** CaptionSourceSettings
    captionSourceSettings_teletextSourceSettings,
    captionSourceSettings_sourceType,
    captionSourceSettings_fileSourceSettings,
    captionSourceSettings_webvttHlsSourceSettings,
    captionSourceSettings_dvbSubSourceSettings,
    captionSourceSettings_trackSourceSettings,
    captionSourceSettings_ancillarySourceSettings,
    captionSourceSettings_embeddedSourceSettings,

    -- ** ChannelMapping
    channelMapping_outputChannels,

    -- ** CmafAdditionalManifest
    cmafAdditionalManifest_manifestNameModifier,
    cmafAdditionalManifest_selectedOutputs,

    -- ** CmafEncryptionSettings
    cmafEncryptionSettings_encryptionMethod,
    cmafEncryptionSettings_constantInitializationVector,
    cmafEncryptionSettings_type,
    cmafEncryptionSettings_staticKeyProvider,
    cmafEncryptionSettings_spekeKeyProvider,
    cmafEncryptionSettings_initializationVectorInManifest,

    -- ** CmafGroupSettings
    cmafGroupSettings_fragmentLength,
    cmafGroupSettings_segmentControl,
    cmafGroupSettings_destination,
    cmafGroupSettings_minBufferTime,
    cmafGroupSettings_mpdProfile,
    cmafGroupSettings_targetDurationCompatibilityMode,
    cmafGroupSettings_imageBasedTrickPlay,
    cmafGroupSettings_writeHlsManifest,
    cmafGroupSettings_additionalManifests,
    cmafGroupSettings_segmentLengthControl,
    cmafGroupSettings_imageBasedTrickPlaySettings,
    cmafGroupSettings_codecSpecification,
    cmafGroupSettings_baseUrl,
    cmafGroupSettings_destinationSettings,
    cmafGroupSettings_minFinalSegmentLength,
    cmafGroupSettings_writeDashManifest,
    cmafGroupSettings_encryption,
    cmafGroupSettings_segmentLength,
    cmafGroupSettings_ptsOffsetHandlingForBFrames,
    cmafGroupSettings_manifestDurationFormat,
    cmafGroupSettings_clientCache,
    cmafGroupSettings_writeSegmentTimelineInRepresentation,
    cmafGroupSettings_streamInfResolution,
    cmafGroupSettings_manifestCompression,

    -- ** CmafImageBasedTrickPlaySettings
    cmafImageBasedTrickPlaySettings_tileWidth,
    cmafImageBasedTrickPlaySettings_thumbnailHeight,
    cmafImageBasedTrickPlaySettings_intervalCadence,
    cmafImageBasedTrickPlaySettings_thumbnailWidth,
    cmafImageBasedTrickPlaySettings_thumbnailInterval,
    cmafImageBasedTrickPlaySettings_tileHeight,

    -- ** CmfcSettings
    cmfcSettings_descriptiveVideoServiceFlag,
    cmfcSettings_audioRenditionSets,
    cmfcSettings_iFrameOnlyManifest,
    cmfcSettings_scte35Esam,
    cmfcSettings_audioDuration,
    cmfcSettings_audioGroupId,
    cmfcSettings_scte35Source,
    cmfcSettings_audioTrackType,

    -- ** ColorCorrector
    colorCorrector_saturation,
    colorCorrector_hue,
    colorCorrector_sampleRangeConversion,
    colorCorrector_colorSpaceConversion,
    colorCorrector_hdr10Metadata,
    colorCorrector_contrast,
    colorCorrector_brightness,

    -- ** ContainerSettings
    containerSettings_m2tsSettings,
    containerSettings_mxfSettings,
    containerSettings_m3u8Settings,
    containerSettings_cmfcSettings,
    containerSettings_movSettings,
    containerSettings_mp4Settings,
    containerSettings_mpdSettings,
    containerSettings_container,
    containerSettings_f4vSettings,

    -- ** DashAdditionalManifest
    dashAdditionalManifest_manifestNameModifier,
    dashAdditionalManifest_selectedOutputs,

    -- ** DashIsoEncryptionSettings
    dashIsoEncryptionSettings_playbackDeviceCompatibility,
    dashIsoEncryptionSettings_spekeKeyProvider,

    -- ** DashIsoGroupSettings
    dashIsoGroupSettings_fragmentLength,
    dashIsoGroupSettings_segmentControl,
    dashIsoGroupSettings_destination,
    dashIsoGroupSettings_hbbtvCompliance,
    dashIsoGroupSettings_minBufferTime,
    dashIsoGroupSettings_mpdProfile,
    dashIsoGroupSettings_imageBasedTrickPlay,
    dashIsoGroupSettings_additionalManifests,
    dashIsoGroupSettings_segmentLengthControl,
    dashIsoGroupSettings_imageBasedTrickPlaySettings,
    dashIsoGroupSettings_baseUrl,
    dashIsoGroupSettings_destinationSettings,
    dashIsoGroupSettings_minFinalSegmentLength,
    dashIsoGroupSettings_audioChannelConfigSchemeIdUri,
    dashIsoGroupSettings_encryption,
    dashIsoGroupSettings_segmentLength,
    dashIsoGroupSettings_ptsOffsetHandlingForBFrames,
    dashIsoGroupSettings_writeSegmentTimelineInRepresentation,

    -- ** DashIsoImageBasedTrickPlaySettings
    dashIsoImageBasedTrickPlaySettings_tileWidth,
    dashIsoImageBasedTrickPlaySettings_thumbnailHeight,
    dashIsoImageBasedTrickPlaySettings_intervalCadence,
    dashIsoImageBasedTrickPlaySettings_thumbnailWidth,
    dashIsoImageBasedTrickPlaySettings_thumbnailInterval,
    dashIsoImageBasedTrickPlaySettings_tileHeight,

    -- ** Deinterlacer
    deinterlacer_control,
    deinterlacer_mode,
    deinterlacer_algorithm,

    -- ** DestinationSettings
    destinationSettings_s3Settings,

    -- ** DolbyVision
    dolbyVision_profile,
    dolbyVision_l6Mode,
    dolbyVision_l6Metadata,

    -- ** DolbyVisionLevel6Metadata
    dolbyVisionLevel6Metadata_maxFall,
    dolbyVisionLevel6Metadata_maxCll,

    -- ** DvbNitSettings
    dvbNitSettings_networkId,
    dvbNitSettings_networkName,
    dvbNitSettings_nitInterval,

    -- ** DvbSdtSettings
    dvbSdtSettings_sdtInterval,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_ddsHandling,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_fallbackFont,
    dvbSubDestinationSettings_height,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_shadowYOffset,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_yPosition,
    dvbSubDestinationSettings_ddsYCoordinate,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_width,
    dvbSubDestinationSettings_ddsXCoordinate,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_teletextSpacing,
    dvbSubDestinationSettings_fontScript,
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_applyFontColor,
    dvbSubDestinationSettings_stylePassthrough,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_hexFontColor,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_subtitlingType,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_tdtInterval,

    -- ** Eac3AtmosSettings
    eac3AtmosSettings_stereoDownmix,
    eac3AtmosSettings_loRoCenterMixLevel,
    eac3AtmosSettings_ltRtCenterMixLevel,
    eac3AtmosSettings_dynamicRangeCompressionLine,
    eac3AtmosSettings_ltRtSurroundMixLevel,
    eac3AtmosSettings_loRoSurroundMixLevel,
    eac3AtmosSettings_dynamicRangeControl,
    eac3AtmosSettings_bitstreamMode,
    eac3AtmosSettings_dynamicRangeCompressionRf,
    eac3AtmosSettings_codingMode,
    eac3AtmosSettings_sampleRate,
    eac3AtmosSettings_speechThreshold,
    eac3AtmosSettings_bitrate,
    eac3AtmosSettings_dialogueIntelligence,
    eac3AtmosSettings_meteringMode,
    eac3AtmosSettings_surroundExMode,
    eac3AtmosSettings_downmixControl,

    -- ** Eac3Settings
    eac3Settings_stereoDownmix,
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_lfeFilter,
    eac3Settings_dynamicRangeCompressionLine,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_metadataControl,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_surroundMode,
    eac3Settings_attenuationControl,
    eac3Settings_passthroughControl,
    eac3Settings_bitstreamMode,
    eac3Settings_lfeControl,
    eac3Settings_dynamicRangeCompressionRf,
    eac3Settings_codingMode,
    eac3Settings_sampleRate,
    eac3Settings_dcFilter,
    eac3Settings_bitrate,
    eac3Settings_phaseControl,
    eac3Settings_surroundExMode,
    eac3Settings_dialnorm,

    -- ** EmbeddedDestinationSettings
    embeddedDestinationSettings_destination608ChannelNumber,
    embeddedDestinationSettings_destination708ServiceNumber,

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_terminateCaptions,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- ** Endpoint
    endpoint_url,

    -- ** EsamManifestConfirmConditionNotification
    esamManifestConfirmConditionNotification_mccXml,

    -- ** EsamSettings
    esamSettings_manifestConfirmConditionNotification,
    esamSettings_responseSignalPreroll,
    esamSettings_signalProcessingNotification,

    -- ** EsamSignalProcessingNotification
    esamSignalProcessingNotification_sccXml,

    -- ** ExtendedDataServices
    extendedDataServices_vchipAction,
    extendedDataServices_copyProtectionAction,

    -- ** F4vSettings
    f4vSettings_moovPlacement,

    -- ** FileGroupSettings
    fileGroupSettings_destination,
    fileGroupSettings_destinationSettings,

    -- ** FileSourceSettings
    fileSourceSettings_framerate,
    fileSourceSettings_convert608To708,
    fileSourceSettings_timeDelta,
    fileSourceSettings_sourceFile,

    -- ** FrameCaptureSettings
    frameCaptureSettings_quality,
    frameCaptureSettings_framerateDenominator,
    frameCaptureSettings_maxCaptures,
    frameCaptureSettings_framerateNumerator,

    -- ** H264QvbrSettings
    h264QvbrSettings_qvbrQualityLevelFineTune,
    h264QvbrSettings_maxAverageBitrate,
    h264QvbrSettings_qvbrQualityLevel,

    -- ** H264Settings
    h264Settings_unregisteredSeiTimecode,
    h264Settings_qualityTuningLevel,
    h264Settings_temporalAdaptiveQuantization,
    h264Settings_sceneChangeDetect,
    h264Settings_hrdBufferInitialFillPercentage,
    h264Settings_slowPal,
    h264Settings_parNumerator,
    h264Settings_gopSize,
    h264Settings_numberBFramesBetweenReferenceFrames,
    h264Settings_gopSizeUnits,
    h264Settings_hrdBufferSize,
    h264Settings_slices,
    h264Settings_rateControlMode,
    h264Settings_numberReferenceFrames,
    h264Settings_telecine,
    h264Settings_dynamicSubGop,
    h264Settings_minIInterval,
    h264Settings_interlaceMode,
    h264Settings_parControl,
    h264Settings_repeatPps,
    h264Settings_scanTypeConversionMode,
    h264Settings_flickerAdaptiveQuantization,
    h264Settings_qvbrSettings,
    h264Settings_softness,
    h264Settings_codecProfile,
    h264Settings_bitrate,
    h264Settings_framerateDenominator,
    h264Settings_framerateConversionAlgorithm,
    h264Settings_codecLevel,
    h264Settings_entropyEncoding,
    h264Settings_framerateControl,
    h264Settings_adaptiveQuantization,
    h264Settings_framerateNumerator,
    h264Settings_gopBReference,
    h264Settings_maxBitrate,
    h264Settings_syntax,
    h264Settings_fieldEncoding,
    h264Settings_gopClosedCadence,
    h264Settings_parDenominator,
    h264Settings_spatialAdaptiveQuantization,

    -- ** H265QvbrSettings
    h265QvbrSettings_qvbrQualityLevelFineTune,
    h265QvbrSettings_maxAverageBitrate,
    h265QvbrSettings_qvbrQualityLevel,

    -- ** H265Settings
    h265Settings_unregisteredSeiTimecode,
    h265Settings_qualityTuningLevel,
    h265Settings_temporalAdaptiveQuantization,
    h265Settings_sceneChangeDetect,
    h265Settings_hrdBufferInitialFillPercentage,
    h265Settings_tiles,
    h265Settings_slowPal,
    h265Settings_temporalIds,
    h265Settings_parNumerator,
    h265Settings_gopSize,
    h265Settings_numberBFramesBetweenReferenceFrames,
    h265Settings_gopSizeUnits,
    h265Settings_hrdBufferSize,
    h265Settings_slices,
    h265Settings_alternateTransferFunctionSei,
    h265Settings_rateControlMode,
    h265Settings_numberReferenceFrames,
    h265Settings_telecine,
    h265Settings_dynamicSubGop,
    h265Settings_minIInterval,
    h265Settings_interlaceMode,
    h265Settings_parControl,
    h265Settings_scanTypeConversionMode,
    h265Settings_flickerAdaptiveQuantization,
    h265Settings_qvbrSettings,
    h265Settings_sampleAdaptiveOffsetFilterMode,
    h265Settings_codecProfile,
    h265Settings_bitrate,
    h265Settings_framerateDenominator,
    h265Settings_framerateConversionAlgorithm,
    h265Settings_codecLevel,
    h265Settings_framerateControl,
    h265Settings_writeMp4PackagingType,
    h265Settings_adaptiveQuantization,
    h265Settings_framerateNumerator,
    h265Settings_gopBReference,
    h265Settings_maxBitrate,
    h265Settings_gopClosedCadence,
    h265Settings_parDenominator,
    h265Settings_spatialAdaptiveQuantization,

    -- ** Hdr10Metadata
    hdr10Metadata_redPrimaryX,
    hdr10Metadata_bluePrimaryX,
    hdr10Metadata_maxFrameAverageLightLevel,
    hdr10Metadata_whitePointY,
    hdr10Metadata_maxContentLightLevel,
    hdr10Metadata_whitePointX,
    hdr10Metadata_bluePrimaryY,
    hdr10Metadata_greenPrimaryY,
    hdr10Metadata_greenPrimaryX,
    hdr10Metadata_minLuminance,
    hdr10Metadata_redPrimaryY,
    hdr10Metadata_maxLuminance,

    -- ** Hdr10Plus
    hdr10Plus_masteringMonitorNits,
    hdr10Plus_targetMonitorNits,

    -- ** HlsAdditionalManifest
    hlsAdditionalManifest_manifestNameModifier,
    hlsAdditionalManifest_selectedOutputs,

    -- ** HlsCaptionLanguageMapping
    hlsCaptionLanguageMapping_customLanguageCode,
    hlsCaptionLanguageMapping_languageCode,
    hlsCaptionLanguageMapping_languageDescription,
    hlsCaptionLanguageMapping_captionChannel,

    -- ** HlsEncryptionSettings
    hlsEncryptionSettings_offlineEncrypted,
    hlsEncryptionSettings_encryptionMethod,
    hlsEncryptionSettings_constantInitializationVector,
    hlsEncryptionSettings_type,
    hlsEncryptionSettings_staticKeyProvider,
    hlsEncryptionSettings_spekeKeyProvider,
    hlsEncryptionSettings_initializationVectorInManifest,

    -- ** HlsGroupSettings
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_segmentControl,
    hlsGroupSettings_destination,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_targetDurationCompatibilityMode,
    hlsGroupSettings_imageBasedTrickPlay,
    hlsGroupSettings_additionalManifests,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_segmentLengthControl,
    hlsGroupSettings_imageBasedTrickPlaySettings,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_baseUrl,
    hlsGroupSettings_destinationSettings,
    hlsGroupSettings_minFinalSegmentLength,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_encryption,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_audioOnlyHeader,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_manifestCompression,

    -- ** HlsImageBasedTrickPlaySettings
    hlsImageBasedTrickPlaySettings_tileWidth,
    hlsImageBasedTrickPlaySettings_thumbnailHeight,
    hlsImageBasedTrickPlaySettings_intervalCadence,
    hlsImageBasedTrickPlaySettings_thumbnailWidth,
    hlsImageBasedTrickPlaySettings_thumbnailInterval,
    hlsImageBasedTrickPlaySettings_tileHeight,

    -- ** HlsRenditionGroupSettings
    hlsRenditionGroupSettings_renditionName,
    hlsRenditionGroupSettings_renditionGroupId,
    hlsRenditionGroupSettings_renditionLanguageCode,

    -- ** HlsSettings
    hlsSettings_descriptiveVideoServiceFlag,
    hlsSettings_audioRenditionSets,
    hlsSettings_iFrameOnlyManifest,
    hlsSettings_audioGroupId,
    hlsSettings_segmentModifier,
    hlsSettings_audioOnlyContainer,
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
    input_videoSelector,
    input_supplementalImps,
    input_programNumber,
    input_audioSelectorGroups,
    input_timecodeSource,
    input_audioSelectors,
    input_decryptionSettings,
    input_deblockFilter,
    input_inputClippings,
    input_crop,
    input_denoiseFilter,
    input_imageInserter,
    input_filterStrength,
    input_psiControl,
    input_captionSelectors,
    input_fileInput,
    input_timecodeStart,
    input_inputScanType,
    input_position,
    input_filterEnable,

    -- ** InputClipping
    inputClipping_endTimecode,
    inputClipping_startTimecode,

    -- ** InputDecryptionSettings
    inputDecryptionSettings_encryptedDecryptionKey,
    inputDecryptionSettings_kmsKeyRegion,
    inputDecryptionSettings_decryptionMode,
    inputDecryptionSettings_initializationVector,

    -- ** InputTemplate
    inputTemplate_videoSelector,
    inputTemplate_programNumber,
    inputTemplate_audioSelectorGroups,
    inputTemplate_timecodeSource,
    inputTemplate_audioSelectors,
    inputTemplate_deblockFilter,
    inputTemplate_inputClippings,
    inputTemplate_crop,
    inputTemplate_denoiseFilter,
    inputTemplate_imageInserter,
    inputTemplate_filterStrength,
    inputTemplate_psiControl,
    inputTemplate_captionSelectors,
    inputTemplate_timecodeStart,
    inputTemplate_inputScanType,
    inputTemplate_position,
    inputTemplate_filterEnable,

    -- ** InsertableImage
    insertableImage_imageX,
    insertableImage_height,
    insertableImage_startTime,
    insertableImage_fadeOut,
    insertableImage_width,
    insertableImage_opacity,
    insertableImage_layer,
    insertableImage_duration,
    insertableImage_imageY,
    insertableImage_imageInserterInput,
    insertableImage_fadeIn,

    -- ** Job
    job_status,
    job_jobTemplate,
    job_accelerationSettings,
    job_priority,
    job_statusUpdateInterval,
    job_arn,
    job_createdAt,
    job_hopDestinations,
    job_retryCount,
    job_simulateReservedQueue,
    job_currentPhase,
    job_queue,
    job_userMetadata,
    job_billingTagsSource,
    job_outputGroupDetails,
    job_errorCode,
    job_queueTransitions,
    job_id,
    job_jobPercentComplete,
    job_timing,
    job_messages,
    job_errorMessage,
    job_accelerationStatus,
    job_role,
    job_settings,

    -- ** JobMessages
    jobMessages_warning,
    jobMessages_info,

    -- ** JobSettings
    jobSettings_nielsenNonLinearWatermark,
    jobSettings_esam,
    jobSettings_inputs,
    jobSettings_timedMetadataInsertion,
    jobSettings_nielsenConfiguration,
    jobSettings_availBlanking,
    jobSettings_extendedDataServices,
    jobSettings_motionImageInserter,
    jobSettings_timecodeConfig,
    jobSettings_outputGroups,
    jobSettings_adAvailOffset,
    jobSettings_kantarWatermark,

    -- ** JobTemplate
    jobTemplate_accelerationSettings,
    jobTemplate_lastUpdated,
    jobTemplate_priority,
    jobTemplate_statusUpdateInterval,
    jobTemplate_arn,
    jobTemplate_createdAt,
    jobTemplate_category,
    jobTemplate_hopDestinations,
    jobTemplate_queue,
    jobTemplate_type,
    jobTemplate_description,
    jobTemplate_settings,
    jobTemplate_name,

    -- ** JobTemplateSettings
    jobTemplateSettings_nielsenNonLinearWatermark,
    jobTemplateSettings_esam,
    jobTemplateSettings_inputs,
    jobTemplateSettings_timedMetadataInsertion,
    jobTemplateSettings_nielsenConfiguration,
    jobTemplateSettings_availBlanking,
    jobTemplateSettings_extendedDataServices,
    jobTemplateSettings_motionImageInserter,
    jobTemplateSettings_timecodeConfig,
    jobTemplateSettings_outputGroups,
    jobTemplateSettings_adAvailOffset,
    jobTemplateSettings_kantarWatermark,

    -- ** KantarWatermarkSettings
    kantarWatermarkSettings_metadata7,
    kantarWatermarkSettings_kantarServerUrl,
    kantarWatermarkSettings_kantarLicenseId,
    kantarWatermarkSettings_logDestination,
    kantarWatermarkSettings_fileOffset,
    kantarWatermarkSettings_metadata6,
    kantarWatermarkSettings_metadata3,
    kantarWatermarkSettings_credentialsSecretName,
    kantarWatermarkSettings_channelName,
    kantarWatermarkSettings_contentReference,
    kantarWatermarkSettings_metadata8,
    kantarWatermarkSettings_metadata5,
    kantarWatermarkSettings_metadata4,

    -- ** M2tsScte35Esam
    m2tsScte35Esam_scte35EsamPid,

    -- ** M2tsSettings
    m2tsSettings_pmtPid,
    m2tsSettings_videoPid,
    m2tsSettings_bufferModel,
    m2tsSettings_programNumber,
    m2tsSettings_scte35Pid,
    m2tsSettings_minEbpInterval,
    m2tsSettings_transportStreamId,
    m2tsSettings_maxPcrInterval,
    m2tsSettings_fragmentTime,
    m2tsSettings_privateMetadataPid,
    m2tsSettings_scte35Esam,
    m2tsSettings_audioDuration,
    m2tsSettings_pmtInterval,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_audioBufferModel,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_pcrPid,
    m2tsSettings_segmentationMarkers,
    m2tsSettings_dvbSubPids,
    m2tsSettings_scte35Source,
    m2tsSettings_patInterval,
    m2tsSettings_forceTsVideoEbpOrder,
    m2tsSettings_esRateInPes,
    m2tsSettings_bitrate,
    m2tsSettings_audioPids,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_nielsenId3,
    m2tsSettings_dataPTSControl,
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
    m3u8Settings_programNumber,
    m3u8Settings_scte35Pid,
    m3u8Settings_transportStreamId,
    m3u8Settings_maxPcrInterval,
    m3u8Settings_privateMetadataPid,
    m3u8Settings_audioDuration,
    m3u8Settings_pmtInterval,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_pcrPid,
    m3u8Settings_timedMetadata,
    m3u8Settings_scte35Source,
    m3u8Settings_patInterval,
    m3u8Settings_audioPids,
    m3u8Settings_nielsenId3,
    m3u8Settings_dataPTSControl,
    m3u8Settings_pcrControl,

    -- ** MotionImageInserter
    motionImageInserter_framerate,
    motionImageInserter_startTime,
    motionImageInserter_offset,
    motionImageInserter_input,
    motionImageInserter_insertionMode,
    motionImageInserter_playback,

    -- ** MotionImageInsertionFramerate
    motionImageInsertionFramerate_framerateDenominator,
    motionImageInsertionFramerate_framerateNumerator,

    -- ** MotionImageInsertionOffset
    motionImageInsertionOffset_imageX,
    motionImageInsertionOffset_imageY,

    -- ** MovSettings
    movSettings_reference,
    movSettings_cslgAtom,
    movSettings_mpeg2FourCCControl,
    movSettings_paddingControl,
    movSettings_clapAtom,

    -- ** Mp2Settings
    mp2Settings_channels,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- ** Mp3Settings
    mp3Settings_channels,
    mp3Settings_rateControlMode,
    mp3Settings_sampleRate,
    mp3Settings_bitrate,
    mp3Settings_vbrQuality,

    -- ** Mp4Settings
    mp4Settings_moovPlacement,
    mp4Settings_cttsVersion,
    mp4Settings_freeSpaceBox,
    mp4Settings_audioDuration,
    mp4Settings_mp4MajorBrand,
    mp4Settings_cslgAtom,

    -- ** MpdSettings
    mpdSettings_scte35Esam,
    mpdSettings_audioDuration,
    mpdSettings_scte35Source,
    mpdSettings_accessibilityCaptionHints,
    mpdSettings_captionContainerType,

    -- ** Mpeg2Settings
    mpeg2Settings_qualityTuningLevel,
    mpeg2Settings_temporalAdaptiveQuantization,
    mpeg2Settings_sceneChangeDetect,
    mpeg2Settings_hrdBufferInitialFillPercentage,
    mpeg2Settings_slowPal,
    mpeg2Settings_parNumerator,
    mpeg2Settings_gopSize,
    mpeg2Settings_numberBFramesBetweenReferenceFrames,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_hrdBufferSize,
    mpeg2Settings_rateControlMode,
    mpeg2Settings_telecine,
    mpeg2Settings_intraDcPrecision,
    mpeg2Settings_dynamicSubGop,
    mpeg2Settings_minIInterval,
    mpeg2Settings_interlaceMode,
    mpeg2Settings_parControl,
    mpeg2Settings_scanTypeConversionMode,
    mpeg2Settings_softness,
    mpeg2Settings_codecProfile,
    mpeg2Settings_bitrate,
    mpeg2Settings_framerateDenominator,
    mpeg2Settings_framerateConversionAlgorithm,
    mpeg2Settings_codecLevel,
    mpeg2Settings_framerateControl,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_maxBitrate,
    mpeg2Settings_syntax,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_parDenominator,
    mpeg2Settings_spatialAdaptiveQuantization,

    -- ** MsSmoothAdditionalManifest
    msSmoothAdditionalManifest_manifestNameModifier,
    msSmoothAdditionalManifest_selectedOutputs,

    -- ** MsSmoothEncryptionSettings
    msSmoothEncryptionSettings_spekeKeyProvider,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_manifestEncoding,
    msSmoothGroupSettings_destination,
    msSmoothGroupSettings_audioDeduplication,
    msSmoothGroupSettings_additionalManifests,
    msSmoothGroupSettings_fragmentLengthControl,
    msSmoothGroupSettings_destinationSettings,
    msSmoothGroupSettings_encryption,

    -- ** MxfSettings
    mxfSettings_xavcProfileSettings,
    mxfSettings_afdSignaling,
    mxfSettings_profile,

    -- ** MxfXavcProfileSettings
    mxfXavcProfileSettings_maxAncDataSize,
    mxfXavcProfileSettings_durationMode,

    -- ** NexGuardFileMarkerSettings
    nexGuardFileMarkerSettings_strength,
    nexGuardFileMarkerSettings_payload,
    nexGuardFileMarkerSettings_preset,
    nexGuardFileMarkerSettings_license,

    -- ** NielsenConfiguration
    nielsenConfiguration_breakoutCode,
    nielsenConfiguration_distributorId,

    -- ** NielsenNonLinearWatermarkSettings
    nielsenNonLinearWatermarkSettings_episodeId,
    nielsenNonLinearWatermarkSettings_activeWatermarkProcess,
    nielsenNonLinearWatermarkSettings_sourceId,
    nielsenNonLinearWatermarkSettings_cbetSourceId,
    nielsenNonLinearWatermarkSettings_ticServerUrl,
    nielsenNonLinearWatermarkSettings_metadataDestination,
    nielsenNonLinearWatermarkSettings_assetName,
    nielsenNonLinearWatermarkSettings_adiFilename,
    nielsenNonLinearWatermarkSettings_assetId,
    nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack,
    nielsenNonLinearWatermarkSettings_sourceWatermarkStatus,

    -- ** NoiseReducer
    noiseReducer_temporalFilterSettings,
    noiseReducer_spatialFilterSettings,
    noiseReducer_filterSettings,
    noiseReducer_filter,

    -- ** NoiseReducerFilterSettings
    noiseReducerFilterSettings_strength,

    -- ** NoiseReducerSpatialFilterSettings
    noiseReducerSpatialFilterSettings_strength,
    noiseReducerSpatialFilterSettings_postFilterSharpenStrength,
    noiseReducerSpatialFilterSettings_speed,

    -- ** NoiseReducerTemporalFilterSettings
    noiseReducerTemporalFilterSettings_postTemporalSharpening,
    noiseReducerTemporalFilterSettings_aggressiveMode,
    noiseReducerTemporalFilterSettings_strength,
    noiseReducerTemporalFilterSettings_speed,

    -- ** OpusSettings
    opusSettings_channels,
    opusSettings_sampleRate,
    opusSettings_bitrate,

    -- ** Output
    output_captionDescriptions,
    output_extension,
    output_videoDescription,
    output_containerSettings,
    output_outputSettings,
    output_preset,
    output_nameModifier,
    output_audioDescriptions,

    -- ** OutputChannelMapping
    outputChannelMapping_inputChannelsFineTune,
    outputChannelMapping_inputChannels,

    -- ** OutputDetail
    outputDetail_videoDetails,
    outputDetail_durationInMs,

    -- ** OutputGroup
    outputGroup_outputGroupSettings,
    outputGroup_outputs,
    outputGroup_customName,
    outputGroup_name,
    outputGroup_automatedEncodingSettings,

    -- ** OutputGroupDetail
    outputGroupDetail_outputDetails,

    -- ** OutputGroupSettings
    outputGroupSettings_fileGroupSettings,
    outputGroupSettings_cmafGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_type,
    outputGroupSettings_dashIsoGroupSettings,

    -- ** OutputSettings
    outputSettings_hlsSettings,

    -- ** PartnerWatermarking
    partnerWatermarking_nexguardFileMarkerSettings,

    -- ** Policy
    policy_s3Inputs,
    policy_httpInputs,
    policy_httpsInputs,

    -- ** Preset
    preset_lastUpdated,
    preset_arn,
    preset_createdAt,
    preset_category,
    preset_type,
    preset_description,
    preset_settings,
    preset_name,

    -- ** PresetSettings
    presetSettings_captionDescriptions,
    presetSettings_videoDescription,
    presetSettings_containerSettings,
    presetSettings_audioDescriptions,

    -- ** ProresSettings
    proresSettings_slowPal,
    proresSettings_parNumerator,
    proresSettings_telecine,
    proresSettings_interlaceMode,
    proresSettings_parControl,
    proresSettings_scanTypeConversionMode,
    proresSettings_codecProfile,
    proresSettings_framerateDenominator,
    proresSettings_framerateConversionAlgorithm,
    proresSettings_framerateControl,
    proresSettings_framerateNumerator,
    proresSettings_chromaSampling,
    proresSettings_parDenominator,

    -- ** Queue
    queue_status,
    queue_lastUpdated,
    queue_arn,
    queue_createdAt,
    queue_reservationPlan,
    queue_pricingPlan,
    queue_submittedJobsCount,
    queue_progressingJobsCount,
    queue_type,
    queue_description,
    queue_name,

    -- ** QueueTransition
    queueTransition_sourceQueue,
    queueTransition_destinationQueue,
    queueTransition_timestamp,

    -- ** Rectangle
    rectangle_height,
    rectangle_width,
    rectangle_x,
    rectangle_y,

    -- ** RemixSettings
    remixSettings_channelMapping,
    remixSettings_channelsIn,
    remixSettings_channelsOut,

    -- ** ReservationPlan
    reservationPlan_status,
    reservationPlan_expiresAt,
    reservationPlan_purchasedAt,
    reservationPlan_commitment,
    reservationPlan_reservedSlots,
    reservationPlan_renewalType,

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
    s3DestinationSettings_accessControl,
    s3DestinationSettings_encryption,

    -- ** S3EncryptionSettings
    s3EncryptionSettings_encryptionType,
    s3EncryptionSettings_kmsKeyArn,
    s3EncryptionSettings_kmsEncryptionContext,

    -- ** SccDestinationSettings
    sccDestinationSettings_framerate,

    -- ** SpekeKeyProvider
    spekeKeyProvider_resourceId,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_url,
    spekeKeyProvider_systemIds,

    -- ** SpekeKeyProviderCmaf
    spekeKeyProviderCmaf_resourceId,
    spekeKeyProviderCmaf_dashSignaledSystemIds,
    spekeKeyProviderCmaf_certificateArn,
    spekeKeyProviderCmaf_url,
    spekeKeyProviderCmaf_hlsSignaledSystemIds,

    -- ** SrtDestinationSettings
    srtDestinationSettings_stylePassthrough,

    -- ** StaticKeyProvider
    staticKeyProvider_staticKeyValue,
    staticKeyProvider_url,
    staticKeyProvider_keyFormat,
    staticKeyProvider_keyFormatVersions,

    -- ** TeletextDestinationSettings
    teletextDestinationSettings_pageTypes,
    teletextDestinationSettings_pageNumber,

    -- ** TeletextSourceSettings
    teletextSourceSettings_pageNumber,

    -- ** TimecodeBurnin
    timecodeBurnin_prefix,
    timecodeBurnin_fontSize,
    timecodeBurnin_position,

    -- ** TimecodeConfig
    timecodeConfig_start,
    timecodeConfig_timestampOffset,
    timecodeConfig_anchor,
    timecodeConfig_source,

    -- ** TimedMetadataInsertion
    timedMetadataInsertion_id3Insertions,

    -- ** Timing
    timing_startTime,
    timing_finishTime,
    timing_submitTime,

    -- ** TrackSourceSettings
    trackSourceSettings_trackNumber,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_stylePassthrough,

    -- ** Vc3Settings
    vc3Settings_slowPal,
    vc3Settings_telecine,
    vc3Settings_interlaceMode,
    vc3Settings_scanTypeConversionMode,
    vc3Settings_framerateDenominator,
    vc3Settings_vc3Class,
    vc3Settings_framerateConversionAlgorithm,
    vc3Settings_framerateControl,
    vc3Settings_framerateNumerator,

    -- ** VideoCodecSettings
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_av1Settings,
    videoCodecSettings_codec,
    videoCodecSettings_xavcSettings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_proresSettings,
    videoCodecSettings_vp9Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_vp8Settings,
    videoCodecSettings_vc3Settings,
    videoCodecSettings_avcIntraSettings,

    -- ** VideoDescription
    videoDescription_timecodeInsertion,
    videoDescription_height,
    videoDescription_afdSignaling,
    videoDescription_sharpness,
    videoDescription_crop,
    videoDescription_width,
    videoDescription_scalingBehavior,
    videoDescription_respondToAfd,
    videoDescription_dropFrameTimecode,
    videoDescription_antiAlias,
    videoDescription_fixedAfd,
    videoDescription_colorMetadata,
    videoDescription_codecSettings,
    videoDescription_videoPreprocessors,
    videoDescription_position,

    -- ** VideoDetail
    videoDetail_heightInPx,
    videoDetail_widthInPx,

    -- ** VideoPreprocessor
    videoPreprocessor_timecodeBurnin,
    videoPreprocessor_dolbyVision,
    videoPreprocessor_colorCorrector,
    videoPreprocessor_deinterlacer,
    videoPreprocessor_noiseReducer,
    videoPreprocessor_imageInserter,
    videoPreprocessor_hdr10Plus,
    videoPreprocessor_partnerWatermarking,

    -- ** VideoSelector
    videoSelector_programNumber,
    videoSelector_alphaBehavior,
    videoSelector_colorSpaceUsage,
    videoSelector_hdr10Metadata,
    videoSelector_pid,
    videoSelector_rotate,
    videoSelector_colorSpace,
    videoSelector_sampleRange,

    -- ** VorbisSettings
    vorbisSettings_channels,
    vorbisSettings_sampleRate,
    vorbisSettings_vbrQuality,

    -- ** Vp8Settings
    vp8Settings_qualityTuningLevel,
    vp8Settings_parNumerator,
    vp8Settings_gopSize,
    vp8Settings_hrdBufferSize,
    vp8Settings_rateControlMode,
    vp8Settings_parControl,
    vp8Settings_bitrate,
    vp8Settings_framerateDenominator,
    vp8Settings_framerateConversionAlgorithm,
    vp8Settings_framerateControl,
    vp8Settings_framerateNumerator,
    vp8Settings_maxBitrate,
    vp8Settings_parDenominator,

    -- ** Vp9Settings
    vp9Settings_qualityTuningLevel,
    vp9Settings_parNumerator,
    vp9Settings_gopSize,
    vp9Settings_hrdBufferSize,
    vp9Settings_rateControlMode,
    vp9Settings_parControl,
    vp9Settings_bitrate,
    vp9Settings_framerateDenominator,
    vp9Settings_framerateConversionAlgorithm,
    vp9Settings_framerateControl,
    vp9Settings_framerateNumerator,
    vp9Settings_maxBitrate,
    vp9Settings_parDenominator,

    -- ** WavSettings
    wavSettings_bitDepth,
    wavSettings_channels,
    wavSettings_format,
    wavSettings_sampleRate,

    -- ** WebvttDestinationSettings
    webvttDestinationSettings_stylePassthrough,

    -- ** WebvttHlsSourceSettings
    webvttHlsSourceSettings_renditionName,
    webvttHlsSourceSettings_renditionGroupId,
    webvttHlsSourceSettings_renditionLanguageCode,

    -- ** Xavc4kIntraCbgProfileSettings
    xavc4kIntraCbgProfileSettings_xavcClass,

    -- ** Xavc4kIntraVbrProfileSettings
    xavc4kIntraVbrProfileSettings_xavcClass,

    -- ** Xavc4kProfileSettings
    xavc4kProfileSettings_qualityTuningLevel,
    xavc4kProfileSettings_hrdBufferSize,
    xavc4kProfileSettings_slices,
    xavc4kProfileSettings_bitrateClass,
    xavc4kProfileSettings_flickerAdaptiveQuantization,
    xavc4kProfileSettings_codecProfile,
    xavc4kProfileSettings_gopBReference,
    xavc4kProfileSettings_gopClosedCadence,

    -- ** XavcHdIntraCbgProfileSettings
    xavcHdIntraCbgProfileSettings_xavcClass,

    -- ** XavcHdProfileSettings
    xavcHdProfileSettings_qualityTuningLevel,
    xavcHdProfileSettings_hrdBufferSize,
    xavcHdProfileSettings_slices,
    xavcHdProfileSettings_bitrateClass,
    xavcHdProfileSettings_telecine,
    xavcHdProfileSettings_interlaceMode,
    xavcHdProfileSettings_flickerAdaptiveQuantization,
    xavcHdProfileSettings_gopBReference,
    xavcHdProfileSettings_gopClosedCadence,

    -- ** XavcSettings
    xavcSettings_temporalAdaptiveQuantization,
    xavcSettings_slowPal,
    xavcSettings_xavc4kProfileSettings,
    xavcSettings_xavcHdIntraCbgProfileSettings,
    xavcSettings_xavc4kIntraVbrProfileSettings,
    xavcSettings_xavc4kIntraCbgProfileSettings,
    xavcSettings_profile,
    xavcSettings_softness,
    xavcSettings_framerateDenominator,
    xavcSettings_framerateConversionAlgorithm,
    xavcSettings_entropyEncoding,
    xavcSettings_framerateControl,
    xavcSettings_adaptiveQuantization,
    xavcSettings_framerateNumerator,
    xavcSettings_xavcHdProfileSettings,
    xavcSettings_spatialAdaptiveQuantization,
  )
where

import Network.AWS.MediaConvert.AssociateCertificate
import Network.AWS.MediaConvert.CancelJob
import Network.AWS.MediaConvert.CreateJob
import Network.AWS.MediaConvert.CreateJobTemplate
import Network.AWS.MediaConvert.CreatePreset
import Network.AWS.MediaConvert.CreateQueue
import Network.AWS.MediaConvert.DeleteJobTemplate
import Network.AWS.MediaConvert.DeletePolicy
import Network.AWS.MediaConvert.DeletePreset
import Network.AWS.MediaConvert.DeleteQueue
import Network.AWS.MediaConvert.DescribeEndpoints
import Network.AWS.MediaConvert.DisassociateCertificate
import Network.AWS.MediaConvert.GetJob
import Network.AWS.MediaConvert.GetJobTemplate
import Network.AWS.MediaConvert.GetPolicy
import Network.AWS.MediaConvert.GetPreset
import Network.AWS.MediaConvert.GetQueue
import Network.AWS.MediaConvert.ListJobTemplates
import Network.AWS.MediaConvert.ListJobs
import Network.AWS.MediaConvert.ListPresets
import Network.AWS.MediaConvert.ListQueues
import Network.AWS.MediaConvert.ListTagsForResource
import Network.AWS.MediaConvert.PutPolicy
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
import Network.AWS.MediaConvert.Types.CmafImageBasedTrickPlaySettings
import Network.AWS.MediaConvert.Types.CmfcSettings
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
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
import Network.AWS.MediaConvert.Types.ExtendedDataServices
import Network.AWS.MediaConvert.Types.F4vSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264Settings
import Network.AWS.MediaConvert.Types.H265QvbrSettings
import Network.AWS.MediaConvert.Types.H265Settings
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.Hdr10Plus
import Network.AWS.MediaConvert.Types.HlsAdditionalManifest
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
import Network.AWS.MediaConvert.Types.HlsEncryptionSettings
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.HlsImageBasedTrickPlaySettings
import Network.AWS.MediaConvert.Types.HlsRenditionGroupSettings
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
import Network.AWS.MediaConvert.Types.KantarWatermarkSettings
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
import Network.AWS.MediaConvert.Types.MxfXavcProfileSettings
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
import Network.AWS.MediaConvert.Types.Policy
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
import Network.AWS.MediaConvert.Types.SrtDestinationSettings
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
import Network.AWS.MediaConvert.Types.WebvttDestinationSettings
import Network.AWS.MediaConvert.Types.WebvttHlsSourceSettings
import Network.AWS.MediaConvert.Types.Xavc4kIntraCbgProfileSettings
import Network.AWS.MediaConvert.Types.Xavc4kIntraVbrProfileSettings
import Network.AWS.MediaConvert.Types.Xavc4kProfileSettings
import Network.AWS.MediaConvert.Types.XavcHdIntraCbgProfileSettings
import Network.AWS.MediaConvert.Types.XavcHdProfileSettings
import Network.AWS.MediaConvert.Types.XavcSettings
import Network.AWS.MediaConvert.UntagResource
import Network.AWS.MediaConvert.UpdateJobTemplate
import Network.AWS.MediaConvert.UpdatePreset
import Network.AWS.MediaConvert.UpdateQueue
