{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Lens
  ( -- * Operations

    -- ** AssociateCertificate
    associateCertificate_arn,
    associateCertificateResponse_httpStatus,

    -- ** CancelJob
    cancelJob_id,
    cancelJobResponse_httpStatus,

    -- ** CreateJob
    createJob_tags,
    createJob_hopDestinations,
    createJob_clientRequestToken,
    createJob_statusUpdateInterval,
    createJob_simulateReservedQueue,
    createJob_jobTemplate,
    createJob_priority,
    createJob_accelerationSettings,
    createJob_queue,
    createJob_userMetadata,
    createJob_billingTagsSource,
    createJob_role,
    createJob_settings,
    createJobResponse_job,
    createJobResponse_httpStatus,

    -- ** CreateJobTemplate
    createJobTemplate_tags,
    createJobTemplate_hopDestinations,
    createJobTemplate_statusUpdateInterval,
    createJobTemplate_description,
    createJobTemplate_priority,
    createJobTemplate_category,
    createJobTemplate_accelerationSettings,
    createJobTemplate_queue,
    createJobTemplate_settings,
    createJobTemplate_name,
    createJobTemplateResponse_jobTemplate,
    createJobTemplateResponse_httpStatus,

    -- ** CreatePreset
    createPreset_tags,
    createPreset_description,
    createPreset_category,
    createPreset_settings,
    createPreset_name,
    createPresetResponse_preset,
    createPresetResponse_httpStatus,

    -- ** CreateQueue
    createQueue_tags,
    createQueue_status,
    createQueue_description,
    createQueue_pricingPlan,
    createQueue_reservationPlanSettings,
    createQueue_name,
    createQueueResponse_queue,
    createQueueResponse_httpStatus,

    -- ** DeleteJobTemplate
    deleteJobTemplate_name,
    deleteJobTemplateResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicyResponse_httpStatus,

    -- ** DeletePreset
    deletePreset_name,
    deletePresetResponse_httpStatus,

    -- ** DeleteQueue
    deleteQueue_name,
    deleteQueueResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_nextToken,
    describeEndpoints_maxResults,
    describeEndpoints_mode,
    describeEndpointsResponse_nextToken,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,

    -- ** DisassociateCertificate
    disassociateCertificate_arn,
    disassociateCertificateResponse_httpStatus,

    -- ** GetJob
    getJob_id,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** GetJobTemplate
    getJobTemplate_name,
    getJobTemplateResponse_jobTemplate,
    getJobTemplateResponse_httpStatus,

    -- ** GetPolicy
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** GetPreset
    getPreset_name,
    getPresetResponse_preset,
    getPresetResponse_httpStatus,

    -- ** GetQueue
    getQueue_name,
    getQueueResponse_queue,
    getQueueResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_nextToken,
    listJobTemplates_listBy,
    listJobTemplates_maxResults,
    listJobTemplates_category,
    listJobTemplates_order,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_status,
    listJobs_maxResults,
    listJobs_order,
    listJobs_queue,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** ListPresets
    listPresets_nextToken,
    listPresets_listBy,
    listPresets_maxResults,
    listPresets_category,
    listPresets_order,
    listPresetsResponse_presets,
    listPresetsResponse_nextToken,
    listPresetsResponse_httpStatus,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_listBy,
    listQueues_maxResults,
    listQueues_order,
    listQueuesResponse_nextToken,
    listQueuesResponse_queues,
    listQueuesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutPolicy
    putPolicy_policy,
    putPolicyResponse_policy,
    putPolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_arn,
    untagResourceResponse_httpStatus,

    -- ** UpdateJobTemplate
    updateJobTemplate_hopDestinations,
    updateJobTemplate_statusUpdateInterval,
    updateJobTemplate_description,
    updateJobTemplate_settings,
    updateJobTemplate_priority,
    updateJobTemplate_category,
    updateJobTemplate_accelerationSettings,
    updateJobTemplate_queue,
    updateJobTemplate_name,
    updateJobTemplateResponse_jobTemplate,
    updateJobTemplateResponse_httpStatus,

    -- ** UpdatePreset
    updatePreset_description,
    updatePreset_settings,
    updatePreset_category,
    updatePreset_name,
    updatePresetResponse_preset,
    updatePresetResponse_httpStatus,

    -- ** UpdateQueue
    updateQueue_status,
    updateQueue_description,
    updateQueue_reservationPlanSettings,
    updateQueue_name,
    updateQueueResponse_queue,
    updateQueueResponse_httpStatus,

    -- * Types

    -- ** AacSettings
    aacSettings_bitrate,
    aacSettings_specification,
    aacSettings_sampleRate,
    aacSettings_codecProfile,
    aacSettings_vbrQuality,
    aacSettings_codingMode,
    aacSettings_audioDescriptionBroadcasterMix,
    aacSettings_rateControlMode,
    aacSettings_rawFormat,

    -- ** Ac3Settings
    ac3Settings_bitstreamMode,
    ac3Settings_dynamicRangeCompressionRf,
    ac3Settings_bitrate,
    ac3Settings_sampleRate,
    ac3Settings_dynamicRangeCompressionProfile,
    ac3Settings_dialnorm,
    ac3Settings_dynamicRangeCompressionLine,
    ac3Settings_codingMode,
    ac3Settings_metadataControl,
    ac3Settings_lfeFilter,

    -- ** AccelerationSettings
    accelerationSettings_mode,

    -- ** AiffSettings
    aiffSettings_bitDepth,
    aiffSettings_channels,
    aiffSettings_sampleRate,

    -- ** AllowedRenditionSize
    allowedRenditionSize_required,
    allowedRenditionSize_width,
    allowedRenditionSize_height,

    -- ** AncillarySourceSettings
    ancillarySourceSettings_convert608To708,
    ancillarySourceSettings_terminateCaptions,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- ** AudioChannelTaggingSettings
    audioChannelTaggingSettings_channelTag,

    -- ** AudioCodecSettings
    audioCodecSettings_aiffSettings,
    audioCodecSettings_vorbisSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_wavSettings,
    audioCodecSettings_opusSettings,
    audioCodecSettings_codec,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_mp3Settings,
    audioCodecSettings_eac3AtmosSettings,
    audioCodecSettings_aacSettings,

    -- ** AudioDescription
    audioDescription_customLanguageCode,
    audioDescription_audioNormalizationSettings,
    audioDescription_codecSettings,
    audioDescription_remixSettings,
    audioDescription_languageCode,
    audioDescription_audioChannelTaggingSettings,
    audioDescription_audioSourceName,
    audioDescription_audioTypeControl,
    audioDescription_audioType,
    audioDescription_languageCodeControl,
    audioDescription_streamName,

    -- ** AudioNormalizationSettings
    audioNormalizationSettings_correctionGateLevel,
    audioNormalizationSettings_loudnessLogging,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_peakCalculation,
    audioNormalizationSettings_algorithmControl,

    -- ** AudioSelector
    audioSelector_externalAudioFileInput,
    audioSelector_audioDurationCorrection,
    audioSelector_pids,
    audioSelector_hlsRenditionGroupSettings,
    audioSelector_customLanguageCode,
    audioSelector_defaultSelection,
    audioSelector_offset,
    audioSelector_programSelection,
    audioSelector_remixSettings,
    audioSelector_languageCode,
    audioSelector_selectorType,
    audioSelector_tracks,

    -- ** AudioSelectorGroup
    audioSelectorGroup_audioSelectorNames,

    -- ** AutomatedAbrRule
    automatedAbrRule_type,
    automatedAbrRule_minTopRenditionSize,
    automatedAbrRule_minBottomRenditionSize,
    automatedAbrRule_forceIncludeRenditions,
    automatedAbrRule_allowedRenditions,

    -- ** AutomatedAbrSettings
    automatedAbrSettings_minAbrBitrate,
    automatedAbrSettings_rules,
    automatedAbrSettings_maxAbrBitrate,
    automatedAbrSettings_maxRenditions,

    -- ** AutomatedEncodingSettings
    automatedEncodingSettings_abrSettings,

    -- ** Av1QvbrSettings
    av1QvbrSettings_qvbrQualityLevel,
    av1QvbrSettings_qvbrQualityLevelFineTune,

    -- ** Av1Settings
    av1Settings_framerateDenominator,
    av1Settings_bitDepth,
    av1Settings_framerateControl,
    av1Settings_qvbrSettings,
    av1Settings_maxBitrate,
    av1Settings_framerateNumerator,
    av1Settings_slices,
    av1Settings_rateControlMode,
    av1Settings_framerateConversionAlgorithm,
    av1Settings_numberBFramesBetweenReferenceFrames,
    av1Settings_adaptiveQuantization,
    av1Settings_gopSize,
    av1Settings_spatialAdaptiveQuantization,

    -- ** AvailBlanking
    availBlanking_availBlankingImage,

    -- ** AvcIntraSettings
    avcIntraSettings_telecine,
    avcIntraSettings_framerateDenominator,
    avcIntraSettings_scanTypeConversionMode,
    avcIntraSettings_framerateControl,
    avcIntraSettings_framerateNumerator,
    avcIntraSettings_avcIntraUhdSettings,
    avcIntraSettings_slowPal,
    avcIntraSettings_interlaceMode,
    avcIntraSettings_framerateConversionAlgorithm,
    avcIntraSettings_avcIntraClass,

    -- ** AvcIntraUhdSettings
    avcIntraUhdSettings_qualityTuningLevel,

    -- ** BurninDestinationSettings
    burninDestinationSettings_fontResolution,
    burninDestinationSettings_yPosition,
    burninDestinationSettings_hexFontColor,
    burninDestinationSettings_shadowXOffset,
    burninDestinationSettings_alignment,
    burninDestinationSettings_xPosition,
    burninDestinationSettings_fontSize,
    burninDestinationSettings_outlineSize,
    burninDestinationSettings_applyFontColor,
    burninDestinationSettings_outlineColor,
    burninDestinationSettings_teletextSpacing,
    burninDestinationSettings_stylePassthrough,
    burninDestinationSettings_backgroundColor,
    burninDestinationSettings_fontOpacity,
    burninDestinationSettings_fallbackFont,
    burninDestinationSettings_shadowYOffset,
    burninDestinationSettings_backgroundOpacity,
    burninDestinationSettings_fontScript,
    burninDestinationSettings_shadowOpacity,
    burninDestinationSettings_fontColor,
    burninDestinationSettings_shadowColor,

    -- ** CaptionDescription
    captionDescription_captionSelectorName,
    captionDescription_customLanguageCode,
    captionDescription_languageDescription,
    captionDescription_languageCode,
    captionDescription_destinationSettings,

    -- ** CaptionDescriptionPreset
    captionDescriptionPreset_customLanguageCode,
    captionDescriptionPreset_languageDescription,
    captionDescriptionPreset_languageCode,
    captionDescriptionPreset_destinationSettings,

    -- ** CaptionDestinationSettings
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_sccDestinationSettings,
    captionDestinationSettings_webvttDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_destinationType,
    captionDestinationSettings_burninDestinationSettings,
    captionDestinationSettings_imscDestinationSettings,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_srtDestinationSettings,

    -- ** CaptionSelector
    captionSelector_customLanguageCode,
    captionSelector_languageCode,
    captionSelector_sourceSettings,

    -- ** CaptionSourceFramerate
    captionSourceFramerate_framerateDenominator,
    captionSourceFramerate_framerateNumerator,

    -- ** CaptionSourceSettings
    captionSourceSettings_fileSourceSettings,
    captionSourceSettings_ancillarySourceSettings,
    captionSourceSettings_dvbSubSourceSettings,
    captionSourceSettings_webvttHlsSourceSettings,
    captionSourceSettings_sourceType,
    captionSourceSettings_embeddedSourceSettings,
    captionSourceSettings_trackSourceSettings,
    captionSourceSettings_teletextSourceSettings,

    -- ** ChannelMapping
    channelMapping_outputChannels,

    -- ** CmafAdditionalManifest
    cmafAdditionalManifest_selectedOutputs,
    cmafAdditionalManifest_manifestNameModifier,

    -- ** CmafEncryptionSettings
    cmafEncryptionSettings_type,
    cmafEncryptionSettings_initializationVectorInManifest,
    cmafEncryptionSettings_spekeKeyProvider,
    cmafEncryptionSettings_constantInitializationVector,
    cmafEncryptionSettings_encryptionMethod,
    cmafEncryptionSettings_staticKeyProvider,

    -- ** CmafGroupSettings
    cmafGroupSettings_destination,
    cmafGroupSettings_baseUrl,
    cmafGroupSettings_imageBasedTrickPlaySettings,
    cmafGroupSettings_streamInfResolution,
    cmafGroupSettings_mpdProfile,
    cmafGroupSettings_manifestCompression,
    cmafGroupSettings_ptsOffsetHandlingForBFrames,
    cmafGroupSettings_segmentLength,
    cmafGroupSettings_codecSpecification,
    cmafGroupSettings_manifestDurationFormat,
    cmafGroupSettings_minBufferTime,
    cmafGroupSettings_segmentControl,
    cmafGroupSettings_fragmentLength,
    cmafGroupSettings_writeHlsManifest,
    cmafGroupSettings_encryption,
    cmafGroupSettings_writeDashManifest,
    cmafGroupSettings_imageBasedTrickPlay,
    cmafGroupSettings_clientCache,
    cmafGroupSettings_additionalManifests,
    cmafGroupSettings_segmentLengthControl,
    cmafGroupSettings_minFinalSegmentLength,
    cmafGroupSettings_writeSegmentTimelineInRepresentation,
    cmafGroupSettings_targetDurationCompatibilityMode,
    cmafGroupSettings_destinationSettings,

    -- ** CmafImageBasedTrickPlaySettings
    cmafImageBasedTrickPlaySettings_tileWidth,
    cmafImageBasedTrickPlaySettings_thumbnailWidth,
    cmafImageBasedTrickPlaySettings_tileHeight,
    cmafImageBasedTrickPlaySettings_thumbnailHeight,
    cmafImageBasedTrickPlaySettings_thumbnailInterval,
    cmafImageBasedTrickPlaySettings_intervalCadence,

    -- ** CmfcSettings
    cmfcSettings_descriptiveVideoServiceFlag,
    cmfcSettings_audioDuration,
    cmfcSettings_audioTrackType,
    cmfcSettings_timedMetadata,
    cmfcSettings_klvMetadata,
    cmfcSettings_iFrameOnlyManifest,
    cmfcSettings_audioRenditionSets,
    cmfcSettings_scte35Esam,
    cmfcSettings_scte35Source,
    cmfcSettings_audioGroupId,

    -- ** ColorCorrector
    colorCorrector_colorSpaceConversion,
    colorCorrector_saturation,
    colorCorrector_sdrReferenceWhiteLevel,
    colorCorrector_brightness,
    colorCorrector_hue,
    colorCorrector_hdr10Metadata,
    colorCorrector_contrast,
    colorCorrector_sampleRangeConversion,

    -- ** ContainerSettings
    containerSettings_movSettings,
    containerSettings_mpdSettings,
    containerSettings_cmfcSettings,
    containerSettings_m3u8Settings,
    containerSettings_m2tsSettings,
    containerSettings_mxfSettings,
    containerSettings_container,
    containerSettings_mp4Settings,
    containerSettings_f4vSettings,

    -- ** DashAdditionalManifest
    dashAdditionalManifest_selectedOutputs,
    dashAdditionalManifest_manifestNameModifier,

    -- ** DashIsoEncryptionSettings
    dashIsoEncryptionSettings_playbackDeviceCompatibility,
    dashIsoEncryptionSettings_spekeKeyProvider,

    -- ** DashIsoGroupSettings
    dashIsoGroupSettings_destination,
    dashIsoGroupSettings_baseUrl,
    dashIsoGroupSettings_imageBasedTrickPlaySettings,
    dashIsoGroupSettings_hbbtvCompliance,
    dashIsoGroupSettings_mpdProfile,
    dashIsoGroupSettings_ptsOffsetHandlingForBFrames,
    dashIsoGroupSettings_segmentLength,
    dashIsoGroupSettings_minBufferTime,
    dashIsoGroupSettings_audioChannelConfigSchemeIdUri,
    dashIsoGroupSettings_segmentControl,
    dashIsoGroupSettings_fragmentLength,
    dashIsoGroupSettings_encryption,
    dashIsoGroupSettings_imageBasedTrickPlay,
    dashIsoGroupSettings_additionalManifests,
    dashIsoGroupSettings_segmentLengthControl,
    dashIsoGroupSettings_minFinalSegmentLength,
    dashIsoGroupSettings_writeSegmentTimelineInRepresentation,
    dashIsoGroupSettings_destinationSettings,

    -- ** DashIsoImageBasedTrickPlaySettings
    dashIsoImageBasedTrickPlaySettings_tileWidth,
    dashIsoImageBasedTrickPlaySettings_thumbnailWidth,
    dashIsoImageBasedTrickPlaySettings_tileHeight,
    dashIsoImageBasedTrickPlaySettings_thumbnailHeight,
    dashIsoImageBasedTrickPlaySettings_thumbnailInterval,
    dashIsoImageBasedTrickPlaySettings_intervalCadence,

    -- ** Deinterlacer
    deinterlacer_mode,
    deinterlacer_algorithm,
    deinterlacer_control,

    -- ** DestinationSettings
    destinationSettings_s3Settings,

    -- ** DolbyVision
    dolbyVision_l6Metadata,
    dolbyVision_profile,
    dolbyVision_mapping,
    dolbyVision_l6Mode,

    -- ** DolbyVisionLevel6Metadata
    dolbyVisionLevel6Metadata_maxCll,
    dolbyVisionLevel6Metadata_maxFall,

    -- ** DvbNitSettings
    dvbNitSettings_nitInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- ** DvbSdtSettings
    dvbSdtSettings_sdtInterval,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_serviceName,

    -- ** DvbSubDestinationSettings
    dvbSubDestinationSettings_ddsHandling,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_yPosition,
    dvbSubDestinationSettings_hexFontColor,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_applyFontColor,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_teletextSpacing,
    dvbSubDestinationSettings_ddsXCoordinate,
    dvbSubDestinationSettings_stylePassthrough,
    dvbSubDestinationSettings_width,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_ddsYCoordinate,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_fallbackFont,
    dvbSubDestinationSettings_shadowYOffset,
    dvbSubDestinationSettings_subtitlingType,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_fontScript,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_height,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_shadowColor,

    -- ** DvbSubSourceSettings
    dvbSubSourceSettings_pid,

    -- ** DvbTdtSettings
    dvbTdtSettings_tdtInterval,

    -- ** Eac3AtmosSettings
    eac3AtmosSettings_bitstreamMode,
    eac3AtmosSettings_dialogueIntelligence,
    eac3AtmosSettings_dynamicRangeCompressionRf,
    eac3AtmosSettings_bitrate,
    eac3AtmosSettings_ltRtCenterMixLevel,
    eac3AtmosSettings_surroundExMode,
    eac3AtmosSettings_ltRtSurroundMixLevel,
    eac3AtmosSettings_sampleRate,
    eac3AtmosSettings_meteringMode,
    eac3AtmosSettings_dynamicRangeControl,
    eac3AtmosSettings_dynamicRangeCompressionLine,
    eac3AtmosSettings_codingMode,
    eac3AtmosSettings_downmixControl,
    eac3AtmosSettings_loRoCenterMixLevel,
    eac3AtmosSettings_loRoSurroundMixLevel,
    eac3AtmosSettings_speechThreshold,
    eac3AtmosSettings_stereoDownmix,

    -- ** Eac3Settings
    eac3Settings_bitstreamMode,
    eac3Settings_surroundMode,
    eac3Settings_lfeControl,
    eac3Settings_passthroughControl,
    eac3Settings_attenuationControl,
    eac3Settings_dynamicRangeCompressionRf,
    eac3Settings_bitrate,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_surroundExMode,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_sampleRate,
    eac3Settings_phaseControl,
    eac3Settings_dialnorm,
    eac3Settings_dynamicRangeCompressionLine,
    eac3Settings_codingMode,
    eac3Settings_dcFilter,
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_stereoDownmix,
    eac3Settings_metadataControl,
    eac3Settings_lfeFilter,

    -- ** EmbeddedDestinationSettings
    embeddedDestinationSettings_destination708ServiceNumber,
    embeddedDestinationSettings_destination608ChannelNumber,

    -- ** EmbeddedSourceSettings
    embeddedSourceSettings_source608ChannelNumber,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_terminateCaptions,

    -- ** Endpoint
    endpoint_url,

    -- ** EsamManifestConfirmConditionNotification
    esamManifestConfirmConditionNotification_mccXml,

    -- ** EsamSettings
    esamSettings_responseSignalPreroll,
    esamSettings_signalProcessingNotification,
    esamSettings_manifestConfirmConditionNotification,

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
    fileSourceSettings_timeDelta,
    fileSourceSettings_convert608To708,
    fileSourceSettings_timeDeltaUnits,
    fileSourceSettings_sourceFile,
    fileSourceSettings_framerate,

    -- ** ForceIncludeRenditionSize
    forceIncludeRenditionSize_width,
    forceIncludeRenditionSize_height,

    -- ** FrameCaptureSettings
    frameCaptureSettings_framerateDenominator,
    frameCaptureSettings_quality,
    frameCaptureSettings_framerateNumerator,
    frameCaptureSettings_maxCaptures,

    -- ** H264QvbrSettings
    h264QvbrSettings_maxAverageBitrate,
    h264QvbrSettings_qvbrQualityLevel,
    h264QvbrSettings_qvbrQualityLevelFineTune,

    -- ** H264Settings
    h264Settings_parNumerator,
    h264Settings_repeatPps,
    h264Settings_gopSizeUnits,
    h264Settings_telecine,
    h264Settings_framerateDenominator,
    h264Settings_dynamicSubGop,
    h264Settings_scanTypeConversionMode,
    h264Settings_hrdBufferSize,
    h264Settings_bitrate,
    h264Settings_sceneChangeDetect,
    h264Settings_framerateControl,
    h264Settings_qvbrSettings,
    h264Settings_parControl,
    h264Settings_qualityTuningLevel,
    h264Settings_maxBitrate,
    h264Settings_framerateNumerator,
    h264Settings_unregisteredSeiTimecode,
    h264Settings_codecProfile,
    h264Settings_fieldEncoding,
    h264Settings_syntax,
    h264Settings_numberReferenceFrames,
    h264Settings_temporalAdaptiveQuantization,
    h264Settings_hrdBufferFinalFillPercentage,
    h264Settings_hrdBufferInitialFillPercentage,
    h264Settings_gopClosedCadence,
    h264Settings_slowPal,
    h264Settings_interlaceMode,
    h264Settings_parDenominator,
    h264Settings_entropyEncoding,
    h264Settings_softness,
    h264Settings_slices,
    h264Settings_flickerAdaptiveQuantization,
    h264Settings_rateControlMode,
    h264Settings_framerateConversionAlgorithm,
    h264Settings_numberBFramesBetweenReferenceFrames,
    h264Settings_gopBReference,
    h264Settings_codecLevel,
    h264Settings_adaptiveQuantization,
    h264Settings_minIInterval,
    h264Settings_gopSize,
    h264Settings_spatialAdaptiveQuantization,

    -- ** H265QvbrSettings
    h265QvbrSettings_maxAverageBitrate,
    h265QvbrSettings_qvbrQualityLevel,
    h265QvbrSettings_qvbrQualityLevelFineTune,

    -- ** H265Settings
    h265Settings_parNumerator,
    h265Settings_temporalIds,
    h265Settings_gopSizeUnits,
    h265Settings_telecine,
    h265Settings_framerateDenominator,
    h265Settings_dynamicSubGop,
    h265Settings_scanTypeConversionMode,
    h265Settings_writeMp4PackagingType,
    h265Settings_hrdBufferSize,
    h265Settings_bitrate,
    h265Settings_sceneChangeDetect,
    h265Settings_framerateControl,
    h265Settings_qvbrSettings,
    h265Settings_parControl,
    h265Settings_qualityTuningLevel,
    h265Settings_maxBitrate,
    h265Settings_tiles,
    h265Settings_sampleAdaptiveOffsetFilterMode,
    h265Settings_framerateNumerator,
    h265Settings_unregisteredSeiTimecode,
    h265Settings_codecProfile,
    h265Settings_alternateTransferFunctionSei,
    h265Settings_numberReferenceFrames,
    h265Settings_temporalAdaptiveQuantization,
    h265Settings_hrdBufferFinalFillPercentage,
    h265Settings_hrdBufferInitialFillPercentage,
    h265Settings_gopClosedCadence,
    h265Settings_slowPal,
    h265Settings_interlaceMode,
    h265Settings_parDenominator,
    h265Settings_slices,
    h265Settings_flickerAdaptiveQuantization,
    h265Settings_rateControlMode,
    h265Settings_framerateConversionAlgorithm,
    h265Settings_numberBFramesBetweenReferenceFrames,
    h265Settings_gopBReference,
    h265Settings_codecLevel,
    h265Settings_adaptiveQuantization,
    h265Settings_minIInterval,
    h265Settings_gopSize,
    h265Settings_spatialAdaptiveQuantization,

    -- ** Hdr10Metadata
    hdr10Metadata_redPrimaryX,
    hdr10Metadata_whitePointY,
    hdr10Metadata_greenPrimaryX,
    hdr10Metadata_maxContentLightLevel,
    hdr10Metadata_redPrimaryY,
    hdr10Metadata_bluePrimaryX,
    hdr10Metadata_whitePointX,
    hdr10Metadata_maxLuminance,
    hdr10Metadata_maxFrameAverageLightLevel,
    hdr10Metadata_greenPrimaryY,
    hdr10Metadata_minLuminance,
    hdr10Metadata_bluePrimaryY,

    -- ** Hdr10Plus
    hdr10Plus_targetMonitorNits,
    hdr10Plus_masteringMonitorNits,

    -- ** HlsAdditionalManifest
    hlsAdditionalManifest_selectedOutputs,
    hlsAdditionalManifest_manifestNameModifier,

    -- ** HlsCaptionLanguageMapping
    hlsCaptionLanguageMapping_customLanguageCode,
    hlsCaptionLanguageMapping_captionChannel,
    hlsCaptionLanguageMapping_languageDescription,
    hlsCaptionLanguageMapping_languageCode,

    -- ** HlsEncryptionSettings
    hlsEncryptionSettings_type,
    hlsEncryptionSettings_initializationVectorInManifest,
    hlsEncryptionSettings_spekeKeyProvider,
    hlsEncryptionSettings_constantInitializationVector,
    hlsEncryptionSettings_encryptionMethod,
    hlsEncryptionSettings_staticKeyProvider,
    hlsEncryptionSettings_offlineEncrypted,

    -- ** HlsGroupSettings
    hlsGroupSettings_destination,
    hlsGroupSettings_baseUrl,
    hlsGroupSettings_imageBasedTrickPlaySettings,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_audioOnlyHeader,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_segmentControl,
    hlsGroupSettings_captionSegmentLengthControl,
    hlsGroupSettings_encryption,
    hlsGroupSettings_imageBasedTrickPlay,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_additionalManifests,
    hlsGroupSettings_segmentLengthControl,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_timestampDeltaMilliseconds,
    hlsGroupSettings_minFinalSegmentLength,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_targetDurationCompatibilityMode,
    hlsGroupSettings_destinationSettings,

    -- ** HlsImageBasedTrickPlaySettings
    hlsImageBasedTrickPlaySettings_tileWidth,
    hlsImageBasedTrickPlaySettings_thumbnailWidth,
    hlsImageBasedTrickPlaySettings_tileHeight,
    hlsImageBasedTrickPlaySettings_thumbnailHeight,
    hlsImageBasedTrickPlaySettings_thumbnailInterval,
    hlsImageBasedTrickPlaySettings_intervalCadence,

    -- ** HlsRenditionGroupSettings
    hlsRenditionGroupSettings_renditionLanguageCode,
    hlsRenditionGroupSettings_renditionName,
    hlsRenditionGroupSettings_renditionGroupId,

    -- ** HlsSettings
    hlsSettings_descriptiveVideoServiceFlag,
    hlsSettings_audioTrackType,
    hlsSettings_iFrameOnlyManifest,
    hlsSettings_audioRenditionSets,
    hlsSettings_audioOnlyContainer,
    hlsSettings_segmentModifier,
    hlsSettings_audioGroupId,

    -- ** HopDestination
    hopDestination_priority,
    hopDestination_queue,
    hopDestination_waitMinutes,

    -- ** Id3Insertion
    id3Insertion_id3,
    id3Insertion_timecode,

    -- ** ImageInserter
    imageInserter_insertableImages,
    imageInserter_sdrReferenceWhiteLevel,

    -- ** ImscDestinationSettings
    imscDestinationSettings_stylePassthrough,
    imscDestinationSettings_accessibility,

    -- ** Input
    input_deblockFilter,
    input_psiControl,
    input_captionSelectors,
    input_audioSelectorGroups,
    input_filterStrength,
    input_timecodeStart,
    input_dolbyVisionMetadataXml,
    input_inputClippings,
    input_audioSelectors,
    input_timecodeSource,
    input_videoGenerator,
    input_programNumber,
    input_crop,
    input_supplementalImps,
    input_filterEnable,
    input_imageInserter,
    input_denoiseFilter,
    input_position,
    input_inputScanType,
    input_decryptionSettings,
    input_fileInput,
    input_videoSelector,

    -- ** InputClipping
    inputClipping_startTimecode,
    inputClipping_endTimecode,

    -- ** InputDecryptionSettings
    inputDecryptionSettings_initializationVector,
    inputDecryptionSettings_encryptedDecryptionKey,
    inputDecryptionSettings_decryptionMode,
    inputDecryptionSettings_kmsKeyRegion,

    -- ** InputTemplate
    inputTemplate_deblockFilter,
    inputTemplate_psiControl,
    inputTemplate_captionSelectors,
    inputTemplate_audioSelectorGroups,
    inputTemplate_filterStrength,
    inputTemplate_timecodeStart,
    inputTemplate_dolbyVisionMetadataXml,
    inputTemplate_inputClippings,
    inputTemplate_audioSelectors,
    inputTemplate_timecodeSource,
    inputTemplate_programNumber,
    inputTemplate_crop,
    inputTemplate_filterEnable,
    inputTemplate_imageInserter,
    inputTemplate_denoiseFilter,
    inputTemplate_position,
    inputTemplate_inputScanType,
    inputTemplate_videoSelector,

    -- ** InputVideoGenerator
    inputVideoGenerator_duration,

    -- ** InsertableImage
    insertableImage_imageInserterInput,
    insertableImage_fadeOut,
    insertableImage_imageX,
    insertableImage_width,
    insertableImage_duration,
    insertableImage_layer,
    insertableImage_opacity,
    insertableImage_height,
    insertableImage_startTime,
    insertableImage_fadeIn,
    insertableImage_imageY,

    -- ** Job
    job_queueTransitions,
    job_hopDestinations,
    job_timing,
    job_errorMessage,
    job_statusUpdateInterval,
    job_arn,
    job_currentPhase,
    job_status,
    job_jobPercentComplete,
    job_id,
    job_simulateReservedQueue,
    job_messages,
    job_jobTemplate,
    job_retryCount,
    job_priority,
    job_errorCode,
    job_outputGroupDetails,
    job_accelerationSettings,
    job_accelerationStatus,
    job_createdAt,
    job_queue,
    job_userMetadata,
    job_billingTagsSource,
    job_role,
    job_settings,

    -- ** JobMessages
    jobMessages_info,
    jobMessages_warning,

    -- ** JobSettings
    jobSettings_availBlanking,
    jobSettings_esam,
    jobSettings_nielsenNonLinearWatermark,
    jobSettings_motionImageInserter,
    jobSettings_nielsenConfiguration,
    jobSettings_outputGroups,
    jobSettings_timedMetadataInsertion,
    jobSettings_extendedDataServices,
    jobSettings_kantarWatermark,
    jobSettings_inputs,
    jobSettings_timecodeConfig,
    jobSettings_adAvailOffset,

    -- ** JobTemplate
    jobTemplate_hopDestinations,
    jobTemplate_type,
    jobTemplate_statusUpdateInterval,
    jobTemplate_arn,
    jobTemplate_description,
    jobTemplate_lastUpdated,
    jobTemplate_priority,
    jobTemplate_category,
    jobTemplate_accelerationSettings,
    jobTemplate_createdAt,
    jobTemplate_queue,
    jobTemplate_settings,
    jobTemplate_name,

    -- ** JobTemplateSettings
    jobTemplateSettings_availBlanking,
    jobTemplateSettings_esam,
    jobTemplateSettings_nielsenNonLinearWatermark,
    jobTemplateSettings_motionImageInserter,
    jobTemplateSettings_nielsenConfiguration,
    jobTemplateSettings_outputGroups,
    jobTemplateSettings_timedMetadataInsertion,
    jobTemplateSettings_extendedDataServices,
    jobTemplateSettings_kantarWatermark,
    jobTemplateSettings_inputs,
    jobTemplateSettings_timecodeConfig,
    jobTemplateSettings_adAvailOffset,

    -- ** KantarWatermarkSettings
    kantarWatermarkSettings_metadata4,
    kantarWatermarkSettings_contentReference,
    kantarWatermarkSettings_channelName,
    kantarWatermarkSettings_metadata7,
    kantarWatermarkSettings_metadata5,
    kantarWatermarkSettings_logDestination,
    kantarWatermarkSettings_metadata3,
    kantarWatermarkSettings_fileOffset,
    kantarWatermarkSettings_metadata8,
    kantarWatermarkSettings_kantarLicenseId,
    kantarWatermarkSettings_kantarServerUrl,
    kantarWatermarkSettings_metadata6,
    kantarWatermarkSettings_credentialsSecretName,

    -- ** M2tsScte35Esam
    m2tsScte35Esam_scte35EsamPid,

    -- ** M2tsSettings
    m2tsSettings_transportStreamId,
    m2tsSettings_segmentationMarkers,
    m2tsSettings_audioDuration,
    m2tsSettings_minEbpInterval,
    m2tsSettings_pcrControl,
    m2tsSettings_fragmentTime,
    m2tsSettings_pcrPid,
    m2tsSettings_ebpPlacement,
    m2tsSettings_dvbTdtSettings,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_bitrate,
    m2tsSettings_klvMetadata,
    m2tsSettings_patInterval,
    m2tsSettings_esRateInPes,
    m2tsSettings_audioBufferModel,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_rateMode,
    m2tsSettings_scte35Esam,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_forceTsVideoEbpOrder,
    m2tsSettings_bufferModel,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_segmentationTime,
    m2tsSettings_maxPcrInterval,
    m2tsSettings_videoPid,
    m2tsSettings_programNumber,
    m2tsSettings_privateMetadataPid,
    m2tsSettings_pmtInterval,
    m2tsSettings_scte35Pid,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_dataPTSControl,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_segmentationStyle,
    m2tsSettings_scte35Source,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_pmtPid,
    m2tsSettings_nielsenId3,
    m2tsSettings_audioPids,
    m2tsSettings_dvbSubPids,

    -- ** M3u8Settings
    m3u8Settings_transportStreamId,
    m3u8Settings_audioDuration,
    m3u8Settings_pcrControl,
    m3u8Settings_pcrPid,
    m3u8Settings_timedMetadata,
    m3u8Settings_patInterval,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_maxPcrInterval,
    m3u8Settings_videoPid,
    m3u8Settings_programNumber,
    m3u8Settings_privateMetadataPid,
    m3u8Settings_pmtInterval,
    m3u8Settings_scte35Pid,
    m3u8Settings_dataPTSControl,
    m3u8Settings_scte35Source,
    m3u8Settings_pmtPid,
    m3u8Settings_nielsenId3,
    m3u8Settings_audioPids,

    -- ** MinBottomRenditionSize
    minBottomRenditionSize_width,
    minBottomRenditionSize_height,

    -- ** MinTopRenditionSize
    minTopRenditionSize_width,
    minTopRenditionSize_height,

    -- ** MotionImageInserter
    motionImageInserter_insertionMode,
    motionImageInserter_playback,
    motionImageInserter_offset,
    motionImageInserter_input,
    motionImageInserter_startTime,
    motionImageInserter_framerate,

    -- ** MotionImageInsertionFramerate
    motionImageInsertionFramerate_framerateDenominator,
    motionImageInsertionFramerate_framerateNumerator,

    -- ** MotionImageInsertionOffset
    motionImageInsertionOffset_imageX,
    motionImageInsertionOffset_imageY,

    -- ** MovSettings
    movSettings_paddingControl,
    movSettings_cslgAtom,
    movSettings_reference,
    movSettings_clapAtom,
    movSettings_mpeg2FourCCControl,

    -- ** Mp2Settings
    mp2Settings_channels,
    mp2Settings_bitrate,
    mp2Settings_sampleRate,

    -- ** Mp3Settings
    mp3Settings_channels,
    mp3Settings_bitrate,
    mp3Settings_sampleRate,
    mp3Settings_vbrQuality,
    mp3Settings_rateControlMode,

    -- ** Mp4Settings
    mp4Settings_audioDuration,
    mp4Settings_freeSpaceBox,
    mp4Settings_moovPlacement,
    mp4Settings_cslgAtom,
    mp4Settings_cttsVersion,
    mp4Settings_mp4MajorBrand,

    -- ** MpdSettings
    mpdSettings_audioDuration,
    mpdSettings_timedMetadata,
    mpdSettings_klvMetadata,
    mpdSettings_scte35Esam,
    mpdSettings_accessibilityCaptionHints,
    mpdSettings_captionContainerType,
    mpdSettings_scte35Source,

    -- ** Mpeg2Settings
    mpeg2Settings_parNumerator,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_telecine,
    mpeg2Settings_framerateDenominator,
    mpeg2Settings_dynamicSubGop,
    mpeg2Settings_scanTypeConversionMode,
    mpeg2Settings_hrdBufferSize,
    mpeg2Settings_bitrate,
    mpeg2Settings_sceneChangeDetect,
    mpeg2Settings_framerateControl,
    mpeg2Settings_parControl,
    mpeg2Settings_qualityTuningLevel,
    mpeg2Settings_maxBitrate,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_codecProfile,
    mpeg2Settings_syntax,
    mpeg2Settings_temporalAdaptiveQuantization,
    mpeg2Settings_hrdBufferFinalFillPercentage,
    mpeg2Settings_hrdBufferInitialFillPercentage,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_slowPal,
    mpeg2Settings_interlaceMode,
    mpeg2Settings_parDenominator,
    mpeg2Settings_softness,
    mpeg2Settings_rateControlMode,
    mpeg2Settings_framerateConversionAlgorithm,
    mpeg2Settings_numberBFramesBetweenReferenceFrames,
    mpeg2Settings_intraDcPrecision,
    mpeg2Settings_codecLevel,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_minIInterval,
    mpeg2Settings_gopSize,
    mpeg2Settings_spatialAdaptiveQuantization,

    -- ** MsSmoothAdditionalManifest
    msSmoothAdditionalManifest_selectedOutputs,
    msSmoothAdditionalManifest_manifestNameModifier,

    -- ** MsSmoothEncryptionSettings
    msSmoothEncryptionSettings_spekeKeyProvider,

    -- ** MsSmoothGroupSettings
    msSmoothGroupSettings_destination,
    msSmoothGroupSettings_fragmentLengthControl,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_audioDeduplication,
    msSmoothGroupSettings_encryption,
    msSmoothGroupSettings_additionalManifests,
    msSmoothGroupSettings_manifestEncoding,
    msSmoothGroupSettings_destinationSettings,

    -- ** MxfSettings
    mxfSettings_afdSignaling,
    mxfSettings_profile,
    mxfSettings_xavcProfileSettings,

    -- ** MxfXavcProfileSettings
    mxfXavcProfileSettings_durationMode,
    mxfXavcProfileSettings_maxAncDataSize,

    -- ** NexGuardFileMarkerSettings
    nexGuardFileMarkerSettings_strength,
    nexGuardFileMarkerSettings_license,
    nexGuardFileMarkerSettings_payload,
    nexGuardFileMarkerSettings_preset,

    -- ** NielsenConfiguration
    nielsenConfiguration_breakoutCode,
    nielsenConfiguration_distributorId,

    -- ** NielsenNonLinearWatermarkSettings
    nielsenNonLinearWatermarkSettings_ticServerUrl,
    nielsenNonLinearWatermarkSettings_sourceWatermarkStatus,
    nielsenNonLinearWatermarkSettings_assetId,
    nielsenNonLinearWatermarkSettings_sourceId,
    nielsenNonLinearWatermarkSettings_assetName,
    nielsenNonLinearWatermarkSettings_episodeId,
    nielsenNonLinearWatermarkSettings_activeWatermarkProcess,
    nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack,
    nielsenNonLinearWatermarkSettings_cbetSourceId,
    nielsenNonLinearWatermarkSettings_adiFilename,
    nielsenNonLinearWatermarkSettings_metadataDestination,

    -- ** NoiseReducer
    noiseReducer_spatialFilterSettings,
    noiseReducer_temporalFilterSettings,
    noiseReducer_filter,
    noiseReducer_filterSettings,

    -- ** NoiseReducerFilterSettings
    noiseReducerFilterSettings_strength,

    -- ** NoiseReducerSpatialFilterSettings
    noiseReducerSpatialFilterSettings_strength,
    noiseReducerSpatialFilterSettings_postFilterSharpenStrength,
    noiseReducerSpatialFilterSettings_speed,

    -- ** NoiseReducerTemporalFilterSettings
    noiseReducerTemporalFilterSettings_strength,
    noiseReducerTemporalFilterSettings_aggressiveMode,
    noiseReducerTemporalFilterSettings_postTemporalSharpening,
    noiseReducerTemporalFilterSettings_postTemporalSharpeningStrength,
    noiseReducerTemporalFilterSettings_speed,

    -- ** OpusSettings
    opusSettings_channels,
    opusSettings_bitrate,
    opusSettings_sampleRate,

    -- ** Output
    output_containerSettings,
    output_extension,
    output_nameModifier,
    output_captionDescriptions,
    output_preset,
    output_audioDescriptions,
    output_outputSettings,
    output_videoDescription,

    -- ** OutputChannelMapping
    outputChannelMapping_inputChannelsFineTune,
    outputChannelMapping_inputChannels,

    -- ** OutputDetail
    outputDetail_durationInMs,
    outputDetail_videoDetails,

    -- ** OutputGroup
    outputGroup_name,
    outputGroup_outputGroupSettings,
    outputGroup_automatedEncodingSettings,
    outputGroup_outputs,
    outputGroup_customName,

    -- ** OutputGroupDetail
    outputGroupDetail_outputDetails,

    -- ** OutputGroupSettings
    outputGroupSettings_dashIsoGroupSettings,
    outputGroupSettings_type,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_fileGroupSettings,
    outputGroupSettings_cmafGroupSettings,

    -- ** OutputSettings
    outputSettings_hlsSettings,

    -- ** PartnerWatermarking
    partnerWatermarking_nexguardFileMarkerSettings,

    -- ** Policy
    policy_s3Inputs,
    policy_httpInputs,
    policy_httpsInputs,

    -- ** Preset
    preset_type,
    preset_arn,
    preset_description,
    preset_lastUpdated,
    preset_category,
    preset_createdAt,
    preset_settings,
    preset_name,

    -- ** PresetSettings
    presetSettings_containerSettings,
    presetSettings_captionDescriptions,
    presetSettings_audioDescriptions,
    presetSettings_videoDescription,

    -- ** ProresSettings
    proresSettings_parNumerator,
    proresSettings_telecine,
    proresSettings_framerateDenominator,
    proresSettings_scanTypeConversionMode,
    proresSettings_framerateControl,
    proresSettings_parControl,
    proresSettings_framerateNumerator,
    proresSettings_codecProfile,
    proresSettings_slowPal,
    proresSettings_chromaSampling,
    proresSettings_interlaceMode,
    proresSettings_parDenominator,
    proresSettings_framerateConversionAlgorithm,

    -- ** Queue
    queue_progressingJobsCount,
    queue_reservationPlan,
    queue_submittedJobsCount,
    queue_type,
    queue_arn,
    queue_status,
    queue_description,
    queue_lastUpdated,
    queue_pricingPlan,
    queue_createdAt,
    queue_name,

    -- ** QueueTransition
    queueTransition_destinationQueue,
    queueTransition_timestamp,
    queueTransition_sourceQueue,

    -- ** Rectangle
    rectangle_x,
    rectangle_width,
    rectangle_y,
    rectangle_height,

    -- ** RemixSettings
    remixSettings_channelMapping,
    remixSettings_channelsIn,
    remixSettings_channelsOut,

    -- ** ReservationPlan
    reservationPlan_commitment,
    reservationPlan_expiresAt,
    reservationPlan_status,
    reservationPlan_renewalType,
    reservationPlan_reservedSlots,
    reservationPlan_purchasedAt,

    -- ** ReservationPlanSettings
    reservationPlanSettings_commitment,
    reservationPlanSettings_reservedSlots,
    reservationPlanSettings_renewalType,

    -- ** ResourceTags
    resourceTags_tags,
    resourceTags_arn,

    -- ** S3DestinationAccessControl
    s3DestinationAccessControl_cannedAcl,

    -- ** S3DestinationSettings
    s3DestinationSettings_accessControl,
    s3DestinationSettings_encryption,

    -- ** S3EncryptionSettings
    s3EncryptionSettings_kmsEncryptionContext,
    s3EncryptionSettings_kmsKeyArn,
    s3EncryptionSettings_encryptionType,

    -- ** SccDestinationSettings
    sccDestinationSettings_framerate,

    -- ** SpekeKeyProvider
    spekeKeyProvider_resourceId,
    spekeKeyProvider_url,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_systemIds,

    -- ** SpekeKeyProviderCmaf
    spekeKeyProviderCmaf_resourceId,
    spekeKeyProviderCmaf_dashSignaledSystemIds,
    spekeKeyProviderCmaf_url,
    spekeKeyProviderCmaf_hlsSignaledSystemIds,
    spekeKeyProviderCmaf_certificateArn,

    -- ** SrtDestinationSettings
    srtDestinationSettings_stylePassthrough,

    -- ** StaticKeyProvider
    staticKeyProvider_staticKeyValue,
    staticKeyProvider_url,
    staticKeyProvider_keyFormat,
    staticKeyProvider_keyFormatVersions,

    -- ** TeletextDestinationSettings
    teletextDestinationSettings_pageNumber,
    teletextDestinationSettings_pageTypes,

    -- ** TeletextSourceSettings
    teletextSourceSettings_pageNumber,

    -- ** TimecodeBurnin
    timecodeBurnin_fontSize,
    timecodeBurnin_prefix,
    timecodeBurnin_position,

    -- ** TimecodeConfig
    timecodeConfig_timestampOffset,
    timecodeConfig_start,
    timecodeConfig_source,
    timecodeConfig_anchor,

    -- ** TimedMetadataInsertion
    timedMetadataInsertion_id3Insertions,

    -- ** Timing
    timing_submitTime,
    timing_finishTime,
    timing_startTime,

    -- ** TrackSourceSettings
    trackSourceSettings_trackNumber,

    -- ** TtmlDestinationSettings
    ttmlDestinationSettings_stylePassthrough,

    -- ** Vc3Settings
    vc3Settings_telecine,
    vc3Settings_framerateDenominator,
    vc3Settings_scanTypeConversionMode,
    vc3Settings_framerateControl,
    vc3Settings_framerateNumerator,
    vc3Settings_slowPal,
    vc3Settings_interlaceMode,
    vc3Settings_framerateConversionAlgorithm,
    vc3Settings_vc3Class,

    -- ** VideoCodecSettings
    videoCodecSettings_xavcSettings,
    videoCodecSettings_avcIntraSettings,
    videoCodecSettings_av1Settings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_vp9Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_vc3Settings,
    videoCodecSettings_codec,
    videoCodecSettings_vp8Settings,
    videoCodecSettings_proresSettings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_frameCaptureSettings,

    -- ** VideoDescription
    videoDescription_respondToAfd,
    videoDescription_antiAlias,
    videoDescription_afdSignaling,
    videoDescription_videoPreprocessors,
    videoDescription_fixedAfd,
    videoDescription_sharpness,
    videoDescription_codecSettings,
    videoDescription_width,
    videoDescription_scalingBehavior,
    videoDescription_crop,
    videoDescription_timecodeInsertion,
    videoDescription_dropFrameTimecode,
    videoDescription_colorMetadata,
    videoDescription_height,
    videoDescription_position,

    -- ** VideoDetail
    videoDetail_widthInPx,
    videoDetail_heightInPx,

    -- ** VideoPreprocessor
    videoPreprocessor_dolbyVision,
    videoPreprocessor_partnerWatermarking,
    videoPreprocessor_colorCorrector,
    videoPreprocessor_timecodeBurnin,
    videoPreprocessor_hdr10Plus,
    videoPreprocessor_imageInserter,
    videoPreprocessor_deinterlacer,
    videoPreprocessor_noiseReducer,

    -- ** VideoSelector
    videoSelector_alphaBehavior,
    videoSelector_pid,
    videoSelector_colorSpace,
    videoSelector_padVideo,
    videoSelector_colorSpaceUsage,
    videoSelector_embeddedTimecodeOverride,
    videoSelector_sampleRange,
    videoSelector_programNumber,
    videoSelector_hdr10Metadata,
    videoSelector_rotate,

    -- ** VorbisSettings
    vorbisSettings_channels,
    vorbisSettings_sampleRate,
    vorbisSettings_vbrQuality,

    -- ** Vp8Settings
    vp8Settings_parNumerator,
    vp8Settings_framerateDenominator,
    vp8Settings_hrdBufferSize,
    vp8Settings_bitrate,
    vp8Settings_framerateControl,
    vp8Settings_parControl,
    vp8Settings_qualityTuningLevel,
    vp8Settings_maxBitrate,
    vp8Settings_framerateNumerator,
    vp8Settings_parDenominator,
    vp8Settings_rateControlMode,
    vp8Settings_framerateConversionAlgorithm,
    vp8Settings_gopSize,

    -- ** Vp9Settings
    vp9Settings_parNumerator,
    vp9Settings_framerateDenominator,
    vp9Settings_hrdBufferSize,
    vp9Settings_bitrate,
    vp9Settings_framerateControl,
    vp9Settings_parControl,
    vp9Settings_qualityTuningLevel,
    vp9Settings_maxBitrate,
    vp9Settings_framerateNumerator,
    vp9Settings_parDenominator,
    vp9Settings_rateControlMode,
    vp9Settings_framerateConversionAlgorithm,
    vp9Settings_gopSize,

    -- ** WavSettings
    wavSettings_bitDepth,
    wavSettings_channels,
    wavSettings_format,
    wavSettings_sampleRate,

    -- ** WebvttDestinationSettings
    webvttDestinationSettings_stylePassthrough,
    webvttDestinationSettings_accessibility,

    -- ** WebvttHlsSourceSettings
    webvttHlsSourceSettings_renditionLanguageCode,
    webvttHlsSourceSettings_renditionName,
    webvttHlsSourceSettings_renditionGroupId,

    -- ** Xavc4kIntraCbgProfileSettings
    xavc4kIntraCbgProfileSettings_xavcClass,

    -- ** Xavc4kIntraVbrProfileSettings
    xavc4kIntraVbrProfileSettings_xavcClass,

    -- ** Xavc4kProfileSettings
    xavc4kProfileSettings_hrdBufferSize,
    xavc4kProfileSettings_qualityTuningLevel,
    xavc4kProfileSettings_codecProfile,
    xavc4kProfileSettings_bitrateClass,
    xavc4kProfileSettings_gopClosedCadence,
    xavc4kProfileSettings_slices,
    xavc4kProfileSettings_flickerAdaptiveQuantization,
    xavc4kProfileSettings_gopBReference,

    -- ** XavcHdIntraCbgProfileSettings
    xavcHdIntraCbgProfileSettings_xavcClass,

    -- ** XavcHdProfileSettings
    xavcHdProfileSettings_telecine,
    xavcHdProfileSettings_hrdBufferSize,
    xavcHdProfileSettings_qualityTuningLevel,
    xavcHdProfileSettings_bitrateClass,
    xavcHdProfileSettings_gopClosedCadence,
    xavcHdProfileSettings_interlaceMode,
    xavcHdProfileSettings_slices,
    xavcHdProfileSettings_flickerAdaptiveQuantization,
    xavcHdProfileSettings_gopBReference,

    -- ** XavcSettings
    xavcSettings_framerateDenominator,
    xavcSettings_xavc4kIntraVbrProfileSettings,
    xavcSettings_framerateControl,
    xavcSettings_profile,
    xavcSettings_framerateNumerator,
    xavcSettings_temporalAdaptiveQuantization,
    xavcSettings_xavcHdIntraCbgProfileSettings,
    xavcSettings_slowPal,
    xavcSettings_xavc4kIntraCbgProfileSettings,
    xavcSettings_entropyEncoding,
    xavcSettings_softness,
    xavcSettings_xavcHdProfileSettings,
    xavcSettings_xavc4kProfileSettings,
    xavcSettings_framerateConversionAlgorithm,
    xavcSettings_adaptiveQuantization,
    xavcSettings_spatialAdaptiveQuantization,
  )
where

import Amazonka.MediaConvert.AssociateCertificate
import Amazonka.MediaConvert.CancelJob
import Amazonka.MediaConvert.CreateJob
import Amazonka.MediaConvert.CreateJobTemplate
import Amazonka.MediaConvert.CreatePreset
import Amazonka.MediaConvert.CreateQueue
import Amazonka.MediaConvert.DeleteJobTemplate
import Amazonka.MediaConvert.DeletePolicy
import Amazonka.MediaConvert.DeletePreset
import Amazonka.MediaConvert.DeleteQueue
import Amazonka.MediaConvert.DescribeEndpoints
import Amazonka.MediaConvert.DisassociateCertificate
import Amazonka.MediaConvert.GetJob
import Amazonka.MediaConvert.GetJobTemplate
import Amazonka.MediaConvert.GetPolicy
import Amazonka.MediaConvert.GetPreset
import Amazonka.MediaConvert.GetQueue
import Amazonka.MediaConvert.ListJobTemplates
import Amazonka.MediaConvert.ListJobs
import Amazonka.MediaConvert.ListPresets
import Amazonka.MediaConvert.ListQueues
import Amazonka.MediaConvert.ListTagsForResource
import Amazonka.MediaConvert.PutPolicy
import Amazonka.MediaConvert.TagResource
import Amazonka.MediaConvert.Types.AacSettings
import Amazonka.MediaConvert.Types.Ac3Settings
import Amazonka.MediaConvert.Types.AccelerationSettings
import Amazonka.MediaConvert.Types.AiffSettings
import Amazonka.MediaConvert.Types.AllowedRenditionSize
import Amazonka.MediaConvert.Types.AncillarySourceSettings
import Amazonka.MediaConvert.Types.AudioChannelTaggingSettings
import Amazonka.MediaConvert.Types.AudioCodecSettings
import Amazonka.MediaConvert.Types.AudioDescription
import Amazonka.MediaConvert.Types.AudioNormalizationSettings
import Amazonka.MediaConvert.Types.AudioSelector
import Amazonka.MediaConvert.Types.AudioSelectorGroup
import Amazonka.MediaConvert.Types.AutomatedAbrRule
import Amazonka.MediaConvert.Types.AutomatedAbrSettings
import Amazonka.MediaConvert.Types.AutomatedEncodingSettings
import Amazonka.MediaConvert.Types.Av1QvbrSettings
import Amazonka.MediaConvert.Types.Av1Settings
import Amazonka.MediaConvert.Types.AvailBlanking
import Amazonka.MediaConvert.Types.AvcIntraSettings
import Amazonka.MediaConvert.Types.AvcIntraUhdSettings
import Amazonka.MediaConvert.Types.BurninDestinationSettings
import Amazonka.MediaConvert.Types.CaptionDescription
import Amazonka.MediaConvert.Types.CaptionDescriptionPreset
import Amazonka.MediaConvert.Types.CaptionDestinationSettings
import Amazonka.MediaConvert.Types.CaptionSelector
import Amazonka.MediaConvert.Types.CaptionSourceFramerate
import Amazonka.MediaConvert.Types.CaptionSourceSettings
import Amazonka.MediaConvert.Types.ChannelMapping
import Amazonka.MediaConvert.Types.CmafAdditionalManifest
import Amazonka.MediaConvert.Types.CmafEncryptionSettings
import Amazonka.MediaConvert.Types.CmafGroupSettings
import Amazonka.MediaConvert.Types.CmafImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.CmfcSettings
import Amazonka.MediaConvert.Types.ColorCorrector
import Amazonka.MediaConvert.Types.ContainerSettings
import Amazonka.MediaConvert.Types.DashAdditionalManifest
import Amazonka.MediaConvert.Types.DashIsoEncryptionSettings
import Amazonka.MediaConvert.Types.DashIsoGroupSettings
import Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.Deinterlacer
import Amazonka.MediaConvert.Types.DestinationSettings
import Amazonka.MediaConvert.Types.DolbyVision
import Amazonka.MediaConvert.Types.DolbyVisionLevel6Metadata
import Amazonka.MediaConvert.Types.DvbNitSettings
import Amazonka.MediaConvert.Types.DvbSdtSettings
import Amazonka.MediaConvert.Types.DvbSubDestinationSettings
import Amazonka.MediaConvert.Types.DvbSubSourceSettings
import Amazonka.MediaConvert.Types.DvbTdtSettings
import Amazonka.MediaConvert.Types.Eac3AtmosSettings
import Amazonka.MediaConvert.Types.Eac3Settings
import Amazonka.MediaConvert.Types.EmbeddedDestinationSettings
import Amazonka.MediaConvert.Types.EmbeddedSourceSettings
import Amazonka.MediaConvert.Types.Endpoint
import Amazonka.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Amazonka.MediaConvert.Types.EsamSettings
import Amazonka.MediaConvert.Types.EsamSignalProcessingNotification
import Amazonka.MediaConvert.Types.ExtendedDataServices
import Amazonka.MediaConvert.Types.F4vSettings
import Amazonka.MediaConvert.Types.FileGroupSettings
import Amazonka.MediaConvert.Types.FileSourceSettings
import Amazonka.MediaConvert.Types.ForceIncludeRenditionSize
import Amazonka.MediaConvert.Types.FrameCaptureSettings
import Amazonka.MediaConvert.Types.H264QvbrSettings
import Amazonka.MediaConvert.Types.H264Settings
import Amazonka.MediaConvert.Types.H265QvbrSettings
import Amazonka.MediaConvert.Types.H265Settings
import Amazonka.MediaConvert.Types.Hdr10Metadata
import Amazonka.MediaConvert.Types.Hdr10Plus
import Amazonka.MediaConvert.Types.HlsAdditionalManifest
import Amazonka.MediaConvert.Types.HlsCaptionLanguageMapping
import Amazonka.MediaConvert.Types.HlsEncryptionSettings
import Amazonka.MediaConvert.Types.HlsGroupSettings
import Amazonka.MediaConvert.Types.HlsImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.HlsRenditionGroupSettings
import Amazonka.MediaConvert.Types.HlsSettings
import Amazonka.MediaConvert.Types.HopDestination
import Amazonka.MediaConvert.Types.Id3Insertion
import Amazonka.MediaConvert.Types.ImageInserter
import Amazonka.MediaConvert.Types.ImscDestinationSettings
import Amazonka.MediaConvert.Types.Input
import Amazonka.MediaConvert.Types.InputClipping
import Amazonka.MediaConvert.Types.InputDecryptionSettings
import Amazonka.MediaConvert.Types.InputTemplate
import Amazonka.MediaConvert.Types.InputVideoGenerator
import Amazonka.MediaConvert.Types.InsertableImage
import Amazonka.MediaConvert.Types.Job
import Amazonka.MediaConvert.Types.JobMessages
import Amazonka.MediaConvert.Types.JobSettings
import Amazonka.MediaConvert.Types.JobTemplate
import Amazonka.MediaConvert.Types.JobTemplateSettings
import Amazonka.MediaConvert.Types.KantarWatermarkSettings
import Amazonka.MediaConvert.Types.M2tsScte35Esam
import Amazonka.MediaConvert.Types.M2tsSettings
import Amazonka.MediaConvert.Types.M3u8Settings
import Amazonka.MediaConvert.Types.MinBottomRenditionSize
import Amazonka.MediaConvert.Types.MinTopRenditionSize
import Amazonka.MediaConvert.Types.MotionImageInserter
import Amazonka.MediaConvert.Types.MotionImageInsertionFramerate
import Amazonka.MediaConvert.Types.MotionImageInsertionOffset
import Amazonka.MediaConvert.Types.MovSettings
import Amazonka.MediaConvert.Types.Mp2Settings
import Amazonka.MediaConvert.Types.Mp3Settings
import Amazonka.MediaConvert.Types.Mp4Settings
import Amazonka.MediaConvert.Types.MpdSettings
import Amazonka.MediaConvert.Types.Mpeg2Settings
import Amazonka.MediaConvert.Types.MsSmoothAdditionalManifest
import Amazonka.MediaConvert.Types.MsSmoothEncryptionSettings
import Amazonka.MediaConvert.Types.MsSmoothGroupSettings
import Amazonka.MediaConvert.Types.MxfSettings
import Amazonka.MediaConvert.Types.MxfXavcProfileSettings
import Amazonka.MediaConvert.Types.NexGuardFileMarkerSettings
import Amazonka.MediaConvert.Types.NielsenConfiguration
import Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Amazonka.MediaConvert.Types.NoiseReducer
import Amazonka.MediaConvert.Types.NoiseReducerFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import Amazonka.MediaConvert.Types.OpusSettings
import Amazonka.MediaConvert.Types.Output
import Amazonka.MediaConvert.Types.OutputChannelMapping
import Amazonka.MediaConvert.Types.OutputDetail
import Amazonka.MediaConvert.Types.OutputGroup
import Amazonka.MediaConvert.Types.OutputGroupDetail
import Amazonka.MediaConvert.Types.OutputGroupSettings
import Amazonka.MediaConvert.Types.OutputSettings
import Amazonka.MediaConvert.Types.PartnerWatermarking
import Amazonka.MediaConvert.Types.Policy
import Amazonka.MediaConvert.Types.Preset
import Amazonka.MediaConvert.Types.PresetSettings
import Amazonka.MediaConvert.Types.ProresSettings
import Amazonka.MediaConvert.Types.Queue
import Amazonka.MediaConvert.Types.QueueTransition
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.RemixSettings
import Amazonka.MediaConvert.Types.ReservationPlan
import Amazonka.MediaConvert.Types.ReservationPlanSettings
import Amazonka.MediaConvert.Types.ResourceTags
import Amazonka.MediaConvert.Types.S3DestinationAccessControl
import Amazonka.MediaConvert.Types.S3DestinationSettings
import Amazonka.MediaConvert.Types.S3EncryptionSettings
import Amazonka.MediaConvert.Types.SccDestinationSettings
import Amazonka.MediaConvert.Types.SpekeKeyProvider
import Amazonka.MediaConvert.Types.SpekeKeyProviderCmaf
import Amazonka.MediaConvert.Types.SrtDestinationSettings
import Amazonka.MediaConvert.Types.StaticKeyProvider
import Amazonka.MediaConvert.Types.TeletextDestinationSettings
import Amazonka.MediaConvert.Types.TeletextSourceSettings
import Amazonka.MediaConvert.Types.TimecodeBurnin
import Amazonka.MediaConvert.Types.TimecodeConfig
import Amazonka.MediaConvert.Types.TimedMetadataInsertion
import Amazonka.MediaConvert.Types.Timing
import Amazonka.MediaConvert.Types.TrackSourceSettings
import Amazonka.MediaConvert.Types.TtmlDestinationSettings
import Amazonka.MediaConvert.Types.Vc3Settings
import Amazonka.MediaConvert.Types.VideoCodecSettings
import Amazonka.MediaConvert.Types.VideoDescription
import Amazonka.MediaConvert.Types.VideoDetail
import Amazonka.MediaConvert.Types.VideoPreprocessor
import Amazonka.MediaConvert.Types.VideoSelector
import Amazonka.MediaConvert.Types.VorbisSettings
import Amazonka.MediaConvert.Types.Vp8Settings
import Amazonka.MediaConvert.Types.Vp9Settings
import Amazonka.MediaConvert.Types.WavSettings
import Amazonka.MediaConvert.Types.WebvttDestinationSettings
import Amazonka.MediaConvert.Types.WebvttHlsSourceSettings
import Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kProfileSettings
import Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.XavcHdProfileSettings
import Amazonka.MediaConvert.Types.XavcSettings
import Amazonka.MediaConvert.UntagResource
import Amazonka.MediaConvert.UpdateJobTemplate
import Amazonka.MediaConvert.UpdatePreset
import Amazonka.MediaConvert.UpdateQueue
