{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _BadGatewayException,
    _InternalServerErrorException,
    _GatewayTimeoutException,
    _ForbiddenException,
    _ConflictException,
    _UnprocessableEntityException,
    _TooManyRequestsException,

    -- * AacCodingMode
    AacCodingMode (..),

    -- * AacInputType
    AacInputType (..),

    -- * AacProfile
    AacProfile (..),

    -- * AacRateControlMode
    AacRateControlMode (..),

    -- * AacRawFormat
    AacRawFormat (..),

    -- * AacSpec
    AacSpec (..),

    -- * AacVbrQuality
    AacVbrQuality (..),

    -- * Ac3BitstreamMode
    Ac3BitstreamMode (..),

    -- * Ac3CodingMode
    Ac3CodingMode (..),

    -- * Ac3DrcProfile
    Ac3DrcProfile (..),

    -- * Ac3LfeFilter
    Ac3LfeFilter (..),

    -- * Ac3MetadataControl
    Ac3MetadataControl (..),

    -- * AcceptHeader
    AcceptHeader (..),

    -- * AfdSignaling
    AfdSignaling (..),

    -- * AudioDescriptionAudioTypeControl
    AudioDescriptionAudioTypeControl (..),

    -- * AudioDescriptionLanguageCodeControl
    AudioDescriptionLanguageCodeControl (..),

    -- * AudioLanguageSelectionPolicy
    AudioLanguageSelectionPolicy (..),

    -- * AudioNormalizationAlgorithm
    AudioNormalizationAlgorithm (..),

    -- * AudioNormalizationAlgorithmControl
    AudioNormalizationAlgorithmControl (..),

    -- * AudioOnlyHlsSegmentType
    AudioOnlyHlsSegmentType (..),

    -- * AudioOnlyHlsTrackType
    AudioOnlyHlsTrackType (..),

    -- * AudioType
    AudioType (..),

    -- * AuthenticationScheme
    AuthenticationScheme (..),

    -- * AvailBlankingState
    AvailBlankingState (..),

    -- * BlackoutSlateNetworkEndBlackout
    BlackoutSlateNetworkEndBlackout (..),

    -- * BlackoutSlateState
    BlackoutSlateState (..),

    -- * BurnInAlignment
    BurnInAlignment (..),

    -- * BurnInBackgroundColor
    BurnInBackgroundColor (..),

    -- * BurnInFontColor
    BurnInFontColor (..),

    -- * BurnInOutlineColor
    BurnInOutlineColor (..),

    -- * BurnInShadowColor
    BurnInShadowColor (..),

    -- * BurnInTeletextGridControl
    BurnInTeletextGridControl (..),

    -- * CdiInputResolution
    CdiInputResolution (..),

    -- * ChannelClass
    ChannelClass (..),

    -- * ChannelState
    ChannelState (..),

    -- * ContentType
    ContentType (..),

    -- * DeviceSettingsSyncState
    DeviceSettingsSyncState (..),

    -- * DeviceUpdateStatus
    DeviceUpdateStatus (..),

    -- * DvbSdtOutputSdt
    DvbSdtOutputSdt (..),

    -- * DvbSubDestinationAlignment
    DvbSubDestinationAlignment (..),

    -- * DvbSubDestinationBackgroundColor
    DvbSubDestinationBackgroundColor (..),

    -- * DvbSubDestinationFontColor
    DvbSubDestinationFontColor (..),

    -- * DvbSubDestinationOutlineColor
    DvbSubDestinationOutlineColor (..),

    -- * DvbSubDestinationShadowColor
    DvbSubDestinationShadowColor (..),

    -- * DvbSubDestinationTeletextGridControl
    DvbSubDestinationTeletextGridControl (..),

    -- * Eac3AttenuationControl
    Eac3AttenuationControl (..),

    -- * Eac3BitstreamMode
    Eac3BitstreamMode (..),

    -- * Eac3CodingMode
    Eac3CodingMode (..),

    -- * Eac3DcFilter
    Eac3DcFilter (..),

    -- * Eac3DrcLine
    Eac3DrcLine (..),

    -- * Eac3DrcRf
    Eac3DrcRf (..),

    -- * Eac3LfeControl
    Eac3LfeControl (..),

    -- * Eac3LfeFilter
    Eac3LfeFilter (..),

    -- * Eac3MetadataControl
    Eac3MetadataControl (..),

    -- * Eac3PassthroughControl
    Eac3PassthroughControl (..),

    -- * Eac3PhaseControl
    Eac3PhaseControl (..),

    -- * Eac3StereoDownmix
    Eac3StereoDownmix (..),

    -- * Eac3SurroundExMode
    Eac3SurroundExMode (..),

    -- * Eac3SurroundMode
    Eac3SurroundMode (..),

    -- * EbuTtDDestinationStyleControl
    EbuTtDDestinationStyleControl (..),

    -- * EbuTtDFillLineGapControl
    EbuTtDFillLineGapControl (..),

    -- * EmbeddedConvert608To708
    EmbeddedConvert608To708 (..),

    -- * EmbeddedScte20Detection
    EmbeddedScte20Detection (..),

    -- * FeatureActivationsInputPrepareScheduleActions
    FeatureActivationsInputPrepareScheduleActions (..),

    -- * FecOutputIncludeFec
    FecOutputIncludeFec (..),

    -- * FixedAfd
    FixedAfd (..),

    -- * Fmp4NielsenId3Behavior
    Fmp4NielsenId3Behavior (..),

    -- * Fmp4TimedMetadataBehavior
    Fmp4TimedMetadataBehavior (..),

    -- * FollowPoint
    FollowPoint (..),

    -- * FrameCaptureIntervalUnit
    FrameCaptureIntervalUnit (..),

    -- * GlobalConfigurationInputEndAction
    GlobalConfigurationInputEndAction (..),

    -- * GlobalConfigurationLowFramerateInputs
    GlobalConfigurationLowFramerateInputs (..),

    -- * GlobalConfigurationOutputLockingMode
    GlobalConfigurationOutputLockingMode (..),

    -- * GlobalConfigurationOutputTimingSource
    GlobalConfigurationOutputTimingSource (..),

    -- * H264AdaptiveQuantization
    H264AdaptiveQuantization (..),

    -- * H264ColorMetadata
    H264ColorMetadata (..),

    -- * H264EntropyEncoding
    H264EntropyEncoding (..),

    -- * H264FlickerAq
    H264FlickerAq (..),

    -- * H264ForceFieldPictures
    H264ForceFieldPictures (..),

    -- * H264FramerateControl
    H264FramerateControl (..),

    -- * H264GopBReference
    H264GopBReference (..),

    -- * H264GopSizeUnits
    H264GopSizeUnits (..),

    -- * H264Level
    H264Level (..),

    -- * H264LookAheadRateControl
    H264LookAheadRateControl (..),

    -- * H264ParControl
    H264ParControl (..),

    -- * H264Profile
    H264Profile (..),

    -- * H264QualityLevel
    H264QualityLevel (..),

    -- * H264RateControlMode
    H264RateControlMode (..),

    -- * H264ScanType
    H264ScanType (..),

    -- * H264SceneChangeDetect
    H264SceneChangeDetect (..),

    -- * H264SpatialAq
    H264SpatialAq (..),

    -- * H264SubGopLength
    H264SubGopLength (..),

    -- * H264Syntax
    H264Syntax (..),

    -- * H264TemporalAq
    H264TemporalAq (..),

    -- * H264TimecodeInsertionBehavior
    H264TimecodeInsertionBehavior (..),

    -- * H265AdaptiveQuantization
    H265AdaptiveQuantization (..),

    -- * H265AlternativeTransferFunction
    H265AlternativeTransferFunction (..),

    -- * H265ColorMetadata
    H265ColorMetadata (..),

    -- * H265FlickerAq
    H265FlickerAq (..),

    -- * H265GopSizeUnits
    H265GopSizeUnits (..),

    -- * H265Level
    H265Level (..),

    -- * H265LookAheadRateControl
    H265LookAheadRateControl (..),

    -- * H265Profile
    H265Profile (..),

    -- * H265RateControlMode
    H265RateControlMode (..),

    -- * H265ScanType
    H265ScanType (..),

    -- * H265SceneChangeDetect
    H265SceneChangeDetect (..),

    -- * H265Tier
    H265Tier (..),

    -- * H265TimecodeInsertionBehavior
    H265TimecodeInsertionBehavior (..),

    -- * HlsAdMarkers
    HlsAdMarkers (..),

    -- * HlsAkamaiHttpTransferMode
    HlsAkamaiHttpTransferMode (..),

    -- * HlsCaptionLanguageSetting
    HlsCaptionLanguageSetting (..),

    -- * HlsClientCache
    HlsClientCache (..),

    -- * HlsCodecSpecification
    HlsCodecSpecification (..),

    -- * HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- * HlsDiscontinuityTags
    HlsDiscontinuityTags (..),

    -- * HlsEncryptionType
    HlsEncryptionType (..),

    -- * HlsH265PackagingType
    HlsH265PackagingType (..),

    -- * HlsId3SegmentTaggingState
    HlsId3SegmentTaggingState (..),

    -- * HlsIncompleteSegmentBehavior
    HlsIncompleteSegmentBehavior (..),

    -- * HlsIvInManifest
    HlsIvInManifest (..),

    -- * HlsIvSource
    HlsIvSource (..),

    -- * HlsManifestCompression
    HlsManifestCompression (..),

    -- * HlsManifestDurationFormat
    HlsManifestDurationFormat (..),

    -- * HlsMediaStoreStorageClass
    HlsMediaStoreStorageClass (..),

    -- * HlsMode
    HlsMode (..),

    -- * HlsOutputSelection
    HlsOutputSelection (..),

    -- * HlsProgramDateTime
    HlsProgramDateTime (..),

    -- * HlsRedundantManifest
    HlsRedundantManifest (..),

    -- * HlsSegmentationMode
    HlsSegmentationMode (..),

    -- * HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- * HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- * HlsTsFileMode
    HlsTsFileMode (..),

    -- * HlsWebdavHttpTransferMode
    HlsWebdavHttpTransferMode (..),

    -- * IFrameOnlyPlaylistType
    IFrameOnlyPlaylistType (..),

    -- * InputClass
    InputClass (..),

    -- * InputCodec
    InputCodec (..),

    -- * InputDeblockFilter
    InputDeblockFilter (..),

    -- * InputDenoiseFilter
    InputDenoiseFilter (..),

    -- * InputDeviceActiveInput
    InputDeviceActiveInput (..),

    -- * InputDeviceConfiguredInput
    InputDeviceConfiguredInput (..),

    -- * InputDeviceConnectionState
    InputDeviceConnectionState (..),

    -- * InputDeviceIpScheme
    InputDeviceIpScheme (..),

    -- * InputDeviceScanType
    InputDeviceScanType (..),

    -- * InputDeviceState
    InputDeviceState (..),

    -- * InputDeviceTransferType
    InputDeviceTransferType (..),

    -- * InputDeviceType
    InputDeviceType (..),

    -- * InputFilter
    InputFilter (..),

    -- * InputLossActionForHlsOut
    InputLossActionForHlsOut (..),

    -- * InputLossActionForMsSmoothOut
    InputLossActionForMsSmoothOut (..),

    -- * InputLossActionForRtmpOut
    InputLossActionForRtmpOut (..),

    -- * InputLossActionForUdpOut
    InputLossActionForUdpOut (..),

    -- * InputLossImageType
    InputLossImageType (..),

    -- * InputMaximumBitrate
    InputMaximumBitrate (..),

    -- * InputPreference
    InputPreference (..),

    -- * InputResolution
    InputResolution (..),

    -- * InputSecurityGroupState
    InputSecurityGroupState (..),

    -- * InputSourceEndBehavior
    InputSourceEndBehavior (..),

    -- * InputSourceType
    InputSourceType (..),

    -- * InputState
    InputState (..),

    -- * InputTimecodeSource
    InputTimecodeSource (..),

    -- * InputType
    InputType (..),

    -- * LastFrameClippingBehavior
    LastFrameClippingBehavior (..),

    -- * LogLevel
    LogLevel (..),

    -- * M2tsAbsentInputAudioBehavior
    M2tsAbsentInputAudioBehavior (..),

    -- * M2tsArib
    M2tsArib (..),

    -- * M2tsAribCaptionsPidControl
    M2tsAribCaptionsPidControl (..),

    -- * M2tsAudioBufferModel
    M2tsAudioBufferModel (..),

    -- * M2tsAudioInterval
    M2tsAudioInterval (..),

    -- * M2tsAudioStreamType
    M2tsAudioStreamType (..),

    -- * M2tsBufferModel
    M2tsBufferModel (..),

    -- * M2tsCcDescriptor
    M2tsCcDescriptor (..),

    -- * M2tsEbifControl
    M2tsEbifControl (..),

    -- * M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- * M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- * M2tsKlv
    M2tsKlv (..),

    -- * M2tsNielsenId3Behavior
    M2tsNielsenId3Behavior (..),

    -- * M2tsPcrControl
    M2tsPcrControl (..),

    -- * M2tsRateMode
    M2tsRateMode (..),

    -- * M2tsScte35Control
    M2tsScte35Control (..),

    -- * M2tsSegmentationMarkers
    M2tsSegmentationMarkers (..),

    -- * M2tsSegmentationStyle
    M2tsSegmentationStyle (..),

    -- * M2tsTimedMetadataBehavior
    M2tsTimedMetadataBehavior (..),

    -- * M3u8NielsenId3Behavior
    M3u8NielsenId3Behavior (..),

    -- * M3u8PcrControl
    M3u8PcrControl (..),

    -- * M3u8Scte35Behavior
    M3u8Scte35Behavior (..),

    -- * M3u8TimedMetadataBehavior
    M3u8TimedMetadataBehavior (..),

    -- * Mp2CodingMode
    Mp2CodingMode (..),

    -- * Mpeg2AdaptiveQuantization
    Mpeg2AdaptiveQuantization (..),

    -- * Mpeg2ColorMetadata
    Mpeg2ColorMetadata (..),

    -- * Mpeg2ColorSpace
    Mpeg2ColorSpace (..),

    -- * Mpeg2DisplayRatio
    Mpeg2DisplayRatio (..),

    -- * Mpeg2GopSizeUnits
    Mpeg2GopSizeUnits (..),

    -- * Mpeg2ScanType
    Mpeg2ScanType (..),

    -- * Mpeg2SubGopLength
    Mpeg2SubGopLength (..),

    -- * Mpeg2TimecodeInsertionBehavior
    Mpeg2TimecodeInsertionBehavior (..),

    -- * MsSmoothH265PackagingType
    MsSmoothH265PackagingType (..),

    -- * MultiplexState
    MultiplexState (..),

    -- * NetworkInputServerValidation
    NetworkInputServerValidation (..),

    -- * NielsenPcmToId3TaggingState
    NielsenPcmToId3TaggingState (..),

    -- * OfferingDurationUnits
    OfferingDurationUnits (..),

    -- * OfferingType
    OfferingType (..),

    -- * PipelineId
    PipelineId (..),

    -- * PreferredChannelPipeline
    PreferredChannelPipeline (..),

    -- * ReservationCodec
    ReservationCodec (..),

    -- * ReservationMaximumBitrate
    ReservationMaximumBitrate (..),

    -- * ReservationMaximumFramerate
    ReservationMaximumFramerate (..),

    -- * ReservationResolution
    ReservationResolution (..),

    -- * ReservationResourceType
    ReservationResourceType (..),

    -- * ReservationSpecialFeature
    ReservationSpecialFeature (..),

    -- * ReservationState
    ReservationState (..),

    -- * ReservationVideoQuality
    ReservationVideoQuality (..),

    -- * RtmpAdMarkers
    RtmpAdMarkers (..),

    -- * RtmpCacheFullBehavior
    RtmpCacheFullBehavior (..),

    -- * RtmpCaptionData
    RtmpCaptionData (..),

    -- * RtmpOutputCertificateMode
    RtmpOutputCertificateMode (..),

    -- * Scte20Convert608To708
    Scte20Convert608To708 (..),

    -- * Scte35AposNoRegionalBlackoutBehavior
    Scte35AposNoRegionalBlackoutBehavior (..),

    -- * Scte35AposWebDeliveryAllowedBehavior
    Scte35AposWebDeliveryAllowedBehavior (..),

    -- * Scte35ArchiveAllowedFlag
    Scte35ArchiveAllowedFlag (..),

    -- * Scte35DeviceRestrictions
    Scte35DeviceRestrictions (..),

    -- * Scte35NoRegionalBlackoutFlag
    Scte35NoRegionalBlackoutFlag (..),

    -- * Scte35SegmentationCancelIndicator
    Scte35SegmentationCancelIndicator (..),

    -- * Scte35SpliceInsertNoRegionalBlackoutBehavior
    Scte35SpliceInsertNoRegionalBlackoutBehavior (..),

    -- * Scte35SpliceInsertWebDeliveryAllowedBehavior
    Scte35SpliceInsertWebDeliveryAllowedBehavior (..),

    -- * Scte35WebDeliveryAllowedFlag
    Scte35WebDeliveryAllowedFlag (..),

    -- * SmoothGroupAudioOnlyTimecodeControl
    SmoothGroupAudioOnlyTimecodeControl (..),

    -- * SmoothGroupCertificateMode
    SmoothGroupCertificateMode (..),

    -- * SmoothGroupEventIdMode
    SmoothGroupEventIdMode (..),

    -- * SmoothGroupEventStopBehavior
    SmoothGroupEventStopBehavior (..),

    -- * SmoothGroupSegmentationMode
    SmoothGroupSegmentationMode (..),

    -- * SmoothGroupSparseTrackType
    SmoothGroupSparseTrackType (..),

    -- * SmoothGroupStreamManifestBehavior
    SmoothGroupStreamManifestBehavior (..),

    -- * SmoothGroupTimestampOffsetMode
    SmoothGroupTimestampOffsetMode (..),

    -- * Smpte2038DataPreference
    Smpte2038DataPreference (..),

    -- * TemporalFilterPostFilterSharpening
    TemporalFilterPostFilterSharpening (..),

    -- * TemporalFilterStrength
    TemporalFilterStrength (..),

    -- * TimecodeConfigSource
    TimecodeConfigSource (..),

    -- * TtmlDestinationStyleControl
    TtmlDestinationStyleControl (..),

    -- * UdpTimedMetadataId3Frame
    UdpTimedMetadataId3Frame (..),

    -- * VideoDescriptionRespondToAfd
    VideoDescriptionRespondToAfd (..),

    -- * VideoDescriptionScalingBehavior
    VideoDescriptionScalingBehavior (..),

    -- * VideoSelectorColorSpace
    VideoSelectorColorSpace (..),

    -- * VideoSelectorColorSpaceUsage
    VideoSelectorColorSpaceUsage (..),

    -- * WavCodingMode
    WavCodingMode (..),

    -- * AacSettings
    AacSettings (..),
    newAacSettings,
    aacSettings_rateControlMode,
    aacSettings_codingMode,
    aacSettings_spec,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_inputType,
    aacSettings_profile,
    aacSettings_vbrQuality,
    aacSettings_bitrate,

    -- * Ac3Settings
    Ac3Settings (..),
    newAc3Settings,
    ac3Settings_dialnorm,
    ac3Settings_drcProfile,
    ac3Settings_codingMode,
    ac3Settings_lfeFilter,
    ac3Settings_bitstreamMode,
    ac3Settings_bitrate,
    ac3Settings_metadataControl,

    -- * AncillarySourceSettings
    AncillarySourceSettings (..),
    newAncillarySourceSettings,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- * ArchiveContainerSettings
    ArchiveContainerSettings (..),
    newArchiveContainerSettings,
    archiveContainerSettings_rawSettings,
    archiveContainerSettings_m2tsSettings,

    -- * ArchiveGroupSettings
    ArchiveGroupSettings (..),
    newArchiveGroupSettings,
    archiveGroupSettings_rolloverInterval,
    archiveGroupSettings_destination,

    -- * ArchiveOutputSettings
    ArchiveOutputSettings (..),
    newArchiveOutputSettings,
    archiveOutputSettings_extension,
    archiveOutputSettings_nameModifier,
    archiveOutputSettings_containerSettings,

    -- * AribDestinationSettings
    AribDestinationSettings (..),
    newAribDestinationSettings,

    -- * AribSourceSettings
    AribSourceSettings (..),
    newAribSourceSettings,

    -- * AudioChannelMapping
    AudioChannelMapping (..),
    newAudioChannelMapping,
    audioChannelMapping_outputChannel,
    audioChannelMapping_inputChannelLevels,

    -- * AudioCodecSettings
    AudioCodecSettings (..),
    newAudioCodecSettings,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_passThroughSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_wavSettings,

    -- * AudioDescription
    AudioDescription (..),
    newAudioDescription,
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

    -- * AudioLanguageSelection
    AudioLanguageSelection (..),
    newAudioLanguageSelection,
    audioLanguageSelection_languageSelectionPolicy,
    audioLanguageSelection_languageCode,

    -- * AudioNormalizationSettings
    AudioNormalizationSettings (..),
    newAudioNormalizationSettings,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithmControl,

    -- * AudioOnlyHlsSettings
    AudioOnlyHlsSettings (..),
    newAudioOnlyHlsSettings,
    audioOnlyHlsSettings_audioGroupId,
    audioOnlyHlsSettings_audioTrackType,
    audioOnlyHlsSettings_segmentType,
    audioOnlyHlsSettings_audioOnlyImage,

    -- * AudioPidSelection
    AudioPidSelection (..),
    newAudioPidSelection,
    audioPidSelection_pid,

    -- * AudioSelector
    AudioSelector (..),
    newAudioSelector,
    audioSelector_selectorSettings,
    audioSelector_name,

    -- * AudioSelectorSettings
    AudioSelectorSettings (..),
    newAudioSelectorSettings,
    audioSelectorSettings_audioLanguageSelection,
    audioSelectorSettings_audioPidSelection,
    audioSelectorSettings_audioTrackSelection,

    -- * AudioSilenceFailoverSettings
    AudioSilenceFailoverSettings (..),
    newAudioSilenceFailoverSettings,
    audioSilenceFailoverSettings_audioSilenceThresholdMsec,
    audioSilenceFailoverSettings_audioSelectorName,

    -- * AudioTrack
    AudioTrack (..),
    newAudioTrack,
    audioTrack_track,

    -- * AudioTrackSelection
    AudioTrackSelection (..),
    newAudioTrackSelection,
    audioTrackSelection_tracks,

    -- * AutomaticInputFailoverSettings
    AutomaticInputFailoverSettings (..),
    newAutomaticInputFailoverSettings,
    automaticInputFailoverSettings_failoverConditions,
    automaticInputFailoverSettings_errorClearTimeMsec,
    automaticInputFailoverSettings_inputPreference,
    automaticInputFailoverSettings_secondaryInputId,

    -- * AvailBlanking
    AvailBlanking (..),
    newAvailBlanking,
    availBlanking_state,
    availBlanking_availBlankingImage,

    -- * AvailConfiguration
    AvailConfiguration (..),
    newAvailConfiguration,
    availConfiguration_availSettings,

    -- * AvailSettings
    AvailSettings (..),
    newAvailSettings,
    availSettings_scte35TimeSignalApos,
    availSettings_scte35SpliceInsert,

    -- * BatchFailedResultModel
    BatchFailedResultModel (..),
    newBatchFailedResultModel,
    batchFailedResultModel_message,
    batchFailedResultModel_arn,
    batchFailedResultModel_id,
    batchFailedResultModel_code,

    -- * BatchScheduleActionCreateRequest
    BatchScheduleActionCreateRequest (..),
    newBatchScheduleActionCreateRequest,
    batchScheduleActionCreateRequest_scheduleActions,

    -- * BatchScheduleActionCreateResult
    BatchScheduleActionCreateResult (..),
    newBatchScheduleActionCreateResult,
    batchScheduleActionCreateResult_scheduleActions,

    -- * BatchScheduleActionDeleteRequest
    BatchScheduleActionDeleteRequest (..),
    newBatchScheduleActionDeleteRequest,
    batchScheduleActionDeleteRequest_actionNames,

    -- * BatchScheduleActionDeleteResult
    BatchScheduleActionDeleteResult (..),
    newBatchScheduleActionDeleteResult,
    batchScheduleActionDeleteResult_scheduleActions,

    -- * BatchSuccessfulResultModel
    BatchSuccessfulResultModel (..),
    newBatchSuccessfulResultModel,
    batchSuccessfulResultModel_arn,
    batchSuccessfulResultModel_id,
    batchSuccessfulResultModel_state,

    -- * BlackoutSlate
    BlackoutSlate (..),
    newBlackoutSlate,
    blackoutSlate_blackoutSlateImage,
    blackoutSlate_networkEndBlackout,
    blackoutSlate_state,
    blackoutSlate_networkEndBlackoutImage,
    blackoutSlate_networkId,

    -- * BurnInDestinationSettings
    BurnInDestinationSettings (..),
    newBurnInDestinationSettings,
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

    -- * CaptionDescription
    CaptionDescription (..),
    newCaptionDescription,
    captionDescription_languageCode,
    captionDescription_languageDescription,
    captionDescription_destinationSettings,
    captionDescription_captionSelectorName,
    captionDescription_name,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings (..),
    newCaptionDestinationSettings,
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

    -- * CaptionLanguageMapping
    CaptionLanguageMapping (..),
    newCaptionLanguageMapping,
    captionLanguageMapping_languageCode,
    captionLanguageMapping_languageDescription,
    captionLanguageMapping_captionChannel,

    -- * CaptionSelector
    CaptionSelector (..),
    newCaptionSelector,
    captionSelector_languageCode,
    captionSelector_selectorSettings,
    captionSelector_name,

    -- * CaptionSelectorSettings
    CaptionSelectorSettings (..),
    newCaptionSelectorSettings,
    captionSelectorSettings_ancillarySourceSettings,
    captionSelectorSettings_embeddedSourceSettings,
    captionSelectorSettings_aribSourceSettings,
    captionSelectorSettings_scte27SourceSettings,
    captionSelectorSettings_dvbSubSourceSettings,
    captionSelectorSettings_scte20SourceSettings,
    captionSelectorSettings_teletextSourceSettings,

    -- * CdiInputSpecification
    CdiInputSpecification (..),
    newCdiInputSpecification,
    cdiInputSpecification_resolution,

    -- * Channel
    Channel (..),
    newChannel,
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

    -- * ChannelEgressEndpoint
    ChannelEgressEndpoint (..),
    newChannelEgressEndpoint,
    channelEgressEndpoint_sourceIp,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
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

    -- * ColorSpacePassthroughSettings
    ColorSpacePassthroughSettings (..),
    newColorSpacePassthroughSettings,

    -- * DvbNitSettings
    DvbNitSettings (..),
    newDvbNitSettings,
    dvbNitSettings_repInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- * DvbSdtSettings
    DvbSdtSettings (..),
    newDvbSdtSettings,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_repInterval,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings (..),
    newDvbSubDestinationSettings,
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

    -- * DvbSubSourceSettings
    DvbSubSourceSettings (..),
    newDvbSubSourceSettings,
    dvbSubSourceSettings_pid,

    -- * DvbTdtSettings
    DvbTdtSettings (..),
    newDvbTdtSettings,
    dvbTdtSettings_repInterval,

    -- * Eac3Settings
    Eac3Settings (..),
    newEac3Settings,
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

    -- * EbuTtDDestinationSettings
    EbuTtDDestinationSettings (..),
    newEbuTtDDestinationSettings,
    ebuTtDDestinationSettings_fillLineGap,
    ebuTtDDestinationSettings_styleControl,
    ebuTtDDestinationSettings_fontFamily,

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    newEmbeddedDestinationSettings,

    -- * EmbeddedPlusScte20DestinationSettings
    EmbeddedPlusScte20DestinationSettings (..),
    newEmbeddedPlusScte20DestinationSettings,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    newEmbeddedSourceSettings,
    embeddedSourceSettings_scte20Detection,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- * EncoderSettings
    EncoderSettings (..),
    newEncoderSettings,
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

    -- * FailoverCondition
    FailoverCondition (..),
    newFailoverCondition,
    failoverCondition_failoverConditionSettings,

    -- * FailoverConditionSettings
    FailoverConditionSettings (..),
    newFailoverConditionSettings,
    failoverConditionSettings_videoBlackSettings,
    failoverConditionSettings_inputLossSettings,
    failoverConditionSettings_audioSilenceSettings,

    -- * FeatureActivations
    FeatureActivations (..),
    newFeatureActivations,
    featureActivations_inputPrepareScheduleActions,

    -- * FecOutputSettings
    FecOutputSettings (..),
    newFecOutputSettings,
    fecOutputSettings_rowLength,
    fecOutputSettings_columnDepth,
    fecOutputSettings_includeFec,

    -- * FixedModeScheduleActionStartSettings
    FixedModeScheduleActionStartSettings (..),
    newFixedModeScheduleActionStartSettings,
    fixedModeScheduleActionStartSettings_time,

    -- * Fmp4HlsSettings
    Fmp4HlsSettings (..),
    newFmp4HlsSettings,
    fmp4HlsSettings_audioRenditionSets,
    fmp4HlsSettings_nielsenId3Behavior,
    fmp4HlsSettings_timedMetadataBehavior,

    -- * FollowModeScheduleActionStartSettings
    FollowModeScheduleActionStartSettings (..),
    newFollowModeScheduleActionStartSettings,
    followModeScheduleActionStartSettings_referenceActionName,
    followModeScheduleActionStartSettings_followPoint,

    -- * FrameCaptureGroupSettings
    FrameCaptureGroupSettings (..),
    newFrameCaptureGroupSettings,
    frameCaptureGroupSettings_destination,

    -- * FrameCaptureHlsSettings
    FrameCaptureHlsSettings (..),
    newFrameCaptureHlsSettings,

    -- * FrameCaptureOutputSettings
    FrameCaptureOutputSettings (..),
    newFrameCaptureOutputSettings,
    frameCaptureOutputSettings_nameModifier,

    -- * FrameCaptureSettings
    FrameCaptureSettings (..),
    newFrameCaptureSettings,
    frameCaptureSettings_captureInterval,
    frameCaptureSettings_captureIntervalUnits,

    -- * GlobalConfiguration
    GlobalConfiguration (..),
    newGlobalConfiguration,
    globalConfiguration_initialAudioGain,
    globalConfiguration_outputLockingMode,
    globalConfiguration_inputEndAction,
    globalConfiguration_inputLossBehavior,
    globalConfiguration_supportLowFramerateInputs,
    globalConfiguration_outputTimingSource,

    -- * H264ColorSpaceSettings
    H264ColorSpaceSettings (..),
    newH264ColorSpaceSettings,
    h264ColorSpaceSettings_rec601Settings,
    h264ColorSpaceSettings_rec709Settings,
    h264ColorSpaceSettings_colorSpacePassthroughSettings,

    -- * H264FilterSettings
    H264FilterSettings (..),
    newH264FilterSettings,
    h264FilterSettings_temporalFilterSettings,

    -- * H264Settings
    H264Settings (..),
    newH264Settings,
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

    -- * H265ColorSpaceSettings
    H265ColorSpaceSettings (..),
    newH265ColorSpaceSettings,
    h265ColorSpaceSettings_rec601Settings,
    h265ColorSpaceSettings_rec709Settings,
    h265ColorSpaceSettings_colorSpacePassthroughSettings,
    h265ColorSpaceSettings_hdr10Settings,

    -- * H265FilterSettings
    H265FilterSettings (..),
    newH265FilterSettings,
    h265FilterSettings_temporalFilterSettings,

    -- * H265Settings
    H265Settings (..),
    newH265Settings,
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

    -- * Hdr10Settings
    Hdr10Settings (..),
    newHdr10Settings,
    hdr10Settings_maxCll,
    hdr10Settings_maxFall,

    -- * HlsAkamaiSettings
    HlsAkamaiSettings (..),
    newHlsAkamaiSettings,
    hlsAkamaiSettings_filecacheDuration,
    hlsAkamaiSettings_numRetries,
    hlsAkamaiSettings_httpTransferMode,
    hlsAkamaiSettings_connectionRetryInterval,
    hlsAkamaiSettings_token,
    hlsAkamaiSettings_restartDelay,
    hlsAkamaiSettings_salt,

    -- * HlsBasicPutSettings
    HlsBasicPutSettings (..),
    newHlsBasicPutSettings,
    hlsBasicPutSettings_filecacheDuration,
    hlsBasicPutSettings_numRetries,
    hlsBasicPutSettings_connectionRetryInterval,
    hlsBasicPutSettings_restartDelay,

    -- * HlsCdnSettings
    HlsCdnSettings (..),
    newHlsCdnSettings,
    hlsCdnSettings_hlsBasicPutSettings,
    hlsCdnSettings_hlsWebdavSettings,
    hlsCdnSettings_hlsAkamaiSettings,
    hlsCdnSettings_hlsMediaStoreSettings,

    -- * HlsGroupSettings
    HlsGroupSettings (..),
    newHlsGroupSettings,
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

    -- * HlsId3SegmentTaggingScheduleActionSettings
    HlsId3SegmentTaggingScheduleActionSettings (..),
    newHlsId3SegmentTaggingScheduleActionSettings,
    hlsId3SegmentTaggingScheduleActionSettings_tag,

    -- * HlsInputSettings
    HlsInputSettings (..),
    newHlsInputSettings,
    hlsInputSettings_retryInterval,
    hlsInputSettings_bandwidth,
    hlsInputSettings_retries,
    hlsInputSettings_bufferSegments,

    -- * HlsMediaStoreSettings
    HlsMediaStoreSettings (..),
    newHlsMediaStoreSettings,
    hlsMediaStoreSettings_filecacheDuration,
    hlsMediaStoreSettings_numRetries,
    hlsMediaStoreSettings_mediaStoreStorageClass,
    hlsMediaStoreSettings_connectionRetryInterval,
    hlsMediaStoreSettings_restartDelay,

    -- * HlsOutputSettings
    HlsOutputSettings (..),
    newHlsOutputSettings,
    hlsOutputSettings_segmentModifier,
    hlsOutputSettings_h265PackagingType,
    hlsOutputSettings_nameModifier,
    hlsOutputSettings_hlsSettings,

    -- * HlsSettings
    HlsSettings (..),
    newHlsSettings,
    hlsSettings_standardHlsSettings,
    hlsSettings_frameCaptureHlsSettings,
    hlsSettings_audioOnlyHlsSettings,
    hlsSettings_fmp4HlsSettings,

    -- * HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings (..),
    newHlsTimedMetadataScheduleActionSettings,
    hlsTimedMetadataScheduleActionSettings_id3,

    -- * HlsWebdavSettings
    HlsWebdavSettings (..),
    newHlsWebdavSettings,
    hlsWebdavSettings_filecacheDuration,
    hlsWebdavSettings_numRetries,
    hlsWebdavSettings_httpTransferMode,
    hlsWebdavSettings_connectionRetryInterval,
    hlsWebdavSettings_restartDelay,

    -- * ImmediateModeScheduleActionStartSettings
    ImmediateModeScheduleActionStartSettings (..),
    newImmediateModeScheduleActionStartSettings,

    -- * Input
    Input (..),
    newInput,
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

    -- * InputAttachment
    InputAttachment (..),
    newInputAttachment,
    inputAttachment_inputSettings,
    inputAttachment_inputId,
    inputAttachment_inputAttachmentName,
    inputAttachment_automaticInputFailoverSettings,

    -- * InputChannelLevel
    InputChannelLevel (..),
    newInputChannelLevel,
    inputChannelLevel_inputChannel,
    inputChannelLevel_gain,

    -- * InputClippingSettings
    InputClippingSettings (..),
    newInputClippingSettings,
    inputClippingSettings_startTimecode,
    inputClippingSettings_stopTimecode,
    inputClippingSettings_inputTimecodeSource,

    -- * InputDestination
    InputDestination (..),
    newInputDestination,
    inputDestination_ip,
    inputDestination_port,
    inputDestination_url,
    inputDestination_vpc,

    -- * InputDestinationRequest
    InputDestinationRequest (..),
    newInputDestinationRequest,
    inputDestinationRequest_streamName,

    -- * InputDestinationVpc
    InputDestinationVpc (..),
    newInputDestinationVpc,
    inputDestinationVpc_availabilityZone,
    inputDestinationVpc_networkInterfaceId,

    -- * InputDeviceConfigurableSettings
    InputDeviceConfigurableSettings (..),
    newInputDeviceConfigurableSettings,
    inputDeviceConfigurableSettings_configuredInput,
    inputDeviceConfigurableSettings_maxBitrate,

    -- * InputDeviceHdSettings
    InputDeviceHdSettings (..),
    newInputDeviceHdSettings,
    inputDeviceHdSettings_height,
    inputDeviceHdSettings_scanType,
    inputDeviceHdSettings_width,
    inputDeviceHdSettings_configuredInput,
    inputDeviceHdSettings_framerate,
    inputDeviceHdSettings_deviceState,
    inputDeviceHdSettings_maxBitrate,
    inputDeviceHdSettings_activeInput,

    -- * InputDeviceNetworkSettings
    InputDeviceNetworkSettings (..),
    newInputDeviceNetworkSettings,
    inputDeviceNetworkSettings_dnsAddresses,
    inputDeviceNetworkSettings_ipAddress,
    inputDeviceNetworkSettings_subnetMask,
    inputDeviceNetworkSettings_ipScheme,
    inputDeviceNetworkSettings_gateway,

    -- * InputDeviceRequest
    InputDeviceRequest (..),
    newInputDeviceRequest,
    inputDeviceRequest_id,

    -- * InputDeviceSettings
    InputDeviceSettings (..),
    newInputDeviceSettings,
    inputDeviceSettings_id,

    -- * InputDeviceSummary
    InputDeviceSummary (..),
    newInputDeviceSummary,
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

    -- * InputDeviceUhdSettings
    InputDeviceUhdSettings (..),
    newInputDeviceUhdSettings,
    inputDeviceUhdSettings_height,
    inputDeviceUhdSettings_scanType,
    inputDeviceUhdSettings_width,
    inputDeviceUhdSettings_configuredInput,
    inputDeviceUhdSettings_framerate,
    inputDeviceUhdSettings_deviceState,
    inputDeviceUhdSettings_maxBitrate,
    inputDeviceUhdSettings_activeInput,

    -- * InputLocation
    InputLocation (..),
    newInputLocation,
    inputLocation_passwordParam,
    inputLocation_username,
    inputLocation_uri,

    -- * InputLossBehavior
    InputLossBehavior (..),
    newInputLossBehavior,
    inputLossBehavior_blackFrameMsec,
    inputLossBehavior_inputLossImageColor,
    inputLossBehavior_inputLossImageSlate,
    inputLossBehavior_repeatFrameMsec,
    inputLossBehavior_inputLossImageType,

    -- * InputLossFailoverSettings
    InputLossFailoverSettings (..),
    newInputLossFailoverSettings,
    inputLossFailoverSettings_inputLossThresholdMsec,

    -- * InputPrepareScheduleActionSettings
    InputPrepareScheduleActionSettings (..),
    newInputPrepareScheduleActionSettings,
    inputPrepareScheduleActionSettings_inputAttachmentNameReference,
    inputPrepareScheduleActionSettings_urlPath,
    inputPrepareScheduleActionSettings_inputClippingSettings,

    -- * InputSecurityGroup
    InputSecurityGroup (..),
    newInputSecurityGroup,
    inputSecurityGroup_arn,
    inputSecurityGroup_id,
    inputSecurityGroup_state,
    inputSecurityGroup_tags,
    inputSecurityGroup_whitelistRules,
    inputSecurityGroup_inputs,

    -- * InputSettings
    InputSettings (..),
    newInputSettings,
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

    -- * InputSource
    InputSource (..),
    newInputSource,
    inputSource_passwordParam,
    inputSource_username,
    inputSource_url,

    -- * InputSourceRequest
    InputSourceRequest (..),
    newInputSourceRequest,
    inputSourceRequest_passwordParam,
    inputSourceRequest_username,
    inputSourceRequest_url,

    -- * InputSpecification
    InputSpecification (..),
    newInputSpecification,
    inputSpecification_codec,
    inputSpecification_maximumBitrate,
    inputSpecification_resolution,

    -- * InputSwitchScheduleActionSettings
    InputSwitchScheduleActionSettings (..),
    newInputSwitchScheduleActionSettings,
    inputSwitchScheduleActionSettings_urlPath,
    inputSwitchScheduleActionSettings_inputClippingSettings,
    inputSwitchScheduleActionSettings_inputAttachmentNameReference,

    -- * InputVpcRequest
    InputVpcRequest (..),
    newInputVpcRequest,
    inputVpcRequest_securityGroupIds,
    inputVpcRequest_subnetIds,

    -- * InputWhitelistRule
    InputWhitelistRule (..),
    newInputWhitelistRule,
    inputWhitelistRule_cidr,

    -- * InputWhitelistRuleCidr
    InputWhitelistRuleCidr (..),
    newInputWhitelistRuleCidr,
    inputWhitelistRuleCidr_cidr,

    -- * KeyProviderSettings
    KeyProviderSettings (..),
    newKeyProviderSettings,
    keyProviderSettings_staticKeySettings,

    -- * M2tsSettings
    M2tsSettings (..),
    newM2tsSettings,
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

    -- * M3u8Settings
    M3u8Settings (..),
    newM3u8Settings,
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

    -- * MediaConnectFlow
    MediaConnectFlow (..),
    newMediaConnectFlow,
    mediaConnectFlow_flowArn,

    -- * MediaConnectFlowRequest
    MediaConnectFlowRequest (..),
    newMediaConnectFlowRequest,
    mediaConnectFlowRequest_flowArn,

    -- * MediaPackageGroupSettings
    MediaPackageGroupSettings (..),
    newMediaPackageGroupSettings,
    mediaPackageGroupSettings_destination,

    -- * MediaPackageOutputDestinationSettings
    MediaPackageOutputDestinationSettings (..),
    newMediaPackageOutputDestinationSettings,
    mediaPackageOutputDestinationSettings_channelId,

    -- * MediaPackageOutputSettings
    MediaPackageOutputSettings (..),
    newMediaPackageOutputSettings,

    -- * Mp2Settings
    Mp2Settings (..),
    newMp2Settings,
    mp2Settings_codingMode,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- * Mpeg2FilterSettings
    Mpeg2FilterSettings (..),
    newMpeg2FilterSettings,
    mpeg2FilterSettings_temporalFilterSettings,

    -- * Mpeg2Settings
    Mpeg2Settings (..),
    newMpeg2Settings,
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

    -- * MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    newMsSmoothGroupSettings,
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

    -- * MsSmoothOutputSettings
    MsSmoothOutputSettings (..),
    newMsSmoothOutputSettings,
    msSmoothOutputSettings_h265PackagingType,
    msSmoothOutputSettings_nameModifier,

    -- * Multiplex
    Multiplex (..),
    newMultiplex,
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

    -- * MultiplexGroupSettings
    MultiplexGroupSettings (..),
    newMultiplexGroupSettings,

    -- * MultiplexMediaConnectOutputDestinationSettings
    MultiplexMediaConnectOutputDestinationSettings (..),
    newMultiplexMediaConnectOutputDestinationSettings,
    multiplexMediaConnectOutputDestinationSettings_entitlementArn,

    -- * MultiplexOutputDestination
    MultiplexOutputDestination (..),
    newMultiplexOutputDestination,
    multiplexOutputDestination_mediaConnectSettings,

    -- * MultiplexOutputSettings
    MultiplexOutputSettings (..),
    newMultiplexOutputSettings,
    multiplexOutputSettings_destination,

    -- * MultiplexProgram
    MultiplexProgram (..),
    newMultiplexProgram,
    multiplexProgram_packetIdentifiersMap,
    multiplexProgram_multiplexProgramSettings,
    multiplexProgram_channelId,
    multiplexProgram_programName,
    multiplexProgram_pipelineDetails,

    -- * MultiplexProgramChannelDestinationSettings
    MultiplexProgramChannelDestinationSettings (..),
    newMultiplexProgramChannelDestinationSettings,
    multiplexProgramChannelDestinationSettings_multiplexId,
    multiplexProgramChannelDestinationSettings_programName,

    -- * MultiplexProgramPacketIdentifiersMap
    MultiplexProgramPacketIdentifiersMap (..),
    newMultiplexProgramPacketIdentifiersMap,
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

    -- * MultiplexProgramPipelineDetail
    MultiplexProgramPipelineDetail (..),
    newMultiplexProgramPipelineDetail,
    multiplexProgramPipelineDetail_pipelineId,
    multiplexProgramPipelineDetail_activeChannelPipeline,

    -- * MultiplexProgramServiceDescriptor
    MultiplexProgramServiceDescriptor (..),
    newMultiplexProgramServiceDescriptor,
    multiplexProgramServiceDescriptor_providerName,
    multiplexProgramServiceDescriptor_serviceName,

    -- * MultiplexProgramSettings
    MultiplexProgramSettings (..),
    newMultiplexProgramSettings,
    multiplexProgramSettings_preferredChannelPipeline,
    multiplexProgramSettings_serviceDescriptor,
    multiplexProgramSettings_videoSettings,
    multiplexProgramSettings_programNumber,

    -- * MultiplexProgramSummary
    MultiplexProgramSummary (..),
    newMultiplexProgramSummary,
    multiplexProgramSummary_channelId,
    multiplexProgramSummary_programName,

    -- * MultiplexSettings
    MultiplexSettings (..),
    newMultiplexSettings,
    multiplexSettings_transportStreamReservedBitrate,
    multiplexSettings_maximumVideoBufferDelayMilliseconds,
    multiplexSettings_transportStreamBitrate,
    multiplexSettings_transportStreamId,

    -- * MultiplexSettingsSummary
    MultiplexSettingsSummary (..),
    newMultiplexSettingsSummary,
    multiplexSettingsSummary_transportStreamBitrate,

    -- * MultiplexStatmuxVideoSettings
    MultiplexStatmuxVideoSettings (..),
    newMultiplexStatmuxVideoSettings,
    multiplexStatmuxVideoSettings_minimumBitrate,
    multiplexStatmuxVideoSettings_maximumBitrate,
    multiplexStatmuxVideoSettings_priority,

    -- * MultiplexSummary
    MultiplexSummary (..),
    newMultiplexSummary,
    multiplexSummary_availabilityZones,
    multiplexSummary_arn,
    multiplexSummary_id,
    multiplexSummary_pipelinesRunningCount,
    multiplexSummary_programCount,
    multiplexSummary_state,
    multiplexSummary_name,
    multiplexSummary_tags,
    multiplexSummary_multiplexSettings,

    -- * MultiplexVideoSettings
    MultiplexVideoSettings (..),
    newMultiplexVideoSettings,
    multiplexVideoSettings_constantBitrate,
    multiplexVideoSettings_statmuxSettings,

    -- * NetworkInputSettings
    NetworkInputSettings (..),
    newNetworkInputSettings,
    networkInputSettings_hlsInputSettings,
    networkInputSettings_serverValidation,

    -- * NielsenConfiguration
    NielsenConfiguration (..),
    newNielsenConfiguration,
    nielsenConfiguration_nielsenPcmToId3Tagging,
    nielsenConfiguration_distributorId,

    -- * Offering
    Offering (..),
    newOffering,
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

    -- * Output
    Output (..),
    newOutput,
    output_audioDescriptionNames,
    output_outputName,
    output_videoDescriptionName,
    output_captionDescriptionNames,
    output_outputSettings,

    -- * OutputDestination
    OutputDestination (..),
    newOutputDestination,
    outputDestination_mediaPackageSettings,
    outputDestination_id,
    outputDestination_multiplexSettings,
    outputDestination_settings,

    -- * OutputDestinationSettings
    OutputDestinationSettings (..),
    newOutputDestinationSettings,
    outputDestinationSettings_passwordParam,
    outputDestinationSettings_username,
    outputDestinationSettings_streamName,
    outputDestinationSettings_url,

    -- * OutputGroup
    OutputGroup (..),
    newOutputGroup,
    outputGroup_name,
    outputGroup_outputs,
    outputGroup_outputGroupSettings,

    -- * OutputGroupSettings
    OutputGroupSettings (..),
    newOutputGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_frameCaptureGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_mediaPackageGroupSettings,
    outputGroupSettings_rtmpGroupSettings,
    outputGroupSettings_udpGroupSettings,
    outputGroupSettings_archiveGroupSettings,
    outputGroupSettings_multiplexGroupSettings,

    -- * OutputLocationRef
    OutputLocationRef (..),
    newOutputLocationRef,
    outputLocationRef_destinationRefId,

    -- * OutputSettings
    OutputSettings (..),
    newOutputSettings,
    outputSettings_rtmpOutputSettings,
    outputSettings_msSmoothOutputSettings,
    outputSettings_udpOutputSettings,
    outputSettings_mediaPackageOutputSettings,
    outputSettings_frameCaptureOutputSettings,
    outputSettings_archiveOutputSettings,
    outputSettings_hlsOutputSettings,
    outputSettings_multiplexOutputSettings,

    -- * PassThroughSettings
    PassThroughSettings (..),
    newPassThroughSettings,

    -- * PauseStateScheduleActionSettings
    PauseStateScheduleActionSettings (..),
    newPauseStateScheduleActionSettings,
    pauseStateScheduleActionSettings_pipelines,

    -- * PipelineDetail
    PipelineDetail (..),
    newPipelineDetail,
    pipelineDetail_pipelineId,
    pipelineDetail_activeInputAttachmentName,
    pipelineDetail_activeInputSwitchActionName,

    -- * PipelinePauseStateSettings
    PipelinePauseStateSettings (..),
    newPipelinePauseStateSettings,
    pipelinePauseStateSettings_pipelineId,

    -- * RawSettings
    RawSettings (..),
    newRawSettings,

    -- * Rec601Settings
    Rec601Settings (..),
    newRec601Settings,

    -- * Rec709Settings
    Rec709Settings (..),
    newRec709Settings,

    -- * RemixSettings
    RemixSettings (..),
    newRemixSettings,
    remixSettings_channelsIn,
    remixSettings_channelsOut,
    remixSettings_channelMappings,

    -- * Reservation
    Reservation (..),
    newReservation,
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

    -- * ReservationResourceSpecification
    ReservationResourceSpecification (..),
    newReservationResourceSpecification,
    reservationResourceSpecification_maximumFramerate,
    reservationResourceSpecification_videoQuality,
    reservationResourceSpecification_codec,
    reservationResourceSpecification_maximumBitrate,
    reservationResourceSpecification_specialFeature,
    reservationResourceSpecification_channelClass,
    reservationResourceSpecification_resourceType,
    reservationResourceSpecification_resolution,

    -- * RtmpCaptionInfoDestinationSettings
    RtmpCaptionInfoDestinationSettings (..),
    newRtmpCaptionInfoDestinationSettings,

    -- * RtmpGroupSettings
    RtmpGroupSettings (..),
    newRtmpGroupSettings,
    rtmpGroupSettings_adMarkers,
    rtmpGroupSettings_captionData,
    rtmpGroupSettings_cacheFullBehavior,
    rtmpGroupSettings_cacheLength,
    rtmpGroupSettings_authenticationScheme,
    rtmpGroupSettings_inputLossAction,
    rtmpGroupSettings_restartDelay,

    -- * RtmpOutputSettings
    RtmpOutputSettings (..),
    newRtmpOutputSettings,
    rtmpOutputSettings_certificateMode,
    rtmpOutputSettings_numRetries,
    rtmpOutputSettings_connectionRetryInterval,
    rtmpOutputSettings_destination,

    -- * ScheduleAction
    ScheduleAction (..),
    newScheduleAction,
    scheduleAction_actionName,
    scheduleAction_scheduleActionStartSettings,
    scheduleAction_scheduleActionSettings,

    -- * ScheduleActionSettings
    ScheduleActionSettings (..),
    newScheduleActionSettings,
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

    -- * ScheduleActionStartSettings
    ScheduleActionStartSettings (..),
    newScheduleActionStartSettings,
    scheduleActionStartSettings_followModeScheduleActionStartSettings,
    scheduleActionStartSettings_immediateModeScheduleActionStartSettings,
    scheduleActionStartSettings_fixedModeScheduleActionStartSettings,

    -- * Scte20PlusEmbeddedDestinationSettings
    Scte20PlusEmbeddedDestinationSettings (..),
    newScte20PlusEmbeddedDestinationSettings,

    -- * Scte20SourceSettings
    Scte20SourceSettings (..),
    newScte20SourceSettings,
    scte20SourceSettings_convert608To708,
    scte20SourceSettings_source608ChannelNumber,

    -- * Scte27DestinationSettings
    Scte27DestinationSettings (..),
    newScte27DestinationSettings,

    -- * Scte27SourceSettings
    Scte27SourceSettings (..),
    newScte27SourceSettings,
    scte27SourceSettings_pid,

    -- * Scte35DeliveryRestrictions
    Scte35DeliveryRestrictions (..),
    newScte35DeliveryRestrictions,
    scte35DeliveryRestrictions_deviceRestrictions,
    scte35DeliveryRestrictions_archiveAllowedFlag,
    scte35DeliveryRestrictions_webDeliveryAllowedFlag,
    scte35DeliveryRestrictions_noRegionalBlackoutFlag,

    -- * Scte35Descriptor
    Scte35Descriptor (..),
    newScte35Descriptor,
    scte35Descriptor_scte35DescriptorSettings,

    -- * Scte35DescriptorSettings
    Scte35DescriptorSettings (..),
    newScte35DescriptorSettings,
    scte35DescriptorSettings_segmentationDescriptorScte35DescriptorSettings,

    -- * Scte35ReturnToNetworkScheduleActionSettings
    Scte35ReturnToNetworkScheduleActionSettings (..),
    newScte35ReturnToNetworkScheduleActionSettings,
    scte35ReturnToNetworkScheduleActionSettings_spliceEventId,

    -- * Scte35SegmentationDescriptor
    Scte35SegmentationDescriptor (..),
    newScte35SegmentationDescriptor,
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

    -- * Scte35SpliceInsert
    Scte35SpliceInsert (..),
    newScte35SpliceInsert,
    scte35SpliceInsert_adAvailOffset,
    scte35SpliceInsert_noRegionalBlackoutFlag,
    scte35SpliceInsert_webDeliveryAllowedFlag,

    -- * Scte35SpliceInsertScheduleActionSettings
    Scte35SpliceInsertScheduleActionSettings (..),
    newScte35SpliceInsertScheduleActionSettings,
    scte35SpliceInsertScheduleActionSettings_duration,
    scte35SpliceInsertScheduleActionSettings_spliceEventId,

    -- * Scte35TimeSignalApos
    Scte35TimeSignalApos (..),
    newScte35TimeSignalApos,
    scte35TimeSignalApos_adAvailOffset,
    scte35TimeSignalApos_noRegionalBlackoutFlag,
    scte35TimeSignalApos_webDeliveryAllowedFlag,

    -- * Scte35TimeSignalScheduleActionSettings
    Scte35TimeSignalScheduleActionSettings (..),
    newScte35TimeSignalScheduleActionSettings,
    scte35TimeSignalScheduleActionSettings_scte35Descriptors,

    -- * SmpteTtDestinationSettings
    SmpteTtDestinationSettings (..),
    newSmpteTtDestinationSettings,

    -- * StandardHlsSettings
    StandardHlsSettings (..),
    newStandardHlsSettings,
    standardHlsSettings_audioRenditionSets,
    standardHlsSettings_m3u8Settings,

    -- * StartTimecode
    StartTimecode (..),
    newStartTimecode,
    startTimecode_timecode,

    -- * StaticImageActivateScheduleActionSettings
    StaticImageActivateScheduleActionSettings (..),
    newStaticImageActivateScheduleActionSettings,
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

    -- * StaticImageDeactivateScheduleActionSettings
    StaticImageDeactivateScheduleActionSettings (..),
    newStaticImageDeactivateScheduleActionSettings,
    staticImageDeactivateScheduleActionSettings_layer,
    staticImageDeactivateScheduleActionSettings_fadeOut,

    -- * StaticKeySettings
    StaticKeySettings (..),
    newStaticKeySettings,
    staticKeySettings_keyProviderServer,
    staticKeySettings_staticKeyValue,

    -- * StopTimecode
    StopTimecode (..),
    newStopTimecode,
    stopTimecode_timecode,
    stopTimecode_lastFrameClippingBehavior,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings (..),
    newTeletextDestinationSettings,

    -- * TeletextSourceSettings
    TeletextSourceSettings (..),
    newTeletextSourceSettings,
    teletextSourceSettings_pageNumber,

    -- * TemporalFilterSettings
    TemporalFilterSettings (..),
    newTemporalFilterSettings,
    temporalFilterSettings_postFilterSharpening,
    temporalFilterSettings_strength,

    -- * TimecodeConfig
    TimecodeConfig (..),
    newTimecodeConfig,
    timecodeConfig_syncThreshold,
    timecodeConfig_source,

    -- * TransferringInputDeviceSummary
    TransferringInputDeviceSummary (..),
    newTransferringInputDeviceSummary,
    transferringInputDeviceSummary_transferType,
    transferringInputDeviceSummary_message,
    transferringInputDeviceSummary_id,
    transferringInputDeviceSummary_targetCustomerId,

    -- * TtmlDestinationSettings
    TtmlDestinationSettings (..),
    newTtmlDestinationSettings,
    ttmlDestinationSettings_styleControl,

    -- * UdpContainerSettings
    UdpContainerSettings (..),
    newUdpContainerSettings,
    udpContainerSettings_m2tsSettings,

    -- * UdpGroupSettings
    UdpGroupSettings (..),
    newUdpGroupSettings,
    udpGroupSettings_timedMetadataId3Period,
    udpGroupSettings_timedMetadataId3Frame,
    udpGroupSettings_inputLossAction,

    -- * UdpOutputSettings
    UdpOutputSettings (..),
    newUdpOutputSettings,
    udpOutputSettings_bufferMsec,
    udpOutputSettings_fecOutputSettings,
    udpOutputSettings_destination,
    udpOutputSettings_containerSettings,

    -- * VideoBlackFailoverSettings
    VideoBlackFailoverSettings (..),
    newVideoBlackFailoverSettings,
    videoBlackFailoverSettings_blackDetectThreshold,
    videoBlackFailoverSettings_videoBlackThresholdMsec,

    -- * VideoCodecSettings
    VideoCodecSettings (..),
    newVideoCodecSettings,
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_h265Settings,

    -- * VideoDescription
    VideoDescription (..),
    newVideoDescription,
    videoDescription_height,
    videoDescription_respondToAfd,
    videoDescription_width,
    videoDescription_codecSettings,
    videoDescription_scalingBehavior,
    videoDescription_sharpness,
    videoDescription_name,

    -- * VideoSelector
    VideoSelector (..),
    newVideoSelector,
    videoSelector_colorSpaceUsage,
    videoSelector_selectorSettings,
    videoSelector_colorSpace,

    -- * VideoSelectorPid
    VideoSelectorPid (..),
    newVideoSelectorPid,
    videoSelectorPid_pid,

    -- * VideoSelectorProgramId
    VideoSelectorProgramId (..),
    newVideoSelectorProgramId,
    videoSelectorProgramId_programId,

    -- * VideoSelectorSettings
    VideoSelectorSettings (..),
    newVideoSelectorSettings,
    videoSelectorSettings_videoSelectorPid,
    videoSelectorSettings_videoSelectorProgramId,

    -- * VpcOutputSettings
    VpcOutputSettings (..),
    newVpcOutputSettings,
    vpcOutputSettings_securityGroupIds,
    vpcOutputSettings_publicAddressAllocationIds,
    vpcOutputSettings_subnetIds,

    -- * WavSettings
    WavSettings (..),
    newWavSettings,
    wavSettings_codingMode,
    wavSettings_bitDepth,
    wavSettings_sampleRate,

    -- * WebvttDestinationSettings
    WebvttDestinationSettings (..),
    newWebvttDestinationSettings,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacCodingMode
import Network.AWS.MediaLive.Types.AacInputType
import Network.AWS.MediaLive.Types.AacProfile
import Network.AWS.MediaLive.Types.AacRateControlMode
import Network.AWS.MediaLive.Types.AacRawFormat
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.AacSpec
import Network.AWS.MediaLive.Types.AacVbrQuality
import Network.AWS.MediaLive.Types.Ac3BitstreamMode
import Network.AWS.MediaLive.Types.Ac3CodingMode
import Network.AWS.MediaLive.Types.Ac3DrcProfile
import Network.AWS.MediaLive.Types.Ac3LfeFilter
import Network.AWS.MediaLive.Types.Ac3MetadataControl
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.AcceptHeader
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.AudioChannelMapping
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescription
import Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
import Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Network.AWS.MediaLive.Types.AudioLanguageSelection
import Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
import Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
import Network.AWS.MediaLive.Types.AudioPidSelection
import Network.AWS.MediaLive.Types.AudioSelector
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings
import Network.AWS.MediaLive.Types.AudioTrack
import Network.AWS.MediaLive.Types.AudioTrackSelection
import Network.AWS.MediaLive.Types.AudioType
import Network.AWS.MediaLive.Types.AuthenticationScheme
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.AvailBlanking
import Network.AWS.MediaLive.Types.AvailBlankingState
import Network.AWS.MediaLive.Types.AvailConfiguration
import Network.AWS.MediaLive.Types.AvailSettings
import Network.AWS.MediaLive.Types.BatchFailedResultModel
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
import Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
import Network.AWS.MediaLive.Types.BlackoutSlate
import Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
import Network.AWS.MediaLive.Types.BlackoutSlateState
import Network.AWS.MediaLive.Types.BurnInAlignment
import Network.AWS.MediaLive.Types.BurnInBackgroundColor
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.BurnInFontColor
import Network.AWS.MediaLive.Types.BurnInOutlineColor
import Network.AWS.MediaLive.Types.BurnInShadowColor
import Network.AWS.MediaLive.Types.BurnInTeletextGridControl
import Network.AWS.MediaLive.Types.CaptionDescription
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
import Network.AWS.MediaLive.Types.CaptionLanguageMapping
import Network.AWS.MediaLive.Types.CaptionSelector
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
import Network.AWS.MediaLive.Types.CdiInputResolution
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.Channel
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.ChannelSummary
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.ContentType
import Network.AWS.MediaLive.Types.DeviceSettingsSyncState
import Network.AWS.MediaLive.Types.DeviceUpdateStatus
import Network.AWS.MediaLive.Types.DvbNitSettings
import Network.AWS.MediaLive.Types.DvbSdtOutputSdt
import Network.AWS.MediaLive.Types.DvbSdtSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
import Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
import Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
import Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
import Network.AWS.MediaLive.Types.DvbSubDestinationTeletextGridControl
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.DvbTdtSettings
import Network.AWS.MediaLive.Types.Eac3AttenuationControl
import Network.AWS.MediaLive.Types.Eac3BitstreamMode
import Network.AWS.MediaLive.Types.Eac3CodingMode
import Network.AWS.MediaLive.Types.Eac3DcFilter
import Network.AWS.MediaLive.Types.Eac3DrcLine
import Network.AWS.MediaLive.Types.Eac3DrcRf
import Network.AWS.MediaLive.Types.Eac3LfeControl
import Network.AWS.MediaLive.Types.Eac3LfeFilter
import Network.AWS.MediaLive.Types.Eac3MetadataControl
import Network.AWS.MediaLive.Types.Eac3PassthroughControl
import Network.AWS.MediaLive.Types.Eac3PhaseControl
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.Eac3StereoDownmix
import Network.AWS.MediaLive.Types.Eac3SurroundExMode
import Network.AWS.MediaLive.Types.Eac3SurroundMode
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl
import Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
import Network.AWS.MediaLive.Types.EmbeddedConvert608To708
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedScte20Detection
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.EncoderSettings
import Network.AWS.MediaLive.Types.FailoverCondition
import Network.AWS.MediaLive.Types.FailoverConditionSettings
import Network.AWS.MediaLive.Types.FeatureActivations
import Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import Network.AWS.MediaLive.Types.FecOutputIncludeFec
import Network.AWS.MediaLive.Types.FecOutputSettings
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Fmp4HlsSettings
import Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
import Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.FollowPoint
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureHlsSettings
import Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureSettings
import Network.AWS.MediaLive.Types.GlobalConfiguration
import Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
import Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Network.AWS.MediaLive.Types.H264AdaptiveQuantization
import Network.AWS.MediaLive.Types.H264ColorMetadata
import Network.AWS.MediaLive.Types.H264ColorSpaceSettings
import Network.AWS.MediaLive.Types.H264EntropyEncoding
import Network.AWS.MediaLive.Types.H264FilterSettings
import Network.AWS.MediaLive.Types.H264FlickerAq
import Network.AWS.MediaLive.Types.H264ForceFieldPictures
import Network.AWS.MediaLive.Types.H264FramerateControl
import Network.AWS.MediaLive.Types.H264GopBReference
import Network.AWS.MediaLive.Types.H264GopSizeUnits
import Network.AWS.MediaLive.Types.H264Level
import Network.AWS.MediaLive.Types.H264LookAheadRateControl
import Network.AWS.MediaLive.Types.H264ParControl
import Network.AWS.MediaLive.Types.H264Profile
import Network.AWS.MediaLive.Types.H264QualityLevel
import Network.AWS.MediaLive.Types.H264RateControlMode
import Network.AWS.MediaLive.Types.H264ScanType
import Network.AWS.MediaLive.Types.H264SceneChangeDetect
import Network.AWS.MediaLive.Types.H264Settings
import Network.AWS.MediaLive.Types.H264SpatialAq
import Network.AWS.MediaLive.Types.H264SubGopLength
import Network.AWS.MediaLive.Types.H264Syntax
import Network.AWS.MediaLive.Types.H264TemporalAq
import Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
import Network.AWS.MediaLive.Types.H265AdaptiveQuantization
import Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
import Network.AWS.MediaLive.Types.H265ColorMetadata
import Network.AWS.MediaLive.Types.H265ColorSpaceSettings
import Network.AWS.MediaLive.Types.H265FilterSettings
import Network.AWS.MediaLive.Types.H265FlickerAq
import Network.AWS.MediaLive.Types.H265GopSizeUnits
import Network.AWS.MediaLive.Types.H265Level
import Network.AWS.MediaLive.Types.H265LookAheadRateControl
import Network.AWS.MediaLive.Types.H265Profile
import Network.AWS.MediaLive.Types.H265RateControlMode
import Network.AWS.MediaLive.Types.H265ScanType
import Network.AWS.MediaLive.Types.H265SceneChangeDetect
import Network.AWS.MediaLive.Types.H265Settings
import Network.AWS.MediaLive.Types.H265Tier
import Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
import Network.AWS.MediaLive.Types.Hdr10Settings
import Network.AWS.MediaLive.Types.HlsAdMarkers
import Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
import Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
import Network.AWS.MediaLive.Types.HlsCdnSettings
import Network.AWS.MediaLive.Types.HlsClientCache
import Network.AWS.MediaLive.Types.HlsCodecSpecification
import Network.AWS.MediaLive.Types.HlsDirectoryStructure
import Network.AWS.MediaLive.Types.HlsDiscontinuityTags
import Network.AWS.MediaLive.Types.HlsEncryptionType
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.HlsH265PackagingType
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
import Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
import Network.AWS.MediaLive.Types.HlsInputSettings
import Network.AWS.MediaLive.Types.HlsIvInManifest
import Network.AWS.MediaLive.Types.HlsIvSource
import Network.AWS.MediaLive.Types.HlsManifestCompression
import Network.AWS.MediaLive.Types.HlsManifestDurationFormat
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
import Network.AWS.MediaLive.Types.HlsMode
import Network.AWS.MediaLive.Types.HlsOutputSelection
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.HlsProgramDateTime
import Network.AWS.MediaLive.Types.HlsRedundantManifest
import Network.AWS.MediaLive.Types.HlsSegmentationMode
import Network.AWS.MediaLive.Types.HlsSettings
import Network.AWS.MediaLive.Types.HlsStreamInfResolution
import Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsTsFileMode
import Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Input
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputChannelLevel
import Network.AWS.MediaLive.Types.InputClass
import Network.AWS.MediaLive.Types.InputClippingSettings
import Network.AWS.MediaLive.Types.InputCodec
import Network.AWS.MediaLive.Types.InputDeblockFilter
import Network.AWS.MediaLive.Types.InputDenoiseFilter
import Network.AWS.MediaLive.Types.InputDestination
import Network.AWS.MediaLive.Types.InputDestinationRequest
import Network.AWS.MediaLive.Types.InputDestinationVpc
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceIpScheme
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceRequest
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputDeviceState
import Network.AWS.MediaLive.Types.InputDeviceSummary
import Network.AWS.MediaLive.Types.InputDeviceTransferType
import Network.AWS.MediaLive.Types.InputDeviceType
import Network.AWS.MediaLive.Types.InputDeviceUhdSettings
import Network.AWS.MediaLive.Types.InputFilter
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.MediaLive.Types.InputLossActionForHlsOut
import Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
import Network.AWS.MediaLive.Types.InputLossActionForRtmpOut
import Network.AWS.MediaLive.Types.InputLossActionForUdpOut
import Network.AWS.MediaLive.Types.InputLossBehavior
import Network.AWS.MediaLive.Types.InputLossFailoverSettings
import Network.AWS.MediaLive.Types.InputLossImageType
import Network.AWS.MediaLive.Types.InputMaximumBitrate
import Network.AWS.MediaLive.Types.InputPreference
import Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
import Network.AWS.MediaLive.Types.InputResolution
import Network.AWS.MediaLive.Types.InputSecurityGroup
import Network.AWS.MediaLive.Types.InputSecurityGroupState
import Network.AWS.MediaLive.Types.InputSettings
import Network.AWS.MediaLive.Types.InputSource
import Network.AWS.MediaLive.Types.InputSourceEndBehavior
import Network.AWS.MediaLive.Types.InputSourceRequest
import Network.AWS.MediaLive.Types.InputSourceType
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.InputState
import Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
import Network.AWS.MediaLive.Types.InputTimecodeSource
import Network.AWS.MediaLive.Types.InputType
import Network.AWS.MediaLive.Types.InputVpcRequest
import Network.AWS.MediaLive.Types.InputWhitelistRule
import Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
import Network.AWS.MediaLive.Types.KeyProviderSettings
import Network.AWS.MediaLive.Types.LastFrameClippingBehavior
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
import Network.AWS.MediaLive.Types.M2tsArib
import Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
import Network.AWS.MediaLive.Types.M2tsAudioBufferModel
import Network.AWS.MediaLive.Types.M2tsAudioInterval
import Network.AWS.MediaLive.Types.M2tsAudioStreamType
import Network.AWS.MediaLive.Types.M2tsBufferModel
import Network.AWS.MediaLive.Types.M2tsCcDescriptor
import Network.AWS.MediaLive.Types.M2tsEbifControl
import Network.AWS.MediaLive.Types.M2tsEbpPlacement
import Network.AWS.MediaLive.Types.M2tsEsRateInPes
import Network.AWS.MediaLive.Types.M2tsKlv
import Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
import Network.AWS.MediaLive.Types.M2tsPcrControl
import Network.AWS.MediaLive.Types.M2tsRateMode
import Network.AWS.MediaLive.Types.M2tsScte35Control
import Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
import Network.AWS.MediaLive.Types.M2tsSegmentationStyle
import Network.AWS.MediaLive.Types.M2tsSettings
import Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
import Network.AWS.MediaLive.Types.M3u8PcrControl
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
import Network.AWS.MediaLive.Types.M3u8Settings
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
import Network.AWS.MediaLive.Types.MediaConnectFlow
import Network.AWS.MediaLive.Types.MediaConnectFlowRequest
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.Mp2CodingMode
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization
import Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
import Network.AWS.MediaLive.Types.Mpeg2ColorSpace
import Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
import Network.AWS.MediaLive.Types.Mpeg2FilterSettings
import Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits
import Network.AWS.MediaLive.Types.Mpeg2ScanType
import Network.AWS.MediaLive.Types.Mpeg2Settings
import Network.AWS.MediaLive.Types.Mpeg2SubGopLength
import Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
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
import Network.AWS.MediaLive.Types.MultiplexState
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
import Network.AWS.MediaLive.Types.MultiplexSummary
import Network.AWS.MediaLive.Types.MultiplexVideoSettings
import Network.AWS.MediaLive.Types.NetworkInputServerValidation
import Network.AWS.MediaLive.Types.NetworkInputSettings
import Network.AWS.MediaLive.Types.NielsenConfiguration
import Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
import Network.AWS.MediaLive.Types.Offering
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
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
import Network.AWS.MediaLive.Types.PipelineId
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
import Network.AWS.MediaLive.Types.PreferredChannelPipeline
import Network.AWS.MediaLive.Types.RawSettings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import Network.AWS.MediaLive.Types.RemixSettings
import Network.AWS.MediaLive.Types.Reservation
import Network.AWS.MediaLive.Types.ReservationCodec
import Network.AWS.MediaLive.Types.ReservationMaximumBitrate
import Network.AWS.MediaLive.Types.ReservationMaximumFramerate
import Network.AWS.MediaLive.Types.ReservationResolution
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.MediaLive.Types.ReservationResourceType
import Network.AWS.MediaLive.Types.ReservationSpecialFeature
import Network.AWS.MediaLive.Types.ReservationState
import Network.AWS.MediaLive.Types.ReservationVideoQuality
import Network.AWS.MediaLive.Types.RtmpAdMarkers
import Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
import Network.AWS.MediaLive.Types.RtmpCaptionData
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.ScheduleAction
import Network.AWS.MediaLive.Types.ScheduleActionSettings
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
import Network.AWS.MediaLive.Types.Scte20Convert608To708
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
import Network.AWS.MediaLive.Types.Scte35Descriptor
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
import Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
import Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
import Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
import Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
import Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
import Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
import Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior
import Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
import Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
import Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
import Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
import Network.AWS.MediaLive.Types.Smpte2038DataPreference
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.StandardHlsSettings
import Network.AWS.MediaLive.Types.StartTimecode
import Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticKeySettings
import Network.AWS.MediaLive.Types.StopTimecode
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import Network.AWS.MediaLive.Types.TemporalFilterStrength
import Network.AWS.MediaLive.Types.TimecodeConfig
import Network.AWS.MediaLive.Types.TimecodeConfigSource
import Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
import Network.AWS.MediaLive.Types.UdpContainerSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings
import Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
import Network.AWS.MediaLive.Types.VideoBlackFailoverSettings
import Network.AWS.MediaLive.Types.VideoCodecSettings
import Network.AWS.MediaLive.Types.VideoDescription
import Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
import Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
import Network.AWS.MediaLive.Types.VideoSelector
import Network.AWS.MediaLive.Types.VideoSelectorColorSpace
import Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
import Network.AWS.MediaLive.Types.VideoSelectorSettings
import Network.AWS.MediaLive.Types.VpcOutputSettings
import Network.AWS.MediaLive.Types.WavCodingMode
import Network.AWS.MediaLive.Types.WavSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-14@ of the Amazon Elemental MediaLive SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MediaLive",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "medialive",
      Core._serviceSigningName = "medialive",
      Core._serviceVersion = "2017-10-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MediaLive",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Placeholder documentation for NotFoundException
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | Placeholder documentation for BadRequestException
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | Placeholder documentation for BadGatewayException
_BadGatewayException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError
    defaultService
    "BadGatewayException"
    Core.. Core.hasStatus 502

-- | Placeholder documentation for InternalServerErrorException
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Core.. Core.hasStatus 500

-- | Placeholder documentation for GatewayTimeoutException
_GatewayTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GatewayTimeoutException =
  Core._MatchServiceError
    defaultService
    "GatewayTimeoutException"
    Core.. Core.hasStatus 504

-- | Placeholder documentation for ForbiddenException
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Core.. Core.hasStatus 403

-- | Placeholder documentation for ConflictException
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Core.. Core.hasStatus 409

-- | Placeholder documentation for UnprocessableEntityException
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Core.. Core.hasStatus 422

-- | Placeholder documentation for TooManyRequestsException
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429
