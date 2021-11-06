{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _GatewayTimeoutException,
    _UnprocessableEntityException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _BadGatewayException,
    _BadRequestException,

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

    -- * DvbSubOcrLanguage
    DvbSubOcrLanguage (..),

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

    -- * HlsScte35SourceType
    HlsScte35SourceType (..),

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

    -- * MotionGraphicsInsertion
    MotionGraphicsInsertion (..),

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

    -- * NielsenWatermarksCbetStepaside
    NielsenWatermarksCbetStepaside (..),

    -- * NielsenWatermarksDistributionTypes
    NielsenWatermarksDistributionTypes (..),

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

    -- * S3CannedAcl
    S3CannedAcl (..),

    -- * Scte20Convert608To708
    Scte20Convert608To708 (..),

    -- * Scte27OcrLanguage
    Scte27OcrLanguage (..),

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

    -- * WebvttDestinationStyleControl
    WebvttDestinationStyleControl (..),

    -- * AacSettings
    AacSettings (..),
    newAacSettings,
    aacSettings_rawFormat,
    aacSettings_codingMode,
    aacSettings_profile,
    aacSettings_rateControlMode,
    aacSettings_sampleRate,
    aacSettings_spec,
    aacSettings_bitrate,
    aacSettings_vbrQuality,
    aacSettings_inputType,

    -- * Ac3Settings
    Ac3Settings (..),
    newAc3Settings,
    ac3Settings_lfeFilter,
    ac3Settings_metadataControl,
    ac3Settings_bitstreamMode,
    ac3Settings_codingMode,
    ac3Settings_bitrate,
    ac3Settings_dialnorm,
    ac3Settings_drcProfile,

    -- * AncillarySourceSettings
    AncillarySourceSettings (..),
    newAncillarySourceSettings,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- * ArchiveCdnSettings
    ArchiveCdnSettings (..),
    newArchiveCdnSettings,
    archiveCdnSettings_archiveS3Settings,

    -- * ArchiveContainerSettings
    ArchiveContainerSettings (..),
    newArchiveContainerSettings,
    archiveContainerSettings_m2tsSettings,
    archiveContainerSettings_rawSettings,

    -- * ArchiveGroupSettings
    ArchiveGroupSettings (..),
    newArchiveGroupSettings,
    archiveGroupSettings_rolloverInterval,
    archiveGroupSettings_archiveCdnSettings,
    archiveGroupSettings_destination,

    -- * ArchiveOutputSettings
    ArchiveOutputSettings (..),
    newArchiveOutputSettings,
    archiveOutputSettings_extension,
    archiveOutputSettings_nameModifier,
    archiveOutputSettings_containerSettings,

    -- * ArchiveS3Settings
    ArchiveS3Settings (..),
    newArchiveS3Settings,
    archiveS3Settings_cannedAcl,

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
    audioCodecSettings_passThroughSettings,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_wavSettings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_eac3Settings,

    -- * AudioDescription
    AudioDescription (..),
    newAudioDescription,
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

    -- * AudioHlsRenditionSelection
    AudioHlsRenditionSelection (..),
    newAudioHlsRenditionSelection,
    audioHlsRenditionSelection_name,
    audioHlsRenditionSelection_groupId,

    -- * AudioLanguageSelection
    AudioLanguageSelection (..),
    newAudioLanguageSelection,
    audioLanguageSelection_languageSelectionPolicy,
    audioLanguageSelection_languageCode,

    -- * AudioNormalizationSettings
    AudioNormalizationSettings (..),
    newAudioNormalizationSettings,
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithm,

    -- * AudioOnlyHlsSettings
    AudioOnlyHlsSettings (..),
    newAudioOnlyHlsSettings,
    audioOnlyHlsSettings_audioOnlyImage,
    audioOnlyHlsSettings_segmentType,
    audioOnlyHlsSettings_audioGroupId,
    audioOnlyHlsSettings_audioTrackType,

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
    audioSelectorSettings_audioTrackSelection,
    audioSelectorSettings_audioHlsRenditionSelection,
    audioSelectorSettings_audioPidSelection,

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

    -- * AudioWatermarkSettings
    AudioWatermarkSettings (..),
    newAudioWatermarkSettings,
    audioWatermarkSettings_nielsenWatermarksSettings,

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
    availSettings_scte35SpliceInsert,
    availSettings_scte35TimeSignalApos,

    -- * BatchFailedResultModel
    BatchFailedResultModel (..),
    newBatchFailedResultModel,
    batchFailedResultModel_arn,
    batchFailedResultModel_id,
    batchFailedResultModel_code,
    batchFailedResultModel_message,

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
    batchSuccessfulResultModel_state,
    batchSuccessfulResultModel_arn,
    batchSuccessfulResultModel_id,

    -- * BlackoutSlate
    BlackoutSlate (..),
    newBlackoutSlate,
    blackoutSlate_networkEndBlackoutImage,
    blackoutSlate_state,
    blackoutSlate_networkEndBlackout,
    blackoutSlate_networkId,
    blackoutSlate_blackoutSlateImage,

    -- * BurnInDestinationSettings
    BurnInDestinationSettings (..),
    newBurnInDestinationSettings,
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

    -- * CaptionDescription
    CaptionDescription (..),
    newCaptionDescription,
    captionDescription_languageCode,
    captionDescription_destinationSettings,
    captionDescription_languageDescription,
    captionDescription_captionSelectorName,
    captionDescription_name,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings (..),
    newCaptionDestinationSettings,
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

    -- * CaptionLanguageMapping
    CaptionLanguageMapping (..),
    newCaptionLanguageMapping,
    captionLanguageMapping_languageCode,
    captionLanguageMapping_languageDescription,
    captionLanguageMapping_captionChannel,

    -- * CaptionRectangle
    CaptionRectangle (..),
    newCaptionRectangle,
    captionRectangle_topOffset,
    captionRectangle_height,
    captionRectangle_width,
    captionRectangle_leftOffset,

    -- * CaptionSelector
    CaptionSelector (..),
    newCaptionSelector,
    captionSelector_languageCode,
    captionSelector_selectorSettings,
    captionSelector_name,

    -- * CaptionSelectorSettings
    CaptionSelectorSettings (..),
    newCaptionSelectorSettings,
    captionSelectorSettings_teletextSourceSettings,
    captionSelectorSettings_aribSourceSettings,
    captionSelectorSettings_scte27SourceSettings,
    captionSelectorSettings_dvbSubSourceSettings,
    captionSelectorSettings_ancillarySourceSettings,
    captionSelectorSettings_scte20SourceSettings,
    captionSelectorSettings_embeddedSourceSettings,

    -- * CdiInputSpecification
    CdiInputSpecification (..),
    newCdiInputSpecification,
    cdiInputSpecification_resolution,

    -- * Channel
    Channel (..),
    newChannel,
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

    -- * ChannelEgressEndpoint
    ChannelEgressEndpoint (..),
    newChannelEgressEndpoint,
    channelEgressEndpoint_sourceIp,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
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
    dvbSdtSettings_repInterval,
    dvbSdtSettings_serviceProviderName,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings (..),
    newDvbSubDestinationSettings,
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

    -- * DvbSubSourceSettings
    DvbSubSourceSettings (..),
    newDvbSubSourceSettings,
    dvbSubSourceSettings_ocrLanguage,
    dvbSubSourceSettings_pid,

    -- * DvbTdtSettings
    DvbTdtSettings (..),
    newDvbTdtSettings,
    dvbTdtSettings_repInterval,

    -- * Eac3Settings
    Eac3Settings (..),
    newEac3Settings,
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

    -- * EbuTtDDestinationSettings
    EbuTtDDestinationSettings (..),
    newEbuTtDDestinationSettings,
    ebuTtDDestinationSettings_fillLineGap,
    ebuTtDDestinationSettings_copyrightHolder,
    ebuTtDDestinationSettings_fontFamily,
    ebuTtDDestinationSettings_styleControl,

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    newEmbeddedDestinationSettings,

    -- * EmbeddedPlusScte20DestinationSettings
    EmbeddedPlusScte20DestinationSettings (..),
    newEmbeddedPlusScte20DestinationSettings,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    newEmbeddedSourceSettings,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_scte20Detection,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- * EncoderSettings
    EncoderSettings (..),
    newEncoderSettings,
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
    fecOutputSettings_includeFec,
    fecOutputSettings_columnDepth,

    -- * FixedModeScheduleActionStartSettings
    FixedModeScheduleActionStartSettings (..),
    newFixedModeScheduleActionStartSettings,
    fixedModeScheduleActionStartSettings_time,

    -- * Fmp4HlsSettings
    Fmp4HlsSettings (..),
    newFmp4HlsSettings,
    fmp4HlsSettings_nielsenId3Behavior,
    fmp4HlsSettings_audioRenditionSets,
    fmp4HlsSettings_timedMetadataBehavior,

    -- * FollowModeScheduleActionStartSettings
    FollowModeScheduleActionStartSettings (..),
    newFollowModeScheduleActionStartSettings,
    followModeScheduleActionStartSettings_referenceActionName,
    followModeScheduleActionStartSettings_followPoint,

    -- * FrameCaptureCdnSettings
    FrameCaptureCdnSettings (..),
    newFrameCaptureCdnSettings,
    frameCaptureCdnSettings_frameCaptureS3Settings,

    -- * FrameCaptureGroupSettings
    FrameCaptureGroupSettings (..),
    newFrameCaptureGroupSettings,
    frameCaptureGroupSettings_frameCaptureCdnSettings,
    frameCaptureGroupSettings_destination,

    -- * FrameCaptureHlsSettings
    FrameCaptureHlsSettings (..),
    newFrameCaptureHlsSettings,

    -- * FrameCaptureOutputSettings
    FrameCaptureOutputSettings (..),
    newFrameCaptureOutputSettings,
    frameCaptureOutputSettings_nameModifier,

    -- * FrameCaptureS3Settings
    FrameCaptureS3Settings (..),
    newFrameCaptureS3Settings,
    frameCaptureS3Settings_cannedAcl,

    -- * FrameCaptureSettings
    FrameCaptureSettings (..),
    newFrameCaptureSettings,
    frameCaptureSettings_captureIntervalUnits,
    frameCaptureSettings_captureInterval,

    -- * GlobalConfiguration
    GlobalConfiguration (..),
    newGlobalConfiguration,
    globalConfiguration_outputLockingMode,
    globalConfiguration_inputLossBehavior,
    globalConfiguration_initialAudioGain,
    globalConfiguration_supportLowFramerateInputs,
    globalConfiguration_inputEndAction,
    globalConfiguration_outputTimingSource,

    -- * H264ColorSpaceSettings
    H264ColorSpaceSettings (..),
    newH264ColorSpaceSettings,
    h264ColorSpaceSettings_rec709Settings,
    h264ColorSpaceSettings_rec601Settings,
    h264ColorSpaceSettings_colorSpacePassthroughSettings,

    -- * H264FilterSettings
    H264FilterSettings (..),
    newH264FilterSettings,
    h264FilterSettings_temporalFilterSettings,

    -- * H264Settings
    H264Settings (..),
    newH264Settings,
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

    -- * H265ColorSpaceSettings
    H265ColorSpaceSettings (..),
    newH265ColorSpaceSettings,
    h265ColorSpaceSettings_hdr10Settings,
    h265ColorSpaceSettings_rec709Settings,
    h265ColorSpaceSettings_rec601Settings,
    h265ColorSpaceSettings_colorSpacePassthroughSettings,

    -- * H265FilterSettings
    H265FilterSettings (..),
    newH265FilterSettings,
    h265FilterSettings_temporalFilterSettings,

    -- * H265Settings
    H265Settings (..),
    newH265Settings,
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

    -- * Hdr10Settings
    Hdr10Settings (..),
    newHdr10Settings,
    hdr10Settings_maxFall,
    hdr10Settings_maxCll,

    -- * HlsAkamaiSettings
    HlsAkamaiSettings (..),
    newHlsAkamaiSettings,
    hlsAkamaiSettings_httpTransferMode,
    hlsAkamaiSettings_numRetries,
    hlsAkamaiSettings_token,
    hlsAkamaiSettings_connectionRetryInterval,
    hlsAkamaiSettings_filecacheDuration,
    hlsAkamaiSettings_restartDelay,
    hlsAkamaiSettings_salt,

    -- * HlsBasicPutSettings
    HlsBasicPutSettings (..),
    newHlsBasicPutSettings,
    hlsBasicPutSettings_numRetries,
    hlsBasicPutSettings_connectionRetryInterval,
    hlsBasicPutSettings_filecacheDuration,
    hlsBasicPutSettings_restartDelay,

    -- * HlsCdnSettings
    HlsCdnSettings (..),
    newHlsCdnSettings,
    hlsCdnSettings_hlsAkamaiSettings,
    hlsCdnSettings_hlsMediaStoreSettings,
    hlsCdnSettings_hlsS3Settings,
    hlsCdnSettings_hlsBasicPutSettings,
    hlsCdnSettings_hlsWebdavSettings,

    -- * HlsGroupSettings
    HlsGroupSettings (..),
    newHlsGroupSettings,
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

    -- * HlsId3SegmentTaggingScheduleActionSettings
    HlsId3SegmentTaggingScheduleActionSettings (..),
    newHlsId3SegmentTaggingScheduleActionSettings,
    hlsId3SegmentTaggingScheduleActionSettings_tag,

    -- * HlsInputSettings
    HlsInputSettings (..),
    newHlsInputSettings,
    hlsInputSettings_bufferSegments,
    hlsInputSettings_retries,
    hlsInputSettings_retryInterval,
    hlsInputSettings_bandwidth,
    hlsInputSettings_scte35Source,

    -- * HlsMediaStoreSettings
    HlsMediaStoreSettings (..),
    newHlsMediaStoreSettings,
    hlsMediaStoreSettings_numRetries,
    hlsMediaStoreSettings_connectionRetryInterval,
    hlsMediaStoreSettings_filecacheDuration,
    hlsMediaStoreSettings_mediaStoreStorageClass,
    hlsMediaStoreSettings_restartDelay,

    -- * HlsOutputSettings
    HlsOutputSettings (..),
    newHlsOutputSettings,
    hlsOutputSettings_h265PackagingType,
    hlsOutputSettings_segmentModifier,
    hlsOutputSettings_nameModifier,
    hlsOutputSettings_hlsSettings,

    -- * HlsS3Settings
    HlsS3Settings (..),
    newHlsS3Settings,
    hlsS3Settings_cannedAcl,

    -- * HlsSettings
    HlsSettings (..),
    newHlsSettings,
    hlsSettings_fmp4HlsSettings,
    hlsSettings_audioOnlyHlsSettings,
    hlsSettings_frameCaptureHlsSettings,
    hlsSettings_standardHlsSettings,

    -- * HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings (..),
    newHlsTimedMetadataScheduleActionSettings,
    hlsTimedMetadataScheduleActionSettings_id3,

    -- * HlsWebdavSettings
    HlsWebdavSettings (..),
    newHlsWebdavSettings,
    hlsWebdavSettings_httpTransferMode,
    hlsWebdavSettings_numRetries,
    hlsWebdavSettings_connectionRetryInterval,
    hlsWebdavSettings_filecacheDuration,
    hlsWebdavSettings_restartDelay,

    -- * HtmlMotionGraphicsSettings
    HtmlMotionGraphicsSettings (..),
    newHtmlMotionGraphicsSettings,

    -- * ImmediateModeScheduleActionStartSettings
    ImmediateModeScheduleActionStartSettings (..),
    newImmediateModeScheduleActionStartSettings,

    -- * Input
    Input (..),
    newInput,
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

    -- * InputAttachment
    InputAttachment (..),
    newInputAttachment,
    inputAttachment_inputAttachmentName,
    inputAttachment_inputId,
    inputAttachment_automaticInputFailoverSettings,
    inputAttachment_inputSettings,

    -- * InputChannelLevel
    InputChannelLevel (..),
    newInputChannelLevel,
    inputChannelLevel_inputChannel,
    inputChannelLevel_gain,

    -- * InputClippingSettings
    InputClippingSettings (..),
    newInputClippingSettings,
    inputClippingSettings_stopTimecode,
    inputClippingSettings_startTimecode,
    inputClippingSettings_inputTimecodeSource,

    -- * InputDestination
    InputDestination (..),
    newInputDestination,
    inputDestination_url,
    inputDestination_ip,
    inputDestination_vpc,
    inputDestination_port,

    -- * InputDestinationRequest
    InputDestinationRequest (..),
    newInputDestinationRequest,
    inputDestinationRequest_streamName,

    -- * InputDestinationVpc
    InputDestinationVpc (..),
    newInputDestinationVpc,
    inputDestinationVpc_networkInterfaceId,
    inputDestinationVpc_availabilityZone,

    -- * InputDeviceConfigurableSettings
    InputDeviceConfigurableSettings (..),
    newInputDeviceConfigurableSettings,
    inputDeviceConfigurableSettings_configuredInput,
    inputDeviceConfigurableSettings_maxBitrate,

    -- * InputDeviceHdSettings
    InputDeviceHdSettings (..),
    newInputDeviceHdSettings,
    inputDeviceHdSettings_framerate,
    inputDeviceHdSettings_scanType,
    inputDeviceHdSettings_deviceState,
    inputDeviceHdSettings_height,
    inputDeviceHdSettings_activeInput,
    inputDeviceHdSettings_width,
    inputDeviceHdSettings_configuredInput,
    inputDeviceHdSettings_maxBitrate,

    -- * InputDeviceNetworkSettings
    InputDeviceNetworkSettings (..),
    newInputDeviceNetworkSettings,
    inputDeviceNetworkSettings_ipAddress,
    inputDeviceNetworkSettings_gateway,
    inputDeviceNetworkSettings_dnsAddresses,
    inputDeviceNetworkSettings_ipScheme,
    inputDeviceNetworkSettings_subnetMask,

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

    -- * InputDeviceUhdSettings
    InputDeviceUhdSettings (..),
    newInputDeviceUhdSettings,
    inputDeviceUhdSettings_framerate,
    inputDeviceUhdSettings_scanType,
    inputDeviceUhdSettings_deviceState,
    inputDeviceUhdSettings_height,
    inputDeviceUhdSettings_activeInput,
    inputDeviceUhdSettings_width,
    inputDeviceUhdSettings_configuredInput,
    inputDeviceUhdSettings_maxBitrate,

    -- * InputLocation
    InputLocation (..),
    newInputLocation,
    inputLocation_username,
    inputLocation_passwordParam,
    inputLocation_uri,

    -- * InputLossBehavior
    InputLossBehavior (..),
    newInputLossBehavior,
    inputLossBehavior_inputLossImageColor,
    inputLossBehavior_blackFrameMsec,
    inputLossBehavior_repeatFrameMsec,
    inputLossBehavior_inputLossImageType,
    inputLossBehavior_inputLossImageSlate,

    -- * InputLossFailoverSettings
    InputLossFailoverSettings (..),
    newInputLossFailoverSettings,
    inputLossFailoverSettings_inputLossThresholdMsec,

    -- * InputPrepareScheduleActionSettings
    InputPrepareScheduleActionSettings (..),
    newInputPrepareScheduleActionSettings,
    inputPrepareScheduleActionSettings_inputAttachmentNameReference,
    inputPrepareScheduleActionSettings_inputClippingSettings,
    inputPrepareScheduleActionSettings_urlPath,

    -- * InputSecurityGroup
    InputSecurityGroup (..),
    newInputSecurityGroup,
    inputSecurityGroup_state,
    inputSecurityGroup_arn,
    inputSecurityGroup_inputs,
    inputSecurityGroup_id,
    inputSecurityGroup_whitelistRules,
    inputSecurityGroup_tags,

    -- * InputSettings
    InputSettings (..),
    newInputSettings,
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

    -- * InputSource
    InputSource (..),
    newInputSource,
    inputSource_url,
    inputSource_username,
    inputSource_passwordParam,

    -- * InputSourceRequest
    InputSourceRequest (..),
    newInputSourceRequest,
    inputSourceRequest_url,
    inputSourceRequest_username,
    inputSourceRequest_passwordParam,

    -- * InputSpecification
    InputSpecification (..),
    newInputSpecification,
    inputSpecification_resolution,
    inputSpecification_codec,
    inputSpecification_maximumBitrate,

    -- * InputSwitchScheduleActionSettings
    InputSwitchScheduleActionSettings (..),
    newInputSwitchScheduleActionSettings,
    inputSwitchScheduleActionSettings_inputClippingSettings,
    inputSwitchScheduleActionSettings_urlPath,
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

    -- * M3u8Settings
    M3u8Settings (..),
    newM3u8Settings,
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

    -- * MotionGraphicsActivateScheduleActionSettings
    MotionGraphicsActivateScheduleActionSettings (..),
    newMotionGraphicsActivateScheduleActionSettings,
    motionGraphicsActivateScheduleActionSettings_url,
    motionGraphicsActivateScheduleActionSettings_username,
    motionGraphicsActivateScheduleActionSettings_passwordParam,
    motionGraphicsActivateScheduleActionSettings_duration,

    -- * MotionGraphicsConfiguration
    MotionGraphicsConfiguration (..),
    newMotionGraphicsConfiguration,
    motionGraphicsConfiguration_motionGraphicsInsertion,
    motionGraphicsConfiguration_motionGraphicsSettings,

    -- * MotionGraphicsDeactivateScheduleActionSettings
    MotionGraphicsDeactivateScheduleActionSettings (..),
    newMotionGraphicsDeactivateScheduleActionSettings,

    -- * MotionGraphicsSettings
    MotionGraphicsSettings (..),
    newMotionGraphicsSettings,
    motionGraphicsSettings_htmlMotionGraphicsSettings,

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

    -- * MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    newMsSmoothGroupSettings,
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

    -- * MsSmoothOutputSettings
    MsSmoothOutputSettings (..),
    newMsSmoothOutputSettings,
    msSmoothOutputSettings_h265PackagingType,
    msSmoothOutputSettings_nameModifier,

    -- * Multiplex
    Multiplex (..),
    newMultiplex,
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
    multiplexProgram_pipelineDetails,
    multiplexProgram_programName,
    multiplexProgram_channelId,
    multiplexProgram_multiplexProgramSettings,

    -- * MultiplexProgramChannelDestinationSettings
    MultiplexProgramChannelDestinationSettings (..),
    newMultiplexProgramChannelDestinationSettings,
    multiplexProgramChannelDestinationSettings_multiplexId,
    multiplexProgramChannelDestinationSettings_programName,

    -- * MultiplexProgramPacketIdentifiersMap
    MultiplexProgramPacketIdentifiersMap (..),
    newMultiplexProgramPacketIdentifiersMap,
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
    multiplexProgramSettings_videoSettings,
    multiplexProgramSettings_serviceDescriptor,
    multiplexProgramSettings_programNumber,

    -- * MultiplexProgramSummary
    MultiplexProgramSummary (..),
    newMultiplexProgramSummary,
    multiplexProgramSummary_programName,
    multiplexProgramSummary_channelId,

    -- * MultiplexSettings
    MultiplexSettings (..),
    newMultiplexSettings,
    multiplexSettings_maximumVideoBufferDelayMilliseconds,
    multiplexSettings_transportStreamReservedBitrate,
    multiplexSettings_transportStreamBitrate,
    multiplexSettings_transportStreamId,

    -- * MultiplexSettingsSummary
    MultiplexSettingsSummary (..),
    newMultiplexSettingsSummary,
    multiplexSettingsSummary_transportStreamBitrate,

    -- * MultiplexStatmuxVideoSettings
    MultiplexStatmuxVideoSettings (..),
    newMultiplexStatmuxVideoSettings,
    multiplexStatmuxVideoSettings_priority,
    multiplexStatmuxVideoSettings_minimumBitrate,
    multiplexStatmuxVideoSettings_maximumBitrate,

    -- * MultiplexSummary
    MultiplexSummary (..),
    newMultiplexSummary,
    multiplexSummary_state,
    multiplexSummary_arn,
    multiplexSummary_pipelinesRunningCount,
    multiplexSummary_availabilityZones,
    multiplexSummary_programCount,
    multiplexSummary_name,
    multiplexSummary_id,
    multiplexSummary_multiplexSettings,
    multiplexSummary_tags,

    -- * MultiplexVideoSettings
    MultiplexVideoSettings (..),
    newMultiplexVideoSettings,
    multiplexVideoSettings_statmuxSettings,
    multiplexVideoSettings_constantBitrate,

    -- * NetworkInputSettings
    NetworkInputSettings (..),
    newNetworkInputSettings,
    networkInputSettings_hlsInputSettings,
    networkInputSettings_serverValidation,

    -- * NielsenCBET
    NielsenCBET (..),
    newNielsenCBET,
    nielsenCBET_cbetCheckDigitString,
    nielsenCBET_cbetStepaside,
    nielsenCBET_csid,

    -- * NielsenConfiguration
    NielsenConfiguration (..),
    newNielsenConfiguration,
    nielsenConfiguration_distributorId,
    nielsenConfiguration_nielsenPcmToId3Tagging,

    -- * NielsenNaesIiNw
    NielsenNaesIiNw (..),
    newNielsenNaesIiNw,
    nielsenNaesIiNw_checkDigitString,
    nielsenNaesIiNw_sid,

    -- * NielsenWatermarksSettings
    NielsenWatermarksSettings (..),
    newNielsenWatermarksSettings,
    nielsenWatermarksSettings_nielsenCbetSettings,
    nielsenWatermarksSettings_nielsenNaesIiNwSettings,
    nielsenWatermarksSettings_nielsenDistributionType,

    -- * Offering
    Offering (..),
    newOffering,
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

    -- * Output
    Output (..),
    newOutput,
    output_captionDescriptionNames,
    output_videoDescriptionName,
    output_outputName,
    output_audioDescriptionNames,
    output_outputSettings,

    -- * OutputDestination
    OutputDestination (..),
    newOutputDestination,
    outputDestination_settings,
    outputDestination_mediaPackageSettings,
    outputDestination_id,
    outputDestination_multiplexSettings,

    -- * OutputDestinationSettings
    OutputDestinationSettings (..),
    newOutputDestinationSettings,
    outputDestinationSettings_url,
    outputDestinationSettings_username,
    outputDestinationSettings_passwordParam,
    outputDestinationSettings_streamName,

    -- * OutputGroup
    OutputGroup (..),
    newOutputGroup,
    outputGroup_name,
    outputGroup_outputs,
    outputGroup_outputGroupSettings,

    -- * OutputGroupSettings
    OutputGroupSettings (..),
    newOutputGroupSettings,
    outputGroupSettings_mediaPackageGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_rtmpGroupSettings,
    outputGroupSettings_multiplexGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_archiveGroupSettings,
    outputGroupSettings_udpGroupSettings,
    outputGroupSettings_frameCaptureGroupSettings,

    -- * OutputLocationRef
    OutputLocationRef (..),
    newOutputLocationRef,
    outputLocationRef_destinationRefId,

    -- * OutputSettings
    OutputSettings (..),
    newOutputSettings,
    outputSettings_multiplexOutputSettings,
    outputSettings_archiveOutputSettings,
    outputSettings_rtmpOutputSettings,
    outputSettings_mediaPackageOutputSettings,
    outputSettings_hlsOutputSettings,
    outputSettings_frameCaptureOutputSettings,
    outputSettings_udpOutputSettings,
    outputSettings_msSmoothOutputSettings,

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
    pipelineDetail_activeInputSwitchActionName,
    pipelineDetail_activeMotionGraphicsUri,
    pipelineDetail_activeInputAttachmentName,
    pipelineDetail_activeMotionGraphicsActionName,

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

    -- * ReservationResourceSpecification
    ReservationResourceSpecification (..),
    newReservationResourceSpecification,
    reservationResourceSpecification_videoQuality,
    reservationResourceSpecification_maximumFramerate,
    reservationResourceSpecification_resourceType,
    reservationResourceSpecification_resolution,
    reservationResourceSpecification_codec,
    reservationResourceSpecification_specialFeature,
    reservationResourceSpecification_channelClass,
    reservationResourceSpecification_maximumBitrate,

    -- * RtmpCaptionInfoDestinationSettings
    RtmpCaptionInfoDestinationSettings (..),
    newRtmpCaptionInfoDestinationSettings,

    -- * RtmpGroupSettings
    RtmpGroupSettings (..),
    newRtmpGroupSettings,
    rtmpGroupSettings_inputLossAction,
    rtmpGroupSettings_captionData,
    rtmpGroupSettings_adMarkers,
    rtmpGroupSettings_restartDelay,
    rtmpGroupSettings_authenticationScheme,
    rtmpGroupSettings_cacheLength,
    rtmpGroupSettings_cacheFullBehavior,

    -- * RtmpOutputSettings
    RtmpOutputSettings (..),
    newRtmpOutputSettings,
    rtmpOutputSettings_numRetries,
    rtmpOutputSettings_certificateMode,
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

    -- * ScheduleActionStartSettings
    ScheduleActionStartSettings (..),
    newScheduleActionStartSettings,
    scheduleActionStartSettings_immediateModeScheduleActionStartSettings,
    scheduleActionStartSettings_followModeScheduleActionStartSettings,
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
    scte27SourceSettings_ocrLanguage,
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

    -- * Scte35SpliceInsert
    Scte35SpliceInsert (..),
    newScte35SpliceInsert,
    scte35SpliceInsert_webDeliveryAllowedFlag,
    scte35SpliceInsert_adAvailOffset,
    scte35SpliceInsert_noRegionalBlackoutFlag,

    -- * Scte35SpliceInsertScheduleActionSettings
    Scte35SpliceInsertScheduleActionSettings (..),
    newScte35SpliceInsertScheduleActionSettings,
    scte35SpliceInsertScheduleActionSettings_duration,
    scte35SpliceInsertScheduleActionSettings_spliceEventId,

    -- * Scte35TimeSignalApos
    Scte35TimeSignalApos (..),
    newScte35TimeSignalApos,
    scte35TimeSignalApos_webDeliveryAllowedFlag,
    scte35TimeSignalApos_adAvailOffset,
    scte35TimeSignalApos_noRegionalBlackoutFlag,

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

    -- * StaticImageDeactivateScheduleActionSettings
    StaticImageDeactivateScheduleActionSettings (..),
    newStaticImageDeactivateScheduleActionSettings,
    staticImageDeactivateScheduleActionSettings_fadeOut,
    staticImageDeactivateScheduleActionSettings_layer,

    -- * StaticKeySettings
    StaticKeySettings (..),
    newStaticKeySettings,
    staticKeySettings_keyProviderServer,
    staticKeySettings_staticKeyValue,

    -- * StopTimecode
    StopTimecode (..),
    newStopTimecode,
    stopTimecode_lastFrameClippingBehavior,
    stopTimecode_timecode,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings (..),
    newTeletextDestinationSettings,

    -- * TeletextSourceSettings
    TeletextSourceSettings (..),
    newTeletextSourceSettings,
    teletextSourceSettings_outputRectangle,
    teletextSourceSettings_pageNumber,

    -- * TemporalFilterSettings
    TemporalFilterSettings (..),
    newTemporalFilterSettings,
    temporalFilterSettings_strength,
    temporalFilterSettings_postFilterSharpening,

    -- * TimecodeConfig
    TimecodeConfig (..),
    newTimecodeConfig,
    timecodeConfig_syncThreshold,
    timecodeConfig_source,

    -- * TransferringInputDeviceSummary
    TransferringInputDeviceSummary (..),
    newTransferringInputDeviceSummary,
    transferringInputDeviceSummary_transferType,
    transferringInputDeviceSummary_id,
    transferringInputDeviceSummary_targetCustomerId,
    transferringInputDeviceSummary_message,

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
    udpGroupSettings_inputLossAction,
    udpGroupSettings_timedMetadataId3Frame,

    -- * UdpOutputSettings
    UdpOutputSettings (..),
    newUdpOutputSettings,
    udpOutputSettings_fecOutputSettings,
    udpOutputSettings_bufferMsec,
    udpOutputSettings_destination,
    udpOutputSettings_containerSettings,

    -- * VideoBlackFailoverSettings
    VideoBlackFailoverSettings (..),
    newVideoBlackFailoverSettings,
    videoBlackFailoverSettings_videoBlackThresholdMsec,
    videoBlackFailoverSettings_blackDetectThreshold,

    -- * VideoCodecSettings
    VideoCodecSettings (..),
    newVideoCodecSettings,
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_mpeg2Settings,

    -- * VideoDescription
    VideoDescription (..),
    newVideoDescription,
    videoDescription_height,
    videoDescription_sharpness,
    videoDescription_width,
    videoDescription_scalingBehavior,
    videoDescription_respondToAfd,
    videoDescription_codecSettings,
    videoDescription_name,

    -- * VideoSelector
    VideoSelector (..),
    newVideoSelector,
    videoSelector_selectorSettings,
    videoSelector_colorSpaceUsage,
    videoSelector_colorSpaceSettings,
    videoSelector_colorSpace,

    -- * VideoSelectorColorSpaceSettings
    VideoSelectorColorSpaceSettings (..),
    newVideoSelectorColorSpaceSettings,
    videoSelectorColorSpaceSettings_hdr10Settings,

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
    videoSelectorSettings_videoSelectorProgramId,
    videoSelectorSettings_videoSelectorPid,

    -- * VpcOutputSettings
    VpcOutputSettings (..),
    newVpcOutputSettings,
    vpcOutputSettings_securityGroupIds,
    vpcOutputSettings_publicAddressAllocationIds,
    vpcOutputSettings_subnetIds,

    -- * VpcOutputSettingsDescription
    VpcOutputSettingsDescription (..),
    newVpcOutputSettingsDescription,
    vpcOutputSettingsDescription_securityGroupIds,
    vpcOutputSettingsDescription_subnetIds,
    vpcOutputSettingsDescription_networkInterfaceIds,
    vpcOutputSettingsDescription_availabilityZones,

    -- * WavSettings
    WavSettings (..),
    newWavSettings,
    wavSettings_bitDepth,
    wavSettings_codingMode,
    wavSettings_sampleRate,

    -- * WebvttDestinationSettings
    WebvttDestinationSettings (..),
    newWebvttDestinationSettings,
    webvttDestinationSettings_styleControl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.AacCodingMode
import Amazonka.MediaLive.Types.AacInputType
import Amazonka.MediaLive.Types.AacProfile
import Amazonka.MediaLive.Types.AacRateControlMode
import Amazonka.MediaLive.Types.AacRawFormat
import Amazonka.MediaLive.Types.AacSettings
import Amazonka.MediaLive.Types.AacSpec
import Amazonka.MediaLive.Types.AacVbrQuality
import Amazonka.MediaLive.Types.Ac3BitstreamMode
import Amazonka.MediaLive.Types.Ac3CodingMode
import Amazonka.MediaLive.Types.Ac3DrcProfile
import Amazonka.MediaLive.Types.Ac3LfeFilter
import Amazonka.MediaLive.Types.Ac3MetadataControl
import Amazonka.MediaLive.Types.Ac3Settings
import Amazonka.MediaLive.Types.AcceptHeader
import Amazonka.MediaLive.Types.AfdSignaling
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
import Amazonka.MediaLive.Types.AudioDescriptionAudioTypeControl
import Amazonka.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Amazonka.MediaLive.Types.AudioHlsRenditionSelection
import Amazonka.MediaLive.Types.AudioLanguageSelection
import Amazonka.MediaLive.Types.AudioLanguageSelectionPolicy
import Amazonka.MediaLive.Types.AudioNormalizationAlgorithm
import Amazonka.MediaLive.Types.AudioNormalizationAlgorithmControl
import Amazonka.MediaLive.Types.AudioNormalizationSettings
import Amazonka.MediaLive.Types.AudioOnlyHlsSegmentType
import Amazonka.MediaLive.Types.AudioOnlyHlsSettings
import Amazonka.MediaLive.Types.AudioOnlyHlsTrackType
import Amazonka.MediaLive.Types.AudioPidSelection
import Amazonka.MediaLive.Types.AudioSelector
import Amazonka.MediaLive.Types.AudioSelectorSettings
import Amazonka.MediaLive.Types.AudioSilenceFailoverSettings
import Amazonka.MediaLive.Types.AudioTrack
import Amazonka.MediaLive.Types.AudioTrackSelection
import Amazonka.MediaLive.Types.AudioType
import Amazonka.MediaLive.Types.AudioWatermarkSettings
import Amazonka.MediaLive.Types.AuthenticationScheme
import Amazonka.MediaLive.Types.AutomaticInputFailoverSettings
import Amazonka.MediaLive.Types.AvailBlanking
import Amazonka.MediaLive.Types.AvailBlankingState
import Amazonka.MediaLive.Types.AvailConfiguration
import Amazonka.MediaLive.Types.AvailSettings
import Amazonka.MediaLive.Types.BatchFailedResultModel
import Amazonka.MediaLive.Types.BatchScheduleActionCreateRequest
import Amazonka.MediaLive.Types.BatchScheduleActionCreateResult
import Amazonka.MediaLive.Types.BatchScheduleActionDeleteRequest
import Amazonka.MediaLive.Types.BatchScheduleActionDeleteResult
import Amazonka.MediaLive.Types.BatchSuccessfulResultModel
import Amazonka.MediaLive.Types.BlackoutSlate
import Amazonka.MediaLive.Types.BlackoutSlateNetworkEndBlackout
import Amazonka.MediaLive.Types.BlackoutSlateState
import Amazonka.MediaLive.Types.BurnInAlignment
import Amazonka.MediaLive.Types.BurnInBackgroundColor
import Amazonka.MediaLive.Types.BurnInDestinationSettings
import Amazonka.MediaLive.Types.BurnInFontColor
import Amazonka.MediaLive.Types.BurnInOutlineColor
import Amazonka.MediaLive.Types.BurnInShadowColor
import Amazonka.MediaLive.Types.BurnInTeletextGridControl
import Amazonka.MediaLive.Types.CaptionDescription
import Amazonka.MediaLive.Types.CaptionDestinationSettings
import Amazonka.MediaLive.Types.CaptionLanguageMapping
import Amazonka.MediaLive.Types.CaptionRectangle
import Amazonka.MediaLive.Types.CaptionSelector
import Amazonka.MediaLive.Types.CaptionSelectorSettings
import Amazonka.MediaLive.Types.CdiInputResolution
import Amazonka.MediaLive.Types.CdiInputSpecification
import Amazonka.MediaLive.Types.Channel
import Amazonka.MediaLive.Types.ChannelClass
import Amazonka.MediaLive.Types.ChannelEgressEndpoint
import Amazonka.MediaLive.Types.ChannelState
import Amazonka.MediaLive.Types.ChannelSummary
import Amazonka.MediaLive.Types.ColorSpacePassthroughSettings
import Amazonka.MediaLive.Types.ContentType
import Amazonka.MediaLive.Types.DeviceSettingsSyncState
import Amazonka.MediaLive.Types.DeviceUpdateStatus
import Amazonka.MediaLive.Types.DvbNitSettings
import Amazonka.MediaLive.Types.DvbSdtOutputSdt
import Amazonka.MediaLive.Types.DvbSdtSettings
import Amazonka.MediaLive.Types.DvbSubDestinationAlignment
import Amazonka.MediaLive.Types.DvbSubDestinationBackgroundColor
import Amazonka.MediaLive.Types.DvbSubDestinationFontColor
import Amazonka.MediaLive.Types.DvbSubDestinationOutlineColor
import Amazonka.MediaLive.Types.DvbSubDestinationSettings
import Amazonka.MediaLive.Types.DvbSubDestinationShadowColor
import Amazonka.MediaLive.Types.DvbSubDestinationTeletextGridControl
import Amazonka.MediaLive.Types.DvbSubOcrLanguage
import Amazonka.MediaLive.Types.DvbSubSourceSettings
import Amazonka.MediaLive.Types.DvbTdtSettings
import Amazonka.MediaLive.Types.Eac3AttenuationControl
import Amazonka.MediaLive.Types.Eac3BitstreamMode
import Amazonka.MediaLive.Types.Eac3CodingMode
import Amazonka.MediaLive.Types.Eac3DcFilter
import Amazonka.MediaLive.Types.Eac3DrcLine
import Amazonka.MediaLive.Types.Eac3DrcRf
import Amazonka.MediaLive.Types.Eac3LfeControl
import Amazonka.MediaLive.Types.Eac3LfeFilter
import Amazonka.MediaLive.Types.Eac3MetadataControl
import Amazonka.MediaLive.Types.Eac3PassthroughControl
import Amazonka.MediaLive.Types.Eac3PhaseControl
import Amazonka.MediaLive.Types.Eac3Settings
import Amazonka.MediaLive.Types.Eac3StereoDownmix
import Amazonka.MediaLive.Types.Eac3SurroundExMode
import Amazonka.MediaLive.Types.Eac3SurroundMode
import Amazonka.MediaLive.Types.EbuTtDDestinationSettings
import Amazonka.MediaLive.Types.EbuTtDDestinationStyleControl
import Amazonka.MediaLive.Types.EbuTtDFillLineGapControl
import Amazonka.MediaLive.Types.EmbeddedConvert608To708
import Amazonka.MediaLive.Types.EmbeddedDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Amazonka.MediaLive.Types.EmbeddedScte20Detection
import Amazonka.MediaLive.Types.EmbeddedSourceSettings
import Amazonka.MediaLive.Types.EncoderSettings
import Amazonka.MediaLive.Types.FailoverCondition
import Amazonka.MediaLive.Types.FailoverConditionSettings
import Amazonka.MediaLive.Types.FeatureActivations
import Amazonka.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import Amazonka.MediaLive.Types.FecOutputIncludeFec
import Amazonka.MediaLive.Types.FecOutputSettings
import Amazonka.MediaLive.Types.FixedAfd
import Amazonka.MediaLive.Types.FixedModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.Fmp4HlsSettings
import Amazonka.MediaLive.Types.Fmp4NielsenId3Behavior
import Amazonka.MediaLive.Types.Fmp4TimedMetadataBehavior
import Amazonka.MediaLive.Types.FollowModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.FollowPoint
import Amazonka.MediaLive.Types.FrameCaptureCdnSettings
import Amazonka.MediaLive.Types.FrameCaptureGroupSettings
import Amazonka.MediaLive.Types.FrameCaptureHlsSettings
import Amazonka.MediaLive.Types.FrameCaptureIntervalUnit
import Amazonka.MediaLive.Types.FrameCaptureOutputSettings
import Amazonka.MediaLive.Types.FrameCaptureS3Settings
import Amazonka.MediaLive.Types.FrameCaptureSettings
import Amazonka.MediaLive.Types.GlobalConfiguration
import Amazonka.MediaLive.Types.GlobalConfigurationInputEndAction
import Amazonka.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Amazonka.MediaLive.Types.GlobalConfigurationOutputLockingMode
import Amazonka.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Amazonka.MediaLive.Types.H264AdaptiveQuantization
import Amazonka.MediaLive.Types.H264ColorMetadata
import Amazonka.MediaLive.Types.H264ColorSpaceSettings
import Amazonka.MediaLive.Types.H264EntropyEncoding
import Amazonka.MediaLive.Types.H264FilterSettings
import Amazonka.MediaLive.Types.H264FlickerAq
import Amazonka.MediaLive.Types.H264ForceFieldPictures
import Amazonka.MediaLive.Types.H264FramerateControl
import Amazonka.MediaLive.Types.H264GopBReference
import Amazonka.MediaLive.Types.H264GopSizeUnits
import Amazonka.MediaLive.Types.H264Level
import Amazonka.MediaLive.Types.H264LookAheadRateControl
import Amazonka.MediaLive.Types.H264ParControl
import Amazonka.MediaLive.Types.H264Profile
import Amazonka.MediaLive.Types.H264QualityLevel
import Amazonka.MediaLive.Types.H264RateControlMode
import Amazonka.MediaLive.Types.H264ScanType
import Amazonka.MediaLive.Types.H264SceneChangeDetect
import Amazonka.MediaLive.Types.H264Settings
import Amazonka.MediaLive.Types.H264SpatialAq
import Amazonka.MediaLive.Types.H264SubGopLength
import Amazonka.MediaLive.Types.H264Syntax
import Amazonka.MediaLive.Types.H264TemporalAq
import Amazonka.MediaLive.Types.H264TimecodeInsertionBehavior
import Amazonka.MediaLive.Types.H265AdaptiveQuantization
import Amazonka.MediaLive.Types.H265AlternativeTransferFunction
import Amazonka.MediaLive.Types.H265ColorMetadata
import Amazonka.MediaLive.Types.H265ColorSpaceSettings
import Amazonka.MediaLive.Types.H265FilterSettings
import Amazonka.MediaLive.Types.H265FlickerAq
import Amazonka.MediaLive.Types.H265GopSizeUnits
import Amazonka.MediaLive.Types.H265Level
import Amazonka.MediaLive.Types.H265LookAheadRateControl
import Amazonka.MediaLive.Types.H265Profile
import Amazonka.MediaLive.Types.H265RateControlMode
import Amazonka.MediaLive.Types.H265ScanType
import Amazonka.MediaLive.Types.H265SceneChangeDetect
import Amazonka.MediaLive.Types.H265Settings
import Amazonka.MediaLive.Types.H265Tier
import Amazonka.MediaLive.Types.H265TimecodeInsertionBehavior
import Amazonka.MediaLive.Types.Hdr10Settings
import Amazonka.MediaLive.Types.HlsAdMarkers
import Amazonka.MediaLive.Types.HlsAkamaiHttpTransferMode
import Amazonka.MediaLive.Types.HlsAkamaiSettings
import Amazonka.MediaLive.Types.HlsBasicPutSettings
import Amazonka.MediaLive.Types.HlsCaptionLanguageSetting
import Amazonka.MediaLive.Types.HlsCdnSettings
import Amazonka.MediaLive.Types.HlsClientCache
import Amazonka.MediaLive.Types.HlsCodecSpecification
import Amazonka.MediaLive.Types.HlsDirectoryStructure
import Amazonka.MediaLive.Types.HlsDiscontinuityTags
import Amazonka.MediaLive.Types.HlsEncryptionType
import Amazonka.MediaLive.Types.HlsGroupSettings
import Amazonka.MediaLive.Types.HlsH265PackagingType
import Amazonka.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Amazonka.MediaLive.Types.HlsId3SegmentTaggingState
import Amazonka.MediaLive.Types.HlsIncompleteSegmentBehavior
import Amazonka.MediaLive.Types.HlsInputSettings
import Amazonka.MediaLive.Types.HlsIvInManifest
import Amazonka.MediaLive.Types.HlsIvSource
import Amazonka.MediaLive.Types.HlsManifestCompression
import Amazonka.MediaLive.Types.HlsManifestDurationFormat
import Amazonka.MediaLive.Types.HlsMediaStoreSettings
import Amazonka.MediaLive.Types.HlsMediaStoreStorageClass
import Amazonka.MediaLive.Types.HlsMode
import Amazonka.MediaLive.Types.HlsOutputSelection
import Amazonka.MediaLive.Types.HlsOutputSettings
import Amazonka.MediaLive.Types.HlsProgramDateTime
import Amazonka.MediaLive.Types.HlsRedundantManifest
import Amazonka.MediaLive.Types.HlsS3Settings
import Amazonka.MediaLive.Types.HlsScte35SourceType
import Amazonka.MediaLive.Types.HlsSegmentationMode
import Amazonka.MediaLive.Types.HlsSettings
import Amazonka.MediaLive.Types.HlsStreamInfResolution
import Amazonka.MediaLive.Types.HlsTimedMetadataId3Frame
import Amazonka.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Amazonka.MediaLive.Types.HlsTsFileMode
import Amazonka.MediaLive.Types.HlsWebdavHttpTransferMode
import Amazonka.MediaLive.Types.HlsWebdavSettings
import Amazonka.MediaLive.Types.HtmlMotionGraphicsSettings
import Amazonka.MediaLive.Types.IFrameOnlyPlaylistType
import Amazonka.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import Amazonka.MediaLive.Types.Input
import Amazonka.MediaLive.Types.InputAttachment
import Amazonka.MediaLive.Types.InputChannelLevel
import Amazonka.MediaLive.Types.InputClass
import Amazonka.MediaLive.Types.InputClippingSettings
import Amazonka.MediaLive.Types.InputCodec
import Amazonka.MediaLive.Types.InputDeblockFilter
import Amazonka.MediaLive.Types.InputDenoiseFilter
import Amazonka.MediaLive.Types.InputDestination
import Amazonka.MediaLive.Types.InputDestinationRequest
import Amazonka.MediaLive.Types.InputDestinationVpc
import Amazonka.MediaLive.Types.InputDeviceActiveInput
import Amazonka.MediaLive.Types.InputDeviceConfigurableSettings
import Amazonka.MediaLive.Types.InputDeviceConfiguredInput
import Amazonka.MediaLive.Types.InputDeviceConnectionState
import Amazonka.MediaLive.Types.InputDeviceHdSettings
import Amazonka.MediaLive.Types.InputDeviceIpScheme
import Amazonka.MediaLive.Types.InputDeviceNetworkSettings
import Amazonka.MediaLive.Types.InputDeviceRequest
import Amazonka.MediaLive.Types.InputDeviceScanType
import Amazonka.MediaLive.Types.InputDeviceSettings
import Amazonka.MediaLive.Types.InputDeviceState
import Amazonka.MediaLive.Types.InputDeviceSummary
import Amazonka.MediaLive.Types.InputDeviceTransferType
import Amazonka.MediaLive.Types.InputDeviceType
import Amazonka.MediaLive.Types.InputDeviceUhdSettings
import Amazonka.MediaLive.Types.InputFilter
import Amazonka.MediaLive.Types.InputLocation
import Amazonka.MediaLive.Types.InputLossActionForHlsOut
import Amazonka.MediaLive.Types.InputLossActionForMsSmoothOut
import Amazonka.MediaLive.Types.InputLossActionForRtmpOut
import Amazonka.MediaLive.Types.InputLossActionForUdpOut
import Amazonka.MediaLive.Types.InputLossBehavior
import Amazonka.MediaLive.Types.InputLossFailoverSettings
import Amazonka.MediaLive.Types.InputLossImageType
import Amazonka.MediaLive.Types.InputMaximumBitrate
import Amazonka.MediaLive.Types.InputPreference
import Amazonka.MediaLive.Types.InputPrepareScheduleActionSettings
import Amazonka.MediaLive.Types.InputResolution
import Amazonka.MediaLive.Types.InputSecurityGroup
import Amazonka.MediaLive.Types.InputSecurityGroupState
import Amazonka.MediaLive.Types.InputSettings
import Amazonka.MediaLive.Types.InputSource
import Amazonka.MediaLive.Types.InputSourceEndBehavior
import Amazonka.MediaLive.Types.InputSourceRequest
import Amazonka.MediaLive.Types.InputSourceType
import Amazonka.MediaLive.Types.InputSpecification
import Amazonka.MediaLive.Types.InputState
import Amazonka.MediaLive.Types.InputSwitchScheduleActionSettings
import Amazonka.MediaLive.Types.InputTimecodeSource
import Amazonka.MediaLive.Types.InputType
import Amazonka.MediaLive.Types.InputVpcRequest
import Amazonka.MediaLive.Types.InputWhitelistRule
import Amazonka.MediaLive.Types.InputWhitelistRuleCidr
import Amazonka.MediaLive.Types.KeyProviderSettings
import Amazonka.MediaLive.Types.LastFrameClippingBehavior
import Amazonka.MediaLive.Types.LogLevel
import Amazonka.MediaLive.Types.M2tsAbsentInputAudioBehavior
import Amazonka.MediaLive.Types.M2tsArib
import Amazonka.MediaLive.Types.M2tsAribCaptionsPidControl
import Amazonka.MediaLive.Types.M2tsAudioBufferModel
import Amazonka.MediaLive.Types.M2tsAudioInterval
import Amazonka.MediaLive.Types.M2tsAudioStreamType
import Amazonka.MediaLive.Types.M2tsBufferModel
import Amazonka.MediaLive.Types.M2tsCcDescriptor
import Amazonka.MediaLive.Types.M2tsEbifControl
import Amazonka.MediaLive.Types.M2tsEbpPlacement
import Amazonka.MediaLive.Types.M2tsEsRateInPes
import Amazonka.MediaLive.Types.M2tsKlv
import Amazonka.MediaLive.Types.M2tsNielsenId3Behavior
import Amazonka.MediaLive.Types.M2tsPcrControl
import Amazonka.MediaLive.Types.M2tsRateMode
import Amazonka.MediaLive.Types.M2tsScte35Control
import Amazonka.MediaLive.Types.M2tsSegmentationMarkers
import Amazonka.MediaLive.Types.M2tsSegmentationStyle
import Amazonka.MediaLive.Types.M2tsSettings
import Amazonka.MediaLive.Types.M2tsTimedMetadataBehavior
import Amazonka.MediaLive.Types.M3u8NielsenId3Behavior
import Amazonka.MediaLive.Types.M3u8PcrControl
import Amazonka.MediaLive.Types.M3u8Scte35Behavior
import Amazonka.MediaLive.Types.M3u8Settings
import Amazonka.MediaLive.Types.M3u8TimedMetadataBehavior
import Amazonka.MediaLive.Types.MediaConnectFlow
import Amazonka.MediaLive.Types.MediaConnectFlowRequest
import Amazonka.MediaLive.Types.MediaPackageGroupSettings
import Amazonka.MediaLive.Types.MediaPackageOutputDestinationSettings
import Amazonka.MediaLive.Types.MediaPackageOutputSettings
import Amazonka.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsConfiguration
import Amazonka.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsInsertion
import Amazonka.MediaLive.Types.MotionGraphicsSettings
import Amazonka.MediaLive.Types.Mp2CodingMode
import Amazonka.MediaLive.Types.Mp2Settings
import Amazonka.MediaLive.Types.Mpeg2AdaptiveQuantization
import Amazonka.MediaLive.Types.Mpeg2ColorMetadata
import Amazonka.MediaLive.Types.Mpeg2ColorSpace
import Amazonka.MediaLive.Types.Mpeg2DisplayRatio
import Amazonka.MediaLive.Types.Mpeg2FilterSettings
import Amazonka.MediaLive.Types.Mpeg2GopSizeUnits
import Amazonka.MediaLive.Types.Mpeg2ScanType
import Amazonka.MediaLive.Types.Mpeg2Settings
import Amazonka.MediaLive.Types.Mpeg2SubGopLength
import Amazonka.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
import Amazonka.MediaLive.Types.MsSmoothGroupSettings
import Amazonka.MediaLive.Types.MsSmoothH265PackagingType
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
import Amazonka.MediaLive.Types.MultiplexState
import Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings
import Amazonka.MediaLive.Types.MultiplexSummary
import Amazonka.MediaLive.Types.MultiplexVideoSettings
import Amazonka.MediaLive.Types.NetworkInputServerValidation
import Amazonka.MediaLive.Types.NetworkInputSettings
import Amazonka.MediaLive.Types.NielsenCBET
import Amazonka.MediaLive.Types.NielsenConfiguration
import Amazonka.MediaLive.Types.NielsenNaesIiNw
import Amazonka.MediaLive.Types.NielsenPcmToId3TaggingState
import Amazonka.MediaLive.Types.NielsenWatermarksCbetStepaside
import Amazonka.MediaLive.Types.NielsenWatermarksDistributionTypes
import Amazonka.MediaLive.Types.NielsenWatermarksSettings
import Amazonka.MediaLive.Types.Offering
import Amazonka.MediaLive.Types.OfferingDurationUnits
import Amazonka.MediaLive.Types.OfferingType
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
import Amazonka.MediaLive.Types.PipelineId
import Amazonka.MediaLive.Types.PipelinePauseStateSettings
import Amazonka.MediaLive.Types.PreferredChannelPipeline
import Amazonka.MediaLive.Types.RawSettings
import Amazonka.MediaLive.Types.Rec601Settings
import Amazonka.MediaLive.Types.Rec709Settings
import Amazonka.MediaLive.Types.RemixSettings
import Amazonka.MediaLive.Types.Reservation
import Amazonka.MediaLive.Types.ReservationCodec
import Amazonka.MediaLive.Types.ReservationMaximumBitrate
import Amazonka.MediaLive.Types.ReservationMaximumFramerate
import Amazonka.MediaLive.Types.ReservationResolution
import Amazonka.MediaLive.Types.ReservationResourceSpecification
import Amazonka.MediaLive.Types.ReservationResourceType
import Amazonka.MediaLive.Types.ReservationSpecialFeature
import Amazonka.MediaLive.Types.ReservationState
import Amazonka.MediaLive.Types.ReservationVideoQuality
import Amazonka.MediaLive.Types.RtmpAdMarkers
import Amazonka.MediaLive.Types.RtmpCacheFullBehavior
import Amazonka.MediaLive.Types.RtmpCaptionData
import Amazonka.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Amazonka.MediaLive.Types.RtmpGroupSettings
import Amazonka.MediaLive.Types.RtmpOutputCertificateMode
import Amazonka.MediaLive.Types.RtmpOutputSettings
import Amazonka.MediaLive.Types.S3CannedAcl
import Amazonka.MediaLive.Types.ScheduleAction
import Amazonka.MediaLive.Types.ScheduleActionSettings
import Amazonka.MediaLive.Types.ScheduleActionStartSettings
import Amazonka.MediaLive.Types.Scte20Convert608To708
import Amazonka.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Amazonka.MediaLive.Types.Scte20SourceSettings
import Amazonka.MediaLive.Types.Scte27DestinationSettings
import Amazonka.MediaLive.Types.Scte27OcrLanguage
import Amazonka.MediaLive.Types.Scte27SourceSettings
import Amazonka.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Amazonka.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import Amazonka.MediaLive.Types.Scte35ArchiveAllowedFlag
import Amazonka.MediaLive.Types.Scte35DeliveryRestrictions
import Amazonka.MediaLive.Types.Scte35Descriptor
import Amazonka.MediaLive.Types.Scte35DescriptorSettings
import Amazonka.MediaLive.Types.Scte35DeviceRestrictions
import Amazonka.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Amazonka.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35SegmentationCancelIndicator
import Amazonka.MediaLive.Types.Scte35SegmentationDescriptor
import Amazonka.MediaLive.Types.Scte35SpliceInsert
import Amazonka.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
import Amazonka.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
import Amazonka.MediaLive.Types.Scte35TimeSignalApos
import Amazonka.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import Amazonka.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
import Amazonka.MediaLive.Types.SmoothGroupCertificateMode
import Amazonka.MediaLive.Types.SmoothGroupEventIdMode
import Amazonka.MediaLive.Types.SmoothGroupEventStopBehavior
import Amazonka.MediaLive.Types.SmoothGroupSegmentationMode
import Amazonka.MediaLive.Types.SmoothGroupSparseTrackType
import Amazonka.MediaLive.Types.SmoothGroupStreamManifestBehavior
import Amazonka.MediaLive.Types.SmoothGroupTimestampOffsetMode
import Amazonka.MediaLive.Types.Smpte2038DataPreference
import Amazonka.MediaLive.Types.SmpteTtDestinationSettings
import Amazonka.MediaLive.Types.StandardHlsSettings
import Amazonka.MediaLive.Types.StartTimecode
import Amazonka.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Amazonka.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import Amazonka.MediaLive.Types.StaticKeySettings
import Amazonka.MediaLive.Types.StopTimecode
import Amazonka.MediaLive.Types.TeletextDestinationSettings
import Amazonka.MediaLive.Types.TeletextSourceSettings
import Amazonka.MediaLive.Types.TemporalFilterPostFilterSharpening
import Amazonka.MediaLive.Types.TemporalFilterSettings
import Amazonka.MediaLive.Types.TemporalFilterStrength
import Amazonka.MediaLive.Types.TimecodeConfig
import Amazonka.MediaLive.Types.TimecodeConfigSource
import Amazonka.MediaLive.Types.TransferringInputDeviceSummary
import Amazonka.MediaLive.Types.TtmlDestinationSettings
import Amazonka.MediaLive.Types.TtmlDestinationStyleControl
import Amazonka.MediaLive.Types.UdpContainerSettings
import Amazonka.MediaLive.Types.UdpGroupSettings
import Amazonka.MediaLive.Types.UdpOutputSettings
import Amazonka.MediaLive.Types.UdpTimedMetadataId3Frame
import Amazonka.MediaLive.Types.VideoBlackFailoverSettings
import Amazonka.MediaLive.Types.VideoCodecSettings
import Amazonka.MediaLive.Types.VideoDescription
import Amazonka.MediaLive.Types.VideoDescriptionRespondToAfd
import Amazonka.MediaLive.Types.VideoDescriptionScalingBehavior
import Amazonka.MediaLive.Types.VideoSelector
import Amazonka.MediaLive.Types.VideoSelectorColorSpace
import Amazonka.MediaLive.Types.VideoSelectorColorSpaceSettings
import Amazonka.MediaLive.Types.VideoSelectorColorSpaceUsage
import Amazonka.MediaLive.Types.VideoSelectorPid
import Amazonka.MediaLive.Types.VideoSelectorProgramId
import Amazonka.MediaLive.Types.VideoSelectorSettings
import Amazonka.MediaLive.Types.VpcOutputSettings
import Amazonka.MediaLive.Types.VpcOutputSettingsDescription
import Amazonka.MediaLive.Types.WavCodingMode
import Amazonka.MediaLive.Types.WavSettings
import Amazonka.MediaLive.Types.WebvttDestinationSettings
import Amazonka.MediaLive.Types.WebvttDestinationStyleControl
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      Core._serviceTimeout = Prelude.Just 70,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Placeholder documentation for GatewayTimeoutException
_GatewayTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GatewayTimeoutException =
  Core._MatchServiceError
    defaultService
    "GatewayTimeoutException"
    Prelude.. Core.hasStatus 504

-- | Placeholder documentation for UnprocessableEntityException
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | Placeholder documentation for ConflictException
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Placeholder documentation for ForbiddenException
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Placeholder documentation for NotFoundException
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Placeholder documentation for TooManyRequestsException
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Placeholder documentation for InternalServerErrorException
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Placeholder documentation for BadGatewayException
_BadGatewayException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError
    defaultService
    "BadGatewayException"
    Prelude.. Core.hasStatus 502

-- | Placeholder documentation for BadRequestException
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
