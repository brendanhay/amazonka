{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types
  ( -- * Service Configuration
    mediaLive,

    -- * Errors

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

    -- * HlsAkamaiHTTPTransferMode
    HlsAkamaiHTTPTransferMode (..),

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

    -- * HlsWebdavHTTPTransferMode
    HlsWebdavHTTPTransferMode (..),

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

    -- * InputDeviceIPScheme
    InputDeviceIPScheme (..),

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

    -- * M2tsCCDescriptor
    M2tsCCDescriptor (..),

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
    AacSettings,
    aacSettings,
    aRawFormat,
    aCodingMode,
    aProfile,
    aRateControlMode,
    aSampleRate,
    aSpec,
    aBitrate,
    aVbrQuality,
    aInputType,

    -- * Ac3Settings
    Ac3Settings,
    ac3Settings,
    asLfeFilter,
    asMetadataControl,
    asBitstreamMode,
    asCodingMode,
    asBitrate,
    asDialnorm,
    asDrcProfile,

    -- * AncillarySourceSettings
    AncillarySourceSettings,
    ancillarySourceSettings,
    assSourceAncillaryChannelNumber,

    -- * ArchiveContainerSettings
    ArchiveContainerSettings,
    archiveContainerSettings,
    acsM2tsSettings,
    acsRawSettings,

    -- * ArchiveGroupSettings
    ArchiveGroupSettings,
    archiveGroupSettings,
    agsRolloverInterval,
    agsDestination,

    -- * ArchiveOutputSettings
    ArchiveOutputSettings,
    archiveOutputSettings,
    aosExtension,
    aosNameModifier,
    aosContainerSettings,

    -- * AribDestinationSettings
    AribDestinationSettings,
    aribDestinationSettings,

    -- * AribSourceSettings
    AribSourceSettings,
    aribSourceSettings,

    -- * AudioChannelMapping
    AudioChannelMapping,
    audioChannelMapping,
    acmOutputChannel,
    acmInputChannelLevels,

    -- * AudioCodecSettings
    AudioCodecSettings,
    audioCodecSettings,
    acsPassThroughSettings,
    acsAc3Settings,
    acsMp2Settings,
    acsWavSettings,
    acsAacSettings,
    acsEac3Settings,

    -- * AudioDescription
    AudioDescription,
    audioDescription,
    adLanguageCode,
    adAudioType,
    adAudioNormalizationSettings,
    adLanguageCodeControl,
    adCodecSettings,
    adStreamName,
    adRemixSettings,
    adAudioTypeControl,
    adAudioSelectorName,
    adName,

    -- * AudioLanguageSelection
    AudioLanguageSelection,
    audioLanguageSelection,
    alsLanguageSelectionPolicy,
    alsLanguageCode,

    -- * AudioNormalizationSettings
    AudioNormalizationSettings,
    audioNormalizationSettings,
    ansAlgorithmControl,
    ansTargetLkfs,
    ansAlgorithm,

    -- * AudioOnlyHlsSettings
    AudioOnlyHlsSettings,
    audioOnlyHlsSettings,
    aohsAudioOnlyImage,
    aohsSegmentType,
    aohsAudioGroupId,
    aohsAudioTrackType,

    -- * AudioPidSelection
    AudioPidSelection,
    audioPidSelection,
    apsPid,

    -- * AudioSelector
    AudioSelector,
    audioSelector,
    asSelectorSettings,
    asName,

    -- * AudioSelectorSettings
    AudioSelectorSettings,
    audioSelectorSettings,
    assAudioLanguageSelection,
    assAudioTrackSelection,
    assAudioPidSelection,

    -- * AudioTrack
    AudioTrack,
    audioTrack,
    atTrack,

    -- * AudioTrackSelection
    AudioTrackSelection,
    audioTrackSelection,
    atsTracks,

    -- * AutomaticInputFailoverSettings
    AutomaticInputFailoverSettings,
    automaticInputFailoverSettings,
    aifsFailoverConditions,
    aifsErrorClearTimeMsec,
    aifsInputPreference,
    aifsSecondaryInputId,

    -- * AvailBlanking
    AvailBlanking,
    availBlanking,
    abState,
    abAvailBlankingImage,

    -- * AvailConfiguration
    AvailConfiguration,
    availConfiguration,
    acAvailSettings,

    -- * AvailSettings
    AvailSettings,
    availSettings,
    asScte35SpliceInsert,
    asScte35TimeSignalApos,

    -- * BatchFailedResultModel
    BatchFailedResultModel,
    batchFailedResultModel,
    bfrmARN,
    bfrmId,
    bfrmCode,
    bfrmMessage,

    -- * BatchScheduleActionCreateRequest
    BatchScheduleActionCreateRequest,
    batchScheduleActionCreateRequest,
    bsacrScheduleActions,

    -- * BatchScheduleActionCreateResult
    BatchScheduleActionCreateResult,
    batchScheduleActionCreateResult,
    bScheduleActions,

    -- * BatchScheduleActionDeleteRequest
    BatchScheduleActionDeleteRequest,
    batchScheduleActionDeleteRequest,
    bsadrActionNames,

    -- * BatchScheduleActionDeleteResult
    BatchScheduleActionDeleteResult,
    batchScheduleActionDeleteResult,
    bsadrScheduleActions,

    -- * BatchSuccessfulResultModel
    BatchSuccessfulResultModel,
    batchSuccessfulResultModel,
    bsrmState,
    bsrmARN,
    bsrmId,

    -- * BlackoutSlate
    BlackoutSlate,
    blackoutSlate,
    bsNetworkEndBlackoutImage,
    bsState,
    bsNetworkEndBlackout,
    bsNetworkId,
    bsBlackoutSlateImage,

    -- * BurnInDestinationSettings
    BurnInDestinationSettings,
    burnInDestinationSettings,
    bidsBackgroundOpacity,
    bidsFontOpacity,
    bidsShadowYOffset,
    bidsFontResolution,
    bidsYPosition,
    bidsBackgroundColor,
    bidsShadowXOffset,
    bidsFontSize,
    bidsXPosition,
    bidsAlignment,
    bidsShadowOpacity,
    bidsTeletextGridControl,
    bidsOutlineColor,
    bidsOutlineSize,
    bidsFont,
    bidsShadowColor,
    bidsFontColor,

    -- * CaptionDescription
    CaptionDescription,
    captionDescription,
    cdLanguageCode,
    cdDestinationSettings,
    cdLanguageDescription,
    cdCaptionSelectorName,
    cdName,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings,
    captionDestinationSettings,
    cdsTeletextDestinationSettings,
    cdsEbuTtDDestinationSettings,
    cdsRtmpCaptionInfoDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsScte27DestinationSettings,
    cdsTtmlDestinationSettings,
    cdsScte20PlusEmbeddedDestinationSettings,
    cdsEmbeddedPlusScte20DestinationSettings,
    cdsSmpteTtDestinationSettings,
    cdsWebvttDestinationSettings,
    cdsEmbeddedDestinationSettings,
    cdsBurnInDestinationSettings,
    cdsAribDestinationSettings,

    -- * CaptionLanguageMapping
    CaptionLanguageMapping,
    captionLanguageMapping,
    clmLanguageCode,
    clmLanguageDescription,
    clmCaptionChannel,

    -- * CaptionSelector
    CaptionSelector,
    captionSelector,
    cLanguageCode,
    cSelectorSettings,
    cName,

    -- * CaptionSelectorSettings
    CaptionSelectorSettings,
    captionSelectorSettings,
    cssTeletextSourceSettings,
    cssAribSourceSettings,
    cssScte27SourceSettings,
    cssDvbSubSourceSettings,
    cssAncillarySourceSettings,
    cssScte20SourceSettings,
    cssEmbeddedSourceSettings,

    -- * CdiInputSpecification
    CdiInputSpecification,
    cdiInputSpecification,
    cisResolution,

    -- * Channel
    Channel,
    channel,
    chaState,
    chaLogLevel,
    chaARN,
    chaPipelinesRunningCount,
    chaPipelineDetails,
    chaInputSpecification,
    chaInputAttachments,
    chaDestinations,
    chaName,
    chaCdiInputSpecification,
    chaId,
    chaChannelClass,
    chaEgressEndpoints,
    chaTags,
    chaEncoderSettings,
    chaRoleARN,

    -- * ChannelEgressEndpoint
    ChannelEgressEndpoint,
    channelEgressEndpoint,
    ceeSourceIP,

    -- * ChannelSummary
    ChannelSummary,
    channelSummary,
    csState,
    csLogLevel,
    csARN,
    csPipelinesRunningCount,
    csInputSpecification,
    csInputAttachments,
    csDestinations,
    csName,
    csCdiInputSpecification,
    csId,
    csChannelClass,
    csEgressEndpoints,
    csTags,
    csRoleARN,

    -- * ColorSpacePassthroughSettings
    ColorSpacePassthroughSettings,
    colorSpacePassthroughSettings,

    -- * DvbNitSettings
    DvbNitSettings,
    dvbNitSettings,
    dnsRepInterval,
    dnsNetworkName,
    dnsNetworkId,

    -- * DvbSdtSettings
    DvbSdtSettings,
    dvbSdtSettings,
    dssRepInterval,
    dssServiceProviderName,
    dssOutputSdt,
    dssServiceName,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings,
    dvbSubDestinationSettings,
    dsdsBackgroundOpacity,
    dsdsFontOpacity,
    dsdsShadowYOffset,
    dsdsFontResolution,
    dsdsYPosition,
    dsdsBackgroundColor,
    dsdsShadowXOffset,
    dsdsFontSize,
    dsdsXPosition,
    dsdsAlignment,
    dsdsShadowOpacity,
    dsdsTeletextGridControl,
    dsdsOutlineColor,
    dsdsOutlineSize,
    dsdsFont,
    dsdsShadowColor,
    dsdsFontColor,

    -- * DvbSubSourceSettings
    DvbSubSourceSettings,
    dvbSubSourceSettings,
    dsssPid,

    -- * DvbTdtSettings
    DvbTdtSettings,
    dvbTdtSettings,
    dtsRepInterval,

    -- * Eac3Settings
    Eac3Settings,
    eac3Settings,
    esStereoDownmix,
    esLoRoCenterMixLevel,
    esLtRtCenterMixLevel,
    esLfeFilter,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esLoRoSurroundMixLevel,
    esSurroundMode,
    esAttenuationControl,
    esPassthroughControl,
    esBitstreamMode,
    esLfeControl,
    esCodingMode,
    esDrcLine,
    esDrcRf,
    esDcFilter,
    esBitrate,
    esPhaseControl,
    esSurroundExMode,
    esDialnorm,

    -- * EbuTtDDestinationSettings
    EbuTtDDestinationSettings,
    ebuTtDDestinationSettings,
    etddsFillLineGap,
    etddsFontFamily,
    etddsStyleControl,

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings,
    embeddedDestinationSettings,

    -- * EmbeddedPlusScte20DestinationSettings
    EmbeddedPlusScte20DestinationSettings,
    embeddedPlusScte20DestinationSettings,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings,
    embeddedSourceSettings,
    essConvert608To708,
    essScte20Detection,
    essSource608TrackNumber,
    essSource608ChannelNumber,

    -- * EncoderSettings
    EncoderSettings,
    encoderSettings,
    esCaptionDescriptions,
    esAvailConfiguration,
    esFeatureActivations,
    esNielsenConfiguration,
    esAvailBlanking,
    esGlobalConfiguration,
    esBlackoutSlate,
    esVideoDescriptions,
    esAudioDescriptions,
    esOutputGroups,
    esTimecodeConfig,

    -- * FailoverCondition
    FailoverCondition,
    failoverCondition,
    fcFailoverConditionSettings,

    -- * FailoverConditionSettings
    FailoverConditionSettings,
    failoverConditionSettings,
    fcsInputLossSettings,

    -- * FeatureActivations
    FeatureActivations,
    featureActivations,
    faInputPrepareScheduleActions,

    -- * FecOutputSettings
    FecOutputSettings,
    fecOutputSettings,
    fosRowLength,
    fosIncludeFec,
    fosColumnDepth,

    -- * FixedModeScheduleActionStartSettings
    FixedModeScheduleActionStartSettings,
    fixedModeScheduleActionStartSettings,
    fmsassTime,

    -- * Fmp4HlsSettings
    Fmp4HlsSettings,
    fmp4HlsSettings,
    fhsNielsenId3Behavior,
    fhsAudioRenditionSets,
    fhsTimedMetadataBehavior,

    -- * FollowModeScheduleActionStartSettings
    FollowModeScheduleActionStartSettings,
    followModeScheduleActionStartSettings,
    fmsassReferenceActionName,
    fmsassFollowPoint,

    -- * FrameCaptureGroupSettings
    FrameCaptureGroupSettings,
    frameCaptureGroupSettings,
    fcgsDestination,

    -- * FrameCaptureOutputSettings
    FrameCaptureOutputSettings,
    frameCaptureOutputSettings,
    fcosNameModifier,

    -- * FrameCaptureSettings
    FrameCaptureSettings,
    frameCaptureSettings,
    fcsCaptureIntervalUnits,
    fcsCaptureInterval,

    -- * GlobalConfiguration
    GlobalConfiguration,
    globalConfiguration,
    gcOutputLockingMode,
    gcInputLossBehavior,
    gcInitialAudioGain,
    gcSupportLowFramerateInputs,
    gcInputEndAction,
    gcOutputTimingSource,

    -- * H264ColorSpaceSettings
    H264ColorSpaceSettings,
    h264ColorSpaceSettings,
    hRec709Settings,
    hRec601Settings,
    hColorSpacePassthroughSettings,

    -- * H264FilterSettings
    H264FilterSettings,
    h264FilterSettings,
    hTemporalFilterSettings,

    -- * H264Settings
    H264Settings,
    h264Settings,
    hssTemporalAq,
    hssSceneChangeDetect,
    hssScanType,
    hssTimecodeInsertion,
    hssParNumerator,
    hssAfdSignaling,
    hssGopSize,
    hssGopSizeUnits,
    hssSubgopLength,
    hssQualityLevel,
    hssSlices,
    hssProfile,
    hssRateControlMode,
    hssMinIInterval,
    hssQvbrQualityLevel,
    hssColorSpaceSettings,
    hssParControl,
    hssFlickerAq,
    hssBufSize,
    hssSpatialAq,
    hssGopNumBFrames,
    hssFixedAfd,
    hssSoftness,
    hssFilterSettings,
    hssBitrate,
    hssFramerateDenominator,
    hssForceFieldPictures,
    hssEntropyEncoding,
    hssFramerateControl,
    hssColorMetadata,
    hssLookAheadRateControl,
    hssAdaptiveQuantization,
    hssFramerateNumerator,
    hssLevel,
    hssGopBReference,
    hssMaxBitrate,
    hssSyntax,
    hssBufFillPct,
    hssGopClosedCadence,
    hssNumRefFrames,
    hssParDenominator,

    -- * H265ColorSpaceSettings
    H265ColorSpaceSettings,
    h265ColorSpaceSettings,
    hcssHdr10Settings,
    hcssRec709Settings,
    hcssRec601Settings,
    hcssColorSpacePassthroughSettings,

    -- * H265FilterSettings
    H265FilterSettings,
    h265FilterSettings,
    hfsTemporalFilterSettings,

    -- * H265Settings
    H265Settings,
    h265Settings,
    hsSceneChangeDetect,
    hsScanType,
    hsTimecodeInsertion,
    hsParNumerator,
    hsAfdSignaling,
    hsGopSize,
    hsGopSizeUnits,
    hsSlices,
    hsProfile,
    hsAlternativeTransferFunction,
    hsRateControlMode,
    hsMinIInterval,
    hsQvbrQualityLevel,
    hsColorSpaceSettings,
    hsFlickerAq,
    hsBufSize,
    hsTier,
    hsFixedAfd,
    hsFilterSettings,
    hsBitrate,
    hsColorMetadata,
    hsLookAheadRateControl,
    hsAdaptiveQuantization,
    hsLevel,
    hsMaxBitrate,
    hsGopClosedCadence,
    hsParDenominator,
    hsFramerateNumerator,
    hsFramerateDenominator,

    -- * Hdr10Settings
    Hdr10Settings,
    hdr10Settings,
    hsMaxFall,
    hsMaxCll,

    -- * HlsAkamaiSettings
    HlsAkamaiSettings,
    hlsAkamaiSettings,
    hasHTTPTransferMode,
    hasNumRetries,
    hasToken,
    hasConnectionRetryInterval,
    hasFilecacheDuration,
    hasRestartDelay,
    hasSalt,

    -- * HlsBasicPutSettings
    HlsBasicPutSettings,
    hlsBasicPutSettings,
    hbpsNumRetries,
    hbpsConnectionRetryInterval,
    hbpsFilecacheDuration,
    hbpsRestartDelay,

    -- * HlsCdnSettings
    HlsCdnSettings,
    hlsCdnSettings,
    hcsHlsAkamaiSettings,
    hcsHlsMediaStoreSettings,
    hcsHlsBasicPutSettings,
    hcsHlsWebdavSettings,

    -- * HlsGroupSettings
    HlsGroupSettings,
    hlsGroupSettings,
    hgsDirectoryStructure,
    hgsEncryptionType,
    hgsTimedMetadataId3Period,
    hgsIvInManifest,
    hgsDiscontinuityTags,
    hgsTsFileMode,
    hgsMinSegmentLength,
    hgsIFrameOnlyPlaylists,
    hgsProgramDateTime,
    hgsIndexNSegments,
    hgsProgramDateTimePeriod,
    hgsCodecSpecification,
    hgsHlsCdnSettings,
    hgsCaptionLanguageMappings,
    hgsInputLossAction,
    hgsMode,
    hgsKeyProviderSettings,
    hgsIncompleteSegmentBehavior,
    hgsConstantIv,
    hgsBaseURLManifest,
    hgsAdMarkers,
    hgsKeyFormat,
    hgsSegmentLength,
    hgsHlsId3SegmentTagging,
    hgsTimedMetadataId3Frame,
    hgsBaseURLContent,
    hgsOutputSelection,
    hgsCaptionLanguageSetting,
    hgsSegmentsPerSubdirectory,
    hgsManifestDurationFormat,
    hgsIvSource,
    hgsSegmentationMode,
    hgsKeyFormatVersions,
    hgsClientCache,
    hgsTimestampDeltaMilliseconds,
    hgsBaseURLManifest1,
    hgsRedundantManifest,
    hgsStreamInfResolution,
    hgsKeepSegments,
    hgsBaseURLContent1,
    hgsManifestCompression,
    hgsDestination,

    -- * HlsId3SegmentTaggingScheduleActionSettings
    HlsId3SegmentTaggingScheduleActionSettings,
    hlsId3SegmentTaggingScheduleActionSettings,
    histsasTag,

    -- * HlsInputSettings
    HlsInputSettings,
    hlsInputSettings,
    hisBufferSegments,
    hisRetries,
    hisRetryInterval,
    hisBandwidth,

    -- * HlsMediaStoreSettings
    HlsMediaStoreSettings,
    hlsMediaStoreSettings,
    hmssNumRetries,
    hmssConnectionRetryInterval,
    hmssFilecacheDuration,
    hmssMediaStoreStorageClass,
    hmssRestartDelay,

    -- * HlsOutputSettings
    HlsOutputSettings,
    hlsOutputSettings,
    hosH265PackagingType,
    hosSegmentModifier,
    hosNameModifier,
    hosHlsSettings,

    -- * HlsSettings
    HlsSettings,
    hlsSettings,
    hsFmp4HlsSettings,
    hsAudioOnlyHlsSettings,
    hsStandardHlsSettings,

    -- * HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings,
    hlsTimedMetadataScheduleActionSettings,
    htmsasId3,

    -- * HlsWebdavSettings
    HlsWebdavSettings,
    hlsWebdavSettings,
    hwsHTTPTransferMode,
    hwsNumRetries,
    hwsConnectionRetryInterval,
    hwsFilecacheDuration,
    hwsRestartDelay,

    -- * ImmediateModeScheduleActionStartSettings
    ImmediateModeScheduleActionStartSettings,
    immediateModeScheduleActionStartSettings,

    -- * Input
    Input,
    input,
    iState,
    iSecurityGroups,
    iARN,
    iInputDevices,
    iSources,
    iDestinations,
    iName,
    iAttachedChannels,
    iId,
    iInputClass,
    iType,
    iMediaConnectFlows,
    iInputSourceType,
    iTags,
    iRoleARN,

    -- * InputAttachment
    InputAttachment,
    inputAttachment,
    iaInputAttachmentName,
    iaInputId,
    iaAutomaticInputFailoverSettings,
    iaInputSettings,

    -- * InputChannelLevel
    InputChannelLevel,
    inputChannelLevel,
    iclInputChannel,
    iclGain,

    -- * InputClippingSettings
    InputClippingSettings,
    inputClippingSettings,
    icsStopTimecode,
    icsStartTimecode,
    icsInputTimecodeSource,

    -- * InputDestination
    InputDestination,
    inputDestination,
    idURL,
    idIP,
    idVPC,
    idPort,

    -- * InputDestinationRequest
    InputDestinationRequest,
    inputDestinationRequest,
    idrStreamName,

    -- * InputDestinationVPC
    InputDestinationVPC,
    inputDestinationVPC,
    idvNetworkInterfaceId,
    idvAvailabilityZone,

    -- * InputDeviceConfigurableSettings
    InputDeviceConfigurableSettings,
    inputDeviceConfigurableSettings,
    idcsConfiguredInput,
    idcsMaxBitrate,

    -- * InputDeviceHdSettings
    InputDeviceHdSettings,
    inputDeviceHdSettings,
    idhsFramerate,
    idhsScanType,
    idhsDeviceState,
    idhsHeight,
    idhsActiveInput,
    idhsWidth,
    idhsConfiguredInput,
    idhsMaxBitrate,

    -- * InputDeviceNetworkSettings
    InputDeviceNetworkSettings,
    inputDeviceNetworkSettings,
    idnsIPAddress,
    idnsGateway,
    idnsDNSAddresses,
    idnsIPScheme,
    idnsSubnetMask,

    -- * InputDeviceRequest
    InputDeviceRequest,
    inputDeviceRequest,
    idrId,

    -- * InputDeviceSettings
    InputDeviceSettings,
    inputDeviceSettings,
    idssId,

    -- * InputDeviceSummary
    InputDeviceSummary,
    inputDeviceSummary,
    idsARN,
    idsMACAddress,
    idsHdDeviceSettings,
    idsName,
    idsId,
    idsDeviceUpdateStatus,
    idsDeviceSettingsSyncState,
    idsType,
    idsSerialNumber,
    idsNetworkSettings,
    idsConnectionState,

    -- * InputLocation
    InputLocation,
    inputLocation,
    ilUsername,
    ilPasswordParam,
    ilURI,

    -- * InputLossBehavior
    InputLossBehavior,
    inputLossBehavior,
    ilbInputLossImageColor,
    ilbBlackFrameMsec,
    ilbRepeatFrameMsec,
    ilbInputLossImageType,
    ilbInputLossImageSlate,

    -- * InputLossFailoverSettings
    InputLossFailoverSettings,
    inputLossFailoverSettings,
    ilfsInputLossThresholdMsec,

    -- * InputPrepareScheduleActionSettings
    InputPrepareScheduleActionSettings,
    inputPrepareScheduleActionSettings,
    ipsasInputAttachmentNameReference,
    ipsasInputClippingSettings,
    ipsasURLPath,

    -- * InputSecurityGroup
    InputSecurityGroup,
    inputSecurityGroup,
    isgState,
    isgARN,
    isgInputs,
    isgId,
    isgWhitelistRules,
    isgTags,

    -- * InputSettings
    InputSettings,
    inputSettings,
    isVideoSelector,
    isSmpte2038DataPreference,
    isNetworkInputSettings,
    isAudioSelectors,
    isDeblockFilter,
    isDenoiseFilter,
    isFilterStrength,
    isCaptionSelectors,
    isInputFilter,
    isSourceEndBehavior,

    -- * InputSource
    InputSource,
    inputSource,
    isURL,
    isUsername,
    isPasswordParam,

    -- * InputSourceRequest
    InputSourceRequest,
    inputSourceRequest,
    isrURL,
    isrUsername,
    isrPasswordParam,

    -- * InputSpecification
    InputSpecification,
    inputSpecification,
    isResolution,
    isCodec,
    isMaximumBitrate,

    -- * InputSwitchScheduleActionSettings
    InputSwitchScheduleActionSettings,
    inputSwitchScheduleActionSettings,
    issasInputClippingSettings,
    issasURLPath,
    issasInputAttachmentNameReference,

    -- * InputVPCRequest
    InputVPCRequest,
    inputVPCRequest,
    ivrSecurityGroupIds,
    ivrSubnetIds,

    -- * InputWhitelistRule
    InputWhitelistRule,
    inputWhitelistRule,
    iwrCidr,

    -- * InputWhitelistRuleCidr
    InputWhitelistRuleCidr,
    inputWhitelistRuleCidr,
    iwrcCidr,

    -- * KeyProviderSettings
    KeyProviderSettings,
    keyProviderSettings,
    kpsStaticKeySettings,

    -- * M2tsSettings
    M2tsSettings,
    m2tsSettings,
    mPmtPid,
    mEtvSignalPid,
    mVideoPid,
    mNielsenId3Behavior,
    mBufferModel,
    mScte35Pid,
    mTransportStreamId,
    mProgramNum,
    mFragmentTime,
    mTimedMetadataBehavior,
    mCCDescriptor,
    mPmtInterval,
    mDvbSdtSettings,
    mEcmPid,
    mNullPacketBitrate,
    mAudioBufferModel,
    mTimedMetadataPid,
    mKlv,
    mAudioFramesPerPes,
    mPcrPeriod,
    mPcrPid,
    mSegmentationMarkers,
    mAribCaptionsPidControl,
    mKlvDataPids,
    mEbpLookaheadMs,
    mDvbSubPids,
    mScte27Pids,
    mPatInterval,
    mAudioStreamType,
    mEsRateInPes,
    mEtvPlatformPid,
    mBitrate,
    mScte35Control,
    mAudioPids,
    mDvbTeletextPid,
    mEbif,
    mArib,
    mAribCaptionsPid,
    mAbsentInputAudioBehavior,
    mSegmentationTime,
    mEbpAudioInterval,
    mDvbNitSettings,
    mPcrControl,
    mEbpPlacement,
    mRateMode,
    mSegmentationStyle,
    mDvbTdtSettings,

    -- * M3u8Settings
    M3u8Settings,
    m3u8Settings,
    mssPmtPid,
    mssVideoPid,
    mssNielsenId3Behavior,
    mssScte35Pid,
    mssTransportStreamId,
    mssProgramNum,
    mssTimedMetadataBehavior,
    mssPmtInterval,
    mssEcmPid,
    mssTimedMetadataPid,
    mssAudioFramesPerPes,
    mssPcrPeriod,
    mssPcrPid,
    mssPatInterval,
    mssAudioPids,
    mssScte35Behavior,
    mssPcrControl,

    -- * MediaConnectFlow
    MediaConnectFlow,
    mediaConnectFlow,
    mcfFlowARN,

    -- * MediaConnectFlowRequest
    MediaConnectFlowRequest,
    mediaConnectFlowRequest,
    mcfrFlowARN,

    -- * MediaPackageGroupSettings
    MediaPackageGroupSettings,
    mediaPackageGroupSettings,
    mpgsDestination,

    -- * MediaPackageOutputDestinationSettings
    MediaPackageOutputDestinationSettings,
    mediaPackageOutputDestinationSettings,
    mpodsChannelId,

    -- * MediaPackageOutputSettings
    MediaPackageOutputSettings,
    mediaPackageOutputSettings,

    -- * Mp2Settings
    Mp2Settings,
    mp2Settings,
    msCodingMode,
    msSampleRate,
    msBitrate,

    -- * Mpeg2FilterSettings
    Mpeg2FilterSettings,
    mpeg2FilterSettings,
    mfsTemporalFilterSettings,

    -- * Mpeg2Settings
    Mpeg2Settings,
    mpeg2Settings,
    msScanType,
    msTimecodeInsertion,
    msAfdSignaling,
    msGopSize,
    msGopSizeUnits,
    msSubgopLength,
    msDisplayAspectRatio,
    msGopNumBFrames,
    msFixedAfd,
    msFilterSettings,
    msColorMetadata,
    msAdaptiveQuantization,
    msGopClosedCadence,
    msColorSpace,
    msFramerateNumerator,
    msFramerateDenominator,

    -- * MsSmoothGroupSettings
    MsSmoothGroupSettings,
    msSmoothGroupSettings,
    msgsFragmentLength,
    msgsStreamManifestBehavior,
    msgsSendDelayMs,
    msgsEventStopBehavior,
    msgsTimestampOffsetMode,
    msgsNumRetries,
    msgsAcquisitionPointId,
    msgsInputLossAction,
    msgsTimestampOffset,
    msgsCertificateMode,
    msgsSparseTrackType,
    msgsConnectionRetryInterval,
    msgsFilecacheDuration,
    msgsRestartDelay,
    msgsEventIdMode,
    msgsAudioOnlyTimecodeControl,
    msgsSegmentationMode,
    msgsEventId,
    msgsDestination,

    -- * MsSmoothOutputSettings
    MsSmoothOutputSettings,
    msSmoothOutputSettings,
    msosH265PackagingType,
    msosNameModifier,

    -- * Multiplex
    Multiplex,
    multiplex,
    mState,
    mARN,
    mPipelinesRunningCount,
    mAvailabilityZones,
    mProgramCount,
    mDestinations,
    mName,
    mId,
    mMultiplexSettings,
    mTags,

    -- * MultiplexGroupSettings
    MultiplexGroupSettings,
    multiplexGroupSettings,

    -- * MultiplexMediaConnectOutputDestinationSettings
    MultiplexMediaConnectOutputDestinationSettings,
    multiplexMediaConnectOutputDestinationSettings,
    mmcodsEntitlementARN,

    -- * MultiplexOutputDestination
    MultiplexOutputDestination,
    multiplexOutputDestination,
    modMediaConnectSettings,

    -- * MultiplexOutputSettings
    MultiplexOutputSettings,
    multiplexOutputSettings,
    mosDestination,

    -- * MultiplexProgram
    MultiplexProgram,
    multiplexProgram,
    mpPacketIdentifiersMap,
    mpPipelineDetails,
    mpProgramName,
    mpChannelId,
    mpMultiplexProgramSettings,

    -- * MultiplexProgramChannelDestinationSettings
    MultiplexProgramChannelDestinationSettings,
    multiplexProgramChannelDestinationSettings,
    mpcdsMultiplexId,
    mpcdsProgramName,

    -- * MultiplexProgramPacketIdentifiersMap
    MultiplexProgramPacketIdentifiersMap,
    multiplexProgramPacketIdentifiersMap,
    mppimPmtPid,
    mppimEtvSignalPid,
    mppimVideoPid,
    mppimScte35Pid,
    mppimPrivateMetadataPid,
    mppimTimedMetadataPid,
    mppimPcrPid,
    mppimKlvDataPids,
    mppimDvbSubPids,
    mppimScte27Pids,
    mppimEtvPlatformPid,
    mppimAudioPids,
    mppimDvbTeletextPid,

    -- * MultiplexProgramPipelineDetail
    MultiplexProgramPipelineDetail,
    multiplexProgramPipelineDetail,
    mppdPipelineId,
    mppdActiveChannelPipeline,

    -- * MultiplexProgramServiceDescriptor
    MultiplexProgramServiceDescriptor,
    multiplexProgramServiceDescriptor,
    mpsdProviderName,
    mpsdServiceName,

    -- * MultiplexProgramSettings
    MultiplexProgramSettings,
    multiplexProgramSettings,
    mpsPreferredChannelPipeline,
    mpsVideoSettings,
    mpsServiceDescriptor,
    mpsProgramNumber,

    -- * MultiplexProgramSummary
    MultiplexProgramSummary,
    multiplexProgramSummary,
    mpsProgramName,
    mpsChannelId,

    -- * MultiplexSettings
    MultiplexSettings,
    multiplexSettings,
    msMaximumVideoBufferDelayMilliseconds,
    msTransportStreamReservedBitrate,
    msTransportStreamBitrate,
    msTransportStreamId,

    -- * MultiplexSettingsSummary
    MultiplexSettingsSummary,
    multiplexSettingsSummary,
    mssTransportStreamBitrate,

    -- * MultiplexStatmuxVideoSettings
    MultiplexStatmuxVideoSettings,
    multiplexStatmuxVideoSettings,
    msvsPriority,
    msvsMinimumBitrate,
    msvsMaximumBitrate,

    -- * MultiplexSummary
    MultiplexSummary,
    multiplexSummary,
    msState,
    msARN,
    msPipelinesRunningCount,
    msAvailabilityZones,
    msProgramCount,
    msName,
    msId,
    msMultiplexSettings,
    msTags,

    -- * MultiplexVideoSettings
    MultiplexVideoSettings,
    multiplexVideoSettings,
    mvsStatmuxSettings,
    mvsConstantBitrate,

    -- * NetworkInputSettings
    NetworkInputSettings,
    networkInputSettings,
    nisHlsInputSettings,
    nisServerValidation,

    -- * NielsenConfiguration
    NielsenConfiguration,
    nielsenConfiguration,
    ncDistributorId,
    ncNielsenPcmToId3Tagging,

    -- * Offering
    Offering,
    offering,
    oResourceSpecification,
    oCurrencyCode,
    oARN,
    oOfferingId,
    oRegion,
    oOfferingType,
    oUsagePrice,
    oFixedPrice,
    oDurationUnits,
    oOfferingDescription,
    oDuration,

    -- * Output
    Output,
    output,
    oCaptionDescriptionNames,
    oVideoDescriptionName,
    oOutputName,
    oAudioDescriptionNames,
    oOutputSettings,

    -- * OutputDestination
    OutputDestination,
    outputDestination,
    odSettings,
    odMediaPackageSettings,
    odId,
    odMultiplexSettings,

    -- * OutputDestinationSettings
    OutputDestinationSettings,
    outputDestinationSettings,
    odsURL,
    odsUsername,
    odsPasswordParam,
    odsStreamName,

    -- * OutputGroup
    OutputGroup,
    outputGroup,
    ogName,
    ogOutputs,
    ogOutputGroupSettings,

    -- * OutputGroupSettings
    OutputGroupSettings,
    outputGroupSettings,
    ogsMediaPackageGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsRtmpGroupSettings,
    ogsMultiplexGroupSettings,
    ogsHlsGroupSettings,
    ogsArchiveGroupSettings,
    ogsUdpGroupSettings,
    ogsFrameCaptureGroupSettings,

    -- * OutputLocationRef
    OutputLocationRef,
    outputLocationRef,
    olrDestinationRefId,

    -- * OutputSettings
    OutputSettings,
    outputSettings,
    osMultiplexOutputSettings,
    osArchiveOutputSettings,
    osRtmpOutputSettings,
    osMediaPackageOutputSettings,
    osHlsOutputSettings,
    osFrameCaptureOutputSettings,
    osUdpOutputSettings,
    osMsSmoothOutputSettings,

    -- * PassThroughSettings
    PassThroughSettings,
    passThroughSettings,

    -- * PauseStateScheduleActionSettings
    PauseStateScheduleActionSettings,
    pauseStateScheduleActionSettings,
    pssasPipelines,

    -- * PipelineDetail
    PipelineDetail,
    pipelineDetail,
    pdPipelineId,
    pdActiveInputSwitchActionName,
    pdActiveInputAttachmentName,

    -- * PipelinePauseStateSettings
    PipelinePauseStateSettings,
    pipelinePauseStateSettings,
    ppssPipelineId,

    -- * RawSettings
    RawSettings,
    rawSettings,

    -- * Rec601Settings
    Rec601Settings,
    rec601Settings,

    -- * Rec709Settings
    Rec709Settings,
    rec709Settings,

    -- * RemixSettings
    RemixSettings,
    remixSettings,
    rsChannelsIn,
    rsChannelsOut,
    rsChannelMappings,

    -- * Reservation
    Reservation,
    reservation,
    rState,
    rResourceSpecification,
    rCurrencyCode,
    rARN,
    rStart,
    rCount,
    rEnd,
    rName,
    rReservationId,
    rOfferingId,
    rRegion,
    rOfferingType,
    rUsagePrice,
    rFixedPrice,
    rDurationUnits,
    rOfferingDescription,
    rDuration,
    rTags,

    -- * ReservationResourceSpecification
    ReservationResourceSpecification,
    reservationResourceSpecification,
    rrsVideoQuality,
    rrsMaximumFramerate,
    rrsResourceType,
    rrsResolution,
    rrsCodec,
    rrsSpecialFeature,
    rrsChannelClass,
    rrsMaximumBitrate,

    -- * RtmpCaptionInfoDestinationSettings
    RtmpCaptionInfoDestinationSettings,
    rtmpCaptionInfoDestinationSettings,

    -- * RtmpGroupSettings
    RtmpGroupSettings,
    rtmpGroupSettings,
    rgsInputLossAction,
    rgsCaptionData,
    rgsAdMarkers,
    rgsRestartDelay,
    rgsAuthenticationScheme,
    rgsCacheLength,
    rgsCacheFullBehavior,

    -- * RtmpOutputSettings
    RtmpOutputSettings,
    rtmpOutputSettings,
    rosNumRetries,
    rosCertificateMode,
    rosConnectionRetryInterval,
    rosDestination,

    -- * ScheduleAction
    ScheduleAction,
    scheduleAction,
    saActionName,
    saScheduleActionStartSettings,
    saScheduleActionSettings,

    -- * ScheduleActionSettings
    ScheduleActionSettings,
    scheduleActionSettings,
    sasStaticImageDeactivateSettings,
    sasScte35SpliceInsertSettings,
    sasStaticImageActivateSettings,
    sasScte35TimeSignalSettings,
    sasInputPrepareSettings,
    sasHlsId3SegmentTaggingSettings,
    sasScte35ReturnToNetworkSettings,
    sasPauseStateSettings,
    sasHlsTimedMetadataSettings,
    sasInputSwitchSettings,

    -- * ScheduleActionStartSettings
    ScheduleActionStartSettings,
    scheduleActionStartSettings,
    sassImmediateModeScheduleActionStartSettings,
    sassFollowModeScheduleActionStartSettings,
    sassFixedModeScheduleActionStartSettings,

    -- * Scte20PlusEmbeddedDestinationSettings
    Scte20PlusEmbeddedDestinationSettings,
    scte20PlusEmbeddedDestinationSettings,

    -- * Scte20SourceSettings
    Scte20SourceSettings,
    scte20SourceSettings,
    sssConvert608To708,
    sssSource608ChannelNumber,

    -- * Scte27DestinationSettings
    Scte27DestinationSettings,
    scte27DestinationSettings,

    -- * Scte27SourceSettings
    Scte27SourceSettings,
    scte27SourceSettings,
    sssPid,

    -- * Scte35DeliveryRestrictions
    Scte35DeliveryRestrictions,
    scte35DeliveryRestrictions,
    sdrDeviceRestrictions,
    sdrArchiveAllowedFlag,
    sdrWebDeliveryAllowedFlag,
    sdrNoRegionalBlackoutFlag,

    -- * Scte35Descriptor
    Scte35Descriptor,
    scte35Descriptor,
    sdScte35DescriptorSettings,

    -- * Scte35DescriptorSettings
    Scte35DescriptorSettings,
    scte35DescriptorSettings,
    sdsSegmentationDescriptorScte35DescriptorSettings,

    -- * Scte35ReturnToNetworkScheduleActionSettings
    Scte35ReturnToNetworkScheduleActionSettings,
    scte35ReturnToNetworkScheduleActionSettings,
    srtnsasSpliceEventId,

    -- * Scte35SegmentationDescriptor
    Scte35SegmentationDescriptor,
    scte35SegmentationDescriptor,
    ssdSegmentationUpidType,
    ssdSegmentsExpected,
    ssdSubSegmentsExpected,
    ssdSegmentNum,
    ssdSegmentationDuration,
    ssdSegmentationTypeId,
    ssdDeliveryRestrictions,
    ssdSegmentationUpid,
    ssdSubSegmentNum,
    ssdSegmentationEventId,
    ssdSegmentationCancelIndicator,

    -- * Scte35SpliceInsert
    Scte35SpliceInsert,
    scte35SpliceInsert,
    ssiWebDeliveryAllowedFlag,
    ssiAdAvailOffset,
    ssiNoRegionalBlackoutFlag,

    -- * Scte35SpliceInsertScheduleActionSettings
    Scte35SpliceInsertScheduleActionSettings,
    scte35SpliceInsertScheduleActionSettings,
    ssisasDuration,
    ssisasSpliceEventId,

    -- * Scte35TimeSignalApos
    Scte35TimeSignalApos,
    scte35TimeSignalApos,
    stsaWebDeliveryAllowedFlag,
    stsaAdAvailOffset,
    stsaNoRegionalBlackoutFlag,

    -- * Scte35TimeSignalScheduleActionSettings
    Scte35TimeSignalScheduleActionSettings,
    scte35TimeSignalScheduleActionSettings,
    stssasScte35Descriptors,

    -- * SmpteTtDestinationSettings
    SmpteTtDestinationSettings,
    smpteTtDestinationSettings,

    -- * StandardHlsSettings
    StandardHlsSettings,
    standardHlsSettings,
    shsAudioRenditionSets,
    shsM3u8Settings,

    -- * StartTimecode
    StartTimecode,
    startTimecode,
    sTimecode,

    -- * StaticImageActivateScheduleActionSettings
    StaticImageActivateScheduleActionSettings,
    staticImageActivateScheduleActionSettings,
    siasasImageX,
    siasasHeight,
    siasasFadeOut,
    siasasWidth,
    siasasOpacity,
    siasasLayer,
    siasasDuration,
    siasasImageY,
    siasasFadeIn,
    siasasImage,

    -- * StaticImageDeactivateScheduleActionSettings
    StaticImageDeactivateScheduleActionSettings,
    staticImageDeactivateScheduleActionSettings,
    sidsasFadeOut,
    sidsasLayer,

    -- * StaticKeySettings
    StaticKeySettings,
    staticKeySettings,
    sksKeyProviderServer,
    sksStaticKeyValue,

    -- * StopTimecode
    StopTimecode,
    stopTimecode,
    stLastFrameClippingBehavior,
    stTimecode,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings,
    teletextDestinationSettings,

    -- * TeletextSourceSettings
    TeletextSourceSettings,
    teletextSourceSettings,
    tssPageNumber,

    -- * TemporalFilterSettings
    TemporalFilterSettings,
    temporalFilterSettings,
    tfsStrength,
    tfsPostFilterSharpening,

    -- * TimecodeConfig
    TimecodeConfig,
    timecodeConfig,
    tcSyncThreshold,
    tcSource,

    -- * TransferringInputDeviceSummary
    TransferringInputDeviceSummary,
    transferringInputDeviceSummary,
    tidsTransferType,
    tidsId,
    tidsTargetCustomerId,
    tidsMessage,

    -- * TtmlDestinationSettings
    TtmlDestinationSettings,
    ttmlDestinationSettings,
    tdsStyleControl,

    -- * UdpContainerSettings
    UdpContainerSettings,
    udpContainerSettings,
    ucsM2tsSettings,

    -- * UdpGroupSettings
    UdpGroupSettings,
    udpGroupSettings,
    ugsTimedMetadataId3Period,
    ugsInputLossAction,
    ugsTimedMetadataId3Frame,

    -- * UdpOutputSettings
    UdpOutputSettings,
    udpOutputSettings,
    uosFecOutputSettings,
    uosBufferMsec,
    uosDestination,
    uosContainerSettings,

    -- * VideoCodecSettings
    VideoCodecSettings,
    videoCodecSettings,
    vcsFrameCaptureSettings,
    vcsH265Settings,
    vcsH264Settings,
    vcsMpeg2Settings,

    -- * VideoDescription
    VideoDescription,
    videoDescription,
    vdHeight,
    vdSharpness,
    vdWidth,
    vdScalingBehavior,
    vdRespondToAfd,
    vdCodecSettings,
    vdName,

    -- * VideoSelector
    VideoSelector,
    videoSelector,
    vsSelectorSettings,
    vsColorSpaceUsage,
    vsColorSpace,

    -- * VideoSelectorPid
    VideoSelectorPid,
    videoSelectorPid,
    vspPid,

    -- * VideoSelectorProgramId
    VideoSelectorProgramId,
    videoSelectorProgramId,
    vspiProgramId,

    -- * VideoSelectorSettings
    VideoSelectorSettings,
    videoSelectorSettings,
    vssVideoSelectorProgramId,
    vssVideoSelectorPid,

    -- * WavSettings
    WavSettings,
    wavSettings,
    wsBitDepth,
    wsCodingMode,
    wsSampleRate,

    -- * WebvttDestinationSettings
    WebvttDestinationSettings,
    webvttDestinationSettings,
  )
where

import Network.AWS.Lens
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
import Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode
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
import Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode
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
import Network.AWS.MediaLive.Types.InputDestinationVPC
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceIPScheme
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceRequest
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputDeviceState
import Network.AWS.MediaLive.Types.InputDeviceSummary
import Network.AWS.MediaLive.Types.InputDeviceTransferType
import Network.AWS.MediaLive.Types.InputDeviceType
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
import Network.AWS.MediaLive.Types.InputVPCRequest
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
import Network.AWS.MediaLive.Types.M2tsCCDescriptor
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
import Network.AWS.MediaLive.Types.WavCodingMode
import Network.AWS.MediaLive.Types.WavSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-14@ of the Amazon Elemental MediaLive SDK configuration.
mediaLive :: Service
mediaLive =
  Service
    { _svcAbbrev = "MediaLive",
      _svcSigner = v4,
      _svcPrefix = "medialive",
      _svcVersion = "2017-10-14",
      _svcEndpoint = defaultEndpoint mediaLive,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "MediaLive",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
