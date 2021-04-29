{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
    _TooManyRequestsException,

    -- * AacAudioDescriptionBroadcasterMix
    AacAudioDescriptionBroadcasterMix (..),

    -- * AacCodecProfile
    AacCodecProfile (..),

    -- * AacCodingMode
    AacCodingMode (..),

    -- * AacRateControlMode
    AacRateControlMode (..),

    -- * AacRawFormat
    AacRawFormat (..),

    -- * AacSpecification
    AacSpecification (..),

    -- * AacVbrQuality
    AacVbrQuality (..),

    -- * Ac3BitstreamMode
    Ac3BitstreamMode (..),

    -- * Ac3CodingMode
    Ac3CodingMode (..),

    -- * Ac3DynamicRangeCompressionProfile
    Ac3DynamicRangeCompressionProfile (..),

    -- * Ac3LfeFilter
    Ac3LfeFilter (..),

    -- * Ac3MetadataControl
    Ac3MetadataControl (..),

    -- * AccelerationMode
    AccelerationMode (..),

    -- * AccelerationStatus
    AccelerationStatus (..),

    -- * AfdSignaling
    AfdSignaling (..),

    -- * AlphaBehavior
    AlphaBehavior (..),

    -- * AncillaryConvert608To708
    AncillaryConvert608To708 (..),

    -- * AncillaryTerminateCaptions
    AncillaryTerminateCaptions (..),

    -- * AntiAlias
    AntiAlias (..),

    -- * AudioChannelTag
    AudioChannelTag (..),

    -- * AudioCodec
    AudioCodec (..),

    -- * AudioDefaultSelection
    AudioDefaultSelection (..),

    -- * AudioLanguageCodeControl
    AudioLanguageCodeControl (..),

    -- * AudioNormalizationAlgorithm
    AudioNormalizationAlgorithm (..),

    -- * AudioNormalizationAlgorithmControl
    AudioNormalizationAlgorithmControl (..),

    -- * AudioNormalizationLoudnessLogging
    AudioNormalizationLoudnessLogging (..),

    -- * AudioNormalizationPeakCalculation
    AudioNormalizationPeakCalculation (..),

    -- * AudioSelectorType
    AudioSelectorType (..),

    -- * AudioTypeControl
    AudioTypeControl (..),

    -- * Av1AdaptiveQuantization
    Av1AdaptiveQuantization (..),

    -- * Av1FramerateControl
    Av1FramerateControl (..),

    -- * Av1FramerateConversionAlgorithm
    Av1FramerateConversionAlgorithm (..),

    -- * Av1RateControlMode
    Av1RateControlMode (..),

    -- * Av1SpatialAdaptiveQuantization
    Av1SpatialAdaptiveQuantization (..),

    -- * AvcIntraClass
    AvcIntraClass (..),

    -- * AvcIntraFramerateControl
    AvcIntraFramerateControl (..),

    -- * AvcIntraFramerateConversionAlgorithm
    AvcIntraFramerateConversionAlgorithm (..),

    -- * AvcIntraInterlaceMode
    AvcIntraInterlaceMode (..),

    -- * AvcIntraScanTypeConversionMode
    AvcIntraScanTypeConversionMode (..),

    -- * AvcIntraSlowPal
    AvcIntraSlowPal (..),

    -- * AvcIntraTelecine
    AvcIntraTelecine (..),

    -- * AvcIntraUhdQualityTuningLevel
    AvcIntraUhdQualityTuningLevel (..),

    -- * BillingTagsSource
    BillingTagsSource (..),

    -- * BurninSubtitleAlignment
    BurninSubtitleAlignment (..),

    -- * BurninSubtitleBackgroundColor
    BurninSubtitleBackgroundColor (..),

    -- * BurninSubtitleFontColor
    BurninSubtitleFontColor (..),

    -- * BurninSubtitleOutlineColor
    BurninSubtitleOutlineColor (..),

    -- * BurninSubtitleShadowColor
    BurninSubtitleShadowColor (..),

    -- * BurninSubtitleTeletextSpacing
    BurninSubtitleTeletextSpacing (..),

    -- * CaptionDestinationType
    CaptionDestinationType (..),

    -- * CaptionSourceType
    CaptionSourceType (..),

    -- * CmafClientCache
    CmafClientCache (..),

    -- * CmafCodecSpecification
    CmafCodecSpecification (..),

    -- * CmafEncryptionType
    CmafEncryptionType (..),

    -- * CmafInitializationVectorInManifest
    CmafInitializationVectorInManifest (..),

    -- * CmafKeyProviderType
    CmafKeyProviderType (..),

    -- * CmafManifestCompression
    CmafManifestCompression (..),

    -- * CmafManifestDurationFormat
    CmafManifestDurationFormat (..),

    -- * CmafMpdProfile
    CmafMpdProfile (..),

    -- * CmafSegmentControl
    CmafSegmentControl (..),

    -- * CmafStreamInfResolution
    CmafStreamInfResolution (..),

    -- * CmafWriteDASHManifest
    CmafWriteDASHManifest (..),

    -- * CmafWriteHLSManifest
    CmafWriteHLSManifest (..),

    -- * CmafWriteSegmentTimelineInRepresentation
    CmafWriteSegmentTimelineInRepresentation (..),

    -- * CmfcAudioDuration
    CmfcAudioDuration (..),

    -- * CmfcIFrameOnlyManifest
    CmfcIFrameOnlyManifest (..),

    -- * CmfcScte35Esam
    CmfcScte35Esam (..),

    -- * CmfcScte35Source
    CmfcScte35Source (..),

    -- * ColorMetadata
    ColorMetadata (..),

    -- * ColorSpace
    ColorSpace (..),

    -- * ColorSpaceConversion
    ColorSpaceConversion (..),

    -- * ColorSpaceUsage
    ColorSpaceUsage (..),

    -- * Commitment
    Commitment (..),

    -- * ContainerType
    ContainerType (..),

    -- * DashIsoHbbtvCompliance
    DashIsoHbbtvCompliance (..),

    -- * DashIsoMpdProfile
    DashIsoMpdProfile (..),

    -- * DashIsoPlaybackDeviceCompatibility
    DashIsoPlaybackDeviceCompatibility (..),

    -- * DashIsoSegmentControl
    DashIsoSegmentControl (..),

    -- * DashIsoWriteSegmentTimelineInRepresentation
    DashIsoWriteSegmentTimelineInRepresentation (..),

    -- * DecryptionMode
    DecryptionMode (..),

    -- * DeinterlaceAlgorithm
    DeinterlaceAlgorithm (..),

    -- * DeinterlacerControl
    DeinterlacerControl (..),

    -- * DeinterlacerMode
    DeinterlacerMode (..),

    -- * DescribeEndpointsMode
    DescribeEndpointsMode (..),

    -- * DolbyVisionLevel6Mode
    DolbyVisionLevel6Mode (..),

    -- * DolbyVisionProfile
    DolbyVisionProfile (..),

    -- * DropFrameTimecode
    DropFrameTimecode (..),

    -- * DvbSubtitleAlignment
    DvbSubtitleAlignment (..),

    -- * DvbSubtitleBackgroundColor
    DvbSubtitleBackgroundColor (..),

    -- * DvbSubtitleFontColor
    DvbSubtitleFontColor (..),

    -- * DvbSubtitleOutlineColor
    DvbSubtitleOutlineColor (..),

    -- * DvbSubtitleShadowColor
    DvbSubtitleShadowColor (..),

    -- * DvbSubtitleTeletextSpacing
    DvbSubtitleTeletextSpacing (..),

    -- * DvbSubtitlingType
    DvbSubtitlingType (..),

    -- * Eac3AtmosBitstreamMode
    Eac3AtmosBitstreamMode (..),

    -- * Eac3AtmosCodingMode
    Eac3AtmosCodingMode (..),

    -- * Eac3AtmosDialogueIntelligence
    Eac3AtmosDialogueIntelligence (..),

    -- * Eac3AtmosDynamicRangeCompressionLine
    Eac3AtmosDynamicRangeCompressionLine (..),

    -- * Eac3AtmosDynamicRangeCompressionRf
    Eac3AtmosDynamicRangeCompressionRf (..),

    -- * Eac3AtmosMeteringMode
    Eac3AtmosMeteringMode (..),

    -- * Eac3AtmosStereoDownmix
    Eac3AtmosStereoDownmix (..),

    -- * Eac3AtmosSurroundExMode
    Eac3AtmosSurroundExMode (..),

    -- * Eac3AttenuationControl
    Eac3AttenuationControl (..),

    -- * Eac3BitstreamMode
    Eac3BitstreamMode (..),

    -- * Eac3CodingMode
    Eac3CodingMode (..),

    -- * Eac3DcFilter
    Eac3DcFilter (..),

    -- * Eac3DynamicRangeCompressionLine
    Eac3DynamicRangeCompressionLine (..),

    -- * Eac3DynamicRangeCompressionRf
    Eac3DynamicRangeCompressionRf (..),

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

    -- * EmbeddedConvert608To708
    EmbeddedConvert608To708 (..),

    -- * EmbeddedTerminateCaptions
    EmbeddedTerminateCaptions (..),

    -- * F4vMoovPlacement
    F4vMoovPlacement (..),

    -- * FileSourceConvert608To708
    FileSourceConvert608To708 (..),

    -- * FontScript
    FontScript (..),

    -- * H264AdaptiveQuantization
    H264AdaptiveQuantization (..),

    -- * H264CodecLevel
    H264CodecLevel (..),

    -- * H264CodecProfile
    H264CodecProfile (..),

    -- * H264DynamicSubGop
    H264DynamicSubGop (..),

    -- * H264EntropyEncoding
    H264EntropyEncoding (..),

    -- * H264FieldEncoding
    H264FieldEncoding (..),

    -- * H264FlickerAdaptiveQuantization
    H264FlickerAdaptiveQuantization (..),

    -- * H264FramerateControl
    H264FramerateControl (..),

    -- * H264FramerateConversionAlgorithm
    H264FramerateConversionAlgorithm (..),

    -- * H264GopBReference
    H264GopBReference (..),

    -- * H264GopSizeUnits
    H264GopSizeUnits (..),

    -- * H264InterlaceMode
    H264InterlaceMode (..),

    -- * H264ParControl
    H264ParControl (..),

    -- * H264QualityTuningLevel
    H264QualityTuningLevel (..),

    -- * H264RateControlMode
    H264RateControlMode (..),

    -- * H264RepeatPps
    H264RepeatPps (..),

    -- * H264ScanTypeConversionMode
    H264ScanTypeConversionMode (..),

    -- * H264SceneChangeDetect
    H264SceneChangeDetect (..),

    -- * H264SlowPal
    H264SlowPal (..),

    -- * H264SpatialAdaptiveQuantization
    H264SpatialAdaptiveQuantization (..),

    -- * H264Syntax
    H264Syntax (..),

    -- * H264Telecine
    H264Telecine (..),

    -- * H264TemporalAdaptiveQuantization
    H264TemporalAdaptiveQuantization (..),

    -- * H264UnregisteredSeiTimecode
    H264UnregisteredSeiTimecode (..),

    -- * H265AdaptiveQuantization
    H265AdaptiveQuantization (..),

    -- * H265AlternateTransferFunctionSei
    H265AlternateTransferFunctionSei (..),

    -- * H265CodecLevel
    H265CodecLevel (..),

    -- * H265CodecProfile
    H265CodecProfile (..),

    -- * H265DynamicSubGop
    H265DynamicSubGop (..),

    -- * H265FlickerAdaptiveQuantization
    H265FlickerAdaptiveQuantization (..),

    -- * H265FramerateControl
    H265FramerateControl (..),

    -- * H265FramerateConversionAlgorithm
    H265FramerateConversionAlgorithm (..),

    -- * H265GopBReference
    H265GopBReference (..),

    -- * H265GopSizeUnits
    H265GopSizeUnits (..),

    -- * H265InterlaceMode
    H265InterlaceMode (..),

    -- * H265ParControl
    H265ParControl (..),

    -- * H265QualityTuningLevel
    H265QualityTuningLevel (..),

    -- * H265RateControlMode
    H265RateControlMode (..),

    -- * H265SampleAdaptiveOffsetFilterMode
    H265SampleAdaptiveOffsetFilterMode (..),

    -- * H265ScanTypeConversionMode
    H265ScanTypeConversionMode (..),

    -- * H265SceneChangeDetect
    H265SceneChangeDetect (..),

    -- * H265SlowPal
    H265SlowPal (..),

    -- * H265SpatialAdaptiveQuantization
    H265SpatialAdaptiveQuantization (..),

    -- * H265Telecine
    H265Telecine (..),

    -- * H265TemporalAdaptiveQuantization
    H265TemporalAdaptiveQuantization (..),

    -- * H265TemporalIds
    H265TemporalIds (..),

    -- * H265Tiles
    H265Tiles (..),

    -- * H265UnregisteredSeiTimecode
    H265UnregisteredSeiTimecode (..),

    -- * H265WriteMp4PackagingType
    H265WriteMp4PackagingType (..),

    -- * HlsAdMarkers
    HlsAdMarkers (..),

    -- * HlsAudioOnlyContainer
    HlsAudioOnlyContainer (..),

    -- * HlsAudioOnlyHeader
    HlsAudioOnlyHeader (..),

    -- * HlsAudioTrackType
    HlsAudioTrackType (..),

    -- * HlsCaptionLanguageSetting
    HlsCaptionLanguageSetting (..),

    -- * HlsClientCache
    HlsClientCache (..),

    -- * HlsCodecSpecification
    HlsCodecSpecification (..),

    -- * HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- * HlsEncryptionType
    HlsEncryptionType (..),

    -- * HlsIFrameOnlyManifest
    HlsIFrameOnlyManifest (..),

    -- * HlsInitializationVectorInManifest
    HlsInitializationVectorInManifest (..),

    -- * HlsKeyProviderType
    HlsKeyProviderType (..),

    -- * HlsManifestCompression
    HlsManifestCompression (..),

    -- * HlsManifestDurationFormat
    HlsManifestDurationFormat (..),

    -- * HlsOfflineEncrypted
    HlsOfflineEncrypted (..),

    -- * HlsOutputSelection
    HlsOutputSelection (..),

    -- * HlsProgramDateTime
    HlsProgramDateTime (..),

    -- * HlsSegmentControl
    HlsSegmentControl (..),

    -- * HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- * HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- * ImscStylePassthrough
    ImscStylePassthrough (..),

    -- * InputDeblockFilter
    InputDeblockFilter (..),

    -- * InputDenoiseFilter
    InputDenoiseFilter (..),

    -- * InputFilterEnable
    InputFilterEnable (..),

    -- * InputPsiControl
    InputPsiControl (..),

    -- * InputRotate
    InputRotate (..),

    -- * InputScanType
    InputScanType (..),

    -- * InputTimecodeSource
    InputTimecodeSource (..),

    -- * JobPhase
    JobPhase (..),

    -- * JobStatus
    JobStatus (..),

    -- * JobTemplateListBy
    JobTemplateListBy (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * M2tsAudioBufferModel
    M2tsAudioBufferModel (..),

    -- * M2tsAudioDuration
    M2tsAudioDuration (..),

    -- * M2tsBufferModel
    M2tsBufferModel (..),

    -- * M2tsEbpAudioInterval
    M2tsEbpAudioInterval (..),

    -- * M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- * M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- * M2tsForceTsVideoEbpOrder
    M2tsForceTsVideoEbpOrder (..),

    -- * M2tsNielsenId3
    M2tsNielsenId3 (..),

    -- * M2tsPcrControl
    M2tsPcrControl (..),

    -- * M2tsRateMode
    M2tsRateMode (..),

    -- * M2tsScte35Source
    M2tsScte35Source (..),

    -- * M2tsSegmentationMarkers
    M2tsSegmentationMarkers (..),

    -- * M2tsSegmentationStyle
    M2tsSegmentationStyle (..),

    -- * M3u8AudioDuration
    M3u8AudioDuration (..),

    -- * M3u8NielsenId3
    M3u8NielsenId3 (..),

    -- * M3u8PcrControl
    M3u8PcrControl (..),

    -- * M3u8Scte35Source
    M3u8Scte35Source (..),

    -- * MotionImageInsertionMode
    MotionImageInsertionMode (..),

    -- * MotionImagePlayback
    MotionImagePlayback (..),

    -- * MovClapAtom
    MovClapAtom (..),

    -- * MovCslgAtom
    MovCslgAtom (..),

    -- * MovMpeg2FourCCControl
    MovMpeg2FourCCControl (..),

    -- * MovPaddingControl
    MovPaddingControl (..),

    -- * MovReference
    MovReference (..),

    -- * Mp3RateControlMode
    Mp3RateControlMode (..),

    -- * Mp4CslgAtom
    Mp4CslgAtom (..),

    -- * Mp4FreeSpaceBox
    Mp4FreeSpaceBox (..),

    -- * Mp4MoovPlacement
    Mp4MoovPlacement (..),

    -- * MpdAccessibilityCaptionHints
    MpdAccessibilityCaptionHints (..),

    -- * MpdAudioDuration
    MpdAudioDuration (..),

    -- * MpdCaptionContainerType
    MpdCaptionContainerType (..),

    -- * MpdScte35Esam
    MpdScte35Esam (..),

    -- * MpdScte35Source
    MpdScte35Source (..),

    -- * Mpeg2AdaptiveQuantization
    Mpeg2AdaptiveQuantization (..),

    -- * Mpeg2CodecLevel
    Mpeg2CodecLevel (..),

    -- * Mpeg2CodecProfile
    Mpeg2CodecProfile (..),

    -- * Mpeg2DynamicSubGop
    Mpeg2DynamicSubGop (..),

    -- * Mpeg2FramerateControl
    Mpeg2FramerateControl (..),

    -- * Mpeg2FramerateConversionAlgorithm
    Mpeg2FramerateConversionAlgorithm (..),

    -- * Mpeg2GopSizeUnits
    Mpeg2GopSizeUnits (..),

    -- * Mpeg2InterlaceMode
    Mpeg2InterlaceMode (..),

    -- * Mpeg2IntraDcPrecision
    Mpeg2IntraDcPrecision (..),

    -- * Mpeg2ParControl
    Mpeg2ParControl (..),

    -- * Mpeg2QualityTuningLevel
    Mpeg2QualityTuningLevel (..),

    -- * Mpeg2RateControlMode
    Mpeg2RateControlMode (..),

    -- * Mpeg2ScanTypeConversionMode
    Mpeg2ScanTypeConversionMode (..),

    -- * Mpeg2SceneChangeDetect
    Mpeg2SceneChangeDetect (..),

    -- * Mpeg2SlowPal
    Mpeg2SlowPal (..),

    -- * Mpeg2SpatialAdaptiveQuantization
    Mpeg2SpatialAdaptiveQuantization (..),

    -- * Mpeg2Syntax
    Mpeg2Syntax (..),

    -- * Mpeg2Telecine
    Mpeg2Telecine (..),

    -- * Mpeg2TemporalAdaptiveQuantization
    Mpeg2TemporalAdaptiveQuantization (..),

    -- * MsSmoothAudioDeduplication
    MsSmoothAudioDeduplication (..),

    -- * MsSmoothManifestEncoding
    MsSmoothManifestEncoding (..),

    -- * MxfAfdSignaling
    MxfAfdSignaling (..),

    -- * MxfProfile
    MxfProfile (..),

    -- * NielsenActiveWatermarkProcessType
    NielsenActiveWatermarkProcessType (..),

    -- * NielsenSourceWatermarkStatusType
    NielsenSourceWatermarkStatusType (..),

    -- * NielsenUniqueTicPerAudioTrackType
    NielsenUniqueTicPerAudioTrackType (..),

    -- * NoiseFilterPostTemporalSharpening
    NoiseFilterPostTemporalSharpening (..),

    -- * NoiseReducerFilter
    NoiseReducerFilter (..),

    -- * Order
    Order (..),

    -- * OutputGroupType
    OutputGroupType (..),

    -- * OutputSdt
    OutputSdt (..),

    -- * PresetListBy
    PresetListBy (..),

    -- * PricingPlan
    PricingPlan (..),

    -- * ProresCodecProfile
    ProresCodecProfile (..),

    -- * ProresFramerateControl
    ProresFramerateControl (..),

    -- * ProresFramerateConversionAlgorithm
    ProresFramerateConversionAlgorithm (..),

    -- * ProresInterlaceMode
    ProresInterlaceMode (..),

    -- * ProresParControl
    ProresParControl (..),

    -- * ProresScanTypeConversionMode
    ProresScanTypeConversionMode (..),

    -- * ProresSlowPal
    ProresSlowPal (..),

    -- * ProresTelecine
    ProresTelecine (..),

    -- * QueueListBy
    QueueListBy (..),

    -- * QueueStatus
    QueueStatus (..),

    -- * RenewalType
    RenewalType (..),

    -- * ReservationPlanStatus
    ReservationPlanStatus (..),

    -- * RespondToAfd
    RespondToAfd (..),

    -- * S3ObjectCannedAcl
    S3ObjectCannedAcl (..),

    -- * S3ServerSideEncryptionType
    S3ServerSideEncryptionType (..),

    -- * ScalingBehavior
    ScalingBehavior (..),

    -- * SccDestinationFramerate
    SccDestinationFramerate (..),

    -- * SimulateReservedQueue
    SimulateReservedQueue (..),

    -- * StatusUpdateInterval
    StatusUpdateInterval (..),

    -- * TeletextPageType
    TeletextPageType (..),

    -- * TimecodeBurninPosition
    TimecodeBurninPosition (..),

    -- * TimecodeSource
    TimecodeSource (..),

    -- * TimedMetadata
    TimedMetadata (..),

    -- * TtmlStylePassthrough
    TtmlStylePassthrough (..),

    -- * Type
    Type (..),

    -- * Vc3Class
    Vc3Class (..),

    -- * Vc3FramerateControl
    Vc3FramerateControl (..),

    -- * Vc3FramerateConversionAlgorithm
    Vc3FramerateConversionAlgorithm (..),

    -- * Vc3InterlaceMode
    Vc3InterlaceMode (..),

    -- * Vc3ScanTypeConversionMode
    Vc3ScanTypeConversionMode (..),

    -- * Vc3SlowPal
    Vc3SlowPal (..),

    -- * Vc3Telecine
    Vc3Telecine (..),

    -- * VideoCodec
    VideoCodec (..),

    -- * VideoTimecodeInsertion
    VideoTimecodeInsertion (..),

    -- * Vp8FramerateControl
    Vp8FramerateControl (..),

    -- * Vp8FramerateConversionAlgorithm
    Vp8FramerateConversionAlgorithm (..),

    -- * Vp8ParControl
    Vp8ParControl (..),

    -- * Vp8QualityTuningLevel
    Vp8QualityTuningLevel (..),

    -- * Vp8RateControlMode
    Vp8RateControlMode (..),

    -- * Vp9FramerateControl
    Vp9FramerateControl (..),

    -- * Vp9FramerateConversionAlgorithm
    Vp9FramerateConversionAlgorithm (..),

    -- * Vp9ParControl
    Vp9ParControl (..),

    -- * Vp9QualityTuningLevel
    Vp9QualityTuningLevel (..),

    -- * Vp9RateControlMode
    Vp9RateControlMode (..),

    -- * WatermarkingStrength
    WatermarkingStrength (..),

    -- * WavFormat
    WavFormat (..),

    -- * AacSettings
    AacSettings (..),
    newAacSettings,
    aacSettings_audioDescriptionBroadcasterMix,
    aacSettings_rateControlMode,
    aacSettings_codingMode,
    aacSettings_codecProfile,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_vbrQuality,
    aacSettings_bitrate,
    aacSettings_specification,

    -- * Ac3Settings
    Ac3Settings (..),
    newAc3Settings,
    ac3Settings_dialnorm,
    ac3Settings_codingMode,
    ac3Settings_lfeFilter,
    ac3Settings_dynamicRangeCompressionProfile,
    ac3Settings_sampleRate,
    ac3Settings_bitstreamMode,
    ac3Settings_bitrate,
    ac3Settings_metadataControl,

    -- * AccelerationSettings
    AccelerationSettings (..),
    newAccelerationSettings,
    accelerationSettings_mode,

    -- * AiffSettings
    AiffSettings (..),
    newAiffSettings,
    aiffSettings_channels,
    aiffSettings_bitDepth,
    aiffSettings_sampleRate,

    -- * AncillarySourceSettings
    AncillarySourceSettings (..),
    newAncillarySourceSettings,
    ancillarySourceSettings_terminateCaptions,
    ancillarySourceSettings_convert608To708,
    ancillarySourceSettings_sourceAncillaryChannelNumber,

    -- * AudioChannelTaggingSettings
    AudioChannelTaggingSettings (..),
    newAudioChannelTaggingSettings,
    audioChannelTaggingSettings_channelTag,

    -- * AudioCodecSettings
    AudioCodecSettings (..),
    newAudioCodecSettings,
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

    -- * AudioDescription
    AudioDescription (..),
    newAudioDescription,
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

    -- * AudioNormalizationSettings
    AudioNormalizationSettings (..),
    newAudioNormalizationSettings,
    audioNormalizationSettings_correctionGateLevel,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_peakCalculation,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_loudnessLogging,

    -- * AudioSelector
    AudioSelector (..),
    newAudioSelector,
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

    -- * AudioSelectorGroup
    AudioSelectorGroup (..),
    newAudioSelectorGroup,
    audioSelectorGroup_audioSelectorNames,

    -- * AutomatedAbrSettings
    AutomatedAbrSettings (..),
    newAutomatedAbrSettings,
    automatedAbrSettings_minAbrBitrate,
    automatedAbrSettings_maxRenditions,
    automatedAbrSettings_maxAbrBitrate,

    -- * AutomatedEncodingSettings
    AutomatedEncodingSettings (..),
    newAutomatedEncodingSettings,
    automatedEncodingSettings_abrSettings,

    -- * Av1QvbrSettings
    Av1QvbrSettings (..),
    newAv1QvbrSettings,
    av1QvbrSettings_qvbrQualityLevelFineTune,
    av1QvbrSettings_qvbrQualityLevel,

    -- * Av1Settings
    Av1Settings (..),
    newAv1Settings,
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

    -- * AvailBlanking
    AvailBlanking (..),
    newAvailBlanking,
    availBlanking_availBlankingImage,

    -- * AvcIntraSettings
    AvcIntraSettings (..),
    newAvcIntraSettings,
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

    -- * AvcIntraUhdSettings
    AvcIntraUhdSettings (..),
    newAvcIntraUhdSettings,
    avcIntraUhdSettings_qualityTuningLevel,

    -- * BurninDestinationSettings
    BurninDestinationSettings (..),
    newBurninDestinationSettings,
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

    -- * CaptionDescription
    CaptionDescription (..),
    newCaptionDescription,
    captionDescription_languageCode,
    captionDescription_languageDescription,
    captionDescription_customLanguageCode,
    captionDescription_captionSelectorName,
    captionDescription_destinationSettings,

    -- * CaptionDescriptionPreset
    CaptionDescriptionPreset (..),
    newCaptionDescriptionPreset,
    captionDescriptionPreset_languageCode,
    captionDescriptionPreset_languageDescription,
    captionDescriptionPreset_customLanguageCode,
    captionDescriptionPreset_destinationSettings,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings (..),
    newCaptionDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_destinationType,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_burninDestinationSettings,
    captionDestinationSettings_imscDestinationSettings,
    captionDestinationSettings_sccDestinationSettings,

    -- * CaptionSelector
    CaptionSelector (..),
    newCaptionSelector,
    captionSelector_languageCode,
    captionSelector_customLanguageCode,
    captionSelector_sourceSettings,

    -- * CaptionSourceFramerate
    CaptionSourceFramerate (..),
    newCaptionSourceFramerate,
    captionSourceFramerate_framerateNumerator,
    captionSourceFramerate_framerateDenominator,

    -- * CaptionSourceSettings
    CaptionSourceSettings (..),
    newCaptionSourceSettings,
    captionSourceSettings_ancillarySourceSettings,
    captionSourceSettings_trackSourceSettings,
    captionSourceSettings_embeddedSourceSettings,
    captionSourceSettings_dvbSubSourceSettings,
    captionSourceSettings_fileSourceSettings,
    captionSourceSettings_teletextSourceSettings,
    captionSourceSettings_sourceType,

    -- * ChannelMapping
    ChannelMapping (..),
    newChannelMapping,
    channelMapping_outputChannels,

    -- * CmafAdditionalManifest
    CmafAdditionalManifest (..),
    newCmafAdditionalManifest,
    cmafAdditionalManifest_manifestNameModifier,
    cmafAdditionalManifest_selectedOutputs,

    -- * CmafEncryptionSettings
    CmafEncryptionSettings (..),
    newCmafEncryptionSettings,
    cmafEncryptionSettings_spekeKeyProvider,
    cmafEncryptionSettings_encryptionMethod,
    cmafEncryptionSettings_constantInitializationVector,
    cmafEncryptionSettings_initializationVectorInManifest,
    cmafEncryptionSettings_staticKeyProvider,
    cmafEncryptionSettings_type,

    -- * CmafGroupSettings
    CmafGroupSettings (..),
    newCmafGroupSettings,
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

    -- * CmfcSettings
    CmfcSettings (..),
    newCmfcSettings,
    cmfcSettings_iFrameOnlyManifest,
    cmfcSettings_audioDuration,
    cmfcSettings_scte35Esam,
    cmfcSettings_scte35Source,

    -- * ColorCorrector
    ColorCorrector (..),
    newColorCorrector,
    colorCorrector_saturation,
    colorCorrector_colorSpaceConversion,
    colorCorrector_hdr10Metadata,
    colorCorrector_brightness,
    colorCorrector_hue,
    colorCorrector_contrast,

    -- * ContainerSettings
    ContainerSettings (..),
    newContainerSettings,
    containerSettings_container,
    containerSettings_mpdSettings,
    containerSettings_mp4Settings,
    containerSettings_f4vSettings,
    containerSettings_mxfSettings,
    containerSettings_movSettings,
    containerSettings_cmfcSettings,
    containerSettings_m3u8Settings,
    containerSettings_m2tsSettings,

    -- * DashAdditionalManifest
    DashAdditionalManifest (..),
    newDashAdditionalManifest,
    dashAdditionalManifest_manifestNameModifier,
    dashAdditionalManifest_selectedOutputs,

    -- * DashIsoEncryptionSettings
    DashIsoEncryptionSettings (..),
    newDashIsoEncryptionSettings,
    dashIsoEncryptionSettings_spekeKeyProvider,
    dashIsoEncryptionSettings_playbackDeviceCompatibility,

    -- * DashIsoGroupSettings
    DashIsoGroupSettings (..),
    newDashIsoGroupSettings,
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

    -- * Deinterlacer
    Deinterlacer (..),
    newDeinterlacer,
    deinterlacer_algorithm,
    deinterlacer_mode,
    deinterlacer_control,

    -- * DestinationSettings
    DestinationSettings (..),
    newDestinationSettings,
    destinationSettings_s3Settings,

    -- * DolbyVision
    DolbyVision (..),
    newDolbyVision,
    dolbyVision_l6Mode,
    dolbyVision_l6Metadata,
    dolbyVision_profile,

    -- * DolbyVisionLevel6Metadata
    DolbyVisionLevel6Metadata (..),
    newDolbyVisionLevel6Metadata,
    dolbyVisionLevel6Metadata_maxCll,
    dolbyVisionLevel6Metadata_maxFall,

    -- * DvbNitSettings
    DvbNitSettings (..),
    newDvbNitSettings,
    dvbNitSettings_nitInterval,
    dvbNitSettings_networkName,
    dvbNitSettings_networkId,

    -- * DvbSdtSettings
    DvbSdtSettings (..),
    newDvbSdtSettings,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_sdtInterval,
    dvbSdtSettings_serviceProviderName,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings (..),
    newDvbSubDestinationSettings,
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

    -- * DvbSubSourceSettings
    DvbSubSourceSettings (..),
    newDvbSubSourceSettings,
    dvbSubSourceSettings_pid,

    -- * DvbTdtSettings
    DvbTdtSettings (..),
    newDvbTdtSettings,
    dvbTdtSettings_tdtInterval,

    -- * Eac3AtmosSettings
    Eac3AtmosSettings (..),
    newEac3AtmosSettings,
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

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    newEmbeddedDestinationSettings,
    embeddedDestinationSettings_destination708ServiceNumber,
    embeddedDestinationSettings_destination608ChannelNumber,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    newEmbeddedSourceSettings,
    embeddedSourceSettings_terminateCaptions,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_source608ChannelNumber,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_url,

    -- * EsamManifestConfirmConditionNotification
    EsamManifestConfirmConditionNotification (..),
    newEsamManifestConfirmConditionNotification,
    esamManifestConfirmConditionNotification_mccXml,

    -- * EsamSettings
    EsamSettings (..),
    newEsamSettings,
    esamSettings_responseSignalPreroll,
    esamSettings_manifestConfirmConditionNotification,
    esamSettings_signalProcessingNotification,

    -- * EsamSignalProcessingNotification
    EsamSignalProcessingNotification (..),
    newEsamSignalProcessingNotification,
    esamSignalProcessingNotification_sccXml,

    -- * F4vSettings
    F4vSettings (..),
    newF4vSettings,
    f4vSettings_moovPlacement,

    -- * FileGroupSettings
    FileGroupSettings (..),
    newFileGroupSettings,
    fileGroupSettings_destination,
    fileGroupSettings_destinationSettings,

    -- * FileSourceSettings
    FileSourceSettings (..),
    newFileSourceSettings,
    fileSourceSettings_convert608To708,
    fileSourceSettings_framerate,
    fileSourceSettings_sourceFile,
    fileSourceSettings_timeDelta,

    -- * FrameCaptureSettings
    FrameCaptureSettings (..),
    newFrameCaptureSettings,
    frameCaptureSettings_framerateNumerator,
    frameCaptureSettings_maxCaptures,
    frameCaptureSettings_framerateDenominator,
    frameCaptureSettings_quality,

    -- * H264QvbrSettings
    H264QvbrSettings (..),
    newH264QvbrSettings,
    h264QvbrSettings_qvbrQualityLevelFineTune,
    h264QvbrSettings_qvbrQualityLevel,
    h264QvbrSettings_maxAverageBitrate,

    -- * H264Settings
    H264Settings (..),
    newH264Settings,
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

    -- * H265QvbrSettings
    H265QvbrSettings (..),
    newH265QvbrSettings,
    h265QvbrSettings_qvbrQualityLevelFineTune,
    h265QvbrSettings_qvbrQualityLevel,
    h265QvbrSettings_maxAverageBitrate,

    -- * H265Settings
    H265Settings (..),
    newH265Settings,
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

    -- * Hdr10Metadata
    Hdr10Metadata (..),
    newHdr10Metadata,
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

    -- * HlsAdditionalManifest
    HlsAdditionalManifest (..),
    newHlsAdditionalManifest,
    hlsAdditionalManifest_manifestNameModifier,
    hlsAdditionalManifest_selectedOutputs,

    -- * HlsCaptionLanguageMapping
    HlsCaptionLanguageMapping (..),
    newHlsCaptionLanguageMapping,
    hlsCaptionLanguageMapping_languageCode,
    hlsCaptionLanguageMapping_languageDescription,
    hlsCaptionLanguageMapping_customLanguageCode,
    hlsCaptionLanguageMapping_captionChannel,

    -- * HlsEncryptionSettings
    HlsEncryptionSettings (..),
    newHlsEncryptionSettings,
    hlsEncryptionSettings_offlineEncrypted,
    hlsEncryptionSettings_spekeKeyProvider,
    hlsEncryptionSettings_encryptionMethod,
    hlsEncryptionSettings_constantInitializationVector,
    hlsEncryptionSettings_initializationVectorInManifest,
    hlsEncryptionSettings_staticKeyProvider,
    hlsEncryptionSettings_type,

    -- * HlsGroupSettings
    HlsGroupSettings (..),
    newHlsGroupSettings,
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

    -- * HlsSettings
    HlsSettings (..),
    newHlsSettings,
    hlsSettings_audioRenditionSets,
    hlsSettings_iFrameOnlyManifest,
    hlsSettings_segmentModifier,
    hlsSettings_audioOnlyContainer,
    hlsSettings_audioGroupId,
    hlsSettings_audioTrackType,

    -- * HopDestination
    HopDestination (..),
    newHopDestination,
    hopDestination_priority,
    hopDestination_queue,
    hopDestination_waitMinutes,

    -- * Id3Insertion
    Id3Insertion (..),
    newId3Insertion,
    id3Insertion_id3,
    id3Insertion_timecode,

    -- * ImageInserter
    ImageInserter (..),
    newImageInserter,
    imageInserter_insertableImages,

    -- * ImscDestinationSettings
    ImscDestinationSettings (..),
    newImscDestinationSettings,
    imscDestinationSettings_stylePassthrough,

    -- * Input
    Input (..),
    newInput,
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

    -- * InputClipping
    InputClipping (..),
    newInputClipping,
    inputClipping_startTimecode,
    inputClipping_endTimecode,

    -- * InputDecryptionSettings
    InputDecryptionSettings (..),
    newInputDecryptionSettings,
    inputDecryptionSettings_decryptionMode,
    inputDecryptionSettings_encryptedDecryptionKey,
    inputDecryptionSettings_initializationVector,
    inputDecryptionSettings_kmsKeyRegion,

    -- * InputTemplate
    InputTemplate (..),
    newInputTemplate,
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

    -- * InsertableImage
    InsertableImage (..),
    newInsertableImage,
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

    -- * Job
    Job (..),
    newJob,
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

    -- * JobMessages
    JobMessages (..),
    newJobMessages,
    jobMessages_info,
    jobMessages_warning,

    -- * JobSettings
    JobSettings (..),
    newJobSettings,
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

    -- * JobTemplate
    JobTemplate (..),
    newJobTemplate,
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

    -- * JobTemplateSettings
    JobTemplateSettings (..),
    newJobTemplateSettings,
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

    -- * M2tsScte35Esam
    M2tsScte35Esam (..),
    newM2tsScte35Esam,
    m2tsScte35Esam_scte35EsamPid,

    -- * M2tsSettings
    M2tsSettings (..),
    newM2tsSettings,
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

    -- * M3u8Settings
    M3u8Settings (..),
    newM3u8Settings,
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

    -- * MotionImageInserter
    MotionImageInserter (..),
    newMotionImageInserter,
    motionImageInserter_insertionMode,
    motionImageInserter_input,
    motionImageInserter_startTime,
    motionImageInserter_playback,
    motionImageInserter_framerate,
    motionImageInserter_offset,

    -- * MotionImageInsertionFramerate
    MotionImageInsertionFramerate (..),
    newMotionImageInsertionFramerate,
    motionImageInsertionFramerate_framerateNumerator,
    motionImageInsertionFramerate_framerateDenominator,

    -- * MotionImageInsertionOffset
    MotionImageInsertionOffset (..),
    newMotionImageInsertionOffset,
    motionImageInsertionOffset_imageX,
    motionImageInsertionOffset_imageY,

    -- * MovSettings
    MovSettings (..),
    newMovSettings,
    movSettings_paddingControl,
    movSettings_cslgAtom,
    movSettings_mpeg2FourCCControl,
    movSettings_clapAtom,
    movSettings_reference,

    -- * Mp2Settings
    Mp2Settings (..),
    newMp2Settings,
    mp2Settings_channels,
    mp2Settings_sampleRate,
    mp2Settings_bitrate,

    -- * Mp3Settings
    Mp3Settings (..),
    newMp3Settings,
    mp3Settings_rateControlMode,
    mp3Settings_channels,
    mp3Settings_sampleRate,
    mp3Settings_vbrQuality,
    mp3Settings_bitrate,

    -- * Mp4Settings
    Mp4Settings (..),
    newMp4Settings,
    mp4Settings_cslgAtom,
    mp4Settings_mp4MajorBrand,
    mp4Settings_audioDuration,
    mp4Settings_freeSpaceBox,
    mp4Settings_moovPlacement,
    mp4Settings_cttsVersion,

    -- * MpdSettings
    MpdSettings (..),
    newMpdSettings,
    mpdSettings_accessibilityCaptionHints,
    mpdSettings_captionContainerType,
    mpdSettings_audioDuration,
    mpdSettings_scte35Esam,
    mpdSettings_scte35Source,

    -- * Mpeg2Settings
    Mpeg2Settings (..),
    newMpeg2Settings,
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

    -- * MsSmoothAdditionalManifest
    MsSmoothAdditionalManifest (..),
    newMsSmoothAdditionalManifest,
    msSmoothAdditionalManifest_manifestNameModifier,
    msSmoothAdditionalManifest_selectedOutputs,

    -- * MsSmoothEncryptionSettings
    MsSmoothEncryptionSettings (..),
    newMsSmoothEncryptionSettings,
    msSmoothEncryptionSettings_spekeKeyProvider,

    -- * MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    newMsSmoothGroupSettings,
    msSmoothGroupSettings_manifestEncoding,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_additionalManifests,
    msSmoothGroupSettings_encryption,
    msSmoothGroupSettings_destination,
    msSmoothGroupSettings_destinationSettings,
    msSmoothGroupSettings_audioDeduplication,

    -- * MxfSettings
    MxfSettings (..),
    newMxfSettings,
    mxfSettings_profile,
    mxfSettings_afdSignaling,

    -- * NexGuardFileMarkerSettings
    NexGuardFileMarkerSettings (..),
    newNexGuardFileMarkerSettings,
    nexGuardFileMarkerSettings_payload,
    nexGuardFileMarkerSettings_license,
    nexGuardFileMarkerSettings_preset,
    nexGuardFileMarkerSettings_strength,

    -- * NielsenConfiguration
    NielsenConfiguration (..),
    newNielsenConfiguration,
    nielsenConfiguration_breakoutCode,
    nielsenConfiguration_distributorId,

    -- * NielsenNonLinearWatermarkSettings
    NielsenNonLinearWatermarkSettings (..),
    newNielsenNonLinearWatermarkSettings,
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

    -- * NoiseReducer
    NoiseReducer (..),
    newNoiseReducer,
    noiseReducer_spatialFilterSettings,
    noiseReducer_temporalFilterSettings,
    noiseReducer_filterSettings,
    noiseReducer_filter,

    -- * NoiseReducerFilterSettings
    NoiseReducerFilterSettings (..),
    newNoiseReducerFilterSettings,
    noiseReducerFilterSettings_strength,

    -- * NoiseReducerSpatialFilterSettings
    NoiseReducerSpatialFilterSettings (..),
    newNoiseReducerSpatialFilterSettings,
    noiseReducerSpatialFilterSettings_speed,
    noiseReducerSpatialFilterSettings_postFilterSharpenStrength,
    noiseReducerSpatialFilterSettings_strength,

    -- * NoiseReducerTemporalFilterSettings
    NoiseReducerTemporalFilterSettings (..),
    newNoiseReducerTemporalFilterSettings,
    noiseReducerTemporalFilterSettings_postTemporalSharpening,
    noiseReducerTemporalFilterSettings_speed,
    noiseReducerTemporalFilterSettings_aggressiveMode,
    noiseReducerTemporalFilterSettings_strength,

    -- * OpusSettings
    OpusSettings (..),
    newOpusSettings,
    opusSettings_channels,
    opusSettings_sampleRate,
    opusSettings_bitrate,

    -- * Output
    Output (..),
    newOutput,
    output_audioDescriptions,
    output_preset,
    output_containerSettings,
    output_videoDescription,
    output_extension,
    output_captionDescriptions,
    output_nameModifier,
    output_outputSettings,

    -- * OutputChannelMapping
    OutputChannelMapping (..),
    newOutputChannelMapping,
    outputChannelMapping_inputChannels,
    outputChannelMapping_inputChannelsFineTune,

    -- * OutputDetail
    OutputDetail (..),
    newOutputDetail,
    outputDetail_videoDetails,
    outputDetail_durationInMs,

    -- * OutputGroup
    OutputGroup (..),
    newOutputGroup,
    outputGroup_outputs,
    outputGroup_automatedEncodingSettings,
    outputGroup_outputGroupSettings,
    outputGroup_name,
    outputGroup_customName,

    -- * OutputGroupDetail
    OutputGroupDetail (..),
    newOutputGroupDetail,
    outputGroupDetail_outputDetails,

    -- * OutputGroupSettings
    OutputGroupSettings (..),
    newOutputGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_fileGroupSettings,
    outputGroupSettings_dashIsoGroupSettings,
    outputGroupSettings_cmafGroupSettings,
    outputGroupSettings_type,

    -- * OutputSettings
    OutputSettings (..),
    newOutputSettings,
    outputSettings_hlsSettings,

    -- * PartnerWatermarking
    PartnerWatermarking (..),
    newPartnerWatermarking,
    partnerWatermarking_nexguardFileMarkerSettings,

    -- * Preset
    Preset (..),
    newPreset,
    preset_category,
    preset_arn,
    preset_createdAt,
    preset_lastUpdated,
    preset_description,
    preset_type,
    preset_settings,
    preset_name,

    -- * PresetSettings
    PresetSettings (..),
    newPresetSettings,
    presetSettings_audioDescriptions,
    presetSettings_containerSettings,
    presetSettings_videoDescription,
    presetSettings_captionDescriptions,

    -- * ProresSettings
    ProresSettings (..),
    newProresSettings,
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

    -- * Queue
    Queue (..),
    newQueue,
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

    -- * QueueTransition
    QueueTransition (..),
    newQueueTransition,
    queueTransition_sourceQueue,
    queueTransition_timestamp,
    queueTransition_destinationQueue,

    -- * Rectangle
    Rectangle (..),
    newRectangle,
    rectangle_height,
    rectangle_y,
    rectangle_width,
    rectangle_x,

    -- * RemixSettings
    RemixSettings (..),
    newRemixSettings,
    remixSettings_channelMapping,
    remixSettings_channelsIn,
    remixSettings_channelsOut,

    -- * ReservationPlan
    ReservationPlan (..),
    newReservationPlan,
    reservationPlan_status,
    reservationPlan_reservedSlots,
    reservationPlan_expiresAt,
    reservationPlan_purchasedAt,
    reservationPlan_renewalType,
    reservationPlan_commitment,

    -- * ReservationPlanSettings
    ReservationPlanSettings (..),
    newReservationPlanSettings,
    reservationPlanSettings_commitment,
    reservationPlanSettings_reservedSlots,
    reservationPlanSettings_renewalType,

    -- * ResourceTags
    ResourceTags (..),
    newResourceTags,
    resourceTags_arn,
    resourceTags_tags,

    -- * S3DestinationAccessControl
    S3DestinationAccessControl (..),
    newS3DestinationAccessControl,
    s3DestinationAccessControl_cannedAcl,

    -- * S3DestinationSettings
    S3DestinationSettings (..),
    newS3DestinationSettings,
    s3DestinationSettings_encryption,
    s3DestinationSettings_accessControl,

    -- * S3EncryptionSettings
    S3EncryptionSettings (..),
    newS3EncryptionSettings,
    s3EncryptionSettings_encryptionType,
    s3EncryptionSettings_kmsKeyArn,

    -- * SccDestinationSettings
    SccDestinationSettings (..),
    newSccDestinationSettings,
    sccDestinationSettings_framerate,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,

    -- * SpekeKeyProviderCmaf
    SpekeKeyProviderCmaf (..),
    newSpekeKeyProviderCmaf,
    spekeKeyProviderCmaf_resourceId,
    spekeKeyProviderCmaf_certificateArn,
    spekeKeyProviderCmaf_url,
    spekeKeyProviderCmaf_hlsSignaledSystemIds,
    spekeKeyProviderCmaf_dashSignaledSystemIds,

    -- * StaticKeyProvider
    StaticKeyProvider (..),
    newStaticKeyProvider,
    staticKeyProvider_keyFormat,
    staticKeyProvider_staticKeyValue,
    staticKeyProvider_url,
    staticKeyProvider_keyFormatVersions,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings (..),
    newTeletextDestinationSettings,
    teletextDestinationSettings_pageTypes,
    teletextDestinationSettings_pageNumber,

    -- * TeletextSourceSettings
    TeletextSourceSettings (..),
    newTeletextSourceSettings,
    teletextSourceSettings_pageNumber,

    -- * TimecodeBurnin
    TimecodeBurnin (..),
    newTimecodeBurnin,
    timecodeBurnin_prefix,
    timecodeBurnin_position,
    timecodeBurnin_fontSize,

    -- * TimecodeConfig
    TimecodeConfig (..),
    newTimecodeConfig,
    timecodeConfig_anchor,
    timecodeConfig_source,
    timecodeConfig_timestampOffset,
    timecodeConfig_start,

    -- * TimedMetadataInsertion
    TimedMetadataInsertion (..),
    newTimedMetadataInsertion,
    timedMetadataInsertion_id3Insertions,

    -- * Timing
    Timing (..),
    newTiming,
    timing_finishTime,
    timing_startTime,
    timing_submitTime,

    -- * TrackSourceSettings
    TrackSourceSettings (..),
    newTrackSourceSettings,
    trackSourceSettings_trackNumber,

    -- * TtmlDestinationSettings
    TtmlDestinationSettings (..),
    newTtmlDestinationSettings,
    ttmlDestinationSettings_stylePassthrough,

    -- * Vc3Settings
    Vc3Settings (..),
    newVc3Settings,
    vc3Settings_interlaceMode,
    vc3Settings_telecine,
    vc3Settings_framerateNumerator,
    vc3Settings_framerateDenominator,
    vc3Settings_scanTypeConversionMode,
    vc3Settings_framerateControl,
    vc3Settings_framerateConversionAlgorithm,
    vc3Settings_vc3Class,
    vc3Settings_slowPal,

    -- * VideoCodecSettings
    VideoCodecSettings (..),
    newVideoCodecSettings,
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

    -- * VideoDescription
    VideoDescription (..),
    newVideoDescription,
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

    -- * VideoDetail
    VideoDetail (..),
    newVideoDetail,
    videoDetail_widthInPx,
    videoDetail_heightInPx,

    -- * VideoPreprocessor
    VideoPreprocessor (..),
    newVideoPreprocessor,
    videoPreprocessor_imageInserter,
    videoPreprocessor_timecodeBurnin,
    videoPreprocessor_deinterlacer,
    videoPreprocessor_noiseReducer,
    videoPreprocessor_partnerWatermarking,
    videoPreprocessor_colorCorrector,
    videoPreprocessor_dolbyVision,

    -- * VideoSelector
    VideoSelector (..),
    newVideoSelector,
    videoSelector_colorSpaceUsage,
    videoSelector_hdr10Metadata,
    videoSelector_programNumber,
    videoSelector_rotate,
    videoSelector_colorSpace,
    videoSelector_alphaBehavior,
    videoSelector_pid,

    -- * VorbisSettings
    VorbisSettings (..),
    newVorbisSettings,
    vorbisSettings_channels,
    vorbisSettings_sampleRate,
    vorbisSettings_vbrQuality,

    -- * Vp8Settings
    Vp8Settings (..),
    newVp8Settings,
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

    -- * Vp9Settings
    Vp9Settings (..),
    newVp9Settings,
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

    -- * WavSettings
    WavSettings (..),
    newWavSettings,
    wavSettings_format,
    wavSettings_channels,
    wavSettings_bitDepth,
    wavSettings_sampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
import Network.AWS.MediaConvert.Types.AacCodecProfile
import Network.AWS.MediaConvert.Types.AacCodingMode
import Network.AWS.MediaConvert.Types.AacRateControlMode
import Network.AWS.MediaConvert.Types.AacRawFormat
import Network.AWS.MediaConvert.Types.AacSettings
import Network.AWS.MediaConvert.Types.AacSpecification
import Network.AWS.MediaConvert.Types.AacVbrQuality
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
import Network.AWS.MediaConvert.Types.Ac3CodingMode
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
import Network.AWS.MediaConvert.Types.Ac3MetadataControl
import Network.AWS.MediaConvert.Types.Ac3Settings
import Network.AWS.MediaConvert.Types.AccelerationMode
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.AccelerationStatus
import Network.AWS.MediaConvert.Types.AfdSignaling
import Network.AWS.MediaConvert.Types.AiffSettings
import Network.AWS.MediaConvert.Types.AlphaBehavior
import Network.AWS.MediaConvert.Types.AncillaryConvert608To708
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
import Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
import Network.AWS.MediaConvert.Types.AntiAlias
import Network.AWS.MediaConvert.Types.AudioChannelTag
import Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
import Network.AWS.MediaConvert.Types.AudioCodec
import Network.AWS.MediaConvert.Types.AudioCodecSettings
import Network.AWS.MediaConvert.Types.AudioDefaultSelection
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
import Network.AWS.MediaConvert.Types.AudioNormalizationSettings
import Network.AWS.MediaConvert.Types.AudioSelector
import Network.AWS.MediaConvert.Types.AudioSelectorGroup
import Network.AWS.MediaConvert.Types.AudioSelectorType
import Network.AWS.MediaConvert.Types.AudioTypeControl
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
import Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
import Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Av1FramerateControl
import Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
import Network.AWS.MediaConvert.Types.Av1RateControlMode
import Network.AWS.MediaConvert.Types.Av1Settings
import Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.AvcIntraClass
import Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
import Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode
import Network.AWS.MediaConvert.Types.AvcIntraScanTypeConversionMode
import Network.AWS.MediaConvert.Types.AvcIntraSettings
import Network.AWS.MediaConvert.Types.AvcIntraSlowPal
import Network.AWS.MediaConvert.Types.AvcIntraTelecine
import Network.AWS.MediaConvert.Types.AvcIntraUhdQualityTuningLevel
import Network.AWS.MediaConvert.Types.AvcIntraUhdSettings
import Network.AWS.MediaConvert.Types.BillingTagsSource
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
import Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
import Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
import Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
import Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.CaptionDescription
import Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionDestinationType
import Network.AWS.MediaConvert.Types.CaptionSelector
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
import Network.AWS.MediaConvert.Types.CaptionSourceSettings
import Network.AWS.MediaConvert.Types.CaptionSourceType
import Network.AWS.MediaConvert.Types.ChannelMapping
import Network.AWS.MediaConvert.Types.CmafAdditionalManifest
import Network.AWS.MediaConvert.Types.CmafClientCache
import Network.AWS.MediaConvert.Types.CmafCodecSpecification
import Network.AWS.MediaConvert.Types.CmafEncryptionSettings
import Network.AWS.MediaConvert.Types.CmafEncryptionType
import Network.AWS.MediaConvert.Types.CmafGroupSettings
import Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.CmafKeyProviderType
import Network.AWS.MediaConvert.Types.CmafManifestCompression
import Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
import Network.AWS.MediaConvert.Types.CmafMpdProfile
import Network.AWS.MediaConvert.Types.CmafSegmentControl
import Network.AWS.MediaConvert.Types.CmafStreamInfResolution
import Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
import Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
import Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.CmfcIFrameOnlyManifest
import Network.AWS.MediaConvert.Types.CmfcScte35Esam
import Network.AWS.MediaConvert.Types.CmfcScte35Source
import Network.AWS.MediaConvert.Types.CmfcSettings
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.ColorMetadata
import Network.AWS.MediaConvert.Types.ColorSpace
import Network.AWS.MediaConvert.Types.ColorSpaceConversion
import Network.AWS.MediaConvert.Types.ColorSpaceUsage
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.ContainerType
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
import Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DecryptionMode
import Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
import Network.AWS.MediaConvert.Types.Deinterlacer
import Network.AWS.MediaConvert.Types.DeinterlacerControl
import Network.AWS.MediaConvert.Types.DeinterlacerMode
import Network.AWS.MediaConvert.Types.DescribeEndpointsMode
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.MediaConvert.Types.DolbyVision
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
import Network.AWS.MediaConvert.Types.DolbyVisionProfile
import Network.AWS.MediaConvert.Types.DropFrameTimecode
import Network.AWS.MediaConvert.Types.DvbNitSettings
import Network.AWS.MediaConvert.Types.DvbSdtSettings
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
import Network.AWS.MediaConvert.Types.DvbTdtSettings
import Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
import Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
import Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
import Network.AWS.MediaConvert.Types.Eac3AtmosSettings
import Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
import Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
import Network.AWS.MediaConvert.Types.Eac3AttenuationControl
import Network.AWS.MediaConvert.Types.Eac3BitstreamMode
import Network.AWS.MediaConvert.Types.Eac3CodingMode
import Network.AWS.MediaConvert.Types.Eac3DcFilter
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3LfeControl
import Network.AWS.MediaConvert.Types.Eac3LfeFilter
import Network.AWS.MediaConvert.Types.Eac3MetadataControl
import Network.AWS.MediaConvert.Types.Eac3PassthroughControl
import Network.AWS.MediaConvert.Types.Eac3PhaseControl
import Network.AWS.MediaConvert.Types.Eac3Settings
import Network.AWS.MediaConvert.Types.Eac3StereoDownmix
import Network.AWS.MediaConvert.Types.Eac3SurroundExMode
import Network.AWS.MediaConvert.Types.Eac3SurroundMode
import Network.AWS.MediaConvert.Types.EmbeddedConvert608To708
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
import Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
import Network.AWS.MediaConvert.Types.Endpoint
import Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
import Network.AWS.MediaConvert.Types.F4vMoovPlacement
import Network.AWS.MediaConvert.Types.F4vSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.FileSourceConvert608To708
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.FontScript
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
import Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264CodecLevel
import Network.AWS.MediaConvert.Types.H264CodecProfile
import Network.AWS.MediaConvert.Types.H264DynamicSubGop
import Network.AWS.MediaConvert.Types.H264EntropyEncoding
import Network.AWS.MediaConvert.Types.H264FieldEncoding
import Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264FramerateControl
import Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H264GopBReference
import Network.AWS.MediaConvert.Types.H264GopSizeUnits
import Network.AWS.MediaConvert.Types.H264InterlaceMode
import Network.AWS.MediaConvert.Types.H264ParControl
import Network.AWS.MediaConvert.Types.H264QualityTuningLevel
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264RateControlMode
import Network.AWS.MediaConvert.Types.H264RepeatPps
import Network.AWS.MediaConvert.Types.H264ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
import Network.AWS.MediaConvert.Types.H264Settings
import Network.AWS.MediaConvert.Types.H264SlowPal
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264Syntax
import Network.AWS.MediaConvert.Types.H264Telecine
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
import Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
import Network.AWS.MediaConvert.Types.H265CodecLevel
import Network.AWS.MediaConvert.Types.H265CodecProfile
import Network.AWS.MediaConvert.Types.H265DynamicSubGop
import Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265FramerateControl
import Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H265GopBReference
import Network.AWS.MediaConvert.Types.H265GopSizeUnits
import Network.AWS.MediaConvert.Types.H265InterlaceMode
import Network.AWS.MediaConvert.Types.H265ParControl
import Network.AWS.MediaConvert.Types.H265QualityTuningLevel
import Network.AWS.MediaConvert.Types.H265QvbrSettings
import Network.AWS.MediaConvert.Types.H265RateControlMode
import Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
import Network.AWS.MediaConvert.Types.H265ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.H265SceneChangeDetect
import Network.AWS.MediaConvert.Types.H265Settings
import Network.AWS.MediaConvert.Types.H265SlowPal
import Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265Telecine
import Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265TemporalIds
import Network.AWS.MediaConvert.Types.H265Tiles
import Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
import Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
import Network.AWS.MediaConvert.Types.Hdr10Metadata
import Network.AWS.MediaConvert.Types.HlsAdMarkers
import Network.AWS.MediaConvert.Types.HlsAdditionalManifest
import Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
import Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
import Network.AWS.MediaConvert.Types.HlsAudioTrackType
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
import Network.AWS.MediaConvert.Types.HlsClientCache
import Network.AWS.MediaConvert.Types.HlsCodecSpecification
import Network.AWS.MediaConvert.Types.HlsDirectoryStructure
import Network.AWS.MediaConvert.Types.HlsEncryptionSettings
import Network.AWS.MediaConvert.Types.HlsEncryptionType
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
import Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.HlsKeyProviderType
import Network.AWS.MediaConvert.Types.HlsManifestCompression
import Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
import Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
import Network.AWS.MediaConvert.Types.HlsOutputSelection
import Network.AWS.MediaConvert.Types.HlsProgramDateTime
import Network.AWS.MediaConvert.Types.HlsSegmentControl
import Network.AWS.MediaConvert.Types.HlsSettings
import Network.AWS.MediaConvert.Types.HlsStreamInfResolution
import Network.AWS.MediaConvert.Types.HlsTimedMetadataId3Frame
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.Id3Insertion
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
import Network.AWS.MediaConvert.Types.ImscStylePassthrough
import Network.AWS.MediaConvert.Types.Input
import Network.AWS.MediaConvert.Types.InputClipping
import Network.AWS.MediaConvert.Types.InputDeblockFilter
import Network.AWS.MediaConvert.Types.InputDecryptionSettings
import Network.AWS.MediaConvert.Types.InputDenoiseFilter
import Network.AWS.MediaConvert.Types.InputFilterEnable
import Network.AWS.MediaConvert.Types.InputPsiControl
import Network.AWS.MediaConvert.Types.InputRotate
import Network.AWS.MediaConvert.Types.InputScanType
import Network.AWS.MediaConvert.Types.InputTemplate
import Network.AWS.MediaConvert.Types.InputTimecodeSource
import Network.AWS.MediaConvert.Types.InsertableImage
import Network.AWS.MediaConvert.Types.Job
import Network.AWS.MediaConvert.Types.JobMessages
import Network.AWS.MediaConvert.Types.JobPhase
import Network.AWS.MediaConvert.Types.JobSettings
import Network.AWS.MediaConvert.Types.JobStatus
import Network.AWS.MediaConvert.Types.JobTemplate
import Network.AWS.MediaConvert.Types.JobTemplateListBy
import Network.AWS.MediaConvert.Types.JobTemplateSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
import Network.AWS.MediaConvert.Types.M2tsAudioDuration
import Network.AWS.MediaConvert.Types.M2tsBufferModel
import Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
import Network.AWS.MediaConvert.Types.M2tsEbpPlacement
import Network.AWS.MediaConvert.Types.M2tsEsRateInPes
import Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
import Network.AWS.MediaConvert.Types.M2tsNielsenId3
import Network.AWS.MediaConvert.Types.M2tsPcrControl
import Network.AWS.MediaConvert.Types.M2tsRateMode
import Network.AWS.MediaConvert.Types.M2tsScte35Esam
import Network.AWS.MediaConvert.Types.M2tsScte35Source
import Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
import Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
import Network.AWS.MediaConvert.Types.M2tsSettings
import Network.AWS.MediaConvert.Types.M3u8AudioDuration
import Network.AWS.MediaConvert.Types.M3u8NielsenId3
import Network.AWS.MediaConvert.Types.M3u8PcrControl
import Network.AWS.MediaConvert.Types.M3u8Scte35Source
import Network.AWS.MediaConvert.Types.M3u8Settings
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
import Network.AWS.MediaConvert.Types.MotionImageInsertionMode
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
import Network.AWS.MediaConvert.Types.MotionImagePlayback
import Network.AWS.MediaConvert.Types.MovClapAtom
import Network.AWS.MediaConvert.Types.MovCslgAtom
import Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
import Network.AWS.MediaConvert.Types.MovPaddingControl
import Network.AWS.MediaConvert.Types.MovReference
import Network.AWS.MediaConvert.Types.MovSettings
import Network.AWS.MediaConvert.Types.Mp2Settings
import Network.AWS.MediaConvert.Types.Mp3RateControlMode
import Network.AWS.MediaConvert.Types.Mp3Settings
import Network.AWS.MediaConvert.Types.Mp4CslgAtom
import Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
import Network.AWS.MediaConvert.Types.Mp4MoovPlacement
import Network.AWS.MediaConvert.Types.Mp4Settings
import Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
import Network.AWS.MediaConvert.Types.MpdAudioDuration
import Network.AWS.MediaConvert.Types.MpdCaptionContainerType
import Network.AWS.MediaConvert.Types.MpdScte35Esam
import Network.AWS.MediaConvert.Types.MpdScte35Source
import Network.AWS.MediaConvert.Types.MpdSettings
import Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
import Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
import Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
import Network.AWS.MediaConvert.Types.Mpeg2FramerateControl
import Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
import Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
import Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
import Network.AWS.MediaConvert.Types.Mpeg2ParControl
import Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
import Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
import Network.AWS.MediaConvert.Types.Mpeg2ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
import Network.AWS.MediaConvert.Types.Mpeg2Settings
import Network.AWS.MediaConvert.Types.Mpeg2SlowPal
import Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.Mpeg2Syntax
import Network.AWS.MediaConvert.Types.Mpeg2Telecine
import Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
import Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
import Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
import Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
import Network.AWS.MediaConvert.Types.MxfAfdSignaling
import Network.AWS.MediaConvert.Types.MxfProfile
import Network.AWS.MediaConvert.Types.MxfSettings
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
import Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.NoiseReducerFilter
import Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import Network.AWS.MediaConvert.Types.OpusSettings
import Network.AWS.MediaConvert.Types.Order
import Network.AWS.MediaConvert.Types.Output
import Network.AWS.MediaConvert.Types.OutputChannelMapping
import Network.AWS.MediaConvert.Types.OutputDetail
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.OutputGroupDetail
import Network.AWS.MediaConvert.Types.OutputGroupSettings
import Network.AWS.MediaConvert.Types.OutputGroupType
import Network.AWS.MediaConvert.Types.OutputSdt
import Network.AWS.MediaConvert.Types.OutputSettings
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.Preset
import Network.AWS.MediaConvert.Types.PresetListBy
import Network.AWS.MediaConvert.Types.PresetSettings
import Network.AWS.MediaConvert.Types.PricingPlan
import Network.AWS.MediaConvert.Types.ProresCodecProfile
import Network.AWS.MediaConvert.Types.ProresFramerateControl
import Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.ProresInterlaceMode
import Network.AWS.MediaConvert.Types.ProresParControl
import Network.AWS.MediaConvert.Types.ProresScanTypeConversionMode
import Network.AWS.MediaConvert.Types.ProresSettings
import Network.AWS.MediaConvert.Types.ProresSlowPal
import Network.AWS.MediaConvert.Types.ProresTelecine
import Network.AWS.MediaConvert.Types.Queue
import Network.AWS.MediaConvert.Types.QueueListBy
import Network.AWS.MediaConvert.Types.QueueStatus
import Network.AWS.MediaConvert.Types.QueueTransition
import Network.AWS.MediaConvert.Types.Rectangle
import Network.AWS.MediaConvert.Types.RemixSettings
import Network.AWS.MediaConvert.Types.RenewalType
import Network.AWS.MediaConvert.Types.ReservationPlan
import Network.AWS.MediaConvert.Types.ReservationPlanSettings
import Network.AWS.MediaConvert.Types.ReservationPlanStatus
import Network.AWS.MediaConvert.Types.ResourceTags
import Network.AWS.MediaConvert.Types.RespondToAfd
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
import Network.AWS.MediaConvert.Types.S3DestinationSettings
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
import Network.AWS.MediaConvert.Types.S3ObjectCannedAcl
import Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
import Network.AWS.MediaConvert.Types.ScalingBehavior
import Network.AWS.MediaConvert.Types.SccDestinationFramerate
import Network.AWS.MediaConvert.Types.SccDestinationSettings
import Network.AWS.MediaConvert.Types.SimulateReservedQueue
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
import Network.AWS.MediaConvert.Types.TeletextPageType
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
import Network.AWS.MediaConvert.Types.TimecodeBurnin
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimecodeSource
import Network.AWS.MediaConvert.Types.TimedMetadata
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import Network.AWS.MediaConvert.Types.Timing
import Network.AWS.MediaConvert.Types.TrackSourceSettings
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough
import Network.AWS.MediaConvert.Types.Type
import Network.AWS.MediaConvert.Types.Vc3Class
import Network.AWS.MediaConvert.Types.Vc3FramerateControl
import Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vc3InterlaceMode
import Network.AWS.MediaConvert.Types.Vc3ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.Vc3Settings
import Network.AWS.MediaConvert.Types.Vc3SlowPal
import Network.AWS.MediaConvert.Types.Vc3Telecine
import Network.AWS.MediaConvert.Types.VideoCodec
import Network.AWS.MediaConvert.Types.VideoCodecSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import Network.AWS.MediaConvert.Types.VideoDetail
import Network.AWS.MediaConvert.Types.VideoPreprocessor
import Network.AWS.MediaConvert.Types.VideoSelector
import Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
import Network.AWS.MediaConvert.Types.VorbisSettings
import Network.AWS.MediaConvert.Types.Vp8FramerateControl
import Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp8ParControl
import Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp8RateControlMode
import Network.AWS.MediaConvert.Types.Vp8Settings
import Network.AWS.MediaConvert.Types.Vp9FramerateControl
import Network.AWS.MediaConvert.Types.Vp9FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp9ParControl
import Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp9RateControlMode
import Network.AWS.MediaConvert.Types.Vp9Settings
import Network.AWS.MediaConvert.Types.WatermarkingStrength
import Network.AWS.MediaConvert.Types.WavFormat
import Network.AWS.MediaConvert.Types.WavSettings
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-08-29@ of the Amazon Elemental MediaConvert SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "MediaConvert",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "mediaconvert",
      Prelude._svcVersion = "2017-08-29",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "MediaConvert",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource you requested doesn\'t exist.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The service can\'t process your request because of a problem in the
-- request. Please check your request form and syntax.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The service encountered an unexpected condition and can\'t fulfill your
-- request.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Prelude.hasStatus 500

-- | You don\'t have permissions for this action with the credentials you
-- sent.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Prelude.hasStatus 403

-- | The service couldn\'t complete your request because there is a conflict
-- with the current state of the resource.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Prelude.hasStatus 409

-- | Too many requests have been sent in too short of a time. The service
-- limits the rate at which it will accept requests.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
