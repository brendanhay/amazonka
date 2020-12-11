-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types
  ( -- * Service configuration
    mediaConvertService,

    -- * Errors

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

    -- * AvcIntraSlowPal
    AvcIntraSlowPal (..),

    -- * AvcIntraTelecine
    AvcIntraTelecine (..),

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

    -- * S3ObjectCannedACL
    S3ObjectCannedACL (..),

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
    mkAacSettings,
    assAudioDescriptionBroadcasterMix,
    assRawFormat,
    assCodingMode,
    assRateControlMode,
    assSampleRate,
    assSpecification,
    assCodecProfile,
    assBitrate,
    assVbrQuality,

    -- * Ac3Settings
    Ac3Settings (..),
    mkAc3Settings,
    aLfeFilter,
    aMetadataControl,
    aBitstreamMode,
    aCodingMode,
    aSampleRate,
    aDynamicRangeCompressionProfile,
    aBitrate,
    aDialnorm,

    -- * AccelerationSettings
    AccelerationSettings (..),
    mkAccelerationSettings,
    asMode,

    -- * AiffSettings
    AiffSettings (..),
    mkAiffSettings,
    asBitDepth,
    asChannels,
    asSampleRate,

    -- * AncillarySourceSettings
    AncillarySourceSettings (..),
    mkAncillarySourceSettings,
    assConvert608To708,
    assTerminateCaptions,
    assSourceAncillaryChannelNumber,

    -- * AudioChannelTaggingSettings
    AudioChannelTaggingSettings (..),
    mkAudioChannelTaggingSettings,
    actsChannelTag,

    -- * AudioCodecSettings
    AudioCodecSettings (..),
    mkAudioCodecSettings,
    acsAiffSettings,
    acsCodec,
    acsAc3Settings,
    acsOpusSettings,
    acsMp2Settings,
    acsWavSettings,
    acsEac3AtmosSettings,
    acsMp3Settings,
    acsVorbisSettings,
    acsAacSettings,
    acsEac3Settings,

    -- * AudioDescription
    AudioDescription (..),
    mkAudioDescription,
    adAudioSourceName,
    adCustomLanguageCode,
    adLanguageCode,
    adAudioChannelTaggingSettings,
    adAudioType,
    adAudioNormalizationSettings,
    adLanguageCodeControl,
    adCodecSettings,
    adStreamName,
    adRemixSettings,
    adAudioTypeControl,

    -- * AudioNormalizationSettings
    AudioNormalizationSettings (..),
    mkAudioNormalizationSettings,
    ansAlgorithmControl,
    ansTargetLkfs,
    ansPeakCalculation,
    ansCorrectionGateLevel,
    ansAlgorithm,
    ansLoudnessLogging,

    -- * AudioSelector
    AudioSelector (..),
    mkAudioSelector,
    asTracks,
    asCustomLanguageCode,
    asProgramSelection,
    asLanguageCode,
    asOffset,
    asDefaultSelection,
    asPids,
    asSelectorType,
    asExternalAudioFileInput,
    asRemixSettings,

    -- * AudioSelectorGroup
    AudioSelectorGroup (..),
    mkAudioSelectorGroup,
    asgAudioSelectorNames,

    -- * AutomatedAbrSettings
    AutomatedAbrSettings (..),
    mkAutomatedAbrSettings,
    aasMaxRenditions,
    aasMaxAbrBitrate,
    aasMinAbrBitrate,

    -- * AutomatedEncodingSettings
    AutomatedEncodingSettings (..),
    mkAutomatedEncodingSettings,
    aesAbrSettings,

    -- * Av1QvbrSettings
    Av1QvbrSettings (..),
    mkAv1QvbrSettings,
    aqsQvbrQualityLevelFineTune,
    aqsQvbrQualityLevel,

    -- * Av1Settings
    Av1Settings (..),
    mkAv1Settings,
    asGopSize,
    asNumberBFramesBetweenReferenceFrames,
    asSlices,
    asRateControlMode,
    asQvbrSettings,
    asFramerateDenominator,
    asFramerateConversionAlgorithm,
    asFramerateControl,
    asAdaptiveQuantization,
    asFramerateNumerator,
    asMaxBitrate,
    asSpatialAdaptiveQuantization,

    -- * AvailBlanking
    AvailBlanking (..),
    mkAvailBlanking,
    abAvailBlankingImage,

    -- * AvcIntraSettings
    AvcIntraSettings (..),
    mkAvcIntraSettings,
    aisSlowPal,
    aisTelecine,
    aisInterlaceMode,
    aisAvcIntraClass,
    aisFramerateDenominator,
    aisFramerateConversionAlgorithm,
    aisFramerateControl,
    aisFramerateNumerator,

    -- * BurninDestinationSettings
    BurninDestinationSettings (..),
    mkBurninDestinationSettings,
    bdsBackgroundOpacity,
    bdsFontOpacity,
    bdsShadowYOffset,
    bdsFontResolution,
    bdsYPosition,
    bdsBackgroundColor,
    bdsShadowXOffset,
    bdsFontSize,
    bdsXPosition,
    bdsTeletextSpacing,
    bdsFontScript,
    bdsAlignment,
    bdsShadowOpacity,
    bdsOutlineColor,
    bdsOutlineSize,
    bdsShadowColor,
    bdsFontColor,

    -- * CaptionDescription
    CaptionDescription (..),
    mkCaptionDescription,
    cdCaptionSelectorName,
    cdCustomLanguageCode,
    cdLanguageCode,
    cdDestinationSettings,
    cdLanguageDescription,

    -- * CaptionDescriptionPreset
    CaptionDescriptionPreset (..),
    mkCaptionDescriptionPreset,
    cdpCustomLanguageCode,
    cdpLanguageCode,
    cdpDestinationSettings,
    cdpLanguageDescription,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings (..),
    mkCaptionDestinationSettings,
    cdsTeletextDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsTtmlDestinationSettings,
    cdsDestinationType,
    cdsEmbeddedDestinationSettings,
    cdsSccDestinationSettings,
    cdsBurninDestinationSettings,
    cdsImscDestinationSettings,

    -- * CaptionSelector
    CaptionSelector (..),
    mkCaptionSelector,
    csCustomLanguageCode,
    csLanguageCode,
    csSourceSettings,

    -- * CaptionSourceFramerate
    CaptionSourceFramerate (..),
    mkCaptionSourceFramerate,
    csfFramerateDenominator,
    csfFramerateNumerator,

    -- * CaptionSourceSettings
    CaptionSourceSettings (..),
    mkCaptionSourceSettings,
    cssTeletextSourceSettings,
    cssSourceType,
    cssFileSourceSettings,
    cssDvbSubSourceSettings,
    cssTrackSourceSettings,
    cssAncillarySourceSettings,
    cssEmbeddedSourceSettings,

    -- * ChannelMapping
    ChannelMapping (..),
    mkChannelMapping,
    cmOutputChannels,

    -- * CmafAdditionalManifest
    CmafAdditionalManifest (..),
    mkCmafAdditionalManifest,
    camManifestNameModifier,
    camSelectedOutputs,

    -- * CmafEncryptionSettings
    CmafEncryptionSettings (..),
    mkCmafEncryptionSettings,
    cesEncryptionMethod,
    cesConstantInitializationVector,
    cesType,
    cesStaticKeyProvider,
    cesSpekeKeyProvider,
    cesInitializationVectorInManifest,

    -- * CmafGroupSettings
    CmafGroupSettings (..),
    mkCmafGroupSettings,
    cgsFragmentLength,
    cgsSegmentControl,
    cgsDestination,
    cgsMinBufferTime,
    cgsMpdProfile,
    cgsWriteHlsManifest,
    cgsAdditionalManifests,
    cgsCodecSpecification,
    cgsBaseURL,
    cgsDestinationSettings,
    cgsMinFinalSegmentLength,
    cgsWriteDashManifest,
    cgsEncryption,
    cgsSegmentLength,
    cgsManifestDurationFormat,
    cgsClientCache,
    cgsWriteSegmentTimelineInRepresentation,
    cgsStreamInfResolution,
    cgsManifestCompression,

    -- * CmfcSettings
    CmfcSettings (..),
    mkCmfcSettings,
    csScte35Esam,
    csAudioDuration,
    csScte35Source,

    -- * ColorCorrector
    ColorCorrector (..),
    mkColorCorrector,
    ccSaturation,
    ccHue,
    ccColorSpaceConversion,
    ccHdr10Metadata,
    ccContrast,
    ccBrightness,

    -- * ContainerSettings
    ContainerSettings (..),
    mkContainerSettings,
    csM2tsSettings,
    csMxfSettings,
    csM3u8Settings,
    csCmfcSettings,
    csMovSettings,
    csMp4Settings,
    csMpdSettings,
    csContainer,
    csF4vSettings,

    -- * DashAdditionalManifest
    DashAdditionalManifest (..),
    mkDashAdditionalManifest,
    damManifestNameModifier,
    damSelectedOutputs,

    -- * DashIsoEncryptionSettings
    DashIsoEncryptionSettings (..),
    mkDashIsoEncryptionSettings,
    diesPlaybackDeviceCompatibility,
    diesSpekeKeyProvider,

    -- * DashIsoGroupSettings
    DashIsoGroupSettings (..),
    mkDashIsoGroupSettings,
    digsFragmentLength,
    digsSegmentControl,
    digsDestination,
    digsHbbtvCompliance,
    digsMinBufferTime,
    digsMpdProfile,
    digsAdditionalManifests,
    digsBaseURL,
    digsDestinationSettings,
    digsMinFinalSegmentLength,
    digsEncryption,
    digsSegmentLength,
    digsWriteSegmentTimelineInRepresentation,

    -- * Deinterlacer
    Deinterlacer (..),
    mkDeinterlacer,
    dControl,
    dMode,
    dAlgorithm,

    -- * DestinationSettings
    DestinationSettings (..),
    mkDestinationSettings,
    dsS3Settings,

    -- * DolbyVision
    DolbyVision (..),
    mkDolbyVision,
    dvProfile,
    dvL6Mode,
    dvL6Metadata,

    -- * DolbyVisionLevel6Metadata
    DolbyVisionLevel6Metadata (..),
    mkDolbyVisionLevel6Metadata,
    dvlmMaxFall,
    dvlmMaxCll,

    -- * DvbNitSettings
    DvbNitSettings (..),
    mkDvbNitSettings,
    dnsNetworkId,
    dnsNetworkName,
    dnsNitInterval,

    -- * DvbSdtSettings
    DvbSdtSettings (..),
    mkDvbSdtSettings,
    dssSdtInterval,
    dssServiceProviderName,
    dssOutputSdt,
    dssServiceName,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings (..),
    mkDvbSubDestinationSettings,
    dsdsBackgroundOpacity,
    dsdsFontOpacity,
    dsdsShadowYOffset,
    dsdsFontResolution,
    dsdsYPosition,
    dsdsBackgroundColor,
    dsdsShadowXOffset,
    dsdsFontSize,
    dsdsXPosition,
    dsdsTeletextSpacing,
    dsdsFontScript,
    dsdsAlignment,
    dsdsShadowOpacity,
    dsdsOutlineColor,
    dsdsOutlineSize,
    dsdsShadowColor,
    dsdsFontColor,
    dsdsSubtitlingType,

    -- * DvbSubSourceSettings
    DvbSubSourceSettings (..),
    mkDvbSubSourceSettings,
    dsssPid,

    -- * DvbTdtSettings
    DvbTdtSettings (..),
    mkDvbTdtSettings,
    dtsTdtInterval,

    -- * Eac3AtmosSettings
    Eac3AtmosSettings (..),
    mkEac3AtmosSettings,
    easStereoDownmix,
    easLoRoCenterMixLevel,
    easLtRtCenterMixLevel,
    easDynamicRangeCompressionLine,
    easLtRtSurroundMixLevel,
    easLoRoSurroundMixLevel,
    easBitstreamMode,
    easDynamicRangeCompressionRf,
    easCodingMode,
    easSampleRate,
    easSpeechThreshold,
    easBitrate,
    easDialogueIntelligence,
    easMeteringMode,
    easSurroundExMode,

    -- * Eac3Settings
    Eac3Settings (..),
    mkEac3Settings,
    esStereoDownmix,
    esLoRoCenterMixLevel,
    esLtRtCenterMixLevel,
    esLfeFilter,
    esDynamicRangeCompressionLine,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esLoRoSurroundMixLevel,
    esSurroundMode,
    esAttenuationControl,
    esPassthroughControl,
    esBitstreamMode,
    esLfeControl,
    esDynamicRangeCompressionRf,
    esCodingMode,
    esSampleRate,
    esDcFilter,
    esBitrate,
    esPhaseControl,
    esSurroundExMode,
    esDialnorm,

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    mkEmbeddedDestinationSettings,
    edsDestination608ChannelNumber,
    edsDestination708ServiceNumber,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    mkEmbeddedSourceSettings,
    essConvert608To708,
    essTerminateCaptions,
    essSource608TrackNumber,
    essSource608ChannelNumber,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eURL,

    -- * EsamManifestConfirmConditionNotification
    EsamManifestConfirmConditionNotification (..),
    mkEsamManifestConfirmConditionNotification,
    emccnMccXML,

    -- * EsamSettings
    EsamSettings (..),
    mkEsamSettings,
    esManifestConfirmConditionNotification,
    esResponseSignalPreroll,
    esSignalProcessingNotification,

    -- * EsamSignalProcessingNotification
    EsamSignalProcessingNotification (..),
    mkEsamSignalProcessingNotification,
    espnSccXML,

    -- * F4vSettings
    F4vSettings (..),
    mkF4vSettings,
    fsMoovPlacement,

    -- * FileGroupSettings
    FileGroupSettings (..),
    mkFileGroupSettings,
    fgsDestination,
    fgsDestinationSettings,

    -- * FileSourceSettings
    FileSourceSettings (..),
    mkFileSourceSettings,
    fssFramerate,
    fssConvert608To708,
    fssTimeDelta,
    fssSourceFile,

    -- * FrameCaptureSettings
    FrameCaptureSettings (..),
    mkFrameCaptureSettings,
    fcsQuality,
    fcsFramerateDenominator,
    fcsMaxCaptures,
    fcsFramerateNumerator,

    -- * H264QvbrSettings
    H264QvbrSettings (..),
    mkH264QvbrSettings,
    hQvbrQualityLevelFineTune,
    hMaxAverageBitrate,
    hQvbrQualityLevel,

    -- * H264Settings
    H264Settings (..),
    mkH264Settings,
    hUnregisteredSeiTimecode,
    hQualityTuningLevel,
    hTemporalAdaptiveQuantization,
    hSceneChangeDetect,
    hHrdBufferInitialFillPercentage,
    hSlowPal,
    hParNumerator,
    hGopSize,
    hNumberBFramesBetweenReferenceFrames,
    hGopSizeUnits,
    hHrdBufferSize,
    hSlices,
    hRateControlMode,
    hNumberReferenceFrames,
    hTelecine,
    hDynamicSubGop,
    hMinIInterval,
    hInterlaceMode,
    hParControl,
    hRepeatPps,
    hFlickerAdaptiveQuantization,
    hQvbrSettings,
    hSoftness,
    hCodecProfile,
    hBitrate,
    hFramerateDenominator,
    hFramerateConversionAlgorithm,
    hCodecLevel,
    hEntropyEncoding,
    hFramerateControl,
    hAdaptiveQuantization,
    hFramerateNumerator,
    hGopBReference,
    hMaxBitrate,
    hSyntax,
    hFieldEncoding,
    hGopClosedCadence,
    hParDenominator,
    hSpatialAdaptiveQuantization,

    -- * H265QvbrSettings
    H265QvbrSettings (..),
    mkH265QvbrSettings,
    hqsQvbrQualityLevelFineTune,
    hqsMaxAverageBitrate,
    hqsQvbrQualityLevel,

    -- * H265Settings
    H265Settings (..),
    mkH265Settings,
    hsUnregisteredSeiTimecode,
    hsQualityTuningLevel,
    hsTemporalAdaptiveQuantization,
    hsSceneChangeDetect,
    hsHrdBufferInitialFillPercentage,
    hsTiles,
    hsSlowPal,
    hsTemporalIds,
    hsParNumerator,
    hsGopSize,
    hsNumberBFramesBetweenReferenceFrames,
    hsGopSizeUnits,
    hsHrdBufferSize,
    hsSlices,
    hsAlternateTransferFunctionSei,
    hsRateControlMode,
    hsNumberReferenceFrames,
    hsTelecine,
    hsDynamicSubGop,
    hsMinIInterval,
    hsInterlaceMode,
    hsParControl,
    hsFlickerAdaptiveQuantization,
    hsQvbrSettings,
    hsSampleAdaptiveOffsetFilterMode,
    hsCodecProfile,
    hsBitrate,
    hsFramerateDenominator,
    hsFramerateConversionAlgorithm,
    hsCodecLevel,
    hsFramerateControl,
    hsWriteMp4PackagingType,
    hsAdaptiveQuantization,
    hsFramerateNumerator,
    hsGopBReference,
    hsMaxBitrate,
    hsGopClosedCadence,
    hsParDenominator,
    hsSpatialAdaptiveQuantization,

    -- * Hdr10Metadata
    Hdr10Metadata (..),
    mkHdr10Metadata,
    hmRedPrimaryX,
    hmBluePrimaryX,
    hmMaxFrameAverageLightLevel,
    hmWhitePointY,
    hmMaxContentLightLevel,
    hmWhitePointX,
    hmBluePrimaryY,
    hmGreenPrimaryY,
    hmGreenPrimaryX,
    hmMinLuminance,
    hmRedPrimaryY,
    hmMaxLuminance,

    -- * HlsAdditionalManifest
    HlsAdditionalManifest (..),
    mkHlsAdditionalManifest,
    hamManifestNameModifier,
    hamSelectedOutputs,

    -- * HlsCaptionLanguageMapping
    HlsCaptionLanguageMapping (..),
    mkHlsCaptionLanguageMapping,
    hclmCustomLanguageCode,
    hclmLanguageCode,
    hclmLanguageDescription,
    hclmCaptionChannel,

    -- * HlsEncryptionSettings
    HlsEncryptionSettings (..),
    mkHlsEncryptionSettings,
    hesOfflineEncrypted,
    hesEncryptionMethod,
    hesConstantInitializationVector,
    hesType,
    hesStaticKeyProvider,
    hesSpekeKeyProvider,
    hesInitializationVectorInManifest,

    -- * HlsGroupSettings
    HlsGroupSettings (..),
    mkHlsGroupSettings,
    hgsDirectoryStructure,
    hgsSegmentControl,
    hgsDestination,
    hgsTimedMetadataId3Period,
    hgsAdditionalManifests,
    hgsMinSegmentLength,
    hgsProgramDateTime,
    hgsProgramDateTimePeriod,
    hgsCodecSpecification,
    hgsCaptionLanguageMappings,
    hgsBaseURL,
    hgsDestinationSettings,
    hgsMinFinalSegmentLength,
    hgsAdMarkers,
    hgsEncryption,
    hgsSegmentLength,
    hgsTimedMetadataId3Frame,
    hgsOutputSelection,
    hgsCaptionLanguageSetting,
    hgsSegmentsPerSubdirectory,
    hgsManifestDurationFormat,
    hgsAudioOnlyHeader,
    hgsClientCache,
    hgsTimestampDeltaMilliseconds,
    hgsStreamInfResolution,
    hgsManifestCompression,

    -- * HlsSettings
    HlsSettings (..),
    mkHlsSettings,
    hsAudioRenditionSets,
    hsIFrameOnlyManifest,
    hsAudioGroupId,
    hsSegmentModifier,
    hsAudioOnlyContainer,
    hsAudioTrackType,

    -- * HopDestination
    HopDestination (..),
    mkHopDestination,
    hdPriority,
    hdQueue,
    hdWaitMinutes,

    -- * Id3Insertion
    Id3Insertion (..),
    mkId3Insertion,
    iiId3,
    iiTimecode,

    -- * ImageInserter
    ImageInserter (..),
    mkImageInserter,
    iiInsertableImages,

    -- * ImscDestinationSettings
    ImscDestinationSettings (..),
    mkImscDestinationSettings,
    idsStylePassthrough,

    -- * Input
    Input (..),
    mkInput,
    iVideoSelector,
    iSupplementalImps,
    iProgramNumber,
    iAudioSelectorGroups,
    iTimecodeSource,
    iAudioSelectors,
    iDecryptionSettings,
    iDeblockFilter,
    iInputClippings,
    iCrop,
    iDenoiseFilter,
    iImageInserter,
    iFilterStrength,
    iPsiControl,
    iCaptionSelectors,
    iFileInput,
    iTimecodeStart,
    iInputScanType,
    iPosition,
    iFilterEnable,

    -- * InputClipping
    InputClipping (..),
    mkInputClipping,
    icEndTimecode,
    icStartTimecode,

    -- * InputDecryptionSettings
    InputDecryptionSettings (..),
    mkInputDecryptionSettings,
    idsEncryptedDecryptionKey,
    idsKMSKeyRegion,
    idsDecryptionMode,
    idsInitializationVector,

    -- * InputTemplate
    InputTemplate (..),
    mkInputTemplate,
    itVideoSelector,
    itProgramNumber,
    itAudioSelectorGroups,
    itTimecodeSource,
    itAudioSelectors,
    itDeblockFilter,
    itInputClippings,
    itCrop,
    itDenoiseFilter,
    itImageInserter,
    itFilterStrength,
    itPsiControl,
    itCaptionSelectors,
    itTimecodeStart,
    itInputScanType,
    itPosition,
    itFilterEnable,

    -- * InsertableImage
    InsertableImage (..),
    mkInsertableImage,
    iiImageX,
    iiHeight,
    iiStartTime,
    iiFadeOut,
    iiWidth,
    iiOpacity,
    iiLayer,
    iiDuration,
    iiImageY,
    iiImageInserterInput,
    iiFadeIn,

    -- * Job
    Job (..),
    mkJob,
    jStatus,
    jJobTemplate,
    jAccelerationSettings,
    jPriority,
    jStatusUpdateInterval,
    jARN,
    jCreatedAt,
    jHopDestinations,
    jRetryCount,
    jSimulateReservedQueue,
    jCurrentPhase,
    jQueue,
    jUserMetadata,
    jBillingTagsSource,
    jOutputGroupDetails,
    jErrorCode,
    jQueueTransitions,
    jId,
    jJobPercentComplete,
    jTiming,
    jMessages,
    jErrorMessage,
    jAccelerationStatus,
    jRole,
    jSettings,

    -- * JobMessages
    JobMessages (..),
    mkJobMessages,
    jmWarning,
    jmInfo,

    -- * JobSettings
    JobSettings (..),
    mkJobSettings,
    jsNielsenNonLinearWatermark,
    jsEsam,
    jsInputs,
    jsTimedMetadataInsertion,
    jsNielsenConfiguration,
    jsAvailBlanking,
    jsMotionImageInserter,
    jsTimecodeConfig,
    jsOutputGroups,
    jsAdAvailOffset,

    -- * JobTemplate
    JobTemplate (..),
    mkJobTemplate,
    jtAccelerationSettings,
    jtLastUpdated,
    jtPriority,
    jtStatusUpdateInterval,
    jtARN,
    jtCreatedAt,
    jtCategory,
    jtHopDestinations,
    jtQueue,
    jtType,
    jtDescription,
    jtSettings,
    jtName,

    -- * JobTemplateSettings
    JobTemplateSettings (..),
    mkJobTemplateSettings,
    jtsNielsenNonLinearWatermark,
    jtsEsam,
    jtsInputs,
    jtsTimedMetadataInsertion,
    jtsNielsenConfiguration,
    jtsAvailBlanking,
    jtsMotionImageInserter,
    jtsTimecodeConfig,
    jtsOutputGroups,
    jtsAdAvailOffset,

    -- * M2tsScte35Esam
    M2tsScte35Esam (..),
    mkM2tsScte35Esam,
    mseScte35EsamPid,

    -- * M2tsSettings
    M2tsSettings (..),
    mkM2tsSettings,
    mPmtPid,
    mVideoPid,
    mBufferModel,
    mProgramNumber,
    mScte35Pid,
    mMinEbpInterval,
    mTransportStreamId,
    mMaxPcrInterval,
    mFragmentTime,
    mPrivateMetadataPid,
    mScte35Esam,
    mAudioDuration,
    mPmtInterval,
    mDvbSdtSettings,
    mNullPacketBitrate,
    mAudioBufferModel,
    mTimedMetadataPid,
    mAudioFramesPerPes,
    mPcrPid,
    mSegmentationMarkers,
    mDvbSubPids,
    mScte35Source,
    mPatInterval,
    mForceTsVideoEbpOrder,
    mEsRateInPes,
    mBitrate,
    mAudioPids,
    mDvbTeletextPid,
    mNielsenId3,
    mSegmentationTime,
    mEbpAudioInterval,
    mDvbNitSettings,
    mPcrControl,
    mEbpPlacement,
    mRateMode,
    mSegmentationStyle,
    mDvbTdtSettings,

    -- * M3u8Settings
    M3u8Settings (..),
    mkM3u8Settings,
    msPmtPid,
    msVideoPid,
    msProgramNumber,
    msScte35Pid,
    msTransportStreamId,
    msPrivateMetadataPid,
    msAudioDuration,
    msPmtInterval,
    msTimedMetadataPid,
    msAudioFramesPerPes,
    msPcrPid,
    msTimedMetadata,
    msScte35Source,
    msPatInterval,
    msAudioPids,
    msNielsenId3,
    msPcrControl,

    -- * MotionImageInserter
    MotionImageInserter (..),
    mkMotionImageInserter,
    miiFramerate,
    miiStartTime,
    miiOffset,
    miiInput,
    miiInsertionMode,
    miiPlayback,

    -- * MotionImageInsertionFramerate
    MotionImageInsertionFramerate (..),
    mkMotionImageInsertionFramerate,
    miifFramerateDenominator,
    miifFramerateNumerator,

    -- * MotionImageInsertionOffset
    MotionImageInsertionOffset (..),
    mkMotionImageInsertionOffset,
    miioImageX,
    miioImageY,

    -- * MovSettings
    MovSettings (..),
    mkMovSettings,
    msReference,
    msCslgAtom,
    msMpeg2FourCCControl,
    msPaddingControl,
    msClapAtom,

    -- * Mp2Settings
    Mp2Settings (..),
    mkMp2Settings,
    mssChannels,
    mssSampleRate,
    mssBitrate,

    -- * Mp3Settings
    Mp3Settings (..),
    mkMp3Settings,
    mp3Channels,
    mp3RateControlMode,
    mp3SampleRate,
    mp3Bitrate,
    mp3VbrQuality,

    -- * Mp4Settings
    Mp4Settings (..),
    mkMp4Settings,
    mssMoovPlacement,
    mssCttsVersion,
    mssFreeSpaceBox,
    mssAudioDuration,
    mssMp4MajorBrand,
    mssCslgAtom,

    -- * MpdSettings
    MpdSettings (..),
    mkMpdSettings,
    mpdScte35Esam,
    mpdAudioDuration,
    mpdScte35Source,
    mpdAccessibilityCaptionHints,
    mpdCaptionContainerType,

    -- * Mpeg2Settings
    Mpeg2Settings (..),
    mkMpeg2Settings,
    msQualityTuningLevel,
    msTemporalAdaptiveQuantization,
    msSceneChangeDetect,
    msHrdBufferInitialFillPercentage,
    msSlowPal,
    msParNumerator,
    msGopSize,
    msNumberBFramesBetweenReferenceFrames,
    msGopSizeUnits,
    msHrdBufferSize,
    msRateControlMode,
    msTelecine,
    msIntraDcPrecision,
    msDynamicSubGop,
    msMinIInterval,
    msInterlaceMode,
    msParControl,
    msSoftness,
    msCodecProfile,
    msBitrate,
    msFramerateDenominator,
    msFramerateConversionAlgorithm,
    msCodecLevel,
    msFramerateControl,
    msAdaptiveQuantization,
    msFramerateNumerator,
    msMaxBitrate,
    msSyntax,
    msGopClosedCadence,
    msParDenominator,
    msSpatialAdaptiveQuantization,

    -- * MsSmoothAdditionalManifest
    MsSmoothAdditionalManifest (..),
    mkMsSmoothAdditionalManifest,
    msamManifestNameModifier,
    msamSelectedOutputs,

    -- * MsSmoothEncryptionSettings
    MsSmoothEncryptionSettings (..),
    mkMsSmoothEncryptionSettings,
    msesSpekeKeyProvider,

    -- * MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    mkMsSmoothGroupSettings,
    msgsFragmentLength,
    msgsManifestEncoding,
    msgsDestination,
    msgsAudioDeduplication,
    msgsAdditionalManifests,
    msgsDestinationSettings,
    msgsEncryption,

    -- * MxfSettings
    MxfSettings (..),
    mkMxfSettings,
    msAfdSignaling,
    msProfile,

    -- * NexGuardFileMarkerSettings
    NexGuardFileMarkerSettings (..),
    mkNexGuardFileMarkerSettings,
    ngfmsStrength,
    ngfmsPayload,
    ngfmsPreset,
    ngfmsLicense,

    -- * NielsenConfiguration
    NielsenConfiguration (..),
    mkNielsenConfiguration,
    ncBreakoutCode,
    ncDistributorId,

    -- * NielsenNonLinearWatermarkSettings
    NielsenNonLinearWatermarkSettings (..),
    mkNielsenNonLinearWatermarkSettings,
    nnlwsEpisodeId,
    nnlwsActiveWatermarkProcess,
    nnlwsSourceId,
    nnlwsCbetSourceId,
    nnlwsTicServerURL,
    nnlwsMetadataDestination,
    nnlwsAssetName,
    nnlwsAdiFilename,
    nnlwsAssetId,
    nnlwsUniqueTicPerAudioTrack,
    nnlwsSourceWatermarkStatus,

    -- * NoiseReducer
    NoiseReducer (..),
    mkNoiseReducer,
    nrTemporalFilterSettings,
    nrSpatialFilterSettings,
    nrFilterSettings,
    nrFilter,

    -- * NoiseReducerFilterSettings
    NoiseReducerFilterSettings (..),
    mkNoiseReducerFilterSettings,
    nrfsStrength,

    -- * NoiseReducerSpatialFilterSettings
    NoiseReducerSpatialFilterSettings (..),
    mkNoiseReducerSpatialFilterSettings,
    nrsfsStrength,
    nrsfsPostFilterSharpenStrength,
    nrsfsSpeed,

    -- * NoiseReducerTemporalFilterSettings
    NoiseReducerTemporalFilterSettings (..),
    mkNoiseReducerTemporalFilterSettings,
    nrtfsPostTemporalSharpening,
    nrtfsAggressiveMode,
    nrtfsStrength,
    nrtfsSpeed,

    -- * OpusSettings
    OpusSettings (..),
    mkOpusSettings,
    osChannels,
    osSampleRate,
    osBitrate,

    -- * Output
    Output (..),
    mkOutput,
    oCaptionDescriptions,
    oExtension,
    oVideoDescription,
    oContainerSettings,
    oOutputSettings,
    oPreset,
    oNameModifier,
    oAudioDescriptions,

    -- * OutputChannelMapping
    OutputChannelMapping (..),
    mkOutputChannelMapping,
    ocmInputChannels,

    -- * OutputDetail
    OutputDetail (..),
    mkOutputDetail,
    odVideoDetails,
    odDurationInMs,

    -- * OutputGroup
    OutputGroup (..),
    mkOutputGroup,
    ogOutputGroupSettings,
    ogOutputs,
    ogCustomName,
    ogName,
    ogAutomatedEncodingSettings,

    -- * OutputGroupDetail
    OutputGroupDetail (..),
    mkOutputGroupDetail,
    ogdOutputDetails,

    -- * OutputGroupSettings
    OutputGroupSettings (..),
    mkOutputGroupSettings,
    ogsFileGroupSettings,
    ogsCmafGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsHlsGroupSettings,
    ogsType,
    ogsDashIsoGroupSettings,

    -- * OutputSettings
    OutputSettings (..),
    mkOutputSettings,
    osHlsSettings,

    -- * PartnerWatermarking
    PartnerWatermarking (..),
    mkPartnerWatermarking,
    pwNexguardFileMarkerSettings,

    -- * Preset
    Preset (..),
    mkPreset,
    pLastUpdated,
    pARN,
    pCreatedAt,
    pCategory,
    pType,
    pDescription,
    pSettings,
    pName,

    -- * PresetSettings
    PresetSettings (..),
    mkPresetSettings,
    psCaptionDescriptions,
    psVideoDescription,
    psContainerSettings,
    psAudioDescriptions,

    -- * ProresSettings
    ProresSettings (..),
    mkProresSettings,
    psSlowPal,
    psParNumerator,
    psTelecine,
    psInterlaceMode,
    psParControl,
    psCodecProfile,
    psFramerateDenominator,
    psFramerateConversionAlgorithm,
    psFramerateControl,
    psFramerateNumerator,
    psParDenominator,

    -- * Queue
    Queue (..),
    mkQueue,
    qStatus,
    qLastUpdated,
    qARN,
    qCreatedAt,
    qReservationPlan,
    qPricingPlan,
    qSubmittedJobsCount,
    qProgressingJobsCount,
    qType,
    qDescription,
    qName,

    -- * QueueTransition
    QueueTransition (..),
    mkQueueTransition,
    qtSourceQueue,
    qtDestinationQueue,
    qtTimestamp,

    -- * Rectangle
    Rectangle (..),
    mkRectangle,
    rHeight,
    rWidth,
    rX,
    rY,

    -- * RemixSettings
    RemixSettings (..),
    mkRemixSettings,
    rsChannelMapping,
    rsChannelsIn,
    rsChannelsOut,

    -- * ReservationPlan
    ReservationPlan (..),
    mkReservationPlan,
    rpStatus,
    rpExpiresAt,
    rpPurchasedAt,
    rpCommitment,
    rpReservedSlots,
    rpRenewalType,

    -- * ReservationPlanSettings
    ReservationPlanSettings (..),
    mkReservationPlanSettings,
    rpsCommitment,
    rpsReservedSlots,
    rpsRenewalType,

    -- * ResourceTags
    ResourceTags (..),
    mkResourceTags,
    rtARN,
    rtTags,

    -- * S3DestinationAccessControl
    S3DestinationAccessControl (..),
    mkS3DestinationAccessControl,
    sdacCannedACL,

    -- * S3DestinationSettings
    S3DestinationSettings (..),
    mkS3DestinationSettings,
    sdsAccessControl,
    sdsEncryption,

    -- * S3EncryptionSettings
    S3EncryptionSettings (..),
    mkS3EncryptionSettings,
    sesEncryptionType,
    sesKMSKeyARN,

    -- * SccDestinationSettings
    SccDestinationSettings (..),
    mkSccDestinationSettings,
    sdsFramerate,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    mkSpekeKeyProvider,
    sResourceId,
    sCertificateARN,
    sURL,
    sSystemIds,

    -- * SpekeKeyProviderCmaf
    SpekeKeyProviderCmaf (..),
    mkSpekeKeyProviderCmaf,
    skpcResourceId,
    skpcDashSignaledSystemIds,
    skpcCertificateARN,
    skpcURL,
    skpcHlsSignaledSystemIds,

    -- * StaticKeyProvider
    StaticKeyProvider (..),
    mkStaticKeyProvider,
    skpStaticKeyValue,
    skpURL,
    skpKeyFormat,
    skpKeyFormatVersions,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings (..),
    mkTeletextDestinationSettings,
    tdsPageTypes,
    tdsPageNumber,

    -- * TeletextSourceSettings
    TeletextSourceSettings (..),
    mkTeletextSourceSettings,
    tssPageNumber,

    -- * TimecodeBurnin
    TimecodeBurnin (..),
    mkTimecodeBurnin,
    tbPrefix,
    tbFontSize,
    tbPosition,

    -- * TimecodeConfig
    TimecodeConfig (..),
    mkTimecodeConfig,
    tcStart,
    tcTimestampOffset,
    tcAnchor,
    tcSource,

    -- * TimedMetadataInsertion
    TimedMetadataInsertion (..),
    mkTimedMetadataInsertion,
    tmiId3Insertions,

    -- * Timing
    Timing (..),
    mkTiming,
    tStartTime,
    tFinishTime,
    tSubmitTime,

    -- * TrackSourceSettings
    TrackSourceSettings (..),
    mkTrackSourceSettings,
    tssTrackNumber,

    -- * TtmlDestinationSettings
    TtmlDestinationSettings (..),
    mkTtmlDestinationSettings,
    tdsStylePassthrough,

    -- * Vc3Settings
    Vc3Settings (..),
    mkVc3Settings,
    vssSlowPal,
    vssTelecine,
    vssInterlaceMode,
    vssFramerateDenominator,
    vssVc3Class,
    vssFramerateConversionAlgorithm,
    vssFramerateControl,
    vssFramerateNumerator,

    -- * VideoCodecSettings
    VideoCodecSettings (..),
    mkVideoCodecSettings,
    vcsFrameCaptureSettings,
    vcsAv1Settings,
    vcsCodec,
    vcsH265Settings,
    vcsProresSettings,
    vcsVp9Settings,
    vcsH264Settings,
    vcsMpeg2Settings,
    vcsVp8Settings,
    vcsVc3Settings,
    vcsAvcIntraSettings,

    -- * VideoDescription
    VideoDescription (..),
    mkVideoDescription,
    vdTimecodeInsertion,
    vdHeight,
    vdAfdSignaling,
    vdSharpness,
    vdCrop,
    vdWidth,
    vdScalingBehavior,
    vdRespondToAfd,
    vdDropFrameTimecode,
    vdAntiAlias,
    vdFixedAfd,
    vdColorMetadata,
    vdCodecSettings,
    vdVideoPreprocessors,
    vdPosition,

    -- * VideoDetail
    VideoDetail (..),
    mkVideoDetail,
    vdHeightInPx,
    vdWidthInPx,

    -- * VideoPreprocessor
    VideoPreprocessor (..),
    mkVideoPreprocessor,
    vpTimecodeBurnin,
    vpDolbyVision,
    vpColorCorrector,
    vpDeinterlacer,
    vpNoiseReducer,
    vpImageInserter,
    vpPartnerWatermarking,

    -- * VideoSelector
    VideoSelector (..),
    mkVideoSelector,
    vsProgramNumber,
    vsAlphaBehavior,
    vsColorSpaceUsage,
    vsHdr10Metadata,
    vsPid,
    vsRotate,
    vsColorSpace,

    -- * VorbisSettings
    VorbisSettings (..),
    mkVorbisSettings,
    vsChannels,
    vsSampleRate,
    vsVbrQuality,

    -- * Vp8Settings
    Vp8Settings (..),
    mkVp8Settings,
    vQualityTuningLevel,
    vParNumerator,
    vGopSize,
    vHrdBufferSize,
    vRateControlMode,
    vParControl,
    vBitrate,
    vFramerateDenominator,
    vFramerateConversionAlgorithm,
    vFramerateControl,
    vFramerateNumerator,
    vMaxBitrate,
    vParDenominator,

    -- * Vp9Settings
    Vp9Settings (..),
    mkVp9Settings,
    vsQualityTuningLevel,
    vsParNumerator,
    vsGopSize,
    vsHrdBufferSize,
    vsRateControlMode,
    vsParControl,
    vsBitrate,
    vsFramerateDenominator,
    vsFramerateConversionAlgorithm,
    vsFramerateControl,
    vsFramerateNumerator,
    vsMaxBitrate,
    vsParDenominator,

    -- * WavSettings
    WavSettings (..),
    mkWavSettings,
    wsBitDepth,
    wsChannels,
    wsFormat,
    wsSampleRate,
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
import Network.AWS.MediaConvert.Types.AvcIntraSettings
import Network.AWS.MediaConvert.Types.AvcIntraSlowPal
import Network.AWS.MediaConvert.Types.AvcIntraTelecine
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
import Network.AWS.MediaConvert.Types.S3ObjectCannedACL
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-08-29@ of the Amazon Elemental MediaConvert SDK configuration.
mediaConvertService :: Lude.Service
mediaConvertService =
  Lude.Service
    { Lude._svcAbbrev = "MediaConvert",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "mediaconvert",
      Lude._svcVersion = "2017-08-29",
      Lude._svcEndpoint = Lude.defaultEndpoint mediaConvertService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "MediaConvert",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
