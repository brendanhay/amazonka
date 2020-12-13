{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elemental MediaConvert
module Network.AWS.MediaConvert
  ( -- * Service configuration
    mediaConvertService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeletePreset
    module Network.AWS.MediaConvert.DeletePreset,

    -- ** UpdatePreset
    module Network.AWS.MediaConvert.UpdatePreset,

    -- ** ListTagsForResource
    module Network.AWS.MediaConvert.ListTagsForResource,

    -- ** ListQueues (Paginated)
    module Network.AWS.MediaConvert.ListQueues,

    -- ** DeleteQueue
    module Network.AWS.MediaConvert.DeleteQueue,

    -- ** UpdateQueue
    module Network.AWS.MediaConvert.UpdateQueue,

    -- ** GetPreset
    module Network.AWS.MediaConvert.GetPreset,

    -- ** CreateJob
    module Network.AWS.MediaConvert.CreateJob,

    -- ** ListJobs (Paginated)
    module Network.AWS.MediaConvert.ListJobs,

    -- ** GetJob
    module Network.AWS.MediaConvert.GetJob,

    -- ** CreatePreset
    module Network.AWS.MediaConvert.CreatePreset,

    -- ** ListPresets (Paginated)
    module Network.AWS.MediaConvert.ListPresets,

    -- ** DisassociateCertificate
    module Network.AWS.MediaConvert.DisassociateCertificate,

    -- ** GetQueue
    module Network.AWS.MediaConvert.GetQueue,

    -- ** DescribeEndpoints (Paginated)
    module Network.AWS.MediaConvert.DescribeEndpoints,

    -- ** CreateQueue
    module Network.AWS.MediaConvert.CreateQueue,

    -- ** TagResource
    module Network.AWS.MediaConvert.TagResource,

    -- ** CreateJobTemplate
    module Network.AWS.MediaConvert.CreateJobTemplate,

    -- ** UntagResource
    module Network.AWS.MediaConvert.UntagResource,

    -- ** DeleteJobTemplate
    module Network.AWS.MediaConvert.DeleteJobTemplate,

    -- ** UpdateJobTemplate
    module Network.AWS.MediaConvert.UpdateJobTemplate,

    -- ** ListJobTemplates (Paginated)
    module Network.AWS.MediaConvert.ListJobTemplates,

    -- ** GetJobTemplate
    module Network.AWS.MediaConvert.GetJobTemplate,

    -- ** AssociateCertificate
    module Network.AWS.MediaConvert.AssociateCertificate,

    -- ** CancelJob
    module Network.AWS.MediaConvert.CancelJob,

    -- * Types

    -- ** AacAudioDescriptionBroadcasterMix
    AacAudioDescriptionBroadcasterMix (..),

    -- ** AacCodecProfile
    AacCodecProfile (..),

    -- ** AacCodingMode
    AacCodingMode (..),

    -- ** AacRateControlMode
    AacRateControlMode (..),

    -- ** AacRawFormat
    AacRawFormat (..),

    -- ** AacSpecification
    AacSpecification (..),

    -- ** AacVbrQuality
    AacVbrQuality (..),

    -- ** Ac3BitstreamMode
    Ac3BitstreamMode (..),

    -- ** Ac3CodingMode
    Ac3CodingMode (..),

    -- ** Ac3DynamicRangeCompressionProfile
    Ac3DynamicRangeCompressionProfile (..),

    -- ** Ac3LfeFilter
    Ac3LfeFilter (..),

    -- ** Ac3MetadataControl
    Ac3MetadataControl (..),

    -- ** AccelerationMode
    AccelerationMode (..),

    -- ** AccelerationStatus
    AccelerationStatus (..),

    -- ** AfdSignaling
    AfdSignaling (..),

    -- ** AlphaBehavior
    AlphaBehavior (..),

    -- ** AncillaryConvert608To708
    AncillaryConvert608To708 (..),

    -- ** AncillaryTerminateCaptions
    AncillaryTerminateCaptions (..),

    -- ** AntiAlias
    AntiAlias (..),

    -- ** AudioChannelTag
    AudioChannelTag (..),

    -- ** AudioCodec
    AudioCodec (..),

    -- ** AudioDefaultSelection
    AudioDefaultSelection (..),

    -- ** AudioLanguageCodeControl
    AudioLanguageCodeControl (..),

    -- ** AudioNormalizationAlgorithm
    AudioNormalizationAlgorithm (..),

    -- ** AudioNormalizationAlgorithmControl
    AudioNormalizationAlgorithmControl (..),

    -- ** AudioNormalizationLoudnessLogging
    AudioNormalizationLoudnessLogging (..),

    -- ** AudioNormalizationPeakCalculation
    AudioNormalizationPeakCalculation (..),

    -- ** AudioSelectorType
    AudioSelectorType (..),

    -- ** AudioTypeControl
    AudioTypeControl (..),

    -- ** Av1AdaptiveQuantization
    Av1AdaptiveQuantization (..),

    -- ** Av1FramerateControl
    Av1FramerateControl (..),

    -- ** Av1FramerateConversionAlgorithm
    Av1FramerateConversionAlgorithm (..),

    -- ** Av1RateControlMode
    Av1RateControlMode (..),

    -- ** Av1SpatialAdaptiveQuantization
    Av1SpatialAdaptiveQuantization (..),

    -- ** AvcIntraClass
    AvcIntraClass (..),

    -- ** AvcIntraFramerateControl
    AvcIntraFramerateControl (..),

    -- ** AvcIntraFramerateConversionAlgorithm
    AvcIntraFramerateConversionAlgorithm (..),

    -- ** AvcIntraInterlaceMode
    AvcIntraInterlaceMode (..),

    -- ** AvcIntraSlowPal
    AvcIntraSlowPal (..),

    -- ** AvcIntraTelecine
    AvcIntraTelecine (..),

    -- ** BillingTagsSource
    BillingTagsSource (..),

    -- ** BurninSubtitleAlignment
    BurninSubtitleAlignment (..),

    -- ** BurninSubtitleBackgroundColor
    BurninSubtitleBackgroundColor (..),

    -- ** BurninSubtitleFontColor
    BurninSubtitleFontColor (..),

    -- ** BurninSubtitleOutlineColor
    BurninSubtitleOutlineColor (..),

    -- ** BurninSubtitleShadowColor
    BurninSubtitleShadowColor (..),

    -- ** BurninSubtitleTeletextSpacing
    BurninSubtitleTeletextSpacing (..),

    -- ** CaptionDestinationType
    CaptionDestinationType (..),

    -- ** CaptionSourceType
    CaptionSourceType (..),

    -- ** CmafClientCache
    CmafClientCache (..),

    -- ** CmafCodecSpecification
    CmafCodecSpecification (..),

    -- ** CmafEncryptionType
    CmafEncryptionType (..),

    -- ** CmafInitializationVectorInManifest
    CmafInitializationVectorInManifest (..),

    -- ** CmafKeyProviderType
    CmafKeyProviderType (..),

    -- ** CmafManifestCompression
    CmafManifestCompression (..),

    -- ** CmafManifestDurationFormat
    CmafManifestDurationFormat (..),

    -- ** CmafMpdProfile
    CmafMpdProfile (..),

    -- ** CmafSegmentControl
    CmafSegmentControl (..),

    -- ** CmafStreamInfResolution
    CmafStreamInfResolution (..),

    -- ** CmafWriteDASHManifest
    CmafWriteDASHManifest (..),

    -- ** CmafWriteHLSManifest
    CmafWriteHLSManifest (..),

    -- ** CmafWriteSegmentTimelineInRepresentation
    CmafWriteSegmentTimelineInRepresentation (..),

    -- ** CmfcAudioDuration
    CmfcAudioDuration (..),

    -- ** CmfcScte35Esam
    CmfcScte35Esam (..),

    -- ** CmfcScte35Source
    CmfcScte35Source (..),

    -- ** ColorMetadata
    ColorMetadata (..),

    -- ** ColorSpace
    ColorSpace (..),

    -- ** ColorSpaceConversion
    ColorSpaceConversion (..),

    -- ** ColorSpaceUsage
    ColorSpaceUsage (..),

    -- ** Commitment
    Commitment (..),

    -- ** ContainerType
    ContainerType (..),

    -- ** DashIsoHbbtvCompliance
    DashIsoHbbtvCompliance (..),

    -- ** DashIsoMpdProfile
    DashIsoMpdProfile (..),

    -- ** DashIsoPlaybackDeviceCompatibility
    DashIsoPlaybackDeviceCompatibility (..),

    -- ** DashIsoSegmentControl
    DashIsoSegmentControl (..),

    -- ** DashIsoWriteSegmentTimelineInRepresentation
    DashIsoWriteSegmentTimelineInRepresentation (..),

    -- ** DecryptionMode
    DecryptionMode (..),

    -- ** DeinterlaceAlgorithm
    DeinterlaceAlgorithm (..),

    -- ** DeinterlacerControl
    DeinterlacerControl (..),

    -- ** DeinterlacerMode
    DeinterlacerMode (..),

    -- ** DescribeEndpointsMode
    DescribeEndpointsMode (..),

    -- ** DolbyVisionLevel6Mode
    DolbyVisionLevel6Mode (..),

    -- ** DolbyVisionProfile
    DolbyVisionProfile (..),

    -- ** DropFrameTimecode
    DropFrameTimecode (..),

    -- ** DvbSubtitleAlignment
    DvbSubtitleAlignment (..),

    -- ** DvbSubtitleBackgroundColor
    DvbSubtitleBackgroundColor (..),

    -- ** DvbSubtitleFontColor
    DvbSubtitleFontColor (..),

    -- ** DvbSubtitleOutlineColor
    DvbSubtitleOutlineColor (..),

    -- ** DvbSubtitleShadowColor
    DvbSubtitleShadowColor (..),

    -- ** DvbSubtitleTeletextSpacing
    DvbSubtitleTeletextSpacing (..),

    -- ** DvbSubtitlingType
    DvbSubtitlingType (..),

    -- ** Eac3AtmosBitstreamMode
    Eac3AtmosBitstreamMode (..),

    -- ** Eac3AtmosCodingMode
    Eac3AtmosCodingMode (..),

    -- ** Eac3AtmosDialogueIntelligence
    Eac3AtmosDialogueIntelligence (..),

    -- ** Eac3AtmosDynamicRangeCompressionLine
    Eac3AtmosDynamicRangeCompressionLine (..),

    -- ** Eac3AtmosDynamicRangeCompressionRf
    Eac3AtmosDynamicRangeCompressionRf (..),

    -- ** Eac3AtmosMeteringMode
    Eac3AtmosMeteringMode (..),

    -- ** Eac3AtmosStereoDownmix
    Eac3AtmosStereoDownmix (..),

    -- ** Eac3AtmosSurroundExMode
    Eac3AtmosSurroundExMode (..),

    -- ** Eac3AttenuationControl
    Eac3AttenuationControl (..),

    -- ** Eac3BitstreamMode
    Eac3BitstreamMode (..),

    -- ** Eac3CodingMode
    Eac3CodingMode (..),

    -- ** Eac3DcFilter
    Eac3DcFilter (..),

    -- ** Eac3DynamicRangeCompressionLine
    Eac3DynamicRangeCompressionLine (..),

    -- ** Eac3DynamicRangeCompressionRf
    Eac3DynamicRangeCompressionRf (..),

    -- ** Eac3LfeControl
    Eac3LfeControl (..),

    -- ** Eac3LfeFilter
    Eac3LfeFilter (..),

    -- ** Eac3MetadataControl
    Eac3MetadataControl (..),

    -- ** Eac3PassthroughControl
    Eac3PassthroughControl (..),

    -- ** Eac3PhaseControl
    Eac3PhaseControl (..),

    -- ** Eac3StereoDownmix
    Eac3StereoDownmix (..),

    -- ** Eac3SurroundExMode
    Eac3SurroundExMode (..),

    -- ** Eac3SurroundMode
    Eac3SurroundMode (..),

    -- ** EmbeddedConvert608To708
    EmbeddedConvert608To708 (..),

    -- ** EmbeddedTerminateCaptions
    EmbeddedTerminateCaptions (..),

    -- ** F4vMoovPlacement
    F4vMoovPlacement (..),

    -- ** FileSourceConvert608To708
    FileSourceConvert608To708 (..),

    -- ** FontScript
    FontScript (..),

    -- ** H264AdaptiveQuantization
    H264AdaptiveQuantization (..),

    -- ** H264CodecLevel
    H264CodecLevel (..),

    -- ** H264CodecProfile
    H264CodecProfile (..),

    -- ** H264DynamicSubGop
    H264DynamicSubGop (..),

    -- ** H264EntropyEncoding
    H264EntropyEncoding (..),

    -- ** H264FieldEncoding
    H264FieldEncoding (..),

    -- ** H264FlickerAdaptiveQuantization
    H264FlickerAdaptiveQuantization (..),

    -- ** H264FramerateControl
    H264FramerateControl (..),

    -- ** H264FramerateConversionAlgorithm
    H264FramerateConversionAlgorithm (..),

    -- ** H264GopBReference
    H264GopBReference (..),

    -- ** H264GopSizeUnits
    H264GopSizeUnits (..),

    -- ** H264InterlaceMode
    H264InterlaceMode (..),

    -- ** H264ParControl
    H264ParControl (..),

    -- ** H264QualityTuningLevel
    H264QualityTuningLevel (..),

    -- ** H264RateControlMode
    H264RateControlMode (..),

    -- ** H264RepeatPps
    H264RepeatPps (..),

    -- ** H264SceneChangeDetect
    H264SceneChangeDetect (..),

    -- ** H264SlowPal
    H264SlowPal (..),

    -- ** H264SpatialAdaptiveQuantization
    H264SpatialAdaptiveQuantization (..),

    -- ** H264Syntax
    H264Syntax (..),

    -- ** H264Telecine
    H264Telecine (..),

    -- ** H264TemporalAdaptiveQuantization
    H264TemporalAdaptiveQuantization (..),

    -- ** H264UnregisteredSeiTimecode
    H264UnregisteredSeiTimecode (..),

    -- ** H265AdaptiveQuantization
    H265AdaptiveQuantization (..),

    -- ** H265AlternateTransferFunctionSei
    H265AlternateTransferFunctionSei (..),

    -- ** H265CodecLevel
    H265CodecLevel (..),

    -- ** H265CodecProfile
    H265CodecProfile (..),

    -- ** H265DynamicSubGop
    H265DynamicSubGop (..),

    -- ** H265FlickerAdaptiveQuantization
    H265FlickerAdaptiveQuantization (..),

    -- ** H265FramerateControl
    H265FramerateControl (..),

    -- ** H265FramerateConversionAlgorithm
    H265FramerateConversionAlgorithm (..),

    -- ** H265GopBReference
    H265GopBReference (..),

    -- ** H265GopSizeUnits
    H265GopSizeUnits (..),

    -- ** H265InterlaceMode
    H265InterlaceMode (..),

    -- ** H265ParControl
    H265ParControl (..),

    -- ** H265QualityTuningLevel
    H265QualityTuningLevel (..),

    -- ** H265RateControlMode
    H265RateControlMode (..),

    -- ** H265SampleAdaptiveOffsetFilterMode
    H265SampleAdaptiveOffsetFilterMode (..),

    -- ** H265SceneChangeDetect
    H265SceneChangeDetect (..),

    -- ** H265SlowPal
    H265SlowPal (..),

    -- ** H265SpatialAdaptiveQuantization
    H265SpatialAdaptiveQuantization (..),

    -- ** H265Telecine
    H265Telecine (..),

    -- ** H265TemporalAdaptiveQuantization
    H265TemporalAdaptiveQuantization (..),

    -- ** H265TemporalIds
    H265TemporalIds (..),

    -- ** H265Tiles
    H265Tiles (..),

    -- ** H265UnregisteredSeiTimecode
    H265UnregisteredSeiTimecode (..),

    -- ** H265WriteMp4PackagingType
    H265WriteMp4PackagingType (..),

    -- ** HlsAdMarkers
    HlsAdMarkers (..),

    -- ** HlsAudioOnlyContainer
    HlsAudioOnlyContainer (..),

    -- ** HlsAudioOnlyHeader
    HlsAudioOnlyHeader (..),

    -- ** HlsAudioTrackType
    HlsAudioTrackType (..),

    -- ** HlsCaptionLanguageSetting
    HlsCaptionLanguageSetting (..),

    -- ** HlsClientCache
    HlsClientCache (..),

    -- ** HlsCodecSpecification
    HlsCodecSpecification (..),

    -- ** HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- ** HlsEncryptionType
    HlsEncryptionType (..),

    -- ** HlsIFrameOnlyManifest
    HlsIFrameOnlyManifest (..),

    -- ** HlsInitializationVectorInManifest
    HlsInitializationVectorInManifest (..),

    -- ** HlsKeyProviderType
    HlsKeyProviderType (..),

    -- ** HlsManifestCompression
    HlsManifestCompression (..),

    -- ** HlsManifestDurationFormat
    HlsManifestDurationFormat (..),

    -- ** HlsOfflineEncrypted
    HlsOfflineEncrypted (..),

    -- ** HlsOutputSelection
    HlsOutputSelection (..),

    -- ** HlsProgramDateTime
    HlsProgramDateTime (..),

    -- ** HlsSegmentControl
    HlsSegmentControl (..),

    -- ** HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- ** HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- ** ImscStylePassthrough
    ImscStylePassthrough (..),

    -- ** InputDeblockFilter
    InputDeblockFilter (..),

    -- ** InputDenoiseFilter
    InputDenoiseFilter (..),

    -- ** InputFilterEnable
    InputFilterEnable (..),

    -- ** InputPsiControl
    InputPsiControl (..),

    -- ** InputRotate
    InputRotate (..),

    -- ** InputScanType
    InputScanType (..),

    -- ** InputTimecodeSource
    InputTimecodeSource (..),

    -- ** JobPhase
    JobPhase (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobTemplateListBy
    JobTemplateListBy (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** M2tsAudioBufferModel
    M2tsAudioBufferModel (..),

    -- ** M2tsAudioDuration
    M2tsAudioDuration (..),

    -- ** M2tsBufferModel
    M2tsBufferModel (..),

    -- ** M2tsEbpAudioInterval
    M2tsEbpAudioInterval (..),

    -- ** M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- ** M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- ** M2tsForceTsVideoEbpOrder
    M2tsForceTsVideoEbpOrder (..),

    -- ** M2tsNielsenId3
    M2tsNielsenId3 (..),

    -- ** M2tsPcrControl
    M2tsPcrControl (..),

    -- ** M2tsRateMode
    M2tsRateMode (..),

    -- ** M2tsScte35Source
    M2tsScte35Source (..),

    -- ** M2tsSegmentationMarkers
    M2tsSegmentationMarkers (..),

    -- ** M2tsSegmentationStyle
    M2tsSegmentationStyle (..),

    -- ** M3u8AudioDuration
    M3u8AudioDuration (..),

    -- ** M3u8NielsenId3
    M3u8NielsenId3 (..),

    -- ** M3u8PcrControl
    M3u8PcrControl (..),

    -- ** M3u8Scte35Source
    M3u8Scte35Source (..),

    -- ** MotionImageInsertionMode
    MotionImageInsertionMode (..),

    -- ** MotionImagePlayback
    MotionImagePlayback (..),

    -- ** MovClapAtom
    MovClapAtom (..),

    -- ** MovCslgAtom
    MovCslgAtom (..),

    -- ** MovMpeg2FourCCControl
    MovMpeg2FourCCControl (..),

    -- ** MovPaddingControl
    MovPaddingControl (..),

    -- ** MovReference
    MovReference (..),

    -- ** Mp3RateControlMode
    Mp3RateControlMode (..),

    -- ** Mp4CslgAtom
    Mp4CslgAtom (..),

    -- ** Mp4FreeSpaceBox
    Mp4FreeSpaceBox (..),

    -- ** Mp4MoovPlacement
    Mp4MoovPlacement (..),

    -- ** MpdAccessibilityCaptionHints
    MpdAccessibilityCaptionHints (..),

    -- ** MpdAudioDuration
    MpdAudioDuration (..),

    -- ** MpdCaptionContainerType
    MpdCaptionContainerType (..),

    -- ** MpdScte35Esam
    MpdScte35Esam (..),

    -- ** MpdScte35Source
    MpdScte35Source (..),

    -- ** Mpeg2AdaptiveQuantization
    Mpeg2AdaptiveQuantization (..),

    -- ** Mpeg2CodecLevel
    Mpeg2CodecLevel (..),

    -- ** Mpeg2CodecProfile
    Mpeg2CodecProfile (..),

    -- ** Mpeg2DynamicSubGop
    Mpeg2DynamicSubGop (..),

    -- ** Mpeg2FramerateControl
    Mpeg2FramerateControl (..),

    -- ** Mpeg2FramerateConversionAlgorithm
    Mpeg2FramerateConversionAlgorithm (..),

    -- ** Mpeg2GopSizeUnits
    Mpeg2GopSizeUnits (..),

    -- ** Mpeg2InterlaceMode
    Mpeg2InterlaceMode (..),

    -- ** Mpeg2IntraDcPrecision
    Mpeg2IntraDcPrecision (..),

    -- ** Mpeg2ParControl
    Mpeg2ParControl (..),

    -- ** Mpeg2QualityTuningLevel
    Mpeg2QualityTuningLevel (..),

    -- ** Mpeg2RateControlMode
    Mpeg2RateControlMode (..),

    -- ** Mpeg2SceneChangeDetect
    Mpeg2SceneChangeDetect (..),

    -- ** Mpeg2SlowPal
    Mpeg2SlowPal (..),

    -- ** Mpeg2SpatialAdaptiveQuantization
    Mpeg2SpatialAdaptiveQuantization (..),

    -- ** Mpeg2Syntax
    Mpeg2Syntax (..),

    -- ** Mpeg2Telecine
    Mpeg2Telecine (..),

    -- ** Mpeg2TemporalAdaptiveQuantization
    Mpeg2TemporalAdaptiveQuantization (..),

    -- ** MsSmoothAudioDeduplication
    MsSmoothAudioDeduplication (..),

    -- ** MsSmoothManifestEncoding
    MsSmoothManifestEncoding (..),

    -- ** MxfAfdSignaling
    MxfAfdSignaling (..),

    -- ** MxfProfile
    MxfProfile (..),

    -- ** NielsenActiveWatermarkProcessType
    NielsenActiveWatermarkProcessType (..),

    -- ** NielsenSourceWatermarkStatusType
    NielsenSourceWatermarkStatusType (..),

    -- ** NielsenUniqueTicPerAudioTrackType
    NielsenUniqueTicPerAudioTrackType (..),

    -- ** NoiseFilterPostTemporalSharpening
    NoiseFilterPostTemporalSharpening (..),

    -- ** NoiseReducerFilter
    NoiseReducerFilter (..),

    -- ** Order
    Order (..),

    -- ** OutputGroupType
    OutputGroupType (..),

    -- ** OutputSdt
    OutputSdt (..),

    -- ** PresetListBy
    PresetListBy (..),

    -- ** PricingPlan
    PricingPlan (..),

    -- ** ProresCodecProfile
    ProresCodecProfile (..),

    -- ** ProresFramerateControl
    ProresFramerateControl (..),

    -- ** ProresFramerateConversionAlgorithm
    ProresFramerateConversionAlgorithm (..),

    -- ** ProresInterlaceMode
    ProresInterlaceMode (..),

    -- ** ProresParControl
    ProresParControl (..),

    -- ** ProresSlowPal
    ProresSlowPal (..),

    -- ** ProresTelecine
    ProresTelecine (..),

    -- ** QueueListBy
    QueueListBy (..),

    -- ** QueueStatus
    QueueStatus (..),

    -- ** RenewalType
    RenewalType (..),

    -- ** ReservationPlanStatus
    ReservationPlanStatus (..),

    -- ** RespondToAfd
    RespondToAfd (..),

    -- ** S3ObjectCannedACL
    S3ObjectCannedACL (..),

    -- ** S3ServerSideEncryptionType
    S3ServerSideEncryptionType (..),

    -- ** ScalingBehavior
    ScalingBehavior (..),

    -- ** SccDestinationFramerate
    SccDestinationFramerate (..),

    -- ** SimulateReservedQueue
    SimulateReservedQueue (..),

    -- ** StatusUpdateInterval
    StatusUpdateInterval (..),

    -- ** TeletextPageType
    TeletextPageType (..),

    -- ** TimecodeBurninPosition
    TimecodeBurninPosition (..),

    -- ** TimecodeSource
    TimecodeSource (..),

    -- ** TimedMetadata
    TimedMetadata (..),

    -- ** TtmlStylePassthrough
    TtmlStylePassthrough (..),

    -- ** Type
    Type (..),

    -- ** Vc3Class
    Vc3Class (..),

    -- ** Vc3FramerateControl
    Vc3FramerateControl (..),

    -- ** Vc3FramerateConversionAlgorithm
    Vc3FramerateConversionAlgorithm (..),

    -- ** Vc3InterlaceMode
    Vc3InterlaceMode (..),

    -- ** Vc3SlowPal
    Vc3SlowPal (..),

    -- ** Vc3Telecine
    Vc3Telecine (..),

    -- ** VideoCodec
    VideoCodec (..),

    -- ** VideoTimecodeInsertion
    VideoTimecodeInsertion (..),

    -- ** Vp8FramerateControl
    Vp8FramerateControl (..),

    -- ** Vp8FramerateConversionAlgorithm
    Vp8FramerateConversionAlgorithm (..),

    -- ** Vp8ParControl
    Vp8ParControl (..),

    -- ** Vp8QualityTuningLevel
    Vp8QualityTuningLevel (..),

    -- ** Vp8RateControlMode
    Vp8RateControlMode (..),

    -- ** Vp9FramerateControl
    Vp9FramerateControl (..),

    -- ** Vp9FramerateConversionAlgorithm
    Vp9FramerateConversionAlgorithm (..),

    -- ** Vp9ParControl
    Vp9ParControl (..),

    -- ** Vp9QualityTuningLevel
    Vp9QualityTuningLevel (..),

    -- ** Vp9RateControlMode
    Vp9RateControlMode (..),

    -- ** WatermarkingStrength
    WatermarkingStrength (..),

    -- ** WavFormat
    WavFormat (..),

    -- ** AacSettings
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

    -- ** Ac3Settings
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

    -- ** AccelerationSettings
    AccelerationSettings (..),
    mkAccelerationSettings,
    asMode,

    -- ** AiffSettings
    AiffSettings (..),
    mkAiffSettings,
    asBitDepth,
    asChannels,
    asSampleRate,

    -- ** AncillarySourceSettings
    AncillarySourceSettings (..),
    mkAncillarySourceSettings,
    assConvert608To708,
    assTerminateCaptions,
    assSourceAncillaryChannelNumber,

    -- ** AudioChannelTaggingSettings
    AudioChannelTaggingSettings (..),
    mkAudioChannelTaggingSettings,
    actsChannelTag,

    -- ** AudioCodecSettings
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

    -- ** AudioDescription
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

    -- ** AudioNormalizationSettings
    AudioNormalizationSettings (..),
    mkAudioNormalizationSettings,
    ansAlgorithmControl,
    ansTargetLkfs,
    ansPeakCalculation,
    ansCorrectionGateLevel,
    ansAlgorithm,
    ansLoudnessLogging,

    -- ** AudioSelector
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

    -- ** AudioSelectorGroup
    AudioSelectorGroup (..),
    mkAudioSelectorGroup,
    asgAudioSelectorNames,

    -- ** AutomatedAbrSettings
    AutomatedAbrSettings (..),
    mkAutomatedAbrSettings,
    aasMaxRenditions,
    aasMaxAbrBitrate,
    aasMinAbrBitrate,

    -- ** AutomatedEncodingSettings
    AutomatedEncodingSettings (..),
    mkAutomatedEncodingSettings,
    aesAbrSettings,

    -- ** Av1QvbrSettings
    Av1QvbrSettings (..),
    mkAv1QvbrSettings,
    aqsQvbrQualityLevelFineTune,
    aqsQvbrQualityLevel,

    -- ** Av1Settings
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

    -- ** AvailBlanking
    AvailBlanking (..),
    mkAvailBlanking,
    abAvailBlankingImage,

    -- ** AvcIntraSettings
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

    -- ** BurninDestinationSettings
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

    -- ** CaptionDescription
    CaptionDescription (..),
    mkCaptionDescription,
    cdCaptionSelectorName,
    cdCustomLanguageCode,
    cdLanguageCode,
    cdDestinationSettings,
    cdLanguageDescription,

    -- ** CaptionDescriptionPreset
    CaptionDescriptionPreset (..),
    mkCaptionDescriptionPreset,
    cdpCustomLanguageCode,
    cdpLanguageCode,
    cdpDestinationSettings,
    cdpLanguageDescription,

    -- ** CaptionDestinationSettings
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

    -- ** CaptionSelector
    CaptionSelector (..),
    mkCaptionSelector,
    csCustomLanguageCode,
    csLanguageCode,
    csSourceSettings,

    -- ** CaptionSourceFramerate
    CaptionSourceFramerate (..),
    mkCaptionSourceFramerate,
    csfFramerateDenominator,
    csfFramerateNumerator,

    -- ** CaptionSourceSettings
    CaptionSourceSettings (..),
    mkCaptionSourceSettings,
    cssTeletextSourceSettings,
    cssSourceType,
    cssFileSourceSettings,
    cssDvbSubSourceSettings,
    cssTrackSourceSettings,
    cssAncillarySourceSettings,
    cssEmbeddedSourceSettings,

    -- ** ChannelMapping
    ChannelMapping (..),
    mkChannelMapping,
    cmOutputChannels,

    -- ** CmafAdditionalManifest
    CmafAdditionalManifest (..),
    mkCmafAdditionalManifest,
    camManifestNameModifier,
    camSelectedOutputs,

    -- ** CmafEncryptionSettings
    CmafEncryptionSettings (..),
    mkCmafEncryptionSettings,
    cesEncryptionMethod,
    cesConstantInitializationVector,
    cesType,
    cesStaticKeyProvider,
    cesSpekeKeyProvider,
    cesInitializationVectorInManifest,

    -- ** CmafGroupSettings
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

    -- ** CmfcSettings
    CmfcSettings (..),
    mkCmfcSettings,
    csScte35Esam,
    csAudioDuration,
    csScte35Source,

    -- ** ColorCorrector
    ColorCorrector (..),
    mkColorCorrector,
    ccSaturation,
    ccHue,
    ccColorSpaceConversion,
    ccHdr10Metadata,
    ccContrast,
    ccBrightness,

    -- ** ContainerSettings
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

    -- ** DashAdditionalManifest
    DashAdditionalManifest (..),
    mkDashAdditionalManifest,
    damManifestNameModifier,
    damSelectedOutputs,

    -- ** DashIsoEncryptionSettings
    DashIsoEncryptionSettings (..),
    mkDashIsoEncryptionSettings,
    diesPlaybackDeviceCompatibility,
    diesSpekeKeyProvider,

    -- ** DashIsoGroupSettings
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

    -- ** Deinterlacer
    Deinterlacer (..),
    mkDeinterlacer,
    dControl,
    dMode,
    dAlgorithm,

    -- ** DestinationSettings
    DestinationSettings (..),
    mkDestinationSettings,
    dsS3Settings,

    -- ** DolbyVision
    DolbyVision (..),
    mkDolbyVision,
    dvProfile,
    dvL6Mode,
    dvL6Metadata,

    -- ** DolbyVisionLevel6Metadata
    DolbyVisionLevel6Metadata (..),
    mkDolbyVisionLevel6Metadata,
    dvlmMaxFall,
    dvlmMaxCll,

    -- ** DvbNitSettings
    DvbNitSettings (..),
    mkDvbNitSettings,
    dnsNetworkId,
    dnsNetworkName,
    dnsNitInterval,

    -- ** DvbSdtSettings
    DvbSdtSettings (..),
    mkDvbSdtSettings,
    dssSdtInterval,
    dssServiceProviderName,
    dssOutputSdt,
    dssServiceName,

    -- ** DvbSubDestinationSettings
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

    -- ** DvbSubSourceSettings
    DvbSubSourceSettings (..),
    mkDvbSubSourceSettings,
    dsssPid,

    -- ** DvbTdtSettings
    DvbTdtSettings (..),
    mkDvbTdtSettings,
    dtsTdtInterval,

    -- ** Eac3AtmosSettings
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

    -- ** Eac3Settings
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

    -- ** EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    mkEmbeddedDestinationSettings,
    edsDestination608ChannelNumber,
    edsDestination708ServiceNumber,

    -- ** EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    mkEmbeddedSourceSettings,
    essConvert608To708,
    essTerminateCaptions,
    essSource608TrackNumber,
    essSource608ChannelNumber,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eURL,

    -- ** EsamManifestConfirmConditionNotification
    EsamManifestConfirmConditionNotification (..),
    mkEsamManifestConfirmConditionNotification,
    emccnMccXML,

    -- ** EsamSettings
    EsamSettings (..),
    mkEsamSettings,
    esManifestConfirmConditionNotification,
    esResponseSignalPreroll,
    esSignalProcessingNotification,

    -- ** EsamSignalProcessingNotification
    EsamSignalProcessingNotification (..),
    mkEsamSignalProcessingNotification,
    espnSccXML,

    -- ** F4vSettings
    F4vSettings (..),
    mkF4vSettings,
    fsMoovPlacement,

    -- ** FileGroupSettings
    FileGroupSettings (..),
    mkFileGroupSettings,
    fgsDestination,
    fgsDestinationSettings,

    -- ** FileSourceSettings
    FileSourceSettings (..),
    mkFileSourceSettings,
    fssFramerate,
    fssConvert608To708,
    fssTimeDelta,
    fssSourceFile,

    -- ** FrameCaptureSettings
    FrameCaptureSettings (..),
    mkFrameCaptureSettings,
    fcsQuality,
    fcsFramerateDenominator,
    fcsMaxCaptures,
    fcsFramerateNumerator,

    -- ** H264QvbrSettings
    H264QvbrSettings (..),
    mkH264QvbrSettings,
    hQvbrQualityLevelFineTune,
    hMaxAverageBitrate,
    hQvbrQualityLevel,

    -- ** H264Settings
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

    -- ** H265QvbrSettings
    H265QvbrSettings (..),
    mkH265QvbrSettings,
    hqsQvbrQualityLevelFineTune,
    hqsMaxAverageBitrate,
    hqsQvbrQualityLevel,

    -- ** H265Settings
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

    -- ** Hdr10Metadata
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

    -- ** HlsAdditionalManifest
    HlsAdditionalManifest (..),
    mkHlsAdditionalManifest,
    hamManifestNameModifier,
    hamSelectedOutputs,

    -- ** HlsCaptionLanguageMapping
    HlsCaptionLanguageMapping (..),
    mkHlsCaptionLanguageMapping,
    hclmCustomLanguageCode,
    hclmLanguageCode,
    hclmLanguageDescription,
    hclmCaptionChannel,

    -- ** HlsEncryptionSettings
    HlsEncryptionSettings (..),
    mkHlsEncryptionSettings,
    hesOfflineEncrypted,
    hesEncryptionMethod,
    hesConstantInitializationVector,
    hesType,
    hesStaticKeyProvider,
    hesSpekeKeyProvider,
    hesInitializationVectorInManifest,

    -- ** HlsGroupSettings
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

    -- ** HlsSettings
    HlsSettings (..),
    mkHlsSettings,
    hsAudioRenditionSets,
    hsIFrameOnlyManifest,
    hsAudioGroupId,
    hsSegmentModifier,
    hsAudioOnlyContainer,
    hsAudioTrackType,

    -- ** HopDestination
    HopDestination (..),
    mkHopDestination,
    hdPriority,
    hdQueue,
    hdWaitMinutes,

    -- ** Id3Insertion
    Id3Insertion (..),
    mkId3Insertion,
    iiId3,
    iiTimecode,

    -- ** ImageInserter
    ImageInserter (..),
    mkImageInserter,
    iiInsertableImages,

    -- ** ImscDestinationSettings
    ImscDestinationSettings (..),
    mkImscDestinationSettings,
    idsStylePassthrough,

    -- ** Input
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

    -- ** InputClipping
    InputClipping (..),
    mkInputClipping,
    icEndTimecode,
    icStartTimecode,

    -- ** InputDecryptionSettings
    InputDecryptionSettings (..),
    mkInputDecryptionSettings,
    idsEncryptedDecryptionKey,
    idsKMSKeyRegion,
    idsDecryptionMode,
    idsInitializationVector,

    -- ** InputTemplate
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

    -- ** InsertableImage
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

    -- ** Job
    Job (..),
    mkJob,
    jStatus,
    jJobTemplate,
    jAccelerationSettings,
    jPriority,
    jStatusUpdateInterval,
    jSettings,
    jARN,
    jCreatedAt,
    jHopDestinations,
    jRetryCount,
    jSimulateReservedQueue,
    jCurrentPhase,
    jQueue,
    jUserMetadata,
    jRole,
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

    -- ** JobMessages
    JobMessages (..),
    mkJobMessages,
    jmWarning,
    jmInfo,

    -- ** JobSettings
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

    -- ** JobTemplate
    JobTemplate (..),
    mkJobTemplate,
    jtAccelerationSettings,
    jtLastUpdated,
    jtPriority,
    jtStatusUpdateInterval,
    jtSettings,
    jtARN,
    jtCreatedAt,
    jtCategory,
    jtHopDestinations,
    jtQueue,
    jtName,
    jtType,
    jtDescription,

    -- ** JobTemplateSettings
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

    -- ** M2tsScte35Esam
    M2tsScte35Esam (..),
    mkM2tsScte35Esam,
    mseScte35EsamPid,

    -- ** M2tsSettings
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

    -- ** M3u8Settings
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

    -- ** MotionImageInserter
    MotionImageInserter (..),
    mkMotionImageInserter,
    miiFramerate,
    miiStartTime,
    miiOffset,
    miiInput,
    miiInsertionMode,
    miiPlayback,

    -- ** MotionImageInsertionFramerate
    MotionImageInsertionFramerate (..),
    mkMotionImageInsertionFramerate,
    miifFramerateDenominator,
    miifFramerateNumerator,

    -- ** MotionImageInsertionOffset
    MotionImageInsertionOffset (..),
    mkMotionImageInsertionOffset,
    miioImageX,
    miioImageY,

    -- ** MovSettings
    MovSettings (..),
    mkMovSettings,
    msReference,
    msCslgAtom,
    msMpeg2FourCCControl,
    msPaddingControl,
    msClapAtom,

    -- ** Mp2Settings
    Mp2Settings (..),
    mkMp2Settings,
    mssChannels,
    mssSampleRate,
    mssBitrate,

    -- ** Mp3Settings
    Mp3Settings (..),
    mkMp3Settings,
    msfChannels,
    msfRateControlMode,
    msfSampleRate,
    msfBitrate,
    msfVbrQuality,

    -- ** Mp4Settings
    Mp4Settings (..),
    mkMp4Settings,
    mssMoovPlacement,
    mssCttsVersion,
    mssFreeSpaceBox,
    mssAudioDuration,
    mssMp4MajorBrand,
    mssCslgAtom,

    -- ** MpdSettings
    MpdSettings (..),
    mkMpdSettings,
    msfScte35Esam,
    msfAudioDuration,
    msfScte35Source,
    msfAccessibilityCaptionHints,
    msfCaptionContainerType,

    -- ** Mpeg2Settings
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

    -- ** MsSmoothAdditionalManifest
    MsSmoothAdditionalManifest (..),
    mkMsSmoothAdditionalManifest,
    msamManifestNameModifier,
    msamSelectedOutputs,

    -- ** MsSmoothEncryptionSettings
    MsSmoothEncryptionSettings (..),
    mkMsSmoothEncryptionSettings,
    msesSpekeKeyProvider,

    -- ** MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    mkMsSmoothGroupSettings,
    msgsFragmentLength,
    msgsManifestEncoding,
    msgsDestination,
    msgsAudioDeduplication,
    msgsAdditionalManifests,
    msgsDestinationSettings,
    msgsEncryption,

    -- ** MxfSettings
    MxfSettings (..),
    mkMxfSettings,
    msAfdSignaling,
    msProfile,

    -- ** NexGuardFileMarkerSettings
    NexGuardFileMarkerSettings (..),
    mkNexGuardFileMarkerSettings,
    ngfmsStrength,
    ngfmsPayload,
    ngfmsPreset,
    ngfmsLicense,

    -- ** NielsenConfiguration
    NielsenConfiguration (..),
    mkNielsenConfiguration,
    ncBreakoutCode,
    ncDistributorId,

    -- ** NielsenNonLinearWatermarkSettings
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

    -- ** NoiseReducer
    NoiseReducer (..),
    mkNoiseReducer,
    nrTemporalFilterSettings,
    nrSpatialFilterSettings,
    nrFilterSettings,
    nrFilter,

    -- ** NoiseReducerFilterSettings
    NoiseReducerFilterSettings (..),
    mkNoiseReducerFilterSettings,
    nrfsStrength,

    -- ** NoiseReducerSpatialFilterSettings
    NoiseReducerSpatialFilterSettings (..),
    mkNoiseReducerSpatialFilterSettings,
    nrsfsStrength,
    nrsfsPostFilterSharpenStrength,
    nrsfsSpeed,

    -- ** NoiseReducerTemporalFilterSettings
    NoiseReducerTemporalFilterSettings (..),
    mkNoiseReducerTemporalFilterSettings,
    nrtfsPostTemporalSharpening,
    nrtfsAggressiveMode,
    nrtfsStrength,
    nrtfsSpeed,

    -- ** OpusSettings
    OpusSettings (..),
    mkOpusSettings,
    osChannels,
    osSampleRate,
    osBitrate,

    -- ** Output
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

    -- ** OutputChannelMapping
    OutputChannelMapping (..),
    mkOutputChannelMapping,
    ocmInputChannels,

    -- ** OutputDetail
    OutputDetail (..),
    mkOutputDetail,
    odVideoDetails,
    odDurationInMs,

    -- ** OutputGroup
    OutputGroup (..),
    mkOutputGroup,
    ogOutputGroupSettings,
    ogOutputs,
    ogCustomName,
    ogName,
    ogAutomatedEncodingSettings,

    -- ** OutputGroupDetail
    OutputGroupDetail (..),
    mkOutputGroupDetail,
    ogdOutputDetails,

    -- ** OutputGroupSettings
    OutputGroupSettings (..),
    mkOutputGroupSettings,
    ogsFileGroupSettings,
    ogsCmafGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsHlsGroupSettings,
    ogsType,
    ogsDashIsoGroupSettings,

    -- ** OutputSettings
    OutputSettings (..),
    mkOutputSettings,
    osHlsSettings,

    -- ** PartnerWatermarking
    PartnerWatermarking (..),
    mkPartnerWatermarking,
    pwNexguardFileMarkerSettings,

    -- ** Preset
    Preset (..),
    mkPreset,
    pLastUpdated,
    pSettings,
    pARN,
    pCreatedAt,
    pCategory,
    pName,
    pType,
    pDescription,

    -- ** PresetSettings
    PresetSettings (..),
    mkPresetSettings,
    psCaptionDescriptions,
    psVideoDescription,
    psContainerSettings,
    psAudioDescriptions,

    -- ** ProresSettings
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

    -- ** Queue
    Queue (..),
    mkQueue,
    qStatus,
    qLastUpdated,
    qARN,
    qCreatedAt,
    qReservationPlan,
    qPricingPlan,
    qSubmittedJobsCount,
    qName,
    qProgressingJobsCount,
    qType,
    qDescription,

    -- ** QueueTransition
    QueueTransition (..),
    mkQueueTransition,
    qtSourceQueue,
    qtDestinationQueue,
    qtTimestamp,

    -- ** Rectangle
    Rectangle (..),
    mkRectangle,
    rHeight,
    rWidth,
    rX,
    rY,

    -- ** RemixSettings
    RemixSettings (..),
    mkRemixSettings,
    rsChannelMapping,
    rsChannelsIn,
    rsChannelsOut,

    -- ** ReservationPlan
    ReservationPlan (..),
    mkReservationPlan,
    rpStatus,
    rpExpiresAt,
    rpPurchasedAt,
    rpCommitment,
    rpReservedSlots,
    rpRenewalType,

    -- ** ReservationPlanSettings
    ReservationPlanSettings (..),
    mkReservationPlanSettings,
    rpsCommitment,
    rpsReservedSlots,
    rpsRenewalType,

    -- ** ResourceTags
    ResourceTags (..),
    mkResourceTags,
    rtARN,
    rtTags,

    -- ** S3DestinationAccessControl
    S3DestinationAccessControl (..),
    mkS3DestinationAccessControl,
    sdacCannedACL,

    -- ** S3DestinationSettings
    S3DestinationSettings (..),
    mkS3DestinationSettings,
    sdsAccessControl,
    sdsEncryption,

    -- ** S3EncryptionSettings
    S3EncryptionSettings (..),
    mkS3EncryptionSettings,
    sesEncryptionType,
    sesKMSKeyARN,

    -- ** SccDestinationSettings
    SccDestinationSettings (..),
    mkSccDestinationSettings,
    sdsFramerate,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (..),
    mkSpekeKeyProvider,
    sResourceId,
    sCertificateARN,
    sURL,
    sSystemIds,

    -- ** SpekeKeyProviderCmaf
    SpekeKeyProviderCmaf (..),
    mkSpekeKeyProviderCmaf,
    skpcResourceId,
    skpcDashSignaledSystemIds,
    skpcCertificateARN,
    skpcURL,
    skpcHlsSignaledSystemIds,

    -- ** StaticKeyProvider
    StaticKeyProvider (..),
    mkStaticKeyProvider,
    skpStaticKeyValue,
    skpURL,
    skpKeyFormat,
    skpKeyFormatVersions,

    -- ** TeletextDestinationSettings
    TeletextDestinationSettings (..),
    mkTeletextDestinationSettings,
    tdsPageTypes,
    tdsPageNumber,

    -- ** TeletextSourceSettings
    TeletextSourceSettings (..),
    mkTeletextSourceSettings,
    tssPageNumber,

    -- ** TimecodeBurnin
    TimecodeBurnin (..),
    mkTimecodeBurnin,
    tbPrefix,
    tbFontSize,
    tbPosition,

    -- ** TimecodeConfig
    TimecodeConfig (..),
    mkTimecodeConfig,
    tcStart,
    tcTimestampOffset,
    tcAnchor,
    tcSource,

    -- ** TimedMetadataInsertion
    TimedMetadataInsertion (..),
    mkTimedMetadataInsertion,
    tmiId3Insertions,

    -- ** Timing
    Timing (..),
    mkTiming,
    tStartTime,
    tFinishTime,
    tSubmitTime,

    -- ** TrackSourceSettings
    TrackSourceSettings (..),
    mkTrackSourceSettings,
    tssTrackNumber,

    -- ** TtmlDestinationSettings
    TtmlDestinationSettings (..),
    mkTtmlDestinationSettings,
    tdsStylePassthrough,

    -- ** Vc3Settings
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

    -- ** VideoCodecSettings
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

    -- ** VideoDescription
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

    -- ** VideoDetail
    VideoDetail (..),
    mkVideoDetail,
    vdHeightInPx,
    vdWidthInPx,

    -- ** VideoPreprocessor
    VideoPreprocessor (..),
    mkVideoPreprocessor,
    vpTimecodeBurnin,
    vpDolbyVision,
    vpColorCorrector,
    vpDeinterlacer,
    vpNoiseReducer,
    vpImageInserter,
    vpPartnerWatermarking,

    -- ** VideoSelector
    VideoSelector (..),
    mkVideoSelector,
    vsProgramNumber,
    vsAlphaBehavior,
    vsColorSpaceUsage,
    vsHdr10Metadata,
    vsPid,
    vsRotate,
    vsColorSpace,

    -- ** VorbisSettings
    VorbisSettings (..),
    mkVorbisSettings,
    vsChannels,
    vsSampleRate,
    vsVbrQuality,

    -- ** Vp8Settings
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

    -- ** Vp9Settings
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

    -- ** WavSettings
    WavSettings (..),
    mkWavSettings,
    wsBitDepth,
    wsChannels,
    wsFormat,
    wsSampleRate,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.UntagResource
import Network.AWS.MediaConvert.UpdateJobTemplate
import Network.AWS.MediaConvert.UpdatePreset
import Network.AWS.MediaConvert.UpdateQueue
import Network.AWS.MediaConvert.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaConvert'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
