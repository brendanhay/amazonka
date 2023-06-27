{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _InternalServerErrorException,
    _NotFoundException,
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

    -- * Ac3DynamicRangeCompressionLine
    Ac3DynamicRangeCompressionLine (..),

    -- * Ac3DynamicRangeCompressionProfile
    Ac3DynamicRangeCompressionProfile (..),

    -- * Ac3DynamicRangeCompressionRf
    Ac3DynamicRangeCompressionRf (..),

    -- * Ac3LfeFilter
    Ac3LfeFilter (..),

    -- * Ac3MetadataControl
    Ac3MetadataControl (..),

    -- * AccelerationMode
    AccelerationMode (..),

    -- * AccelerationStatus
    AccelerationStatus (..),

    -- * AdvancedInputFilter
    AdvancedInputFilter (..),

    -- * AdvancedInputFilterAddTexture
    AdvancedInputFilterAddTexture (..),

    -- * AdvancedInputFilterSharpen
    AdvancedInputFilterSharpen (..),

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

    -- * AudioDurationCorrection
    AudioDurationCorrection (..),

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

    -- * Av1BitDepth
    Av1BitDepth (..),

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

    -- * BandwidthReductionFilterSharpening
    BandwidthReductionFilterSharpening (..),

    -- * BandwidthReductionFilterStrength
    BandwidthReductionFilterStrength (..),

    -- * BillingTagsSource
    BillingTagsSource (..),

    -- * BurnInSubtitleStylePassthrough
    BurnInSubtitleStylePassthrough (..),

    -- * BurninSubtitleAlignment
    BurninSubtitleAlignment (..),

    -- * BurninSubtitleApplyFontColor
    BurninSubtitleApplyFontColor (..),

    -- * BurninSubtitleBackgroundColor
    BurninSubtitleBackgroundColor (..),

    -- * BurninSubtitleFallbackFont
    BurninSubtitleFallbackFont (..),

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

    -- * CaptionSourceConvertPaintOnToPopOn
    CaptionSourceConvertPaintOnToPopOn (..),

    -- * CaptionSourceType
    CaptionSourceType (..),

    -- * CmafClientCache
    CmafClientCache (..),

    -- * CmafCodecSpecification
    CmafCodecSpecification (..),

    -- * CmafEncryptionType
    CmafEncryptionType (..),

    -- * CmafImageBasedTrickPlay
    CmafImageBasedTrickPlay (..),

    -- * CmafInitializationVectorInManifest
    CmafInitializationVectorInManifest (..),

    -- * CmafIntervalCadence
    CmafIntervalCadence (..),

    -- * CmafKeyProviderType
    CmafKeyProviderType (..),

    -- * CmafManifestCompression
    CmafManifestCompression (..),

    -- * CmafManifestDurationFormat
    CmafManifestDurationFormat (..),

    -- * CmafMpdManifestBandwidthType
    CmafMpdManifestBandwidthType (..),

    -- * CmafMpdProfile
    CmafMpdProfile (..),

    -- * CmafPtsOffsetHandlingForBFrames
    CmafPtsOffsetHandlingForBFrames (..),

    -- * CmafSegmentControl
    CmafSegmentControl (..),

    -- * CmafSegmentLengthControl
    CmafSegmentLengthControl (..),

    -- * CmafStreamInfResolution
    CmafStreamInfResolution (..),

    -- * CmafTargetDurationCompatibilityMode
    CmafTargetDurationCompatibilityMode (..),

    -- * CmafVideoCompositionOffsets
    CmafVideoCompositionOffsets (..),

    -- * CmafWriteDASHManifest
    CmafWriteDASHManifest (..),

    -- * CmafWriteHLSManifest
    CmafWriteHLSManifest (..),

    -- * CmafWriteSegmentTimelineInRepresentation
    CmafWriteSegmentTimelineInRepresentation (..),

    -- * CmfcAudioDuration
    CmfcAudioDuration (..),

    -- * CmfcAudioTrackType
    CmfcAudioTrackType (..),

    -- * CmfcDescriptiveVideoServiceFlag
    CmfcDescriptiveVideoServiceFlag (..),

    -- * CmfcIFrameOnlyManifest
    CmfcIFrameOnlyManifest (..),

    -- * CmfcKlvMetadata
    CmfcKlvMetadata (..),

    -- * CmfcManifestMetadataSignaling
    CmfcManifestMetadataSignaling (..),

    -- * CmfcScte35Esam
    CmfcScte35Esam (..),

    -- * CmfcScte35Source
    CmfcScte35Source (..),

    -- * CmfcTimedMetadata
    CmfcTimedMetadata (..),

    -- * CmfcTimedMetadataBoxVersion
    CmfcTimedMetadataBoxVersion (..),

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

    -- * CopyProtectionAction
    CopyProtectionAction (..),

    -- * DashIsoGroupAudioChannelConfigSchemeIdUri
    DashIsoGroupAudioChannelConfigSchemeIdUri (..),

    -- * DashIsoHbbtvCompliance
    DashIsoHbbtvCompliance (..),

    -- * DashIsoImageBasedTrickPlay
    DashIsoImageBasedTrickPlay (..),

    -- * DashIsoIntervalCadence
    DashIsoIntervalCadence (..),

    -- * DashIsoMpdManifestBandwidthType
    DashIsoMpdManifestBandwidthType (..),

    -- * DashIsoMpdProfile
    DashIsoMpdProfile (..),

    -- * DashIsoPlaybackDeviceCompatibility
    DashIsoPlaybackDeviceCompatibility (..),

    -- * DashIsoPtsOffsetHandlingForBFrames
    DashIsoPtsOffsetHandlingForBFrames (..),

    -- * DashIsoSegmentControl
    DashIsoSegmentControl (..),

    -- * DashIsoSegmentLengthControl
    DashIsoSegmentLengthControl (..),

    -- * DashIsoVideoCompositionOffsets
    DashIsoVideoCompositionOffsets (..),

    -- * DashIsoWriteSegmentTimelineInRepresentation
    DashIsoWriteSegmentTimelineInRepresentation (..),

    -- * DashManifestStyle
    DashManifestStyle (..),

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

    -- * DolbyVisionMapping
    DolbyVisionMapping (..),

    -- * DolbyVisionProfile
    DolbyVisionProfile (..),

    -- * DropFrameTimecode
    DropFrameTimecode (..),

    -- * DvbSubSubtitleFallbackFont
    DvbSubSubtitleFallbackFont (..),

    -- * DvbSubtitleAlignment
    DvbSubtitleAlignment (..),

    -- * DvbSubtitleApplyFontColor
    DvbSubtitleApplyFontColor (..),

    -- * DvbSubtitleBackgroundColor
    DvbSubtitleBackgroundColor (..),

    -- * DvbSubtitleFontColor
    DvbSubtitleFontColor (..),

    -- * DvbSubtitleOutlineColor
    DvbSubtitleOutlineColor (..),

    -- * DvbSubtitleShadowColor
    DvbSubtitleShadowColor (..),

    -- * DvbSubtitleStylePassthrough
    DvbSubtitleStylePassthrough (..),

    -- * DvbSubtitleTeletextSpacing
    DvbSubtitleTeletextSpacing (..),

    -- * DvbSubtitlingType
    DvbSubtitlingType (..),

    -- * DvbddsHandling
    DvbddsHandling (..),

    -- * Eac3AtmosBitstreamMode
    Eac3AtmosBitstreamMode (..),

    -- * Eac3AtmosCodingMode
    Eac3AtmosCodingMode (..),

    -- * Eac3AtmosDialogueIntelligence
    Eac3AtmosDialogueIntelligence (..),

    -- * Eac3AtmosDownmixControl
    Eac3AtmosDownmixControl (..),

    -- * Eac3AtmosDynamicRangeCompressionLine
    Eac3AtmosDynamicRangeCompressionLine (..),

    -- * Eac3AtmosDynamicRangeCompressionRf
    Eac3AtmosDynamicRangeCompressionRf (..),

    -- * Eac3AtmosDynamicRangeControl
    Eac3AtmosDynamicRangeControl (..),

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

    -- * EmbeddedTimecodeOverride
    EmbeddedTimecodeOverride (..),

    -- * F4vMoovPlacement
    F4vMoovPlacement (..),

    -- * FileSourceConvert608To708
    FileSourceConvert608To708 (..),

    -- * FileSourceTimeDeltaUnits
    FileSourceTimeDeltaUnits (..),

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

    -- * HDRToSDRToneMapper
    HDRToSDRToneMapper (..),

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

    -- * HlsCaptionSegmentLengthControl
    HlsCaptionSegmentLengthControl (..),

    -- * HlsClientCache
    HlsClientCache (..),

    -- * HlsCodecSpecification
    HlsCodecSpecification (..),

    -- * HlsDescriptiveVideoServiceFlag
    HlsDescriptiveVideoServiceFlag (..),

    -- * HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- * HlsEncryptionType
    HlsEncryptionType (..),

    -- * HlsIFrameOnlyManifest
    HlsIFrameOnlyManifest (..),

    -- * HlsImageBasedTrickPlay
    HlsImageBasedTrickPlay (..),

    -- * HlsInitializationVectorInManifest
    HlsInitializationVectorInManifest (..),

    -- * HlsIntervalCadence
    HlsIntervalCadence (..),

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

    -- * HlsProgressiveWriteHlsManifest
    HlsProgressiveWriteHlsManifest (..),

    -- * HlsSegmentControl
    HlsSegmentControl (..),

    -- * HlsSegmentLengthControl
    HlsSegmentLengthControl (..),

    -- * HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- * HlsTargetDurationCompatibilityMode
    HlsTargetDurationCompatibilityMode (..),

    -- * HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- * ImscAccessibilitySubs
    ImscAccessibilitySubs (..),

    -- * ImscStylePassthrough
    ImscStylePassthrough (..),

    -- * InputDeblockFilter
    InputDeblockFilter (..),

    -- * InputDenoiseFilter
    InputDenoiseFilter (..),

    -- * InputFilterEnable
    InputFilterEnable (..),

    -- * InputPolicy
    InputPolicy (..),

    -- * InputPsiControl
    InputPsiControl (..),

    -- * InputRotate
    InputRotate (..),

    -- * InputSampleRange
    InputSampleRange (..),

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

    -- * M2tsDataPtsControl
    M2tsDataPtsControl (..),

    -- * M2tsEbpAudioInterval
    M2tsEbpAudioInterval (..),

    -- * M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- * M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- * M2tsForceTsVideoEbpOrder
    M2tsForceTsVideoEbpOrder (..),

    -- * M2tsKlvMetadata
    M2tsKlvMetadata (..),

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

    -- * M3u8DataPtsControl
    M3u8DataPtsControl (..),

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

    -- * MpdKlvMetadata
    MpdKlvMetadata (..),

    -- * MpdManifestMetadataSignaling
    MpdManifestMetadataSignaling (..),

    -- * MpdScte35Esam
    MpdScte35Esam (..),

    -- * MpdScte35Source
    MpdScte35Source (..),

    -- * MpdTimedMetadata
    MpdTimedMetadata (..),

    -- * MpdTimedMetadataBoxVersion
    MpdTimedMetadataBoxVersion (..),

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

    -- * MsSmoothFragmentLengthControl
    MsSmoothFragmentLengthControl (..),

    -- * MsSmoothManifestEncoding
    MsSmoothManifestEncoding (..),

    -- * MxfAfdSignaling
    MxfAfdSignaling (..),

    -- * MxfProfile
    MxfProfile (..),

    -- * MxfXavcDurationMode
    MxfXavcDurationMode (..),

    -- * NielsenActiveWatermarkProcessType
    NielsenActiveWatermarkProcessType (..),

    -- * NielsenSourceWatermarkStatusType
    NielsenSourceWatermarkStatusType (..),

    -- * NielsenUniqueTicPerAudioTrackType
    NielsenUniqueTicPerAudioTrackType (..),

    -- * NoiseFilterPostTemporalSharpening
    NoiseFilterPostTemporalSharpening (..),

    -- * NoiseFilterPostTemporalSharpeningStrength
    NoiseFilterPostTemporalSharpeningStrength (..),

    -- * NoiseReducerFilter
    NoiseReducerFilter (..),

    -- * Order
    Order (..),

    -- * OutputGroupType
    OutputGroupType (..),

    -- * OutputSdt
    OutputSdt (..),

    -- * PadVideo
    PadVideo (..),

    -- * PresetListBy
    PresetListBy (..),

    -- * PricingPlan
    PricingPlan (..),

    -- * ProresChromaSampling
    ProresChromaSampling (..),

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

    -- * RequiredFlag
    RequiredFlag (..),

    -- * ReservationPlanStatus
    ReservationPlanStatus (..),

    -- * RespondToAfd
    RespondToAfd (..),

    -- * RuleType
    RuleType (..),

    -- * S3ObjectCannedAcl
    S3ObjectCannedAcl (..),

    -- * S3ServerSideEncryptionType
    S3ServerSideEncryptionType (..),

    -- * SampleRangeConversion
    SampleRangeConversion (..),

    -- * ScalingBehavior
    ScalingBehavior (..),

    -- * SccDestinationFramerate
    SccDestinationFramerate (..),

    -- * SimulateReservedQueue
    SimulateReservedQueue (..),

    -- * SrtStylePassthrough
    SrtStylePassthrough (..),

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

    -- * VchipAction
    VchipAction (..),

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

    -- * WebvttAccessibilitySubs
    WebvttAccessibilitySubs (..),

    -- * WebvttStylePassthrough
    WebvttStylePassthrough (..),

    -- * Xavc4kIntraCbgProfileClass
    Xavc4kIntraCbgProfileClass (..),

    -- * Xavc4kIntraVbrProfileClass
    Xavc4kIntraVbrProfileClass (..),

    -- * Xavc4kProfileBitrateClass
    Xavc4kProfileBitrateClass (..),

    -- * Xavc4kProfileCodecProfile
    Xavc4kProfileCodecProfile (..),

    -- * Xavc4kProfileQualityTuningLevel
    Xavc4kProfileQualityTuningLevel (..),

    -- * XavcAdaptiveQuantization
    XavcAdaptiveQuantization (..),

    -- * XavcEntropyEncoding
    XavcEntropyEncoding (..),

    -- * XavcFlickerAdaptiveQuantization
    XavcFlickerAdaptiveQuantization (..),

    -- * XavcFramerateControl
    XavcFramerateControl (..),

    -- * XavcFramerateConversionAlgorithm
    XavcFramerateConversionAlgorithm (..),

    -- * XavcGopBReference
    XavcGopBReference (..),

    -- * XavcHdIntraCbgProfileClass
    XavcHdIntraCbgProfileClass (..),

    -- * XavcHdProfileBitrateClass
    XavcHdProfileBitrateClass (..),

    -- * XavcHdProfileQualityTuningLevel
    XavcHdProfileQualityTuningLevel (..),

    -- * XavcHdProfileTelecine
    XavcHdProfileTelecine (..),

    -- * XavcInterlaceMode
    XavcInterlaceMode (..),

    -- * XavcProfile
    XavcProfile (..),

    -- * XavcSlowPal
    XavcSlowPal (..),

    -- * XavcSpatialAdaptiveQuantization
    XavcSpatialAdaptiveQuantization (..),

    -- * XavcTemporalAdaptiveQuantization
    XavcTemporalAdaptiveQuantization (..),

    -- * AacSettings
    AacSettings (..),
    newAacSettings,
    aacSettings_audioDescriptionBroadcasterMix,
    aacSettings_bitrate,
    aacSettings_codecProfile,
    aacSettings_codingMode,
    aacSettings_rateControlMode,
    aacSettings_rawFormat,
    aacSettings_sampleRate,
    aacSettings_specification,
    aacSettings_vbrQuality,

    -- * Ac3Settings
    Ac3Settings (..),
    newAc3Settings,
    ac3Settings_bitrate,
    ac3Settings_bitstreamMode,
    ac3Settings_codingMode,
    ac3Settings_dialnorm,
    ac3Settings_dynamicRangeCompressionLine,
    ac3Settings_dynamicRangeCompressionProfile,
    ac3Settings_dynamicRangeCompressionRf,
    ac3Settings_lfeFilter,
    ac3Settings_metadataControl,
    ac3Settings_sampleRate,

    -- * AccelerationSettings
    AccelerationSettings (..),
    newAccelerationSettings,
    accelerationSettings_mode,

    -- * AdvancedInputFilterSettings
    AdvancedInputFilterSettings (..),
    newAdvancedInputFilterSettings,
    advancedInputFilterSettings_addTexture,
    advancedInputFilterSettings_sharpening,

    -- * AiffSettings
    AiffSettings (..),
    newAiffSettings,
    aiffSettings_bitDepth,
    aiffSettings_channels,
    aiffSettings_sampleRate,

    -- * AllowedRenditionSize
    AllowedRenditionSize (..),
    newAllowedRenditionSize,
    allowedRenditionSize_height,
    allowedRenditionSize_required,
    allowedRenditionSize_width,

    -- * AncillarySourceSettings
    AncillarySourceSettings (..),
    newAncillarySourceSettings,
    ancillarySourceSettings_convert608To708,
    ancillarySourceSettings_sourceAncillaryChannelNumber,
    ancillarySourceSettings_terminateCaptions,

    -- * AudioChannelTaggingSettings
    AudioChannelTaggingSettings (..),
    newAudioChannelTaggingSettings,
    audioChannelTaggingSettings_channelTag,

    -- * AudioCodecSettings
    AudioCodecSettings (..),
    newAudioCodecSettings,
    audioCodecSettings_aacSettings,
    audioCodecSettings_ac3Settings,
    audioCodecSettings_aiffSettings,
    audioCodecSettings_codec,
    audioCodecSettings_eac3AtmosSettings,
    audioCodecSettings_eac3Settings,
    audioCodecSettings_mp2Settings,
    audioCodecSettings_mp3Settings,
    audioCodecSettings_opusSettings,
    audioCodecSettings_vorbisSettings,
    audioCodecSettings_wavSettings,

    -- * AudioDescription
    AudioDescription (..),
    newAudioDescription,
    audioDescription_audioChannelTaggingSettings,
    audioDescription_audioNormalizationSettings,
    audioDescription_audioSourceName,
    audioDescription_audioType,
    audioDescription_audioTypeControl,
    audioDescription_codecSettings,
    audioDescription_customLanguageCode,
    audioDescription_languageCode,
    audioDescription_languageCodeControl,
    audioDescription_remixSettings,
    audioDescription_streamName,

    -- * AudioNormalizationSettings
    AudioNormalizationSettings (..),
    newAudioNormalizationSettings,
    audioNormalizationSettings_algorithm,
    audioNormalizationSettings_algorithmControl,
    audioNormalizationSettings_correctionGateLevel,
    audioNormalizationSettings_loudnessLogging,
    audioNormalizationSettings_peakCalculation,
    audioNormalizationSettings_targetLkfs,
    audioNormalizationSettings_truePeakLimiterThreshold,

    -- * AudioSelector
    AudioSelector (..),
    newAudioSelector,
    audioSelector_audioDurationCorrection,
    audioSelector_customLanguageCode,
    audioSelector_defaultSelection,
    audioSelector_externalAudioFileInput,
    audioSelector_hlsRenditionGroupSettings,
    audioSelector_languageCode,
    audioSelector_offset,
    audioSelector_pids,
    audioSelector_programSelection,
    audioSelector_remixSettings,
    audioSelector_selectorType,
    audioSelector_tracks,

    -- * AudioSelectorGroup
    AudioSelectorGroup (..),
    newAudioSelectorGroup,
    audioSelectorGroup_audioSelectorNames,

    -- * AutomatedAbrRule
    AutomatedAbrRule (..),
    newAutomatedAbrRule,
    automatedAbrRule_allowedRenditions,
    automatedAbrRule_forceIncludeRenditions,
    automatedAbrRule_minBottomRenditionSize,
    automatedAbrRule_minTopRenditionSize,
    automatedAbrRule_type,

    -- * AutomatedAbrSettings
    AutomatedAbrSettings (..),
    newAutomatedAbrSettings,
    automatedAbrSettings_maxAbrBitrate,
    automatedAbrSettings_maxRenditions,
    automatedAbrSettings_minAbrBitrate,
    automatedAbrSettings_rules,

    -- * AutomatedEncodingSettings
    AutomatedEncodingSettings (..),
    newAutomatedEncodingSettings,
    automatedEncodingSettings_abrSettings,

    -- * Av1QvbrSettings
    Av1QvbrSettings (..),
    newAv1QvbrSettings,
    av1QvbrSettings_qvbrQualityLevel,
    av1QvbrSettings_qvbrQualityLevelFineTune,

    -- * Av1Settings
    Av1Settings (..),
    newAv1Settings,
    av1Settings_adaptiveQuantization,
    av1Settings_bitDepth,
    av1Settings_framerateControl,
    av1Settings_framerateConversionAlgorithm,
    av1Settings_framerateDenominator,
    av1Settings_framerateNumerator,
    av1Settings_gopSize,
    av1Settings_maxBitrate,
    av1Settings_numberBFramesBetweenReferenceFrames,
    av1Settings_qvbrSettings,
    av1Settings_rateControlMode,
    av1Settings_slices,
    av1Settings_spatialAdaptiveQuantization,

    -- * AvailBlanking
    AvailBlanking (..),
    newAvailBlanking,
    availBlanking_availBlankingImage,

    -- * AvcIntraSettings
    AvcIntraSettings (..),
    newAvcIntraSettings,
    avcIntraSettings_avcIntraClass,
    avcIntraSettings_avcIntraUhdSettings,
    avcIntraSettings_framerateControl,
    avcIntraSettings_framerateConversionAlgorithm,
    avcIntraSettings_framerateDenominator,
    avcIntraSettings_framerateNumerator,
    avcIntraSettings_interlaceMode,
    avcIntraSettings_scanTypeConversionMode,
    avcIntraSettings_slowPal,
    avcIntraSettings_telecine,

    -- * AvcIntraUhdSettings
    AvcIntraUhdSettings (..),
    newAvcIntraUhdSettings,
    avcIntraUhdSettings_qualityTuningLevel,

    -- * BandwidthReductionFilter
    BandwidthReductionFilter (..),
    newBandwidthReductionFilter,
    bandwidthReductionFilter_sharpening,
    bandwidthReductionFilter_strength,

    -- * BurninDestinationSettings
    BurninDestinationSettings (..),
    newBurninDestinationSettings,
    burninDestinationSettings_alignment,
    burninDestinationSettings_applyFontColor,
    burninDestinationSettings_backgroundColor,
    burninDestinationSettings_backgroundOpacity,
    burninDestinationSettings_fallbackFont,
    burninDestinationSettings_fontColor,
    burninDestinationSettings_fontOpacity,
    burninDestinationSettings_fontResolution,
    burninDestinationSettings_fontScript,
    burninDestinationSettings_fontSize,
    burninDestinationSettings_hexFontColor,
    burninDestinationSettings_outlineColor,
    burninDestinationSettings_outlineSize,
    burninDestinationSettings_shadowColor,
    burninDestinationSettings_shadowOpacity,
    burninDestinationSettings_shadowXOffset,
    burninDestinationSettings_shadowYOffset,
    burninDestinationSettings_stylePassthrough,
    burninDestinationSettings_teletextSpacing,
    burninDestinationSettings_xPosition,
    burninDestinationSettings_yPosition,

    -- * CaptionDescription
    CaptionDescription (..),
    newCaptionDescription,
    captionDescription_captionSelectorName,
    captionDescription_customLanguageCode,
    captionDescription_destinationSettings,
    captionDescription_languageCode,
    captionDescription_languageDescription,

    -- * CaptionDescriptionPreset
    CaptionDescriptionPreset (..),
    newCaptionDescriptionPreset,
    captionDescriptionPreset_customLanguageCode,
    captionDescriptionPreset_destinationSettings,
    captionDescriptionPreset_languageCode,
    captionDescriptionPreset_languageDescription,

    -- * CaptionDestinationSettings
    CaptionDestinationSettings (..),
    newCaptionDestinationSettings,
    captionDestinationSettings_burninDestinationSettings,
    captionDestinationSettings_destinationType,
    captionDestinationSettings_dvbSubDestinationSettings,
    captionDestinationSettings_embeddedDestinationSettings,
    captionDestinationSettings_imscDestinationSettings,
    captionDestinationSettings_sccDestinationSettings,
    captionDestinationSettings_srtDestinationSettings,
    captionDestinationSettings_teletextDestinationSettings,
    captionDestinationSettings_ttmlDestinationSettings,
    captionDestinationSettings_webvttDestinationSettings,

    -- * CaptionSelector
    CaptionSelector (..),
    newCaptionSelector,
    captionSelector_customLanguageCode,
    captionSelector_languageCode,
    captionSelector_sourceSettings,

    -- * CaptionSourceFramerate
    CaptionSourceFramerate (..),
    newCaptionSourceFramerate,
    captionSourceFramerate_framerateDenominator,
    captionSourceFramerate_framerateNumerator,

    -- * CaptionSourceSettings
    CaptionSourceSettings (..),
    newCaptionSourceSettings,
    captionSourceSettings_ancillarySourceSettings,
    captionSourceSettings_dvbSubSourceSettings,
    captionSourceSettings_embeddedSourceSettings,
    captionSourceSettings_fileSourceSettings,
    captionSourceSettings_sourceType,
    captionSourceSettings_teletextSourceSettings,
    captionSourceSettings_trackSourceSettings,
    captionSourceSettings_webvttHlsSourceSettings,

    -- * ChannelMapping
    ChannelMapping (..),
    newChannelMapping,
    channelMapping_outputChannels,

    -- * ClipLimits
    ClipLimits (..),
    newClipLimits,
    clipLimits_maximumRGBTolerance,
    clipLimits_maximumYUV,
    clipLimits_minimumRGBTolerance,
    clipLimits_minimumYUV,

    -- * CmafAdditionalManifest
    CmafAdditionalManifest (..),
    newCmafAdditionalManifest,
    cmafAdditionalManifest_manifestNameModifier,
    cmafAdditionalManifest_selectedOutputs,

    -- * CmafEncryptionSettings
    CmafEncryptionSettings (..),
    newCmafEncryptionSettings,
    cmafEncryptionSettings_constantInitializationVector,
    cmafEncryptionSettings_encryptionMethod,
    cmafEncryptionSettings_initializationVectorInManifest,
    cmafEncryptionSettings_spekeKeyProvider,
    cmafEncryptionSettings_staticKeyProvider,
    cmafEncryptionSettings_type,

    -- * CmafGroupSettings
    CmafGroupSettings (..),
    newCmafGroupSettings,
    cmafGroupSettings_additionalManifests,
    cmafGroupSettings_baseUrl,
    cmafGroupSettings_clientCache,
    cmafGroupSettings_codecSpecification,
    cmafGroupSettings_dashManifestStyle,
    cmafGroupSettings_destination,
    cmafGroupSettings_destinationSettings,
    cmafGroupSettings_encryption,
    cmafGroupSettings_fragmentLength,
    cmafGroupSettings_imageBasedTrickPlay,
    cmafGroupSettings_imageBasedTrickPlaySettings,
    cmafGroupSettings_manifestCompression,
    cmafGroupSettings_manifestDurationFormat,
    cmafGroupSettings_minBufferTime,
    cmafGroupSettings_minFinalSegmentLength,
    cmafGroupSettings_mpdManifestBandwidthType,
    cmafGroupSettings_mpdProfile,
    cmafGroupSettings_ptsOffsetHandlingForBFrames,
    cmafGroupSettings_segmentControl,
    cmafGroupSettings_segmentLength,
    cmafGroupSettings_segmentLengthControl,
    cmafGroupSettings_streamInfResolution,
    cmafGroupSettings_targetDurationCompatibilityMode,
    cmafGroupSettings_videoCompositionOffsets,
    cmafGroupSettings_writeDashManifest,
    cmafGroupSettings_writeHlsManifest,
    cmafGroupSettings_writeSegmentTimelineInRepresentation,

    -- * CmafImageBasedTrickPlaySettings
    CmafImageBasedTrickPlaySettings (..),
    newCmafImageBasedTrickPlaySettings,
    cmafImageBasedTrickPlaySettings_intervalCadence,
    cmafImageBasedTrickPlaySettings_thumbnailHeight,
    cmafImageBasedTrickPlaySettings_thumbnailInterval,
    cmafImageBasedTrickPlaySettings_thumbnailWidth,
    cmafImageBasedTrickPlaySettings_tileHeight,
    cmafImageBasedTrickPlaySettings_tileWidth,

    -- * CmfcSettings
    CmfcSettings (..),
    newCmfcSettings,
    cmfcSettings_audioDuration,
    cmfcSettings_audioGroupId,
    cmfcSettings_audioRenditionSets,
    cmfcSettings_audioTrackType,
    cmfcSettings_descriptiveVideoServiceFlag,
    cmfcSettings_iFrameOnlyManifest,
    cmfcSettings_klvMetadata,
    cmfcSettings_manifestMetadataSignaling,
    cmfcSettings_scte35Esam,
    cmfcSettings_scte35Source,
    cmfcSettings_timedMetadata,
    cmfcSettings_timedMetadataBoxVersion,
    cmfcSettings_timedMetadataSchemeIdUri,
    cmfcSettings_timedMetadataValue,

    -- * ColorCorrector
    ColorCorrector (..),
    newColorCorrector,
    colorCorrector_brightness,
    colorCorrector_clipLimits,
    colorCorrector_colorSpaceConversion,
    colorCorrector_contrast,
    colorCorrector_hdr10Metadata,
    colorCorrector_hdrToSdrToneMapper,
    colorCorrector_hue,
    colorCorrector_sampleRangeConversion,
    colorCorrector_saturation,
    colorCorrector_sdrReferenceWhiteLevel,

    -- * ContainerSettings
    ContainerSettings (..),
    newContainerSettings,
    containerSettings_cmfcSettings,
    containerSettings_container,
    containerSettings_f4vSettings,
    containerSettings_m2tsSettings,
    containerSettings_m3u8Settings,
    containerSettings_movSettings,
    containerSettings_mp4Settings,
    containerSettings_mpdSettings,
    containerSettings_mxfSettings,

    -- * DashAdditionalManifest
    DashAdditionalManifest (..),
    newDashAdditionalManifest,
    dashAdditionalManifest_manifestNameModifier,
    dashAdditionalManifest_selectedOutputs,

    -- * DashIsoEncryptionSettings
    DashIsoEncryptionSettings (..),
    newDashIsoEncryptionSettings,
    dashIsoEncryptionSettings_playbackDeviceCompatibility,
    dashIsoEncryptionSettings_spekeKeyProvider,

    -- * DashIsoGroupSettings
    DashIsoGroupSettings (..),
    newDashIsoGroupSettings,
    dashIsoGroupSettings_additionalManifests,
    dashIsoGroupSettings_audioChannelConfigSchemeIdUri,
    dashIsoGroupSettings_baseUrl,
    dashIsoGroupSettings_dashManifestStyle,
    dashIsoGroupSettings_destination,
    dashIsoGroupSettings_destinationSettings,
    dashIsoGroupSettings_encryption,
    dashIsoGroupSettings_fragmentLength,
    dashIsoGroupSettings_hbbtvCompliance,
    dashIsoGroupSettings_imageBasedTrickPlay,
    dashIsoGroupSettings_imageBasedTrickPlaySettings,
    dashIsoGroupSettings_minBufferTime,
    dashIsoGroupSettings_minFinalSegmentLength,
    dashIsoGroupSettings_mpdManifestBandwidthType,
    dashIsoGroupSettings_mpdProfile,
    dashIsoGroupSettings_ptsOffsetHandlingForBFrames,
    dashIsoGroupSettings_segmentControl,
    dashIsoGroupSettings_segmentLength,
    dashIsoGroupSettings_segmentLengthControl,
    dashIsoGroupSettings_videoCompositionOffsets,
    dashIsoGroupSettings_writeSegmentTimelineInRepresentation,

    -- * DashIsoImageBasedTrickPlaySettings
    DashIsoImageBasedTrickPlaySettings (..),
    newDashIsoImageBasedTrickPlaySettings,
    dashIsoImageBasedTrickPlaySettings_intervalCadence,
    dashIsoImageBasedTrickPlaySettings_thumbnailHeight,
    dashIsoImageBasedTrickPlaySettings_thumbnailInterval,
    dashIsoImageBasedTrickPlaySettings_thumbnailWidth,
    dashIsoImageBasedTrickPlaySettings_tileHeight,
    dashIsoImageBasedTrickPlaySettings_tileWidth,

    -- * Deinterlacer
    Deinterlacer (..),
    newDeinterlacer,
    deinterlacer_algorithm,
    deinterlacer_control,
    deinterlacer_mode,

    -- * DestinationSettings
    DestinationSettings (..),
    newDestinationSettings,
    destinationSettings_s3Settings,

    -- * DolbyVision
    DolbyVision (..),
    newDolbyVision,
    dolbyVision_l6Metadata,
    dolbyVision_l6Mode,
    dolbyVision_mapping,
    dolbyVision_profile,

    -- * DolbyVisionLevel6Metadata
    DolbyVisionLevel6Metadata (..),
    newDolbyVisionLevel6Metadata,
    dolbyVisionLevel6Metadata_maxCll,
    dolbyVisionLevel6Metadata_maxFall,

    -- * DvbNitSettings
    DvbNitSettings (..),
    newDvbNitSettings,
    dvbNitSettings_networkId,
    dvbNitSettings_networkName,
    dvbNitSettings_nitInterval,

    -- * DvbSdtSettings
    DvbSdtSettings (..),
    newDvbSdtSettings,
    dvbSdtSettings_outputSdt,
    dvbSdtSettings_sdtInterval,
    dvbSdtSettings_serviceName,
    dvbSdtSettings_serviceProviderName,

    -- * DvbSubDestinationSettings
    DvbSubDestinationSettings (..),
    newDvbSubDestinationSettings,
    dvbSubDestinationSettings_alignment,
    dvbSubDestinationSettings_applyFontColor,
    dvbSubDestinationSettings_backgroundColor,
    dvbSubDestinationSettings_backgroundOpacity,
    dvbSubDestinationSettings_ddsHandling,
    dvbSubDestinationSettings_ddsXCoordinate,
    dvbSubDestinationSettings_ddsYCoordinate,
    dvbSubDestinationSettings_fallbackFont,
    dvbSubDestinationSettings_fontColor,
    dvbSubDestinationSettings_fontOpacity,
    dvbSubDestinationSettings_fontResolution,
    dvbSubDestinationSettings_fontScript,
    dvbSubDestinationSettings_fontSize,
    dvbSubDestinationSettings_height,
    dvbSubDestinationSettings_hexFontColor,
    dvbSubDestinationSettings_outlineColor,
    dvbSubDestinationSettings_outlineSize,
    dvbSubDestinationSettings_shadowColor,
    dvbSubDestinationSettings_shadowOpacity,
    dvbSubDestinationSettings_shadowXOffset,
    dvbSubDestinationSettings_shadowYOffset,
    dvbSubDestinationSettings_stylePassthrough,
    dvbSubDestinationSettings_subtitlingType,
    dvbSubDestinationSettings_teletextSpacing,
    dvbSubDestinationSettings_width,
    dvbSubDestinationSettings_xPosition,
    dvbSubDestinationSettings_yPosition,

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
    eac3AtmosSettings_bitrate,
    eac3AtmosSettings_bitstreamMode,
    eac3AtmosSettings_codingMode,
    eac3AtmosSettings_dialogueIntelligence,
    eac3AtmosSettings_downmixControl,
    eac3AtmosSettings_dynamicRangeCompressionLine,
    eac3AtmosSettings_dynamicRangeCompressionRf,
    eac3AtmosSettings_dynamicRangeControl,
    eac3AtmosSettings_loRoCenterMixLevel,
    eac3AtmosSettings_loRoSurroundMixLevel,
    eac3AtmosSettings_ltRtCenterMixLevel,
    eac3AtmosSettings_ltRtSurroundMixLevel,
    eac3AtmosSettings_meteringMode,
    eac3AtmosSettings_sampleRate,
    eac3AtmosSettings_speechThreshold,
    eac3AtmosSettings_stereoDownmix,
    eac3AtmosSettings_surroundExMode,

    -- * Eac3Settings
    Eac3Settings (..),
    newEac3Settings,
    eac3Settings_attenuationControl,
    eac3Settings_bitrate,
    eac3Settings_bitstreamMode,
    eac3Settings_codingMode,
    eac3Settings_dcFilter,
    eac3Settings_dialnorm,
    eac3Settings_dynamicRangeCompressionLine,
    eac3Settings_dynamicRangeCompressionRf,
    eac3Settings_lfeControl,
    eac3Settings_lfeFilter,
    eac3Settings_loRoCenterMixLevel,
    eac3Settings_loRoSurroundMixLevel,
    eac3Settings_ltRtCenterMixLevel,
    eac3Settings_ltRtSurroundMixLevel,
    eac3Settings_metadataControl,
    eac3Settings_passthroughControl,
    eac3Settings_phaseControl,
    eac3Settings_sampleRate,
    eac3Settings_stereoDownmix,
    eac3Settings_surroundExMode,
    eac3Settings_surroundMode,

    -- * EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    newEmbeddedDestinationSettings,
    embeddedDestinationSettings_destination608ChannelNumber,
    embeddedDestinationSettings_destination708ServiceNumber,

    -- * EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    newEmbeddedSourceSettings,
    embeddedSourceSettings_convert608To708,
    embeddedSourceSettings_source608ChannelNumber,
    embeddedSourceSettings_source608TrackNumber,
    embeddedSourceSettings_terminateCaptions,

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
    esamSettings_manifestConfirmConditionNotification,
    esamSettings_responseSignalPreroll,
    esamSettings_signalProcessingNotification,

    -- * EsamSignalProcessingNotification
    EsamSignalProcessingNotification (..),
    newEsamSignalProcessingNotification,
    esamSignalProcessingNotification_sccXml,

    -- * ExtendedDataServices
    ExtendedDataServices (..),
    newExtendedDataServices,
    extendedDataServices_copyProtectionAction,
    extendedDataServices_vchipAction,

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
    fileSourceSettings_convertPaintToPop,
    fileSourceSettings_framerate,
    fileSourceSettings_sourceFile,
    fileSourceSettings_timeDelta,
    fileSourceSettings_timeDeltaUnits,

    -- * ForceIncludeRenditionSize
    ForceIncludeRenditionSize (..),
    newForceIncludeRenditionSize,
    forceIncludeRenditionSize_height,
    forceIncludeRenditionSize_width,

    -- * FrameCaptureSettings
    FrameCaptureSettings (..),
    newFrameCaptureSettings,
    frameCaptureSettings_framerateDenominator,
    frameCaptureSettings_framerateNumerator,
    frameCaptureSettings_maxCaptures,
    frameCaptureSettings_quality,

    -- * H264QvbrSettings
    H264QvbrSettings (..),
    newH264QvbrSettings,
    h264QvbrSettings_maxAverageBitrate,
    h264QvbrSettings_qvbrQualityLevel,
    h264QvbrSettings_qvbrQualityLevelFineTune,

    -- * H264Settings
    H264Settings (..),
    newH264Settings,
    h264Settings_adaptiveQuantization,
    h264Settings_bandwidthReductionFilter,
    h264Settings_bitrate,
    h264Settings_codecLevel,
    h264Settings_codecProfile,
    h264Settings_dynamicSubGop,
    h264Settings_entropyEncoding,
    h264Settings_fieldEncoding,
    h264Settings_flickerAdaptiveQuantization,
    h264Settings_framerateControl,
    h264Settings_framerateConversionAlgorithm,
    h264Settings_framerateDenominator,
    h264Settings_framerateNumerator,
    h264Settings_gopBReference,
    h264Settings_gopClosedCadence,
    h264Settings_gopSize,
    h264Settings_gopSizeUnits,
    h264Settings_hrdBufferFinalFillPercentage,
    h264Settings_hrdBufferInitialFillPercentage,
    h264Settings_hrdBufferSize,
    h264Settings_interlaceMode,
    h264Settings_maxBitrate,
    h264Settings_minIInterval,
    h264Settings_numberBFramesBetweenReferenceFrames,
    h264Settings_numberReferenceFrames,
    h264Settings_parControl,
    h264Settings_parDenominator,
    h264Settings_parNumerator,
    h264Settings_qualityTuningLevel,
    h264Settings_qvbrSettings,
    h264Settings_rateControlMode,
    h264Settings_repeatPps,
    h264Settings_scanTypeConversionMode,
    h264Settings_sceneChangeDetect,
    h264Settings_slices,
    h264Settings_slowPal,
    h264Settings_softness,
    h264Settings_spatialAdaptiveQuantization,
    h264Settings_syntax,
    h264Settings_telecine,
    h264Settings_temporalAdaptiveQuantization,
    h264Settings_unregisteredSeiTimecode,

    -- * H265QvbrSettings
    H265QvbrSettings (..),
    newH265QvbrSettings,
    h265QvbrSettings_maxAverageBitrate,
    h265QvbrSettings_qvbrQualityLevel,
    h265QvbrSettings_qvbrQualityLevelFineTune,

    -- * H265Settings
    H265Settings (..),
    newH265Settings,
    h265Settings_adaptiveQuantization,
    h265Settings_alternateTransferFunctionSei,
    h265Settings_bandwidthReductionFilter,
    h265Settings_bitrate,
    h265Settings_codecLevel,
    h265Settings_codecProfile,
    h265Settings_dynamicSubGop,
    h265Settings_flickerAdaptiveQuantization,
    h265Settings_framerateControl,
    h265Settings_framerateConversionAlgorithm,
    h265Settings_framerateDenominator,
    h265Settings_framerateNumerator,
    h265Settings_gopBReference,
    h265Settings_gopClosedCadence,
    h265Settings_gopSize,
    h265Settings_gopSizeUnits,
    h265Settings_hrdBufferFinalFillPercentage,
    h265Settings_hrdBufferInitialFillPercentage,
    h265Settings_hrdBufferSize,
    h265Settings_interlaceMode,
    h265Settings_maxBitrate,
    h265Settings_minIInterval,
    h265Settings_numberBFramesBetweenReferenceFrames,
    h265Settings_numberReferenceFrames,
    h265Settings_parControl,
    h265Settings_parDenominator,
    h265Settings_parNumerator,
    h265Settings_qualityTuningLevel,
    h265Settings_qvbrSettings,
    h265Settings_rateControlMode,
    h265Settings_sampleAdaptiveOffsetFilterMode,
    h265Settings_scanTypeConversionMode,
    h265Settings_sceneChangeDetect,
    h265Settings_slices,
    h265Settings_slowPal,
    h265Settings_spatialAdaptiveQuantization,
    h265Settings_telecine,
    h265Settings_temporalAdaptiveQuantization,
    h265Settings_temporalIds,
    h265Settings_tiles,
    h265Settings_unregisteredSeiTimecode,
    h265Settings_writeMp4PackagingType,

    -- * Hdr10Metadata
    Hdr10Metadata (..),
    newHdr10Metadata,
    hdr10Metadata_bluePrimaryX,
    hdr10Metadata_bluePrimaryY,
    hdr10Metadata_greenPrimaryX,
    hdr10Metadata_greenPrimaryY,
    hdr10Metadata_maxContentLightLevel,
    hdr10Metadata_maxFrameAverageLightLevel,
    hdr10Metadata_maxLuminance,
    hdr10Metadata_minLuminance,
    hdr10Metadata_redPrimaryX,
    hdr10Metadata_redPrimaryY,
    hdr10Metadata_whitePointX,
    hdr10Metadata_whitePointY,

    -- * Hdr10Plus
    Hdr10Plus (..),
    newHdr10Plus,
    hdr10Plus_masteringMonitorNits,
    hdr10Plus_targetMonitorNits,

    -- * HlsAdditionalManifest
    HlsAdditionalManifest (..),
    newHlsAdditionalManifest,
    hlsAdditionalManifest_manifestNameModifier,
    hlsAdditionalManifest_selectedOutputs,

    -- * HlsCaptionLanguageMapping
    HlsCaptionLanguageMapping (..),
    newHlsCaptionLanguageMapping,
    hlsCaptionLanguageMapping_captionChannel,
    hlsCaptionLanguageMapping_customLanguageCode,
    hlsCaptionLanguageMapping_languageCode,
    hlsCaptionLanguageMapping_languageDescription,

    -- * HlsEncryptionSettings
    HlsEncryptionSettings (..),
    newHlsEncryptionSettings,
    hlsEncryptionSettings_constantInitializationVector,
    hlsEncryptionSettings_encryptionMethod,
    hlsEncryptionSettings_initializationVectorInManifest,
    hlsEncryptionSettings_offlineEncrypted,
    hlsEncryptionSettings_spekeKeyProvider,
    hlsEncryptionSettings_staticKeyProvider,
    hlsEncryptionSettings_type,

    -- * HlsGroupSettings
    HlsGroupSettings (..),
    newHlsGroupSettings,
    hlsGroupSettings_adMarkers,
    hlsGroupSettings_additionalManifests,
    hlsGroupSettings_audioOnlyHeader,
    hlsGroupSettings_baseUrl,
    hlsGroupSettings_captionLanguageMappings,
    hlsGroupSettings_captionLanguageSetting,
    hlsGroupSettings_captionSegmentLengthControl,
    hlsGroupSettings_clientCache,
    hlsGroupSettings_codecSpecification,
    hlsGroupSettings_destination,
    hlsGroupSettings_destinationSettings,
    hlsGroupSettings_directoryStructure,
    hlsGroupSettings_encryption,
    hlsGroupSettings_imageBasedTrickPlay,
    hlsGroupSettings_imageBasedTrickPlaySettings,
    hlsGroupSettings_manifestCompression,
    hlsGroupSettings_manifestDurationFormat,
    hlsGroupSettings_minFinalSegmentLength,
    hlsGroupSettings_minSegmentLength,
    hlsGroupSettings_outputSelection,
    hlsGroupSettings_programDateTime,
    hlsGroupSettings_programDateTimePeriod,
    hlsGroupSettings_progressiveWriteHlsManifest,
    hlsGroupSettings_segmentControl,
    hlsGroupSettings_segmentLength,
    hlsGroupSettings_segmentLengthControl,
    hlsGroupSettings_segmentsPerSubdirectory,
    hlsGroupSettings_streamInfResolution,
    hlsGroupSettings_targetDurationCompatibilityMode,
    hlsGroupSettings_timedMetadataId3Frame,
    hlsGroupSettings_timedMetadataId3Period,
    hlsGroupSettings_timestampDeltaMilliseconds,

    -- * HlsImageBasedTrickPlaySettings
    HlsImageBasedTrickPlaySettings (..),
    newHlsImageBasedTrickPlaySettings,
    hlsImageBasedTrickPlaySettings_intervalCadence,
    hlsImageBasedTrickPlaySettings_thumbnailHeight,
    hlsImageBasedTrickPlaySettings_thumbnailInterval,
    hlsImageBasedTrickPlaySettings_thumbnailWidth,
    hlsImageBasedTrickPlaySettings_tileHeight,
    hlsImageBasedTrickPlaySettings_tileWidth,

    -- * HlsRenditionGroupSettings
    HlsRenditionGroupSettings (..),
    newHlsRenditionGroupSettings,
    hlsRenditionGroupSettings_renditionGroupId,
    hlsRenditionGroupSettings_renditionLanguageCode,
    hlsRenditionGroupSettings_renditionName,

    -- * HlsSettings
    HlsSettings (..),
    newHlsSettings,
    hlsSettings_audioGroupId,
    hlsSettings_audioOnlyContainer,
    hlsSettings_audioRenditionSets,
    hlsSettings_audioTrackType,
    hlsSettings_descriptiveVideoServiceFlag,
    hlsSettings_iFrameOnlyManifest,
    hlsSettings_segmentModifier,

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
    imageInserter_sdrReferenceWhiteLevel,

    -- * ImscDestinationSettings
    ImscDestinationSettings (..),
    newImscDestinationSettings,
    imscDestinationSettings_accessibility,
    imscDestinationSettings_stylePassthrough,

    -- * Input
    Input (..),
    newInput,
    input_advancedInputFilter,
    input_advancedInputFilterSettings,
    input_audioSelectorGroups,
    input_audioSelectors,
    input_captionSelectors,
    input_crop,
    input_deblockFilter,
    input_decryptionSettings,
    input_denoiseFilter,
    input_dolbyVisionMetadataXml,
    input_fileInput,
    input_filterEnable,
    input_filterStrength,
    input_imageInserter,
    input_inputClippings,
    input_inputScanType,
    input_position,
    input_programNumber,
    input_psiControl,
    input_supplementalImps,
    input_timecodeSource,
    input_timecodeStart,
    input_videoGenerator,
    input_videoSelector,

    -- * InputClipping
    InputClipping (..),
    newInputClipping,
    inputClipping_endTimecode,
    inputClipping_startTimecode,

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
    inputTemplate_advancedInputFilter,
    inputTemplate_advancedInputFilterSettings,
    inputTemplate_audioSelectorGroups,
    inputTemplate_audioSelectors,
    inputTemplate_captionSelectors,
    inputTemplate_crop,
    inputTemplate_deblockFilter,
    inputTemplate_denoiseFilter,
    inputTemplate_dolbyVisionMetadataXml,
    inputTemplate_filterEnable,
    inputTemplate_filterStrength,
    inputTemplate_imageInserter,
    inputTemplate_inputClippings,
    inputTemplate_inputScanType,
    inputTemplate_position,
    inputTemplate_programNumber,
    inputTemplate_psiControl,
    inputTemplate_timecodeSource,
    inputTemplate_timecodeStart,
    inputTemplate_videoSelector,

    -- * InputVideoGenerator
    InputVideoGenerator (..),
    newInputVideoGenerator,
    inputVideoGenerator_duration,

    -- * InsertableImage
    InsertableImage (..),
    newInsertableImage,
    insertableImage_duration,
    insertableImage_fadeIn,
    insertableImage_fadeOut,
    insertableImage_height,
    insertableImage_imageInserterInput,
    insertableImage_imageX,
    insertableImage_imageY,
    insertableImage_layer,
    insertableImage_opacity,
    insertableImage_startTime,
    insertableImage_width,

    -- * Job
    Job (..),
    newJob,
    job_accelerationSettings,
    job_accelerationStatus,
    job_arn,
    job_billingTagsSource,
    job_clientRequestToken,
    job_createdAt,
    job_currentPhase,
    job_errorCode,
    job_errorMessage,
    job_hopDestinations,
    job_id,
    job_jobPercentComplete,
    job_jobTemplate,
    job_messages,
    job_outputGroupDetails,
    job_priority,
    job_queue,
    job_queueTransitions,
    job_retryCount,
    job_simulateReservedQueue,
    job_status,
    job_statusUpdateInterval,
    job_timing,
    job_userMetadata,
    job_warnings,
    job_settings,
    job_role,

    -- * JobMessages
    JobMessages (..),
    newJobMessages,
    jobMessages_info,
    jobMessages_warning,

    -- * JobSettings
    JobSettings (..),
    newJobSettings,
    jobSettings_adAvailOffset,
    jobSettings_availBlanking,
    jobSettings_esam,
    jobSettings_extendedDataServices,
    jobSettings_inputs,
    jobSettings_kantarWatermark,
    jobSettings_motionImageInserter,
    jobSettings_nielsenConfiguration,
    jobSettings_nielsenNonLinearWatermark,
    jobSettings_outputGroups,
    jobSettings_timecodeConfig,
    jobSettings_timedMetadataInsertion,

    -- * JobTemplate
    JobTemplate (..),
    newJobTemplate,
    jobTemplate_accelerationSettings,
    jobTemplate_arn,
    jobTemplate_category,
    jobTemplate_createdAt,
    jobTemplate_description,
    jobTemplate_hopDestinations,
    jobTemplate_lastUpdated,
    jobTemplate_priority,
    jobTemplate_queue,
    jobTemplate_statusUpdateInterval,
    jobTemplate_type,
    jobTemplate_settings,
    jobTemplate_name,

    -- * JobTemplateSettings
    JobTemplateSettings (..),
    newJobTemplateSettings,
    jobTemplateSettings_adAvailOffset,
    jobTemplateSettings_availBlanking,
    jobTemplateSettings_esam,
    jobTemplateSettings_extendedDataServices,
    jobTemplateSettings_inputs,
    jobTemplateSettings_kantarWatermark,
    jobTemplateSettings_motionImageInserter,
    jobTemplateSettings_nielsenConfiguration,
    jobTemplateSettings_nielsenNonLinearWatermark,
    jobTemplateSettings_outputGroups,
    jobTemplateSettings_timecodeConfig,
    jobTemplateSettings_timedMetadataInsertion,

    -- * KantarWatermarkSettings
    KantarWatermarkSettings (..),
    newKantarWatermarkSettings,
    kantarWatermarkSettings_channelName,
    kantarWatermarkSettings_contentReference,
    kantarWatermarkSettings_credentialsSecretName,
    kantarWatermarkSettings_fileOffset,
    kantarWatermarkSettings_kantarLicenseId,
    kantarWatermarkSettings_kantarServerUrl,
    kantarWatermarkSettings_logDestination,
    kantarWatermarkSettings_metadata3,
    kantarWatermarkSettings_metadata4,
    kantarWatermarkSettings_metadata5,
    kantarWatermarkSettings_metadata6,
    kantarWatermarkSettings_metadata7,
    kantarWatermarkSettings_metadata8,

    -- * M2tsScte35Esam
    M2tsScte35Esam (..),
    newM2tsScte35Esam,
    m2tsScte35Esam_scte35EsamPid,

    -- * M2tsSettings
    M2tsSettings (..),
    newM2tsSettings,
    m2tsSettings_audioBufferModel,
    m2tsSettings_audioDuration,
    m2tsSettings_audioFramesPerPes,
    m2tsSettings_audioPids,
    m2tsSettings_bitrate,
    m2tsSettings_bufferModel,
    m2tsSettings_dataPTSControl,
    m2tsSettings_dvbNitSettings,
    m2tsSettings_dvbSdtSettings,
    m2tsSettings_dvbSubPids,
    m2tsSettings_dvbTdtSettings,
    m2tsSettings_dvbTeletextPid,
    m2tsSettings_ebpAudioInterval,
    m2tsSettings_ebpPlacement,
    m2tsSettings_esRateInPes,
    m2tsSettings_forceTsVideoEbpOrder,
    m2tsSettings_fragmentTime,
    m2tsSettings_klvMetadata,
    m2tsSettings_maxPcrInterval,
    m2tsSettings_minEbpInterval,
    m2tsSettings_nielsenId3,
    m2tsSettings_nullPacketBitrate,
    m2tsSettings_patInterval,
    m2tsSettings_pcrControl,
    m2tsSettings_pcrPid,
    m2tsSettings_pmtInterval,
    m2tsSettings_pmtPid,
    m2tsSettings_privateMetadataPid,
    m2tsSettings_programNumber,
    m2tsSettings_rateMode,
    m2tsSettings_scte35Esam,
    m2tsSettings_scte35Pid,
    m2tsSettings_scte35Source,
    m2tsSettings_segmentationMarkers,
    m2tsSettings_segmentationStyle,
    m2tsSettings_segmentationTime,
    m2tsSettings_timedMetadataPid,
    m2tsSettings_transportStreamId,
    m2tsSettings_videoPid,

    -- * M3u8Settings
    M3u8Settings (..),
    newM3u8Settings,
    m3u8Settings_audioDuration,
    m3u8Settings_audioFramesPerPes,
    m3u8Settings_audioPids,
    m3u8Settings_dataPTSControl,
    m3u8Settings_maxPcrInterval,
    m3u8Settings_nielsenId3,
    m3u8Settings_patInterval,
    m3u8Settings_pcrControl,
    m3u8Settings_pcrPid,
    m3u8Settings_pmtInterval,
    m3u8Settings_pmtPid,
    m3u8Settings_privateMetadataPid,
    m3u8Settings_programNumber,
    m3u8Settings_scte35Pid,
    m3u8Settings_scte35Source,
    m3u8Settings_timedMetadata,
    m3u8Settings_timedMetadataPid,
    m3u8Settings_transportStreamId,
    m3u8Settings_videoPid,

    -- * MinBottomRenditionSize
    MinBottomRenditionSize (..),
    newMinBottomRenditionSize,
    minBottomRenditionSize_height,
    minBottomRenditionSize_width,

    -- * MinTopRenditionSize
    MinTopRenditionSize (..),
    newMinTopRenditionSize,
    minTopRenditionSize_height,
    minTopRenditionSize_width,

    -- * MotionImageInserter
    MotionImageInserter (..),
    newMotionImageInserter,
    motionImageInserter_framerate,
    motionImageInserter_input,
    motionImageInserter_insertionMode,
    motionImageInserter_offset,
    motionImageInserter_playback,
    motionImageInserter_startTime,

    -- * MotionImageInsertionFramerate
    MotionImageInsertionFramerate (..),
    newMotionImageInsertionFramerate,
    motionImageInsertionFramerate_framerateDenominator,
    motionImageInsertionFramerate_framerateNumerator,

    -- * MotionImageInsertionOffset
    MotionImageInsertionOffset (..),
    newMotionImageInsertionOffset,
    motionImageInsertionOffset_imageX,
    motionImageInsertionOffset_imageY,

    -- * MovSettings
    MovSettings (..),
    newMovSettings,
    movSettings_clapAtom,
    movSettings_cslgAtom,
    movSettings_mpeg2FourCCControl,
    movSettings_paddingControl,
    movSettings_reference,

    -- * Mp2Settings
    Mp2Settings (..),
    newMp2Settings,
    mp2Settings_bitrate,
    mp2Settings_channels,
    mp2Settings_sampleRate,

    -- * Mp3Settings
    Mp3Settings (..),
    newMp3Settings,
    mp3Settings_bitrate,
    mp3Settings_channels,
    mp3Settings_rateControlMode,
    mp3Settings_sampleRate,
    mp3Settings_vbrQuality,

    -- * Mp4Settings
    Mp4Settings (..),
    newMp4Settings,
    mp4Settings_audioDuration,
    mp4Settings_cslgAtom,
    mp4Settings_cttsVersion,
    mp4Settings_freeSpaceBox,
    mp4Settings_moovPlacement,
    mp4Settings_mp4MajorBrand,

    -- * MpdSettings
    MpdSettings (..),
    newMpdSettings,
    mpdSettings_accessibilityCaptionHints,
    mpdSettings_audioDuration,
    mpdSettings_captionContainerType,
    mpdSettings_klvMetadata,
    mpdSettings_manifestMetadataSignaling,
    mpdSettings_scte35Esam,
    mpdSettings_scte35Source,
    mpdSettings_timedMetadata,
    mpdSettings_timedMetadataBoxVersion,
    mpdSettings_timedMetadataSchemeIdUri,
    mpdSettings_timedMetadataValue,

    -- * Mpeg2Settings
    Mpeg2Settings (..),
    newMpeg2Settings,
    mpeg2Settings_adaptiveQuantization,
    mpeg2Settings_bitrate,
    mpeg2Settings_codecLevel,
    mpeg2Settings_codecProfile,
    mpeg2Settings_dynamicSubGop,
    mpeg2Settings_framerateControl,
    mpeg2Settings_framerateConversionAlgorithm,
    mpeg2Settings_framerateDenominator,
    mpeg2Settings_framerateNumerator,
    mpeg2Settings_gopClosedCadence,
    mpeg2Settings_gopSize,
    mpeg2Settings_gopSizeUnits,
    mpeg2Settings_hrdBufferFinalFillPercentage,
    mpeg2Settings_hrdBufferInitialFillPercentage,
    mpeg2Settings_hrdBufferSize,
    mpeg2Settings_interlaceMode,
    mpeg2Settings_intraDcPrecision,
    mpeg2Settings_maxBitrate,
    mpeg2Settings_minIInterval,
    mpeg2Settings_numberBFramesBetweenReferenceFrames,
    mpeg2Settings_parControl,
    mpeg2Settings_parDenominator,
    mpeg2Settings_parNumerator,
    mpeg2Settings_qualityTuningLevel,
    mpeg2Settings_rateControlMode,
    mpeg2Settings_scanTypeConversionMode,
    mpeg2Settings_sceneChangeDetect,
    mpeg2Settings_slowPal,
    mpeg2Settings_softness,
    mpeg2Settings_spatialAdaptiveQuantization,
    mpeg2Settings_syntax,
    mpeg2Settings_telecine,
    mpeg2Settings_temporalAdaptiveQuantization,

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
    msSmoothGroupSettings_additionalManifests,
    msSmoothGroupSettings_audioDeduplication,
    msSmoothGroupSettings_destination,
    msSmoothGroupSettings_destinationSettings,
    msSmoothGroupSettings_encryption,
    msSmoothGroupSettings_fragmentLength,
    msSmoothGroupSettings_fragmentLengthControl,
    msSmoothGroupSettings_manifestEncoding,

    -- * MxfSettings
    MxfSettings (..),
    newMxfSettings,
    mxfSettings_afdSignaling,
    mxfSettings_profile,
    mxfSettings_xavcProfileSettings,

    -- * MxfXavcProfileSettings
    MxfXavcProfileSettings (..),
    newMxfXavcProfileSettings,
    mxfXavcProfileSettings_durationMode,
    mxfXavcProfileSettings_maxAncDataSize,

    -- * NexGuardFileMarkerSettings
    NexGuardFileMarkerSettings (..),
    newNexGuardFileMarkerSettings,
    nexGuardFileMarkerSettings_license,
    nexGuardFileMarkerSettings_payload,
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
    nielsenNonLinearWatermarkSettings_activeWatermarkProcess,
    nielsenNonLinearWatermarkSettings_adiFilename,
    nielsenNonLinearWatermarkSettings_assetId,
    nielsenNonLinearWatermarkSettings_assetName,
    nielsenNonLinearWatermarkSettings_cbetSourceId,
    nielsenNonLinearWatermarkSettings_episodeId,
    nielsenNonLinearWatermarkSettings_metadataDestination,
    nielsenNonLinearWatermarkSettings_sourceId,
    nielsenNonLinearWatermarkSettings_sourceWatermarkStatus,
    nielsenNonLinearWatermarkSettings_ticServerUrl,
    nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack,

    -- * NoiseReducer
    NoiseReducer (..),
    newNoiseReducer,
    noiseReducer_filter,
    noiseReducer_filterSettings,
    noiseReducer_spatialFilterSettings,
    noiseReducer_temporalFilterSettings,

    -- * NoiseReducerFilterSettings
    NoiseReducerFilterSettings (..),
    newNoiseReducerFilterSettings,
    noiseReducerFilterSettings_strength,

    -- * NoiseReducerSpatialFilterSettings
    NoiseReducerSpatialFilterSettings (..),
    newNoiseReducerSpatialFilterSettings,
    noiseReducerSpatialFilterSettings_postFilterSharpenStrength,
    noiseReducerSpatialFilterSettings_speed,
    noiseReducerSpatialFilterSettings_strength,

    -- * NoiseReducerTemporalFilterSettings
    NoiseReducerTemporalFilterSettings (..),
    newNoiseReducerTemporalFilterSettings,
    noiseReducerTemporalFilterSettings_aggressiveMode,
    noiseReducerTemporalFilterSettings_postTemporalSharpening,
    noiseReducerTemporalFilterSettings_postTemporalSharpeningStrength,
    noiseReducerTemporalFilterSettings_speed,
    noiseReducerTemporalFilterSettings_strength,

    -- * OpusSettings
    OpusSettings (..),
    newOpusSettings,
    opusSettings_bitrate,
    opusSettings_channels,
    opusSettings_sampleRate,

    -- * Output
    Output (..),
    newOutput,
    output_audioDescriptions,
    output_captionDescriptions,
    output_containerSettings,
    output_extension,
    output_nameModifier,
    output_outputSettings,
    output_preset,
    output_videoDescription,

    -- * OutputChannelMapping
    OutputChannelMapping (..),
    newOutputChannelMapping,
    outputChannelMapping_inputChannels,
    outputChannelMapping_inputChannelsFineTune,

    -- * OutputDetail
    OutputDetail (..),
    newOutputDetail,
    outputDetail_durationInMs,
    outputDetail_videoDetails,

    -- * OutputGroup
    OutputGroup (..),
    newOutputGroup,
    outputGroup_automatedEncodingSettings,
    outputGroup_customName,
    outputGroup_name,
    outputGroup_outputGroupSettings,
    outputGroup_outputs,

    -- * OutputGroupDetail
    OutputGroupDetail (..),
    newOutputGroupDetail,
    outputGroupDetail_outputDetails,

    -- * OutputGroupSettings
    OutputGroupSettings (..),
    newOutputGroupSettings,
    outputGroupSettings_cmafGroupSettings,
    outputGroupSettings_dashIsoGroupSettings,
    outputGroupSettings_fileGroupSettings,
    outputGroupSettings_hlsGroupSettings,
    outputGroupSettings_msSmoothGroupSettings,
    outputGroupSettings_type,

    -- * OutputSettings
    OutputSettings (..),
    newOutputSettings,
    outputSettings_hlsSettings,

    -- * PartnerWatermarking
    PartnerWatermarking (..),
    newPartnerWatermarking,
    partnerWatermarking_nexguardFileMarkerSettings,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_httpInputs,
    policy_httpsInputs,
    policy_s3Inputs,

    -- * Preset
    Preset (..),
    newPreset,
    preset_arn,
    preset_category,
    preset_createdAt,
    preset_description,
    preset_lastUpdated,
    preset_type,
    preset_settings,
    preset_name,

    -- * PresetSettings
    PresetSettings (..),
    newPresetSettings,
    presetSettings_audioDescriptions,
    presetSettings_captionDescriptions,
    presetSettings_containerSettings,
    presetSettings_videoDescription,

    -- * ProresSettings
    ProresSettings (..),
    newProresSettings,
    proresSettings_chromaSampling,
    proresSettings_codecProfile,
    proresSettings_framerateControl,
    proresSettings_framerateConversionAlgorithm,
    proresSettings_framerateDenominator,
    proresSettings_framerateNumerator,
    proresSettings_interlaceMode,
    proresSettings_parControl,
    proresSettings_parDenominator,
    proresSettings_parNumerator,
    proresSettings_scanTypeConversionMode,
    proresSettings_slowPal,
    proresSettings_telecine,

    -- * Queue
    Queue (..),
    newQueue,
    queue_arn,
    queue_createdAt,
    queue_description,
    queue_lastUpdated,
    queue_pricingPlan,
    queue_progressingJobsCount,
    queue_reservationPlan,
    queue_status,
    queue_submittedJobsCount,
    queue_type,
    queue_name,

    -- * QueueTransition
    QueueTransition (..),
    newQueueTransition,
    queueTransition_destinationQueue,
    queueTransition_sourceQueue,
    queueTransition_timestamp,

    -- * Rectangle
    Rectangle (..),
    newRectangle,
    rectangle_height,
    rectangle_width,
    rectangle_x,
    rectangle_y,

    -- * RemixSettings
    RemixSettings (..),
    newRemixSettings,
    remixSettings_channelMapping,
    remixSettings_channelsIn,
    remixSettings_channelsOut,

    -- * ReservationPlan
    ReservationPlan (..),
    newReservationPlan,
    reservationPlan_commitment,
    reservationPlan_expiresAt,
    reservationPlan_purchasedAt,
    reservationPlan_renewalType,
    reservationPlan_reservedSlots,
    reservationPlan_status,

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
    s3DestinationSettings_accessControl,
    s3DestinationSettings_encryption,

    -- * S3EncryptionSettings
    S3EncryptionSettings (..),
    newS3EncryptionSettings,
    s3EncryptionSettings_encryptionType,
    s3EncryptionSettings_kmsEncryptionContext,
    s3EncryptionSettings_kmsKeyArn,

    -- * SccDestinationSettings
    SccDestinationSettings (..),
    newSccDestinationSettings,
    sccDestinationSettings_framerate,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,

    -- * SpekeKeyProviderCmaf
    SpekeKeyProviderCmaf (..),
    newSpekeKeyProviderCmaf,
    spekeKeyProviderCmaf_certificateArn,
    spekeKeyProviderCmaf_dashSignaledSystemIds,
    spekeKeyProviderCmaf_hlsSignaledSystemIds,
    spekeKeyProviderCmaf_resourceId,
    spekeKeyProviderCmaf_url,

    -- * SrtDestinationSettings
    SrtDestinationSettings (..),
    newSrtDestinationSettings,
    srtDestinationSettings_stylePassthrough,

    -- * StaticKeyProvider
    StaticKeyProvider (..),
    newStaticKeyProvider,
    staticKeyProvider_keyFormat,
    staticKeyProvider_keyFormatVersions,
    staticKeyProvider_staticKeyValue,
    staticKeyProvider_url,

    -- * TeletextDestinationSettings
    TeletextDestinationSettings (..),
    newTeletextDestinationSettings,
    teletextDestinationSettings_pageNumber,
    teletextDestinationSettings_pageTypes,

    -- * TeletextSourceSettings
    TeletextSourceSettings (..),
    newTeletextSourceSettings,
    teletextSourceSettings_pageNumber,

    -- * TimecodeBurnin
    TimecodeBurnin (..),
    newTimecodeBurnin,
    timecodeBurnin_fontSize,
    timecodeBurnin_position,
    timecodeBurnin_prefix,

    -- * TimecodeConfig
    TimecodeConfig (..),
    newTimecodeConfig,
    timecodeConfig_anchor,
    timecodeConfig_source,
    timecodeConfig_start,
    timecodeConfig_timestampOffset,

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
    vc3Settings_framerateControl,
    vc3Settings_framerateConversionAlgorithm,
    vc3Settings_framerateDenominator,
    vc3Settings_framerateNumerator,
    vc3Settings_interlaceMode,
    vc3Settings_scanTypeConversionMode,
    vc3Settings_slowPal,
    vc3Settings_telecine,
    vc3Settings_vc3Class,

    -- * VideoCodecSettings
    VideoCodecSettings (..),
    newVideoCodecSettings,
    videoCodecSettings_av1Settings,
    videoCodecSettings_avcIntraSettings,
    videoCodecSettings_codec,
    videoCodecSettings_frameCaptureSettings,
    videoCodecSettings_h264Settings,
    videoCodecSettings_h265Settings,
    videoCodecSettings_mpeg2Settings,
    videoCodecSettings_proresSettings,
    videoCodecSettings_vc3Settings,
    videoCodecSettings_vp8Settings,
    videoCodecSettings_vp9Settings,
    videoCodecSettings_xavcSettings,

    -- * VideoDescription
    VideoDescription (..),
    newVideoDescription,
    videoDescription_afdSignaling,
    videoDescription_antiAlias,
    videoDescription_codecSettings,
    videoDescription_colorMetadata,
    videoDescription_crop,
    videoDescription_dropFrameTimecode,
    videoDescription_fixedAfd,
    videoDescription_height,
    videoDescription_position,
    videoDescription_respondToAfd,
    videoDescription_scalingBehavior,
    videoDescription_sharpness,
    videoDescription_timecodeInsertion,
    videoDescription_videoPreprocessors,
    videoDescription_width,

    -- * VideoDetail
    VideoDetail (..),
    newVideoDetail,
    videoDetail_heightInPx,
    videoDetail_widthInPx,

    -- * VideoPreprocessor
    VideoPreprocessor (..),
    newVideoPreprocessor,
    videoPreprocessor_colorCorrector,
    videoPreprocessor_deinterlacer,
    videoPreprocessor_dolbyVision,
    videoPreprocessor_hdr10Plus,
    videoPreprocessor_imageInserter,
    videoPreprocessor_noiseReducer,
    videoPreprocessor_partnerWatermarking,
    videoPreprocessor_timecodeBurnin,

    -- * VideoSelector
    VideoSelector (..),
    newVideoSelector,
    videoSelector_alphaBehavior,
    videoSelector_colorSpace,
    videoSelector_colorSpaceUsage,
    videoSelector_embeddedTimecodeOverride,
    videoSelector_hdr10Metadata,
    videoSelector_padVideo,
    videoSelector_pid,
    videoSelector_programNumber,
    videoSelector_rotate,
    videoSelector_sampleRange,

    -- * VorbisSettings
    VorbisSettings (..),
    newVorbisSettings,
    vorbisSettings_channels,
    vorbisSettings_sampleRate,
    vorbisSettings_vbrQuality,

    -- * Vp8Settings
    Vp8Settings (..),
    newVp8Settings,
    vp8Settings_bitrate,
    vp8Settings_framerateControl,
    vp8Settings_framerateConversionAlgorithm,
    vp8Settings_framerateDenominator,
    vp8Settings_framerateNumerator,
    vp8Settings_gopSize,
    vp8Settings_hrdBufferSize,
    vp8Settings_maxBitrate,
    vp8Settings_parControl,
    vp8Settings_parDenominator,
    vp8Settings_parNumerator,
    vp8Settings_qualityTuningLevel,
    vp8Settings_rateControlMode,

    -- * Vp9Settings
    Vp9Settings (..),
    newVp9Settings,
    vp9Settings_bitrate,
    vp9Settings_framerateControl,
    vp9Settings_framerateConversionAlgorithm,
    vp9Settings_framerateDenominator,
    vp9Settings_framerateNumerator,
    vp9Settings_gopSize,
    vp9Settings_hrdBufferSize,
    vp9Settings_maxBitrate,
    vp9Settings_parControl,
    vp9Settings_parDenominator,
    vp9Settings_parNumerator,
    vp9Settings_qualityTuningLevel,
    vp9Settings_rateControlMode,

    -- * WarningGroup
    WarningGroup (..),
    newWarningGroup,
    warningGroup_count,
    warningGroup_code,

    -- * WavSettings
    WavSettings (..),
    newWavSettings,
    wavSettings_bitDepth,
    wavSettings_channels,
    wavSettings_format,
    wavSettings_sampleRate,

    -- * WebvttDestinationSettings
    WebvttDestinationSettings (..),
    newWebvttDestinationSettings,
    webvttDestinationSettings_accessibility,
    webvttDestinationSettings_stylePassthrough,

    -- * WebvttHlsSourceSettings
    WebvttHlsSourceSettings (..),
    newWebvttHlsSourceSettings,
    webvttHlsSourceSettings_renditionGroupId,
    webvttHlsSourceSettings_renditionLanguageCode,
    webvttHlsSourceSettings_renditionName,

    -- * Xavc4kIntraCbgProfileSettings
    Xavc4kIntraCbgProfileSettings (..),
    newXavc4kIntraCbgProfileSettings,
    xavc4kIntraCbgProfileSettings_xavcClass,

    -- * Xavc4kIntraVbrProfileSettings
    Xavc4kIntraVbrProfileSettings (..),
    newXavc4kIntraVbrProfileSettings,
    xavc4kIntraVbrProfileSettings_xavcClass,

    -- * Xavc4kProfileSettings
    Xavc4kProfileSettings (..),
    newXavc4kProfileSettings,
    xavc4kProfileSettings_bitrateClass,
    xavc4kProfileSettings_codecProfile,
    xavc4kProfileSettings_flickerAdaptiveQuantization,
    xavc4kProfileSettings_gopBReference,
    xavc4kProfileSettings_gopClosedCadence,
    xavc4kProfileSettings_hrdBufferSize,
    xavc4kProfileSettings_qualityTuningLevel,
    xavc4kProfileSettings_slices,

    -- * XavcHdIntraCbgProfileSettings
    XavcHdIntraCbgProfileSettings (..),
    newXavcHdIntraCbgProfileSettings,
    xavcHdIntraCbgProfileSettings_xavcClass,

    -- * XavcHdProfileSettings
    XavcHdProfileSettings (..),
    newXavcHdProfileSettings,
    xavcHdProfileSettings_bitrateClass,
    xavcHdProfileSettings_flickerAdaptiveQuantization,
    xavcHdProfileSettings_gopBReference,
    xavcHdProfileSettings_gopClosedCadence,
    xavcHdProfileSettings_hrdBufferSize,
    xavcHdProfileSettings_interlaceMode,
    xavcHdProfileSettings_qualityTuningLevel,
    xavcHdProfileSettings_slices,
    xavcHdProfileSettings_telecine,

    -- * XavcSettings
    XavcSettings (..),
    newXavcSettings,
    xavcSettings_adaptiveQuantization,
    xavcSettings_entropyEncoding,
    xavcSettings_framerateControl,
    xavcSettings_framerateConversionAlgorithm,
    xavcSettings_framerateDenominator,
    xavcSettings_framerateNumerator,
    xavcSettings_profile,
    xavcSettings_slowPal,
    xavcSettings_softness,
    xavcSettings_spatialAdaptiveQuantization,
    xavcSettings_temporalAdaptiveQuantization,
    xavcSettings_xavc4kIntraCbgProfileSettings,
    xavcSettings_xavc4kIntraVbrProfileSettings,
    xavcSettings_xavc4kProfileSettings,
    xavcSettings_xavcHdIntraCbgProfileSettings,
    xavcSettings_xavcHdProfileSettings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
import Amazonka.MediaConvert.Types.AacCodecProfile
import Amazonka.MediaConvert.Types.AacCodingMode
import Amazonka.MediaConvert.Types.AacRateControlMode
import Amazonka.MediaConvert.Types.AacRawFormat
import Amazonka.MediaConvert.Types.AacSettings
import Amazonka.MediaConvert.Types.AacSpecification
import Amazonka.MediaConvert.Types.AacVbrQuality
import Amazonka.MediaConvert.Types.Ac3BitstreamMode
import Amazonka.MediaConvert.Types.Ac3CodingMode
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Ac3LfeFilter
import Amazonka.MediaConvert.Types.Ac3MetadataControl
import Amazonka.MediaConvert.Types.Ac3Settings
import Amazonka.MediaConvert.Types.AccelerationMode
import Amazonka.MediaConvert.Types.AccelerationSettings
import Amazonka.MediaConvert.Types.AccelerationStatus
import Amazonka.MediaConvert.Types.AdvancedInputFilter
import Amazonka.MediaConvert.Types.AdvancedInputFilterAddTexture
import Amazonka.MediaConvert.Types.AdvancedInputFilterSettings
import Amazonka.MediaConvert.Types.AdvancedInputFilterSharpen
import Amazonka.MediaConvert.Types.AfdSignaling
import Amazonka.MediaConvert.Types.AiffSettings
import Amazonka.MediaConvert.Types.AllowedRenditionSize
import Amazonka.MediaConvert.Types.AlphaBehavior
import Amazonka.MediaConvert.Types.AncillaryConvert608To708
import Amazonka.MediaConvert.Types.AncillarySourceSettings
import Amazonka.MediaConvert.Types.AncillaryTerminateCaptions
import Amazonka.MediaConvert.Types.AntiAlias
import Amazonka.MediaConvert.Types.AudioChannelTag
import Amazonka.MediaConvert.Types.AudioChannelTaggingSettings
import Amazonka.MediaConvert.Types.AudioCodec
import Amazonka.MediaConvert.Types.AudioCodecSettings
import Amazonka.MediaConvert.Types.AudioDefaultSelection
import Amazonka.MediaConvert.Types.AudioDescription
import Amazonka.MediaConvert.Types.AudioDurationCorrection
import Amazonka.MediaConvert.Types.AudioLanguageCodeControl
import Amazonka.MediaConvert.Types.AudioNormalizationAlgorithm
import Amazonka.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Amazonka.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Amazonka.MediaConvert.Types.AudioNormalizationPeakCalculation
import Amazonka.MediaConvert.Types.AudioNormalizationSettings
import Amazonka.MediaConvert.Types.AudioSelector
import Amazonka.MediaConvert.Types.AudioSelectorGroup
import Amazonka.MediaConvert.Types.AudioSelectorType
import Amazonka.MediaConvert.Types.AudioTypeControl
import Amazonka.MediaConvert.Types.AutomatedAbrRule
import Amazonka.MediaConvert.Types.AutomatedAbrSettings
import Amazonka.MediaConvert.Types.AutomatedEncodingSettings
import Amazonka.MediaConvert.Types.Av1AdaptiveQuantization
import Amazonka.MediaConvert.Types.Av1BitDepth
import Amazonka.MediaConvert.Types.Av1FramerateControl
import Amazonka.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Av1QvbrSettings
import Amazonka.MediaConvert.Types.Av1RateControlMode
import Amazonka.MediaConvert.Types.Av1Settings
import Amazonka.MediaConvert.Types.Av1SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.AvailBlanking
import Amazonka.MediaConvert.Types.AvcIntraClass
import Amazonka.MediaConvert.Types.AvcIntraFramerateControl
import Amazonka.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.AvcIntraInterlaceMode
import Amazonka.MediaConvert.Types.AvcIntraScanTypeConversionMode
import Amazonka.MediaConvert.Types.AvcIntraSettings
import Amazonka.MediaConvert.Types.AvcIntraSlowPal
import Amazonka.MediaConvert.Types.AvcIntraTelecine
import Amazonka.MediaConvert.Types.AvcIntraUhdQualityTuningLevel
import Amazonka.MediaConvert.Types.AvcIntraUhdSettings
import Amazonka.MediaConvert.Types.BandwidthReductionFilter
import Amazonka.MediaConvert.Types.BandwidthReductionFilterSharpening
import Amazonka.MediaConvert.Types.BandwidthReductionFilterStrength
import Amazonka.MediaConvert.Types.BillingTagsSource
import Amazonka.MediaConvert.Types.BurnInSubtitleStylePassthrough
import Amazonka.MediaConvert.Types.BurninDestinationSettings
import Amazonka.MediaConvert.Types.BurninSubtitleAlignment
import Amazonka.MediaConvert.Types.BurninSubtitleApplyFontColor
import Amazonka.MediaConvert.Types.BurninSubtitleBackgroundColor
import Amazonka.MediaConvert.Types.BurninSubtitleFallbackFont
import Amazonka.MediaConvert.Types.BurninSubtitleFontColor
import Amazonka.MediaConvert.Types.BurninSubtitleOutlineColor
import Amazonka.MediaConvert.Types.BurninSubtitleShadowColor
import Amazonka.MediaConvert.Types.BurninSubtitleTeletextSpacing
import Amazonka.MediaConvert.Types.CaptionDescription
import Amazonka.MediaConvert.Types.CaptionDescriptionPreset
import Amazonka.MediaConvert.Types.CaptionDestinationSettings
import Amazonka.MediaConvert.Types.CaptionDestinationType
import Amazonka.MediaConvert.Types.CaptionSelector
import Amazonka.MediaConvert.Types.CaptionSourceConvertPaintOnToPopOn
import Amazonka.MediaConvert.Types.CaptionSourceFramerate
import Amazonka.MediaConvert.Types.CaptionSourceSettings
import Amazonka.MediaConvert.Types.CaptionSourceType
import Amazonka.MediaConvert.Types.ChannelMapping
import Amazonka.MediaConvert.Types.ClipLimits
import Amazonka.MediaConvert.Types.CmafAdditionalManifest
import Amazonka.MediaConvert.Types.CmafClientCache
import Amazonka.MediaConvert.Types.CmafCodecSpecification
import Amazonka.MediaConvert.Types.CmafEncryptionSettings
import Amazonka.MediaConvert.Types.CmafEncryptionType
import Amazonka.MediaConvert.Types.CmafGroupSettings
import Amazonka.MediaConvert.Types.CmafImageBasedTrickPlay
import Amazonka.MediaConvert.Types.CmafImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.CmafInitializationVectorInManifest
import Amazonka.MediaConvert.Types.CmafIntervalCadence
import Amazonka.MediaConvert.Types.CmafKeyProviderType
import Amazonka.MediaConvert.Types.CmafManifestCompression
import Amazonka.MediaConvert.Types.CmafManifestDurationFormat
import Amazonka.MediaConvert.Types.CmafMpdManifestBandwidthType
import Amazonka.MediaConvert.Types.CmafMpdProfile
import Amazonka.MediaConvert.Types.CmafPtsOffsetHandlingForBFrames
import Amazonka.MediaConvert.Types.CmafSegmentControl
import Amazonka.MediaConvert.Types.CmafSegmentLengthControl
import Amazonka.MediaConvert.Types.CmafStreamInfResolution
import Amazonka.MediaConvert.Types.CmafTargetDurationCompatibilityMode
import Amazonka.MediaConvert.Types.CmafVideoCompositionOffsets
import Amazonka.MediaConvert.Types.CmafWriteDASHManifest
import Amazonka.MediaConvert.Types.CmafWriteHLSManifest
import Amazonka.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
import Amazonka.MediaConvert.Types.CmfcAudioDuration
import Amazonka.MediaConvert.Types.CmfcAudioTrackType
import Amazonka.MediaConvert.Types.CmfcDescriptiveVideoServiceFlag
import Amazonka.MediaConvert.Types.CmfcIFrameOnlyManifest
import Amazonka.MediaConvert.Types.CmfcKlvMetadata
import Amazonka.MediaConvert.Types.CmfcManifestMetadataSignaling
import Amazonka.MediaConvert.Types.CmfcScte35Esam
import Amazonka.MediaConvert.Types.CmfcScte35Source
import Amazonka.MediaConvert.Types.CmfcSettings
import Amazonka.MediaConvert.Types.CmfcTimedMetadata
import Amazonka.MediaConvert.Types.CmfcTimedMetadataBoxVersion
import Amazonka.MediaConvert.Types.ColorCorrector
import Amazonka.MediaConvert.Types.ColorMetadata
import Amazonka.MediaConvert.Types.ColorSpace
import Amazonka.MediaConvert.Types.ColorSpaceConversion
import Amazonka.MediaConvert.Types.ColorSpaceUsage
import Amazonka.MediaConvert.Types.Commitment
import Amazonka.MediaConvert.Types.ContainerSettings
import Amazonka.MediaConvert.Types.ContainerType
import Amazonka.MediaConvert.Types.CopyProtectionAction
import Amazonka.MediaConvert.Types.DashAdditionalManifest
import Amazonka.MediaConvert.Types.DashIsoEncryptionSettings
import Amazonka.MediaConvert.Types.DashIsoGroupAudioChannelConfigSchemeIdUri
import Amazonka.MediaConvert.Types.DashIsoGroupSettings
import Amazonka.MediaConvert.Types.DashIsoHbbtvCompliance
import Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlay
import Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.DashIsoIntervalCadence
import Amazonka.MediaConvert.Types.DashIsoMpdManifestBandwidthType
import Amazonka.MediaConvert.Types.DashIsoMpdProfile
import Amazonka.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
import Amazonka.MediaConvert.Types.DashIsoPtsOffsetHandlingForBFrames
import Amazonka.MediaConvert.Types.DashIsoSegmentControl
import Amazonka.MediaConvert.Types.DashIsoSegmentLengthControl
import Amazonka.MediaConvert.Types.DashIsoVideoCompositionOffsets
import Amazonka.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Amazonka.MediaConvert.Types.DashManifestStyle
import Amazonka.MediaConvert.Types.DecryptionMode
import Amazonka.MediaConvert.Types.DeinterlaceAlgorithm
import Amazonka.MediaConvert.Types.Deinterlacer
import Amazonka.MediaConvert.Types.DeinterlacerControl
import Amazonka.MediaConvert.Types.DeinterlacerMode
import Amazonka.MediaConvert.Types.DescribeEndpointsMode
import Amazonka.MediaConvert.Types.DestinationSettings
import Amazonka.MediaConvert.Types.DolbyVision
import Amazonka.MediaConvert.Types.DolbyVisionLevel6Metadata
import Amazonka.MediaConvert.Types.DolbyVisionLevel6Mode
import Amazonka.MediaConvert.Types.DolbyVisionMapping
import Amazonka.MediaConvert.Types.DolbyVisionProfile
import Amazonka.MediaConvert.Types.DropFrameTimecode
import Amazonka.MediaConvert.Types.DvbNitSettings
import Amazonka.MediaConvert.Types.DvbSdtSettings
import Amazonka.MediaConvert.Types.DvbSubDestinationSettings
import Amazonka.MediaConvert.Types.DvbSubSourceSettings
import Amazonka.MediaConvert.Types.DvbSubSubtitleFallbackFont
import Amazonka.MediaConvert.Types.DvbSubtitleAlignment
import Amazonka.MediaConvert.Types.DvbSubtitleApplyFontColor
import Amazonka.MediaConvert.Types.DvbSubtitleBackgroundColor
import Amazonka.MediaConvert.Types.DvbSubtitleFontColor
import Amazonka.MediaConvert.Types.DvbSubtitleOutlineColor
import Amazonka.MediaConvert.Types.DvbSubtitleShadowColor
import Amazonka.MediaConvert.Types.DvbSubtitleStylePassthrough
import Amazonka.MediaConvert.Types.DvbSubtitleTeletextSpacing
import Amazonka.MediaConvert.Types.DvbSubtitlingType
import Amazonka.MediaConvert.Types.DvbTdtSettings
import Amazonka.MediaConvert.Types.DvbddsHandling
import Amazonka.MediaConvert.Types.Eac3AtmosBitstreamMode
import Amazonka.MediaConvert.Types.Eac3AtmosCodingMode
import Amazonka.MediaConvert.Types.Eac3AtmosDialogueIntelligence
import Amazonka.MediaConvert.Types.Eac3AtmosDownmixControl
import Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeControl
import Amazonka.MediaConvert.Types.Eac3AtmosMeteringMode
import Amazonka.MediaConvert.Types.Eac3AtmosSettings
import Amazonka.MediaConvert.Types.Eac3AtmosStereoDownmix
import Amazonka.MediaConvert.Types.Eac3AtmosSurroundExMode
import Amazonka.MediaConvert.Types.Eac3AttenuationControl
import Amazonka.MediaConvert.Types.Eac3BitstreamMode
import Amazonka.MediaConvert.Types.Eac3CodingMode
import Amazonka.MediaConvert.Types.Eac3DcFilter
import Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Eac3LfeControl
import Amazonka.MediaConvert.Types.Eac3LfeFilter
import Amazonka.MediaConvert.Types.Eac3MetadataControl
import Amazonka.MediaConvert.Types.Eac3PassthroughControl
import Amazonka.MediaConvert.Types.Eac3PhaseControl
import Amazonka.MediaConvert.Types.Eac3Settings
import Amazonka.MediaConvert.Types.Eac3StereoDownmix
import Amazonka.MediaConvert.Types.Eac3SurroundExMode
import Amazonka.MediaConvert.Types.Eac3SurroundMode
import Amazonka.MediaConvert.Types.EmbeddedConvert608To708
import Amazonka.MediaConvert.Types.EmbeddedDestinationSettings
import Amazonka.MediaConvert.Types.EmbeddedSourceSettings
import Amazonka.MediaConvert.Types.EmbeddedTerminateCaptions
import Amazonka.MediaConvert.Types.EmbeddedTimecodeOverride
import Amazonka.MediaConvert.Types.Endpoint
import Amazonka.MediaConvert.Types.EsamManifestConfirmConditionNotification
import Amazonka.MediaConvert.Types.EsamSettings
import Amazonka.MediaConvert.Types.EsamSignalProcessingNotification
import Amazonka.MediaConvert.Types.ExtendedDataServices
import Amazonka.MediaConvert.Types.F4vMoovPlacement
import Amazonka.MediaConvert.Types.F4vSettings
import Amazonka.MediaConvert.Types.FileGroupSettings
import Amazonka.MediaConvert.Types.FileSourceConvert608To708
import Amazonka.MediaConvert.Types.FileSourceSettings
import Amazonka.MediaConvert.Types.FileSourceTimeDeltaUnits
import Amazonka.MediaConvert.Types.FontScript
import Amazonka.MediaConvert.Types.ForceIncludeRenditionSize
import Amazonka.MediaConvert.Types.FrameCaptureSettings
import Amazonka.MediaConvert.Types.H264AdaptiveQuantization
import Amazonka.MediaConvert.Types.H264CodecLevel
import Amazonka.MediaConvert.Types.H264CodecProfile
import Amazonka.MediaConvert.Types.H264DynamicSubGop
import Amazonka.MediaConvert.Types.H264EntropyEncoding
import Amazonka.MediaConvert.Types.H264FieldEncoding
import Amazonka.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264FramerateControl
import Amazonka.MediaConvert.Types.H264FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.H264GopBReference
import Amazonka.MediaConvert.Types.H264GopSizeUnits
import Amazonka.MediaConvert.Types.H264InterlaceMode
import Amazonka.MediaConvert.Types.H264ParControl
import Amazonka.MediaConvert.Types.H264QualityTuningLevel
import Amazonka.MediaConvert.Types.H264QvbrSettings
import Amazonka.MediaConvert.Types.H264RateControlMode
import Amazonka.MediaConvert.Types.H264RepeatPps
import Amazonka.MediaConvert.Types.H264ScanTypeConversionMode
import Amazonka.MediaConvert.Types.H264SceneChangeDetect
import Amazonka.MediaConvert.Types.H264Settings
import Amazonka.MediaConvert.Types.H264SlowPal
import Amazonka.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264Syntax
import Amazonka.MediaConvert.Types.H264Telecine
import Amazonka.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Amazonka.MediaConvert.Types.H264UnregisteredSeiTimecode
import Amazonka.MediaConvert.Types.H265AdaptiveQuantization
import Amazonka.MediaConvert.Types.H265AlternateTransferFunctionSei
import Amazonka.MediaConvert.Types.H265CodecLevel
import Amazonka.MediaConvert.Types.H265CodecProfile
import Amazonka.MediaConvert.Types.H265DynamicSubGop
import Amazonka.MediaConvert.Types.H265FlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265FramerateControl
import Amazonka.MediaConvert.Types.H265FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.H265GopBReference
import Amazonka.MediaConvert.Types.H265GopSizeUnits
import Amazonka.MediaConvert.Types.H265InterlaceMode
import Amazonka.MediaConvert.Types.H265ParControl
import Amazonka.MediaConvert.Types.H265QualityTuningLevel
import Amazonka.MediaConvert.Types.H265QvbrSettings
import Amazonka.MediaConvert.Types.H265RateControlMode
import Amazonka.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
import Amazonka.MediaConvert.Types.H265ScanTypeConversionMode
import Amazonka.MediaConvert.Types.H265SceneChangeDetect
import Amazonka.MediaConvert.Types.H265Settings
import Amazonka.MediaConvert.Types.H265SlowPal
import Amazonka.MediaConvert.Types.H265SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265Telecine
import Amazonka.MediaConvert.Types.H265TemporalAdaptiveQuantization
import Amazonka.MediaConvert.Types.H265TemporalIds
import Amazonka.MediaConvert.Types.H265Tiles
import Amazonka.MediaConvert.Types.H265UnregisteredSeiTimecode
import Amazonka.MediaConvert.Types.H265WriteMp4PackagingType
import Amazonka.MediaConvert.Types.HDRToSDRToneMapper
import Amazonka.MediaConvert.Types.Hdr10Metadata
import Amazonka.MediaConvert.Types.Hdr10Plus
import Amazonka.MediaConvert.Types.HlsAdMarkers
import Amazonka.MediaConvert.Types.HlsAdditionalManifest
import Amazonka.MediaConvert.Types.HlsAudioOnlyContainer
import Amazonka.MediaConvert.Types.HlsAudioOnlyHeader
import Amazonka.MediaConvert.Types.HlsAudioTrackType
import Amazonka.MediaConvert.Types.HlsCaptionLanguageMapping
import Amazonka.MediaConvert.Types.HlsCaptionLanguageSetting
import Amazonka.MediaConvert.Types.HlsCaptionSegmentLengthControl
import Amazonka.MediaConvert.Types.HlsClientCache
import Amazonka.MediaConvert.Types.HlsCodecSpecification
import Amazonka.MediaConvert.Types.HlsDescriptiveVideoServiceFlag
import Amazonka.MediaConvert.Types.HlsDirectoryStructure
import Amazonka.MediaConvert.Types.HlsEncryptionSettings
import Amazonka.MediaConvert.Types.HlsEncryptionType
import Amazonka.MediaConvert.Types.HlsGroupSettings
import Amazonka.MediaConvert.Types.HlsIFrameOnlyManifest
import Amazonka.MediaConvert.Types.HlsImageBasedTrickPlay
import Amazonka.MediaConvert.Types.HlsImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.HlsInitializationVectorInManifest
import Amazonka.MediaConvert.Types.HlsIntervalCadence
import Amazonka.MediaConvert.Types.HlsKeyProviderType
import Amazonka.MediaConvert.Types.HlsManifestCompression
import Amazonka.MediaConvert.Types.HlsManifestDurationFormat
import Amazonka.MediaConvert.Types.HlsOfflineEncrypted
import Amazonka.MediaConvert.Types.HlsOutputSelection
import Amazonka.MediaConvert.Types.HlsProgramDateTime
import Amazonka.MediaConvert.Types.HlsProgressiveWriteHlsManifest
import Amazonka.MediaConvert.Types.HlsRenditionGroupSettings
import Amazonka.MediaConvert.Types.HlsSegmentControl
import Amazonka.MediaConvert.Types.HlsSegmentLengthControl
import Amazonka.MediaConvert.Types.HlsSettings
import Amazonka.MediaConvert.Types.HlsStreamInfResolution
import Amazonka.MediaConvert.Types.HlsTargetDurationCompatibilityMode
import Amazonka.MediaConvert.Types.HlsTimedMetadataId3Frame
import Amazonka.MediaConvert.Types.HopDestination
import Amazonka.MediaConvert.Types.Id3Insertion
import Amazonka.MediaConvert.Types.ImageInserter
import Amazonka.MediaConvert.Types.ImscAccessibilitySubs
import Amazonka.MediaConvert.Types.ImscDestinationSettings
import Amazonka.MediaConvert.Types.ImscStylePassthrough
import Amazonka.MediaConvert.Types.Input
import Amazonka.MediaConvert.Types.InputClipping
import Amazonka.MediaConvert.Types.InputDeblockFilter
import Amazonka.MediaConvert.Types.InputDecryptionSettings
import Amazonka.MediaConvert.Types.InputDenoiseFilter
import Amazonka.MediaConvert.Types.InputFilterEnable
import Amazonka.MediaConvert.Types.InputPolicy
import Amazonka.MediaConvert.Types.InputPsiControl
import Amazonka.MediaConvert.Types.InputRotate
import Amazonka.MediaConvert.Types.InputSampleRange
import Amazonka.MediaConvert.Types.InputScanType
import Amazonka.MediaConvert.Types.InputTemplate
import Amazonka.MediaConvert.Types.InputTimecodeSource
import Amazonka.MediaConvert.Types.InputVideoGenerator
import Amazonka.MediaConvert.Types.InsertableImage
import Amazonka.MediaConvert.Types.Job
import Amazonka.MediaConvert.Types.JobMessages
import Amazonka.MediaConvert.Types.JobPhase
import Amazonka.MediaConvert.Types.JobSettings
import Amazonka.MediaConvert.Types.JobStatus
import Amazonka.MediaConvert.Types.JobTemplate
import Amazonka.MediaConvert.Types.JobTemplateListBy
import Amazonka.MediaConvert.Types.JobTemplateSettings
import Amazonka.MediaConvert.Types.KantarWatermarkSettings
import Amazonka.MediaConvert.Types.LanguageCode
import Amazonka.MediaConvert.Types.M2tsAudioBufferModel
import Amazonka.MediaConvert.Types.M2tsAudioDuration
import Amazonka.MediaConvert.Types.M2tsBufferModel
import Amazonka.MediaConvert.Types.M2tsDataPtsControl
import Amazonka.MediaConvert.Types.M2tsEbpAudioInterval
import Amazonka.MediaConvert.Types.M2tsEbpPlacement
import Amazonka.MediaConvert.Types.M2tsEsRateInPes
import Amazonka.MediaConvert.Types.M2tsForceTsVideoEbpOrder
import Amazonka.MediaConvert.Types.M2tsKlvMetadata
import Amazonka.MediaConvert.Types.M2tsNielsenId3
import Amazonka.MediaConvert.Types.M2tsPcrControl
import Amazonka.MediaConvert.Types.M2tsRateMode
import Amazonka.MediaConvert.Types.M2tsScte35Esam
import Amazonka.MediaConvert.Types.M2tsScte35Source
import Amazonka.MediaConvert.Types.M2tsSegmentationMarkers
import Amazonka.MediaConvert.Types.M2tsSegmentationStyle
import Amazonka.MediaConvert.Types.M2tsSettings
import Amazonka.MediaConvert.Types.M3u8AudioDuration
import Amazonka.MediaConvert.Types.M3u8DataPtsControl
import Amazonka.MediaConvert.Types.M3u8NielsenId3
import Amazonka.MediaConvert.Types.M3u8PcrControl
import Amazonka.MediaConvert.Types.M3u8Scte35Source
import Amazonka.MediaConvert.Types.M3u8Settings
import Amazonka.MediaConvert.Types.MinBottomRenditionSize
import Amazonka.MediaConvert.Types.MinTopRenditionSize
import Amazonka.MediaConvert.Types.MotionImageInserter
import Amazonka.MediaConvert.Types.MotionImageInsertionFramerate
import Amazonka.MediaConvert.Types.MotionImageInsertionMode
import Amazonka.MediaConvert.Types.MotionImageInsertionOffset
import Amazonka.MediaConvert.Types.MotionImagePlayback
import Amazonka.MediaConvert.Types.MovClapAtom
import Amazonka.MediaConvert.Types.MovCslgAtom
import Amazonka.MediaConvert.Types.MovMpeg2FourCCControl
import Amazonka.MediaConvert.Types.MovPaddingControl
import Amazonka.MediaConvert.Types.MovReference
import Amazonka.MediaConvert.Types.MovSettings
import Amazonka.MediaConvert.Types.Mp2Settings
import Amazonka.MediaConvert.Types.Mp3RateControlMode
import Amazonka.MediaConvert.Types.Mp3Settings
import Amazonka.MediaConvert.Types.Mp4CslgAtom
import Amazonka.MediaConvert.Types.Mp4FreeSpaceBox
import Amazonka.MediaConvert.Types.Mp4MoovPlacement
import Amazonka.MediaConvert.Types.Mp4Settings
import Amazonka.MediaConvert.Types.MpdAccessibilityCaptionHints
import Amazonka.MediaConvert.Types.MpdAudioDuration
import Amazonka.MediaConvert.Types.MpdCaptionContainerType
import Amazonka.MediaConvert.Types.MpdKlvMetadata
import Amazonka.MediaConvert.Types.MpdManifestMetadataSignaling
import Amazonka.MediaConvert.Types.MpdScte35Esam
import Amazonka.MediaConvert.Types.MpdScte35Source
import Amazonka.MediaConvert.Types.MpdSettings
import Amazonka.MediaConvert.Types.MpdTimedMetadata
import Amazonka.MediaConvert.Types.MpdTimedMetadataBoxVersion
import Amazonka.MediaConvert.Types.Mpeg2AdaptiveQuantization
import Amazonka.MediaConvert.Types.Mpeg2CodecLevel
import Amazonka.MediaConvert.Types.Mpeg2CodecProfile
import Amazonka.MediaConvert.Types.Mpeg2DynamicSubGop
import Amazonka.MediaConvert.Types.Mpeg2FramerateControl
import Amazonka.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Mpeg2GopSizeUnits
import Amazonka.MediaConvert.Types.Mpeg2InterlaceMode
import Amazonka.MediaConvert.Types.Mpeg2IntraDcPrecision
import Amazonka.MediaConvert.Types.Mpeg2ParControl
import Amazonka.MediaConvert.Types.Mpeg2QualityTuningLevel
import Amazonka.MediaConvert.Types.Mpeg2RateControlMode
import Amazonka.MediaConvert.Types.Mpeg2ScanTypeConversionMode
import Amazonka.MediaConvert.Types.Mpeg2SceneChangeDetect
import Amazonka.MediaConvert.Types.Mpeg2Settings
import Amazonka.MediaConvert.Types.Mpeg2SlowPal
import Amazonka.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.Mpeg2Syntax
import Amazonka.MediaConvert.Types.Mpeg2Telecine
import Amazonka.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
import Amazonka.MediaConvert.Types.MsSmoothAdditionalManifest
import Amazonka.MediaConvert.Types.MsSmoothAudioDeduplication
import Amazonka.MediaConvert.Types.MsSmoothEncryptionSettings
import Amazonka.MediaConvert.Types.MsSmoothFragmentLengthControl
import Amazonka.MediaConvert.Types.MsSmoothGroupSettings
import Amazonka.MediaConvert.Types.MsSmoothManifestEncoding
import Amazonka.MediaConvert.Types.MxfAfdSignaling
import Amazonka.MediaConvert.Types.MxfProfile
import Amazonka.MediaConvert.Types.MxfSettings
import Amazonka.MediaConvert.Types.MxfXavcDurationMode
import Amazonka.MediaConvert.Types.MxfXavcProfileSettings
import Amazonka.MediaConvert.Types.NexGuardFileMarkerSettings
import Amazonka.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Amazonka.MediaConvert.Types.NielsenConfiguration
import Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Amazonka.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Amazonka.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpeningStrength
import Amazonka.MediaConvert.Types.NoiseReducer
import Amazonka.MediaConvert.Types.NoiseReducerFilter
import Amazonka.MediaConvert.Types.NoiseReducerFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import Amazonka.MediaConvert.Types.OpusSettings
import Amazonka.MediaConvert.Types.Order
import Amazonka.MediaConvert.Types.Output
import Amazonka.MediaConvert.Types.OutputChannelMapping
import Amazonka.MediaConvert.Types.OutputDetail
import Amazonka.MediaConvert.Types.OutputGroup
import Amazonka.MediaConvert.Types.OutputGroupDetail
import Amazonka.MediaConvert.Types.OutputGroupSettings
import Amazonka.MediaConvert.Types.OutputGroupType
import Amazonka.MediaConvert.Types.OutputSdt
import Amazonka.MediaConvert.Types.OutputSettings
import Amazonka.MediaConvert.Types.PadVideo
import Amazonka.MediaConvert.Types.PartnerWatermarking
import Amazonka.MediaConvert.Types.Policy
import Amazonka.MediaConvert.Types.Preset
import Amazonka.MediaConvert.Types.PresetListBy
import Amazonka.MediaConvert.Types.PresetSettings
import Amazonka.MediaConvert.Types.PricingPlan
import Amazonka.MediaConvert.Types.ProresChromaSampling
import Amazonka.MediaConvert.Types.ProresCodecProfile
import Amazonka.MediaConvert.Types.ProresFramerateControl
import Amazonka.MediaConvert.Types.ProresFramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.ProresInterlaceMode
import Amazonka.MediaConvert.Types.ProresParControl
import Amazonka.MediaConvert.Types.ProresScanTypeConversionMode
import Amazonka.MediaConvert.Types.ProresSettings
import Amazonka.MediaConvert.Types.ProresSlowPal
import Amazonka.MediaConvert.Types.ProresTelecine
import Amazonka.MediaConvert.Types.Queue
import Amazonka.MediaConvert.Types.QueueListBy
import Amazonka.MediaConvert.Types.QueueStatus
import Amazonka.MediaConvert.Types.QueueTransition
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.RemixSettings
import Amazonka.MediaConvert.Types.RenewalType
import Amazonka.MediaConvert.Types.RequiredFlag
import Amazonka.MediaConvert.Types.ReservationPlan
import Amazonka.MediaConvert.Types.ReservationPlanSettings
import Amazonka.MediaConvert.Types.ReservationPlanStatus
import Amazonka.MediaConvert.Types.ResourceTags
import Amazonka.MediaConvert.Types.RespondToAfd
import Amazonka.MediaConvert.Types.RuleType
import Amazonka.MediaConvert.Types.S3DestinationAccessControl
import Amazonka.MediaConvert.Types.S3DestinationSettings
import Amazonka.MediaConvert.Types.S3EncryptionSettings
import Amazonka.MediaConvert.Types.S3ObjectCannedAcl
import Amazonka.MediaConvert.Types.S3ServerSideEncryptionType
import Amazonka.MediaConvert.Types.SampleRangeConversion
import Amazonka.MediaConvert.Types.ScalingBehavior
import Amazonka.MediaConvert.Types.SccDestinationFramerate
import Amazonka.MediaConvert.Types.SccDestinationSettings
import Amazonka.MediaConvert.Types.SimulateReservedQueue
import Amazonka.MediaConvert.Types.SpekeKeyProvider
import Amazonka.MediaConvert.Types.SpekeKeyProviderCmaf
import Amazonka.MediaConvert.Types.SrtDestinationSettings
import Amazonka.MediaConvert.Types.SrtStylePassthrough
import Amazonka.MediaConvert.Types.StaticKeyProvider
import Amazonka.MediaConvert.Types.StatusUpdateInterval
import Amazonka.MediaConvert.Types.TeletextDestinationSettings
import Amazonka.MediaConvert.Types.TeletextPageType
import Amazonka.MediaConvert.Types.TeletextSourceSettings
import Amazonka.MediaConvert.Types.TimecodeBurnin
import Amazonka.MediaConvert.Types.TimecodeBurninPosition
import Amazonka.MediaConvert.Types.TimecodeConfig
import Amazonka.MediaConvert.Types.TimecodeSource
import Amazonka.MediaConvert.Types.TimedMetadata
import Amazonka.MediaConvert.Types.TimedMetadataInsertion
import Amazonka.MediaConvert.Types.Timing
import Amazonka.MediaConvert.Types.TrackSourceSettings
import Amazonka.MediaConvert.Types.TtmlDestinationSettings
import Amazonka.MediaConvert.Types.TtmlStylePassthrough
import Amazonka.MediaConvert.Types.Type
import Amazonka.MediaConvert.Types.Vc3Class
import Amazonka.MediaConvert.Types.Vc3FramerateControl
import Amazonka.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Vc3InterlaceMode
import Amazonka.MediaConvert.Types.Vc3ScanTypeConversionMode
import Amazonka.MediaConvert.Types.Vc3Settings
import Amazonka.MediaConvert.Types.Vc3SlowPal
import Amazonka.MediaConvert.Types.Vc3Telecine
import Amazonka.MediaConvert.Types.VchipAction
import Amazonka.MediaConvert.Types.VideoCodec
import Amazonka.MediaConvert.Types.VideoCodecSettings
import Amazonka.MediaConvert.Types.VideoDescription
import Amazonka.MediaConvert.Types.VideoDetail
import Amazonka.MediaConvert.Types.VideoPreprocessor
import Amazonka.MediaConvert.Types.VideoSelector
import Amazonka.MediaConvert.Types.VideoTimecodeInsertion
import Amazonka.MediaConvert.Types.VorbisSettings
import Amazonka.MediaConvert.Types.Vp8FramerateControl
import Amazonka.MediaConvert.Types.Vp8FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Vp8ParControl
import Amazonka.MediaConvert.Types.Vp8QualityTuningLevel
import Amazonka.MediaConvert.Types.Vp8RateControlMode
import Amazonka.MediaConvert.Types.Vp8Settings
import Amazonka.MediaConvert.Types.Vp9FramerateControl
import Amazonka.MediaConvert.Types.Vp9FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Vp9ParControl
import Amazonka.MediaConvert.Types.Vp9QualityTuningLevel
import Amazonka.MediaConvert.Types.Vp9RateControlMode
import Amazonka.MediaConvert.Types.Vp9Settings
import Amazonka.MediaConvert.Types.WarningGroup
import Amazonka.MediaConvert.Types.WatermarkingStrength
import Amazonka.MediaConvert.Types.WavFormat
import Amazonka.MediaConvert.Types.WavSettings
import Amazonka.MediaConvert.Types.WebvttAccessibilitySubs
import Amazonka.MediaConvert.Types.WebvttDestinationSettings
import Amazonka.MediaConvert.Types.WebvttHlsSourceSettings
import Amazonka.MediaConvert.Types.WebvttStylePassthrough
import Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileClass
import Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileClass
import Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kProfileBitrateClass
import Amazonka.MediaConvert.Types.Xavc4kProfileCodecProfile
import Amazonka.MediaConvert.Types.Xavc4kProfileQualityTuningLevel
import Amazonka.MediaConvert.Types.Xavc4kProfileSettings
import Amazonka.MediaConvert.Types.XavcAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcEntropyEncoding
import Amazonka.MediaConvert.Types.XavcFlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcFramerateControl
import Amazonka.MediaConvert.Types.XavcFramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.XavcGopBReference
import Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileClass
import Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.XavcHdProfileBitrateClass
import Amazonka.MediaConvert.Types.XavcHdProfileQualityTuningLevel
import Amazonka.MediaConvert.Types.XavcHdProfileSettings
import Amazonka.MediaConvert.Types.XavcHdProfileTelecine
import Amazonka.MediaConvert.Types.XavcInterlaceMode
import Amazonka.MediaConvert.Types.XavcProfile
import Amazonka.MediaConvert.Types.XavcSettings
import Amazonka.MediaConvert.Types.XavcSlowPal
import Amazonka.MediaConvert.Types.XavcSpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcTemporalAdaptiveQuantization
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-08-29@ of the Amazon Elemental MediaConvert SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaConvert",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mediaconvert",
      Core.signingName = "mediaconvert",
      Core.version = "2017-08-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaConvert",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The service can\'t process your request because of a problem in the
-- request. Please check your request form and syntax.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The service couldn\'t complete your request because there is a conflict
-- with the current state of the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | You don\'t have permissions for this action with the credentials you
-- sent.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The service encountered an unexpected condition and can\'t fulfill your
-- request.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | The resource you requested doesn\'t exist.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Too many requests have been sent in too short of a time. The service
-- limits the rate at which it will accept requests.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
