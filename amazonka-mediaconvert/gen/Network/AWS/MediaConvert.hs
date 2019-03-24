{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elemental MediaConvert
module Network.AWS.MediaConvert
    (
    -- * Service Configuration
      mediaConvert

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** ForbiddenException
    , _ForbiddenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeletePreset
    , module Network.AWS.MediaConvert.DeletePreset

    -- ** UpdatePreset
    , module Network.AWS.MediaConvert.UpdatePreset

    -- ** ListTagsForResource
    , module Network.AWS.MediaConvert.ListTagsForResource

    -- ** ListQueues (Paginated)
    , module Network.AWS.MediaConvert.ListQueues

    -- ** DeleteQueue
    , module Network.AWS.MediaConvert.DeleteQueue

    -- ** UpdateQueue
    , module Network.AWS.MediaConvert.UpdateQueue

    -- ** GetPreset
    , module Network.AWS.MediaConvert.GetPreset

    -- ** CreateJob
    , module Network.AWS.MediaConvert.CreateJob

    -- ** ListJobs (Paginated)
    , module Network.AWS.MediaConvert.ListJobs

    -- ** GetJob
    , module Network.AWS.MediaConvert.GetJob

    -- ** CreatePreset
    , module Network.AWS.MediaConvert.CreatePreset

    -- ** ListPresets (Paginated)
    , module Network.AWS.MediaConvert.ListPresets

    -- ** DisassociateCertificate
    , module Network.AWS.MediaConvert.DisassociateCertificate

    -- ** GetQueue
    , module Network.AWS.MediaConvert.GetQueue

    -- ** DescribeEndpoints (Paginated)
    , module Network.AWS.MediaConvert.DescribeEndpoints

    -- ** CreateQueue
    , module Network.AWS.MediaConvert.CreateQueue

    -- ** TagResource
    , module Network.AWS.MediaConvert.TagResource

    -- ** CreateJobTemplate
    , module Network.AWS.MediaConvert.CreateJobTemplate

    -- ** UntagResource
    , module Network.AWS.MediaConvert.UntagResource

    -- ** DeleteJobTemplate
    , module Network.AWS.MediaConvert.DeleteJobTemplate

    -- ** UpdateJobTemplate
    , module Network.AWS.MediaConvert.UpdateJobTemplate

    -- ** ListJobTemplates (Paginated)
    , module Network.AWS.MediaConvert.ListJobTemplates

    -- ** GetJobTemplate
    , module Network.AWS.MediaConvert.GetJobTemplate

    -- ** AssociateCertificate
    , module Network.AWS.MediaConvert.AssociateCertificate

    -- ** CancelJob
    , module Network.AWS.MediaConvert.CancelJob

    -- * Types

    -- ** AacAudioDescriptionBroadcasterMix
    , AacAudioDescriptionBroadcasterMix (..)

    -- ** AacCodecProfile
    , AacCodecProfile (..)

    -- ** AacCodingMode
    , AacCodingMode (..)

    -- ** AacRateControlMode
    , AacRateControlMode (..)

    -- ** AacRawFormat
    , AacRawFormat (..)

    -- ** AacSpecification
    , AacSpecification (..)

    -- ** AacVbrQuality
    , AacVbrQuality (..)

    -- ** Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- ** Ac3CodingMode
    , Ac3CodingMode (..)

    -- ** Ac3DynamicRangeCompressionProfile
    , Ac3DynamicRangeCompressionProfile (..)

    -- ** Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- ** Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- ** AccelerationMode
    , AccelerationMode (..)

    -- ** AfdSignaling
    , AfdSignaling (..)

    -- ** AntiAlias
    , AntiAlias (..)

    -- ** AudioCodec
    , AudioCodec (..)

    -- ** AudioDefaultSelection
    , AudioDefaultSelection (..)

    -- ** AudioLanguageCodeControl
    , AudioLanguageCodeControl (..)

    -- ** AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- ** AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- ** AudioNormalizationLoudnessLogging
    , AudioNormalizationLoudnessLogging (..)

    -- ** AudioNormalizationPeakCalculation
    , AudioNormalizationPeakCalculation (..)

    -- ** AudioSelectorType
    , AudioSelectorType (..)

    -- ** AudioTypeControl
    , AudioTypeControl (..)

    -- ** BillingTagsSource
    , BillingTagsSource (..)

    -- ** BurninSubtitleAlignment
    , BurninSubtitleAlignment (..)

    -- ** BurninSubtitleBackgroundColor
    , BurninSubtitleBackgroundColor (..)

    -- ** BurninSubtitleFontColor
    , BurninSubtitleFontColor (..)

    -- ** BurninSubtitleOutlineColor
    , BurninSubtitleOutlineColor (..)

    -- ** BurninSubtitleShadowColor
    , BurninSubtitleShadowColor (..)

    -- ** BurninSubtitleTeletextSpacing
    , BurninSubtitleTeletextSpacing (..)

    -- ** CaptionDestinationType
    , CaptionDestinationType (..)

    -- ** CaptionSourceType
    , CaptionSourceType (..)

    -- ** CmafClientCache
    , CmafClientCache (..)

    -- ** CmafCodecSpecification
    , CmafCodecSpecification (..)

    -- ** CmafEncryptionType
    , CmafEncryptionType (..)

    -- ** CmafInitializationVectorInManifest
    , CmafInitializationVectorInManifest (..)

    -- ** CmafKeyProviderType
    , CmafKeyProviderType (..)

    -- ** CmafManifestCompression
    , CmafManifestCompression (..)

    -- ** CmafManifestDurationFormat
    , CmafManifestDurationFormat (..)

    -- ** CmafSegmentControl
    , CmafSegmentControl (..)

    -- ** CmafStreamInfResolution
    , CmafStreamInfResolution (..)

    -- ** CmafWriteDASHManifest
    , CmafWriteDASHManifest (..)

    -- ** CmafWriteHLSManifest
    , CmafWriteHLSManifest (..)

    -- ** ColorMetadata
    , ColorMetadata (..)

    -- ** ColorSpace
    , ColorSpace (..)

    -- ** ColorSpaceConversion
    , ColorSpaceConversion (..)

    -- ** ColorSpaceUsage
    , ColorSpaceUsage (..)

    -- ** Commitment
    , Commitment (..)

    -- ** ContainerType
    , ContainerType (..)

    -- ** DashIsoHbbtvCompliance
    , DashIsoHbbtvCompliance (..)

    -- ** DashIsoSegmentControl
    , DashIsoSegmentControl (..)

    -- ** DashIsoWriteSegmentTimelineInRepresentation
    , DashIsoWriteSegmentTimelineInRepresentation (..)

    -- ** DecryptionMode
    , DecryptionMode (..)

    -- ** DeinterlaceAlgorithm
    , DeinterlaceAlgorithm (..)

    -- ** DeinterlacerControl
    , DeinterlacerControl (..)

    -- ** DeinterlacerMode
    , DeinterlacerMode (..)

    -- ** DescribeEndpointsMode
    , DescribeEndpointsMode (..)

    -- ** DropFrameTimecode
    , DropFrameTimecode (..)

    -- ** DvbSubtitleAlignment
    , DvbSubtitleAlignment (..)

    -- ** DvbSubtitleBackgroundColor
    , DvbSubtitleBackgroundColor (..)

    -- ** DvbSubtitleFontColor
    , DvbSubtitleFontColor (..)

    -- ** DvbSubtitleOutlineColor
    , DvbSubtitleOutlineColor (..)

    -- ** DvbSubtitleShadowColor
    , DvbSubtitleShadowColor (..)

    -- ** DvbSubtitleTeletextSpacing
    , DvbSubtitleTeletextSpacing (..)

    -- ** Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- ** Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- ** Eac3CodingMode
    , Eac3CodingMode (..)

    -- ** Eac3DcFilter
    , Eac3DcFilter (..)

    -- ** Eac3DynamicRangeCompressionLine
    , Eac3DynamicRangeCompressionLine (..)

    -- ** Eac3DynamicRangeCompressionRf
    , Eac3DynamicRangeCompressionRf (..)

    -- ** Eac3LfeControl
    , Eac3LfeControl (..)

    -- ** Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- ** Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- ** Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- ** Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- ** Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- ** Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- ** Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- ** EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- ** F4vMoovPlacement
    , F4vMoovPlacement (..)

    -- ** FileSourceConvert608To708
    , FileSourceConvert608To708 (..)

    -- ** FontScript
    , FontScript (..)

    -- ** H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- ** H264CodecLevel
    , H264CodecLevel (..)

    -- ** H264CodecProfile
    , H264CodecProfile (..)

    -- ** H264DynamicSubGop
    , H264DynamicSubGop (..)

    -- ** H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- ** H264FieldEncoding
    , H264FieldEncoding (..)

    -- ** H264FlickerAdaptiveQuantization
    , H264FlickerAdaptiveQuantization (..)

    -- ** H264FramerateControl
    , H264FramerateControl (..)

    -- ** H264FramerateConversionAlgorithm
    , H264FramerateConversionAlgorithm (..)

    -- ** H264GopBReference
    , H264GopBReference (..)

    -- ** H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- ** H264InterlaceMode
    , H264InterlaceMode (..)

    -- ** H264ParControl
    , H264ParControl (..)

    -- ** H264QualityTuningLevel
    , H264QualityTuningLevel (..)

    -- ** H264RateControlMode
    , H264RateControlMode (..)

    -- ** H264RepeatPps
    , H264RepeatPps (..)

    -- ** H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- ** H264SlowPal
    , H264SlowPal (..)

    -- ** H264SpatialAdaptiveQuantization
    , H264SpatialAdaptiveQuantization (..)

    -- ** H264Syntax
    , H264Syntax (..)

    -- ** H264Telecine
    , H264Telecine (..)

    -- ** H264TemporalAdaptiveQuantization
    , H264TemporalAdaptiveQuantization (..)

    -- ** H264UnregisteredSeiTimecode
    , H264UnregisteredSeiTimecode (..)

    -- ** H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- ** H265AlternateTransferFunctionSei
    , H265AlternateTransferFunctionSei (..)

    -- ** H265CodecLevel
    , H265CodecLevel (..)

    -- ** H265CodecProfile
    , H265CodecProfile (..)

    -- ** H265DynamicSubGop
    , H265DynamicSubGop (..)

    -- ** H265FlickerAdaptiveQuantization
    , H265FlickerAdaptiveQuantization (..)

    -- ** H265FramerateControl
    , H265FramerateControl (..)

    -- ** H265FramerateConversionAlgorithm
    , H265FramerateConversionAlgorithm (..)

    -- ** H265GopBReference
    , H265GopBReference (..)

    -- ** H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- ** H265InterlaceMode
    , H265InterlaceMode (..)

    -- ** H265ParControl
    , H265ParControl (..)

    -- ** H265QualityTuningLevel
    , H265QualityTuningLevel (..)

    -- ** H265RateControlMode
    , H265RateControlMode (..)

    -- ** H265SampleAdaptiveOffsetFilterMode
    , H265SampleAdaptiveOffsetFilterMode (..)

    -- ** H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- ** H265SlowPal
    , H265SlowPal (..)

    -- ** H265SpatialAdaptiveQuantization
    , H265SpatialAdaptiveQuantization (..)

    -- ** H265Telecine
    , H265Telecine (..)

    -- ** H265TemporalAdaptiveQuantization
    , H265TemporalAdaptiveQuantization (..)

    -- ** H265TemporalIds
    , H265TemporalIds (..)

    -- ** H265Tiles
    , H265Tiles (..)

    -- ** H265UnregisteredSeiTimecode
    , H265UnregisteredSeiTimecode (..)

    -- ** H265WriteMp4PackagingType
    , H265WriteMp4PackagingType (..)

    -- ** HlsAdMarkers
    , HlsAdMarkers (..)

    -- ** HlsAudioTrackType
    , HlsAudioTrackType (..)

    -- ** HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- ** HlsClientCache
    , HlsClientCache (..)

    -- ** HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- ** HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- ** HlsEncryptionType
    , HlsEncryptionType (..)

    -- ** HlsIFrameOnlyManifest
    , HlsIFrameOnlyManifest (..)

    -- ** HlsInitializationVectorInManifest
    , HlsInitializationVectorInManifest (..)

    -- ** HlsKeyProviderType
    , HlsKeyProviderType (..)

    -- ** HlsManifestCompression
    , HlsManifestCompression (..)

    -- ** HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- ** HlsOfflineEncrypted
    , HlsOfflineEncrypted (..)

    -- ** HlsOutputSelection
    , HlsOutputSelection (..)

    -- ** HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- ** HlsSegmentControl
    , HlsSegmentControl (..)

    -- ** HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- ** HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- ** InputDeblockFilter
    , InputDeblockFilter (..)

    -- ** InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- ** InputFilterEnable
    , InputFilterEnable (..)

    -- ** InputPsiControl
    , InputPsiControl (..)

    -- ** InputRotate
    , InputRotate (..)

    -- ** InputTimecodeSource
    , InputTimecodeSource (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** JobTemplateListBy
    , JobTemplateListBy (..)

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- ** M2tsBufferModel
    , M2tsBufferModel (..)

    -- ** M2tsEbpAudioInterval
    , M2tsEbpAudioInterval (..)

    -- ** M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- ** M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- ** M2tsForceTsVideoEbpOrder
    , M2tsForceTsVideoEbpOrder (..)

    -- ** M2tsNielsenId3
    , M2tsNielsenId3 (..)

    -- ** M2tsPcrControl
    , M2tsPcrControl (..)

    -- ** M2tsRateMode
    , M2tsRateMode (..)

    -- ** M2tsScte35Source
    , M2tsScte35Source (..)

    -- ** M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- ** M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- ** M3u8NielsenId3
    , M3u8NielsenId3 (..)

    -- ** M3u8PcrControl
    , M3u8PcrControl (..)

    -- ** M3u8Scte35Source
    , M3u8Scte35Source (..)

    -- ** MotionImageInsertionMode
    , MotionImageInsertionMode (..)

    -- ** MotionImagePlayback
    , MotionImagePlayback (..)

    -- ** MovClapAtom
    , MovClapAtom (..)

    -- ** MovCslgAtom
    , MovCslgAtom (..)

    -- ** MovMpeg2FourCCControl
    , MovMpeg2FourCCControl (..)

    -- ** MovPaddingControl
    , MovPaddingControl (..)

    -- ** MovReference
    , MovReference (..)

    -- ** Mp4CslgAtom
    , Mp4CslgAtom (..)

    -- ** Mp4FreeSpaceBox
    , Mp4FreeSpaceBox (..)

    -- ** Mp4MoovPlacement
    , Mp4MoovPlacement (..)

    -- ** Mpeg2AdaptiveQuantization
    , Mpeg2AdaptiveQuantization (..)

    -- ** Mpeg2CodecLevel
    , Mpeg2CodecLevel (..)

    -- ** Mpeg2CodecProfile
    , Mpeg2CodecProfile (..)

    -- ** Mpeg2DynamicSubGop
    , Mpeg2DynamicSubGop (..)

    -- ** Mpeg2FramerateControl
    , Mpeg2FramerateControl (..)

    -- ** Mpeg2FramerateConversionAlgorithm
    , Mpeg2FramerateConversionAlgorithm (..)

    -- ** Mpeg2GopSizeUnits
    , Mpeg2GopSizeUnits (..)

    -- ** Mpeg2InterlaceMode
    , Mpeg2InterlaceMode (..)

    -- ** Mpeg2IntraDcPrecision
    , Mpeg2IntraDcPrecision (..)

    -- ** Mpeg2ParControl
    , Mpeg2ParControl (..)

    -- ** Mpeg2QualityTuningLevel
    , Mpeg2QualityTuningLevel (..)

    -- ** Mpeg2RateControlMode
    , Mpeg2RateControlMode (..)

    -- ** Mpeg2SceneChangeDetect
    , Mpeg2SceneChangeDetect (..)

    -- ** Mpeg2SlowPal
    , Mpeg2SlowPal (..)

    -- ** Mpeg2SpatialAdaptiveQuantization
    , Mpeg2SpatialAdaptiveQuantization (..)

    -- ** Mpeg2Syntax
    , Mpeg2Syntax (..)

    -- ** Mpeg2Telecine
    , Mpeg2Telecine (..)

    -- ** Mpeg2TemporalAdaptiveQuantization
    , Mpeg2TemporalAdaptiveQuantization (..)

    -- ** MsSmoothAudioDeduplication
    , MsSmoothAudioDeduplication (..)

    -- ** MsSmoothManifestEncoding
    , MsSmoothManifestEncoding (..)

    -- ** NoiseReducerFilter
    , NoiseReducerFilter (..)

    -- ** Order
    , Order (..)

    -- ** OutputGroupType
    , OutputGroupType (..)

    -- ** OutputSdt
    , OutputSdt (..)

    -- ** PresetListBy
    , PresetListBy (..)

    -- ** PricingPlan
    , PricingPlan (..)

    -- ** ProresCodecProfile
    , ProresCodecProfile (..)

    -- ** ProresFramerateControl
    , ProresFramerateControl (..)

    -- ** ProresFramerateConversionAlgorithm
    , ProresFramerateConversionAlgorithm (..)

    -- ** ProresInterlaceMode
    , ProresInterlaceMode (..)

    -- ** ProresParControl
    , ProresParControl (..)

    -- ** ProresSlowPal
    , ProresSlowPal (..)

    -- ** ProresTelecine
    , ProresTelecine (..)

    -- ** QueueListBy
    , QueueListBy (..)

    -- ** QueueStatus
    , QueueStatus (..)

    -- ** RenewalType
    , RenewalType (..)

    -- ** ReservationPlanStatus
    , ReservationPlanStatus (..)

    -- ** RespondToAfd
    , RespondToAfd (..)

    -- ** ScalingBehavior
    , ScalingBehavior (..)

    -- ** SccDestinationFramerate
    , SccDestinationFramerate (..)

    -- ** TimecodeBurninPosition
    , TimecodeBurninPosition (..)

    -- ** TimecodeSource
    , TimecodeSource (..)

    -- ** TimedMetadata
    , TimedMetadata (..)

    -- ** TtmlStylePassthrough
    , TtmlStylePassthrough (..)

    -- ** Type
    , Type (..)

    -- ** VideoCodec
    , VideoCodec (..)

    -- ** VideoTimecodeInsertion
    , VideoTimecodeInsertion (..)

    -- ** WavFormat
    , WavFormat (..)

    -- ** AacSettings
    , AacSettings
    , aacSettings
    , assAudioDescriptionBroadcasterMix
    , assRawFormat
    , assCodingMode
    , assRateControlMode
    , assSampleRate
    , assSpecification
    , assCodecProfile
    , assBitrate
    , assVbrQuality

    -- ** Ac3Settings
    , Ac3Settings
    , ac3Settings
    , aLfeFilter
    , aMetadataControl
    , aBitstreamMode
    , aCodingMode
    , aSampleRate
    , aDynamicRangeCompressionProfile
    , aBitrate
    , aDialnorm

    -- ** AccelerationSettings
    , AccelerationSettings
    , accelerationSettings
    , asMode

    -- ** AiffSettings
    , AiffSettings
    , aiffSettings
    , asBitDepth
    , asChannels
    , asSampleRate

    -- ** AncillarySourceSettings
    , AncillarySourceSettings
    , ancillarySourceSettings
    , assSourceAncillaryChannelNumber

    -- ** AudioCodecSettings
    , AudioCodecSettings
    , audioCodecSettings
    , acsAiffSettings
    , acsCodec
    , acsAc3Settings
    , acsMp2Settings
    , acsWavSettings
    , acsAacSettings
    , acsEac3Settings

    -- ** AudioDescription
    , AudioDescription
    , audioDescription
    , adAudioSourceName
    , adCustomLanguageCode
    , adLanguageCode
    , adAudioType
    , adAudioNormalizationSettings
    , adLanguageCodeControl
    , adCodecSettings
    , adStreamName
    , adRemixSettings
    , adAudioTypeControl

    -- ** AudioNormalizationSettings
    , AudioNormalizationSettings
    , audioNormalizationSettings
    , ansAlgorithmControl
    , ansTargetLkfs
    , ansPeakCalculation
    , ansCorrectionGateLevel
    , ansAlgorithm
    , ansLoudnessLogging

    -- ** AudioSelector
    , AudioSelector
    , audioSelector
    , asTracks
    , asCustomLanguageCode
    , asProgramSelection
    , asLanguageCode
    , asOffset
    , asDefaultSelection
    , asPids
    , asSelectorType
    , asExternalAudioFileInput
    , asRemixSettings

    -- ** AudioSelectorGroup
    , AudioSelectorGroup
    , audioSelectorGroup
    , asgAudioSelectorNames

    -- ** AvailBlanking
    , AvailBlanking
    , availBlanking
    , abAvailBlankingImage

    -- ** BurninDestinationSettings
    , BurninDestinationSettings
    , burninDestinationSettings
    , bdsBackgroundOpacity
    , bdsFontOpacity
    , bdsShadowYOffset
    , bdsFontResolution
    , bdsYPosition
    , bdsBackgroundColor
    , bdsShadowXOffset
    , bdsFontSize
    , bdsXPosition
    , bdsTeletextSpacing
    , bdsFontScript
    , bdsAlignment
    , bdsShadowOpacity
    , bdsOutlineColor
    , bdsOutlineSize
    , bdsShadowColor
    , bdsFontColor

    -- ** CaptionDescription
    , CaptionDescription
    , captionDescription
    , cdCaptionSelectorName
    , cdCustomLanguageCode
    , cdLanguageCode
    , cdDestinationSettings
    , cdLanguageDescription

    -- ** CaptionDescriptionPreset
    , CaptionDescriptionPreset
    , captionDescriptionPreset
    , cdpCustomLanguageCode
    , cdpLanguageCode
    , cdpDestinationSettings
    , cdpLanguageDescription

    -- ** CaptionDestinationSettings
    , CaptionDestinationSettings
    , captionDestinationSettings
    , cdsTeletextDestinationSettings
    , cdsDvbSubDestinationSettings
    , cdsTtmlDestinationSettings
    , cdsDestinationType
    , cdsEmbeddedDestinationSettings
    , cdsSccDestinationSettings
    , cdsBurninDestinationSettings

    -- ** CaptionSelector
    , CaptionSelector
    , captionSelector
    , csCustomLanguageCode
    , csLanguageCode
    , csSourceSettings

    -- ** CaptionSourceSettings
    , CaptionSourceSettings
    , captionSourceSettings
    , cssTeletextSourceSettings
    , cssSourceType
    , cssFileSourceSettings
    , cssDvbSubSourceSettings
    , cssTrackSourceSettings
    , cssAncillarySourceSettings
    , cssEmbeddedSourceSettings

    -- ** ChannelMapping
    , ChannelMapping
    , channelMapping
    , cmOutputChannels

    -- ** CmafEncryptionSettings
    , CmafEncryptionSettings
    , cmafEncryptionSettings
    , cesEncryptionMethod
    , cesConstantInitializationVector
    , cesType
    , cesStaticKeyProvider
    , cesInitializationVectorInManifest

    -- ** CmafGroupSettings
    , CmafGroupSettings
    , cmafGroupSettings
    , cgsFragmentLength
    , cgsSegmentControl
    , cgsDestination
    , cgsMinBufferTime
    , cgsWriteHlsManifest
    , cgsCodecSpecification
    , cgsBaseURL
    , cgsMinFinalSegmentLength
    , cgsWriteDashManifest
    , cgsEncryption
    , cgsSegmentLength
    , cgsManifestDurationFormat
    , cgsClientCache
    , cgsStreamInfResolution
    , cgsManifestCompression

    -- ** ColorCorrector
    , ColorCorrector
    , colorCorrector
    , ccSaturation
    , ccHue
    , ccColorSpaceConversion
    , ccHdr10Metadata
    , ccContrast
    , ccBrightness

    -- ** ContainerSettings
    , ContainerSettings
    , containerSettings
    , csM2tsSettings
    , csM3u8Settings
    , csMovSettings
    , csMp4Settings
    , csContainer
    , csF4vSettings

    -- ** DashIsoEncryptionSettings
    , DashIsoEncryptionSettings
    , dashIsoEncryptionSettings
    , diesSpekeKeyProvider

    -- ** DashIsoGroupSettings
    , DashIsoGroupSettings
    , dashIsoGroupSettings
    , digsFragmentLength
    , digsSegmentControl
    , digsDestination
    , digsHbbtvCompliance
    , digsMinBufferTime
    , digsBaseURL
    , digsEncryption
    , digsSegmentLength
    , digsWriteSegmentTimelineInRepresentation

    -- ** Deinterlacer
    , Deinterlacer
    , deinterlacer
    , dControl
    , dMode
    , dAlgorithm

    -- ** DvbNitSettings
    , DvbNitSettings
    , dvbNitSettings
    , dnsNetworkId
    , dnsNetworkName
    , dnsNitInterval

    -- ** DvbSdtSettings
    , DvbSdtSettings
    , dvbSdtSettings
    , dssSdtInterval
    , dssServiceProviderName
    , dssOutputSdt
    , dssServiceName

    -- ** DvbSubDestinationSettings
    , DvbSubDestinationSettings
    , dvbSubDestinationSettings
    , dsdsBackgroundOpacity
    , dsdsFontOpacity
    , dsdsShadowYOffset
    , dsdsFontResolution
    , dsdsYPosition
    , dsdsBackgroundColor
    , dsdsShadowXOffset
    , dsdsFontSize
    , dsdsXPosition
    , dsdsTeletextSpacing
    , dsdsFontScript
    , dsdsAlignment
    , dsdsShadowOpacity
    , dsdsOutlineColor
    , dsdsOutlineSize
    , dsdsShadowColor
    , dsdsFontColor

    -- ** DvbSubSourceSettings
    , DvbSubSourceSettings
    , dvbSubSourceSettings
    , dsssPid

    -- ** DvbTdtSettings
    , DvbTdtSettings
    , dvbTdtSettings
    , dtsTdtInterval

    -- ** Eac3Settings
    , Eac3Settings
    , eac3Settings
    , esStereoDownmix
    , esLoRoCenterMixLevel
    , esLtRtCenterMixLevel
    , esLfeFilter
    , esDynamicRangeCompressionLine
    , esLtRtSurroundMixLevel
    , esMetadataControl
    , esLoRoSurroundMixLevel
    , esSurroundMode
    , esAttenuationControl
    , esPassthroughControl
    , esBitstreamMode
    , esLfeControl
    , esDynamicRangeCompressionRf
    , esCodingMode
    , esSampleRate
    , esDcFilter
    , esBitrate
    , esPhaseControl
    , esSurroundExMode
    , esDialnorm

    -- ** EmbeddedDestinationSettings
    , EmbeddedDestinationSettings
    , embeddedDestinationSettings
    , edsDestination608ChannelNumber

    -- ** EmbeddedSourceSettings
    , EmbeddedSourceSettings
    , embeddedSourceSettings
    , essConvert608To708
    , essSource608TrackNumber
    , essSource608ChannelNumber

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eURL

    -- ** EsamManifestConfirmConditionNotification
    , EsamManifestConfirmConditionNotification
    , esamManifestConfirmConditionNotification
    , emccnMccXML

    -- ** EsamSettings
    , EsamSettings
    , esamSettings
    , esManifestConfirmConditionNotification
    , esResponseSignalPreroll
    , esSignalProcessingNotification

    -- ** EsamSignalProcessingNotification
    , EsamSignalProcessingNotification
    , esamSignalProcessingNotification
    , espnSccXML

    -- ** F4vSettings
    , F4vSettings
    , f4vSettings
    , fsMoovPlacement

    -- ** FileGroupSettings
    , FileGroupSettings
    , fileGroupSettings
    , fgsDestination

    -- ** FileSourceSettings
    , FileSourceSettings
    , fileSourceSettings
    , fssConvert608To708
    , fssTimeDelta
    , fssSourceFile

    -- ** FrameCaptureSettings
    , FrameCaptureSettings
    , frameCaptureSettings
    , fcsQuality
    , fcsFramerateDenominator
    , fcsMaxCaptures
    , fcsFramerateNumerator

    -- ** H264QvbrSettings
    , H264QvbrSettings
    , h264QvbrSettings
    , hMaxAverageBitrate
    , hQvbrQualityLevel

    -- ** H264Settings
    , H264Settings
    , h264Settings
    , hssUnregisteredSeiTimecode
    , hssQualityTuningLevel
    , hssTemporalAdaptiveQuantization
    , hssSceneChangeDetect
    , hssHrdBufferInitialFillPercentage
    , hssSlowPal
    , hssParNumerator
    , hssGopSize
    , hssNumberBFramesBetweenReferenceFrames
    , hssGopSizeUnits
    , hssHrdBufferSize
    , hssSlices
    , hssRateControlMode
    , hssNumberReferenceFrames
    , hssTelecine
    , hssDynamicSubGop
    , hssMinIInterval
    , hssInterlaceMode
    , hssParControl
    , hssRepeatPps
    , hssFlickerAdaptiveQuantization
    , hssQvbrSettings
    , hssSoftness
    , hssCodecProfile
    , hssBitrate
    , hssFramerateDenominator
    , hssFramerateConversionAlgorithm
    , hssCodecLevel
    , hssEntropyEncoding
    , hssFramerateControl
    , hssAdaptiveQuantization
    , hssFramerateNumerator
    , hssGopBReference
    , hssMaxBitrate
    , hssSyntax
    , hssFieldEncoding
    , hssGopClosedCadence
    , hssParDenominator
    , hssSpatialAdaptiveQuantization

    -- ** H265QvbrSettings
    , H265QvbrSettings
    , h265QvbrSettings
    , hqsMaxAverageBitrate
    , hqsQvbrQualityLevel

    -- ** H265Settings
    , H265Settings
    , h265Settings
    , hsUnregisteredSeiTimecode
    , hsQualityTuningLevel
    , hsTemporalAdaptiveQuantization
    , hsSceneChangeDetect
    , hsHrdBufferInitialFillPercentage
    , hsTiles
    , hsSlowPal
    , hsTemporalIds
    , hsParNumerator
    , hsGopSize
    , hsNumberBFramesBetweenReferenceFrames
    , hsGopSizeUnits
    , hsHrdBufferSize
    , hsSlices
    , hsAlternateTransferFunctionSei
    , hsRateControlMode
    , hsNumberReferenceFrames
    , hsTelecine
    , hsDynamicSubGop
    , hsMinIInterval
    , hsInterlaceMode
    , hsParControl
    , hsFlickerAdaptiveQuantization
    , hsQvbrSettings
    , hsSampleAdaptiveOffsetFilterMode
    , hsCodecProfile
    , hsBitrate
    , hsFramerateDenominator
    , hsFramerateConversionAlgorithm
    , hsCodecLevel
    , hsFramerateControl
    , hsWriteMp4PackagingType
    , hsAdaptiveQuantization
    , hsFramerateNumerator
    , hsGopBReference
    , hsMaxBitrate
    , hsGopClosedCadence
    , hsParDenominator
    , hsSpatialAdaptiveQuantization

    -- ** Hdr10Metadata
    , Hdr10Metadata
    , hdr10Metadata
    , hmRedPrimaryX
    , hmBluePrimaryX
    , hmMaxFrameAverageLightLevel
    , hmWhitePointY
    , hmMaxContentLightLevel
    , hmWhitePointX
    , hmBluePrimaryY
    , hmGreenPrimaryY
    , hmGreenPrimaryX
    , hmMinLuminance
    , hmRedPrimaryY
    , hmMaxLuminance

    -- ** HlsCaptionLanguageMapping
    , HlsCaptionLanguageMapping
    , hlsCaptionLanguageMapping
    , hclmCustomLanguageCode
    , hclmLanguageCode
    , hclmLanguageDescription
    , hclmCaptionChannel

    -- ** HlsEncryptionSettings
    , HlsEncryptionSettings
    , hlsEncryptionSettings
    , hesOfflineEncrypted
    , hesEncryptionMethod
    , hesConstantInitializationVector
    , hesType
    , hesStaticKeyProvider
    , hesSpekeKeyProvider
    , hesInitializationVectorInManifest

    -- ** HlsGroupSettings
    , HlsGroupSettings
    , hlsGroupSettings
    , hgsDirectoryStructure
    , hgsSegmentControl
    , hgsDestination
    , hgsTimedMetadataId3Period
    , hgsMinSegmentLength
    , hgsProgramDateTime
    , hgsProgramDateTimePeriod
    , hgsCodecSpecification
    , hgsCaptionLanguageMappings
    , hgsBaseURL
    , hgsMinFinalSegmentLength
    , hgsAdMarkers
    , hgsEncryption
    , hgsSegmentLength
    , hgsTimedMetadataId3Frame
    , hgsOutputSelection
    , hgsCaptionLanguageSetting
    , hgsSegmentsPerSubdirectory
    , hgsManifestDurationFormat
    , hgsClientCache
    , hgsTimestampDeltaMilliseconds
    , hgsStreamInfResolution
    , hgsManifestCompression

    -- ** HlsSettings
    , HlsSettings
    , hlsSettings
    , hsAudioRenditionSets
    , hsIFrameOnlyManifest
    , hsAudioGroupId
    , hsSegmentModifier
    , hsAudioTrackType

    -- ** Id3Insertion
    , Id3Insertion
    , id3Insertion
    , iiId3
    , iiTimecode

    -- ** ImageInserter
    , ImageInserter
    , imageInserter
    , iiInsertableImages

    -- ** Input
    , Input
    , input
    , iVideoSelector
    , iSupplementalImps
    , iProgramNumber
    , iAudioSelectorGroups
    , iTimecodeSource
    , iAudioSelectors
    , iDecryptionSettings
    , iDeblockFilter
    , iInputClippings
    , iDenoiseFilter
    , iImageInserter
    , iFilterStrength
    , iPsiControl
    , iCaptionSelectors
    , iFileInput
    , iFilterEnable

    -- ** InputClipping
    , InputClipping
    , inputClipping
    , icEndTimecode
    , icStartTimecode

    -- ** InputDecryptionSettings
    , InputDecryptionSettings
    , inputDecryptionSettings
    , idsEncryptedDecryptionKey
    , idsKMSKeyRegion
    , idsDecryptionMode
    , idsInitializationVector

    -- ** InputTemplate
    , InputTemplate
    , inputTemplate
    , itVideoSelector
    , itProgramNumber
    , itAudioSelectorGroups
    , itTimecodeSource
    , itAudioSelectors
    , itDeblockFilter
    , itInputClippings
    , itDenoiseFilter
    , itImageInserter
    , itFilterStrength
    , itPsiControl
    , itCaptionSelectors
    , itFilterEnable

    -- ** InsertableImage
    , InsertableImage
    , insertableImage
    , iiImageX
    , iiHeight
    , iiStartTime
    , iiFadeOut
    , iiWidth
    , iiOpacity
    , iiLayer
    , iiDuration
    , iiImageY
    , iiImageInserterInput
    , iiFadeIn

    -- ** Job
    , Job
    , job
    , jStatus
    , jJobTemplate
    , jAccelerationSettings
    , jARN
    , jCreatedAt
    , jQueue
    , jUserMetadata
    , jBillingTagsSource
    , jOutputGroupDetails
    , jErrorCode
    , jId
    , jTiming
    , jErrorMessage
    , jStatusUpdateIntervalInSecs
    , jRole
    , jSettings

    -- ** JobSettings
    , JobSettings
    , jobSettings
    , jsEsam
    , jsInputs
    , jsTimedMetadataInsertion
    , jsNielsenConfiguration
    , jsAvailBlanking
    , jsMotionImageInserter
    , jsTimecodeConfig
    , jsOutputGroups
    , jsAdAvailOffset

    -- ** JobTemplate
    , JobTemplate
    , jobTemplate
    , jtAccelerationSettings
    , jtLastUpdated
    , jtARN
    , jtCreatedAt
    , jtCategory
    , jtQueue
    , jtType
    , jtStatusUpdateIntervalInSecs
    , jtDescription
    , jtSettings
    , jtName

    -- ** JobTemplateSettings
    , JobTemplateSettings
    , jobTemplateSettings
    , jtsEsam
    , jtsInputs
    , jtsTimedMetadataInsertion
    , jtsNielsenConfiguration
    , jtsAvailBlanking
    , jtsMotionImageInserter
    , jtsTimecodeConfig
    , jtsOutputGroups
    , jtsAdAvailOffset

    -- ** M2tsScte35Esam
    , M2tsScte35Esam
    , m2tsScte35Esam
    , mseScte35EsamPid

    -- ** M2tsSettings
    , M2tsSettings
    , m2tsSettings
    , mssPmtPid
    , mssVideoPid
    , mssBufferModel
    , mssProgramNumber
    , mssScte35Pid
    , mssMinEbpInterval
    , mssTransportStreamId
    , mssMaxPcrInterval
    , mssFragmentTime
    , mssPrivateMetadataPid
    , mssScte35Esam
    , mssPmtInterval
    , mssDvbSdtSettings
    , mssNullPacketBitrate
    , mssAudioBufferModel
    , mssTimedMetadataPid
    , mssAudioFramesPerPes
    , mssPcrPid
    , mssSegmentationMarkers
    , mssDvbSubPids
    , mssScte35Source
    , mssPatInterval
    , mssForceTsVideoEbpOrder
    , mssEsRateInPes
    , mssBitrate
    , mssAudioPids
    , mssDvbTeletextPid
    , mssNielsenId3
    , mssSegmentationTime
    , mssEbpAudioInterval
    , mssDvbNitSettings
    , mssPcrControl
    , mssEbpPlacement
    , mssRateMode
    , mssSegmentationStyle
    , mssDvbTdtSettings

    -- ** M3u8Settings
    , M3u8Settings
    , m3u8Settings
    , msPmtPid
    , msVideoPid
    , msProgramNumber
    , msScte35Pid
    , msTransportStreamId
    , msPrivateMetadataPid
    , msPmtInterval
    , msTimedMetadataPid
    , msAudioFramesPerPes
    , msPcrPid
    , msTimedMetadata
    , msScte35Source
    , msPatInterval
    , msAudioPids
    , msNielsenId3
    , msPcrControl

    -- ** MotionImageInserter
    , MotionImageInserter
    , motionImageInserter
    , miiFramerate
    , miiStartTime
    , miiOffset
    , miiInput
    , miiInsertionMode
    , miiPlayback

    -- ** MotionImageInsertionFramerate
    , MotionImageInsertionFramerate
    , motionImageInsertionFramerate
    , miifFramerateDenominator
    , miifFramerateNumerator

    -- ** MotionImageInsertionOffset
    , MotionImageInsertionOffset
    , motionImageInsertionOffset
    , miioImageX
    , miioImageY

    -- ** MovSettings
    , MovSettings
    , movSettings
    , msReference
    , msCslgAtom
    , msMpeg2FourCCControl
    , msPaddingControl
    , msClapAtom

    -- ** Mp2Settings
    , Mp2Settings
    , mp2Settings
    , mChannels
    , mSampleRate
    , mBitrate

    -- ** Mp4Settings
    , Mp4Settings
    , mp4Settings
    , mMoovPlacement
    , mFreeSpaceBox
    , mMp4MajorBrand
    , mCslgAtom

    -- ** Mpeg2Settings
    , Mpeg2Settings
    , mpeg2Settings
    , msQualityTuningLevel
    , msTemporalAdaptiveQuantization
    , msSceneChangeDetect
    , msHrdBufferInitialFillPercentage
    , msSlowPal
    , msParNumerator
    , msGopSize
    , msNumberBFramesBetweenReferenceFrames
    , msGopSizeUnits
    , msHrdBufferSize
    , msRateControlMode
    , msTelecine
    , msIntraDcPrecision
    , msDynamicSubGop
    , msMinIInterval
    , msInterlaceMode
    , msParControl
    , msSoftness
    , msCodecProfile
    , msBitrate
    , msFramerateDenominator
    , msFramerateConversionAlgorithm
    , msCodecLevel
    , msFramerateControl
    , msAdaptiveQuantization
    , msFramerateNumerator
    , msMaxBitrate
    , msSyntax
    , msGopClosedCadence
    , msParDenominator
    , msSpatialAdaptiveQuantization

    -- ** MsSmoothEncryptionSettings
    , MsSmoothEncryptionSettings
    , msSmoothEncryptionSettings
    , msesSpekeKeyProvider

    -- ** MsSmoothGroupSettings
    , MsSmoothGroupSettings
    , msSmoothGroupSettings
    , msgsFragmentLength
    , msgsManifestEncoding
    , msgsDestination
    , msgsAudioDeduplication
    , msgsEncryption

    -- ** NielsenConfiguration
    , NielsenConfiguration
    , nielsenConfiguration
    , ncBreakoutCode
    , ncDistributorId

    -- ** NoiseReducer
    , NoiseReducer
    , noiseReducer
    , nrSpatialFilterSettings
    , nrFilterSettings
    , nrFilter

    -- ** NoiseReducerFilterSettings
    , NoiseReducerFilterSettings
    , noiseReducerFilterSettings
    , nrfsStrength

    -- ** NoiseReducerSpatialFilterSettings
    , NoiseReducerSpatialFilterSettings
    , noiseReducerSpatialFilterSettings
    , nrsfsStrength
    , nrsfsPostFilterSharpenStrength
    , nrsfsSpeed

    -- ** Output
    , Output
    , output
    , oCaptionDescriptions
    , oExtension
    , oVideoDescription
    , oContainerSettings
    , oOutputSettings
    , oPreset
    , oNameModifier
    , oAudioDescriptions

    -- ** OutputChannelMapping
    , OutputChannelMapping
    , outputChannelMapping
    , ocmInputChannels

    -- ** OutputDetail
    , OutputDetail
    , outputDetail
    , odVideoDetails
    , odDurationInMs

    -- ** OutputGroup
    , OutputGroup
    , outputGroup
    , ogOutputGroupSettings
    , ogOutputs
    , ogCustomName
    , ogName

    -- ** OutputGroupDetail
    , OutputGroupDetail
    , outputGroupDetail
    , ogdOutputDetails

    -- ** OutputGroupSettings
    , OutputGroupSettings
    , outputGroupSettings
    , ogsFileGroupSettings
    , ogsCmafGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsHlsGroupSettings
    , ogsType
    , ogsDashIsoGroupSettings

    -- ** OutputSettings
    , OutputSettings
    , outputSettings
    , osHlsSettings

    -- ** Preset
    , Preset
    , preset
    , pLastUpdated
    , pARN
    , pCreatedAt
    , pCategory
    , pType
    , pDescription
    , pSettings
    , pName

    -- ** PresetSettings
    , PresetSettings
    , presetSettings
    , psCaptionDescriptions
    , psVideoDescription
    , psContainerSettings
    , psAudioDescriptions

    -- ** ProresSettings
    , ProresSettings
    , proresSettings
    , psSlowPal
    , psParNumerator
    , psTelecine
    , psInterlaceMode
    , psParControl
    , psCodecProfile
    , psFramerateDenominator
    , psFramerateConversionAlgorithm
    , psFramerateControl
    , psFramerateNumerator
    , psParDenominator

    -- ** Queue
    , Queue
    , queue
    , qStatus
    , qLastUpdated
    , qARN
    , qCreatedAt
    , qReservationPlan
    , qPricingPlan
    , qSubmittedJobsCount
    , qProgressingJobsCount
    , qType
    , qDescription
    , qName

    -- ** Rectangle
    , Rectangle
    , rectangle
    , rHeight
    , rWidth
    , rX
    , rY

    -- ** RemixSettings
    , RemixSettings
    , remixSettings
    , rsChannelMapping
    , rsChannelsIn
    , rsChannelsOut

    -- ** ReservationPlan
    , ReservationPlan
    , reservationPlan
    , rpStatus
    , rpExpiresAt
    , rpPurchasedAt
    , rpCommitment
    , rpReservedSlots
    , rpRenewalType

    -- ** ReservationPlanSettings
    , ReservationPlanSettings
    , reservationPlanSettings
    , rpsCommitment
    , rpsReservedSlots
    , rpsRenewalType

    -- ** ResourceTags
    , ResourceTags
    , resourceTags
    , rtARN
    , rtTags

    -- ** SccDestinationSettings
    , SccDestinationSettings
    , sccDestinationSettings
    , sdsFramerate

    -- ** SpekeKeyProvider
    , SpekeKeyProvider
    , spekeKeyProvider
    , sResourceId
    , sCertificateARN
    , sURL
    , sSystemIds

    -- ** StaticKeyProvider
    , StaticKeyProvider
    , staticKeyProvider
    , skpStaticKeyValue
    , skpURL
    , skpKeyFormat
    , skpKeyFormatVersions

    -- ** TeletextDestinationSettings
    , TeletextDestinationSettings
    , teletextDestinationSettings
    , tdsPageNumber

    -- ** TeletextSourceSettings
    , TeletextSourceSettings
    , teletextSourceSettings
    , tssPageNumber

    -- ** TimecodeBurnin
    , TimecodeBurnin
    , timecodeBurnin
    , tbPrefix
    , tbFontSize
    , tbPosition

    -- ** TimecodeConfig
    , TimecodeConfig
    , timecodeConfig
    , tcStart
    , tcTimestampOffset
    , tcAnchor
    , tcSource

    -- ** TimedMetadataInsertion
    , TimedMetadataInsertion
    , timedMetadataInsertion
    , tmiId3Insertions

    -- ** Timing
    , Timing
    , timing
    , tStartTime
    , tFinishTime
    , tSubmitTime

    -- ** TrackSourceSettings
    , TrackSourceSettings
    , trackSourceSettings
    , tssTrackNumber

    -- ** TtmlDestinationSettings
    , TtmlDestinationSettings
    , ttmlDestinationSettings
    , tdsStylePassthrough

    -- ** VideoCodecSettings
    , VideoCodecSettings
    , videoCodecSettings
    , vcsFrameCaptureSettings
    , vcsCodec
    , vcsH265Settings
    , vcsProresSettings
    , vcsH264Settings
    , vcsMpeg2Settings

    -- ** VideoDescription
    , VideoDescription
    , videoDescription
    , vdTimecodeInsertion
    , vdHeight
    , vdAfdSignaling
    , vdSharpness
    , vdCrop
    , vdWidth
    , vdScalingBehavior
    , vdRespondToAfd
    , vdDropFrameTimecode
    , vdAntiAlias
    , vdFixedAfd
    , vdColorMetadata
    , vdCodecSettings
    , vdVideoPreprocessors
    , vdPosition

    -- ** VideoDetail
    , VideoDetail
    , videoDetail
    , vdHeightInPx
    , vdWidthInPx

    -- ** VideoPreprocessor
    , VideoPreprocessor
    , videoPreprocessor
    , vpTimecodeBurnin
    , vpColorCorrector
    , vpDeinterlacer
    , vpNoiseReducer
    , vpImageInserter

    -- ** VideoSelector
    , VideoSelector
    , videoSelector
    , vsProgramNumber
    , vsColorSpaceUsage
    , vsHdr10Metadata
    , vsPid
    , vsRotate
    , vsColorSpace

    -- ** WavSettings
    , WavSettings
    , wavSettings
    , wsBitDepth
    , wsChannels
    , wsFormat
    , wsSampleRate
    ) where

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
import Network.AWS.MediaConvert.ListJobs
import Network.AWS.MediaConvert.ListJobTemplates
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

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MediaConvert'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
