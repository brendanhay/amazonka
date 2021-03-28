-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _BadRequestException

    -- * Mpeg2Syntax
    , Mpeg2Syntax (..)

    -- * Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- * VideoSelector
    , VideoSelector (..)
    , mkVideoSelector
    , vsAlphaBehavior
    , vsColorSpace
    , vsColorSpaceUsage
    , vsHdr10Metadata
    , vsPid
    , vsProgramNumber
    , vsRotate

    -- * AvcIntraFramerateControl
    , AvcIntraFramerateControl (..)

    -- * ProresCodecProfile
    , ProresCodecProfile (..)

    -- * Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- * H264ParControl
    , H264ParControl (..)

    -- * AvcIntraFramerateConversionAlgorithm
    , AvcIntraFramerateConversionAlgorithm (..)

    -- * Mpeg2DynamicSubGop
    , Mpeg2DynamicSubGop (..)

    -- * H265CodecProfile
    , H265CodecProfile (..)

    -- * FileGroupSettings
    , FileGroupSettings (..)
    , mkFileGroupSettings
    , fgsDestination
    , fgsDestinationSettings

    -- * HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- * TeletextDestinationSettings
    , TeletextDestinationSettings (..)
    , mkTeletextDestinationSettings
    , tdsPageNumber
    , tdsPageTypes

    -- * NoiseReducerFilter
    , NoiseReducerFilter (..)

    -- * HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- * TtmlStylePassthrough
    , TtmlStylePassthrough (..)

    -- * H264InterlaceMode
    , H264InterlaceMode (..)

    -- * FrameCaptureSettings
    , FrameCaptureSettings (..)
    , mkFrameCaptureSettings
    , fcsFramerateDenominator
    , fcsFramerateNumerator
    , fcsMaxCaptures
    , fcsQuality

    -- * H264RepeatPps
    , H264RepeatPps (..)

    -- * TimecodeBurnin
    , TimecodeBurnin (..)
    , mkTimecodeBurnin
    , tbFontSize
    , tbPosition
    , tbPrefix

    -- * NexGuardFileMarkerSettings
    , NexGuardFileMarkerSettings (..)
    , mkNexGuardFileMarkerSettings
    , ngfmsLicense
    , ngfmsPayload
    , ngfmsPreset
    , ngfmsStrength

    -- * JobTemplate
    , JobTemplate (..)
    , mkJobTemplate
    , jtSettings
    , jtName
    , jtAccelerationSettings
    , jtArn
    , jtCategory
    , jtCreatedAt
    , jtDescription
    , jtHopDestinations
    , jtLastUpdated
    , jtPriority
    , jtQueue
    , jtStatusUpdateInterval
    , jtType

    -- * EsamSettings
    , EsamSettings (..)
    , mkEsamSettings
    , esManifestConfirmConditionNotification
    , esResponseSignalPreroll
    , esSignalProcessingNotification

    -- * ContainerType
    , ContainerType (..)

    -- * ReservationPlanStatus
    , ReservationPlanStatus (..)

    -- * M2tsBufferModel
    , M2tsBufferModel (..)

    -- * Vp9FramerateConversionAlgorithm
    , Vp9FramerateConversionAlgorithm (..)

    -- * NielsenSourceWatermarkStatusType
    , NielsenSourceWatermarkStatusType (..)

    -- * VideoCodec
    , VideoCodec (..)

    -- * AccelerationSettings
    , AccelerationSettings (..)
    , mkAccelerationSettings
    , asMode

    -- * H265AlternateTransferFunctionSei
    , H265AlternateTransferFunctionSei (..)

    -- * H264FlickerAdaptiveQuantization
    , H264FlickerAdaptiveQuantization (..)

    -- * InputRotate
    , InputRotate (..)

    -- * ChannelMapping
    , ChannelMapping (..)
    , mkChannelMapping
    , cmOutputChannels

    -- * DashIsoSegmentControl
    , DashIsoSegmentControl (..)

    -- * InputFilterEnable
    , InputFilterEnable (..)

    -- * CmafAdditionalManifest
    , CmafAdditionalManifest (..)
    , mkCmafAdditionalManifest
    , camManifestNameModifier
    , camSelectedOutputs

    -- * JobMessages
    , JobMessages (..)
    , mkJobMessages
    , jmInfo
    , jmWarning

    -- * DvbSubtitleShadowColor
    , DvbSubtitleShadowColor (..)

    -- * Mpeg2SlowPal
    , Mpeg2SlowPal (..)

    -- * MovReference
    , MovReference (..)

    -- * AacAudioDescriptionBroadcasterMix
    , AacAudioDescriptionBroadcasterMix (..)

    -- * Vp9FramerateControl
    , Vp9FramerateControl (..)

    -- * LanguageCode
    , LanguageCode (..)

    -- * Vp8ParControl
    , Vp8ParControl (..)

    -- * VideoCodecSettings
    , VideoCodecSettings (..)
    , mkVideoCodecSettings
    , vcsAv1Settings
    , vcsAvcIntraSettings
    , vcsCodec
    , vcsFrameCaptureSettings
    , vcsH264Settings
    , vcsH265Settings
    , vcsMpeg2Settings
    , vcsProresSettings
    , vcsVc3Settings
    , vcsVp8Settings
    , vcsVp9Settings

    -- * S3EncryptionSettings
    , S3EncryptionSettings (..)
    , mkS3EncryptionSettings
    , sesEncryptionType
    , sesKmsKeyArn

    -- * MovPaddingControl
    , MovPaddingControl (..)

    -- * HlsClientCache
    , HlsClientCache (..)

    -- * JobTemplateSettings
    , JobTemplateSettings (..)
    , mkJobTemplateSettings
    , jtsAdAvailOffset
    , jtsAvailBlanking
    , jtsEsam
    , jtsInputs
    , jtsMotionImageInserter
    , jtsNielsenConfiguration
    , jtsNielsenNonLinearWatermark
    , jtsOutputGroups
    , jtsTimecodeConfig
    , jtsTimedMetadataInsertion

    -- * HlsAudioOnlyHeader
    , HlsAudioOnlyHeader (..)

    -- * H264QvbrSettings
    , H264QvbrSettings (..)
    , mkH264QvbrSettings
    , hqsMaxAverageBitrate
    , hqsQvbrQualityLevel
    , hqsQvbrQualityLevelFineTune

    -- * CmafWriteSegmentTimelineInRepresentation
    , CmafWriteSegmentTimelineInRepresentation (..)

    -- * AutomatedAbrSettings
    , AutomatedAbrSettings (..)
    , mkAutomatedAbrSettings
    , aasMaxAbrBitrate
    , aasMaxRenditions
    , aasMinAbrBitrate

    -- * H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- * Mpeg2SceneChangeDetect
    , Mpeg2SceneChangeDetect (..)

    -- * HlsSettings
    , HlsSettings (..)
    , mkHlsSettings
    , hsAudioGroupId
    , hsAudioOnlyContainer
    , hsAudioRenditionSets
    , hsAudioTrackType
    , hsIFrameOnlyManifest
    , hsSegmentModifier

    -- * InsertableImage
    , InsertableImage (..)
    , mkInsertableImage
    , iiDuration
    , iiFadeIn
    , iiFadeOut
    , iiHeight
    , iiImageInserterInput
    , iiImageX
    , iiImageY
    , iiLayer
    , iiOpacity
    , iiStartTime
    , iiWidth

    -- * CaptionDestinationType
    , CaptionDestinationType (..)

    -- * MsSmoothAdditionalManifest
    , MsSmoothAdditionalManifest (..)
    , mkMsSmoothAdditionalManifest
    , msamManifestNameModifier
    , msamSelectedOutputs

    -- * Vc3InterlaceMode
    , Vc3InterlaceMode (..)

    -- * StatusUpdateInterval
    , StatusUpdateInterval (..)

    -- * EmbeddedTerminateCaptions
    , EmbeddedTerminateCaptions (..)

    -- * AiffSettings
    , AiffSettings (..)
    , mkAiffSettings
    , assBitDepth
    , assChannels
    , assSampleRate

    -- * TimecodeSource
    , TimecodeSource (..)

    -- * QueueStatus
    , QueueStatus (..)

    -- * AfdSignaling
    , AfdSignaling (..)

    -- * AacRateControlMode
    , AacRateControlMode (..)

    -- * ProresParControl
    , ProresParControl (..)

    -- * AudioChannelTaggingSettings
    , AudioChannelTaggingSettings (..)
    , mkAudioChannelTaggingSettings
    , actsChannelTag

    -- * M2tsScte35Esam
    , M2tsScte35Esam (..)
    , mkM2tsScte35Esam
    , mseScte35EsamPid

    -- * OutputGroupSettings
    , OutputGroupSettings (..)
    , mkOutputGroupSettings
    , ogsCmafGroupSettings
    , ogsDashIsoGroupSettings
    , ogsFileGroupSettings
    , ogsHlsGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsType

    -- * Eac3AtmosDialogueIntelligence
    , Eac3AtmosDialogueIntelligence (..)

    -- * TeletextSourceSettings
    , TeletextSourceSettings (..)
    , mkTeletextSourceSettings
    , tssPageNumber

    -- * Mpeg2FramerateConversionAlgorithm
    , Mpeg2FramerateConversionAlgorithm (..)

    -- * Mpeg2FramerateControl
    , Mpeg2FramerateControl (..)

    -- * CaptionDescriptionPreset
    , CaptionDescriptionPreset (..)
    , mkCaptionDescriptionPreset
    , cdpCustomLanguageCode
    , cdpDestinationSettings
    , cdpLanguageCode
    , cdpLanguageDescription

    -- * M2tsSettings
    , M2tsSettings (..)
    , mkM2tsSettings
    , mssAudioBufferModel
    , mssAudioDuration
    , mssAudioFramesPerPes
    , mssAudioPids
    , mssBitrate
    , mssBufferModel
    , mssDvbNitSettings
    , mssDvbSdtSettings
    , mssDvbSubPids
    , mssDvbTdtSettings
    , mssDvbTeletextPid
    , mssEbpAudioInterval
    , mssEbpPlacement
    , mssEsRateInPes
    , mssForceTsVideoEbpOrder
    , mssFragmentTime
    , mssMaxPcrInterval
    , mssMinEbpInterval
    , mssNielsenId3
    , mssNullPacketBitrate
    , mssPatInterval
    , mssPcrControl
    , mssPcrPid
    , mssPmtInterval
    , mssPmtPid
    , mssPrivateMetadataPid
    , mssProgramNumber
    , mssRateMode
    , mssScte35Esam
    , mssScte35Pid
    , mssScte35Source
    , mssSegmentationMarkers
    , mssSegmentationStyle
    , mssSegmentationTime
    , mssTimedMetadataPid
    , mssTransportStreamId
    , mssVideoPid

    -- * H264CodecProfile
    , H264CodecProfile (..)

    -- * Mpeg2CodecLevel
    , Mpeg2CodecLevel (..)

    -- * AudioDefaultSelection
    , AudioDefaultSelection (..)

    -- * NielsenUniqueTicPerAudioTrackType
    , NielsenUniqueTicPerAudioTrackType (..)

    -- * H265UnregisteredSeiTimecode
    , H265UnregisteredSeiTimecode (..)

    -- * H265ParControl
    , H265ParControl (..)

    -- * DvbSubDestinationSettings
    , DvbSubDestinationSettings (..)
    , mkDvbSubDestinationSettings
    , dsdsAlignment
    , dsdsBackgroundColor
    , dsdsBackgroundOpacity
    , dsdsFontColor
    , dsdsFontOpacity
    , dsdsFontResolution
    , dsdsFontScript
    , dsdsFontSize
    , dsdsOutlineColor
    , dsdsOutlineSize
    , dsdsShadowColor
    , dsdsShadowOpacity
    , dsdsShadowXOffset
    , dsdsShadowYOffset
    , dsdsSubtitlingType
    , dsdsTeletextSpacing
    , dsdsXPosition
    , dsdsYPosition

    -- * Mp4CslgAtom
    , Mp4CslgAtom (..)

    -- * Mp3RateControlMode
    , Mp3RateControlMode (..)

    -- * H264FramerateControl
    , H264FramerateControl (..)

    -- * MpdCaptionContainerType
    , MpdCaptionContainerType (..)

    -- * AvcIntraInterlaceMode
    , AvcIntraInterlaceMode (..)

    -- * H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- * HlsOfflineEncrypted
    , HlsOfflineEncrypted (..)

    -- * HlsOutputSelection
    , HlsOutputSelection (..)

    -- * M2tsAudioDuration
    , M2tsAudioDuration (..)

    -- * Eac3AtmosSurroundExMode
    , Eac3AtmosSurroundExMode (..)

    -- * Mpeg2CodecProfile
    , Mpeg2CodecProfile (..)

    -- * H265DynamicSubGop
    , H265DynamicSubGop (..)

    -- * CmafInitializationVectorInManifest
    , CmafInitializationVectorInManifest (..)

    -- * AlphaBehavior
    , AlphaBehavior (..)

    -- * QueueListBy
    , QueueListBy (..)

    -- * VideoPreprocessor
    , VideoPreprocessor (..)
    , mkVideoPreprocessor
    , vpColorCorrector
    , vpDeinterlacer
    , vpDolbyVision
    , vpImageInserter
    , vpNoiseReducer
    , vpPartnerWatermarking
    , vpTimecodeBurnin

    -- * H264CodecLevel
    , H264CodecLevel (..)

    -- * DvbSubtitleTeletextSpacing
    , DvbSubtitleTeletextSpacing (..)

    -- * AudioDescription
    , AudioDescription (..)
    , mkAudioDescription
    , adAudioChannelTaggingSettings
    , adAudioNormalizationSettings
    , adAudioSourceName
    , adAudioType
    , adAudioTypeControl
    , adCodecSettings
    , adCustomLanguageCode
    , adLanguageCode
    , adLanguageCodeControl
    , adRemixSettings
    , adStreamName

    -- * JobPhase
    , JobPhase (..)

    -- * TtmlDestinationSettings
    , TtmlDestinationSettings (..)
    , mkTtmlDestinationSettings
    , tdsStylePassthrough

    -- * Vp8RateControlMode
    , Vp8RateControlMode (..)

    -- * HlsEncryptionType
    , HlsEncryptionType (..)

    -- * H264FramerateConversionAlgorithm
    , H264FramerateConversionAlgorithm (..)

    -- * SccDestinationFramerate
    , SccDestinationFramerate (..)

    -- * MxfProfile
    , MxfProfile (..)

    -- * AacCodingMode
    , AacCodingMode (..)

    -- * Eac3AtmosMeteringMode
    , Eac3AtmosMeteringMode (..)

    -- * DolbyVision
    , DolbyVision (..)
    , mkDolbyVision
    , dvL6Metadata
    , dvL6Mode
    , dvProfile

    -- * HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- * OutputGroup
    , OutputGroup (..)
    , mkOutputGroup
    , ogAutomatedEncodingSettings
    , ogCustomName
    , ogName
    , ogOutputGroupSettings
    , ogOutputs

    -- * HlsIFrameOnlyManifest
    , HlsIFrameOnlyManifest (..)

    -- * HopDestination
    , HopDestination (..)
    , mkHopDestination
    , hdPriority
    , hdQueue
    , hdWaitMinutes

    -- * DolbyVisionProfile
    , DolbyVisionProfile (..)

    -- * CmfcAudioDuration
    , CmfcAudioDuration (..)

    -- * AudioSelector
    , AudioSelector (..)
    , mkAudioSelector
    , asCustomLanguageCode
    , asDefaultSelection
    , asExternalAudioFileInput
    , asLanguageCode
    , asOffset
    , asPids
    , asProgramSelection
    , asRemixSettings
    , asSelectorType
    , asTracks

    -- * H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- * CmafGroupSettings
    , CmafGroupSettings (..)
    , mkCmafGroupSettings
    , cgsAdditionalManifests
    , cgsBaseUrl
    , cgsClientCache
    , cgsCodecSpecification
    , cgsDestination
    , cgsDestinationSettings
    , cgsEncryption
    , cgsFragmentLength
    , cgsManifestCompression
    , cgsManifestDurationFormat
    , cgsMinBufferTime
    , cgsMinFinalSegmentLength
    , cgsMpdProfile
    , cgsSegmentControl
    , cgsSegmentLength
    , cgsStreamInfResolution
    , cgsWriteDashManifest
    , cgsWriteHlsManifest
    , cgsWriteSegmentTimelineInRepresentation

    -- * CmafEncryptionSettings
    , CmafEncryptionSettings (..)
    , mkCmafEncryptionSettings
    , cesConstantInitializationVector
    , cesEncryptionMethod
    , cesInitializationVectorInManifest
    , cesSpekeKeyProvider
    , cesStaticKeyProvider
    , cesType

    -- * H265SlowPal
    , H265SlowPal (..)

    -- * Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- * H265TemporalIds
    , H265TemporalIds (..)

    -- * InputPsiControl
    , InputPsiControl (..)

    -- * M3u8AudioDuration
    , M3u8AudioDuration (..)

    -- * TimedMetadataInsertion
    , TimedMetadataInsertion (..)
    , mkTimedMetadataInsertion
    , tmiId3Insertions

    -- * M3u8NielsenId3
    , M3u8NielsenId3 (..)

    -- * Vp9ParControl
    , Vp9ParControl (..)

    -- * ReservationPlan
    , ReservationPlan (..)
    , mkReservationPlan
    , rpCommitment
    , rpExpiresAt
    , rpPurchasedAt
    , rpRenewalType
    , rpReservedSlots
    , rpStatus

    -- * PricingPlan
    , PricingPlan (..)

    -- * MpdScte35Source
    , MpdScte35Source (..)

    -- * DolbyVisionLevel6Mode
    , DolbyVisionLevel6Mode (..)

    -- * BurninSubtitleBackgroundColor
    , BurninSubtitleBackgroundColor (..)

    -- * ResourceTags
    , ResourceTags (..)
    , mkResourceTags
    , rtArn
    , rtTags

    -- * HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- * ProresSlowPal
    , ProresSlowPal (..)

    -- * Av1Settings
    , Av1Settings (..)
    , mkAv1Settings
    , aAdaptiveQuantization
    , aFramerateControl
    , aFramerateConversionAlgorithm
    , aFramerateDenominator
    , aFramerateNumerator
    , aGopSize
    , aMaxBitrate
    , aNumberBFramesBetweenReferenceFrames
    , aQvbrSettings
    , aRateControlMode
    , aSlices
    , aSpatialAdaptiveQuantization

    -- * AncillaryTerminateCaptions
    , AncillaryTerminateCaptions (..)

    -- * HlsSegmentControl
    , HlsSegmentControl (..)

    -- * NoiseReducerSpatialFilterSettings
    , NoiseReducerSpatialFilterSettings (..)
    , mkNoiseReducerSpatialFilterSettings
    , nrsfsPostFilterSharpenStrength
    , nrsfsSpeed
    , nrsfsStrength

    -- * H265QvbrSettings
    , H265QvbrSettings (..)
    , mkH265QvbrSettings
    , hMaxAverageBitrate
    , hQvbrQualityLevel
    , hQvbrQualityLevelFineTune

    -- * H265SampleAdaptiveOffsetFilterMode
    , H265SampleAdaptiveOffsetFilterMode (..)

    -- * Eac3DcFilter
    , Eac3DcFilter (..)

    -- * SimulateReservedQueue
    , SimulateReservedQueue (..)

    -- * CmafManifestDurationFormat
    , CmafManifestDurationFormat (..)

    -- * HlsAdMarkers
    , HlsAdMarkers (..)

    -- * Mp4MoovPlacement
    , Mp4MoovPlacement (..)

    -- * DvbSubtitleAlignment
    , DvbSubtitleAlignment (..)

    -- * InputDeblockFilter
    , InputDeblockFilter (..)

    -- * HlsAudioTrackType
    , HlsAudioTrackType (..)

    -- * CmfcScte35Esam
    , CmfcScte35Esam (..)

    -- * VideoTimecodeInsertion
    , VideoTimecodeInsertion (..)

    -- * DescribeEndpointsMode
    , DescribeEndpointsMode (..)

    -- * CmafEncryptionType
    , CmafEncryptionType (..)

    -- * AacCodecProfile
    , AacCodecProfile (..)

    -- * NielsenConfiguration
    , NielsenConfiguration (..)
    , mkNielsenConfiguration
    , ncBreakoutCode
    , ncDistributorId

    -- * ColorCorrector
    , ColorCorrector (..)
    , mkColorCorrector
    , ccBrightness
    , ccColorSpaceConversion
    , ccContrast
    , ccHdr10Metadata
    , ccHue
    , ccSaturation

    -- * HlsInitializationVectorInManifest
    , HlsInitializationVectorInManifest (..)

    -- * AudioLanguageCodeControl
    , AudioLanguageCodeControl (..)

    -- * AvcIntraSlowPal
    , AvcIntraSlowPal (..)

    -- * H265TemporalAdaptiveQuantization
    , H265TemporalAdaptiveQuantization (..)

    -- * Input
    , Input (..)
    , mkInput
    , iAudioSelectorGroups
    , iAudioSelectors
    , iCaptionSelectors
    , iCrop
    , iDeblockFilter
    , iDecryptionSettings
    , iDenoiseFilter
    , iFileInput
    , iFilterEnable
    , iFilterStrength
    , iImageInserter
    , iInputClippings
    , iInputScanType
    , iPosition
    , iProgramNumber
    , iPsiControl
    , iSupplementalImps
    , iTimecodeSource
    , iTimecodeStart
    , iVideoSelector

    -- * MxfSettings
    , MxfSettings (..)
    , mkMxfSettings
    , msAfdSignaling
    , msProfile

    -- * DvbSdtSettings
    , DvbSdtSettings (..)
    , mkDvbSdtSettings
    , dssOutputSdt
    , dssSdtInterval
    , dssServiceName
    , dssServiceProviderName

    -- * MxfAfdSignaling
    , MxfAfdSignaling (..)

    -- * FileSourceSettings
    , FileSourceSettings (..)
    , mkFileSourceSettings
    , fssConvert608To708
    , fssFramerate
    , fssSourceFile
    , fssTimeDelta

    -- * M3u8Settings
    , M3u8Settings (..)
    , mkM3u8Settings
    , msAudioDuration
    , msAudioFramesPerPes
    , msAudioPids
    , msNielsenId3
    , msPatInterval
    , msPcrControl
    , msPcrPid
    , msPmtInterval
    , msPmtPid
    , msPrivateMetadataPid
    , msProgramNumber
    , msScte35Pid
    , msScte35Source
    , msTimedMetadata
    , msTimedMetadataPid
    , msTransportStreamId
    , msVideoPid

    -- * Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- * AudioNormalizationSettings
    , AudioNormalizationSettings (..)
    , mkAudioNormalizationSettings
    , ansAlgorithm
    , ansAlgorithmControl
    , ansCorrectionGateLevel
    , ansLoudnessLogging
    , ansPeakCalculation
    , ansTargetLkfs

    -- * CmfcSettings
    , CmfcSettings (..)
    , mkCmfcSettings
    , csAudioDuration
    , csScte35Esam
    , csScte35Source

    -- * Av1FramerateConversionAlgorithm
    , Av1FramerateConversionAlgorithm (..)

    -- * H265FlickerAdaptiveQuantization
    , H265FlickerAdaptiveQuantization (..)

    -- * MsSmoothGroupSettings
    , MsSmoothGroupSettings (..)
    , mkMsSmoothGroupSettings
    , msgsAdditionalManifests
    , msgsAudioDeduplication
    , msgsDestination
    , msgsDestinationSettings
    , msgsEncryption
    , msgsFragmentLength
    , msgsManifestEncoding

    -- * Av1FramerateControl
    , Av1FramerateControl (..)

    -- * InputDecryptionSettings
    , InputDecryptionSettings (..)
    , mkInputDecryptionSettings
    , idsDecryptionMode
    , idsEncryptedDecryptionKey
    , idsInitializationVector
    , idsKmsKeyRegion

    -- * F4vMoovPlacement
    , F4vMoovPlacement (..)

    -- * Eac3DynamicRangeCompressionLine
    , Eac3DynamicRangeCompressionLine (..)

    -- * ImscStylePassthrough
    , ImscStylePassthrough (..)

    -- * Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- * AvailBlanking
    , AvailBlanking (..)
    , mkAvailBlanking
    , abAvailBlankingImage

    -- * DvbSubtitleOutlineColor
    , DvbSubtitleOutlineColor (..)

    -- * ProresFramerateConversionAlgorithm
    , ProresFramerateConversionAlgorithm (..)

    -- * H265CodecLevel
    , H265CodecLevel (..)

    -- * NoiseReducerFilterSettings
    , NoiseReducerFilterSettings (..)
    , mkNoiseReducerFilterSettings
    , nrfsStrength

    -- * H265FramerateConversionAlgorithm
    , H265FramerateConversionAlgorithm (..)

    -- * DestinationSettings
    , DestinationSettings (..)
    , mkDestinationSettings
    , dsS3Settings

    -- * AccelerationMode
    , AccelerationMode (..)

    -- * M3u8Scte35Source
    , M3u8Scte35Source (..)

    -- * DashIsoMpdProfile
    , DashIsoMpdProfile (..)

    -- * NielsenNonLinearWatermarkSettings
    , NielsenNonLinearWatermarkSettings (..)
    , mkNielsenNonLinearWatermarkSettings
    , nnlwsActiveWatermarkProcess
    , nnlwsAdiFilename
    , nnlwsAssetId
    , nnlwsAssetName
    , nnlwsCbetSourceId
    , nnlwsEpisodeId
    , nnlwsMetadataDestination
    , nnlwsSourceId
    , nnlwsSourceWatermarkStatus
    , nnlwsTicServerUrl
    , nnlwsUniqueTicPerAudioTrack

    -- * M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- * H264Syntax
    , H264Syntax (..)

    -- * NoiseFilterPostTemporalSharpening
    , NoiseFilterPostTemporalSharpening (..)

    -- * Queue
    , Queue (..)
    , mkQueue
    , qName
    , qArn
    , qCreatedAt
    , qDescription
    , qLastUpdated
    , qPricingPlan
    , qProgressingJobsCount
    , qReservationPlan
    , qStatus
    , qSubmittedJobsCount
    , qType

    -- * ScalingBehavior
    , ScalingBehavior (..)

    -- * EsamManifestConfirmConditionNotification
    , EsamManifestConfirmConditionNotification (..)
    , mkEsamManifestConfirmConditionNotification
    , emccnMccXml

    -- * ProresFramerateControl
    , ProresFramerateControl (..)

    -- * H264GopBReference
    , H264GopBReference (..)

    -- * Id3Insertion
    , Id3Insertion (..)
    , mkId3Insertion
    , iiId3
    , iiTimecode

    -- * HlsKeyProviderType
    , HlsKeyProviderType (..)

    -- * MovSettings
    , MovSettings (..)
    , mkMovSettings
    , msClapAtom
    , msCslgAtom
    , msMpeg2FourCCControl
    , msPaddingControl
    , msReference

    -- * RespondToAfd
    , RespondToAfd (..)

    -- * MotionImageInserter
    , MotionImageInserter (..)
    , mkMotionImageInserter
    , miiFramerate
    , miiInput
    , miiInsertionMode
    , miiOffset
    , miiPlayback
    , miiStartTime

    -- * MpdAudioDuration
    , MpdAudioDuration (..)

    -- * CmfcScte35Source
    , CmfcScte35Source (..)

    -- * Mpeg2ParControl
    , Mpeg2ParControl (..)

    -- * H265FramerateControl
    , H265FramerateControl (..)

    -- * InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- * ColorSpaceConversion
    , ColorSpaceConversion (..)

    -- * H264SpatialAdaptiveQuantization
    , H264SpatialAdaptiveQuantization (..)

    -- * H264FieldEncoding
    , H264FieldEncoding (..)

    -- * DashIsoHbbtvCompliance
    , DashIsoHbbtvCompliance (..)

    -- * AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- * Ac3Settings
    , Ac3Settings (..)
    , mkAc3Settings
    , aBitrate
    , aBitstreamMode
    , aCodingMode
    , aDialnorm
    , aDynamicRangeCompressionProfile
    , aLfeFilter
    , aMetadataControl
    , aSampleRate

    -- * MpdScte35Esam
    , MpdScte35Esam (..)

    -- * Mpeg2Telecine
    , Mpeg2Telecine (..)

    -- * Mp4FreeSpaceBox
    , Mp4FreeSpaceBox (..)

    -- * Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- * ColorSpaceUsage
    , ColorSpaceUsage (..)

    -- * AudioSelectorGroup
    , AudioSelectorGroup (..)
    , mkAudioSelectorGroup
    , asgAudioSelectorNames

    -- * AacRawFormat
    , AacRawFormat (..)

    -- * H265Settings
    , H265Settings (..)
    , mkH265Settings
    , hAdaptiveQuantization
    , hAlternateTransferFunctionSei
    , hBitrate
    , hCodecLevel
    , hCodecProfile
    , hDynamicSubGop
    , hFlickerAdaptiveQuantization
    , hFramerateControl
    , hFramerateConversionAlgorithm
    , hFramerateDenominator
    , hFramerateNumerator
    , hGopBReference
    , hGopClosedCadence
    , hGopSize
    , hGopSizeUnits
    , hHrdBufferInitialFillPercentage
    , hHrdBufferSize
    , hInterlaceMode
    , hMaxBitrate
    , hMinIInterval
    , hNumberBFramesBetweenReferenceFrames
    , hNumberReferenceFrames
    , hParControl
    , hParDenominator
    , hParNumerator
    , hQualityTuningLevel
    , hQvbrSettings
    , hRateControlMode
    , hSampleAdaptiveOffsetFilterMode
    , hSceneChangeDetect
    , hSlices
    , hSlowPal
    , hSpatialAdaptiveQuantization
    , hTelecine
    , hTemporalAdaptiveQuantization
    , hTemporalIds
    , hTiles
    , hUnregisteredSeiTimecode
    , hWriteMp4PackagingType

    -- * CmafClientCache
    , CmafClientCache (..)

    -- * Deinterlacer
    , Deinterlacer (..)
    , mkDeinterlacer
    , dAlgorithm
    , dControl
    , dMode

    -- * Mp4Settings
    , Mp4Settings (..)
    , mkMp4Settings
    , mAudioDuration
    , mCslgAtom
    , mCttsVersion
    , mFreeSpaceBox
    , mMoovPlacement
    , mMp4MajorBrand

    -- * DropFrameTimecode
    , DropFrameTimecode (..)

    -- * Eac3AtmosStereoDownmix
    , Eac3AtmosStereoDownmix (..)

    -- * CmafWriteHLSManifest
    , CmafWriteHLSManifest (..)

    -- * Eac3DynamicRangeCompressionRf
    , Eac3DynamicRangeCompressionRf (..)

    -- * PresetListBy
    , PresetListBy (..)

    -- * AntiAlias
    , AntiAlias (..)

    -- * DvbSubSourceSettings
    , DvbSubSourceSettings (..)
    , mkDvbSubSourceSettings
    , dsssPid

    -- * OpusSettings
    , OpusSettings (..)
    , mkOpusSettings
    , osBitrate
    , osChannels
    , osSampleRate

    -- * Mpeg2SpatialAdaptiveQuantization
    , Mpeg2SpatialAdaptiveQuantization (..)

    -- * FontScript
    , FontScript (..)

    -- * MpdSettings
    , MpdSettings (..)
    , mkMpdSettings
    , msfAccessibilityCaptionHints
    , msfAudioDuration
    , msfCaptionContainerType
    , msfScte35Esam
    , msfScte35Source

    -- * BurninSubtitleTeletextSpacing
    , BurninSubtitleTeletextSpacing (..)

    -- * H264QualityTuningLevel
    , H264QualityTuningLevel (..)

    -- * ProresSettings
    , ProresSettings (..)
    , mkProresSettings
    , psCodecProfile
    , psFramerateControl
    , psFramerateConversionAlgorithm
    , psFramerateDenominator
    , psFramerateNumerator
    , psInterlaceMode
    , psParControl
    , psParDenominator
    , psParNumerator
    , psSlowPal
    , psTelecine

    -- * H264UnregisteredSeiTimecode
    , H264UnregisteredSeiTimecode (..)

    -- * Mpeg2IntraDcPrecision
    , Mpeg2IntraDcPrecision (..)

    -- * OutputSdt
    , OutputSdt (..)

    -- * CaptionSourceFramerate
    , CaptionSourceFramerate (..)
    , mkCaptionSourceFramerate
    , csfFramerateDenominator
    , csfFramerateNumerator

    -- * Vc3SlowPal
    , Vc3SlowPal (..)

    -- * Av1QvbrSettings
    , Av1QvbrSettings (..)
    , mkAv1QvbrSettings
    , aqsQvbrQualityLevel
    , aqsQvbrQualityLevelFineTune

    -- * NoiseReducer
    , NoiseReducer (..)
    , mkNoiseReducer
    , nrFilter
    , nrFilterSettings
    , nrSpatialFilterSettings
    , nrTemporalFilterSettings

    -- * TimedMetadata
    , TimedMetadata (..)

    -- * Eac3LfeControl
    , Eac3LfeControl (..)

    -- * Output
    , Output (..)
    , mkOutput
    , oAudioDescriptions
    , oCaptionDescriptions
    , oContainerSettings
    , oExtension
    , oNameModifier
    , oOutputSettings
    , oPreset
    , oVideoDescription

    -- * Av1SpatialAdaptiveQuantization
    , Av1SpatialAdaptiveQuantization (..)

    -- * Vc3Telecine
    , Vc3Telecine (..)

    -- * M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- * SpekeKeyProviderCmaf
    , SpekeKeyProviderCmaf (..)
    , mkSpekeKeyProviderCmaf
    , skpcCertificateArn
    , skpcDashSignaledSystemIds
    , skpcHlsSignaledSystemIds
    , skpcResourceId
    , skpcUrl

    -- * Job
    , Job (..)
    , mkJob
    , jRole
    , jSettings
    , jAccelerationSettings
    , jAccelerationStatus
    , jArn
    , jBillingTagsSource
    , jCreatedAt
    , jCurrentPhase
    , jErrorCode
    , jErrorMessage
    , jHopDestinations
    , jId
    , jJobPercentComplete
    , jJobTemplate
    , jMessages
    , jOutputGroupDetails
    , jPriority
    , jQueue
    , jQueueTransitions
    , jRetryCount
    , jSimulateReservedQueue
    , jStatus
    , jStatusUpdateInterval
    , jTiming
    , jUserMetadata

    -- * ImageInserter
    , ImageInserter (..)
    , mkImageInserter
    , iiInsertableImages

    -- * TrackSourceSettings
    , TrackSourceSettings (..)
    , mkTrackSourceSettings
    , tssTrackNumber

    -- * HlsCaptionLanguageMapping
    , HlsCaptionLanguageMapping (..)
    , mkHlsCaptionLanguageMapping
    , hclmCaptionChannel
    , hclmCustomLanguageCode
    , hclmLanguageCode
    , hclmLanguageDescription

    -- * H264TemporalAdaptiveQuantization
    , H264TemporalAdaptiveQuantization (..)

    -- * H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- * BillingTagsSource
    , BillingTagsSource (..)

    -- * AncillarySourceSettings
    , AncillarySourceSettings (..)
    , mkAncillarySourceSettings
    , assConvert608To708
    , assSourceAncillaryChannelNumber
    , assTerminateCaptions

    -- * AudioChannelTag
    , AudioChannelTag (..)

    -- * H265RateControlMode
    , H265RateControlMode (..)

    -- * CmafKeyProviderType
    , CmafKeyProviderType (..)

    -- * S3DestinationAccessControl
    , S3DestinationAccessControl (..)
    , mkS3DestinationAccessControl
    , sdacCannedAcl

    -- * Eac3CodingMode
    , Eac3CodingMode (..)

    -- * BurninSubtitleAlignment
    , BurninSubtitleAlignment (..)

    -- * VideoDescription
    , VideoDescription (..)
    , mkVideoDescription
    , vdAfdSignaling
    , vdAntiAlias
    , vdCodecSettings
    , vdColorMetadata
    , vdCrop
    , vdDropFrameTimecode
    , vdFixedAfd
    , vdHeight
    , vdPosition
    , vdRespondToAfd
    , vdScalingBehavior
    , vdSharpness
    , vdTimecodeInsertion
    , vdVideoPreprocessors
    , vdWidth

    -- * CaptionDestinationSettings
    , CaptionDestinationSettings (..)
    , mkCaptionDestinationSettings
    , cdsBurninDestinationSettings
    , cdsDestinationType
    , cdsDvbSubDestinationSettings
    , cdsEmbeddedDestinationSettings
    , cdsImscDestinationSettings
    , cdsSccDestinationSettings
    , cdsTeletextDestinationSettings
    , cdsTtmlDestinationSettings

    -- * EmbeddedDestinationSettings
    , EmbeddedDestinationSettings (..)
    , mkEmbeddedDestinationSettings
    , edsDestination608ChannelNumber
    , edsDestination708ServiceNumber

    -- * DeinterlacerMode
    , DeinterlacerMode (..)

    -- * DvbSubtitleBackgroundColor
    , DvbSubtitleBackgroundColor (..)

    -- * Hdr10Metadata
    , Hdr10Metadata (..)
    , mkHdr10Metadata
    , hmBluePrimaryX
    , hmBluePrimaryY
    , hmGreenPrimaryX
    , hmGreenPrimaryY
    , hmMaxContentLightLevel
    , hmMaxFrameAverageLightLevel
    , hmMaxLuminance
    , hmMinLuminance
    , hmRedPrimaryX
    , hmRedPrimaryY
    , hmWhitePointX
    , hmWhitePointY

    -- * JobSettings
    , JobSettings (..)
    , mkJobSettings
    , jsAdAvailOffset
    , jsAvailBlanking
    , jsEsam
    , jsInputs
    , jsMotionImageInserter
    , jsNielsenConfiguration
    , jsNielsenNonLinearWatermark
    , jsOutputGroups
    , jsTimecodeConfig
    , jsTimedMetadataInsertion

    -- * Ac3CodingMode
    , Ac3CodingMode (..)

    -- * DvbSubtitleFontColor
    , DvbSubtitleFontColor (..)

    -- * DashIsoPlaybackDeviceCompatibility
    , DashIsoPlaybackDeviceCompatibility (..)

    -- * CmafStreamInfResolution
    , CmafStreamInfResolution (..)

    -- * Eac3AtmosDynamicRangeCompressionLine
    , Eac3AtmosDynamicRangeCompressionLine (..)

    -- * AncillaryConvert608To708
    , AncillaryConvert608To708 (..)

    -- * ContainerSettings
    , ContainerSettings (..)
    , mkContainerSettings
    , csCmfcSettings
    , csContainer
    , csF4vSettings
    , csM2tsSettings
    , csM3u8Settings
    , csMovSettings
    , csMp4Settings
    , csMpdSettings
    , csMxfSettings

    -- * HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- * TimecodeConfig
    , TimecodeConfig (..)
    , mkTimecodeConfig
    , tcAnchor
    , tcSource
    , tcStart
    , tcTimestampOffset

    -- * EsamSignalProcessingNotification
    , EsamSignalProcessingNotification (..)
    , mkEsamSignalProcessingNotification
    , espnSccXml

    -- * Mpeg2TemporalAdaptiveQuantization
    , Mpeg2TemporalAdaptiveQuantization (..)

    -- * CmafCodecSpecification
    , CmafCodecSpecification (..)

    -- * Vp8QualityTuningLevel
    , Vp8QualityTuningLevel (..)

    -- * H264SlowPal
    , H264SlowPal (..)

    -- * Mp2Settings
    , Mp2Settings (..)
    , mkMp2Settings
    , msBitrate
    , msChannels
    , msSampleRate

    -- * H265WriteMp4PackagingType
    , H265WriteMp4PackagingType (..)

    -- * OutputSettings
    , OutputSettings (..)
    , mkOutputSettings
    , osHlsSettings

    -- * M2tsScte35Source
    , M2tsScte35Source (..)

    -- * AvcIntraClass
    , AvcIntraClass (..)

    -- * HlsAudioOnlyContainer
    , HlsAudioOnlyContainer (..)

    -- * SccDestinationSettings
    , SccDestinationSettings (..)
    , mkSccDestinationSettings
    , sdsFramerate

    -- * M2tsForceTsVideoEbpOrder
    , M2tsForceTsVideoEbpOrder (..)

    -- * Vp9Settings
    , Vp9Settings (..)
    , mkVp9Settings
    , vssBitrate
    , vssFramerateControl
    , vssFramerateConversionAlgorithm
    , vssFramerateDenominator
    , vssFramerateNumerator
    , vssGopSize
    , vssHrdBufferSize
    , vssMaxBitrate
    , vssParControl
    , vssParDenominator
    , vssParNumerator
    , vssQualityTuningLevel
    , vssRateControlMode

    -- * H264Settings
    , H264Settings (..)
    , mkH264Settings
    , hsAdaptiveQuantization
    , hsBitrate
    , hsCodecLevel
    , hsCodecProfile
    , hsDynamicSubGop
    , hsEntropyEncoding
    , hsFieldEncoding
    , hsFlickerAdaptiveQuantization
    , hsFramerateControl
    , hsFramerateConversionAlgorithm
    , hsFramerateDenominator
    , hsFramerateNumerator
    , hsGopBReference
    , hsGopClosedCadence
    , hsGopSize
    , hsGopSizeUnits
    , hsHrdBufferInitialFillPercentage
    , hsHrdBufferSize
    , hsInterlaceMode
    , hsMaxBitrate
    , hsMinIInterval
    , hsNumberBFramesBetweenReferenceFrames
    , hsNumberReferenceFrames
    , hsParControl
    , hsParDenominator
    , hsParNumerator
    , hsQualityTuningLevel
    , hsQvbrSettings
    , hsRateControlMode
    , hsRepeatPps
    , hsSceneChangeDetect
    , hsSlices
    , hsSlowPal
    , hsSoftness
    , hsSpatialAdaptiveQuantization
    , hsSyntax
    , hsTelecine
    , hsTemporalAdaptiveQuantization
    , hsUnregisteredSeiTimecode

    -- * M3u8PcrControl
    , M3u8PcrControl (..)

    -- * Preset
    , Preset (..)
    , mkPreset
    , pSettings
    , pName
    , pArn
    , pCategory
    , pCreatedAt
    , pDescription
    , pLastUpdated
    , pType

    -- * H265QualityTuningLevel
    , H265QualityTuningLevel (..)

    -- * NoiseReducerTemporalFilterSettings
    , NoiseReducerTemporalFilterSettings (..)
    , mkNoiseReducerTemporalFilterSettings
    , nrtfsAggressiveMode
    , nrtfsPostTemporalSharpening
    , nrtfsSpeed
    , nrtfsStrength

    -- * M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- * HlsGroupSettings
    , HlsGroupSettings (..)
    , mkHlsGroupSettings
    , hgsAdMarkers
    , hgsAdditionalManifests
    , hgsAudioOnlyHeader
    , hgsBaseUrl
    , hgsCaptionLanguageMappings
    , hgsCaptionLanguageSetting
    , hgsClientCache
    , hgsCodecSpecification
    , hgsDestination
    , hgsDestinationSettings
    , hgsDirectoryStructure
    , hgsEncryption
    , hgsManifestCompression
    , hgsManifestDurationFormat
    , hgsMinFinalSegmentLength
    , hgsMinSegmentLength
    , hgsOutputSelection
    , hgsProgramDateTime
    , hgsProgramDateTimePeriod
    , hgsSegmentControl
    , hgsSegmentLength
    , hgsSegmentsPerSubdirectory
    , hgsStreamInfResolution
    , hgsTimedMetadataId3Frame
    , hgsTimedMetadataId3Period
    , hgsTimestampDeltaMilliseconds

    -- * WavSettings
    , WavSettings (..)
    , mkWavSettings
    , wsBitDepth
    , wsChannels
    , wsFormat
    , wsSampleRate

    -- * MovClapAtom
    , MovClapAtom (..)

    -- * AvcIntraTelecine
    , AvcIntraTelecine (..)

    -- * HlsEncryptionSettings
    , HlsEncryptionSettings (..)
    , mkHlsEncryptionSettings
    , hesConstantInitializationVector
    , hesEncryptionMethod
    , hesInitializationVectorInManifest
    , hesOfflineEncrypted
    , hesSpekeKeyProvider
    , hesStaticKeyProvider
    , hesType

    -- * Mpeg2GopSizeUnits
    , Mpeg2GopSizeUnits (..)

    -- * BurninDestinationSettings
    , BurninDestinationSettings (..)
    , mkBurninDestinationSettings
    , bdsAlignment
    , bdsBackgroundColor
    , bdsBackgroundOpacity
    , bdsFontColor
    , bdsFontOpacity
    , bdsFontResolution
    , bdsFontScript
    , bdsFontSize
    , bdsOutlineColor
    , bdsOutlineSize
    , bdsShadowColor
    , bdsShadowOpacity
    , bdsShadowXOffset
    , bdsShadowYOffset
    , bdsTeletextSpacing
    , bdsXPosition
    , bdsYPosition

    -- * Eac3AtmosSettings
    , Eac3AtmosSettings (..)
    , mkEac3AtmosSettings
    , easBitrate
    , easBitstreamMode
    , easCodingMode
    , easDialogueIntelligence
    , easDynamicRangeCompressionLine
    , easDynamicRangeCompressionRf
    , easLoRoCenterMixLevel
    , easLoRoSurroundMixLevel
    , easLtRtCenterMixLevel
    , easLtRtSurroundMixLevel
    , easMeteringMode
    , easSampleRate
    , easSpeechThreshold
    , easStereoDownmix
    , easSurroundExMode

    -- * CmafSegmentControl
    , CmafSegmentControl (..)

    -- * AudioNormalizationPeakCalculation
    , AudioNormalizationPeakCalculation (..)

    -- * TimecodeBurninPosition
    , TimecodeBurninPosition (..)

    -- * BurninSubtitleOutlineColor
    , BurninSubtitleOutlineColor (..)

    -- * HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- * ProresInterlaceMode
    , ProresInterlaceMode (..)

    -- * Av1AdaptiveQuantization
    , Av1AdaptiveQuantization (..)

    -- * ImscDestinationSettings
    , ImscDestinationSettings (..)
    , mkImscDestinationSettings
    , idsStylePassthrough

    -- * HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- * MotionImageInsertionMode
    , MotionImageInsertionMode (..)

    -- * WatermarkingStrength
    , WatermarkingStrength (..)

    -- * DolbyVisionLevel6Metadata
    , DolbyVisionLevel6Metadata (..)
    , mkDolbyVisionLevel6Metadata
    , dvlmMaxCll
    , dvlmMaxFall

    -- * CmafWriteDASHManifest
    , CmafWriteDASHManifest (..)

    -- * H265InterlaceMode
    , H265InterlaceMode (..)

    -- * AudioCodecSettings
    , AudioCodecSettings (..)
    , mkAudioCodecSettings
    , acsAacSettings
    , acsAc3Settings
    , acsAiffSettings
    , acsCodec
    , acsEac3AtmosSettings
    , acsEac3Settings
    , acsMp2Settings
    , acsMp3Settings
    , acsOpusSettings
    , acsVorbisSettings
    , acsWavSettings

    -- * MovMpeg2FourCCControl
    , MovMpeg2FourCCControl (..)

    -- * AacVbrQuality
    , AacVbrQuality (..)

    -- * S3ObjectCannedAcl
    , S3ObjectCannedAcl (..)

    -- * MsSmoothManifestEncoding
    , MsSmoothManifestEncoding (..)

    -- * M2tsNielsenId3
    , M2tsNielsenId3 (..)

    -- * PresetSettings
    , PresetSettings (..)
    , mkPresetSettings
    , psAudioDescriptions
    , psCaptionDescriptions
    , psContainerSettings
    , psVideoDescription

    -- * Vc3Class
    , Vc3Class (..)

    -- * Mpeg2Settings
    , Mpeg2Settings (..)
    , mkMpeg2Settings
    , msfAdaptiveQuantization
    , msfBitrate
    , msfCodecLevel
    , msfCodecProfile
    , msfDynamicSubGop
    , msfFramerateControl
    , msfFramerateConversionAlgorithm
    , msfFramerateDenominator
    , msfFramerateNumerator
    , msfGopClosedCadence
    , msfGopSize
    , msfGopSizeUnits
    , msfHrdBufferInitialFillPercentage
    , msfHrdBufferSize
    , msfInterlaceMode
    , msfIntraDcPrecision
    , msfMaxBitrate
    , msfMinIInterval
    , msfNumberBFramesBetweenReferenceFrames
    , msfParControl
    , msfParDenominator
    , msfParNumerator
    , msfQualityTuningLevel
    , msfRateControlMode
    , msfSceneChangeDetect
    , msfSlowPal
    , msfSoftness
    , msfSpatialAdaptiveQuantization
    , msfSyntax
    , msfTelecine
    , msfTemporalAdaptiveQuantization

    -- * AutomatedEncodingSettings
    , AutomatedEncodingSettings (..)
    , mkAutomatedEncodingSettings
    , aesAbrSettings

    -- * MovCslgAtom
    , MovCslgAtom (..)

    -- * Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- * AudioCodec
    , AudioCodec (..)

    -- * Eac3AtmosDynamicRangeCompressionRf
    , Eac3AtmosDynamicRangeCompressionRf (..)

    -- * Eac3AtmosBitstreamMode
    , Eac3AtmosBitstreamMode (..)

    -- * Timing
    , Timing (..)
    , mkTiming
    , tFinishTime
    , tStartTime
    , tSubmitTime

    -- * Av1RateControlMode
    , Av1RateControlMode (..)

    -- * Type
    , Type (..)

    -- * OutputChannelMapping
    , OutputChannelMapping (..)
    , mkOutputChannelMapping
    , ocmInputChannels

    -- * CaptionDescription
    , CaptionDescription (..)
    , mkCaptionDescription
    , cdCaptionSelectorName
    , cdCustomLanguageCode
    , cdDestinationSettings
    , cdLanguageCode
    , cdLanguageDescription

    -- * CmafManifestCompression
    , CmafManifestCompression (..)

    -- * H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- * VideoDetail
    , VideoDetail (..)
    , mkVideoDetail
    , vdHeightInPx
    , vdWidthInPx

    -- * H265Telecine
    , H265Telecine (..)

    -- * OutputGroupType
    , OutputGroupType (..)

    -- * H265SpatialAdaptiveQuantization
    , H265SpatialAdaptiveQuantization (..)

    -- * InputClipping
    , InputClipping (..)
    , mkInputClipping
    , icEndTimecode
    , icStartTimecode

    -- * ColorMetadata
    , ColorMetadata (..)

    -- * DashIsoWriteSegmentTimelineInRepresentation
    , DashIsoWriteSegmentTimelineInRepresentation (..)

    -- * DashAdditionalManifest
    , DashAdditionalManifest (..)
    , mkDashAdditionalManifest
    , damManifestNameModifier
    , damSelectedOutputs

    -- * H265GopBReference
    , H265GopBReference (..)

    -- * ProresTelecine
    , ProresTelecine (..)

    -- * JobStatus
    , JobStatus (..)

    -- * CaptionSelector
    , CaptionSelector (..)
    , mkCaptionSelector
    , csCustomLanguageCode
    , csLanguageCode
    , csSourceSettings

    -- * BurninSubtitleShadowColor
    , BurninSubtitleShadowColor (..)

    -- * AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- * CmafMpdProfile
    , CmafMpdProfile (..)

    -- * MotionImageInsertionOffset
    , MotionImageInsertionOffset (..)
    , mkMotionImageInsertionOffset
    , miioImageX
    , miioImageY

    -- * Vp8Settings
    , Vp8Settings (..)
    , mkVp8Settings
    , vBitrate
    , vFramerateControl
    , vFramerateConversionAlgorithm
    , vFramerateDenominator
    , vFramerateNumerator
    , vGopSize
    , vHrdBufferSize
    , vMaxBitrate
    , vParControl
    , vParDenominator
    , vParNumerator
    , vQualityTuningLevel
    , vRateControlMode

    -- * Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eUrl

    -- * EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- * Eac3AtmosCodingMode
    , Eac3AtmosCodingMode (..)

    -- * Commitment
    , Commitment (..)

    -- * WavFormat
    , WavFormat (..)

    -- * AudioSelectorType
    , AudioSelectorType (..)

    -- * CaptionSourceType
    , CaptionSourceType (..)

    -- * Mp3Settings
    , Mp3Settings (..)
    , mkMp3Settings
    , mBitrate
    , mChannels
    , mRateControlMode
    , mSampleRate
    , mVbrQuality

    -- * DeinterlacerControl
    , DeinterlacerControl (..)

    -- * InputTimecodeSource
    , InputTimecodeSource (..)

    -- * Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- * Vc3Settings
    , Vc3Settings (..)
    , mkVc3Settings
    , vsFramerateControl
    , vsFramerateConversionAlgorithm
    , vsFramerateDenominator
    , vsFramerateNumerator
    , vsInterlaceMode
    , vsSlowPal
    , vsTelecine
    , vsVc3Class

    -- * Mpeg2RateControlMode
    , Mpeg2RateControlMode (..)

    -- * Vp9QualityTuningLevel
    , Vp9QualityTuningLevel (..)

    -- * Vc3FramerateConversionAlgorithm
    , Vc3FramerateConversionAlgorithm (..)

    -- * M2tsPcrControl
    , M2tsPcrControl (..)

    -- * InputTemplate
    , InputTemplate (..)
    , mkInputTemplate
    , itAudioSelectorGroups
    , itAudioSelectors
    , itCaptionSelectors
    , itCrop
    , itDeblockFilter
    , itDenoiseFilter
    , itFilterEnable
    , itFilterStrength
    , itImageInserter
    , itInputClippings
    , itInputScanType
    , itPosition
    , itProgramNumber
    , itPsiControl
    , itTimecodeSource
    , itTimecodeStart
    , itVideoSelector

    -- * AacSpecification
    , AacSpecification (..)

    -- * MotionImagePlayback
    , MotionImagePlayback (..)

    -- * BurninSubtitleFontColor
    , BurninSubtitleFontColor (..)

    -- * RemixSettings
    , RemixSettings (..)
    , mkRemixSettings
    , rsChannelMapping
    , rsChannelsIn
    , rsChannelsOut

    -- * HlsManifestCompression
    , HlsManifestCompression (..)

    -- * H265Tiles
    , H265Tiles (..)

    -- * DvbNitSettings
    , DvbNitSettings (..)
    , mkDvbNitSettings
    , dnsNetworkId
    , dnsNetworkName
    , dnsNitInterval

    -- * FileSourceConvert608To708
    , FileSourceConvert608To708 (..)

    -- * QueueTransition
    , QueueTransition (..)
    , mkQueueTransition
    , qtDestinationQueue
    , qtSourceQueue
    , qtTimestamp

    -- * StaticKeyProvider
    , StaticKeyProvider (..)
    , mkStaticKeyProvider
    , skpKeyFormat
    , skpKeyFormatVersions
    , skpStaticKeyValue
    , skpUrl

    -- * JobTemplateListBy
    , JobTemplateListBy (..)

    -- * H264RateControlMode
    , H264RateControlMode (..)

    -- * SpekeKeyProvider
    , SpekeKeyProvider (..)
    , mkSpekeKeyProvider
    , sCertificateArn
    , sResourceId
    , sSystemIds
    , sUrl

    -- * CaptionSourceSettings
    , CaptionSourceSettings (..)
    , mkCaptionSourceSettings
    , cssAncillarySourceSettings
    , cssDvbSubSourceSettings
    , cssEmbeddedSourceSettings
    , cssFileSourceSettings
    , cssSourceType
    , cssTeletextSourceSettings
    , cssTrackSourceSettings

    -- * M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- * M2tsEbpAudioInterval
    , M2tsEbpAudioInterval (..)

    -- * TeletextPageType
    , TeletextPageType (..)

    -- * DecryptionMode
    , DecryptionMode (..)

    -- * Order
    , Order (..)

    -- * ReservationPlanSettings
    , ReservationPlanSettings (..)
    , mkReservationPlanSettings
    , rpsCommitment
    , rpsReservedSlots
    , rpsRenewalType

    -- * OutputDetail
    , OutputDetail (..)
    , mkOutputDetail
    , odDurationInMs
    , odVideoDetails

    -- * Vp8FramerateControl
    , Vp8FramerateControl (..)

    -- * AccelerationStatus
    , AccelerationStatus (..)

    -- * Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- * VorbisSettings
    , VorbisSettings (..)
    , mkVorbisSettings
    , vsChannels
    , vsSampleRate
    , vsVbrQuality

    -- * AacSettings
    , AacSettings (..)
    , mkAacSettings
    , asAudioDescriptionBroadcasterMix
    , asBitrate
    , asCodecProfile
    , asCodingMode
    , asRateControlMode
    , asRawFormat
    , asSampleRate
    , asSpecification
    , asVbrQuality

    -- * PartnerWatermarking
    , PartnerWatermarking (..)
    , mkPartnerWatermarking
    , pwNexguardFileMarkerSettings

    -- * Ac3DynamicRangeCompressionProfile
    , Ac3DynamicRangeCompressionProfile (..)

    -- * RenewalType
    , RenewalType (..)

    -- * EmbeddedSourceSettings
    , EmbeddedSourceSettings (..)
    , mkEmbeddedSourceSettings
    , essConvert608To708
    , essSource608ChannelNumber
    , essSource608TrackNumber
    , essTerminateCaptions

    -- * Vc3FramerateControl
    , Vc3FramerateControl (..)

    -- * H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- * Vp8FramerateConversionAlgorithm
    , Vp8FramerateConversionAlgorithm (..)

    -- * Mpeg2AdaptiveQuantization
    , Mpeg2AdaptiveQuantization (..)

    -- * MsSmoothEncryptionSettings
    , MsSmoothEncryptionSettings (..)
    , mkMsSmoothEncryptionSettings
    , msesSpekeKeyProvider

    -- * F4vSettings
    , F4vSettings (..)
    , mkF4vSettings
    , fsMoovPlacement

    -- * DvbSubtitlingType
    , DvbSubtitlingType (..)

    -- * Eac3Settings
    , Eac3Settings (..)
    , mkEac3Settings
    , esAttenuationControl
    , esBitrate
    , esBitstreamMode
    , esCodingMode
    , esDcFilter
    , esDialnorm
    , esDynamicRangeCompressionLine
    , esDynamicRangeCompressionRf
    , esLfeControl
    , esLfeFilter
    , esLoRoCenterMixLevel
    , esLoRoSurroundMixLevel
    , esLtRtCenterMixLevel
    , esLtRtSurroundMixLevel
    , esMetadataControl
    , esPassthroughControl
    , esPhaseControl
    , esSampleRate
    , esStereoDownmix
    , esSurroundExMode
    , esSurroundMode

    -- * InputScanType
    , InputScanType (..)

    -- * Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- * AvcIntraSettings
    , AvcIntraSettings (..)
    , mkAvcIntraSettings
    , aisAvcIntraClass
    , aisFramerateControl
    , aisFramerateConversionAlgorithm
    , aisFramerateDenominator
    , aisFramerateNumerator
    , aisInterlaceMode
    , aisSlowPal
    , aisTelecine

    -- * Vp9RateControlMode
    , Vp9RateControlMode (..)

    -- * HlsAdditionalManifest
    , HlsAdditionalManifest (..)
    , mkHlsAdditionalManifest
    , hamManifestNameModifier
    , hamSelectedOutputs

    -- * DeinterlaceAlgorithm
    , DeinterlaceAlgorithm (..)

    -- * H264DynamicSubGop
    , H264DynamicSubGop (..)

    -- * NielsenActiveWatermarkProcessType
    , NielsenActiveWatermarkProcessType (..)

    -- * H264Telecine
    , H264Telecine (..)

    -- * Mpeg2QualityTuningLevel
    , Mpeg2QualityTuningLevel (..)

    -- * AudioTypeControl
    , AudioTypeControl (..)

    -- * DashIsoEncryptionSettings
    , DashIsoEncryptionSettings (..)
    , mkDashIsoEncryptionSettings
    , diesPlaybackDeviceCompatibility
    , diesSpekeKeyProvider

    -- * Mpeg2InterlaceMode
    , Mpeg2InterlaceMode (..)

    -- * H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- * ColorSpace
    , ColorSpace (..)

    -- * M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- * DashIsoGroupSettings
    , DashIsoGroupSettings (..)
    , mkDashIsoGroupSettings
    , digsAdditionalManifests
    , digsBaseUrl
    , digsDestination
    , digsDestinationSettings
    , digsEncryption
    , digsFragmentLength
    , digsHbbtvCompliance
    , digsMinBufferTime
    , digsMinFinalSegmentLength
    , digsMpdProfile
    , digsSegmentControl
    , digsSegmentLength
    , digsWriteSegmentTimelineInRepresentation

    -- * OutputGroupDetail
    , OutputGroupDetail (..)
    , mkOutputGroupDetail
    , ogdOutputDetails

    -- * S3DestinationSettings
    , S3DestinationSettings (..)
    , mkS3DestinationSettings
    , sdsAccessControl
    , sdsEncryption

    -- * AudioNormalizationLoudnessLogging
    , AudioNormalizationLoudnessLogging (..)

    -- * Rectangle
    , Rectangle (..)
    , mkRectangle
    , rHeight
    , rWidth
    , rX
    , rY

    -- * Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- * M2tsRateMode
    , M2tsRateMode (..)

    -- * MsSmoothAudioDeduplication
    , MsSmoothAudioDeduplication (..)

    -- * MpdAccessibilityCaptionHints
    , MpdAccessibilityCaptionHints (..)

    -- * MotionImageInsertionFramerate
    , MotionImageInsertionFramerate (..)
    , mkMotionImageInsertionFramerate
    , miifFramerateDenominator
    , miifFramerateNumerator

    -- * DvbTdtSettings
    , DvbTdtSettings (..)
    , mkDvbTdtSettings
    , dtsTdtInterval

    -- * S3ServerSideEncryptionType
    , S3ServerSideEncryptionType (..)

    -- * Eac3SurroundMode
    , Eac3SurroundMode (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.MediaConvert.Types.Mpeg2Syntax
  
import Network.AWS.MediaConvert.Types.Eac3PhaseControl
  
import Network.AWS.MediaConvert.Types.VideoSelector
  
import Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
  
import Network.AWS.MediaConvert.Types.ProresCodecProfile
  
import Network.AWS.MediaConvert.Types.Eac3SurroundExMode
  
import Network.AWS.MediaConvert.Types.H264ParControl
  
import Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
  
import Network.AWS.MediaConvert.Types.H265CodecProfile
  
import Network.AWS.MediaConvert.Types.FileGroupSettings
  
import Network.AWS.MediaConvert.Types.HlsStreamInfResolution
  
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
  
import Network.AWS.MediaConvert.Types.NoiseReducerFilter
  
import Network.AWS.MediaConvert.Types.HlsCodecSpecification
  
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough
  
import Network.AWS.MediaConvert.Types.H264InterlaceMode
  
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
  
import Network.AWS.MediaConvert.Types.H264RepeatPps
  
import Network.AWS.MediaConvert.Types.TimecodeBurnin
  
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
  
import Network.AWS.MediaConvert.Types.JobTemplate
  
import Network.AWS.MediaConvert.Types.EsamSettings
  
import Network.AWS.MediaConvert.Types.ContainerType
  
import Network.AWS.MediaConvert.Types.ReservationPlanStatus
  
import Network.AWS.MediaConvert.Types.M2tsBufferModel
  
import Network.AWS.MediaConvert.Types.Vp9FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
  
import Network.AWS.MediaConvert.Types.VideoCodec
  
import Network.AWS.MediaConvert.Types.AccelerationSettings
  
import Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
  
import Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.InputRotate
  
import Network.AWS.MediaConvert.Types.ChannelMapping
  
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
  
import Network.AWS.MediaConvert.Types.InputFilterEnable
  
import Network.AWS.MediaConvert.Types.CmafAdditionalManifest
  
import Network.AWS.MediaConvert.Types.JobMessages
  
import Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
  
import Network.AWS.MediaConvert.Types.Mpeg2SlowPal
  
import Network.AWS.MediaConvert.Types.MovReference
  
import Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
  
import Network.AWS.MediaConvert.Types.Vp9FramerateControl
  
import Network.AWS.MediaConvert.Types.LanguageCode
  
import Network.AWS.MediaConvert.Types.Vp8ParControl
  
import Network.AWS.MediaConvert.Types.VideoCodecSettings
  
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
  
import Network.AWS.MediaConvert.Types.MovPaddingControl
  
import Network.AWS.MediaConvert.Types.HlsClientCache
  
import Network.AWS.MediaConvert.Types.JobTemplateSettings
  
import Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
  
import Network.AWS.MediaConvert.Types.H264QvbrSettings
  
import Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
  
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
  
import Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
  
import Network.AWS.MediaConvert.Types.HlsSettings
  
import Network.AWS.MediaConvert.Types.InsertableImage
  
import Network.AWS.MediaConvert.Types.CaptionDestinationType
  
import Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
  
import Network.AWS.MediaConvert.Types.Vc3InterlaceMode
  
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
  
import Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
  
import Network.AWS.MediaConvert.Types.AiffSettings
  
import Network.AWS.MediaConvert.Types.TimecodeSource
  
import Network.AWS.MediaConvert.Types.QueueStatus
  
import Network.AWS.MediaConvert.Types.AfdSignaling
  
import Network.AWS.MediaConvert.Types.AacRateControlMode
  
import Network.AWS.MediaConvert.Types.ProresParControl
  
import Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
  
import Network.AWS.MediaConvert.Types.M2tsScte35Esam
  
import Network.AWS.MediaConvert.Types.OutputGroupSettings
  
import Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
  
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
  
import Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.Mpeg2FramerateControl
  
import Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
  
import Network.AWS.MediaConvert.Types.M2tsSettings
  
import Network.AWS.MediaConvert.Types.H264CodecProfile
  
import Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
  
import Network.AWS.MediaConvert.Types.AudioDefaultSelection
  
import Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
  
import Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
  
import Network.AWS.MediaConvert.Types.H265ParControl
  
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
  
import Network.AWS.MediaConvert.Types.Mp4CslgAtom
  
import Network.AWS.MediaConvert.Types.Mp3RateControlMode
  
import Network.AWS.MediaConvert.Types.H264FramerateControl
  
import Network.AWS.MediaConvert.Types.MpdCaptionContainerType
  
import Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode
  
import Network.AWS.MediaConvert.Types.H264EntropyEncoding
  
import Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
  
import Network.AWS.MediaConvert.Types.HlsOutputSelection
  
import Network.AWS.MediaConvert.Types.M2tsAudioDuration
  
import Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
  
import Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
  
import Network.AWS.MediaConvert.Types.H265DynamicSubGop
  
import Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
  
import Network.AWS.MediaConvert.Types.AlphaBehavior
  
import Network.AWS.MediaConvert.Types.QueueListBy
  
import Network.AWS.MediaConvert.Types.VideoPreprocessor
  
import Network.AWS.MediaConvert.Types.H264CodecLevel
  
import Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
  
import Network.AWS.MediaConvert.Types.AudioDescription
  
import Network.AWS.MediaConvert.Types.JobPhase
  
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings
  
import Network.AWS.MediaConvert.Types.Vp8RateControlMode
  
import Network.AWS.MediaConvert.Types.HlsEncryptionType
  
import Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.SccDestinationFramerate
  
import Network.AWS.MediaConvert.Types.MxfProfile
  
import Network.AWS.MediaConvert.Types.AacCodingMode
  
import Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
  
import Network.AWS.MediaConvert.Types.DolbyVision
  
import Network.AWS.MediaConvert.Types.HlsTimedMetadataId3Frame
  
import Network.AWS.MediaConvert.Types.OutputGroup
  
import Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
  
import Network.AWS.MediaConvert.Types.HopDestination
  
import Network.AWS.MediaConvert.Types.DolbyVisionProfile
  
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
  
import Network.AWS.MediaConvert.Types.AudioSelector
  
import Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.CmafGroupSettings
  
import Network.AWS.MediaConvert.Types.CmafEncryptionSettings
  
import Network.AWS.MediaConvert.Types.H265SlowPal
  
import Network.AWS.MediaConvert.Types.Ac3MetadataControl
  
import Network.AWS.MediaConvert.Types.H265TemporalIds
  
import Network.AWS.MediaConvert.Types.InputPsiControl
  
import Network.AWS.MediaConvert.Types.M3u8AudioDuration
  
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
  
import Network.AWS.MediaConvert.Types.M3u8NielsenId3
  
import Network.AWS.MediaConvert.Types.Vp9ParControl
  
import Network.AWS.MediaConvert.Types.ReservationPlan
  
import Network.AWS.MediaConvert.Types.PricingPlan
  
import Network.AWS.MediaConvert.Types.MpdScte35Source
  
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
  
import Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
  
import Network.AWS.MediaConvert.Types.ResourceTags
  
import Network.AWS.MediaConvert.Types.HlsDirectoryStructure
  
import Network.AWS.MediaConvert.Types.ProresSlowPal
  
import Network.AWS.MediaConvert.Types.Av1Settings
  
import Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
  
import Network.AWS.MediaConvert.Types.HlsSegmentControl
  
import Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
  
import Network.AWS.MediaConvert.Types.H265QvbrSettings
  
import Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
  
import Network.AWS.MediaConvert.Types.Eac3DcFilter
  
import Network.AWS.MediaConvert.Types.SimulateReservedQueue
  
import Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
  
import Network.AWS.MediaConvert.Types.HlsAdMarkers
  
import Network.AWS.MediaConvert.Types.Mp4MoovPlacement
  
import Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
  
import Network.AWS.MediaConvert.Types.InputDeblockFilter
  
import Network.AWS.MediaConvert.Types.HlsAudioTrackType
  
import Network.AWS.MediaConvert.Types.CmfcScte35Esam
  
import Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
  
import Network.AWS.MediaConvert.Types.DescribeEndpointsMode
  
import Network.AWS.MediaConvert.Types.CmafEncryptionType
  
import Network.AWS.MediaConvert.Types.AacCodecProfile
  
import Network.AWS.MediaConvert.Types.NielsenConfiguration
  
import Network.AWS.MediaConvert.Types.ColorCorrector
  
import Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
  
import Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
  
import Network.AWS.MediaConvert.Types.AvcIntraSlowPal
  
import Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.Input
  
import Network.AWS.MediaConvert.Types.MxfSettings
  
import Network.AWS.MediaConvert.Types.DvbSdtSettings
  
import Network.AWS.MediaConvert.Types.MxfAfdSignaling
  
import Network.AWS.MediaConvert.Types.FileSourceSettings
  
import Network.AWS.MediaConvert.Types.M3u8Settings
  
import Network.AWS.MediaConvert.Types.Eac3MetadataControl
  
import Network.AWS.MediaConvert.Types.AudioNormalizationSettings
  
import Network.AWS.MediaConvert.Types.CmfcSettings
  
import Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
  
import Network.AWS.MediaConvert.Types.Av1FramerateControl
  
import Network.AWS.MediaConvert.Types.InputDecryptionSettings
  
import Network.AWS.MediaConvert.Types.F4vMoovPlacement
  
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
  
import Network.AWS.MediaConvert.Types.ImscStylePassthrough
  
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
  
import Network.AWS.MediaConvert.Types.AvailBlanking
  
import Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
  
import Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.H265CodecLevel
  
import Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
  
import Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.DestinationSettings
  
import Network.AWS.MediaConvert.Types.AccelerationMode
  
import Network.AWS.MediaConvert.Types.M3u8Scte35Source
  
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
  
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
  
import Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
  
import Network.AWS.MediaConvert.Types.H264Syntax
  
  
import Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
  
import Network.AWS.MediaConvert.Types.Queue
  
import Network.AWS.MediaConvert.Types.ScalingBehavior
  
import Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
  
import Network.AWS.MediaConvert.Types.ProresFramerateControl
  
import Network.AWS.MediaConvert.Types.H264GopBReference
  
import Network.AWS.MediaConvert.Types.Id3Insertion
  
import Network.AWS.MediaConvert.Types.HlsKeyProviderType
  
import Network.AWS.MediaConvert.Types.MovSettings
  
import Network.AWS.MediaConvert.Types.RespondToAfd
  
import Network.AWS.MediaConvert.Types.MotionImageInserter
  
import Network.AWS.MediaConvert.Types.MpdAudioDuration
  
import Network.AWS.MediaConvert.Types.CmfcScte35Source
  
import Network.AWS.MediaConvert.Types.Mpeg2ParControl
  
import Network.AWS.MediaConvert.Types.H265FramerateControl
  
import Network.AWS.MediaConvert.Types.InputDenoiseFilter
  
import Network.AWS.MediaConvert.Types.ColorSpaceConversion
  
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.H264FieldEncoding
  
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
  
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
  
  
import Network.AWS.MediaConvert.Types.Ac3Settings
  
import Network.AWS.MediaConvert.Types.MpdScte35Esam
  
import Network.AWS.MediaConvert.Types.Mpeg2Telecine
  
import Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
  
import Network.AWS.MediaConvert.Types.Eac3BitstreamMode
  
import Network.AWS.MediaConvert.Types.ColorSpaceUsage
  
import Network.AWS.MediaConvert.Types.AudioSelectorGroup
  
import Network.AWS.MediaConvert.Types.AacRawFormat
  
import Network.AWS.MediaConvert.Types.H265Settings
  
import Network.AWS.MediaConvert.Types.CmafClientCache
  
import Network.AWS.MediaConvert.Types.Deinterlacer
  
import Network.AWS.MediaConvert.Types.Mp4Settings
  
import Network.AWS.MediaConvert.Types.DropFrameTimecode
  
import Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
  
import Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
  
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
  
import Network.AWS.MediaConvert.Types.PresetListBy
  
import Network.AWS.MediaConvert.Types.AntiAlias
  
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
  
import Network.AWS.MediaConvert.Types.OpusSettings
  
import Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.FontScript
  
import Network.AWS.MediaConvert.Types.MpdSettings
  
  
import Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
  
import Network.AWS.MediaConvert.Types.H264QualityTuningLevel
  
import Network.AWS.MediaConvert.Types.ProresSettings
  
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
  
import Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
  
import Network.AWS.MediaConvert.Types.OutputSdt
  
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
  
import Network.AWS.MediaConvert.Types.Vc3SlowPal
  
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
  
import Network.AWS.MediaConvert.Types.NoiseReducer
  
import Network.AWS.MediaConvert.Types.TimedMetadata
  
import Network.AWS.MediaConvert.Types.Eac3LfeControl
  
import Network.AWS.MediaConvert.Types.Output
  
import Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.Vc3Telecine
  
import Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
  
import Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
  
import Network.AWS.MediaConvert.Types.Job
  
import Network.AWS.MediaConvert.Types.ImageInserter
  
import Network.AWS.MediaConvert.Types.TrackSourceSettings
  
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
  
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
  
import Network.AWS.MediaConvert.Types.BillingTagsSource
  
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
  
import Network.AWS.MediaConvert.Types.AudioChannelTag
  
import Network.AWS.MediaConvert.Types.H265RateControlMode
  
import Network.AWS.MediaConvert.Types.CmafKeyProviderType
  
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
  
import Network.AWS.MediaConvert.Types.Eac3CodingMode
  
import Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
  
import Network.AWS.MediaConvert.Types.VideoDescription
  
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
  
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
  
import Network.AWS.MediaConvert.Types.DeinterlacerMode
  
import Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
  
import Network.AWS.MediaConvert.Types.Hdr10Metadata
  
import Network.AWS.MediaConvert.Types.JobSettings
  
import Network.AWS.MediaConvert.Types.Ac3CodingMode
  
import Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
  
import Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
  
import Network.AWS.MediaConvert.Types.CmafStreamInfResolution
  
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
  
  
import Network.AWS.MediaConvert.Types.AncillaryConvert608To708
  
import Network.AWS.MediaConvert.Types.ContainerSettings
  
import Network.AWS.MediaConvert.Types.HlsProgramDateTime
  
import Network.AWS.MediaConvert.Types.TimecodeConfig
  
import Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
  
import Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.CmafCodecSpecification
  
import Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
  
import Network.AWS.MediaConvert.Types.H264SlowPal
  
import Network.AWS.MediaConvert.Types.Mp2Settings
  
import Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
  
import Network.AWS.MediaConvert.Types.OutputSettings
  
import Network.AWS.MediaConvert.Types.M2tsScte35Source
  
import Network.AWS.MediaConvert.Types.AvcIntraClass
  
import Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
  
import Network.AWS.MediaConvert.Types.SccDestinationSettings
  
import Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
  
import Network.AWS.MediaConvert.Types.Vp9Settings
  
import Network.AWS.MediaConvert.Types.H264Settings
  
import Network.AWS.MediaConvert.Types.M3u8PcrControl
  
import Network.AWS.MediaConvert.Types.Preset
  
import Network.AWS.MediaConvert.Types.H265QualityTuningLevel
  
import Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
  
import Network.AWS.MediaConvert.Types.M2tsEsRateInPes
  
import Network.AWS.MediaConvert.Types.HlsGroupSettings
  
import Network.AWS.MediaConvert.Types.WavSettings
  
import Network.AWS.MediaConvert.Types.MovClapAtom
  
import Network.AWS.MediaConvert.Types.AvcIntraTelecine
  
  
import Network.AWS.MediaConvert.Types.HlsEncryptionSettings
  
import Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
  
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
  
import Network.AWS.MediaConvert.Types.Eac3AtmosSettings
  
import Network.AWS.MediaConvert.Types.CmafSegmentControl
  
import Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
  
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition
  
import Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
  
import Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
  
import Network.AWS.MediaConvert.Types.ProresInterlaceMode
  
import Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
  
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
  
import Network.AWS.MediaConvert.Types.MotionImageInsertionMode
  
import Network.AWS.MediaConvert.Types.WatermarkingStrength
  
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
  
import Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
  
import Network.AWS.MediaConvert.Types.H265InterlaceMode
  
import Network.AWS.MediaConvert.Types.AudioCodecSettings
  
import Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
  
import Network.AWS.MediaConvert.Types.AacVbrQuality
  
import Network.AWS.MediaConvert.Types.S3ObjectCannedAcl
  
import Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
  
import Network.AWS.MediaConvert.Types.M2tsNielsenId3
  
import Network.AWS.MediaConvert.Types.PresetSettings
  
import Network.AWS.MediaConvert.Types.Vc3Class
  
import Network.AWS.MediaConvert.Types.Mpeg2Settings
  
import Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
  
import Network.AWS.MediaConvert.Types.MovCslgAtom
  
import Network.AWS.MediaConvert.Types.Eac3StereoDownmix
  
import Network.AWS.MediaConvert.Types.AudioCodec
  
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
  
import Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
  
import Network.AWS.MediaConvert.Types.Timing
  
import Network.AWS.MediaConvert.Types.Av1RateControlMode
  
import Network.AWS.MediaConvert.Types.Type
  
import Network.AWS.MediaConvert.Types.OutputChannelMapping
  
import Network.AWS.MediaConvert.Types.CaptionDescription
  
import Network.AWS.MediaConvert.Types.CmafManifestCompression
  
import Network.AWS.MediaConvert.Types.H264GopSizeUnits
  
import Network.AWS.MediaConvert.Types.VideoDetail
  
import Network.AWS.MediaConvert.Types.H265Telecine
  
import Network.AWS.MediaConvert.Types.OutputGroupType
  
import Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.InputClipping
  
import Network.AWS.MediaConvert.Types.ColorMetadata
  
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
  
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
  
import Network.AWS.MediaConvert.Types.H265GopBReference
  
import Network.AWS.MediaConvert.Types.ProresTelecine
  
import Network.AWS.MediaConvert.Types.JobStatus
  
import Network.AWS.MediaConvert.Types.CaptionSelector
  
import Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
  
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
  
import Network.AWS.MediaConvert.Types.CmafMpdProfile
  
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
  
import Network.AWS.MediaConvert.Types.Vp8Settings
  
import Network.AWS.MediaConvert.Types.Endpoint
  
import Network.AWS.MediaConvert.Types.EmbeddedConvert608To708
  
import Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
  
import Network.AWS.MediaConvert.Types.Commitment
  
import Network.AWS.MediaConvert.Types.WavFormat
  
import Network.AWS.MediaConvert.Types.AudioSelectorType
  
import Network.AWS.MediaConvert.Types.CaptionSourceType
  
import Network.AWS.MediaConvert.Types.Mp3Settings
  
import Network.AWS.MediaConvert.Types.DeinterlacerControl
  
import Network.AWS.MediaConvert.Types.InputTimecodeSource
  
import Network.AWS.MediaConvert.Types.Eac3LfeFilter
  
import Network.AWS.MediaConvert.Types.Vc3Settings
  
import Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
  
import Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
  
import Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.M2tsPcrControl
  
import Network.AWS.MediaConvert.Types.InputTemplate
  
import Network.AWS.MediaConvert.Types.AacSpecification
  
import Network.AWS.MediaConvert.Types.MotionImagePlayback
  
import Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
  
import Network.AWS.MediaConvert.Types.RemixSettings
  
import Network.AWS.MediaConvert.Types.HlsManifestCompression
  
import Network.AWS.MediaConvert.Types.H265Tiles
  
import Network.AWS.MediaConvert.Types.DvbNitSettings
  
import Network.AWS.MediaConvert.Types.FileSourceConvert608To708
  
import Network.AWS.MediaConvert.Types.QueueTransition
  
import Network.AWS.MediaConvert.Types.StaticKeyProvider
  
import Network.AWS.MediaConvert.Types.JobTemplateListBy
  
import Network.AWS.MediaConvert.Types.H264RateControlMode
  
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
  
import Network.AWS.MediaConvert.Types.CaptionSourceSettings
  
import Network.AWS.MediaConvert.Types.M2tsEbpPlacement
  
import Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
  
import Network.AWS.MediaConvert.Types.TeletextPageType
  
import Network.AWS.MediaConvert.Types.DecryptionMode
  
import Network.AWS.MediaConvert.Types.Order
  
import Network.AWS.MediaConvert.Types.ReservationPlanSettings
  
import Network.AWS.MediaConvert.Types.OutputDetail
  
import Network.AWS.MediaConvert.Types.Vp8FramerateControl
  
import Network.AWS.MediaConvert.Types.AccelerationStatus
  
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
  
import Network.AWS.MediaConvert.Types.VorbisSettings
  
import Network.AWS.MediaConvert.Types.AacSettings
  
import Network.AWS.MediaConvert.Types.PartnerWatermarking
  
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
  
import Network.AWS.MediaConvert.Types.RenewalType
  
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
  
import Network.AWS.MediaConvert.Types.Vc3FramerateControl
  
import Network.AWS.MediaConvert.Types.H265SceneChangeDetect
  
import Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
  
import Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
  
import Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
  
import Network.AWS.MediaConvert.Types.F4vSettings
  
import Network.AWS.MediaConvert.Types.DvbSubtitlingType
  
import Network.AWS.MediaConvert.Types.Eac3Settings
  
import Network.AWS.MediaConvert.Types.InputScanType
  
import Network.AWS.MediaConvert.Types.Eac3AttenuationControl
  
import Network.AWS.MediaConvert.Types.AvcIntraSettings
  
import Network.AWS.MediaConvert.Types.Vp9RateControlMode
  
import Network.AWS.MediaConvert.Types.HlsAdditionalManifest
  
import Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
  
import Network.AWS.MediaConvert.Types.H264DynamicSubGop
  
import Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
  
import Network.AWS.MediaConvert.Types.H264Telecine
  
import Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
  
  
import Network.AWS.MediaConvert.Types.AudioTypeControl
  
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
  
import Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
  
import Network.AWS.MediaConvert.Types.H265GopSizeUnits
  
import Network.AWS.MediaConvert.Types.ColorSpace
  
import Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
  
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
  
import Network.AWS.MediaConvert.Types.OutputGroupDetail
  
import Network.AWS.MediaConvert.Types.S3DestinationSettings
  
import Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
  
import Network.AWS.MediaConvert.Types.Rectangle
  
import Network.AWS.MediaConvert.Types.Eac3PassthroughControl
  
import Network.AWS.MediaConvert.Types.M2tsRateMode
  
import Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
  
import Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
  
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
  
import Network.AWS.MediaConvert.Types.DvbTdtSettings
  
import Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
  
import Network.AWS.MediaConvert.Types.Eac3SurroundMode
  

-- | API version @2017-08-29@ of the Amazon Elemental MediaConvert SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MediaConvert",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "mediaconvert",
                 Core._svcVersion = "2017-08-29", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MediaConvert",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The service couldn't complete your request because there is a conflict with the current state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | You don't have permissions for this action with the credentials you sent.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException
  = Core._MatchServiceError mkServiceConfig "ForbiddenException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _ForbiddenException #-}
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead"  #-}

-- | The resource you requested doesn't exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Too many requests have been sent in too short of a time. The service limits the rate at which it will accept requests.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | The service encountered an unexpected condition and can't fulfill your request.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException
  = Core._MatchServiceError mkServiceConfig
      "InternalServerErrorException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServerErrorException #-}
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead"  #-}

-- | The service can't process your request because of a problem in the request. Please check your request form and syntax.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}
