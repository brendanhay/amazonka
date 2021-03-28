{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

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

    -- ** Mpeg2Syntax
    , Mpeg2Syntax (..)

    -- ** Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- ** VideoSelector
    , VideoSelector (..)
    , mkVideoSelector
    , vsAlphaBehavior
    , vsColorSpace
    , vsColorSpaceUsage
    , vsHdr10Metadata
    , vsPid
    , vsProgramNumber
    , vsRotate

    -- ** AvcIntraFramerateControl
    , AvcIntraFramerateControl (..)

    -- ** ProresCodecProfile
    , ProresCodecProfile (..)

    -- ** Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- ** H264ParControl
    , H264ParControl (..)

    -- ** AvcIntraFramerateConversionAlgorithm
    , AvcIntraFramerateConversionAlgorithm (..)

    -- ** Mpeg2DynamicSubGop
    , Mpeg2DynamicSubGop (..)

    -- ** H265CodecProfile
    , H265CodecProfile (..)

    -- ** FileGroupSettings
    , FileGroupSettings (..)
    , mkFileGroupSettings
    , fgsDestination
    , fgsDestinationSettings

    -- ** HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- ** TeletextDestinationSettings
    , TeletextDestinationSettings (..)
    , mkTeletextDestinationSettings
    , tdsPageNumber
    , tdsPageTypes

    -- ** NoiseReducerFilter
    , NoiseReducerFilter (..)

    -- ** HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- ** TtmlStylePassthrough
    , TtmlStylePassthrough (..)

    -- ** H264InterlaceMode
    , H264InterlaceMode (..)

    -- ** FrameCaptureSettings
    , FrameCaptureSettings (..)
    , mkFrameCaptureSettings
    , fcsFramerateDenominator
    , fcsFramerateNumerator
    , fcsMaxCaptures
    , fcsQuality

    -- ** H264RepeatPps
    , H264RepeatPps (..)

    -- ** TimecodeBurnin
    , TimecodeBurnin (..)
    , mkTimecodeBurnin
    , tbFontSize
    , tbPosition
    , tbPrefix

    -- ** NexGuardFileMarkerSettings
    , NexGuardFileMarkerSettings (..)
    , mkNexGuardFileMarkerSettings
    , ngfmsLicense
    , ngfmsPayload
    , ngfmsPreset
    , ngfmsStrength

    -- ** JobTemplate
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

    -- ** EsamSettings
    , EsamSettings (..)
    , mkEsamSettings
    , esManifestConfirmConditionNotification
    , esResponseSignalPreroll
    , esSignalProcessingNotification

    -- ** ContainerType
    , ContainerType (..)

    -- ** ReservationPlanStatus
    , ReservationPlanStatus (..)

    -- ** M2tsBufferModel
    , M2tsBufferModel (..)

    -- ** Vp9FramerateConversionAlgorithm
    , Vp9FramerateConversionAlgorithm (..)

    -- ** NielsenSourceWatermarkStatusType
    , NielsenSourceWatermarkStatusType (..)

    -- ** VideoCodec
    , VideoCodec (..)

    -- ** AccelerationSettings
    , AccelerationSettings (..)
    , mkAccelerationSettings
    , asMode

    -- ** H265AlternateTransferFunctionSei
    , H265AlternateTransferFunctionSei (..)

    -- ** H264FlickerAdaptiveQuantization
    , H264FlickerAdaptiveQuantization (..)

    -- ** InputRotate
    , InputRotate (..)

    -- ** ChannelMapping
    , ChannelMapping (..)
    , mkChannelMapping
    , cmOutputChannels

    -- ** DashIsoSegmentControl
    , DashIsoSegmentControl (..)

    -- ** InputFilterEnable
    , InputFilterEnable (..)

    -- ** CmafAdditionalManifest
    , CmafAdditionalManifest (..)
    , mkCmafAdditionalManifest
    , camManifestNameModifier
    , camSelectedOutputs

    -- ** JobMessages
    , JobMessages (..)
    , mkJobMessages
    , jmInfo
    , jmWarning

    -- ** DvbSubtitleShadowColor
    , DvbSubtitleShadowColor (..)

    -- ** Mpeg2SlowPal
    , Mpeg2SlowPal (..)

    -- ** MovReference
    , MovReference (..)

    -- ** AacAudioDescriptionBroadcasterMix
    , AacAudioDescriptionBroadcasterMix (..)

    -- ** Vp9FramerateControl
    , Vp9FramerateControl (..)

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** Vp8ParControl
    , Vp8ParControl (..)

    -- ** VideoCodecSettings
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

    -- ** S3EncryptionSettings
    , S3EncryptionSettings (..)
    , mkS3EncryptionSettings
    , sesEncryptionType
    , sesKmsKeyArn

    -- ** MovPaddingControl
    , MovPaddingControl (..)

    -- ** HlsClientCache
    , HlsClientCache (..)

    -- ** JobTemplateSettings
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

    -- ** HlsAudioOnlyHeader
    , HlsAudioOnlyHeader (..)

    -- ** H264QvbrSettings
    , H264QvbrSettings (..)
    , mkH264QvbrSettings
    , hqsMaxAverageBitrate
    , hqsQvbrQualityLevel
    , hqsQvbrQualityLevelFineTune

    -- ** CmafWriteSegmentTimelineInRepresentation
    , CmafWriteSegmentTimelineInRepresentation (..)

    -- ** AutomatedAbrSettings
    , AutomatedAbrSettings (..)
    , mkAutomatedAbrSettings
    , aasMaxAbrBitrate
    , aasMaxRenditions
    , aasMinAbrBitrate

    -- ** H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- ** Mpeg2SceneChangeDetect
    , Mpeg2SceneChangeDetect (..)

    -- ** HlsSettings
    , HlsSettings (..)
    , mkHlsSettings
    , hsAudioGroupId
    , hsAudioOnlyContainer
    , hsAudioRenditionSets
    , hsAudioTrackType
    , hsIFrameOnlyManifest
    , hsSegmentModifier

    -- ** InsertableImage
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

    -- ** CaptionDestinationType
    , CaptionDestinationType (..)

    -- ** MsSmoothAdditionalManifest
    , MsSmoothAdditionalManifest (..)
    , mkMsSmoothAdditionalManifest
    , msamManifestNameModifier
    , msamSelectedOutputs

    -- ** Vc3InterlaceMode
    , Vc3InterlaceMode (..)

    -- ** StatusUpdateInterval
    , StatusUpdateInterval (..)

    -- ** EmbeddedTerminateCaptions
    , EmbeddedTerminateCaptions (..)

    -- ** AiffSettings
    , AiffSettings (..)
    , mkAiffSettings
    , assBitDepth
    , assChannels
    , assSampleRate

    -- ** TimecodeSource
    , TimecodeSource (..)

    -- ** QueueStatus
    , QueueStatus (..)

    -- ** AfdSignaling
    , AfdSignaling (..)

    -- ** AacRateControlMode
    , AacRateControlMode (..)

    -- ** ProresParControl
    , ProresParControl (..)

    -- ** AudioChannelTaggingSettings
    , AudioChannelTaggingSettings (..)
    , mkAudioChannelTaggingSettings
    , actsChannelTag

    -- ** M2tsScte35Esam
    , M2tsScte35Esam (..)
    , mkM2tsScte35Esam
    , mseScte35EsamPid

    -- ** OutputGroupSettings
    , OutputGroupSettings (..)
    , mkOutputGroupSettings
    , ogsCmafGroupSettings
    , ogsDashIsoGroupSettings
    , ogsFileGroupSettings
    , ogsHlsGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsType

    -- ** Eac3AtmosDialogueIntelligence
    , Eac3AtmosDialogueIntelligence (..)

    -- ** TeletextSourceSettings
    , TeletextSourceSettings (..)
    , mkTeletextSourceSettings
    , tssPageNumber

    -- ** Mpeg2FramerateConversionAlgorithm
    , Mpeg2FramerateConversionAlgorithm (..)

    -- ** Mpeg2FramerateControl
    , Mpeg2FramerateControl (..)

    -- ** CaptionDescriptionPreset
    , CaptionDescriptionPreset (..)
    , mkCaptionDescriptionPreset
    , cdpCustomLanguageCode
    , cdpDestinationSettings
    , cdpLanguageCode
    , cdpLanguageDescription

    -- ** M2tsSettings
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

    -- ** H264CodecProfile
    , H264CodecProfile (..)

    -- ** Mpeg2CodecLevel
    , Mpeg2CodecLevel (..)

    -- ** AudioDefaultSelection
    , AudioDefaultSelection (..)

    -- ** NielsenUniqueTicPerAudioTrackType
    , NielsenUniqueTicPerAudioTrackType (..)

    -- ** H265UnregisteredSeiTimecode
    , H265UnregisteredSeiTimecode (..)

    -- ** H265ParControl
    , H265ParControl (..)

    -- ** DvbSubDestinationSettings
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

    -- ** Mp4CslgAtom
    , Mp4CslgAtom (..)

    -- ** Mp3RateControlMode
    , Mp3RateControlMode (..)

    -- ** H264FramerateControl
    , H264FramerateControl (..)

    -- ** MpdCaptionContainerType
    , MpdCaptionContainerType (..)

    -- ** AvcIntraInterlaceMode
    , AvcIntraInterlaceMode (..)

    -- ** H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- ** HlsOfflineEncrypted
    , HlsOfflineEncrypted (..)

    -- ** HlsOutputSelection
    , HlsOutputSelection (..)

    -- ** M2tsAudioDuration
    , M2tsAudioDuration (..)

    -- ** Eac3AtmosSurroundExMode
    , Eac3AtmosSurroundExMode (..)

    -- ** Mpeg2CodecProfile
    , Mpeg2CodecProfile (..)

    -- ** H265DynamicSubGop
    , H265DynamicSubGop (..)

    -- ** CmafInitializationVectorInManifest
    , CmafInitializationVectorInManifest (..)

    -- ** AlphaBehavior
    , AlphaBehavior (..)

    -- ** QueueListBy
    , QueueListBy (..)

    -- ** VideoPreprocessor
    , VideoPreprocessor (..)
    , mkVideoPreprocessor
    , vpColorCorrector
    , vpDeinterlacer
    , vpDolbyVision
    , vpImageInserter
    , vpNoiseReducer
    , vpPartnerWatermarking
    , vpTimecodeBurnin

    -- ** H264CodecLevel
    , H264CodecLevel (..)

    -- ** DvbSubtitleTeletextSpacing
    , DvbSubtitleTeletextSpacing (..)

    -- ** AudioDescription
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

    -- ** JobPhase
    , JobPhase (..)

    -- ** TtmlDestinationSettings
    , TtmlDestinationSettings (..)
    , mkTtmlDestinationSettings
    , tdsStylePassthrough

    -- ** Vp8RateControlMode
    , Vp8RateControlMode (..)

    -- ** HlsEncryptionType
    , HlsEncryptionType (..)

    -- ** H264FramerateConversionAlgorithm
    , H264FramerateConversionAlgorithm (..)

    -- ** SccDestinationFramerate
    , SccDestinationFramerate (..)

    -- ** MxfProfile
    , MxfProfile (..)

    -- ** AacCodingMode
    , AacCodingMode (..)

    -- ** Eac3AtmosMeteringMode
    , Eac3AtmosMeteringMode (..)

    -- ** DolbyVision
    , DolbyVision (..)
    , mkDolbyVision
    , dvL6Metadata
    , dvL6Mode
    , dvProfile

    -- ** HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- ** OutputGroup
    , OutputGroup (..)
    , mkOutputGroup
    , ogAutomatedEncodingSettings
    , ogCustomName
    , ogName
    , ogOutputGroupSettings
    , ogOutputs

    -- ** HlsIFrameOnlyManifest
    , HlsIFrameOnlyManifest (..)

    -- ** HopDestination
    , HopDestination (..)
    , mkHopDestination
    , hdPriority
    , hdQueue
    , hdWaitMinutes

    -- ** DolbyVisionProfile
    , DolbyVisionProfile (..)

    -- ** CmfcAudioDuration
    , CmfcAudioDuration (..)

    -- ** AudioSelector
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

    -- ** H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- ** CmafGroupSettings
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

    -- ** CmafEncryptionSettings
    , CmafEncryptionSettings (..)
    , mkCmafEncryptionSettings
    , cesConstantInitializationVector
    , cesEncryptionMethod
    , cesInitializationVectorInManifest
    , cesSpekeKeyProvider
    , cesStaticKeyProvider
    , cesType

    -- ** H265SlowPal
    , H265SlowPal (..)

    -- ** Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- ** H265TemporalIds
    , H265TemporalIds (..)

    -- ** InputPsiControl
    , InputPsiControl (..)

    -- ** M3u8AudioDuration
    , M3u8AudioDuration (..)

    -- ** TimedMetadataInsertion
    , TimedMetadataInsertion (..)
    , mkTimedMetadataInsertion
    , tmiId3Insertions

    -- ** M3u8NielsenId3
    , M3u8NielsenId3 (..)

    -- ** Vp9ParControl
    , Vp9ParControl (..)

    -- ** ReservationPlan
    , ReservationPlan (..)
    , mkReservationPlan
    , rpCommitment
    , rpExpiresAt
    , rpPurchasedAt
    , rpRenewalType
    , rpReservedSlots
    , rpStatus

    -- ** PricingPlan
    , PricingPlan (..)

    -- ** MpdScte35Source
    , MpdScte35Source (..)

    -- ** DolbyVisionLevel6Mode
    , DolbyVisionLevel6Mode (..)

    -- ** BurninSubtitleBackgroundColor
    , BurninSubtitleBackgroundColor (..)

    -- ** ResourceTags
    , ResourceTags (..)
    , mkResourceTags
    , rtArn
    , rtTags

    -- ** HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- ** ProresSlowPal
    , ProresSlowPal (..)

    -- ** Av1Settings
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

    -- ** AncillaryTerminateCaptions
    , AncillaryTerminateCaptions (..)

    -- ** HlsSegmentControl
    , HlsSegmentControl (..)

    -- ** NoiseReducerSpatialFilterSettings
    , NoiseReducerSpatialFilterSettings (..)
    , mkNoiseReducerSpatialFilterSettings
    , nrsfsPostFilterSharpenStrength
    , nrsfsSpeed
    , nrsfsStrength

    -- ** H265QvbrSettings
    , H265QvbrSettings (..)
    , mkH265QvbrSettings
    , hMaxAverageBitrate
    , hQvbrQualityLevel
    , hQvbrQualityLevelFineTune

    -- ** H265SampleAdaptiveOffsetFilterMode
    , H265SampleAdaptiveOffsetFilterMode (..)

    -- ** Eac3DcFilter
    , Eac3DcFilter (..)

    -- ** SimulateReservedQueue
    , SimulateReservedQueue (..)

    -- ** CmafManifestDurationFormat
    , CmafManifestDurationFormat (..)

    -- ** HlsAdMarkers
    , HlsAdMarkers (..)

    -- ** Mp4MoovPlacement
    , Mp4MoovPlacement (..)

    -- ** DvbSubtitleAlignment
    , DvbSubtitleAlignment (..)

    -- ** InputDeblockFilter
    , InputDeblockFilter (..)

    -- ** HlsAudioTrackType
    , HlsAudioTrackType (..)

    -- ** CmfcScte35Esam
    , CmfcScte35Esam (..)

    -- ** VideoTimecodeInsertion
    , VideoTimecodeInsertion (..)

    -- ** DescribeEndpointsMode
    , DescribeEndpointsMode (..)

    -- ** CmafEncryptionType
    , CmafEncryptionType (..)

    -- ** AacCodecProfile
    , AacCodecProfile (..)

    -- ** NielsenConfiguration
    , NielsenConfiguration (..)
    , mkNielsenConfiguration
    , ncBreakoutCode
    , ncDistributorId

    -- ** ColorCorrector
    , ColorCorrector (..)
    , mkColorCorrector
    , ccBrightness
    , ccColorSpaceConversion
    , ccContrast
    , ccHdr10Metadata
    , ccHue
    , ccSaturation

    -- ** HlsInitializationVectorInManifest
    , HlsInitializationVectorInManifest (..)

    -- ** AudioLanguageCodeControl
    , AudioLanguageCodeControl (..)

    -- ** AvcIntraSlowPal
    , AvcIntraSlowPal (..)

    -- ** H265TemporalAdaptiveQuantization
    , H265TemporalAdaptiveQuantization (..)

    -- ** Input
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

    -- ** MxfSettings
    , MxfSettings (..)
    , mkMxfSettings
    , msAfdSignaling
    , msProfile

    -- ** DvbSdtSettings
    , DvbSdtSettings (..)
    , mkDvbSdtSettings
    , dssOutputSdt
    , dssSdtInterval
    , dssServiceName
    , dssServiceProviderName

    -- ** MxfAfdSignaling
    , MxfAfdSignaling (..)

    -- ** FileSourceSettings
    , FileSourceSettings (..)
    , mkFileSourceSettings
    , fssConvert608To708
    , fssFramerate
    , fssSourceFile
    , fssTimeDelta

    -- ** M3u8Settings
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

    -- ** Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- ** AudioNormalizationSettings
    , AudioNormalizationSettings (..)
    , mkAudioNormalizationSettings
    , ansAlgorithm
    , ansAlgorithmControl
    , ansCorrectionGateLevel
    , ansLoudnessLogging
    , ansPeakCalculation
    , ansTargetLkfs

    -- ** CmfcSettings
    , CmfcSettings (..)
    , mkCmfcSettings
    , csAudioDuration
    , csScte35Esam
    , csScte35Source

    -- ** Av1FramerateConversionAlgorithm
    , Av1FramerateConversionAlgorithm (..)

    -- ** H265FlickerAdaptiveQuantization
    , H265FlickerAdaptiveQuantization (..)

    -- ** MsSmoothGroupSettings
    , MsSmoothGroupSettings (..)
    , mkMsSmoothGroupSettings
    , msgsAdditionalManifests
    , msgsAudioDeduplication
    , msgsDestination
    , msgsDestinationSettings
    , msgsEncryption
    , msgsFragmentLength
    , msgsManifestEncoding

    -- ** Av1FramerateControl
    , Av1FramerateControl (..)

    -- ** InputDecryptionSettings
    , InputDecryptionSettings (..)
    , mkInputDecryptionSettings
    , idsDecryptionMode
    , idsEncryptedDecryptionKey
    , idsInitializationVector
    , idsKmsKeyRegion

    -- ** F4vMoovPlacement
    , F4vMoovPlacement (..)

    -- ** Eac3DynamicRangeCompressionLine
    , Eac3DynamicRangeCompressionLine (..)

    -- ** ImscStylePassthrough
    , ImscStylePassthrough (..)

    -- ** Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- ** AvailBlanking
    , AvailBlanking (..)
    , mkAvailBlanking
    , abAvailBlankingImage

    -- ** DvbSubtitleOutlineColor
    , DvbSubtitleOutlineColor (..)

    -- ** ProresFramerateConversionAlgorithm
    , ProresFramerateConversionAlgorithm (..)

    -- ** H265CodecLevel
    , H265CodecLevel (..)

    -- ** NoiseReducerFilterSettings
    , NoiseReducerFilterSettings (..)
    , mkNoiseReducerFilterSettings
    , nrfsStrength

    -- ** H265FramerateConversionAlgorithm
    , H265FramerateConversionAlgorithm (..)

    -- ** DestinationSettings
    , DestinationSettings (..)
    , mkDestinationSettings
    , dsS3Settings

    -- ** AccelerationMode
    , AccelerationMode (..)

    -- ** M3u8Scte35Source
    , M3u8Scte35Source (..)

    -- ** DashIsoMpdProfile
    , DashIsoMpdProfile (..)

    -- ** NielsenNonLinearWatermarkSettings
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

    -- ** M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- ** H264Syntax
    , H264Syntax (..)

    -- ** NoiseFilterPostTemporalSharpening
    , NoiseFilterPostTemporalSharpening (..)

    -- ** Queue
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

    -- ** ScalingBehavior
    , ScalingBehavior (..)

    -- ** EsamManifestConfirmConditionNotification
    , EsamManifestConfirmConditionNotification (..)
    , mkEsamManifestConfirmConditionNotification
    , emccnMccXml

    -- ** ProresFramerateControl
    , ProresFramerateControl (..)

    -- ** H264GopBReference
    , H264GopBReference (..)

    -- ** Id3Insertion
    , Id3Insertion (..)
    , mkId3Insertion
    , iiId3
    , iiTimecode

    -- ** HlsKeyProviderType
    , HlsKeyProviderType (..)

    -- ** MovSettings
    , MovSettings (..)
    , mkMovSettings
    , msClapAtom
    , msCslgAtom
    , msMpeg2FourCCControl
    , msPaddingControl
    , msReference

    -- ** RespondToAfd
    , RespondToAfd (..)

    -- ** MotionImageInserter
    , MotionImageInserter (..)
    , mkMotionImageInserter
    , miiFramerate
    , miiInput
    , miiInsertionMode
    , miiOffset
    , miiPlayback
    , miiStartTime

    -- ** MpdAudioDuration
    , MpdAudioDuration (..)

    -- ** CmfcScte35Source
    , CmfcScte35Source (..)

    -- ** Mpeg2ParControl
    , Mpeg2ParControl (..)

    -- ** H265FramerateControl
    , H265FramerateControl (..)

    -- ** InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- ** ColorSpaceConversion
    , ColorSpaceConversion (..)

    -- ** H264SpatialAdaptiveQuantization
    , H264SpatialAdaptiveQuantization (..)

    -- ** H264FieldEncoding
    , H264FieldEncoding (..)

    -- ** DashIsoHbbtvCompliance
    , DashIsoHbbtvCompliance (..)

    -- ** AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- ** Ac3Settings
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

    -- ** MpdScte35Esam
    , MpdScte35Esam (..)

    -- ** Mpeg2Telecine
    , Mpeg2Telecine (..)

    -- ** Mp4FreeSpaceBox
    , Mp4FreeSpaceBox (..)

    -- ** Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- ** ColorSpaceUsage
    , ColorSpaceUsage (..)

    -- ** AudioSelectorGroup
    , AudioSelectorGroup (..)
    , mkAudioSelectorGroup
    , asgAudioSelectorNames

    -- ** AacRawFormat
    , AacRawFormat (..)

    -- ** H265Settings
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

    -- ** CmafClientCache
    , CmafClientCache (..)

    -- ** Deinterlacer
    , Deinterlacer (..)
    , mkDeinterlacer
    , dAlgorithm
    , dControl
    , dMode

    -- ** Mp4Settings
    , Mp4Settings (..)
    , mkMp4Settings
    , mAudioDuration
    , mCslgAtom
    , mCttsVersion
    , mFreeSpaceBox
    , mMoovPlacement
    , mMp4MajorBrand

    -- ** DropFrameTimecode
    , DropFrameTimecode (..)

    -- ** Eac3AtmosStereoDownmix
    , Eac3AtmosStereoDownmix (..)

    -- ** CmafWriteHLSManifest
    , CmafWriteHLSManifest (..)

    -- ** Eac3DynamicRangeCompressionRf
    , Eac3DynamicRangeCompressionRf (..)

    -- ** PresetListBy
    , PresetListBy (..)

    -- ** AntiAlias
    , AntiAlias (..)

    -- ** DvbSubSourceSettings
    , DvbSubSourceSettings (..)
    , mkDvbSubSourceSettings
    , dsssPid

    -- ** OpusSettings
    , OpusSettings (..)
    , mkOpusSettings
    , osBitrate
    , osChannels
    , osSampleRate

    -- ** Mpeg2SpatialAdaptiveQuantization
    , Mpeg2SpatialAdaptiveQuantization (..)

    -- ** FontScript
    , FontScript (..)

    -- ** MpdSettings
    , MpdSettings (..)
    , mkMpdSettings
    , msfAccessibilityCaptionHints
    , msfAudioDuration
    , msfCaptionContainerType
    , msfScte35Esam
    , msfScte35Source

    -- ** BurninSubtitleTeletextSpacing
    , BurninSubtitleTeletextSpacing (..)

    -- ** H264QualityTuningLevel
    , H264QualityTuningLevel (..)

    -- ** ProresSettings
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

    -- ** H264UnregisteredSeiTimecode
    , H264UnregisteredSeiTimecode (..)

    -- ** Mpeg2IntraDcPrecision
    , Mpeg2IntraDcPrecision (..)

    -- ** OutputSdt
    , OutputSdt (..)

    -- ** CaptionSourceFramerate
    , CaptionSourceFramerate (..)
    , mkCaptionSourceFramerate
    , csfFramerateDenominator
    , csfFramerateNumerator

    -- ** Vc3SlowPal
    , Vc3SlowPal (..)

    -- ** Av1QvbrSettings
    , Av1QvbrSettings (..)
    , mkAv1QvbrSettings
    , aqsQvbrQualityLevel
    , aqsQvbrQualityLevelFineTune

    -- ** NoiseReducer
    , NoiseReducer (..)
    , mkNoiseReducer
    , nrFilter
    , nrFilterSettings
    , nrSpatialFilterSettings
    , nrTemporalFilterSettings

    -- ** TimedMetadata
    , TimedMetadata (..)

    -- ** Eac3LfeControl
    , Eac3LfeControl (..)

    -- ** Output
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

    -- ** Av1SpatialAdaptiveQuantization
    , Av1SpatialAdaptiveQuantization (..)

    -- ** Vc3Telecine
    , Vc3Telecine (..)

    -- ** M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- ** SpekeKeyProviderCmaf
    , SpekeKeyProviderCmaf (..)
    , mkSpekeKeyProviderCmaf
    , skpcCertificateArn
    , skpcDashSignaledSystemIds
    , skpcHlsSignaledSystemIds
    , skpcResourceId
    , skpcUrl

    -- ** Job
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

    -- ** ImageInserter
    , ImageInserter (..)
    , mkImageInserter
    , iiInsertableImages

    -- ** TrackSourceSettings
    , TrackSourceSettings (..)
    , mkTrackSourceSettings
    , tssTrackNumber

    -- ** HlsCaptionLanguageMapping
    , HlsCaptionLanguageMapping (..)
    , mkHlsCaptionLanguageMapping
    , hclmCaptionChannel
    , hclmCustomLanguageCode
    , hclmLanguageCode
    , hclmLanguageDescription

    -- ** H264TemporalAdaptiveQuantization
    , H264TemporalAdaptiveQuantization (..)

    -- ** H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- ** BillingTagsSource
    , BillingTagsSource (..)

    -- ** AncillarySourceSettings
    , AncillarySourceSettings (..)
    , mkAncillarySourceSettings
    , assConvert608To708
    , assSourceAncillaryChannelNumber
    , assTerminateCaptions

    -- ** AudioChannelTag
    , AudioChannelTag (..)

    -- ** H265RateControlMode
    , H265RateControlMode (..)

    -- ** CmafKeyProviderType
    , CmafKeyProviderType (..)

    -- ** S3DestinationAccessControl
    , S3DestinationAccessControl (..)
    , mkS3DestinationAccessControl
    , sdacCannedAcl

    -- ** Eac3CodingMode
    , Eac3CodingMode (..)

    -- ** BurninSubtitleAlignment
    , BurninSubtitleAlignment (..)

    -- ** VideoDescription
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

    -- ** CaptionDestinationSettings
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

    -- ** EmbeddedDestinationSettings
    , EmbeddedDestinationSettings (..)
    , mkEmbeddedDestinationSettings
    , edsDestination608ChannelNumber
    , edsDestination708ServiceNumber

    -- ** DeinterlacerMode
    , DeinterlacerMode (..)

    -- ** DvbSubtitleBackgroundColor
    , DvbSubtitleBackgroundColor (..)

    -- ** Hdr10Metadata
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

    -- ** JobSettings
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

    -- ** Ac3CodingMode
    , Ac3CodingMode (..)

    -- ** DvbSubtitleFontColor
    , DvbSubtitleFontColor (..)

    -- ** DashIsoPlaybackDeviceCompatibility
    , DashIsoPlaybackDeviceCompatibility (..)

    -- ** CmafStreamInfResolution
    , CmafStreamInfResolution (..)

    -- ** Eac3AtmosDynamicRangeCompressionLine
    , Eac3AtmosDynamicRangeCompressionLine (..)

    -- ** AncillaryConvert608To708
    , AncillaryConvert608To708 (..)

    -- ** ContainerSettings
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

    -- ** HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- ** TimecodeConfig
    , TimecodeConfig (..)
    , mkTimecodeConfig
    , tcAnchor
    , tcSource
    , tcStart
    , tcTimestampOffset

    -- ** EsamSignalProcessingNotification
    , EsamSignalProcessingNotification (..)
    , mkEsamSignalProcessingNotification
    , espnSccXml

    -- ** Mpeg2TemporalAdaptiveQuantization
    , Mpeg2TemporalAdaptiveQuantization (..)

    -- ** CmafCodecSpecification
    , CmafCodecSpecification (..)

    -- ** Vp8QualityTuningLevel
    , Vp8QualityTuningLevel (..)

    -- ** H264SlowPal
    , H264SlowPal (..)

    -- ** Mp2Settings
    , Mp2Settings (..)
    , mkMp2Settings
    , msBitrate
    , msChannels
    , msSampleRate

    -- ** H265WriteMp4PackagingType
    , H265WriteMp4PackagingType (..)

    -- ** OutputSettings
    , OutputSettings (..)
    , mkOutputSettings
    , osHlsSettings

    -- ** M2tsScte35Source
    , M2tsScte35Source (..)

    -- ** AvcIntraClass
    , AvcIntraClass (..)

    -- ** HlsAudioOnlyContainer
    , HlsAudioOnlyContainer (..)

    -- ** SccDestinationSettings
    , SccDestinationSettings (..)
    , mkSccDestinationSettings
    , sdsFramerate

    -- ** M2tsForceTsVideoEbpOrder
    , M2tsForceTsVideoEbpOrder (..)

    -- ** Vp9Settings
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

    -- ** H264Settings
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

    -- ** M3u8PcrControl
    , M3u8PcrControl (..)

    -- ** Preset
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

    -- ** H265QualityTuningLevel
    , H265QualityTuningLevel (..)

    -- ** NoiseReducerTemporalFilterSettings
    , NoiseReducerTemporalFilterSettings (..)
    , mkNoiseReducerTemporalFilterSettings
    , nrtfsAggressiveMode
    , nrtfsPostTemporalSharpening
    , nrtfsSpeed
    , nrtfsStrength

    -- ** M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- ** HlsGroupSettings
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

    -- ** WavSettings
    , WavSettings (..)
    , mkWavSettings
    , wsBitDepth
    , wsChannels
    , wsFormat
    , wsSampleRate

    -- ** MovClapAtom
    , MovClapAtom (..)

    -- ** AvcIntraTelecine
    , AvcIntraTelecine (..)

    -- ** HlsEncryptionSettings
    , HlsEncryptionSettings (..)
    , mkHlsEncryptionSettings
    , hesConstantInitializationVector
    , hesEncryptionMethod
    , hesInitializationVectorInManifest
    , hesOfflineEncrypted
    , hesSpekeKeyProvider
    , hesStaticKeyProvider
    , hesType

    -- ** Mpeg2GopSizeUnits
    , Mpeg2GopSizeUnits (..)

    -- ** BurninDestinationSettings
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

    -- ** Eac3AtmosSettings
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

    -- ** CmafSegmentControl
    , CmafSegmentControl (..)

    -- ** AudioNormalizationPeakCalculation
    , AudioNormalizationPeakCalculation (..)

    -- ** TimecodeBurninPosition
    , TimecodeBurninPosition (..)

    -- ** BurninSubtitleOutlineColor
    , BurninSubtitleOutlineColor (..)

    -- ** HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- ** ProresInterlaceMode
    , ProresInterlaceMode (..)

    -- ** Av1AdaptiveQuantization
    , Av1AdaptiveQuantization (..)

    -- ** ImscDestinationSettings
    , ImscDestinationSettings (..)
    , mkImscDestinationSettings
    , idsStylePassthrough

    -- ** HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- ** MotionImageInsertionMode
    , MotionImageInsertionMode (..)

    -- ** WatermarkingStrength
    , WatermarkingStrength (..)

    -- ** DolbyVisionLevel6Metadata
    , DolbyVisionLevel6Metadata (..)
    , mkDolbyVisionLevel6Metadata
    , dvlmMaxCll
    , dvlmMaxFall

    -- ** CmafWriteDASHManifest
    , CmafWriteDASHManifest (..)

    -- ** H265InterlaceMode
    , H265InterlaceMode (..)

    -- ** AudioCodecSettings
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

    -- ** MovMpeg2FourCCControl
    , MovMpeg2FourCCControl (..)

    -- ** AacVbrQuality
    , AacVbrQuality (..)

    -- ** S3ObjectCannedAcl
    , S3ObjectCannedAcl (..)

    -- ** MsSmoothManifestEncoding
    , MsSmoothManifestEncoding (..)

    -- ** M2tsNielsenId3
    , M2tsNielsenId3 (..)

    -- ** PresetSettings
    , PresetSettings (..)
    , mkPresetSettings
    , psAudioDescriptions
    , psCaptionDescriptions
    , psContainerSettings
    , psVideoDescription

    -- ** Vc3Class
    , Vc3Class (..)

    -- ** Mpeg2Settings
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

    -- ** AutomatedEncodingSettings
    , AutomatedEncodingSettings (..)
    , mkAutomatedEncodingSettings
    , aesAbrSettings

    -- ** MovCslgAtom
    , MovCslgAtom (..)

    -- ** Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- ** AudioCodec
    , AudioCodec (..)

    -- ** Eac3AtmosDynamicRangeCompressionRf
    , Eac3AtmosDynamicRangeCompressionRf (..)

    -- ** Eac3AtmosBitstreamMode
    , Eac3AtmosBitstreamMode (..)

    -- ** Timing
    , Timing (..)
    , mkTiming
    , tFinishTime
    , tStartTime
    , tSubmitTime

    -- ** Av1RateControlMode
    , Av1RateControlMode (..)

    -- ** Type
    , Type (..)

    -- ** OutputChannelMapping
    , OutputChannelMapping (..)
    , mkOutputChannelMapping
    , ocmInputChannels

    -- ** CaptionDescription
    , CaptionDescription (..)
    , mkCaptionDescription
    , cdCaptionSelectorName
    , cdCustomLanguageCode
    , cdDestinationSettings
    , cdLanguageCode
    , cdLanguageDescription

    -- ** CmafManifestCompression
    , CmafManifestCompression (..)

    -- ** H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- ** VideoDetail
    , VideoDetail (..)
    , mkVideoDetail
    , vdHeightInPx
    , vdWidthInPx

    -- ** H265Telecine
    , H265Telecine (..)

    -- ** OutputGroupType
    , OutputGroupType (..)

    -- ** H265SpatialAdaptiveQuantization
    , H265SpatialAdaptiveQuantization (..)

    -- ** InputClipping
    , InputClipping (..)
    , mkInputClipping
    , icEndTimecode
    , icStartTimecode

    -- ** ColorMetadata
    , ColorMetadata (..)

    -- ** DashIsoWriteSegmentTimelineInRepresentation
    , DashIsoWriteSegmentTimelineInRepresentation (..)

    -- ** DashAdditionalManifest
    , DashAdditionalManifest (..)
    , mkDashAdditionalManifest
    , damManifestNameModifier
    , damSelectedOutputs

    -- ** H265GopBReference
    , H265GopBReference (..)

    -- ** ProresTelecine
    , ProresTelecine (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** CaptionSelector
    , CaptionSelector (..)
    , mkCaptionSelector
    , csCustomLanguageCode
    , csLanguageCode
    , csSourceSettings

    -- ** BurninSubtitleShadowColor
    , BurninSubtitleShadowColor (..)

    -- ** AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- ** CmafMpdProfile
    , CmafMpdProfile (..)

    -- ** MotionImageInsertionOffset
    , MotionImageInsertionOffset (..)
    , mkMotionImageInsertionOffset
    , miioImageX
    , miioImageY

    -- ** Vp8Settings
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

    -- ** Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eUrl

    -- ** EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- ** Eac3AtmosCodingMode
    , Eac3AtmosCodingMode (..)

    -- ** Commitment
    , Commitment (..)

    -- ** WavFormat
    , WavFormat (..)

    -- ** AudioSelectorType
    , AudioSelectorType (..)

    -- ** CaptionSourceType
    , CaptionSourceType (..)

    -- ** Mp3Settings
    , Mp3Settings (..)
    , mkMp3Settings
    , mBitrate
    , mChannels
    , mRateControlMode
    , mSampleRate
    , mVbrQuality

    -- ** DeinterlacerControl
    , DeinterlacerControl (..)

    -- ** InputTimecodeSource
    , InputTimecodeSource (..)

    -- ** Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- ** Vc3Settings
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

    -- ** Mpeg2RateControlMode
    , Mpeg2RateControlMode (..)

    -- ** Vp9QualityTuningLevel
    , Vp9QualityTuningLevel (..)

    -- ** Vc3FramerateConversionAlgorithm
    , Vc3FramerateConversionAlgorithm (..)

    -- ** M2tsPcrControl
    , M2tsPcrControl (..)

    -- ** InputTemplate
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

    -- ** AacSpecification
    , AacSpecification (..)

    -- ** MotionImagePlayback
    , MotionImagePlayback (..)

    -- ** BurninSubtitleFontColor
    , BurninSubtitleFontColor (..)

    -- ** RemixSettings
    , RemixSettings (..)
    , mkRemixSettings
    , rsChannelMapping
    , rsChannelsIn
    , rsChannelsOut

    -- ** HlsManifestCompression
    , HlsManifestCompression (..)

    -- ** H265Tiles
    , H265Tiles (..)

    -- ** DvbNitSettings
    , DvbNitSettings (..)
    , mkDvbNitSettings
    , dnsNetworkId
    , dnsNetworkName
    , dnsNitInterval

    -- ** FileSourceConvert608To708
    , FileSourceConvert608To708 (..)

    -- ** QueueTransition
    , QueueTransition (..)
    , mkQueueTransition
    , qtDestinationQueue
    , qtSourceQueue
    , qtTimestamp

    -- ** StaticKeyProvider
    , StaticKeyProvider (..)
    , mkStaticKeyProvider
    , skpKeyFormat
    , skpKeyFormatVersions
    , skpStaticKeyValue
    , skpUrl

    -- ** JobTemplateListBy
    , JobTemplateListBy (..)

    -- ** H264RateControlMode
    , H264RateControlMode (..)

    -- ** SpekeKeyProvider
    , SpekeKeyProvider (..)
    , mkSpekeKeyProvider
    , sCertificateArn
    , sResourceId
    , sSystemIds
    , sUrl

    -- ** CaptionSourceSettings
    , CaptionSourceSettings (..)
    , mkCaptionSourceSettings
    , cssAncillarySourceSettings
    , cssDvbSubSourceSettings
    , cssEmbeddedSourceSettings
    , cssFileSourceSettings
    , cssSourceType
    , cssTeletextSourceSettings
    , cssTrackSourceSettings

    -- ** M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- ** M2tsEbpAudioInterval
    , M2tsEbpAudioInterval (..)

    -- ** TeletextPageType
    , TeletextPageType (..)

    -- ** DecryptionMode
    , DecryptionMode (..)

    -- ** Order
    , Order (..)

    -- ** ReservationPlanSettings
    , ReservationPlanSettings (..)
    , mkReservationPlanSettings
    , rpsCommitment
    , rpsReservedSlots
    , rpsRenewalType

    -- ** OutputDetail
    , OutputDetail (..)
    , mkOutputDetail
    , odDurationInMs
    , odVideoDetails

    -- ** Vp8FramerateControl
    , Vp8FramerateControl (..)

    -- ** AccelerationStatus
    , AccelerationStatus (..)

    -- ** Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- ** VorbisSettings
    , VorbisSettings (..)
    , mkVorbisSettings
    , vsChannels
    , vsSampleRate
    , vsVbrQuality

    -- ** AacSettings
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

    -- ** PartnerWatermarking
    , PartnerWatermarking (..)
    , mkPartnerWatermarking
    , pwNexguardFileMarkerSettings

    -- ** Ac3DynamicRangeCompressionProfile
    , Ac3DynamicRangeCompressionProfile (..)

    -- ** RenewalType
    , RenewalType (..)

    -- ** EmbeddedSourceSettings
    , EmbeddedSourceSettings (..)
    , mkEmbeddedSourceSettings
    , essConvert608To708
    , essSource608ChannelNumber
    , essSource608TrackNumber
    , essTerminateCaptions

    -- ** Vc3FramerateControl
    , Vc3FramerateControl (..)

    -- ** H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- ** Vp8FramerateConversionAlgorithm
    , Vp8FramerateConversionAlgorithm (..)

    -- ** Mpeg2AdaptiveQuantization
    , Mpeg2AdaptiveQuantization (..)

    -- ** MsSmoothEncryptionSettings
    , MsSmoothEncryptionSettings (..)
    , mkMsSmoothEncryptionSettings
    , msesSpekeKeyProvider

    -- ** F4vSettings
    , F4vSettings (..)
    , mkF4vSettings
    , fsMoovPlacement

    -- ** DvbSubtitlingType
    , DvbSubtitlingType (..)

    -- ** Eac3Settings
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

    -- ** InputScanType
    , InputScanType (..)

    -- ** Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- ** AvcIntraSettings
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

    -- ** Vp9RateControlMode
    , Vp9RateControlMode (..)

    -- ** HlsAdditionalManifest
    , HlsAdditionalManifest (..)
    , mkHlsAdditionalManifest
    , hamManifestNameModifier
    , hamSelectedOutputs

    -- ** DeinterlaceAlgorithm
    , DeinterlaceAlgorithm (..)

    -- ** H264DynamicSubGop
    , H264DynamicSubGop (..)

    -- ** NielsenActiveWatermarkProcessType
    , NielsenActiveWatermarkProcessType (..)

    -- ** H264Telecine
    , H264Telecine (..)

    -- ** Mpeg2QualityTuningLevel
    , Mpeg2QualityTuningLevel (..)

    -- ** AudioTypeControl
    , AudioTypeControl (..)

    -- ** DashIsoEncryptionSettings
    , DashIsoEncryptionSettings (..)
    , mkDashIsoEncryptionSettings
    , diesPlaybackDeviceCompatibility
    , diesSpekeKeyProvider

    -- ** Mpeg2InterlaceMode
    , Mpeg2InterlaceMode (..)

    -- ** H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- ** ColorSpace
    , ColorSpace (..)

    -- ** M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- ** DashIsoGroupSettings
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

    -- ** OutputGroupDetail
    , OutputGroupDetail (..)
    , mkOutputGroupDetail
    , ogdOutputDetails

    -- ** S3DestinationSettings
    , S3DestinationSettings (..)
    , mkS3DestinationSettings
    , sdsAccessControl
    , sdsEncryption

    -- ** AudioNormalizationLoudnessLogging
    , AudioNormalizationLoudnessLogging (..)

    -- ** Rectangle
    , Rectangle (..)
    , mkRectangle
    , rHeight
    , rWidth
    , rX
    , rY

    -- ** Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- ** M2tsRateMode
    , M2tsRateMode (..)

    -- ** MsSmoothAudioDeduplication
    , MsSmoothAudioDeduplication (..)

    -- ** MpdAccessibilityCaptionHints
    , MpdAccessibilityCaptionHints (..)

    -- ** MotionImageInsertionFramerate
    , MotionImageInsertionFramerate (..)
    , mkMotionImageInsertionFramerate
    , miifFramerateDenominator
    , miifFramerateNumerator

    -- ** DvbTdtSettings
    , DvbTdtSettings (..)
    , mkDvbTdtSettings
    , dtsTdtInterval

    -- ** S3ServerSideEncryptionType
    , S3ServerSideEncryptionType (..)

    -- ** Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Waiters
import Network.AWS.MediaConvert.DeletePreset
import Network.AWS.MediaConvert.UpdatePreset
import Network.AWS.MediaConvert.ListTagsForResource
import Network.AWS.MediaConvert.ListQueues
import Network.AWS.MediaConvert.DeleteQueue
import Network.AWS.MediaConvert.UpdateQueue
import Network.AWS.MediaConvert.GetPreset
import Network.AWS.MediaConvert.CreateJob
import Network.AWS.MediaConvert.ListJobs
import Network.AWS.MediaConvert.GetJob
import Network.AWS.MediaConvert.CreatePreset
import Network.AWS.MediaConvert.ListPresets
import Network.AWS.MediaConvert.DisassociateCertificate
import Network.AWS.MediaConvert.GetQueue
import Network.AWS.MediaConvert.DescribeEndpoints
import Network.AWS.MediaConvert.CreateQueue
import Network.AWS.MediaConvert.TagResource
import Network.AWS.MediaConvert.CreateJobTemplate
import Network.AWS.MediaConvert.UntagResource
import Network.AWS.MediaConvert.DeleteJobTemplate
import Network.AWS.MediaConvert.UpdateJobTemplate
import Network.AWS.MediaConvert.ListJobTemplates
import Network.AWS.MediaConvert.GetJobTemplate
import Network.AWS.MediaConvert.AssociateCertificate
import Network.AWS.MediaConvert.CancelJob
import qualified Network.AWS.Prelude as Lude

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
