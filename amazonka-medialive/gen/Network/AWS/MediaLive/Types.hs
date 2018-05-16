{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types
    (
    -- * Service Configuration
      mediaLive

    -- * Errors
    , _GatewayTimeoutException
    , _UnprocessableEntityException
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _BadGatewayException
    , _BadRequestException

    -- * AacCodingMode
    , AacCodingMode (..)

    -- * AacInputType
    , AacInputType (..)

    -- * AacProfile
    , AacProfile (..)

    -- * AacRateControlMode
    , AacRateControlMode (..)

    -- * AacRawFormat
    , AacRawFormat (..)

    -- * AacSpec
    , AacSpec (..)

    -- * AacVbrQuality
    , AacVbrQuality (..)

    -- * Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- * Ac3CodingMode
    , Ac3CodingMode (..)

    -- * Ac3DrcProfile
    , Ac3DrcProfile (..)

    -- * Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- * Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- * AfdSignaling
    , AfdSignaling (..)

    -- * AudioDescriptionAudioTypeControl
    , AudioDescriptionAudioTypeControl (..)

    -- * AudioDescriptionLanguageCodeControl
    , AudioDescriptionLanguageCodeControl (..)

    -- * AudioLanguageSelectionPolicy
    , AudioLanguageSelectionPolicy (..)

    -- * AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- * AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- * AudioOnlyHlsTrackType
    , AudioOnlyHlsTrackType (..)

    -- * AudioType
    , AudioType (..)

    -- * AuthenticationScheme
    , AuthenticationScheme (..)

    -- * AvailBlankingState
    , AvailBlankingState (..)

    -- * BlackoutSlateNetworkEndBlackout
    , BlackoutSlateNetworkEndBlackout (..)

    -- * BlackoutSlateState
    , BlackoutSlateState (..)

    -- * BurnInAlignment
    , BurnInAlignment (..)

    -- * BurnInBackgroundColor
    , BurnInBackgroundColor (..)

    -- * BurnInFontColor
    , BurnInFontColor (..)

    -- * BurnInOutlineColor
    , BurnInOutlineColor (..)

    -- * BurnInShadowColor
    , BurnInShadowColor (..)

    -- * BurnInTeletextGridControl
    , BurnInTeletextGridControl (..)

    -- * ChannelState
    , ChannelState (..)

    -- * DvbSdtOutputSdt
    , DvbSdtOutputSdt (..)

    -- * DvbSubDestinationAlignment
    , DvbSubDestinationAlignment (..)

    -- * DvbSubDestinationBackgroundColor
    , DvbSubDestinationBackgroundColor (..)

    -- * DvbSubDestinationFontColor
    , DvbSubDestinationFontColor (..)

    -- * DvbSubDestinationOutlineColor
    , DvbSubDestinationOutlineColor (..)

    -- * DvbSubDestinationShadowColor
    , DvbSubDestinationShadowColor (..)

    -- * DvbSubDestinationTeletextGridControl
    , DvbSubDestinationTeletextGridControl (..)

    -- * Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- * Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- * Eac3CodingMode
    , Eac3CodingMode (..)

    -- * Eac3DcFilter
    , Eac3DcFilter (..)

    -- * Eac3DrcLine
    , Eac3DrcLine (..)

    -- * Eac3DrcRf
    , Eac3DrcRf (..)

    -- * Eac3LfeControl
    , Eac3LfeControl (..)

    -- * Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- * Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- * Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- * Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- * Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- * Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- * Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- * EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- * EmbeddedScte20Detection
    , EmbeddedScte20Detection (..)

    -- * FecOutputIncludeFec
    , FecOutputIncludeFec (..)

    -- * FixedAfd
    , FixedAfd (..)

    -- * GlobalConfigurationInputEndAction
    , GlobalConfigurationInputEndAction (..)

    -- * GlobalConfigurationLowFramerateInputs
    , GlobalConfigurationLowFramerateInputs (..)

    -- * GlobalConfigurationOutputTimingSource
    , GlobalConfigurationOutputTimingSource (..)

    -- * H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- * H264ColorMetadata
    , H264ColorMetadata (..)

    -- * H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- * H264FlickerAq
    , H264FlickerAq (..)

    -- * H264FramerateControl
    , H264FramerateControl (..)

    -- * H264GopBReference
    , H264GopBReference (..)

    -- * H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- * H264Level
    , H264Level (..)

    -- * H264LookAheadRateControl
    , H264LookAheadRateControl (..)

    -- * H264ParControl
    , H264ParControl (..)

    -- * H264Profile
    , H264Profile (..)

    -- * H264RateControlMode
    , H264RateControlMode (..)

    -- * H264ScanType
    , H264ScanType (..)

    -- * H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- * H264SpatialAq
    , H264SpatialAq (..)

    -- * H264Syntax
    , H264Syntax (..)

    -- * H264TemporalAq
    , H264TemporalAq (..)

    -- * H264TimecodeInsertionBehavior
    , H264TimecodeInsertionBehavior (..)

    -- * HlsAdMarkers
    , HlsAdMarkers (..)

    -- * HlsAkamaiHTTPTransferMode
    , HlsAkamaiHTTPTransferMode (..)

    -- * HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- * HlsClientCache
    , HlsClientCache (..)

    -- * HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- * HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- * HlsEncryptionType
    , HlsEncryptionType (..)

    -- * HlsIvInManifest
    , HlsIvInManifest (..)

    -- * HlsIvSource
    , HlsIvSource (..)

    -- * HlsManifestCompression
    , HlsManifestCompression (..)

    -- * HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- * HlsMediaStoreStorageClass
    , HlsMediaStoreStorageClass (..)

    -- * HlsMode
    , HlsMode (..)

    -- * HlsOutputSelection
    , HlsOutputSelection (..)

    -- * HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- * HlsSegmentationMode
    , HlsSegmentationMode (..)

    -- * HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- * HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- * HlsTsFileMode
    , HlsTsFileMode (..)

    -- * HlsWebdavHTTPTransferMode
    , HlsWebdavHTTPTransferMode (..)

    -- * InputCodec
    , InputCodec (..)

    -- * InputDeblockFilter
    , InputDeblockFilter (..)

    -- * InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- * InputFilter
    , InputFilter (..)

    -- * InputLossActionForHlsOut
    , InputLossActionForHlsOut (..)

    -- * InputLossActionForMsSmoothOut
    , InputLossActionForMsSmoothOut (..)

    -- * InputLossActionForUdpOut
    , InputLossActionForUdpOut (..)

    -- * InputLossImageType
    , InputLossImageType (..)

    -- * InputMaximumBitrate
    , InputMaximumBitrate (..)

    -- * InputResolution
    , InputResolution (..)

    -- * InputSecurityGroupState
    , InputSecurityGroupState (..)

    -- * InputSourceEndBehavior
    , InputSourceEndBehavior (..)

    -- * InputState
    , InputState (..)

    -- * InputType
    , InputType (..)

    -- * M2tsAbsentInputAudioBehavior
    , M2tsAbsentInputAudioBehavior (..)

    -- * M2tsArib
    , M2tsArib (..)

    -- * M2tsAribCaptionsPidControl
    , M2tsAribCaptionsPidControl (..)

    -- * M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- * M2tsAudioInterval
    , M2tsAudioInterval (..)

    -- * M2tsAudioStreamType
    , M2tsAudioStreamType (..)

    -- * M2tsBufferModel
    , M2tsBufferModel (..)

    -- * M2tsCCDescriptor
    , M2tsCCDescriptor (..)

    -- * M2tsEbifControl
    , M2tsEbifControl (..)

    -- * M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- * M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- * M2tsKlv
    , M2tsKlv (..)

    -- * M2tsPcrControl
    , M2tsPcrControl (..)

    -- * M2tsRateMode
    , M2tsRateMode (..)

    -- * M2tsScte35Control
    , M2tsScte35Control (..)

    -- * M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- * M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- * M2tsTimedMetadataBehavior
    , M2tsTimedMetadataBehavior (..)

    -- * M3u8PcrControl
    , M3u8PcrControl (..)

    -- * M3u8Scte35Behavior
    , M3u8Scte35Behavior (..)

    -- * M3u8TimedMetadataBehavior
    , M3u8TimedMetadataBehavior (..)

    -- * Mp2CodingMode
    , Mp2CodingMode (..)

    -- * NetworkInputServerValidation
    , NetworkInputServerValidation (..)

    -- * RtmpCacheFullBehavior
    , RtmpCacheFullBehavior (..)

    -- * RtmpCaptionData
    , RtmpCaptionData (..)

    -- * RtmpOutputCertificateMode
    , RtmpOutputCertificateMode (..)

    -- * Scte20Convert608To708
    , Scte20Convert608To708 (..)

    -- * Scte35AposNoRegionalBlackoutBehavior
    , Scte35AposNoRegionalBlackoutBehavior (..)

    -- * Scte35AposWebDeliveryAllowedBehavior
    , Scte35AposWebDeliveryAllowedBehavior (..)

    -- * Scte35SpliceInsertNoRegionalBlackoutBehavior
    , Scte35SpliceInsertNoRegionalBlackoutBehavior (..)

    -- * Scte35SpliceInsertWebDeliveryAllowedBehavior
    , Scte35SpliceInsertWebDeliveryAllowedBehavior (..)

    -- * SmoothGroupAudioOnlyTimecodeControl
    , SmoothGroupAudioOnlyTimecodeControl (..)

    -- * SmoothGroupCertificateMode
    , SmoothGroupCertificateMode (..)

    -- * SmoothGroupEventIdMode
    , SmoothGroupEventIdMode (..)

    -- * SmoothGroupEventStopBehavior
    , SmoothGroupEventStopBehavior (..)

    -- * SmoothGroupSegmentationMode
    , SmoothGroupSegmentationMode (..)

    -- * SmoothGroupSparseTrackType
    , SmoothGroupSparseTrackType (..)

    -- * SmoothGroupStreamManifestBehavior
    , SmoothGroupStreamManifestBehavior (..)

    -- * SmoothGroupTimestampOffsetMode
    , SmoothGroupTimestampOffsetMode (..)

    -- * TimecodeConfigSource
    , TimecodeConfigSource (..)

    -- * TtmlDestinationStyleControl
    , TtmlDestinationStyleControl (..)

    -- * UdpTimedMetadataId3Frame
    , UdpTimedMetadataId3Frame (..)

    -- * VideoDescriptionRespondToAfd
    , VideoDescriptionRespondToAfd (..)

    -- * VideoDescriptionScalingBehavior
    , VideoDescriptionScalingBehavior (..)

    -- * VideoSelectorColorSpace
    , VideoSelectorColorSpace (..)

    -- * VideoSelectorColorSpaceUsage
    , VideoSelectorColorSpaceUsage (..)

    -- * AacSettings
    , AacSettings
    , aacSettings
    , aRawFormat
    , aCodingMode
    , aProfile
    , aRateControlMode
    , aSampleRate
    , aSpec
    , aBitrate
    , aVbrQuality
    , aInputType

    -- * Ac3Settings
    , Ac3Settings
    , ac3Settings
    , asLfeFilter
    , asMetadataControl
    , asBitstreamMode
    , asCodingMode
    , asBitrate
    , asDialnorm
    , asDrcProfile

    -- * ArchiveContainerSettings
    , ArchiveContainerSettings
    , archiveContainerSettings
    , acsM2tsSettings

    -- * ArchiveGroupSettings
    , ArchiveGroupSettings
    , archiveGroupSettings
    , agsRolloverInterval
    , agsDestination

    -- * ArchiveOutputSettings
    , ArchiveOutputSettings
    , archiveOutputSettings
    , aosExtension
    , aosNameModifier
    , aosContainerSettings

    -- * AribDestinationSettings
    , AribDestinationSettings
    , aribDestinationSettings

    -- * AribSourceSettings
    , AribSourceSettings
    , aribSourceSettings

    -- * AudioChannelMapping
    , AudioChannelMapping
    , audioChannelMapping
    , acmOutputChannel
    , acmInputChannelLevels

    -- * AudioCodecSettings
    , AudioCodecSettings
    , audioCodecSettings
    , acsPassThroughSettings
    , acsAc3Settings
    , acsMp2Settings
    , acsAacSettings
    , acsEac3Settings

    -- * AudioDescription
    , AudioDescription
    , audioDescription
    , adLanguageCode
    , adAudioType
    , adAudioNormalizationSettings
    , adLanguageCodeControl
    , adCodecSettings
    , adStreamName
    , adRemixSettings
    , adAudioTypeControl
    , adAudioSelectorName
    , adName

    -- * AudioLanguageSelection
    , AudioLanguageSelection
    , audioLanguageSelection
    , alsLanguageSelectionPolicy
    , alsLanguageCode

    -- * AudioNormalizationSettings
    , AudioNormalizationSettings
    , audioNormalizationSettings
    , ansAlgorithmControl
    , ansTargetLkfs
    , ansAlgorithm

    -- * AudioOnlyHlsSettings
    , AudioOnlyHlsSettings
    , audioOnlyHlsSettings
    , aohsAudioOnlyImage
    , aohsAudioGroupId
    , aohsAudioTrackType

    -- * AudioPidSelection
    , AudioPidSelection
    , audioPidSelection
    , apsPid

    -- * AudioSelector
    , AudioSelector
    , audioSelector
    , asSelectorSettings
    , asName

    -- * AudioSelectorSettings
    , AudioSelectorSettings
    , audioSelectorSettings
    , assAudioLanguageSelection
    , assAudioPidSelection

    -- * AvailBlanking
    , AvailBlanking
    , availBlanking
    , abState
    , abAvailBlankingImage

    -- * AvailConfiguration
    , AvailConfiguration
    , availConfiguration
    , acAvailSettings

    -- * AvailSettings
    , AvailSettings
    , availSettings
    , asScte35SpliceInsert
    , asScte35TimeSignalApos

    -- * BlackoutSlate
    , BlackoutSlate
    , blackoutSlate
    , bsNetworkEndBlackoutImage
    , bsState
    , bsNetworkEndBlackout
    , bsNetworkId
    , bsBlackoutSlateImage

    -- * BurnInDestinationSettings
    , BurnInDestinationSettings
    , burnInDestinationSettings
    , bidsBackgroundOpacity
    , bidsFontOpacity
    , bidsShadowYOffset
    , bidsFontResolution
    , bidsYPosition
    , bidsBackgroundColor
    , bidsShadowXOffset
    , bidsFontSize
    , bidsXPosition
    , bidsAlignment
    , bidsShadowOpacity
    , bidsTeletextGridControl
    , bidsOutlineColor
    , bidsOutlineSize
    , bidsFont
    , bidsShadowColor
    , bidsFontColor

    -- * CaptionDescription
    , CaptionDescription
    , captionDescription
    , cdLanguageCode
    , cdDestinationSettings
    , cdLanguageDescription
    , cdCaptionSelectorName
    , cdName

    -- * CaptionDestinationSettings
    , CaptionDestinationSettings
    , captionDestinationSettings
    , cdsTeletextDestinationSettings
    , cdsRtmpCaptionInfoDestinationSettings
    , cdsDvbSubDestinationSettings
    , cdsScte27DestinationSettings
    , cdsTtmlDestinationSettings
    , cdsScte20PlusEmbeddedDestinationSettings
    , cdsEmbeddedPlusScte20DestinationSettings
    , cdsSmpteTtDestinationSettings
    , cdsWebvttDestinationSettings
    , cdsEmbeddedDestinationSettings
    , cdsBurnInDestinationSettings
    , cdsAribDestinationSettings

    -- * CaptionLanguageMapping
    , CaptionLanguageMapping
    , captionLanguageMapping
    , clmLanguageCode
    , clmLanguageDescription
    , clmCaptionChannel

    -- * CaptionSelector
    , CaptionSelector
    , captionSelector
    , csLanguageCode
    , csSelectorSettings
    , csName

    -- * CaptionSelectorSettings
    , CaptionSelectorSettings
    , captionSelectorSettings
    , cssTeletextSourceSettings
    , cssAribSourceSettings
    , cssScte27SourceSettings
    , cssDvbSubSourceSettings
    , cssScte20SourceSettings
    , cssEmbeddedSourceSettings

    -- * Channel
    , Channel
    , channel
    , cState
    , cARN
    , cPipelinesRunningCount
    , cInputSpecification
    , cInputAttachments
    , cDestinations
    , cName
    , cId
    , cEgressEndpoints
    , cEncoderSettings
    , cRoleARN

    -- * ChannelEgressEndpoint
    , ChannelEgressEndpoint
    , channelEgressEndpoint
    , ceeSourceIP

    -- * ChannelSummary
    , ChannelSummary
    , channelSummary
    , chaState
    , chaARN
    , chaPipelinesRunningCount
    , chaInputSpecification
    , chaInputAttachments
    , chaDestinations
    , chaName
    , chaId
    , chaEgressEndpoints
    , chaRoleARN

    -- * DvbNitSettings
    , DvbNitSettings
    , dvbNitSettings
    , dnsRepInterval
    , dnsNetworkName
    , dnsNetworkId

    -- * DvbSdtSettings
    , DvbSdtSettings
    , dvbSdtSettings
    , dssRepInterval
    , dssServiceProviderName
    , dssOutputSdt
    , dssServiceName

    -- * DvbSubDestinationSettings
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
    , dsdsAlignment
    , dsdsShadowOpacity
    , dsdsTeletextGridControl
    , dsdsOutlineColor
    , dsdsOutlineSize
    , dsdsFont
    , dsdsShadowColor
    , dsdsFontColor

    -- * DvbSubSourceSettings
    , DvbSubSourceSettings
    , dvbSubSourceSettings
    , dsssPid

    -- * DvbTdtSettings
    , DvbTdtSettings
    , dvbTdtSettings
    , dtsRepInterval

    -- * Eac3Settings
    , Eac3Settings
    , eac3Settings
    , esStereoDownmix
    , esLoRoCenterMixLevel
    , esLtRtCenterMixLevel
    , esLfeFilter
    , esLtRtSurroundMixLevel
    , esMetadataControl
    , esLoRoSurroundMixLevel
    , esSurroundMode
    , esAttenuationControl
    , esPassthroughControl
    , esBitstreamMode
    , esLfeControl
    , esCodingMode
    , esDrcLine
    , esDrcRf
    , esDcFilter
    , esBitrate
    , esPhaseControl
    , esSurroundExMode
    , esDialnorm

    -- * EmbeddedDestinationSettings
    , EmbeddedDestinationSettings
    , embeddedDestinationSettings

    -- * EmbeddedPlusScte20DestinationSettings
    , EmbeddedPlusScte20DestinationSettings
    , embeddedPlusScte20DestinationSettings

    -- * EmbeddedSourceSettings
    , EmbeddedSourceSettings
    , embeddedSourceSettings
    , essConvert608To708
    , essScte20Detection
    , essSource608TrackNumber
    , essSource608ChannelNumber

    -- * EncoderSettings
    , EncoderSettings
    , encoderSettings
    , esCaptionDescriptions
    , esAvailConfiguration
    , esAvailBlanking
    , esGlobalConfiguration
    , esBlackoutSlate
    , esVideoDescriptions
    , esAudioDescriptions
    , esOutputGroups
    , esTimecodeConfig

    -- * FecOutputSettings
    , FecOutputSettings
    , fecOutputSettings
    , fosRowLength
    , fosIncludeFec
    , fosColumnDepth

    -- * GlobalConfiguration
    , GlobalConfiguration
    , globalConfiguration
    , gcInputLossBehavior
    , gcInitialAudioGain
    , gcSupportLowFramerateInputs
    , gcInputEndAction
    , gcOutputTimingSource

    -- * H264Settings
    , H264Settings
    , h264Settings
    , hsTemporalAq
    , hsSceneChangeDetect
    , hsScanType
    , hsTimecodeInsertion
    , hsParNumerator
    , hsAfdSignaling
    , hsGopSize
    , hsGopSizeUnits
    , hsSlices
    , hsProfile
    , hsRateControlMode
    , hsMinIInterval
    , hsParControl
    , hsFlickerAq
    , hsBufSize
    , hsSpatialAq
    , hsGopNumBFrames
    , hsFixedAfd
    , hsSoftness
    , hsBitrate
    , hsFramerateDenominator
    , hsEntropyEncoding
    , hsFramerateControl
    , hsColorMetadata
    , hsLookAheadRateControl
    , hsAdaptiveQuantization
    , hsFramerateNumerator
    , hsLevel
    , hsGopBReference
    , hsMaxBitrate
    , hsSyntax
    , hsBufFillPct
    , hsGopClosedCadence
    , hsNumRefFrames
    , hsParDenominator

    -- * HlsAkamaiSettings
    , HlsAkamaiSettings
    , hlsAkamaiSettings
    , hasHTTPTransferMode
    , hasNumRetries
    , hasToken
    , hasConnectionRetryInterval
    , hasFilecacheDuration
    , hasRestartDelay
    , hasSalt

    -- * HlsBasicPutSettings
    , HlsBasicPutSettings
    , hlsBasicPutSettings
    , hbpsNumRetries
    , hbpsConnectionRetryInterval
    , hbpsFilecacheDuration
    , hbpsRestartDelay

    -- * HlsCdnSettings
    , HlsCdnSettings
    , hlsCdnSettings
    , hcsHlsAkamaiSettings
    , hcsHlsMediaStoreSettings
    , hcsHlsBasicPutSettings
    , hcsHlsWebdavSettings

    -- * HlsGroupSettings
    , HlsGroupSettings
    , hlsGroupSettings
    , hgsDirectoryStructure
    , hgsEncryptionType
    , hgsTimedMetadataId3Period
    , hgsIvInManifest
    , hgsTsFileMode
    , hgsMinSegmentLength
    , hgsProgramDateTime
    , hgsIndexNSegments
    , hgsProgramDateTimePeriod
    , hgsCodecSpecification
    , hgsHlsCdnSettings
    , hgsCaptionLanguageMappings
    , hgsInputLossAction
    , hgsMode
    , hgsKeyProviderSettings
    , hgsConstantIv
    , hgsBaseURLManifest
    , hgsAdMarkers
    , hgsKeyFormat
    , hgsSegmentLength
    , hgsTimedMetadataId3Frame
    , hgsBaseURLContent
    , hgsOutputSelection
    , hgsCaptionLanguageSetting
    , hgsSegmentsPerSubdirectory
    , hgsManifestDurationFormat
    , hgsIvSource
    , hgsSegmentationMode
    , hgsKeyFormatVersions
    , hgsClientCache
    , hgsTimestampDeltaMilliseconds
    , hgsStreamInfResolution
    , hgsKeepSegments
    , hgsManifestCompression
    , hgsDestination

    -- * HlsInputSettings
    , HlsInputSettings
    , hlsInputSettings
    , hisBufferSegments
    , hisRetries
    , hisRetryInterval
    , hisBandwidth

    -- * HlsMediaStoreSettings
    , HlsMediaStoreSettings
    , hlsMediaStoreSettings
    , hmssNumRetries
    , hmssConnectionRetryInterval
    , hmssFilecacheDuration
    , hmssMediaStoreStorageClass
    , hmssRestartDelay

    -- * HlsOutputSettings
    , HlsOutputSettings
    , hlsOutputSettings
    , hosSegmentModifier
    , hosNameModifier
    , hosHlsSettings

    -- * HlsSettings
    , HlsSettings
    , hlsSettings
    , hsAudioOnlyHlsSettings
    , hsStandardHlsSettings

    -- * HlsWebdavSettings
    , HlsWebdavSettings
    , hlsWebdavSettings
    , hwsHTTPTransferMode
    , hwsNumRetries
    , hwsConnectionRetryInterval
    , hwsFilecacheDuration
    , hwsRestartDelay

    -- * Input
    , Input
    , input
    , iState
    , iSecurityGroups
    , iARN
    , iSources
    , iDestinations
    , iName
    , iAttachedChannels
    , iId
    , iType

    -- * InputAttachment
    , InputAttachment
    , inputAttachment
    , iaInputId
    , iaInputSettings

    -- * InputChannelLevel
    , InputChannelLevel
    , inputChannelLevel
    , iclInputChannel
    , iclGain

    -- * InputDestination
    , InputDestination
    , inputDestination
    , idURL
    , idIP
    , idPort

    -- * InputDestinationRequest
    , InputDestinationRequest
    , inputDestinationRequest
    , idrStreamName

    -- * InputLocation
    , InputLocation
    , inputLocation
    , ilUsername
    , ilPasswordParam
    , ilURI

    -- * InputLossBehavior
    , InputLossBehavior
    , inputLossBehavior
    , ilbInputLossImageColor
    , ilbBlackFrameMsec
    , ilbRepeatFrameMsec
    , ilbInputLossImageType
    , ilbInputLossImageSlate

    -- * InputSecurityGroup
    , InputSecurityGroup
    , inputSecurityGroup
    , isgState
    , isgARN
    , isgInputs
    , isgId
    , isgWhitelistRules

    -- * InputSettings
    , InputSettings
    , inputSettings
    , isVideoSelector
    , isNetworkInputSettings
    , isAudioSelectors
    , isDeblockFilter
    , isDenoiseFilter
    , isFilterStrength
    , isCaptionSelectors
    , isInputFilter
    , isSourceEndBehavior

    -- * InputSource
    , InputSource
    , inputSource
    , isURL
    , isUsername
    , isPasswordParam

    -- * InputSourceRequest
    , InputSourceRequest
    , inputSourceRequest
    , isrURL
    , isrUsername
    , isrPasswordParam

    -- * InputSpecification
    , InputSpecification
    , inputSpecification
    , isResolution
    , isCodec
    , isMaximumBitrate

    -- * InputWhitelistRule
    , InputWhitelistRule
    , inputWhitelistRule
    , iwrCidr

    -- * InputWhitelistRuleCidr
    , InputWhitelistRuleCidr
    , inputWhitelistRuleCidr
    , iwrcCidr

    -- * KeyProviderSettings
    , KeyProviderSettings
    , keyProviderSettings
    , kpsStaticKeySettings

    -- * M2tsSettings
    , M2tsSettings
    , m2tsSettings
    , msPmtPid
    , msEtvSignalPid
    , msVideoPid
    , msBufferModel
    , msScte35Pid
    , msTransportStreamId
    , msProgramNum
    , msFragmentTime
    , msTimedMetadataBehavior
    , msCCDescriptor
    , msPmtInterval
    , msDvbSdtSettings
    , msEcmPid
    , msNullPacketBitrate
    , msAudioBufferModel
    , msTimedMetadataPid
    , msKlv
    , msAudioFramesPerPes
    , msPcrPeriod
    , msPcrPid
    , msSegmentationMarkers
    , msAribCaptionsPidControl
    , msKlvDataPids
    , msEbpLookaheadMs
    , msDvbSubPids
    , msScte27Pids
    , msPatInterval
    , msAudioStreamType
    , msEsRateInPes
    , msEtvPlatformPid
    , msBitrate
    , msScte35Control
    , msAudioPids
    , msDvbTeletextPid
    , msEbif
    , msArib
    , msAribCaptionsPid
    , msAbsentInputAudioBehavior
    , msSegmentationTime
    , msEbpAudioInterval
    , msDvbNitSettings
    , msPcrControl
    , msEbpPlacement
    , msRateMode
    , msSegmentationStyle
    , msDvbTdtSettings

    -- * M3u8Settings
    , M3u8Settings
    , m3u8Settings
    , mPmtPid
    , mVideoPid
    , mScte35Pid
    , mTransportStreamId
    , mProgramNum
    , mTimedMetadataBehavior
    , mPmtInterval
    , mEcmPid
    , mTimedMetadataPid
    , mAudioFramesPerPes
    , mPcrPeriod
    , mPcrPid
    , mPatInterval
    , mAudioPids
    , mScte35Behavior
    , mPcrControl

    -- * Mp2Settings
    , Mp2Settings
    , mp2Settings
    , mCodingMode
    , mSampleRate
    , mBitrate

    -- * MsSmoothGroupSettings
    , MsSmoothGroupSettings
    , msSmoothGroupSettings
    , msgsFragmentLength
    , msgsStreamManifestBehavior
    , msgsSendDelayMs
    , msgsEventStopBehavior
    , msgsTimestampOffsetMode
    , msgsNumRetries
    , msgsAcquisitionPointId
    , msgsInputLossAction
    , msgsTimestampOffset
    , msgsCertificateMode
    , msgsSparseTrackType
    , msgsConnectionRetryInterval
    , msgsFilecacheDuration
    , msgsRestartDelay
    , msgsEventIdMode
    , msgsAudioOnlyTimecodeControl
    , msgsSegmentationMode
    , msgsEventId
    , msgsDestination

    -- * MsSmoothOutputSettings
    , MsSmoothOutputSettings
    , msSmoothOutputSettings
    , msosNameModifier

    -- * NetworkInputSettings
    , NetworkInputSettings
    , networkInputSettings
    , nisHlsInputSettings
    , nisServerValidation

    -- * Output
    , Output
    , output
    , oCaptionDescriptionNames
    , oVideoDescriptionName
    , oOutputName
    , oAudioDescriptionNames
    , oOutputSettings

    -- * OutputDestination
    , OutputDestination
    , outputDestination
    , odSettings
    , odId

    -- * OutputDestinationSettings
    , OutputDestinationSettings
    , outputDestinationSettings
    , odsURL
    , odsUsername
    , odsPasswordParam
    , odsStreamName

    -- * OutputGroup
    , OutputGroup
    , outputGroup
    , ogName
    , ogOutputs
    , ogOutputGroupSettings

    -- * OutputGroupSettings
    , OutputGroupSettings
    , outputGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsRtmpGroupSettings
    , ogsHlsGroupSettings
    , ogsArchiveGroupSettings
    , ogsUdpGroupSettings

    -- * OutputLocationRef
    , OutputLocationRef
    , outputLocationRef
    , olrDestinationRefId

    -- * OutputSettings
    , OutputSettings
    , outputSettings
    , osArchiveOutputSettings
    , osRtmpOutputSettings
    , osHlsOutputSettings
    , osUdpOutputSettings
    , osMsSmoothOutputSettings

    -- * PassThroughSettings
    , PassThroughSettings
    , passThroughSettings

    -- * RemixSettings
    , RemixSettings
    , remixSettings
    , rsChannelsIn
    , rsChannelsOut
    , rsChannelMappings

    -- * RtmpCaptionInfoDestinationSettings
    , RtmpCaptionInfoDestinationSettings
    , rtmpCaptionInfoDestinationSettings

    -- * RtmpGroupSettings
    , RtmpGroupSettings
    , rtmpGroupSettings
    , rgsCaptionData
    , rgsRestartDelay
    , rgsAuthenticationScheme
    , rgsCacheLength
    , rgsCacheFullBehavior

    -- * RtmpOutputSettings
    , RtmpOutputSettings
    , rtmpOutputSettings
    , rosNumRetries
    , rosCertificateMode
    , rosConnectionRetryInterval
    , rosDestination

    -- * Scte20PlusEmbeddedDestinationSettings
    , Scte20PlusEmbeddedDestinationSettings
    , scte20PlusEmbeddedDestinationSettings

    -- * Scte20SourceSettings
    , Scte20SourceSettings
    , scte20SourceSettings
    , sssConvert608To708
    , sssSource608ChannelNumber

    -- * Scte27DestinationSettings
    , Scte27DestinationSettings
    , scte27DestinationSettings

    -- * Scte27SourceSettings
    , Scte27SourceSettings
    , scte27SourceSettings
    , sssPid

    -- * Scte35SpliceInsert
    , Scte35SpliceInsert
    , scte35SpliceInsert
    , ssiWebDeliveryAllowedFlag
    , ssiAdAvailOffset
    , ssiNoRegionalBlackoutFlag

    -- * Scte35TimeSignalApos
    , Scte35TimeSignalApos
    , scte35TimeSignalApos
    , stsaWebDeliveryAllowedFlag
    , stsaAdAvailOffset
    , stsaNoRegionalBlackoutFlag

    -- * SmpteTtDestinationSettings
    , SmpteTtDestinationSettings
    , smpteTtDestinationSettings

    -- * StandardHlsSettings
    , StandardHlsSettings
    , standardHlsSettings
    , shsAudioRenditionSets
    , shsM3u8Settings

    -- * StaticKeySettings
    , StaticKeySettings
    , staticKeySettings
    , sksKeyProviderServer
    , sksStaticKeyValue

    -- * TeletextDestinationSettings
    , TeletextDestinationSettings
    , teletextDestinationSettings

    -- * TeletextSourceSettings
    , TeletextSourceSettings
    , teletextSourceSettings
    , tssPageNumber

    -- * TimecodeConfig
    , TimecodeConfig
    , timecodeConfig
    , tcSyncThreshold
    , tcSource

    -- * TtmlDestinationSettings
    , TtmlDestinationSettings
    , ttmlDestinationSettings
    , tdsStyleControl

    -- * UdpContainerSettings
    , UdpContainerSettings
    , udpContainerSettings
    , ucsM2tsSettings

    -- * UdpGroupSettings
    , UdpGroupSettings
    , udpGroupSettings
    , ugsTimedMetadataId3Period
    , ugsInputLossAction
    , ugsTimedMetadataId3Frame

    -- * UdpOutputSettings
    , UdpOutputSettings
    , udpOutputSettings
    , uosFecOutputSettings
    , uosBufferMsec
    , uosDestination
    , uosContainerSettings

    -- * VideoCodecSettings
    , VideoCodecSettings
    , videoCodecSettings
    , vcsH264Settings

    -- * VideoDescription
    , VideoDescription
    , videoDescription
    , vdHeight
    , vdSharpness
    , vdWidth
    , vdScalingBehavior
    , vdRespondToAfd
    , vdCodecSettings
    , vdName

    -- * VideoSelector
    , VideoSelector
    , videoSelector
    , vsSelectorSettings
    , vsColorSpaceUsage
    , vsColorSpace

    -- * VideoSelectorPid
    , VideoSelectorPid
    , videoSelectorPid
    , vspPid

    -- * VideoSelectorProgramId
    , VideoSelectorProgramId
    , videoSelectorProgramId
    , vspiProgramId

    -- * VideoSelectorSettings
    , VideoSelectorSettings
    , videoSelectorSettings
    , vssVideoSelectorProgramId
    , vssVideoSelectorPid

    -- * WebvttDestinationSettings
    , WebvttDestinationSettings
    , webvttDestinationSettings
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Product
import Network.AWS.MediaLive.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-14@ of the Amazon Elemental MediaLive SDK configuration.
mediaLive :: Service
mediaLive =
  Service
    { _svcAbbrev = "MediaLive"
    , _svcSigner = v4
    , _svcPrefix = "medialive"
    , _svcVersion = "2017-10-14"
    , _svcEndpoint = defaultEndpoint mediaLive
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MediaLive"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Placeholder documentation for GatewayTimeoutException
_GatewayTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_GatewayTimeoutException =
  _MatchServiceError mediaLive "GatewayTimeoutException" . hasStatus 504


-- | Placeholder documentation for UnprocessableEntityException
_UnprocessableEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_UnprocessableEntityException =
  _MatchServiceError mediaLive "UnprocessableEntityException" . hasStatus 422


-- | Placeholder documentation for ConflictException
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError mediaLive "ConflictException" . hasStatus 409


-- | Placeholder documentation for ForbiddenException
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
  _MatchServiceError mediaLive "ForbiddenException" . hasStatus 403


-- | Placeholder documentation for NotFoundException
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError mediaLive "NotFoundException" . hasStatus 404


-- | Placeholder documentation for TooManyRequestsException
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError mediaLive "TooManyRequestsException" . hasStatus 429


-- | Placeholder documentation for InternalServerErrorException
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
  _MatchServiceError mediaLive "InternalServerErrorException" . hasStatus 500


-- | Placeholder documentation for BadGatewayException
_BadGatewayException :: AsError a => Getting (First ServiceError) a ServiceError
_BadGatewayException =
  _MatchServiceError mediaLive "BadGatewayException" . hasStatus 502


-- | Placeholder documentation for BadRequestException
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError mediaLive "BadRequestException" . hasStatus 400

