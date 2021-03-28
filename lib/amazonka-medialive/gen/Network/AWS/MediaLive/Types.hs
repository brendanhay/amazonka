-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types
    (
    -- * Service configuration
      mkServiceConfig

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

    -- * H264FlickerAq
    , H264FlickerAq (..)

    -- * ReservationVideoQuality
    , ReservationVideoQuality (..)

    -- * H265FilterSettings
    , H265FilterSettings (..)
    , mkH265FilterSettings
    , hTemporalFilterSettings

    -- * HlsInputSettings
    , HlsInputSettings (..)
    , mkHlsInputSettings
    , hisBandwidth
    , hisBufferSegments
    , hisRetries
    , hisRetryInterval

    -- * M2tsNielsenId3Behavior
    , M2tsNielsenId3Behavior (..)

    -- * MediaPackageGroupSettings
    , MediaPackageGroupSettings (..)
    , mkMediaPackageGroupSettings
    , mpgsDestination

    -- * Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- * AudioLanguageSelection
    , AudioLanguageSelection (..)
    , mkAudioLanguageSelection
    , alsLanguageCode
    , alsLanguageSelectionPolicy

    -- * PreferredChannelPipeline
    , PreferredChannelPipeline (..)

    -- * VideoSelector
    , VideoSelector (..)
    , mkVideoSelector
    , vsColorSpace
    , vsColorSpaceUsage
    , vsSelectorSettings

    -- * InputLossFailoverSettings
    , InputLossFailoverSettings (..)
    , mkInputLossFailoverSettings
    , ilfsInputLossThresholdMsec

    -- * HlsMode
    , HlsMode (..)

    -- * GlobalConfigurationOutputLockingMode
    , GlobalConfigurationOutputLockingMode (..)

    -- * Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- * H264ParControl
    , H264ParControl (..)

    -- * H264ColorSpaceSettings
    , H264ColorSpaceSettings (..)
    , mkH264ColorSpaceSettings
    , hcssColorSpacePassthroughSettings
    , hcssRec601Settings
    , hcssRec709Settings

    -- * FeatureActivationsInputPrepareScheduleActions
    , FeatureActivationsInputPrepareScheduleActions (..)

    -- * HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- * TeletextDestinationSettings
    , TeletextDestinationSettings (..)
    , mkTeletextDestinationSettings

    -- * HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- * FrameCaptureSettings
    , FrameCaptureSettings (..)
    , mkFrameCaptureSettings
    , fcsCaptureInterval
    , fcsCaptureIntervalUnits

    -- * InputDeviceConfiguredInput
    , InputDeviceConfiguredInput (..)

    -- * HlsAkamaiSettings
    , HlsAkamaiSettings (..)
    , mkHlsAkamaiSettings
    , hasConnectionRetryInterval
    , hasFilecacheDuration
    , hasHttpTransferMode
    , hasNumRetries
    , hasRestartDelay
    , hasSalt
    , hasToken

    -- * Mpeg2ColorSpace
    , Mpeg2ColorSpace (..)

    -- * GlobalConfigurationInputEndAction
    , GlobalConfigurationInputEndAction (..)

    -- * SmoothGroupSparseTrackType
    , SmoothGroupSparseTrackType (..)

    -- * MultiplexProgram
    , MultiplexProgram (..)
    , mkMultiplexProgram
    , mpChannelId
    , mpMultiplexProgramSettings
    , mpPacketIdentifiersMap
    , mpPipelineDetails
    , mpProgramName

    -- * BatchScheduleActionDeleteResult
    , BatchScheduleActionDeleteResult (..)
    , mkBatchScheduleActionDeleteResult
    , bsadrScheduleActions

    -- * Fmp4TimedMetadataBehavior
    , Fmp4TimedMetadataBehavior (..)

    -- * EbuTtDDestinationSettings
    , EbuTtDDestinationSettings (..)
    , mkEbuTtDDestinationSettings
    , etddsFillLineGap
    , etddsFontFamily
    , etddsStyleControl

    -- * BurnInShadowColor
    , BurnInShadowColor (..)

    -- * InputDeviceConnectionState
    , InputDeviceConnectionState (..)

    -- * PassThroughSettings
    , PassThroughSettings (..)
    , mkPassThroughSettings

    -- * M2tsBufferModel
    , M2tsBufferModel (..)

    -- * FailoverCondition
    , FailoverCondition (..)
    , mkFailoverCondition
    , fcFailoverConditionSettings

    -- * InputDeviceRequest
    , InputDeviceRequest (..)
    , mkInputDeviceRequest
    , idrId

    -- * InputDestinationVpc
    , InputDestinationVpc (..)
    , mkInputDestinationVpc
    , idvAvailabilityZone
    , idvNetworkInterfaceId

    -- * TimecodeConfigSource
    , TimecodeConfigSource (..)

    -- * ReservationState
    , ReservationState (..)

    -- * ReservationResourceSpecification
    , ReservationResourceSpecification (..)
    , mkReservationResourceSpecification
    , rrsChannelClass
    , rrsCodec
    , rrsMaximumBitrate
    , rrsMaximumFramerate
    , rrsResolution
    , rrsResourceType
    , rrsSpecialFeature
    , rrsVideoQuality

    -- * ReservationMaximumFramerate
    , ReservationMaximumFramerate (..)

    -- * TemporalFilterPostFilterSharpening
    , TemporalFilterPostFilterSharpening (..)

    -- * PipelineId
    , PipelineId (..)

    -- * MultiplexProgramPipelineDetail
    , MultiplexProgramPipelineDetail (..)
    , mkMultiplexProgramPipelineDetail
    , mppdActiveChannelPipeline
    , mppdPipelineId

    -- * HlsRedundantManifest
    , HlsRedundantManifest (..)

    -- * H265Level
    , H265Level (..)

    -- * NielsenPcmToId3TaggingState
    , NielsenPcmToId3TaggingState (..)

    -- * AudioOnlyHlsTrackType
    , AudioOnlyHlsTrackType (..)

    -- * H264SpatialAq
    , H264SpatialAq (..)

    -- * BatchScheduleActionCreateRequest
    , BatchScheduleActionCreateRequest (..)
    , mkBatchScheduleActionCreateRequest
    , bsacrScheduleActions

    -- * VideoCodecSettings
    , VideoCodecSettings (..)
    , mkVideoCodecSettings
    , vcsFrameCaptureSettings
    , vcsH264Settings
    , vcsH265Settings
    , vcsMpeg2Settings

    -- * PipelinePauseStateSettings
    , PipelinePauseStateSettings (..)
    , mkPipelinePauseStateSettings
    , ppssPipelineId

    -- * InputDeviceIpScheme
    , InputDeviceIpScheme (..)

    -- * FailoverConditionSettings
    , FailoverConditionSettings (..)
    , mkFailoverConditionSettings
    , fcsInputLossSettings

    -- * InputVpcRequest
    , InputVpcRequest (..)
    , mkInputVpcRequest
    , ivrSubnetIds
    , ivrSecurityGroupIds

    -- * HlsClientCache
    , HlsClientCache (..)

    -- * RtmpCaptionInfoDestinationSettings
    , RtmpCaptionInfoDestinationSettings (..)
    , mkRtmpCaptionInfoDestinationSettings

    -- * MultiplexOutputSettings
    , MultiplexOutputSettings (..)
    , mkMultiplexOutputSettings
    , mosDestination

    -- * EmbeddedScte20Detection
    , EmbeddedScte20Detection (..)

    -- * H265LookAheadRateControl
    , H265LookAheadRateControl (..)

    -- * H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- * ReservationResourceType
    , ReservationResourceType (..)

    -- * FecOutputSettings
    , FecOutputSettings (..)
    , mkFecOutputSettings
    , fosColumnDepth
    , fosIncludeFec
    , fosRowLength

    -- * MsSmoothH265PackagingType
    , MsSmoothH265PackagingType (..)

    -- * H265Profile
    , H265Profile (..)

    -- * HlsSettings
    , HlsSettings (..)
    , mkHlsSettings
    , hsAudioOnlyHlsSettings
    , hsFmp4HlsSettings
    , hsStandardHlsSettings

    -- * LogLevel
    , LogLevel (..)

    -- * InputSwitchScheduleActionSettings
    , InputSwitchScheduleActionSettings (..)
    , mkInputSwitchScheduleActionSettings
    , issasInputAttachmentNameReference
    , issasInputClippingSettings
    , issasUrlPath

    -- * InputSourceRequest
    , InputSourceRequest (..)
    , mkInputSourceRequest
    , isrPasswordParam
    , isrUrl
    , isrUsername

    -- * Scte35SegmentationDescriptor
    , Scte35SegmentationDescriptor (..)
    , mkScte35SegmentationDescriptor
    , ssdSegmentationEventId
    , ssdSegmentationCancelIndicator
    , ssdDeliveryRestrictions
    , ssdSegmentNum
    , ssdSegmentationDuration
    , ssdSegmentationTypeId
    , ssdSegmentationUpid
    , ssdSegmentationUpidType
    , ssdSegmentsExpected
    , ssdSubSegmentNum
    , ssdSubSegmentsExpected

    -- * BatchScheduleActionCreateResult
    , BatchScheduleActionCreateResult (..)
    , mkBatchScheduleActionCreateResult
    , bScheduleActions

    -- * Hdr10Settings
    , Hdr10Settings (..)
    , mkHdr10Settings
    , hsMaxCll
    , hsMaxFall

    -- * InputWhitelistRule
    , InputWhitelistRule (..)
    , mkInputWhitelistRule
    , iwrCidr

    -- * H264FilterSettings
    , H264FilterSettings (..)
    , mkH264FilterSettings
    , hfsTemporalFilterSettings

    -- * H265ColorSpaceSettings
    , H265ColorSpaceSettings (..)
    , mkH265ColorSpaceSettings
    , hColorSpacePassthroughSettings
    , hHdr10Settings
    , hRec601Settings
    , hRec709Settings

    -- * AfdSignaling
    , AfdSignaling (..)

    -- * AacRateControlMode
    , AacRateControlMode (..)

    -- * HlsTsFileMode
    , HlsTsFileMode (..)

    -- * InputDeviceNetworkSettings
    , InputDeviceNetworkSettings (..)
    , mkInputDeviceNetworkSettings
    , idnsDnsAddresses
    , idnsGateway
    , idnsIpAddress
    , idnsIpScheme
    , idnsSubnetMask

    -- * SmoothGroupTimestampOffsetMode
    , SmoothGroupTimestampOffsetMode (..)

    -- * ChannelEgressEndpoint
    , ChannelEgressEndpoint (..)
    , mkChannelEgressEndpoint
    , ceeSourceIp

    -- * InputLossActionForRtmpOut
    , InputLossActionForRtmpOut (..)

    -- * OutputGroupSettings
    , OutputGroupSettings (..)
    , mkOutputGroupSettings
    , ogsArchiveGroupSettings
    , ogsFrameCaptureGroupSettings
    , ogsHlsGroupSettings
    , ogsMediaPackageGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsMultiplexGroupSettings
    , ogsRtmpGroupSettings
    , ogsUdpGroupSettings

    -- * TeletextSourceSettings
    , TeletextSourceSettings (..)
    , mkTeletextSourceSettings
    , tssPageNumber

    -- * M2tsSettings
    , M2tsSettings (..)
    , mkM2tsSettings
    , mAbsentInputAudioBehavior
    , mArib
    , mAribCaptionsPid
    , mAribCaptionsPidControl
    , mAudioBufferModel
    , mAudioFramesPerPes
    , mAudioPids
    , mAudioStreamType
    , mBitrate
    , mBufferModel
    , mCcDescriptor
    , mDvbNitSettings
    , mDvbSdtSettings
    , mDvbSubPids
    , mDvbTdtSettings
    , mDvbTeletextPid
    , mEbif
    , mEbpAudioInterval
    , mEbpLookaheadMs
    , mEbpPlacement
    , mEcmPid
    , mEsRateInPes
    , mEtvPlatformPid
    , mEtvSignalPid
    , mFragmentTime
    , mKlv
    , mKlvDataPids
    , mNielsenId3Behavior
    , mNullPacketBitrate
    , mPatInterval
    , mPcrControl
    , mPcrPeriod
    , mPcrPid
    , mPmtInterval
    , mPmtPid
    , mProgramNum
    , mRateMode
    , mScte27Pids
    , mScte35Control
    , mScte35Pid
    , mSegmentationMarkers
    , mSegmentationStyle
    , mSegmentationTime
    , mTimedMetadataBehavior
    , mTimedMetadataPid
    , mTransportStreamId
    , mVideoPid

    -- * DvbSubDestinationSettings
    , DvbSubDestinationSettings (..)
    , mkDvbSubDestinationSettings
    , dsdsAlignment
    , dsdsBackgroundColor
    , dsdsBackgroundOpacity
    , dsdsFont
    , dsdsFontColor
    , dsdsFontOpacity
    , dsdsFontResolution
    , dsdsFontSize
    , dsdsOutlineColor
    , dsdsOutlineSize
    , dsdsShadowColor
    , dsdsShadowOpacity
    , dsdsShadowXOffset
    , dsdsShadowYOffset
    , dsdsTeletextGridControl
    , dsdsXPosition
    , dsdsYPosition

    -- * Smpte2038DataPreference
    , Smpte2038DataPreference (..)

    -- * VideoSelectorProgramId
    , VideoSelectorProgramId (..)
    , mkVideoSelectorProgramId
    , vspiProgramId

    -- * SmoothGroupEventStopBehavior
    , SmoothGroupEventStopBehavior (..)

    -- * NetworkInputSettings
    , NetworkInputSettings (..)
    , mkNetworkInputSettings
    , nisHlsInputSettings
    , nisServerValidation

    -- * Fmp4HlsSettings
    , Fmp4HlsSettings (..)
    , mkFmp4HlsSettings
    , fhsAudioRenditionSets
    , fhsNielsenId3Behavior
    , fhsTimedMetadataBehavior

    -- * Scte27DestinationSettings
    , Scte27DestinationSettings (..)
    , mkScte27DestinationSettings

    -- * MediaPackageOutputDestinationSettings
    , MediaPackageOutputDestinationSettings (..)
    , mkMediaPackageOutputDestinationSettings
    , mpodsChannelId

    -- * H264FramerateControl
    , H264FramerateControl (..)

    -- * Mpeg2FilterSettings
    , Mpeg2FilterSettings (..)
    , mkMpeg2FilterSettings
    , mfsTemporalFilterSettings

    -- * H264ForceFieldPictures
    , H264ForceFieldPictures (..)

    -- * InputCodec
    , InputCodec (..)

    -- * SmoothGroupAudioOnlyTimecodeControl
    , SmoothGroupAudioOnlyTimecodeControl (..)

    -- * InputResolution
    , InputResolution (..)

    -- * M2tsTimedMetadataBehavior
    , M2tsTimedMetadataBehavior (..)

    -- * H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- * Scte35SpliceInsert
    , Scte35SpliceInsert (..)
    , mkScte35SpliceInsert
    , ssiAdAvailOffset
    , ssiNoRegionalBlackoutFlag
    , ssiWebDeliveryAllowedFlag

    -- * HlsIvInManifest
    , HlsIvInManifest (..)

    -- * HlsOutputSelection
    , HlsOutputSelection (..)

    -- * MultiplexProgramPacketIdentifiersMap
    , MultiplexProgramPacketIdentifiersMap (..)
    , mkMultiplexProgramPacketIdentifiersMap
    , mppimAudioPids
    , mppimDvbSubPids
    , mppimDvbTeletextPid
    , mppimEtvPlatformPid
    , mppimEtvSignalPid
    , mppimKlvDataPids
    , mppimPcrPid
    , mppimPmtPid
    , mppimPrivateMetadataPid
    , mppimScte27Pids
    , mppimScte35Pid
    , mppimTimedMetadataPid
    , mppimVideoPid

    -- * InputSecurityGroupState
    , InputSecurityGroupState (..)

    -- * AribSourceSettings
    , AribSourceSettings (..)
    , mkAribSourceSettings

    -- * ArchiveContainerSettings
    , ArchiveContainerSettings (..)
    , mkArchiveContainerSettings
    , acsM2tsSettings
    , acsRawSettings

    -- * AudioOnlyHlsSettings
    , AudioOnlyHlsSettings (..)
    , mkAudioOnlyHlsSettings
    , aohsAudioGroupId
    , aohsAudioOnlyImage
    , aohsAudioTrackType
    , aohsSegmentType

    -- * MediaConnectFlow
    , MediaConnectFlow (..)
    , mkMediaConnectFlow
    , mcfFlowArn

    -- * M3u8Scte35Behavior
    , M3u8Scte35Behavior (..)

    -- * Reservation
    , Reservation (..)
    , mkReservation
    , rArn
    , rCount
    , rCurrencyCode
    , rDuration
    , rDurationUnits
    , rEnd
    , rFixedPrice
    , rName
    , rOfferingDescription
    , rOfferingId
    , rOfferingType
    , rRegion
    , rReservationId
    , rResourceSpecification
    , rStart
    , rState
    , rTags
    , rUsagePrice

    -- * ArchiveOutputSettings
    , ArchiveOutputSettings (..)
    , mkArchiveOutputSettings
    , aosContainerSettings
    , aosExtension
    , aosNameModifier

    -- * AudioDescription
    , AudioDescription (..)
    , mkAudioDescription
    , adAudioSelectorName
    , adName
    , adAudioNormalizationSettings
    , adAudioType
    , adAudioTypeControl
    , adCodecSettings
    , adLanguageCode
    , adLanguageCodeControl
    , adRemixSettings
    , adStreamName

    -- * Offering
    , Offering (..)
    , mkOffering
    , oArn
    , oCurrencyCode
    , oDuration
    , oDurationUnits
    , oFixedPrice
    , oOfferingDescription
    , oOfferingId
    , oOfferingType
    , oRegion
    , oResourceSpecification
    , oUsagePrice

    -- * TtmlDestinationSettings
    , TtmlDestinationSettings (..)
    , mkTtmlDestinationSettings
    , tdsStyleControl

    -- * HlsEncryptionType
    , HlsEncryptionType (..)

    -- * MultiplexProgramSummary
    , MultiplexProgramSummary (..)
    , mkMultiplexProgramSummary
    , mpsChannelId
    , mpsProgramName

    -- * HlsId3SegmentTaggingState
    , HlsId3SegmentTaggingState (..)

    -- * AacCodingMode
    , AacCodingMode (..)

    -- * HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- * OutputGroup
    , OutputGroup (..)
    , mkOutputGroup
    , ogOutputs
    , ogOutputGroupSettings
    , ogName

    -- * H264ColorMetadata
    , H264ColorMetadata (..)

    -- * SmoothGroupSegmentationMode
    , SmoothGroupSegmentationMode (..)

    -- * StopTimecode
    , StopTimecode (..)
    , mkStopTimecode
    , sLastFrameClippingBehavior
    , sTimecode

    -- * AudioSelector
    , AudioSelector (..)
    , mkAudioSelector
    , asName
    , asSelectorSettings

    -- * H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- * H264LookAheadRateControl
    , H264LookAheadRateControl (..)

    -- * OutputDestination
    , OutputDestination (..)
    , mkOutputDestination
    , odId
    , odMediaPackageSettings
    , odMultiplexSettings
    , odSettings

    -- * HlsCdnSettings
    , HlsCdnSettings (..)
    , mkHlsCdnSettings
    , hcsHlsAkamaiSettings
    , hcsHlsBasicPutSettings
    , hcsHlsMediaStoreSettings
    , hcsHlsWebdavSettings

    -- * InputSpecification
    , InputSpecification (..)
    , mkInputSpecification
    , isCodec
    , isMaximumBitrate
    , isResolution

    -- * InputDestinationRequest
    , InputDestinationRequest (..)
    , mkInputDestinationRequest
    , idrStreamName

    -- * OutputLocationRef
    , OutputLocationRef (..)
    , mkOutputLocationRef
    , olrDestinationRefId

    -- * Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- * BurnInAlignment
    , BurnInAlignment (..)

    -- * M3u8TimedMetadataBehavior
    , M3u8TimedMetadataBehavior (..)

    -- * HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- * AvailConfiguration
    , AvailConfiguration (..)
    , mkAvailConfiguration
    , acAvailSettings

    -- * InputMaximumBitrate
    , InputMaximumBitrate (..)

    -- * M2tsCcDescriptor
    , M2tsCcDescriptor (..)

    -- * UdpContainerSettings
    , UdpContainerSettings (..)
    , mkUdpContainerSettings
    , ucsM2tsSettings

    -- * PipelineDetail
    , PipelineDetail (..)
    , mkPipelineDetail
    , pdActiveInputAttachmentName
    , pdActiveInputSwitchActionName
    , pdPipelineId

    -- * H265Tier
    , H265Tier (..)

    -- * InputSource
    , InputSource (..)
    , mkInputSource
    , isPasswordParam
    , isUrl
    , isUsername

    -- * Channel
    , Channel (..)
    , mkChannel
    , cArn
    , cCdiInputSpecification
    , cChannelClass
    , cDestinations
    , cEgressEndpoints
    , cEncoderSettings
    , cId
    , cInputAttachments
    , cInputSpecification
    , cLogLevel
    , cName
    , cPipelineDetails
    , cPipelinesRunningCount
    , cRoleArn
    , cState
    , cTags

    -- * Eac3DcFilter
    , Eac3DcFilter (..)

    -- * DvbSubDestinationBackgroundColor
    , DvbSubDestinationBackgroundColor (..)

    -- * H264TimecodeInsertionBehavior
    , H264TimecodeInsertionBehavior (..)

    -- * FeatureActivations
    , FeatureActivations (..)
    , mkFeatureActivations
    , faInputPrepareScheduleActions

    -- * HlsAdMarkers
    , HlsAdMarkers (..)

    -- * InputChannelLevel
    , InputChannelLevel (..)
    , mkInputChannelLevel
    , iclInputChannel
    , iclGain

    -- * PauseStateScheduleActionSettings
    , PauseStateScheduleActionSettings (..)
    , mkPauseStateScheduleActionSettings
    , pssasPipelines

    -- * KeyProviderSettings
    , KeyProviderSettings (..)
    , mkKeyProviderSettings
    , kpsStaticKeySettings

    -- * Mpeg2DisplayRatio
    , Mpeg2DisplayRatio (..)

    -- * ReservationCodec
    , ReservationCodec (..)

    -- * InputDeblockFilter
    , InputDeblockFilter (..)

    -- * EbuTtDFillLineGapControl
    , EbuTtDFillLineGapControl (..)

    -- * ReservationResolution
    , ReservationResolution (..)

    -- * Scte35SegmentationCancelIndicator
    , Scte35SegmentationCancelIndicator (..)

    -- * H265ScanType
    , H265ScanType (..)

    -- * AacSpec
    , AacSpec (..)

    -- * MultiplexProgramChannelDestinationSettings
    , MultiplexProgramChannelDestinationSettings (..)
    , mkMultiplexProgramChannelDestinationSettings
    , mpcdsMultiplexId
    , mpcdsProgramName

    -- * Scte35SpliceInsertScheduleActionSettings
    , Scte35SpliceInsertScheduleActionSettings (..)
    , mkScte35SpliceInsertScheduleActionSettings
    , ssisasSpliceEventId
    , ssisasDuration

    -- * Scte20PlusEmbeddedDestinationSettings
    , Scte20PlusEmbeddedDestinationSettings (..)
    , mkScte20PlusEmbeddedDestinationSettings

    -- * AudioType
    , AudioType (..)

    -- * EmbeddedPlusScte20DestinationSettings
    , EmbeddedPlusScte20DestinationSettings (..)
    , mkEmbeddedPlusScte20DestinationSettings

    -- * NielsenConfiguration
    , NielsenConfiguration (..)
    , mkNielsenConfiguration
    , ncDistributorId
    , ncNielsenPcmToId3Tagging

    -- * CdiInputResolution
    , CdiInputResolution (..)

    -- * HlsIncompleteSegmentBehavior
    , HlsIncompleteSegmentBehavior (..)

    -- * CaptionSelectorSettings
    , CaptionSelectorSettings (..)
    , mkCaptionSelectorSettings
    , cssAncillarySourceSettings
    , cssAribSourceSettings
    , cssDvbSubSourceSettings
    , cssEmbeddedSourceSettings
    , cssScte20SourceSettings
    , cssScte27SourceSettings
    , cssTeletextSourceSettings

    -- * BatchSuccessfulResultModel
    , BatchSuccessfulResultModel (..)
    , mkBatchSuccessfulResultModel
    , bsrmArn
    , bsrmId
    , bsrmState

    -- * Input
    , Input (..)
    , mkInput
    , iArn
    , iAttachedChannels
    , iDestinations
    , iId
    , iInputClass
    , iInputDevices
    , iInputSourceType
    , iMediaConnectFlows
    , iName
    , iRoleArn
    , iSecurityGroups
    , iSources
    , iState
    , iTags
    , iType

    -- * DvbSdtSettings
    , DvbSdtSettings (..)
    , mkDvbSdtSettings
    , dssOutputSdt
    , dssRepInterval
    , dssServiceName
    , dssServiceProviderName

    -- * Scte20Convert608To708
    , Scte20Convert608To708 (..)

    -- * M3u8Settings
    , M3u8Settings (..)
    , mkM3u8Settings
    , msAudioFramesPerPes
    , msAudioPids
    , msEcmPid
    , msNielsenId3Behavior
    , msPatInterval
    , msPcrControl
    , msPcrPeriod
    , msPcrPid
    , msPmtInterval
    , msPmtPid
    , msProgramNum
    , msScte35Behavior
    , msScte35Pid
    , msTimedMetadataBehavior
    , msTimedMetadataPid
    , msTransportStreamId
    , msVideoPid

    -- * AvailBlankingState
    , AvailBlankingState (..)

    -- * Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- * AudioNormalizationSettings
    , AudioNormalizationSettings (..)
    , mkAudioNormalizationSettings
    , ansAlgorithm
    , ansAlgorithmControl
    , ansTargetLkfs

    -- * H264Level
    , H264Level (..)

    -- * SmpteTtDestinationSettings
    , SmpteTtDestinationSettings (..)
    , mkSmpteTtDestinationSettings

    -- * AudioOnlyHlsSegmentType
    , AudioOnlyHlsSegmentType (..)

    -- * Mpeg2TimecodeInsertionBehavior
    , Mpeg2TimecodeInsertionBehavior (..)

    -- * Scte35NoRegionalBlackoutFlag
    , Scte35NoRegionalBlackoutFlag (..)

    -- * ScheduleActionSettings
    , ScheduleActionSettings (..)
    , mkScheduleActionSettings
    , sasHlsId3SegmentTaggingSettings
    , sasHlsTimedMetadataSettings
    , sasInputPrepareSettings
    , sasInputSwitchSettings
    , sasPauseStateSettings
    , sasScte35ReturnToNetworkSettings
    , sasScte35SpliceInsertSettings
    , sasScte35TimeSignalSettings
    , sasStaticImageActivateSettings
    , sasStaticImageDeactivateSettings

    -- * MsSmoothGroupSettings
    , MsSmoothGroupSettings (..)
    , mkMsSmoothGroupSettings
    , msgsDestination
    , msgsAcquisitionPointId
    , msgsAudioOnlyTimecodeControl
    , msgsCertificateMode
    , msgsConnectionRetryInterval
    , msgsEventId
    , msgsEventIdMode
    , msgsEventStopBehavior
    , msgsFilecacheDuration
    , msgsFragmentLength
    , msgsInputLossAction
    , msgsNumRetries
    , msgsRestartDelay
    , msgsSegmentationMode
    , msgsSendDelayMs
    , msgsSparseTrackType
    , msgsStreamManifestBehavior
    , msgsTimestampOffset
    , msgsTimestampOffsetMode

    -- * VideoDescriptionRespondToAfd
    , VideoDescriptionRespondToAfd (..)

    -- * MultiplexStatmuxVideoSettings
    , MultiplexStatmuxVideoSettings (..)
    , mkMultiplexStatmuxVideoSettings
    , msvsMaximumBitrate
    , msvsMinimumBitrate
    , msvsPriority

    -- * TemporalFilterStrength
    , TemporalFilterStrength (..)

    -- * InputLossBehavior
    , InputLossBehavior (..)
    , mkInputLossBehavior
    , ilbBlackFrameMsec
    , ilbInputLossImageColor
    , ilbInputLossImageSlate
    , ilbInputLossImageType
    , ilbRepeatFrameMsec

    -- * Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- * VideoSelectorSettings
    , VideoSelectorSettings (..)
    , mkVideoSelectorSettings
    , vssVideoSelectorPid
    , vssVideoSelectorProgramId

    -- * AvailBlanking
    , AvailBlanking (..)
    , mkAvailBlanking
    , abAvailBlankingImage
    , abState

    -- * HlsMediaStoreSettings
    , HlsMediaStoreSettings (..)
    , mkHlsMediaStoreSettings
    , hmssConnectionRetryInterval
    , hmssFilecacheDuration
    , hmssMediaStoreStorageClass
    , hmssNumRetries
    , hmssRestartDelay

    -- * AudioTrackSelection
    , AudioTrackSelection (..)
    , mkAudioTrackSelection
    , atsTracks

    -- * MultiplexProgramSettings
    , MultiplexProgramSettings (..)
    , mkMultiplexProgramSettings
    , mpsProgramNumber
    , mpsPreferredChannelPipeline
    , mpsServiceDescriptor
    , mpsVideoSettings

    -- * InputPrepareScheduleActionSettings
    , InputPrepareScheduleActionSettings (..)
    , mkInputPrepareScheduleActionSettings
    , ipsasInputAttachmentNameReference
    , ipsasInputClippingSettings
    , ipsasUrlPath

    -- * Scte35TimeSignalApos
    , Scte35TimeSignalApos (..)
    , mkScte35TimeSignalApos
    , stsaAdAvailOffset
    , stsaNoRegionalBlackoutFlag
    , stsaWebDeliveryAllowedFlag

    -- * TemporalFilterSettings
    , TemporalFilterSettings (..)
    , mkTemporalFilterSettings
    , tfsPostFilterSharpening
    , tfsStrength

    -- * M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- * AudioTrack
    , AudioTrack (..)
    , mkAudioTrack
    , atTrack

    -- * M2tsKlv
    , M2tsKlv (..)

    -- * AudioChannelMapping
    , AudioChannelMapping (..)
    , mkAudioChannelMapping
    , acmOutputChannel
    , acmInputChannelLevels

    -- * BlackoutSlateState
    , BlackoutSlateState (..)

    -- * H264Syntax
    , H264Syntax (..)

    -- * MultiplexSettingsSummary
    , MultiplexSettingsSummary (..)
    , mkMultiplexSettingsSummary
    , mTransportStreamBitrate

    -- * RtmpOutputSettings
    , RtmpOutputSettings (..)
    , mkRtmpOutputSettings
    , rosDestination
    , rosCertificateMode
    , rosConnectionRetryInterval
    , rosNumRetries

    -- * HlsAkamaiHttpTransferMode
    , HlsAkamaiHttpTransferMode (..)

    -- * H264GopBReference
    , H264GopBReference (..)

    -- * RtmpGroupSettings
    , RtmpGroupSettings (..)
    , mkRtmpGroupSettings
    , rgsAdMarkers
    , rgsAuthenticationScheme
    , rgsCacheFullBehavior
    , rgsCacheLength
    , rgsCaptionData
    , rgsInputLossAction
    , rgsRestartDelay

    -- * BurnInOutlineColor
    , BurnInOutlineColor (..)

    -- * BatchFailedResultModel
    , BatchFailedResultModel (..)
    , mkBatchFailedResultModel
    , bfrmArn
    , bfrmCode
    , bfrmId
    , bfrmMessage

    -- * HlsId3SegmentTaggingScheduleActionSettings
    , HlsId3SegmentTaggingScheduleActionSettings (..)
    , mkHlsId3SegmentTaggingScheduleActionSettings
    , histsasTag

    -- * GlobalConfigurationOutputTimingSource
    , GlobalConfigurationOutputTimingSource (..)

    -- * RawSettings
    , RawSettings (..)
    , mkRawSettings

    -- * HlsH265PackagingType
    , HlsH265PackagingType (..)

    -- * InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- * Scte35Descriptor
    , Scte35Descriptor (..)
    , mkScte35Descriptor
    , sdScte35DescriptorSettings

    -- * AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- * Ac3Settings
    , Ac3Settings (..)
    , mkAc3Settings
    , aBitrate
    , aBitstreamMode
    , aCodingMode
    , aDialnorm
    , aDrcProfile
    , aLfeFilter
    , aMetadataControl

    -- * Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- * AudioLanguageSelectionPolicy
    , AudioLanguageSelectionPolicy (..)

    -- * MultiplexMediaConnectOutputDestinationSettings
    , MultiplexMediaConnectOutputDestinationSettings (..)
    , mkMultiplexMediaConnectOutputDestinationSettings
    , mmcodsEntitlementArn

    -- * ImmediateModeScheduleActionStartSettings
    , ImmediateModeScheduleActionStartSettings (..)
    , mkImmediateModeScheduleActionStartSettings

    -- * Mp2CodingMode
    , Mp2CodingMode (..)

    -- * AacRawFormat
    , AacRawFormat (..)

    -- * MediaPackageOutputSettings
    , MediaPackageOutputSettings (..)
    , mkMediaPackageOutputSettings

    -- * H265Settings
    , H265Settings (..)
    , mkH265Settings
    , hFramerateNumerator
    , hFramerateDenominator
    , hAdaptiveQuantization
    , hAfdSignaling
    , hAlternativeTransferFunction
    , hBitrate
    , hBufSize
    , hColorMetadata
    , hColorSpaceSettings
    , hFilterSettings
    , hFixedAfd
    , hFlickerAq
    , hGopClosedCadence
    , hGopSize
    , hGopSizeUnits
    , hLevel
    , hLookAheadRateControl
    , hMaxBitrate
    , hMinIInterval
    , hParDenominator
    , hParNumerator
    , hProfile
    , hQvbrQualityLevel
    , hRateControlMode
    , hScanType
    , hSceneChangeDetect
    , hSlices
    , hTier
    , hTimecodeInsertion

    -- * Scte27SourceSettings
    , Scte27SourceSettings (..)
    , mkScte27SourceSettings
    , sssPid

    -- * FollowModeScheduleActionStartSettings
    , FollowModeScheduleActionStartSettings (..)
    , mkFollowModeScheduleActionStartSettings
    , fmsassReferenceActionName
    , fmsassFollowPoint

    -- * DvbSubSourceSettings
    , DvbSubSourceSettings (..)
    , mkDvbSubSourceSettings
    , dsssPid

    -- * InputDeviceActiveInput
    , InputDeviceActiveInput (..)

    -- * MultiplexSummary
    , MultiplexSummary (..)
    , mkMultiplexSummary
    , msArn
    , msAvailabilityZones
    , msId
    , msMultiplexSettings
    , msName
    , msPipelinesRunningCount
    , msProgramCount
    , msState
    , msTags

    -- * MultiplexVideoSettings
    , MultiplexVideoSettings (..)
    , mkMultiplexVideoSettings
    , mvsConstantBitrate
    , mvsStatmuxSettings

    -- * H264TemporalAq
    , H264TemporalAq (..)

    -- * HlsWebdavHttpTransferMode
    , HlsWebdavHttpTransferMode (..)

    -- * ChannelState
    , ChannelState (..)

    -- * InputDeviceConfigurableSettings
    , InputDeviceConfigurableSettings (..)
    , mkInputDeviceConfigurableSettings
    , idcsConfiguredInput
    , idcsMaxBitrate

    -- * Scte35DeliveryRestrictions
    , Scte35DeliveryRestrictions (..)
    , mkScte35DeliveryRestrictions
    , sdrDeviceRestrictions
    , sdrArchiveAllowedFlag
    , sdrWebDeliveryAllowedFlag
    , sdrNoRegionalBlackoutFlag

    -- * Eac3LfeControl
    , Eac3LfeControl (..)

    -- * Output
    , Output (..)
    , mkOutput
    , oOutputSettings
    , oAudioDescriptionNames
    , oCaptionDescriptionNames
    , oOutputName
    , oVideoDescriptionName

    -- * InputAttachment
    , InputAttachment (..)
    , mkInputAttachment
    , iaAutomaticInputFailoverSettings
    , iaInputAttachmentName
    , iaInputId
    , iaInputSettings

    -- * M2tsEbifControl
    , M2tsEbifControl (..)

    -- * DvbSubDestinationAlignment
    , DvbSubDestinationAlignment (..)

    -- * M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- * H264ScanType
    , H264ScanType (..)

    -- * InputSecurityGroup
    , InputSecurityGroup (..)
    , mkInputSecurityGroup
    , isgArn
    , isgId
    , isgInputs
    , isgState
    , isgTags
    , isgWhitelistRules

    -- * InputLossActionForUdpOut
    , InputLossActionForUdpOut (..)

    -- * Ac3DrcProfile
    , Ac3DrcProfile (..)

    -- * Scte35WebDeliveryAllowedFlag
    , Scte35WebDeliveryAllowedFlag (..)

    -- * M3u8NielsenId3Behavior
    , M3u8NielsenId3Behavior (..)

    -- * BurnInBackgroundColor
    , BurnInBackgroundColor (..)

    -- * Eac3DrcLine
    , Eac3DrcLine (..)

    -- * H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- * AncillarySourceSettings
    , AncillarySourceSettings (..)
    , mkAncillarySourceSettings
    , assSourceAncillaryChannelNumber

    -- * BatchScheduleActionDeleteRequest
    , BatchScheduleActionDeleteRequest (..)
    , mkBatchScheduleActionDeleteRequest
    , bsadrActionNames

    -- * M2tsAribCaptionsPidControl
    , M2tsAribCaptionsPidControl (..)

    -- * H265RateControlMode
    , H265RateControlMode (..)

    -- * DvbSdtOutputSdt
    , DvbSdtOutputSdt (..)

    -- * Rec709Settings
    , Rec709Settings (..)
    , mkRec709Settings

    -- * Eac3CodingMode
    , Eac3CodingMode (..)

    -- * VideoDescription
    , VideoDescription (..)
    , mkVideoDescription
    , vdName
    , vdCodecSettings
    , vdHeight
    , vdRespondToAfd
    , vdScalingBehavior
    , vdSharpness
    , vdWidth

    -- * WebvttDestinationSettings
    , WebvttDestinationSettings (..)
    , mkWebvttDestinationSettings

    -- * ChannelSummary
    , ChannelSummary (..)
    , mkChannelSummary
    , csfArn
    , csfCdiInputSpecification
    , csfChannelClass
    , csfDestinations
    , csfEgressEndpoints
    , csfId
    , csfInputAttachments
    , csfInputSpecification
    , csfLogLevel
    , csfName
    , csfPipelinesRunningCount
    , csfRoleArn
    , csfState
    , csfTags

    -- * SmoothGroupEventIdMode
    , SmoothGroupEventIdMode (..)

    -- * RtmpAdMarkers
    , RtmpAdMarkers (..)

    -- * CaptionDestinationSettings
    , CaptionDestinationSettings (..)
    , mkCaptionDestinationSettings
    , cdsAribDestinationSettings
    , cdsBurnInDestinationSettings
    , cdsDvbSubDestinationSettings
    , cdsEbuTtDDestinationSettings
    , cdsEmbeddedDestinationSettings
    , cdsEmbeddedPlusScte20DestinationSettings
    , cdsRtmpCaptionInfoDestinationSettings
    , cdsScte20PlusEmbeddedDestinationSettings
    , cdsScte27DestinationSettings
    , cdsSmpteTtDestinationSettings
    , cdsTeletextDestinationSettings
    , cdsTtmlDestinationSettings
    , cdsWebvttDestinationSettings

    -- * InputDeviceHdSettings
    , InputDeviceHdSettings (..)
    , mkInputDeviceHdSettings
    , idhsActiveInput
    , idhsConfiguredInput
    , idhsDeviceState
    , idhsFramerate
    , idhsHeight
    , idhsMaxBitrate
    , idhsScanType
    , idhsWidth

    -- * EmbeddedDestinationSettings
    , EmbeddedDestinationSettings (..)
    , mkEmbeddedDestinationSettings

    -- * H265AlternativeTransferFunction
    , H265AlternativeTransferFunction (..)

    -- * MultiplexState
    , MultiplexState (..)

    -- * AcceptHeader
    , AcceptHeader (..)

    -- * SmoothGroupStreamManifestBehavior
    , SmoothGroupStreamManifestBehavior (..)

    -- * MultiplexProgramServiceDescriptor
    , MultiplexProgramServiceDescriptor (..)
    , mkMultiplexProgramServiceDescriptor
    , mpsdProviderName
    , mpsdServiceName

    -- * M2tsAudioStreamType
    , M2tsAudioStreamType (..)

    -- * StaticKeySettings
    , StaticKeySettings (..)
    , mkStaticKeySettings
    , sksStaticKeyValue
    , sksKeyProviderServer

    -- * Scte35TimeSignalScheduleActionSettings
    , Scte35TimeSignalScheduleActionSettings (..)
    , mkScte35TimeSignalScheduleActionSettings
    , stssasScte35Descriptors

    -- * BurnInFontColor
    , BurnInFontColor (..)

    -- * H265TimecodeInsertionBehavior
    , H265TimecodeInsertionBehavior (..)

    -- * InputLossActionForMsSmoothOut
    , InputLossActionForMsSmoothOut (..)

    -- * HlsSegmentationMode
    , HlsSegmentationMode (..)

    -- * Ac3CodingMode
    , Ac3CodingMode (..)

    -- * MultiplexGroupSettings
    , MultiplexGroupSettings (..)
    , mkMultiplexGroupSettings

    -- * InputDeviceSummary
    , InputDeviceSummary (..)
    , mkInputDeviceSummary
    , idsArn
    , idsConnectionState
    , idsDeviceSettingsSyncState
    , idsDeviceUpdateStatus
    , idsHdDeviceSettings
    , idsId
    , idsMacAddress
    , idsName
    , idsNetworkSettings
    , idsSerialNumber
    , idsType

    -- * InputDestination
    , InputDestination (..)
    , mkInputDestination
    , idIp
    , idPort
    , idUrl
    , idVpc

    -- * HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- * TimecodeConfig
    , TimecodeConfig (..)
    , mkTimecodeConfig
    , tcSource
    , tcSyncThreshold

    -- * RtmpOutputCertificateMode
    , RtmpOutputCertificateMode (..)

    -- * Mp2Settings
    , Mp2Settings (..)
    , mkMp2Settings
    , msBitrate
    , msCodingMode
    , msSampleRate

    -- * DvbSubDestinationTeletextGridControl
    , DvbSubDestinationTeletextGridControl (..)

    -- * Mpeg2ScanType
    , Mpeg2ScanType (..)

    -- * OutputSettings
    , OutputSettings (..)
    , mkOutputSettings
    , osArchiveOutputSettings
    , osFrameCaptureOutputSettings
    , osHlsOutputSettings
    , osMediaPackageOutputSettings
    , osMsSmoothOutputSettings
    , osMultiplexOutputSettings
    , osRtmpOutputSettings
    , osUdpOutputSettings

    -- * EbuTtDDestinationStyleControl
    , EbuTtDDestinationStyleControl (..)

    -- * AutomaticInputFailoverSettings
    , AutomaticInputFailoverSettings (..)
    , mkAutomaticInputFailoverSettings
    , aifsSecondaryInputId
    , aifsErrorClearTimeMsec
    , aifsFailoverConditions
    , aifsInputPreference

    -- * Scte35AposWebDeliveryAllowedBehavior
    , Scte35AposWebDeliveryAllowedBehavior (..)

    -- * InputLossImageType
    , InputLossImageType (..)

    -- * H264Settings
    , H264Settings (..)
    , mkH264Settings
    , hsAdaptiveQuantization
    , hsAfdSignaling
    , hsBitrate
    , hsBufFillPct
    , hsBufSize
    , hsColorMetadata
    , hsColorSpaceSettings
    , hsEntropyEncoding
    , hsFilterSettings
    , hsFixedAfd
    , hsFlickerAq
    , hsForceFieldPictures
    , hsFramerateControl
    , hsFramerateDenominator
    , hsFramerateNumerator
    , hsGopBReference
    , hsGopClosedCadence
    , hsGopNumBFrames
    , hsGopSize
    , hsGopSizeUnits
    , hsLevel
    , hsLookAheadRateControl
    , hsMaxBitrate
    , hsMinIInterval
    , hsNumRefFrames
    , hsParControl
    , hsParDenominator
    , hsParNumerator
    , hsProfile
    , hsQualityLevel
    , hsQvbrQualityLevel
    , hsRateControlMode
    , hsScanType
    , hsSceneChangeDetect
    , hsSlices
    , hsSoftness
    , hsSpatialAq
    , hsSubgopLength
    , hsSyntax
    , hsTemporalAq
    , hsTimecodeInsertion

    -- * M3u8PcrControl
    , M3u8PcrControl (..)

    -- * FixedAfd
    , FixedAfd (..)

    -- * CdiInputSpecification
    , CdiInputSpecification (..)
    , mkCdiInputSpecification
    , cisResolution

    -- * VideoSelectorColorSpace
    , VideoSelectorColorSpace (..)

    -- * InputDeviceTransferType
    , InputDeviceTransferType (..)

    -- * ReservationMaximumBitrate
    , ReservationMaximumBitrate (..)

    -- * M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- * ChannelClass
    , ChannelClass (..)

    -- * HlsGroupSettings
    , HlsGroupSettings (..)
    , mkHlsGroupSettings
    , hgsDestination
    , hgsAdMarkers
    , hgsBaseUrlContent
    , hgsBaseUrlContent1
    , hgsBaseUrlManifest
    , hgsBaseUrlManifest1
    , hgsCaptionLanguageMappings
    , hgsCaptionLanguageSetting
    , hgsClientCache
    , hgsCodecSpecification
    , hgsConstantIv
    , hgsDirectoryStructure
    , hgsDiscontinuityTags
    , hgsEncryptionType
    , hgsHlsCdnSettings
    , hgsHlsId3SegmentTagging
    , hgsIFrameOnlyPlaylists
    , hgsIncompleteSegmentBehavior
    , hgsIndexNSegments
    , hgsInputLossAction
    , hgsIvInManifest
    , hgsIvSource
    , hgsKeepSegments
    , hgsKeyFormat
    , hgsKeyFormatVersions
    , hgsKeyProviderSettings
    , hgsManifestCompression
    , hgsManifestDurationFormat
    , hgsMinSegmentLength
    , hgsMode
    , hgsOutputSelection
    , hgsProgramDateTime
    , hgsProgramDateTimePeriod
    , hgsRedundantManifest
    , hgsSegmentLength
    , hgsSegmentationMode
    , hgsSegmentsPerSubdirectory
    , hgsStreamInfResolution
    , hgsTimedMetadataId3Frame
    , hgsTimedMetadataId3Period
    , hgsTimestampDeltaMilliseconds
    , hgsTsFileMode

    -- * DvbSubDestinationOutlineColor
    , DvbSubDestinationOutlineColor (..)

    -- * InputPreference
    , InputPreference (..)

    -- * BurnInDestinationSettings
    , BurnInDestinationSettings (..)
    , mkBurnInDestinationSettings
    , bidsAlignment
    , bidsBackgroundColor
    , bidsBackgroundOpacity
    , bidsFont
    , bidsFontColor
    , bidsFontOpacity
    , bidsFontResolution
    , bidsFontSize
    , bidsOutlineColor
    , bidsOutlineSize
    , bidsShadowColor
    , bidsShadowOpacity
    , bidsShadowXOffset
    , bidsShadowYOffset
    , bidsTeletextGridControl
    , bidsXPosition
    , bidsYPosition

    -- * WavSettings
    , WavSettings (..)
    , mkWavSettings
    , wsBitDepth
    , wsCodingMode
    , wsSampleRate

    -- * ReservationSpecialFeature
    , ReservationSpecialFeature (..)

    -- * MultiplexSettings
    , MultiplexSettings (..)
    , mkMultiplexSettings
    , mssTransportStreamBitrate
    , mssTransportStreamId
    , mssMaximumVideoBufferDelayMilliseconds
    , mssTransportStreamReservedBitrate

    -- * TtmlDestinationStyleControl
    , TtmlDestinationStyleControl (..)

    -- * Mpeg2GopSizeUnits
    , Mpeg2GopSizeUnits (..)

    -- * GlobalConfigurationLowFramerateInputs
    , GlobalConfigurationLowFramerateInputs (..)

    -- * Eac3DrcRf
    , Eac3DrcRf (..)

    -- * InputClippingSettings
    , InputClippingSettings (..)
    , mkInputClippingSettings
    , icsInputTimecodeSource
    , icsStartTimecode
    , icsStopTimecode

    -- * DeviceUpdateStatus
    , DeviceUpdateStatus (..)

    -- * LastFrameClippingBehavior
    , LastFrameClippingBehavior (..)

    -- * HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- * Mpeg2SubGopLength
    , Mpeg2SubGopLength (..)

    -- * HlsTimedMetadataScheduleActionSettings
    , HlsTimedMetadataScheduleActionSettings (..)
    , mkHlsTimedMetadataScheduleActionSettings
    , htmsasId3

    -- * DeviceSettingsSyncState
    , DeviceSettingsSyncState (..)

    -- * RtmpCaptionData
    , RtmpCaptionData (..)

    -- * Mpeg2ColorMetadata
    , Mpeg2ColorMetadata (..)

    -- * AuthenticationScheme
    , AuthenticationScheme (..)

    -- * HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- * FollowPoint
    , FollowPoint (..)

    -- * HlsOutputSettings
    , HlsOutputSettings (..)
    , mkHlsOutputSettings
    , hosHlsSettings
    , hosH265PackagingType
    , hosNameModifier
    , hosSegmentModifier

    -- * HlsIvSource
    , HlsIvSource (..)

    -- * H265FlickerAq
    , H265FlickerAq (..)

    -- * AudioCodecSettings
    , AudioCodecSettings (..)
    , mkAudioCodecSettings
    , acsAacSettings
    , acsAc3Settings
    , acsEac3Settings
    , acsMp2Settings
    , acsPassThroughSettings
    , acsWavSettings

    -- * AacInputType
    , AacInputType (..)

    -- * AacVbrQuality
    , AacVbrQuality (..)

    -- * M2tsArib
    , M2tsArib (..)

    -- * InputSourceEndBehavior
    , InputSourceEndBehavior (..)

    -- * HlsDiscontinuityTags
    , HlsDiscontinuityTags (..)

    -- * Mpeg2Settings
    , Mpeg2Settings (..)
    , mkMpeg2Settings
    , msFramerateNumerator
    , msFramerateDenominator
    , msAdaptiveQuantization
    , msAfdSignaling
    , msColorMetadata
    , msColorSpace
    , msDisplayAspectRatio
    , msFilterSettings
    , msFixedAfd
    , msGopClosedCadence
    , msGopNumBFrames
    , msGopSize
    , msGopSizeUnits
    , msScanType
    , msSubgopLength
    , msTimecodeInsertion

    -- * Scte35SpliceInsertWebDeliveryAllowedBehavior
    , Scte35SpliceInsertWebDeliveryAllowedBehavior (..)

    -- * InputClass
    , InputClass (..)

    -- * Scte35DeviceRestrictions
    , Scte35DeviceRestrictions (..)

    -- * RtmpCacheFullBehavior
    , RtmpCacheFullBehavior (..)

    -- * Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- * OfferingType
    , OfferingType (..)

    -- * H264SubGopLength
    , H264SubGopLength (..)

    -- * Multiplex
    , Multiplex (..)
    , mkMultiplex
    , mArn
    , mAvailabilityZones
    , mDestinations
    , mId
    , mMultiplexSettings
    , mName
    , mPipelinesRunningCount
    , mProgramCount
    , mState
    , mTags

    -- * CaptionDescription
    , CaptionDescription (..)
    , mkCaptionDescription
    , cdCaptionSelectorName
    , cdName
    , cdDestinationSettings
    , cdLanguageCode
    , cdLanguageDescription

    -- * H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- * M2tsScte35Control
    , M2tsScte35Control (..)

    -- * VideoSelectorColorSpaceUsage
    , VideoSelectorColorSpaceUsage (..)

    -- * StaticImageActivateScheduleActionSettings
    , StaticImageActivateScheduleActionSettings (..)
    , mkStaticImageActivateScheduleActionSettings
    , siasasImage
    , siasasDuration
    , siasasFadeIn
    , siasasFadeOut
    , siasasHeight
    , siasasImageX
    , siasasImageY
    , siasasLayer
    , siasasOpacity
    , siasasWidth

    -- * FrameCaptureIntervalUnit
    , FrameCaptureIntervalUnit (..)

    -- * ArchiveGroupSettings
    , ArchiveGroupSettings (..)
    , mkArchiveGroupSettings
    , agsDestination
    , agsRolloverInterval

    -- * AacProfile
    , AacProfile (..)

    -- * StartTimecode
    , StartTimecode (..)
    , mkStartTimecode
    , stTimecode

    -- * AudioDescriptionLanguageCodeControl
    , AudioDescriptionLanguageCodeControl (..)

    -- * CaptionLanguageMapping
    , CaptionLanguageMapping (..)
    , mkCaptionLanguageMapping
    , clmLanguageCode
    , clmLanguageDescription
    , clmCaptionChannel

    -- * CaptionSelector
    , CaptionSelector (..)
    , mkCaptionSelector
    , csName
    , csLanguageCode
    , csSelectorSettings

    -- * InputDeviceSettings
    , InputDeviceSettings (..)
    , mkInputDeviceSettings
    , idssId

    -- * HlsMediaStoreStorageClass
    , HlsMediaStoreStorageClass (..)

    -- * TransferringInputDeviceSummary
    , TransferringInputDeviceSummary (..)
    , mkTransferringInputDeviceSummary
    , tidsId
    , tidsMessage
    , tidsTargetCustomerId
    , tidsTransferType

    -- * AudioPidSelection
    , AudioPidSelection (..)
    , mkAudioPidSelection
    , apsPid

    -- * DvbSubDestinationShadowColor
    , DvbSubDestinationShadowColor (..)

    -- * InputLocation
    , InputLocation (..)
    , mkInputLocation
    , ilUri
    , ilPasswordParam
    , ilUsername

    -- * UdpGroupSettings
    , UdpGroupSettings (..)
    , mkUdpGroupSettings
    , ugsInputLossAction
    , ugsTimedMetadataId3Frame
    , ugsTimedMetadataId3Period

    -- * H264QualityLevel
    , H264QualityLevel (..)

    -- * Fmp4NielsenId3Behavior
    , Fmp4NielsenId3Behavior (..)

    -- * FrameCaptureOutputSettings
    , FrameCaptureOutputSettings (..)
    , mkFrameCaptureOutputSettings
    , fcosNameModifier

    -- * AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- * GlobalConfiguration
    , GlobalConfiguration (..)
    , mkGlobalConfiguration
    , gcInitialAudioGain
    , gcInputEndAction
    , gcInputLossBehavior
    , gcOutputLockingMode
    , gcOutputTimingSource
    , gcSupportLowFramerateInputs

    -- * UdpOutputSettings
    , UdpOutputSettings (..)
    , mkUdpOutputSettings
    , uosDestination
    , uosContainerSettings
    , uosBufferMsec
    , uosFecOutputSettings

    -- * H264Profile
    , H264Profile (..)

    -- * WavCodingMode
    , WavCodingMode (..)

    -- * MultiplexOutputDestination
    , MultiplexOutputDestination (..)
    , mkMultiplexOutputDestination
    , modMediaConnectSettings

    -- * ScheduleAction
    , ScheduleAction (..)
    , mkScheduleAction
    , saActionName
    , saScheduleActionStartSettings
    , saScheduleActionSettings

    -- * FixedModeScheduleActionStartSettings
    , FixedModeScheduleActionStartSettings (..)
    , mkFixedModeScheduleActionStartSettings
    , fmsassTime

    -- * OfferingDurationUnits
    , OfferingDurationUnits (..)

    -- * Scte35AposNoRegionalBlackoutBehavior
    , Scte35AposNoRegionalBlackoutBehavior (..)

    -- * EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- * Scte20SourceSettings
    , Scte20SourceSettings (..)
    , mkScte20SourceSettings
    , sssConvert608To708
    , sssSource608ChannelNumber

    -- * InputSettings
    , InputSettings (..)
    , mkInputSettings
    , isAudioSelectors
    , isCaptionSelectors
    , isDeblockFilter
    , isDenoiseFilter
    , isFilterStrength
    , isInputFilter
    , isNetworkInputSettings
    , isSmpte2038DataPreference
    , isSourceEndBehavior
    , isVideoSelector

    -- * InputTimecodeSource
    , InputTimecodeSource (..)

    -- * SmoothGroupCertificateMode
    , SmoothGroupCertificateMode (..)

    -- * InputSourceType
    , InputSourceType (..)

    -- * Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- * FrameCaptureGroupSettings
    , FrameCaptureGroupSettings (..)
    , mkFrameCaptureGroupSettings
    , fcgsDestination

    -- * M2tsAbsentInputAudioBehavior
    , M2tsAbsentInputAudioBehavior (..)

    -- * FecOutputIncludeFec
    , FecOutputIncludeFec (..)

    -- * NetworkInputServerValidation
    , NetworkInputServerValidation (..)

    -- * M2tsPcrControl
    , M2tsPcrControl (..)

    -- * AudioSelectorSettings
    , AudioSelectorSettings (..)
    , mkAudioSelectorSettings
    , assAudioLanguageSelection
    , assAudioPidSelection
    , assAudioTrackSelection

    -- * InputFilter
    , InputFilter (..)

    -- * RemixSettings
    , RemixSettings (..)
    , mkRemixSettings
    , rsChannelMappings
    , rsChannelsIn
    , rsChannelsOut

    -- * HlsManifestCompression
    , HlsManifestCompression (..)

    -- * InputDeviceType
    , InputDeviceType (..)

    -- * Rec601Settings
    , Rec601Settings (..)
    , mkRec601Settings

    -- * AvailSettings
    , AvailSettings (..)
    , mkAvailSettings
    , asScte35SpliceInsert
    , asScte35TimeSignalApos

    -- * DvbNitSettings
    , DvbNitSettings (..)
    , mkDvbNitSettings
    , dnsNetworkName
    , dnsNetworkId
    , dnsRepInterval

    -- * BurnInTeletextGridControl
    , BurnInTeletextGridControl (..)

    -- * MsSmoothOutputSettings
    , MsSmoothOutputSettings (..)
    , mkMsSmoothOutputSettings
    , msosH265PackagingType
    , msosNameModifier

    -- * UdpTimedMetadataId3Frame
    , UdpTimedMetadataId3Frame (..)

    -- * OutputDestinationSettings
    , OutputDestinationSettings (..)
    , mkOutputDestinationSettings
    , odsPasswordParam
    , odsStreamName
    , odsUrl
    , odsUsername

    -- * H264RateControlMode
    , H264RateControlMode (..)

    -- * BlackoutSlateNetworkEndBlackout
    , BlackoutSlateNetworkEndBlackout (..)

    -- * InputType
    , InputType (..)

    -- * M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- * ColorSpacePassthroughSettings
    , ColorSpacePassthroughSettings (..)
    , mkColorSpacePassthroughSettings

    -- * IFrameOnlyPlaylistType
    , IFrameOnlyPlaylistType (..)

    -- * DvbSubDestinationFontColor
    , DvbSubDestinationFontColor (..)

    -- * VideoDescriptionScalingBehavior
    , VideoDescriptionScalingBehavior (..)

    -- * Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- * ScheduleActionStartSettings
    , ScheduleActionStartSettings (..)
    , mkScheduleActionStartSettings
    , sassFixedModeScheduleActionStartSettings
    , sassFollowModeScheduleActionStartSettings
    , sassImmediateModeScheduleActionStartSettings

    -- * AacSettings
    , AacSettings (..)
    , mkAacSettings
    , asBitrate
    , asCodingMode
    , asInputType
    , asProfile
    , asRateControlMode
    , asRawFormat
    , asSampleRate
    , asSpec
    , asVbrQuality

    -- * Scte35ReturnToNetworkScheduleActionSettings
    , Scte35ReturnToNetworkScheduleActionSettings (..)
    , mkScte35ReturnToNetworkScheduleActionSettings
    , srtnsasSpliceEventId

    -- * EmbeddedSourceSettings
    , EmbeddedSourceSettings (..)
    , mkEmbeddedSourceSettings
    , essConvert608To708
    , essScte20Detection
    , essSource608ChannelNumber
    , essSource608TrackNumber

    -- * H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- * Mpeg2AdaptiveQuantization
    , Mpeg2AdaptiveQuantization (..)

    -- * BlackoutSlate
    , BlackoutSlate (..)
    , mkBlackoutSlate
    , bsBlackoutSlateImage
    , bsNetworkEndBlackout
    , bsNetworkEndBlackoutImage
    , bsNetworkId
    , bsState

    -- * M2tsAudioInterval
    , M2tsAudioInterval (..)

    -- * Eac3Settings
    , Eac3Settings (..)
    , mkEac3Settings
    , esAttenuationControl
    , esBitrate
    , esBitstreamMode
    , esCodingMode
    , esDcFilter
    , esDialnorm
    , esDrcLine
    , esDrcRf
    , esLfeControl
    , esLfeFilter
    , esLoRoCenterMixLevel
    , esLoRoSurroundMixLevel
    , esLtRtCenterMixLevel
    , esLtRtSurroundMixLevel
    , esMetadataControl
    , esPassthroughControl
    , esPhaseControl
    , esStereoDownmix
    , esSurroundExMode
    , esSurroundMode

    -- * Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- * InputState
    , InputState (..)

    -- * MediaConnectFlowRequest
    , MediaConnectFlowRequest (..)
    , mkMediaConnectFlowRequest
    , mcfrFlowArn

    -- * HlsBasicPutSettings
    , HlsBasicPutSettings (..)
    , mkHlsBasicPutSettings
    , hbpsConnectionRetryInterval
    , hbpsFilecacheDuration
    , hbpsNumRetries
    , hbpsRestartDelay

    -- * EncoderSettings
    , EncoderSettings (..)
    , mkEncoderSettings
    , esVideoDescriptions
    , esAudioDescriptions
    , esOutputGroups
    , esTimecodeConfig
    , esAvailBlanking
    , esAvailConfiguration
    , esBlackoutSlate
    , esCaptionDescriptions
    , esFeatureActivations
    , esGlobalConfiguration
    , esNielsenConfiguration

    -- * InputDeviceScanType
    , InputDeviceScanType (..)

    -- * Scte35ArchiveAllowedFlag
    , Scte35ArchiveAllowedFlag (..)

    -- * AudioDescriptionAudioTypeControl
    , AudioDescriptionAudioTypeControl (..)

    -- * H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- * ContentType
    , ContentType (..)

    -- * H265ColorMetadata
    , H265ColorMetadata (..)

    -- * M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- * Scte35SpliceInsertNoRegionalBlackoutBehavior
    , Scte35SpliceInsertNoRegionalBlackoutBehavior (..)

    -- * InputDeviceState
    , InputDeviceState (..)

    -- * StandardHlsSettings
    , StandardHlsSettings (..)
    , mkStandardHlsSettings
    , shsM3u8Settings
    , shsAudioRenditionSets

    -- * VideoSelectorPid
    , VideoSelectorPid (..)
    , mkVideoSelectorPid
    , vspPid

    -- * Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- * M2tsRateMode
    , M2tsRateMode (..)

    -- * HlsWebdavSettings
    , HlsWebdavSettings (..)
    , mkHlsWebdavSettings
    , hwsConnectionRetryInterval
    , hwsFilecacheDuration
    , hwsHttpTransferMode
    , hwsNumRetries
    , hwsRestartDelay

    -- * StaticImageDeactivateScheduleActionSettings
    , StaticImageDeactivateScheduleActionSettings (..)
    , mkStaticImageDeactivateScheduleActionSettings
    , sidsasFadeOut
    , sidsasLayer

    -- * DvbTdtSettings
    , DvbTdtSettings (..)
    , mkDvbTdtSettings
    , dtsRepInterval

    -- * AribDestinationSettings
    , AribDestinationSettings (..)
    , mkAribDestinationSettings

    -- * InputLossActionForHlsOut
    , InputLossActionForHlsOut (..)

    -- * Scte35DescriptorSettings
    , Scte35DescriptorSettings (..)
    , mkScte35DescriptorSettings
    , sdsSegmentationDescriptorScte35DescriptorSettings

    -- * Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- * InputWhitelistRuleCidr
    , InputWhitelistRuleCidr (..)
    , mkInputWhitelistRuleCidr
    , iwrcCidr
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.MediaLive.Types.H264FlickerAq
  
  
import Network.AWS.MediaLive.Types.ReservationVideoQuality
  
import Network.AWS.MediaLive.Types.H265FilterSettings
  
import Network.AWS.MediaLive.Types.HlsInputSettings
  
import Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
  
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
  
import Network.AWS.MediaLive.Types.Eac3PhaseControl
  
import Network.AWS.MediaLive.Types.AudioLanguageSelection
  
import Network.AWS.MediaLive.Types.PreferredChannelPipeline
  
import Network.AWS.MediaLive.Types.VideoSelector
  
import Network.AWS.MediaLive.Types.InputLossFailoverSettings
  
import Network.AWS.MediaLive.Types.HlsMode
  
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode
  
import Network.AWS.MediaLive.Types.Eac3SurroundExMode
  
import Network.AWS.MediaLive.Types.H264ParControl
  
import Network.AWS.MediaLive.Types.H264ColorSpaceSettings
  
import Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
  
import Network.AWS.MediaLive.Types.HlsStreamInfResolution
  
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
  
import Network.AWS.MediaLive.Types.HlsCodecSpecification
  
import Network.AWS.MediaLive.Types.FrameCaptureSettings
  
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
  
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
  
import Network.AWS.MediaLive.Types.Mpeg2ColorSpace
  
import Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
  
import Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
  
import Network.AWS.MediaLive.Types.MultiplexProgram
  
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
  
import Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
  
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
  
import Network.AWS.MediaLive.Types.BurnInShadowColor
  
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
  
import Network.AWS.MediaLive.Types.PassThroughSettings
  
import Network.AWS.MediaLive.Types.M2tsBufferModel
  
import Network.AWS.MediaLive.Types.FailoverCondition
  
import Network.AWS.MediaLive.Types.InputDeviceRequest
  
import Network.AWS.MediaLive.Types.InputDestinationVpc
  
import Network.AWS.MediaLive.Types.TimecodeConfigSource
  
import Network.AWS.MediaLive.Types.ReservationState
  
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
  
import Network.AWS.MediaLive.Types.ReservationMaximumFramerate
  
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
  
import Network.AWS.MediaLive.Types.PipelineId
  
import Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
  
import Network.AWS.MediaLive.Types.HlsRedundantManifest
  
import Network.AWS.MediaLive.Types.H265Level
  
import Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
  
import Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
  
import Network.AWS.MediaLive.Types.H264SpatialAq
  
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
  
import Network.AWS.MediaLive.Types.VideoCodecSettings
  
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
  
import Network.AWS.MediaLive.Types.InputDeviceIpScheme
  
import Network.AWS.MediaLive.Types.FailoverConditionSettings
  
import Network.AWS.MediaLive.Types.InputVpcRequest
  
import Network.AWS.MediaLive.Types.HlsClientCache
  
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
  
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
  
import Network.AWS.MediaLive.Types.EmbeddedScte20Detection
  
import Network.AWS.MediaLive.Types.H265LookAheadRateControl
  
import Network.AWS.MediaLive.Types.H265AdaptiveQuantization
  
import Network.AWS.MediaLive.Types.ReservationResourceType
  
import Network.AWS.MediaLive.Types.FecOutputSettings
  
import Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
  
import Network.AWS.MediaLive.Types.H265Profile
  
import Network.AWS.MediaLive.Types.HlsSettings
  
import Network.AWS.MediaLive.Types.LogLevel
  
import Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
  
import Network.AWS.MediaLive.Types.InputSourceRequest
  
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
  
import Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
  
import Network.AWS.MediaLive.Types.Hdr10Settings
  
import Network.AWS.MediaLive.Types.InputWhitelistRule
  
import Network.AWS.MediaLive.Types.H264FilterSettings
  
import Network.AWS.MediaLive.Types.H265ColorSpaceSettings
  
import Network.AWS.MediaLive.Types.AfdSignaling
  
import Network.AWS.MediaLive.Types.AacRateControlMode
  
import Network.AWS.MediaLive.Types.HlsTsFileMode
  
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
  
import Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
  
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
  
import Network.AWS.MediaLive.Types.InputLossActionForRtmpOut
  
import Network.AWS.MediaLive.Types.OutputGroupSettings
  
import Network.AWS.MediaLive.Types.TeletextSourceSettings
  
import Network.AWS.MediaLive.Types.M2tsSettings
  
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
  
import Network.AWS.MediaLive.Types.Smpte2038DataPreference
  
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
  
import Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior
  
import Network.AWS.MediaLive.Types.NetworkInputSettings
  
import Network.AWS.MediaLive.Types.Fmp4HlsSettings
  
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
  
import Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
  
import Network.AWS.MediaLive.Types.H264FramerateControl
  
import Network.AWS.MediaLive.Types.Mpeg2FilterSettings
  
import Network.AWS.MediaLive.Types.H264ForceFieldPictures
  
import Network.AWS.MediaLive.Types.InputCodec
  
import Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
  
import Network.AWS.MediaLive.Types.InputResolution
  
import Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
  
import Network.AWS.MediaLive.Types.H264EntropyEncoding
  
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
  
import Network.AWS.MediaLive.Types.HlsIvInManifest
  
import Network.AWS.MediaLive.Types.HlsOutputSelection
  
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
  
import Network.AWS.MediaLive.Types.InputSecurityGroupState
  
import Network.AWS.MediaLive.Types.AribSourceSettings
  
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
  
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
  
import Network.AWS.MediaLive.Types.MediaConnectFlow
  
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
  
import Network.AWS.MediaLive.Types.Reservation
  
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
  
import Network.AWS.MediaLive.Types.AudioDescription
  
import Network.AWS.MediaLive.Types.Offering
  
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
  
import Network.AWS.MediaLive.Types.HlsEncryptionType
  
import Network.AWS.MediaLive.Types.MultiplexProgramSummary
  
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
  
import Network.AWS.MediaLive.Types.AacCodingMode
  
import Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
  
import Network.AWS.MediaLive.Types.OutputGroup
  
import Network.AWS.MediaLive.Types.H264ColorMetadata
  
import Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
  
import Network.AWS.MediaLive.Types.StopTimecode
  
import Network.AWS.MediaLive.Types.AudioSelector
  
import Network.AWS.MediaLive.Types.H264AdaptiveQuantization
  
import Network.AWS.MediaLive.Types.H264LookAheadRateControl
  
import Network.AWS.MediaLive.Types.OutputDestination
  
import Network.AWS.MediaLive.Types.HlsCdnSettings
  
import Network.AWS.MediaLive.Types.InputSpecification
  
import Network.AWS.MediaLive.Types.InputDestinationRequest
  
import Network.AWS.MediaLive.Types.OutputLocationRef
  
import Network.AWS.MediaLive.Types.Ac3MetadataControl
  
import Network.AWS.MediaLive.Types.BurnInAlignment
  
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
  
import Network.AWS.MediaLive.Types.HlsDirectoryStructure
  
import Network.AWS.MediaLive.Types.AvailConfiguration
  
import Network.AWS.MediaLive.Types.InputMaximumBitrate
  
import Network.AWS.MediaLive.Types.M2tsCcDescriptor
  
import Network.AWS.MediaLive.Types.UdpContainerSettings
  
import Network.AWS.MediaLive.Types.PipelineDetail
  
import Network.AWS.MediaLive.Types.H265Tier
  
import Network.AWS.MediaLive.Types.InputSource
  
import Network.AWS.MediaLive.Types.Channel
  
import Network.AWS.MediaLive.Types.Eac3DcFilter
  
import Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
  
import Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
  
import Network.AWS.MediaLive.Types.FeatureActivations
  
import Network.AWS.MediaLive.Types.HlsAdMarkers
  
import Network.AWS.MediaLive.Types.InputChannelLevel
  
import Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
  
import Network.AWS.MediaLive.Types.KeyProviderSettings
  
import Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
  
import Network.AWS.MediaLive.Types.ReservationCodec
  
import Network.AWS.MediaLive.Types.InputDeblockFilter
  
import Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
  
  
import Network.AWS.MediaLive.Types.ReservationResolution
  
import Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
  
import Network.AWS.MediaLive.Types.H265ScanType
  
import Network.AWS.MediaLive.Types.AacSpec
  
import Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
  
import Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
  
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
  
import Network.AWS.MediaLive.Types.AudioType
  
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
  
import Network.AWS.MediaLive.Types.NielsenConfiguration
  
import Network.AWS.MediaLive.Types.CdiInputResolution
  
import Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
  
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
  
import Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
  
import Network.AWS.MediaLive.Types.Input
  
import Network.AWS.MediaLive.Types.DvbSdtSettings
  
import Network.AWS.MediaLive.Types.Scte20Convert608To708
  
import Network.AWS.MediaLive.Types.M3u8Settings
  
import Network.AWS.MediaLive.Types.AvailBlankingState
  
import Network.AWS.MediaLive.Types.Eac3MetadataControl
  
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
  
import Network.AWS.MediaLive.Types.H264Level
  
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
  
import Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
  
import Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
  
import Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
  
import Network.AWS.MediaLive.Types.ScheduleActionSettings
  
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
  
import Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
  
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
  
import Network.AWS.MediaLive.Types.TemporalFilterStrength
  
import Network.AWS.MediaLive.Types.InputLossBehavior
  
import Network.AWS.MediaLive.Types.Ac3BitstreamMode
  
import Network.AWS.MediaLive.Types.VideoSelectorSettings
  
import Network.AWS.MediaLive.Types.AvailBlanking
  
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
  
import Network.AWS.MediaLive.Types.AudioTrackSelection
  
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
  
import Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
  
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
  
import Network.AWS.MediaLive.Types.TemporalFilterSettings
  
import Network.AWS.MediaLive.Types.M2tsAudioBufferModel
  
import Network.AWS.MediaLive.Types.AudioTrack
  
import Network.AWS.MediaLive.Types.M2tsKlv
  
import Network.AWS.MediaLive.Types.AudioChannelMapping
  
import Network.AWS.MediaLive.Types.BlackoutSlateState
  
import Network.AWS.MediaLive.Types.H264Syntax
  
  
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
  
import Network.AWS.MediaLive.Types.RtmpOutputSettings
  
import Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode
  
import Network.AWS.MediaLive.Types.H264GopBReference
  
import Network.AWS.MediaLive.Types.RtmpGroupSettings
  
import Network.AWS.MediaLive.Types.BurnInOutlineColor
  
import Network.AWS.MediaLive.Types.BatchFailedResultModel
  
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
  
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
  
import Network.AWS.MediaLive.Types.RawSettings
  
import Network.AWS.MediaLive.Types.HlsH265PackagingType
  
import Network.AWS.MediaLive.Types.InputDenoiseFilter
  
import Network.AWS.MediaLive.Types.Scte35Descriptor
  
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
  
  
import Network.AWS.MediaLive.Types.Ac3Settings
  
import Network.AWS.MediaLive.Types.Eac3BitstreamMode
  
import Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
  
import Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
  
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
  
import Network.AWS.MediaLive.Types.Mp2CodingMode
  
import Network.AWS.MediaLive.Types.AacRawFormat
  
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
  
import Network.AWS.MediaLive.Types.H265Settings
  
import Network.AWS.MediaLive.Types.Scte27SourceSettings
  
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
  
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
  
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
  
import Network.AWS.MediaLive.Types.MultiplexSummary
  
import Network.AWS.MediaLive.Types.MultiplexVideoSettings
  
  
import Network.AWS.MediaLive.Types.H264TemporalAq
  
import Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
  
import Network.AWS.MediaLive.Types.ChannelState
  
import Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
  
import Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
  
import Network.AWS.MediaLive.Types.Eac3LfeControl
  
import Network.AWS.MediaLive.Types.Output
  
import Network.AWS.MediaLive.Types.InputAttachment
  
import Network.AWS.MediaLive.Types.M2tsEbifControl
  
import Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
  
import Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
  
import Network.AWS.MediaLive.Types.H264ScanType
  
import Network.AWS.MediaLive.Types.InputSecurityGroup
  
import Network.AWS.MediaLive.Types.InputLossActionForUdpOut
  
import Network.AWS.MediaLive.Types.Ac3DrcProfile
  
import Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
  
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
  
import Network.AWS.MediaLive.Types.BurnInBackgroundColor
  
import Network.AWS.MediaLive.Types.Eac3DrcLine
  
import Network.AWS.MediaLive.Types.H264SceneChangeDetect
  
import Network.AWS.MediaLive.Types.AncillarySourceSettings
  
import Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
  
import Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
  
import Network.AWS.MediaLive.Types.H265RateControlMode
  
import Network.AWS.MediaLive.Types.DvbSdtOutputSdt
  
import Network.AWS.MediaLive.Types.Rec709Settings
  
import Network.AWS.MediaLive.Types.Eac3CodingMode
  
import Network.AWS.MediaLive.Types.VideoDescription
  
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
  
import Network.AWS.MediaLive.Types.ChannelSummary
  
import Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
  
import Network.AWS.MediaLive.Types.RtmpAdMarkers
  
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
  
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
  
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
  
import Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
  
import Network.AWS.MediaLive.Types.MultiplexState
  
import Network.AWS.MediaLive.Types.AcceptHeader
  
import Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
  
import Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
  
import Network.AWS.MediaLive.Types.M2tsAudioStreamType
  
import Network.AWS.MediaLive.Types.StaticKeySettings
  
import Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
  
import Network.AWS.MediaLive.Types.BurnInFontColor
  
import Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
  
import Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
  
import Network.AWS.MediaLive.Types.HlsSegmentationMode
  
import Network.AWS.MediaLive.Types.Ac3CodingMode
  
  
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
  
import Network.AWS.MediaLive.Types.InputDeviceSummary
  
import Network.AWS.MediaLive.Types.InputDestination
  
import Network.AWS.MediaLive.Types.HlsProgramDateTime
  
import Network.AWS.MediaLive.Types.TimecodeConfig
  
import Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
  
import Network.AWS.MediaLive.Types.Mp2Settings
  
import Network.AWS.MediaLive.Types.DvbSubDestinationTeletextGridControl
  
import Network.AWS.MediaLive.Types.Mpeg2ScanType
  
import Network.AWS.MediaLive.Types.OutputSettings
  
import Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl
  
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
  
import Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
  
import Network.AWS.MediaLive.Types.InputLossImageType
  
import Network.AWS.MediaLive.Types.H264Settings
  
import Network.AWS.MediaLive.Types.M3u8PcrControl
  
import Network.AWS.MediaLive.Types.FixedAfd
  
import Network.AWS.MediaLive.Types.CdiInputSpecification
  
import Network.AWS.MediaLive.Types.VideoSelectorColorSpace
  
import Network.AWS.MediaLive.Types.InputDeviceTransferType
  
import Network.AWS.MediaLive.Types.ReservationMaximumBitrate
  
import Network.AWS.MediaLive.Types.M2tsEsRateInPes
  
import Network.AWS.MediaLive.Types.ChannelClass
  
import Network.AWS.MediaLive.Types.HlsGroupSettings
  
import Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
  
import Network.AWS.MediaLive.Types.InputPreference
  
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
  
import Network.AWS.MediaLive.Types.WavSettings
  
import Network.AWS.MediaLive.Types.ReservationSpecialFeature
  
import Network.AWS.MediaLive.Types.MultiplexSettings
  
import Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
  
  
import Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits
  
import Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
  
import Network.AWS.MediaLive.Types.Eac3DrcRf
  
import Network.AWS.MediaLive.Types.InputClippingSettings
  
import Network.AWS.MediaLive.Types.DeviceUpdateStatus
  
import Network.AWS.MediaLive.Types.LastFrameClippingBehavior
  
import Network.AWS.MediaLive.Types.HlsManifestDurationFormat
  
import Network.AWS.MediaLive.Types.Mpeg2SubGopLength
  
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
  
import Network.AWS.MediaLive.Types.DeviceSettingsSyncState
  
import Network.AWS.MediaLive.Types.RtmpCaptionData
  
import Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
  
import Network.AWS.MediaLive.Types.AuthenticationScheme
  
import Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
  
import Network.AWS.MediaLive.Types.FollowPoint
  
import Network.AWS.MediaLive.Types.HlsOutputSettings
  
import Network.AWS.MediaLive.Types.HlsIvSource
  
import Network.AWS.MediaLive.Types.H265FlickerAq
  
import Network.AWS.MediaLive.Types.AudioCodecSettings
  
import Network.AWS.MediaLive.Types.AacInputType
  
import Network.AWS.MediaLive.Types.AacVbrQuality
  
import Network.AWS.MediaLive.Types.M2tsArib
  
import Network.AWS.MediaLive.Types.InputSourceEndBehavior
  
import Network.AWS.MediaLive.Types.HlsDiscontinuityTags
  
import Network.AWS.MediaLive.Types.Mpeg2Settings
  
import Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  
import Network.AWS.MediaLive.Types.InputClass
  
import Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
  
import Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
  
import Network.AWS.MediaLive.Types.Eac3StereoDownmix
  
  
import Network.AWS.MediaLive.Types.OfferingType
  
import Network.AWS.MediaLive.Types.H264SubGopLength
  
import Network.AWS.MediaLive.Types.Multiplex
  
import Network.AWS.MediaLive.Types.CaptionDescription
  
import Network.AWS.MediaLive.Types.H264GopSizeUnits
  
import Network.AWS.MediaLive.Types.M2tsScte35Control
  
import Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
  
import Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
  
import Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
  
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
  
import Network.AWS.MediaLive.Types.AacProfile
  
import Network.AWS.MediaLive.Types.StartTimecode
  
import Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
  
import Network.AWS.MediaLive.Types.CaptionLanguageMapping
  
import Network.AWS.MediaLive.Types.CaptionSelector
  
import Network.AWS.MediaLive.Types.InputDeviceSettings
  
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
  
import Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
  
import Network.AWS.MediaLive.Types.AudioPidSelection
  
import Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
  
import Network.AWS.MediaLive.Types.InputLocation
  
import Network.AWS.MediaLive.Types.UdpGroupSettings
  
import Network.AWS.MediaLive.Types.H264QualityLevel
  
import Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
  
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
  
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
  
import Network.AWS.MediaLive.Types.GlobalConfiguration
  
import Network.AWS.MediaLive.Types.UdpOutputSettings
  
import Network.AWS.MediaLive.Types.H264Profile
  
import Network.AWS.MediaLive.Types.WavCodingMode
  
import Network.AWS.MediaLive.Types.MultiplexOutputDestination
  
import Network.AWS.MediaLive.Types.ScheduleAction
  
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
  
import Network.AWS.MediaLive.Types.OfferingDurationUnits
  
import Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
  
import Network.AWS.MediaLive.Types.EmbeddedConvert608To708
  
import Network.AWS.MediaLive.Types.Scte20SourceSettings
  
import Network.AWS.MediaLive.Types.InputSettings
  
import Network.AWS.MediaLive.Types.InputTimecodeSource
  
import Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
  
import Network.AWS.MediaLive.Types.InputSourceType
  
import Network.AWS.MediaLive.Types.Eac3LfeFilter
  
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
  
import Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
  
import Network.AWS.MediaLive.Types.FecOutputIncludeFec
  
import Network.AWS.MediaLive.Types.NetworkInputServerValidation
  
import Network.AWS.MediaLive.Types.M2tsPcrControl
  
import Network.AWS.MediaLive.Types.AudioSelectorSettings
  
import Network.AWS.MediaLive.Types.InputFilter
  
import Network.AWS.MediaLive.Types.RemixSettings
  
import Network.AWS.MediaLive.Types.HlsManifestCompression
  
import Network.AWS.MediaLive.Types.InputDeviceType
  
import Network.AWS.MediaLive.Types.Rec601Settings
  
import Network.AWS.MediaLive.Types.AvailSettings
  
import Network.AWS.MediaLive.Types.DvbNitSettings
  
import Network.AWS.MediaLive.Types.BurnInTeletextGridControl
  
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
  
import Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
  
import Network.AWS.MediaLive.Types.OutputDestinationSettings
  
import Network.AWS.MediaLive.Types.H264RateControlMode
  
import Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
  
import Network.AWS.MediaLive.Types.InputType
  
import Network.AWS.MediaLive.Types.M2tsEbpPlacement
  
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
  
import Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
  
import Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
  
import Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
  
import Network.AWS.MediaLive.Types.Ac3LfeFilter
  
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
  
import Network.AWS.MediaLive.Types.AacSettings
  
import Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
  
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
  
import Network.AWS.MediaLive.Types.H265SceneChangeDetect
  
import Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization
  
import Network.AWS.MediaLive.Types.BlackoutSlate
  
import Network.AWS.MediaLive.Types.M2tsAudioInterval
  
import Network.AWS.MediaLive.Types.Eac3Settings
  
import Network.AWS.MediaLive.Types.Eac3AttenuationControl
  
import Network.AWS.MediaLive.Types.InputState
  
import Network.AWS.MediaLive.Types.MediaConnectFlowRequest
  
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
  
import Network.AWS.MediaLive.Types.EncoderSettings
  
import Network.AWS.MediaLive.Types.InputDeviceScanType
  
import Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
  
  
import Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
  
import Network.AWS.MediaLive.Types.H265GopSizeUnits
  
import Network.AWS.MediaLive.Types.ContentType
  
import Network.AWS.MediaLive.Types.H265ColorMetadata
  
import Network.AWS.MediaLive.Types.M2tsSegmentationStyle
  
import Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
  
import Network.AWS.MediaLive.Types.InputDeviceState
  
import Network.AWS.MediaLive.Types.StandardHlsSettings
  
import Network.AWS.MediaLive.Types.VideoSelectorPid
  
import Network.AWS.MediaLive.Types.Eac3PassthroughControl
  
import Network.AWS.MediaLive.Types.M2tsRateMode
  
import Network.AWS.MediaLive.Types.HlsWebdavSettings
  
import Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
  
import Network.AWS.MediaLive.Types.DvbTdtSettings
  
import Network.AWS.MediaLive.Types.AribDestinationSettings
  
import Network.AWS.MediaLive.Types.InputLossActionForHlsOut
  
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
  
import Network.AWS.MediaLive.Types.Eac3SurroundMode
  
import Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
  

-- | API version @2017-10-14@ of the Amazon Elemental MediaLive SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MediaLive",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "medialive",
                 Core._svcVersion = "2017-10-14", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MediaLive",
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

-- | Placeholder documentation for GatewayTimeoutException
_GatewayTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GatewayTimeoutException
  = Core._MatchServiceError mkServiceConfig "GatewayTimeoutException"
      Core.. Core.hasStatues 504
{-# INLINEABLE _GatewayTimeoutException #-}
{-# DEPRECATED _GatewayTimeoutException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for UnprocessableEntityException
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException
  = Core._MatchServiceError mkServiceConfig
      "UnprocessableEntityException"
      Core.. Core.hasStatues 422
{-# INLINEABLE _UnprocessableEntityException #-}
{-# DEPRECATED _UnprocessableEntityException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for ConflictException
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for ForbiddenException
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException
  = Core._MatchServiceError mkServiceConfig "ForbiddenException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _ForbiddenException #-}
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for NotFoundException
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for TooManyRequestsException
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for InternalServerErrorException
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException
  = Core._MatchServiceError mkServiceConfig
      "InternalServerErrorException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServerErrorException #-}
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for BadGatewayException
_BadGatewayException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadGatewayException
  = Core._MatchServiceError mkServiceConfig "BadGatewayException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _BadGatewayException #-}
{-# DEPRECATED _BadGatewayException "Use generic-lens or generic-optics instead"  #-}

-- | Placeholder documentation for BadRequestException
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}
