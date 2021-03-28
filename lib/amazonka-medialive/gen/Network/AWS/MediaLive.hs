{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API for AWS Elemental MediaLive
module Network.AWS.MediaLive
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** GatewayTimeoutException
    , _GatewayTimeoutException

    -- ** UnprocessableEntityException
    , _UnprocessableEntityException

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

    -- ** BadGatewayException
    , _BadGatewayException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- ** ChannelRunning
    , mkChannelRunning

    -- ** InputAttached
    , mkInputAttached

    -- ** MultiplexRunning
    , mkMultiplexRunning

    -- ** MultiplexDeleted
    , mkMultiplexDeleted

    -- ** InputDetached
    , mkInputDetached

    -- ** InputDeleted
    , mkInputDeleted

    -- ** ChannelStopped
    , mkChannelStopped

    -- ** MultiplexCreated
    , mkMultiplexCreated

    -- ** ChannelCreated
    , mkChannelCreated

    -- ** ChannelDeleted
    , mkChannelDeleted

    -- ** MultiplexStopped
    , mkMultiplexStopped

    -- * Operations
    -- $operations

    -- ** UpdateChannelClass 
    , module Network.AWS.MediaLive.UpdateChannelClass

    -- ** ListMultiplexes (Paginated)
    , module Network.AWS.MediaLive.ListMultiplexes

    -- ** BatchStart 
    , module Network.AWS.MediaLive.BatchStart

    -- ** CreateMultiplex 
    , module Network.AWS.MediaLive.CreateMultiplex

    -- ** ListInputDeviceTransfers (Paginated)
    , module Network.AWS.MediaLive.ListInputDeviceTransfers

    -- ** ListInputDevices (Paginated)
    , module Network.AWS.MediaLive.ListInputDevices

    -- ** ListInputs (Paginated)
    , module Network.AWS.MediaLive.ListInputs

    -- ** DescribeInputDeviceThumbnail 
    , module Network.AWS.MediaLive.DescribeInputDeviceThumbnail

    -- ** ListChannels (Paginated)
    , module Network.AWS.MediaLive.ListChannels

    -- ** DescribeInputSecurityGroup 
    , module Network.AWS.MediaLive.DescribeInputSecurityGroup

    -- ** CreateInput 
    , module Network.AWS.MediaLive.CreateInput

    -- ** ListTagsForResource 
    , module Network.AWS.MediaLive.ListTagsForResource

    -- ** DeleteChannel 
    , module Network.AWS.MediaLive.DeleteChannel

    -- ** UpdateChannel 
    , module Network.AWS.MediaLive.UpdateChannel

    -- ** AcceptInputDeviceTransfer 
    , module Network.AWS.MediaLive.AcceptInputDeviceTransfer

    -- ** DescribeReservation 
    , module Network.AWS.MediaLive.DescribeReservation

    -- ** CreateTags 
    , module Network.AWS.MediaLive.CreateTags

    -- ** StopMultiplex 
    , module Network.AWS.MediaLive.StopMultiplex

    -- ** DeleteTags 
    , module Network.AWS.MediaLive.DeleteTags

    -- ** CreateInputSecurityGroup 
    , module Network.AWS.MediaLive.CreateInputSecurityGroup

    -- ** StartChannel 
    , module Network.AWS.MediaLive.StartChannel

    -- ** CancelInputDeviceTransfer 
    , module Network.AWS.MediaLive.CancelInputDeviceTransfer

    -- ** ListInputSecurityGroups (Paginated)
    , module Network.AWS.MediaLive.ListInputSecurityGroups

    -- ** DeleteReservation 
    , module Network.AWS.MediaLive.DeleteReservation

    -- ** UpdateReservation 
    , module Network.AWS.MediaLive.UpdateReservation

    -- ** BatchStop 
    , module Network.AWS.MediaLive.BatchStop

    -- ** DeleteSchedule 
    , module Network.AWS.MediaLive.DeleteSchedule

    -- ** CreateChannel 
    , module Network.AWS.MediaLive.CreateChannel

    -- ** DeleteInput 
    , module Network.AWS.MediaLive.DeleteInput

    -- ** UpdateInput 
    , module Network.AWS.MediaLive.UpdateInput

    -- ** UpdateInputDevice 
    , module Network.AWS.MediaLive.UpdateInputDevice

    -- ** RejectInputDeviceTransfer 
    , module Network.AWS.MediaLive.RejectInputDeviceTransfer

    -- ** DescribeOffering 
    , module Network.AWS.MediaLive.DescribeOffering

    -- ** TransferInputDevice 
    , module Network.AWS.MediaLive.TransferInputDevice

    -- ** DeleteMultiplexProgram 
    , module Network.AWS.MediaLive.DeleteMultiplexProgram

    -- ** UpdateMultiplexProgram 
    , module Network.AWS.MediaLive.UpdateMultiplexProgram

    -- ** BatchDelete 
    , module Network.AWS.MediaLive.BatchDelete

    -- ** ListMultiplexPrograms (Paginated)
    , module Network.AWS.MediaLive.ListMultiplexPrograms

    -- ** DescribeMultiplex 
    , module Network.AWS.MediaLive.DescribeMultiplex

    -- ** BatchUpdateSchedule 
    , module Network.AWS.MediaLive.BatchUpdateSchedule

    -- ** CreateMultiplexProgram 
    , module Network.AWS.MediaLive.CreateMultiplexProgram

    -- ** DescribeSchedule (Paginated)
    , module Network.AWS.MediaLive.DescribeSchedule

    -- ** StartMultiplex 
    , module Network.AWS.MediaLive.StartMultiplex

    -- ** StopChannel 
    , module Network.AWS.MediaLive.StopChannel

    -- ** DescribeInput 
    , module Network.AWS.MediaLive.DescribeInput

    -- ** PurchaseOffering 
    , module Network.AWS.MediaLive.PurchaseOffering

    -- ** DescribeInputDevice 
    , module Network.AWS.MediaLive.DescribeInputDevice

    -- ** DescribeChannel 
    , module Network.AWS.MediaLive.DescribeChannel

    -- ** UpdateInputSecurityGroup 
    , module Network.AWS.MediaLive.UpdateInputSecurityGroup

    -- ** DeleteInputSecurityGroup 
    , module Network.AWS.MediaLive.DeleteInputSecurityGroup

    -- ** ListReservations (Paginated)
    , module Network.AWS.MediaLive.ListReservations

    -- ** DeleteMultiplex 
    , module Network.AWS.MediaLive.DeleteMultiplex

    -- ** UpdateMultiplex 
    , module Network.AWS.MediaLive.UpdateMultiplex

    -- ** DescribeMultiplexProgram 
    , module Network.AWS.MediaLive.DescribeMultiplexProgram

    -- ** ListOfferings (Paginated)
    , module Network.AWS.MediaLive.ListOfferings

    -- * Types

    -- ** H264FlickerAq
    , H264FlickerAq (..)

    -- ** ReservationVideoQuality
    , ReservationVideoQuality (..)

    -- ** H265FilterSettings
    , H265FilterSettings (..)
    , mkH265FilterSettings
    , hTemporalFilterSettings

    -- ** HlsInputSettings
    , HlsInputSettings (..)
    , mkHlsInputSettings
    , hisBandwidth
    , hisBufferSegments
    , hisRetries
    , hisRetryInterval

    -- ** M2tsNielsenId3Behavior
    , M2tsNielsenId3Behavior (..)

    -- ** MediaPackageGroupSettings
    , MediaPackageGroupSettings (..)
    , mkMediaPackageGroupSettings
    , mpgsDestination

    -- ** Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- ** AudioLanguageSelection
    , AudioLanguageSelection (..)
    , mkAudioLanguageSelection
    , alsLanguageCode
    , alsLanguageSelectionPolicy

    -- ** PreferredChannelPipeline
    , PreferredChannelPipeline (..)

    -- ** VideoSelector
    , VideoSelector (..)
    , mkVideoSelector
    , vsColorSpace
    , vsColorSpaceUsage
    , vsSelectorSettings

    -- ** InputLossFailoverSettings
    , InputLossFailoverSettings (..)
    , mkInputLossFailoverSettings
    , ilfsInputLossThresholdMsec

    -- ** HlsMode
    , HlsMode (..)

    -- ** GlobalConfigurationOutputLockingMode
    , GlobalConfigurationOutputLockingMode (..)

    -- ** Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- ** H264ParControl
    , H264ParControl (..)

    -- ** H264ColorSpaceSettings
    , H264ColorSpaceSettings (..)
    , mkH264ColorSpaceSettings
    , hcssColorSpacePassthroughSettings
    , hcssRec601Settings
    , hcssRec709Settings

    -- ** FeatureActivationsInputPrepareScheduleActions
    , FeatureActivationsInputPrepareScheduleActions (..)

    -- ** HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- ** TeletextDestinationSettings
    , TeletextDestinationSettings (..)
    , mkTeletextDestinationSettings

    -- ** HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- ** FrameCaptureSettings
    , FrameCaptureSettings (..)
    , mkFrameCaptureSettings
    , fcsCaptureInterval
    , fcsCaptureIntervalUnits

    -- ** InputDeviceConfiguredInput
    , InputDeviceConfiguredInput (..)

    -- ** HlsAkamaiSettings
    , HlsAkamaiSettings (..)
    , mkHlsAkamaiSettings
    , hasConnectionRetryInterval
    , hasFilecacheDuration
    , hasHttpTransferMode
    , hasNumRetries
    , hasRestartDelay
    , hasSalt
    , hasToken

    -- ** Mpeg2ColorSpace
    , Mpeg2ColorSpace (..)

    -- ** GlobalConfigurationInputEndAction
    , GlobalConfigurationInputEndAction (..)

    -- ** SmoothGroupSparseTrackType
    , SmoothGroupSparseTrackType (..)

    -- ** MultiplexProgram
    , MultiplexProgram (..)
    , mkMultiplexProgram
    , mpChannelId
    , mpMultiplexProgramSettings
    , mpPacketIdentifiersMap
    , mpPipelineDetails
    , mpProgramName

    -- ** BatchScheduleActionDeleteResult
    , BatchScheduleActionDeleteResult (..)
    , mkBatchScheduleActionDeleteResult
    , bsadrScheduleActions

    -- ** Fmp4TimedMetadataBehavior
    , Fmp4TimedMetadataBehavior (..)

    -- ** EbuTtDDestinationSettings
    , EbuTtDDestinationSettings (..)
    , mkEbuTtDDestinationSettings
    , etddsFillLineGap
    , etddsFontFamily
    , etddsStyleControl

    -- ** BurnInShadowColor
    , BurnInShadowColor (..)

    -- ** InputDeviceConnectionState
    , InputDeviceConnectionState (..)

    -- ** PassThroughSettings
    , PassThroughSettings (..)
    , mkPassThroughSettings

    -- ** M2tsBufferModel
    , M2tsBufferModel (..)

    -- ** FailoverCondition
    , FailoverCondition (..)
    , mkFailoverCondition
    , fcFailoverConditionSettings

    -- ** InputDeviceRequest
    , InputDeviceRequest (..)
    , mkInputDeviceRequest
    , idrId

    -- ** InputDestinationVpc
    , InputDestinationVpc (..)
    , mkInputDestinationVpc
    , idvAvailabilityZone
    , idvNetworkInterfaceId

    -- ** TimecodeConfigSource
    , TimecodeConfigSource (..)

    -- ** ReservationState
    , ReservationState (..)

    -- ** ReservationResourceSpecification
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

    -- ** ReservationMaximumFramerate
    , ReservationMaximumFramerate (..)

    -- ** TemporalFilterPostFilterSharpening
    , TemporalFilterPostFilterSharpening (..)

    -- ** PipelineId
    , PipelineId (..)

    -- ** MultiplexProgramPipelineDetail
    , MultiplexProgramPipelineDetail (..)
    , mkMultiplexProgramPipelineDetail
    , mppdActiveChannelPipeline
    , mppdPipelineId

    -- ** HlsRedundantManifest
    , HlsRedundantManifest (..)

    -- ** H265Level
    , H265Level (..)

    -- ** NielsenPcmToId3TaggingState
    , NielsenPcmToId3TaggingState (..)

    -- ** AudioOnlyHlsTrackType
    , AudioOnlyHlsTrackType (..)

    -- ** H264SpatialAq
    , H264SpatialAq (..)

    -- ** BatchScheduleActionCreateRequest
    , BatchScheduleActionCreateRequest (..)
    , mkBatchScheduleActionCreateRequest
    , bsacrScheduleActions

    -- ** VideoCodecSettings
    , VideoCodecSettings (..)
    , mkVideoCodecSettings
    , vcsFrameCaptureSettings
    , vcsH264Settings
    , vcsH265Settings
    , vcsMpeg2Settings

    -- ** PipelinePauseStateSettings
    , PipelinePauseStateSettings (..)
    , mkPipelinePauseStateSettings
    , ppssPipelineId

    -- ** InputDeviceIpScheme
    , InputDeviceIpScheme (..)

    -- ** FailoverConditionSettings
    , FailoverConditionSettings (..)
    , mkFailoverConditionSettings
    , fcsInputLossSettings

    -- ** InputVpcRequest
    , InputVpcRequest (..)
    , mkInputVpcRequest
    , ivrSubnetIds
    , ivrSecurityGroupIds

    -- ** HlsClientCache
    , HlsClientCache (..)

    -- ** RtmpCaptionInfoDestinationSettings
    , RtmpCaptionInfoDestinationSettings (..)
    , mkRtmpCaptionInfoDestinationSettings

    -- ** MultiplexOutputSettings
    , MultiplexOutputSettings (..)
    , mkMultiplexOutputSettings
    , mosDestination

    -- ** EmbeddedScte20Detection
    , EmbeddedScte20Detection (..)

    -- ** H265LookAheadRateControl
    , H265LookAheadRateControl (..)

    -- ** H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- ** ReservationResourceType
    , ReservationResourceType (..)

    -- ** FecOutputSettings
    , FecOutputSettings (..)
    , mkFecOutputSettings
    , fosColumnDepth
    , fosIncludeFec
    , fosRowLength

    -- ** MsSmoothH265PackagingType
    , MsSmoothH265PackagingType (..)

    -- ** H265Profile
    , H265Profile (..)

    -- ** HlsSettings
    , HlsSettings (..)
    , mkHlsSettings
    , hsAudioOnlyHlsSettings
    , hsFmp4HlsSettings
    , hsStandardHlsSettings

    -- ** LogLevel
    , LogLevel (..)

    -- ** InputSwitchScheduleActionSettings
    , InputSwitchScheduleActionSettings (..)
    , mkInputSwitchScheduleActionSettings
    , issasInputAttachmentNameReference
    , issasInputClippingSettings
    , issasUrlPath

    -- ** InputSourceRequest
    , InputSourceRequest (..)
    , mkInputSourceRequest
    , isrPasswordParam
    , isrUrl
    , isrUsername

    -- ** Scte35SegmentationDescriptor
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

    -- ** BatchScheduleActionCreateResult
    , BatchScheduleActionCreateResult (..)
    , mkBatchScheduleActionCreateResult
    , bScheduleActions

    -- ** Hdr10Settings
    , Hdr10Settings (..)
    , mkHdr10Settings
    , hsMaxCll
    , hsMaxFall

    -- ** InputWhitelistRule
    , InputWhitelistRule (..)
    , mkInputWhitelistRule
    , iwrCidr

    -- ** H264FilterSettings
    , H264FilterSettings (..)
    , mkH264FilterSettings
    , hfsTemporalFilterSettings

    -- ** H265ColorSpaceSettings
    , H265ColorSpaceSettings (..)
    , mkH265ColorSpaceSettings
    , hColorSpacePassthroughSettings
    , hHdr10Settings
    , hRec601Settings
    , hRec709Settings

    -- ** AfdSignaling
    , AfdSignaling (..)

    -- ** AacRateControlMode
    , AacRateControlMode (..)

    -- ** HlsTsFileMode
    , HlsTsFileMode (..)

    -- ** InputDeviceNetworkSettings
    , InputDeviceNetworkSettings (..)
    , mkInputDeviceNetworkSettings
    , idnsDnsAddresses
    , idnsGateway
    , idnsIpAddress
    , idnsIpScheme
    , idnsSubnetMask

    -- ** SmoothGroupTimestampOffsetMode
    , SmoothGroupTimestampOffsetMode (..)

    -- ** ChannelEgressEndpoint
    , ChannelEgressEndpoint (..)
    , mkChannelEgressEndpoint
    , ceeSourceIp

    -- ** InputLossActionForRtmpOut
    , InputLossActionForRtmpOut (..)

    -- ** OutputGroupSettings
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

    -- ** TeletextSourceSettings
    , TeletextSourceSettings (..)
    , mkTeletextSourceSettings
    , tssPageNumber

    -- ** M2tsSettings
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

    -- ** DvbSubDestinationSettings
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

    -- ** Smpte2038DataPreference
    , Smpte2038DataPreference (..)

    -- ** VideoSelectorProgramId
    , VideoSelectorProgramId (..)
    , mkVideoSelectorProgramId
    , vspiProgramId

    -- ** SmoothGroupEventStopBehavior
    , SmoothGroupEventStopBehavior (..)

    -- ** NetworkInputSettings
    , NetworkInputSettings (..)
    , mkNetworkInputSettings
    , nisHlsInputSettings
    , nisServerValidation

    -- ** Fmp4HlsSettings
    , Fmp4HlsSettings (..)
    , mkFmp4HlsSettings
    , fhsAudioRenditionSets
    , fhsNielsenId3Behavior
    , fhsTimedMetadataBehavior

    -- ** Scte27DestinationSettings
    , Scte27DestinationSettings (..)
    , mkScte27DestinationSettings

    -- ** MediaPackageOutputDestinationSettings
    , MediaPackageOutputDestinationSettings (..)
    , mkMediaPackageOutputDestinationSettings
    , mpodsChannelId

    -- ** H264FramerateControl
    , H264FramerateControl (..)

    -- ** Mpeg2FilterSettings
    , Mpeg2FilterSettings (..)
    , mkMpeg2FilterSettings
    , mfsTemporalFilterSettings

    -- ** H264ForceFieldPictures
    , H264ForceFieldPictures (..)

    -- ** InputCodec
    , InputCodec (..)

    -- ** SmoothGroupAudioOnlyTimecodeControl
    , SmoothGroupAudioOnlyTimecodeControl (..)

    -- ** InputResolution
    , InputResolution (..)

    -- ** M2tsTimedMetadataBehavior
    , M2tsTimedMetadataBehavior (..)

    -- ** H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- ** Scte35SpliceInsert
    , Scte35SpliceInsert (..)
    , mkScte35SpliceInsert
    , ssiAdAvailOffset
    , ssiNoRegionalBlackoutFlag
    , ssiWebDeliveryAllowedFlag

    -- ** HlsIvInManifest
    , HlsIvInManifest (..)

    -- ** HlsOutputSelection
    , HlsOutputSelection (..)

    -- ** MultiplexProgramPacketIdentifiersMap
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

    -- ** InputSecurityGroupState
    , InputSecurityGroupState (..)

    -- ** AribSourceSettings
    , AribSourceSettings (..)
    , mkAribSourceSettings

    -- ** ArchiveContainerSettings
    , ArchiveContainerSettings (..)
    , mkArchiveContainerSettings
    , acsM2tsSettings
    , acsRawSettings

    -- ** AudioOnlyHlsSettings
    , AudioOnlyHlsSettings (..)
    , mkAudioOnlyHlsSettings
    , aohsAudioGroupId
    , aohsAudioOnlyImage
    , aohsAudioTrackType
    , aohsSegmentType

    -- ** MediaConnectFlow
    , MediaConnectFlow (..)
    , mkMediaConnectFlow
    , mcfFlowArn

    -- ** M3u8Scte35Behavior
    , M3u8Scte35Behavior (..)

    -- ** Reservation
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

    -- ** ArchiveOutputSettings
    , ArchiveOutputSettings (..)
    , mkArchiveOutputSettings
    , aosContainerSettings
    , aosExtension
    , aosNameModifier

    -- ** AudioDescription
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

    -- ** Offering
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

    -- ** TtmlDestinationSettings
    , TtmlDestinationSettings (..)
    , mkTtmlDestinationSettings
    , tdsStyleControl

    -- ** HlsEncryptionType
    , HlsEncryptionType (..)

    -- ** MultiplexProgramSummary
    , MultiplexProgramSummary (..)
    , mkMultiplexProgramSummary
    , mpsChannelId
    , mpsProgramName

    -- ** HlsId3SegmentTaggingState
    , HlsId3SegmentTaggingState (..)

    -- ** AacCodingMode
    , AacCodingMode (..)

    -- ** HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- ** OutputGroup
    , OutputGroup (..)
    , mkOutputGroup
    , ogOutputs
    , ogOutputGroupSettings
    , ogName

    -- ** H264ColorMetadata
    , H264ColorMetadata (..)

    -- ** SmoothGroupSegmentationMode
    , SmoothGroupSegmentationMode (..)

    -- ** StopTimecode
    , StopTimecode (..)
    , mkStopTimecode
    , sLastFrameClippingBehavior
    , sTimecode

    -- ** AudioSelector
    , AudioSelector (..)
    , mkAudioSelector
    , asName
    , asSelectorSettings

    -- ** H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- ** H264LookAheadRateControl
    , H264LookAheadRateControl (..)

    -- ** OutputDestination
    , OutputDestination (..)
    , mkOutputDestination
    , odId
    , odMediaPackageSettings
    , odMultiplexSettings
    , odSettings

    -- ** HlsCdnSettings
    , HlsCdnSettings (..)
    , mkHlsCdnSettings
    , hcsHlsAkamaiSettings
    , hcsHlsBasicPutSettings
    , hcsHlsMediaStoreSettings
    , hcsHlsWebdavSettings

    -- ** InputSpecification
    , InputSpecification (..)
    , mkInputSpecification
    , isCodec
    , isMaximumBitrate
    , isResolution

    -- ** InputDestinationRequest
    , InputDestinationRequest (..)
    , mkInputDestinationRequest
    , idrStreamName

    -- ** OutputLocationRef
    , OutputLocationRef (..)
    , mkOutputLocationRef
    , olrDestinationRefId

    -- ** Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- ** BurnInAlignment
    , BurnInAlignment (..)

    -- ** M3u8TimedMetadataBehavior
    , M3u8TimedMetadataBehavior (..)

    -- ** HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- ** AvailConfiguration
    , AvailConfiguration (..)
    , mkAvailConfiguration
    , acAvailSettings

    -- ** InputMaximumBitrate
    , InputMaximumBitrate (..)

    -- ** M2tsCcDescriptor
    , M2tsCcDescriptor (..)

    -- ** UdpContainerSettings
    , UdpContainerSettings (..)
    , mkUdpContainerSettings
    , ucsM2tsSettings

    -- ** PipelineDetail
    , PipelineDetail (..)
    , mkPipelineDetail
    , pdActiveInputAttachmentName
    , pdActiveInputSwitchActionName
    , pdPipelineId

    -- ** H265Tier
    , H265Tier (..)

    -- ** InputSource
    , InputSource (..)
    , mkInputSource
    , isPasswordParam
    , isUrl
    , isUsername

    -- ** Channel
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

    -- ** Eac3DcFilter
    , Eac3DcFilter (..)

    -- ** DvbSubDestinationBackgroundColor
    , DvbSubDestinationBackgroundColor (..)

    -- ** H264TimecodeInsertionBehavior
    , H264TimecodeInsertionBehavior (..)

    -- ** FeatureActivations
    , FeatureActivations (..)
    , mkFeatureActivations
    , faInputPrepareScheduleActions

    -- ** HlsAdMarkers
    , HlsAdMarkers (..)

    -- ** InputChannelLevel
    , InputChannelLevel (..)
    , mkInputChannelLevel
    , iclInputChannel
    , iclGain

    -- ** PauseStateScheduleActionSettings
    , PauseStateScheduleActionSettings (..)
    , mkPauseStateScheduleActionSettings
    , pssasPipelines

    -- ** KeyProviderSettings
    , KeyProviderSettings (..)
    , mkKeyProviderSettings
    , kpsStaticKeySettings

    -- ** Mpeg2DisplayRatio
    , Mpeg2DisplayRatio (..)

    -- ** ReservationCodec
    , ReservationCodec (..)

    -- ** InputDeblockFilter
    , InputDeblockFilter (..)

    -- ** EbuTtDFillLineGapControl
    , EbuTtDFillLineGapControl (..)

    -- ** ReservationResolution
    , ReservationResolution (..)

    -- ** Scte35SegmentationCancelIndicator
    , Scte35SegmentationCancelIndicator (..)

    -- ** H265ScanType
    , H265ScanType (..)

    -- ** AacSpec
    , AacSpec (..)

    -- ** MultiplexProgramChannelDestinationSettings
    , MultiplexProgramChannelDestinationSettings (..)
    , mkMultiplexProgramChannelDestinationSettings
    , mpcdsMultiplexId
    , mpcdsProgramName

    -- ** Scte35SpliceInsertScheduleActionSettings
    , Scte35SpliceInsertScheduleActionSettings (..)
    , mkScte35SpliceInsertScheduleActionSettings
    , ssisasSpliceEventId
    , ssisasDuration

    -- ** Scte20PlusEmbeddedDestinationSettings
    , Scte20PlusEmbeddedDestinationSettings (..)
    , mkScte20PlusEmbeddedDestinationSettings

    -- ** AudioType
    , AudioType (..)

    -- ** EmbeddedPlusScte20DestinationSettings
    , EmbeddedPlusScte20DestinationSettings (..)
    , mkEmbeddedPlusScte20DestinationSettings

    -- ** NielsenConfiguration
    , NielsenConfiguration (..)
    , mkNielsenConfiguration
    , ncDistributorId
    , ncNielsenPcmToId3Tagging

    -- ** CdiInputResolution
    , CdiInputResolution (..)

    -- ** HlsIncompleteSegmentBehavior
    , HlsIncompleteSegmentBehavior (..)

    -- ** CaptionSelectorSettings
    , CaptionSelectorSettings (..)
    , mkCaptionSelectorSettings
    , cssAncillarySourceSettings
    , cssAribSourceSettings
    , cssDvbSubSourceSettings
    , cssEmbeddedSourceSettings
    , cssScte20SourceSettings
    , cssScte27SourceSettings
    , cssTeletextSourceSettings

    -- ** BatchSuccessfulResultModel
    , BatchSuccessfulResultModel (..)
    , mkBatchSuccessfulResultModel
    , bsrmArn
    , bsrmId
    , bsrmState

    -- ** Input
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

    -- ** DvbSdtSettings
    , DvbSdtSettings (..)
    , mkDvbSdtSettings
    , dssOutputSdt
    , dssRepInterval
    , dssServiceName
    , dssServiceProviderName

    -- ** Scte20Convert608To708
    , Scte20Convert608To708 (..)

    -- ** M3u8Settings
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

    -- ** AvailBlankingState
    , AvailBlankingState (..)

    -- ** Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- ** AudioNormalizationSettings
    , AudioNormalizationSettings (..)
    , mkAudioNormalizationSettings
    , ansAlgorithm
    , ansAlgorithmControl
    , ansTargetLkfs

    -- ** H264Level
    , H264Level (..)

    -- ** SmpteTtDestinationSettings
    , SmpteTtDestinationSettings (..)
    , mkSmpteTtDestinationSettings

    -- ** AudioOnlyHlsSegmentType
    , AudioOnlyHlsSegmentType (..)

    -- ** Mpeg2TimecodeInsertionBehavior
    , Mpeg2TimecodeInsertionBehavior (..)

    -- ** Scte35NoRegionalBlackoutFlag
    , Scte35NoRegionalBlackoutFlag (..)

    -- ** ScheduleActionSettings
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

    -- ** MsSmoothGroupSettings
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

    -- ** VideoDescriptionRespondToAfd
    , VideoDescriptionRespondToAfd (..)

    -- ** MultiplexStatmuxVideoSettings
    , MultiplexStatmuxVideoSettings (..)
    , mkMultiplexStatmuxVideoSettings
    , msvsMaximumBitrate
    , msvsMinimumBitrate
    , msvsPriority

    -- ** TemporalFilterStrength
    , TemporalFilterStrength (..)

    -- ** InputLossBehavior
    , InputLossBehavior (..)
    , mkInputLossBehavior
    , ilbBlackFrameMsec
    , ilbInputLossImageColor
    , ilbInputLossImageSlate
    , ilbInputLossImageType
    , ilbRepeatFrameMsec

    -- ** Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- ** VideoSelectorSettings
    , VideoSelectorSettings (..)
    , mkVideoSelectorSettings
    , vssVideoSelectorPid
    , vssVideoSelectorProgramId

    -- ** AvailBlanking
    , AvailBlanking (..)
    , mkAvailBlanking
    , abAvailBlankingImage
    , abState

    -- ** HlsMediaStoreSettings
    , HlsMediaStoreSettings (..)
    , mkHlsMediaStoreSettings
    , hmssConnectionRetryInterval
    , hmssFilecacheDuration
    , hmssMediaStoreStorageClass
    , hmssNumRetries
    , hmssRestartDelay

    -- ** AudioTrackSelection
    , AudioTrackSelection (..)
    , mkAudioTrackSelection
    , atsTracks

    -- ** MultiplexProgramSettings
    , MultiplexProgramSettings (..)
    , mkMultiplexProgramSettings
    , mpsProgramNumber
    , mpsPreferredChannelPipeline
    , mpsServiceDescriptor
    , mpsVideoSettings

    -- ** InputPrepareScheduleActionSettings
    , InputPrepareScheduleActionSettings (..)
    , mkInputPrepareScheduleActionSettings
    , ipsasInputAttachmentNameReference
    , ipsasInputClippingSettings
    , ipsasUrlPath

    -- ** Scte35TimeSignalApos
    , Scte35TimeSignalApos (..)
    , mkScte35TimeSignalApos
    , stsaAdAvailOffset
    , stsaNoRegionalBlackoutFlag
    , stsaWebDeliveryAllowedFlag

    -- ** TemporalFilterSettings
    , TemporalFilterSettings (..)
    , mkTemporalFilterSettings
    , tfsPostFilterSharpening
    , tfsStrength

    -- ** M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- ** AudioTrack
    , AudioTrack (..)
    , mkAudioTrack
    , atTrack

    -- ** M2tsKlv
    , M2tsKlv (..)

    -- ** AudioChannelMapping
    , AudioChannelMapping (..)
    , mkAudioChannelMapping
    , acmOutputChannel
    , acmInputChannelLevels

    -- ** BlackoutSlateState
    , BlackoutSlateState (..)

    -- ** H264Syntax
    , H264Syntax (..)

    -- ** MultiplexSettingsSummary
    , MultiplexSettingsSummary (..)
    , mkMultiplexSettingsSummary
    , mTransportStreamBitrate

    -- ** RtmpOutputSettings
    , RtmpOutputSettings (..)
    , mkRtmpOutputSettings
    , rosDestination
    , rosCertificateMode
    , rosConnectionRetryInterval
    , rosNumRetries

    -- ** HlsAkamaiHttpTransferMode
    , HlsAkamaiHttpTransferMode (..)

    -- ** H264GopBReference
    , H264GopBReference (..)

    -- ** RtmpGroupSettings
    , RtmpGroupSettings (..)
    , mkRtmpGroupSettings
    , rgsAdMarkers
    , rgsAuthenticationScheme
    , rgsCacheFullBehavior
    , rgsCacheLength
    , rgsCaptionData
    , rgsInputLossAction
    , rgsRestartDelay

    -- ** BurnInOutlineColor
    , BurnInOutlineColor (..)

    -- ** BatchFailedResultModel
    , BatchFailedResultModel (..)
    , mkBatchFailedResultModel
    , bfrmArn
    , bfrmCode
    , bfrmId
    , bfrmMessage

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    , HlsId3SegmentTaggingScheduleActionSettings (..)
    , mkHlsId3SegmentTaggingScheduleActionSettings
    , histsasTag

    -- ** GlobalConfigurationOutputTimingSource
    , GlobalConfigurationOutputTimingSource (..)

    -- ** RawSettings
    , RawSettings (..)
    , mkRawSettings

    -- ** HlsH265PackagingType
    , HlsH265PackagingType (..)

    -- ** InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- ** Scte35Descriptor
    , Scte35Descriptor (..)
    , mkScte35Descriptor
    , sdScte35DescriptorSettings

    -- ** AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- ** Ac3Settings
    , Ac3Settings (..)
    , mkAc3Settings
    , aBitrate
    , aBitstreamMode
    , aCodingMode
    , aDialnorm
    , aDrcProfile
    , aLfeFilter
    , aMetadataControl

    -- ** Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- ** AudioLanguageSelectionPolicy
    , AudioLanguageSelectionPolicy (..)

    -- ** MultiplexMediaConnectOutputDestinationSettings
    , MultiplexMediaConnectOutputDestinationSettings (..)
    , mkMultiplexMediaConnectOutputDestinationSettings
    , mmcodsEntitlementArn

    -- ** ImmediateModeScheduleActionStartSettings
    , ImmediateModeScheduleActionStartSettings (..)
    , mkImmediateModeScheduleActionStartSettings

    -- ** Mp2CodingMode
    , Mp2CodingMode (..)

    -- ** AacRawFormat
    , AacRawFormat (..)

    -- ** MediaPackageOutputSettings
    , MediaPackageOutputSettings (..)
    , mkMediaPackageOutputSettings

    -- ** H265Settings
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

    -- ** Scte27SourceSettings
    , Scte27SourceSettings (..)
    , mkScte27SourceSettings
    , sssPid

    -- ** FollowModeScheduleActionStartSettings
    , FollowModeScheduleActionStartSettings (..)
    , mkFollowModeScheduleActionStartSettings
    , fmsassReferenceActionName
    , fmsassFollowPoint

    -- ** DvbSubSourceSettings
    , DvbSubSourceSettings (..)
    , mkDvbSubSourceSettings
    , dsssPid

    -- ** InputDeviceActiveInput
    , InputDeviceActiveInput (..)

    -- ** MultiplexSummary
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

    -- ** MultiplexVideoSettings
    , MultiplexVideoSettings (..)
    , mkMultiplexVideoSettings
    , mvsConstantBitrate
    , mvsStatmuxSettings

    -- ** H264TemporalAq
    , H264TemporalAq (..)

    -- ** HlsWebdavHttpTransferMode
    , HlsWebdavHttpTransferMode (..)

    -- ** ChannelState
    , ChannelState (..)

    -- ** InputDeviceConfigurableSettings
    , InputDeviceConfigurableSettings (..)
    , mkInputDeviceConfigurableSettings
    , idcsConfiguredInput
    , idcsMaxBitrate

    -- ** Scte35DeliveryRestrictions
    , Scte35DeliveryRestrictions (..)
    , mkScte35DeliveryRestrictions
    , sdrDeviceRestrictions
    , sdrArchiveAllowedFlag
    , sdrWebDeliveryAllowedFlag
    , sdrNoRegionalBlackoutFlag

    -- ** Eac3LfeControl
    , Eac3LfeControl (..)

    -- ** Output
    , Output (..)
    , mkOutput
    , oOutputSettings
    , oAudioDescriptionNames
    , oCaptionDescriptionNames
    , oOutputName
    , oVideoDescriptionName

    -- ** InputAttachment
    , InputAttachment (..)
    , mkInputAttachment
    , iaAutomaticInputFailoverSettings
    , iaInputAttachmentName
    , iaInputId
    , iaInputSettings

    -- ** M2tsEbifControl
    , M2tsEbifControl (..)

    -- ** DvbSubDestinationAlignment
    , DvbSubDestinationAlignment (..)

    -- ** M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- ** H264ScanType
    , H264ScanType (..)

    -- ** InputSecurityGroup
    , InputSecurityGroup (..)
    , mkInputSecurityGroup
    , isgArn
    , isgId
    , isgInputs
    , isgState
    , isgTags
    , isgWhitelistRules

    -- ** InputLossActionForUdpOut
    , InputLossActionForUdpOut (..)

    -- ** Ac3DrcProfile
    , Ac3DrcProfile (..)

    -- ** Scte35WebDeliveryAllowedFlag
    , Scte35WebDeliveryAllowedFlag (..)

    -- ** M3u8NielsenId3Behavior
    , M3u8NielsenId3Behavior (..)

    -- ** BurnInBackgroundColor
    , BurnInBackgroundColor (..)

    -- ** Eac3DrcLine
    , Eac3DrcLine (..)

    -- ** H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- ** AncillarySourceSettings
    , AncillarySourceSettings (..)
    , mkAncillarySourceSettings
    , assSourceAncillaryChannelNumber

    -- ** BatchScheduleActionDeleteRequest
    , BatchScheduleActionDeleteRequest (..)
    , mkBatchScheduleActionDeleteRequest
    , bsadrActionNames

    -- ** M2tsAribCaptionsPidControl
    , M2tsAribCaptionsPidControl (..)

    -- ** H265RateControlMode
    , H265RateControlMode (..)

    -- ** DvbSdtOutputSdt
    , DvbSdtOutputSdt (..)

    -- ** Rec709Settings
    , Rec709Settings (..)
    , mkRec709Settings

    -- ** Eac3CodingMode
    , Eac3CodingMode (..)

    -- ** VideoDescription
    , VideoDescription (..)
    , mkVideoDescription
    , vdName
    , vdCodecSettings
    , vdHeight
    , vdRespondToAfd
    , vdScalingBehavior
    , vdSharpness
    , vdWidth

    -- ** WebvttDestinationSettings
    , WebvttDestinationSettings (..)
    , mkWebvttDestinationSettings

    -- ** ChannelSummary
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

    -- ** SmoothGroupEventIdMode
    , SmoothGroupEventIdMode (..)

    -- ** RtmpAdMarkers
    , RtmpAdMarkers (..)

    -- ** CaptionDestinationSettings
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

    -- ** InputDeviceHdSettings
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

    -- ** EmbeddedDestinationSettings
    , EmbeddedDestinationSettings (..)
    , mkEmbeddedDestinationSettings

    -- ** H265AlternativeTransferFunction
    , H265AlternativeTransferFunction (..)

    -- ** MultiplexState
    , MultiplexState (..)

    -- ** AcceptHeader
    , AcceptHeader (..)

    -- ** SmoothGroupStreamManifestBehavior
    , SmoothGroupStreamManifestBehavior (..)

    -- ** MultiplexProgramServiceDescriptor
    , MultiplexProgramServiceDescriptor (..)
    , mkMultiplexProgramServiceDescriptor
    , mpsdProviderName
    , mpsdServiceName

    -- ** M2tsAudioStreamType
    , M2tsAudioStreamType (..)

    -- ** StaticKeySettings
    , StaticKeySettings (..)
    , mkStaticKeySettings
    , sksStaticKeyValue
    , sksKeyProviderServer

    -- ** Scte35TimeSignalScheduleActionSettings
    , Scte35TimeSignalScheduleActionSettings (..)
    , mkScte35TimeSignalScheduleActionSettings
    , stssasScte35Descriptors

    -- ** BurnInFontColor
    , BurnInFontColor (..)

    -- ** H265TimecodeInsertionBehavior
    , H265TimecodeInsertionBehavior (..)

    -- ** InputLossActionForMsSmoothOut
    , InputLossActionForMsSmoothOut (..)

    -- ** HlsSegmentationMode
    , HlsSegmentationMode (..)

    -- ** Ac3CodingMode
    , Ac3CodingMode (..)

    -- ** MultiplexGroupSettings
    , MultiplexGroupSettings (..)
    , mkMultiplexGroupSettings

    -- ** InputDeviceSummary
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

    -- ** InputDestination
    , InputDestination (..)
    , mkInputDestination
    , idIp
    , idPort
    , idUrl
    , idVpc

    -- ** HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- ** TimecodeConfig
    , TimecodeConfig (..)
    , mkTimecodeConfig
    , tcSource
    , tcSyncThreshold

    -- ** RtmpOutputCertificateMode
    , RtmpOutputCertificateMode (..)

    -- ** Mp2Settings
    , Mp2Settings (..)
    , mkMp2Settings
    , msBitrate
    , msCodingMode
    , msSampleRate

    -- ** DvbSubDestinationTeletextGridControl
    , DvbSubDestinationTeletextGridControl (..)

    -- ** Mpeg2ScanType
    , Mpeg2ScanType (..)

    -- ** OutputSettings
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

    -- ** EbuTtDDestinationStyleControl
    , EbuTtDDestinationStyleControl (..)

    -- ** AutomaticInputFailoverSettings
    , AutomaticInputFailoverSettings (..)
    , mkAutomaticInputFailoverSettings
    , aifsSecondaryInputId
    , aifsErrorClearTimeMsec
    , aifsFailoverConditions
    , aifsInputPreference

    -- ** Scte35AposWebDeliveryAllowedBehavior
    , Scte35AposWebDeliveryAllowedBehavior (..)

    -- ** InputLossImageType
    , InputLossImageType (..)

    -- ** H264Settings
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

    -- ** M3u8PcrControl
    , M3u8PcrControl (..)

    -- ** FixedAfd
    , FixedAfd (..)

    -- ** CdiInputSpecification
    , CdiInputSpecification (..)
    , mkCdiInputSpecification
    , cisResolution

    -- ** VideoSelectorColorSpace
    , VideoSelectorColorSpace (..)

    -- ** InputDeviceTransferType
    , InputDeviceTransferType (..)

    -- ** ReservationMaximumBitrate
    , ReservationMaximumBitrate (..)

    -- ** M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- ** ChannelClass
    , ChannelClass (..)

    -- ** HlsGroupSettings
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

    -- ** DvbSubDestinationOutlineColor
    , DvbSubDestinationOutlineColor (..)

    -- ** InputPreference
    , InputPreference (..)

    -- ** BurnInDestinationSettings
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

    -- ** WavSettings
    , WavSettings (..)
    , mkWavSettings
    , wsBitDepth
    , wsCodingMode
    , wsSampleRate

    -- ** ReservationSpecialFeature
    , ReservationSpecialFeature (..)

    -- ** MultiplexSettings
    , MultiplexSettings (..)
    , mkMultiplexSettings
    , mssTransportStreamBitrate
    , mssTransportStreamId
    , mssMaximumVideoBufferDelayMilliseconds
    , mssTransportStreamReservedBitrate

    -- ** TtmlDestinationStyleControl
    , TtmlDestinationStyleControl (..)

    -- ** Mpeg2GopSizeUnits
    , Mpeg2GopSizeUnits (..)

    -- ** GlobalConfigurationLowFramerateInputs
    , GlobalConfigurationLowFramerateInputs (..)

    -- ** Eac3DrcRf
    , Eac3DrcRf (..)

    -- ** InputClippingSettings
    , InputClippingSettings (..)
    , mkInputClippingSettings
    , icsInputTimecodeSource
    , icsStartTimecode
    , icsStopTimecode

    -- ** DeviceUpdateStatus
    , DeviceUpdateStatus (..)

    -- ** LastFrameClippingBehavior
    , LastFrameClippingBehavior (..)

    -- ** HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- ** Mpeg2SubGopLength
    , Mpeg2SubGopLength (..)

    -- ** HlsTimedMetadataScheduleActionSettings
    , HlsTimedMetadataScheduleActionSettings (..)
    , mkHlsTimedMetadataScheduleActionSettings
    , htmsasId3

    -- ** DeviceSettingsSyncState
    , DeviceSettingsSyncState (..)

    -- ** RtmpCaptionData
    , RtmpCaptionData (..)

    -- ** Mpeg2ColorMetadata
    , Mpeg2ColorMetadata (..)

    -- ** AuthenticationScheme
    , AuthenticationScheme (..)

    -- ** HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- ** FollowPoint
    , FollowPoint (..)

    -- ** HlsOutputSettings
    , HlsOutputSettings (..)
    , mkHlsOutputSettings
    , hosHlsSettings
    , hosH265PackagingType
    , hosNameModifier
    , hosSegmentModifier

    -- ** HlsIvSource
    , HlsIvSource (..)

    -- ** H265FlickerAq
    , H265FlickerAq (..)

    -- ** AudioCodecSettings
    , AudioCodecSettings (..)
    , mkAudioCodecSettings
    , acsAacSettings
    , acsAc3Settings
    , acsEac3Settings
    , acsMp2Settings
    , acsPassThroughSettings
    , acsWavSettings

    -- ** AacInputType
    , AacInputType (..)

    -- ** AacVbrQuality
    , AacVbrQuality (..)

    -- ** M2tsArib
    , M2tsArib (..)

    -- ** InputSourceEndBehavior
    , InputSourceEndBehavior (..)

    -- ** HlsDiscontinuityTags
    , HlsDiscontinuityTags (..)

    -- ** Mpeg2Settings
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

    -- ** Scte35SpliceInsertWebDeliveryAllowedBehavior
    , Scte35SpliceInsertWebDeliveryAllowedBehavior (..)

    -- ** InputClass
    , InputClass (..)

    -- ** Scte35DeviceRestrictions
    , Scte35DeviceRestrictions (..)

    -- ** RtmpCacheFullBehavior
    , RtmpCacheFullBehavior (..)

    -- ** Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- ** OfferingType
    , OfferingType (..)

    -- ** H264SubGopLength
    , H264SubGopLength (..)

    -- ** Multiplex
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

    -- ** CaptionDescription
    , CaptionDescription (..)
    , mkCaptionDescription
    , cdCaptionSelectorName
    , cdName
    , cdDestinationSettings
    , cdLanguageCode
    , cdLanguageDescription

    -- ** H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- ** M2tsScte35Control
    , M2tsScte35Control (..)

    -- ** VideoSelectorColorSpaceUsage
    , VideoSelectorColorSpaceUsage (..)

    -- ** StaticImageActivateScheduleActionSettings
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

    -- ** FrameCaptureIntervalUnit
    , FrameCaptureIntervalUnit (..)

    -- ** ArchiveGroupSettings
    , ArchiveGroupSettings (..)
    , mkArchiveGroupSettings
    , agsDestination
    , agsRolloverInterval

    -- ** AacProfile
    , AacProfile (..)

    -- ** StartTimecode
    , StartTimecode (..)
    , mkStartTimecode
    , stTimecode

    -- ** AudioDescriptionLanguageCodeControl
    , AudioDescriptionLanguageCodeControl (..)

    -- ** CaptionLanguageMapping
    , CaptionLanguageMapping (..)
    , mkCaptionLanguageMapping
    , clmLanguageCode
    , clmLanguageDescription
    , clmCaptionChannel

    -- ** CaptionSelector
    , CaptionSelector (..)
    , mkCaptionSelector
    , csName
    , csLanguageCode
    , csSelectorSettings

    -- ** InputDeviceSettings
    , InputDeviceSettings (..)
    , mkInputDeviceSettings
    , idssId

    -- ** HlsMediaStoreStorageClass
    , HlsMediaStoreStorageClass (..)

    -- ** TransferringInputDeviceSummary
    , TransferringInputDeviceSummary (..)
    , mkTransferringInputDeviceSummary
    , tidsId
    , tidsMessage
    , tidsTargetCustomerId
    , tidsTransferType

    -- ** AudioPidSelection
    , AudioPidSelection (..)
    , mkAudioPidSelection
    , apsPid

    -- ** DvbSubDestinationShadowColor
    , DvbSubDestinationShadowColor (..)

    -- ** InputLocation
    , InputLocation (..)
    , mkInputLocation
    , ilUri
    , ilPasswordParam
    , ilUsername

    -- ** UdpGroupSettings
    , UdpGroupSettings (..)
    , mkUdpGroupSettings
    , ugsInputLossAction
    , ugsTimedMetadataId3Frame
    , ugsTimedMetadataId3Period

    -- ** H264QualityLevel
    , H264QualityLevel (..)

    -- ** Fmp4NielsenId3Behavior
    , Fmp4NielsenId3Behavior (..)

    -- ** FrameCaptureOutputSettings
    , FrameCaptureOutputSettings (..)
    , mkFrameCaptureOutputSettings
    , fcosNameModifier

    -- ** AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- ** GlobalConfiguration
    , GlobalConfiguration (..)
    , mkGlobalConfiguration
    , gcInitialAudioGain
    , gcInputEndAction
    , gcInputLossBehavior
    , gcOutputLockingMode
    , gcOutputTimingSource
    , gcSupportLowFramerateInputs

    -- ** UdpOutputSettings
    , UdpOutputSettings (..)
    , mkUdpOutputSettings
    , uosDestination
    , uosContainerSettings
    , uosBufferMsec
    , uosFecOutputSettings

    -- ** H264Profile
    , H264Profile (..)

    -- ** WavCodingMode
    , WavCodingMode (..)

    -- ** MultiplexOutputDestination
    , MultiplexOutputDestination (..)
    , mkMultiplexOutputDestination
    , modMediaConnectSettings

    -- ** ScheduleAction
    , ScheduleAction (..)
    , mkScheduleAction
    , saActionName
    , saScheduleActionStartSettings
    , saScheduleActionSettings

    -- ** FixedModeScheduleActionStartSettings
    , FixedModeScheduleActionStartSettings (..)
    , mkFixedModeScheduleActionStartSettings
    , fmsassTime

    -- ** OfferingDurationUnits
    , OfferingDurationUnits (..)

    -- ** Scte35AposNoRegionalBlackoutBehavior
    , Scte35AposNoRegionalBlackoutBehavior (..)

    -- ** EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- ** Scte20SourceSettings
    , Scte20SourceSettings (..)
    , mkScte20SourceSettings
    , sssConvert608To708
    , sssSource608ChannelNumber

    -- ** InputSettings
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

    -- ** InputTimecodeSource
    , InputTimecodeSource (..)

    -- ** SmoothGroupCertificateMode
    , SmoothGroupCertificateMode (..)

    -- ** InputSourceType
    , InputSourceType (..)

    -- ** Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- ** FrameCaptureGroupSettings
    , FrameCaptureGroupSettings (..)
    , mkFrameCaptureGroupSettings
    , fcgsDestination

    -- ** M2tsAbsentInputAudioBehavior
    , M2tsAbsentInputAudioBehavior (..)

    -- ** FecOutputIncludeFec
    , FecOutputIncludeFec (..)

    -- ** NetworkInputServerValidation
    , NetworkInputServerValidation (..)

    -- ** M2tsPcrControl
    , M2tsPcrControl (..)

    -- ** AudioSelectorSettings
    , AudioSelectorSettings (..)
    , mkAudioSelectorSettings
    , assAudioLanguageSelection
    , assAudioPidSelection
    , assAudioTrackSelection

    -- ** InputFilter
    , InputFilter (..)

    -- ** RemixSettings
    , RemixSettings (..)
    , mkRemixSettings
    , rsChannelMappings
    , rsChannelsIn
    , rsChannelsOut

    -- ** HlsManifestCompression
    , HlsManifestCompression (..)

    -- ** InputDeviceType
    , InputDeviceType (..)

    -- ** Rec601Settings
    , Rec601Settings (..)
    , mkRec601Settings

    -- ** AvailSettings
    , AvailSettings (..)
    , mkAvailSettings
    , asScte35SpliceInsert
    , asScte35TimeSignalApos

    -- ** DvbNitSettings
    , DvbNitSettings (..)
    , mkDvbNitSettings
    , dnsNetworkName
    , dnsNetworkId
    , dnsRepInterval

    -- ** BurnInTeletextGridControl
    , BurnInTeletextGridControl (..)

    -- ** MsSmoothOutputSettings
    , MsSmoothOutputSettings (..)
    , mkMsSmoothOutputSettings
    , msosH265PackagingType
    , msosNameModifier

    -- ** UdpTimedMetadataId3Frame
    , UdpTimedMetadataId3Frame (..)

    -- ** OutputDestinationSettings
    , OutputDestinationSettings (..)
    , mkOutputDestinationSettings
    , odsPasswordParam
    , odsStreamName
    , odsUrl
    , odsUsername

    -- ** H264RateControlMode
    , H264RateControlMode (..)

    -- ** BlackoutSlateNetworkEndBlackout
    , BlackoutSlateNetworkEndBlackout (..)

    -- ** InputType
    , InputType (..)

    -- ** M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- ** ColorSpacePassthroughSettings
    , ColorSpacePassthroughSettings (..)
    , mkColorSpacePassthroughSettings

    -- ** IFrameOnlyPlaylistType
    , IFrameOnlyPlaylistType (..)

    -- ** DvbSubDestinationFontColor
    , DvbSubDestinationFontColor (..)

    -- ** VideoDescriptionScalingBehavior
    , VideoDescriptionScalingBehavior (..)

    -- ** Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- ** ScheduleActionStartSettings
    , ScheduleActionStartSettings (..)
    , mkScheduleActionStartSettings
    , sassFixedModeScheduleActionStartSettings
    , sassFollowModeScheduleActionStartSettings
    , sassImmediateModeScheduleActionStartSettings

    -- ** AacSettings
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

    -- ** Scte35ReturnToNetworkScheduleActionSettings
    , Scte35ReturnToNetworkScheduleActionSettings (..)
    , mkScte35ReturnToNetworkScheduleActionSettings
    , srtnsasSpliceEventId

    -- ** EmbeddedSourceSettings
    , EmbeddedSourceSettings (..)
    , mkEmbeddedSourceSettings
    , essConvert608To708
    , essScte20Detection
    , essSource608ChannelNumber
    , essSource608TrackNumber

    -- ** H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- ** Mpeg2AdaptiveQuantization
    , Mpeg2AdaptiveQuantization (..)

    -- ** BlackoutSlate
    , BlackoutSlate (..)
    , mkBlackoutSlate
    , bsBlackoutSlateImage
    , bsNetworkEndBlackout
    , bsNetworkEndBlackoutImage
    , bsNetworkId
    , bsState

    -- ** M2tsAudioInterval
    , M2tsAudioInterval (..)

    -- ** Eac3Settings
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

    -- ** Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- ** InputState
    , InputState (..)

    -- ** MediaConnectFlowRequest
    , MediaConnectFlowRequest (..)
    , mkMediaConnectFlowRequest
    , mcfrFlowArn

    -- ** HlsBasicPutSettings
    , HlsBasicPutSettings (..)
    , mkHlsBasicPutSettings
    , hbpsConnectionRetryInterval
    , hbpsFilecacheDuration
    , hbpsNumRetries
    , hbpsRestartDelay

    -- ** EncoderSettings
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

    -- ** InputDeviceScanType
    , InputDeviceScanType (..)

    -- ** Scte35ArchiveAllowedFlag
    , Scte35ArchiveAllowedFlag (..)

    -- ** AudioDescriptionAudioTypeControl
    , AudioDescriptionAudioTypeControl (..)

    -- ** H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- ** ContentType
    , ContentType (..)

    -- ** H265ColorMetadata
    , H265ColorMetadata (..)

    -- ** M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- ** Scte35SpliceInsertNoRegionalBlackoutBehavior
    , Scte35SpliceInsertNoRegionalBlackoutBehavior (..)

    -- ** InputDeviceState
    , InputDeviceState (..)

    -- ** StandardHlsSettings
    , StandardHlsSettings (..)
    , mkStandardHlsSettings
    , shsM3u8Settings
    , shsAudioRenditionSets

    -- ** VideoSelectorPid
    , VideoSelectorPid (..)
    , mkVideoSelectorPid
    , vspPid

    -- ** Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- ** M2tsRateMode
    , M2tsRateMode (..)

    -- ** HlsWebdavSettings
    , HlsWebdavSettings (..)
    , mkHlsWebdavSettings
    , hwsConnectionRetryInterval
    , hwsFilecacheDuration
    , hwsHttpTransferMode
    , hwsNumRetries
    , hwsRestartDelay

    -- ** StaticImageDeactivateScheduleActionSettings
    , StaticImageDeactivateScheduleActionSettings (..)
    , mkStaticImageDeactivateScheduleActionSettings
    , sidsasFadeOut
    , sidsasLayer

    -- ** DvbTdtSettings
    , DvbTdtSettings (..)
    , mkDvbTdtSettings
    , dtsRepInterval

    -- ** AribDestinationSettings
    , AribDestinationSettings (..)
    , mkAribDestinationSettings

    -- ** InputLossActionForHlsOut
    , InputLossActionForHlsOut (..)

    -- ** Scte35DescriptorSettings
    , Scte35DescriptorSettings (..)
    , mkScte35DescriptorSettings
    , sdsSegmentationDescriptorScte35DescriptorSettings

    -- ** Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- ** InputWhitelistRuleCidr
    , InputWhitelistRuleCidr (..)
    , mkInputWhitelistRuleCidr
    , iwrcCidr

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Waiters
import Network.AWS.MediaLive.UpdateChannelClass
import Network.AWS.MediaLive.ListMultiplexes
import Network.AWS.MediaLive.BatchStart
import Network.AWS.MediaLive.CreateMultiplex
import Network.AWS.MediaLive.ListInputDeviceTransfers
import Network.AWS.MediaLive.ListInputDevices
import Network.AWS.MediaLive.ListInputs
import Network.AWS.MediaLive.DescribeInputDeviceThumbnail
import Network.AWS.MediaLive.ListChannels
import Network.AWS.MediaLive.DescribeInputSecurityGroup
import Network.AWS.MediaLive.CreateInput
import Network.AWS.MediaLive.ListTagsForResource
import Network.AWS.MediaLive.DeleteChannel
import Network.AWS.MediaLive.UpdateChannel
import Network.AWS.MediaLive.AcceptInputDeviceTransfer
import Network.AWS.MediaLive.DescribeReservation
import Network.AWS.MediaLive.CreateTags
import Network.AWS.MediaLive.StopMultiplex
import Network.AWS.MediaLive.DeleteTags
import Network.AWS.MediaLive.CreateInputSecurityGroup
import Network.AWS.MediaLive.StartChannel
import Network.AWS.MediaLive.CancelInputDeviceTransfer
import Network.AWS.MediaLive.ListInputSecurityGroups
import Network.AWS.MediaLive.DeleteReservation
import Network.AWS.MediaLive.UpdateReservation
import Network.AWS.MediaLive.BatchStop
import Network.AWS.MediaLive.DeleteSchedule
import Network.AWS.MediaLive.CreateChannel
import Network.AWS.MediaLive.DeleteInput
import Network.AWS.MediaLive.UpdateInput
import Network.AWS.MediaLive.UpdateInputDevice
import Network.AWS.MediaLive.RejectInputDeviceTransfer
import Network.AWS.MediaLive.DescribeOffering
import Network.AWS.MediaLive.TransferInputDevice
import Network.AWS.MediaLive.DeleteMultiplexProgram
import Network.AWS.MediaLive.UpdateMultiplexProgram
import Network.AWS.MediaLive.BatchDelete
import Network.AWS.MediaLive.ListMultiplexPrograms
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.BatchUpdateSchedule
import Network.AWS.MediaLive.CreateMultiplexProgram
import Network.AWS.MediaLive.DescribeSchedule
import Network.AWS.MediaLive.StartMultiplex
import Network.AWS.MediaLive.StopChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.PurchaseOffering
import Network.AWS.MediaLive.DescribeInputDevice
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.UpdateInputSecurityGroup
import Network.AWS.MediaLive.DeleteInputSecurityGroup
import Network.AWS.MediaLive.ListReservations
import Network.AWS.MediaLive.DeleteMultiplex
import Network.AWS.MediaLive.UpdateMultiplex
import Network.AWS.MediaLive.DescribeMultiplexProgram
import Network.AWS.MediaLive.ListOfferings
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MediaLive'.
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
