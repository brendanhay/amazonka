{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API for AWS Elemental MediaLive
module Network.AWS.MediaLive
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** BadGatewayException
    _BadGatewayException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** GatewayTimeoutException
    _GatewayTimeoutException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** ConflictException
    _ConflictException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- ** MultiplexRunning
    newMultiplexRunning,

    -- ** ChannelRunning
    newChannelRunning,

    -- ** ChannelDeleted
    newChannelDeleted,

    -- ** InputDeleted
    newInputDeleted,

    -- ** InputAttached
    newInputAttached,

    -- ** MultiplexStopped
    newMultiplexStopped,

    -- ** ChannelCreated
    newChannelCreated,

    -- ** MultiplexCreated
    newMultiplexCreated,

    -- ** InputDetached
    newInputDetached,

    -- ** MultiplexDeleted
    newMultiplexDeleted,

    -- ** ChannelStopped
    newChannelStopped,

    -- * Operations
    -- $operations

    -- ** DescribeInputDeviceThumbnail
    DescribeInputDeviceThumbnail (DescribeInputDeviceThumbnail'),
    newDescribeInputDeviceThumbnail,
    DescribeInputDeviceThumbnailResponse (DescribeInputDeviceThumbnailResponse'),
    newDescribeInputDeviceThumbnailResponse,

    -- ** UpdateInputDevice
    UpdateInputDevice' (UpdateInputDevice''),
    newUpdateInputDevice',
    UpdateInputDeviceResponse (UpdateInputDeviceResponse'),
    newUpdateInputDeviceResponse,

    -- ** ListInputs (Paginated)
    ListInputs (ListInputs'),
    newListInputs,
    ListInputsResponse (ListInputsResponse'),
    newListInputsResponse,

    -- ** CreateChannel
    CreateChannel' (CreateChannel''),
    newCreateChannel',
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** DeleteInput
    DeleteInput (DeleteInput'),
    newDeleteInput,
    DeleteInputResponse (DeleteInputResponse'),
    newDeleteInputResponse,

    -- ** ListInputDevices (Paginated)
    ListInputDevices (ListInputDevices'),
    newListInputDevices,
    ListInputDevicesResponse (ListInputDevicesResponse'),
    newListInputDevicesResponse,

    -- ** UpdateInput
    UpdateInput' (UpdateInput''),
    newUpdateInput',
    UpdateInputResponse (UpdateInputResponse'),
    newUpdateInputResponse,

    -- ** ListInputDeviceTransfers (Paginated)
    ListInputDeviceTransfers (ListInputDeviceTransfers'),
    newListInputDeviceTransfers,
    ListInputDeviceTransfersResponse (ListInputDeviceTransfersResponse'),
    newListInputDeviceTransfersResponse,

    -- ** BatchStop
    BatchStop' (BatchStop''),
    newBatchStop',
    BatchStopResponse (BatchStopResponse'),
    newBatchStopResponse,

    -- ** UpdateChannelClass
    UpdateChannelClass' (UpdateChannelClass''),
    newUpdateChannelClass',
    UpdateChannelClassResponse (UpdateChannelClassResponse'),
    newUpdateChannelClassResponse,

    -- ** BatchStart
    BatchStart' (BatchStart''),
    newBatchStart',
    BatchStartResponse (BatchStartResponse'),
    newBatchStartResponse,

    -- ** ListOfferings (Paginated)
    ListOfferings (ListOfferings'),
    newListOfferings,
    ListOfferingsResponse (ListOfferingsResponse'),
    newListOfferingsResponse,

    -- ** UpdateMultiplex
    UpdateMultiplex' (UpdateMultiplex''),
    newUpdateMultiplex',
    UpdateMultiplexResponse (UpdateMultiplexResponse'),
    newUpdateMultiplexResponse,

    -- ** DeleteMultiplex
    DeleteMultiplex (DeleteMultiplex'),
    newDeleteMultiplex,
    DeleteMultiplexResponse (DeleteMultiplexResponse'),
    newDeleteMultiplexResponse,

    -- ** DeleteInputSecurityGroup
    DeleteInputSecurityGroup (DeleteInputSecurityGroup'),
    newDeleteInputSecurityGroup,
    DeleteInputSecurityGroupResponse (DeleteInputSecurityGroupResponse'),
    newDeleteInputSecurityGroupResponse,

    -- ** UpdateInputSecurityGroup
    UpdateInputSecurityGroup (UpdateInputSecurityGroup'),
    newUpdateInputSecurityGroup,
    UpdateInputSecurityGroupResponse (UpdateInputSecurityGroupResponse'),
    newUpdateInputSecurityGroupResponse,

    -- ** ListInputSecurityGroups (Paginated)
    ListInputSecurityGroups (ListInputSecurityGroups'),
    newListInputSecurityGroups,
    ListInputSecurityGroupsResponse (ListInputSecurityGroupsResponse'),
    newListInputSecurityGroupsResponse,

    -- ** DescribeInput
    DescribeInput (DescribeInput'),
    newDescribeInput,
    DescribeInputResponse (DescribeInputResponse'),
    newDescribeInputResponse,

    -- ** CreateInputSecurityGroup
    CreateInputSecurityGroup (CreateInputSecurityGroup'),
    newCreateInputSecurityGroup,
    CreateInputSecurityGroupResponse (CreateInputSecurityGroupResponse'),
    newCreateInputSecurityGroupResponse,

    -- ** StartChannel
    StartChannel (StartChannel'),
    newStartChannel,
    StartChannelResponse (StartChannelResponse'),
    newStartChannelResponse,

    -- ** DescribeInputDevice
    DescribeInputDevice (DescribeInputDevice'),
    newDescribeInputDevice,
    DescribeInputDeviceResponse (DescribeInputDeviceResponse'),
    newDescribeInputDeviceResponse,

    -- ** StopChannel
    StopChannel (StopChannel'),
    newStopChannel,
    StopChannelResponse (StopChannelResponse'),
    newStopChannelResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** BatchUpdateSchedule
    BatchUpdateSchedule (BatchUpdateSchedule'),
    newBatchUpdateSchedule,
    BatchUpdateScheduleResponse (BatchUpdateScheduleResponse'),
    newBatchUpdateScheduleResponse,

    -- ** DescribeOffering
    DescribeOffering (DescribeOffering'),
    newDescribeOffering,
    DescribeOfferingResponse (DescribeOfferingResponse'),
    newDescribeOfferingResponse,

    -- ** AcceptInputDeviceTransfer
    AcceptInputDeviceTransfer (AcceptInputDeviceTransfer'),
    newAcceptInputDeviceTransfer,
    AcceptInputDeviceTransferResponse (AcceptInputDeviceTransferResponse'),
    newAcceptInputDeviceTransferResponse,

    -- ** DeleteMultiplexProgram
    DeleteMultiplexProgram (DeleteMultiplexProgram'),
    newDeleteMultiplexProgram,
    DeleteMultiplexProgramResponse (DeleteMultiplexProgramResponse'),
    newDeleteMultiplexProgramResponse,

    -- ** UpdateMultiplexProgram
    UpdateMultiplexProgram' (UpdateMultiplexProgram''),
    newUpdateMultiplexProgram',
    UpdateMultiplexProgramResponse (UpdateMultiplexProgramResponse'),
    newUpdateMultiplexProgramResponse,

    -- ** DescribeReservation
    DescribeReservation (DescribeReservation'),
    newDescribeReservation,
    DescribeReservationResponse (DescribeReservationResponse'),
    newDescribeReservationResponse,

    -- ** DescribeInputSecurityGroup
    DescribeInputSecurityGroup (DescribeInputSecurityGroup'),
    newDescribeInputSecurityGroup,
    DescribeInputSecurityGroupResponse (DescribeInputSecurityGroupResponse'),
    newDescribeInputSecurityGroupResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** RejectInputDeviceTransfer
    RejectInputDeviceTransfer (RejectInputDeviceTransfer'),
    newRejectInputDeviceTransfer,
    RejectInputDeviceTransferResponse (RejectInputDeviceTransferResponse'),
    newRejectInputDeviceTransferResponse,

    -- ** CreateMultiplex
    CreateMultiplex' (CreateMultiplex''),
    newCreateMultiplex',
    CreateMultiplexResponse (CreateMultiplexResponse'),
    newCreateMultiplexResponse,

    -- ** CreatePartnerInput
    CreatePartnerInput' (CreatePartnerInput''),
    newCreatePartnerInput',
    CreatePartnerInputResponse (CreatePartnerInputResponse'),
    newCreatePartnerInputResponse,

    -- ** DeleteSchedule
    DeleteSchedule (DeleteSchedule'),
    newDeleteSchedule,
    DeleteScheduleResponse (DeleteScheduleResponse'),
    newDeleteScheduleResponse,

    -- ** ListMultiplexes (Paginated)
    ListMultiplexes (ListMultiplexes'),
    newListMultiplexes,
    ListMultiplexesResponse (ListMultiplexesResponse'),
    newListMultiplexesResponse,

    -- ** UpdateReservation
    UpdateReservation' (UpdateReservation''),
    newUpdateReservation',
    UpdateReservationResponse (UpdateReservationResponse'),
    newUpdateReservationResponse,

    -- ** DeleteReservation
    DeleteReservation (DeleteReservation'),
    newDeleteReservation,
    DeleteReservationResponse (DeleteReservationResponse'),
    newDeleteReservationResponse,

    -- ** DescribeMultiplexProgram
    DescribeMultiplexProgram (DescribeMultiplexProgram'),
    newDescribeMultiplexProgram,
    DescribeMultiplexProgramResponse (DescribeMultiplexProgramResponse'),
    newDescribeMultiplexProgramResponse,

    -- ** ListReservations (Paginated)
    ListReservations (ListReservations'),
    newListReservations,
    ListReservationsResponse (ListReservationsResponse'),
    newListReservationsResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** CancelInputDeviceTransfer
    CancelInputDeviceTransfer (CancelInputDeviceTransfer'),
    newCancelInputDeviceTransfer,
    CancelInputDeviceTransferResponse (CancelInputDeviceTransferResponse'),
    newCancelInputDeviceTransferResponse,

    -- ** PurchaseOffering
    PurchaseOffering' (PurchaseOffering''),
    newPurchaseOffering',
    PurchaseOfferingResponse (PurchaseOfferingResponse'),
    newPurchaseOfferingResponse,

    -- ** StartMultiplex
    StartMultiplex (StartMultiplex'),
    newStartMultiplex,
    StartMultiplexResponse (StartMultiplexResponse'),
    newStartMultiplexResponse,

    -- ** StopMultiplex
    StopMultiplex (StopMultiplex'),
    newStopMultiplex,
    StopMultiplexResponse (StopMultiplexResponse'),
    newStopMultiplexResponse,

    -- ** DescribeSchedule (Paginated)
    DescribeSchedule (DescribeSchedule'),
    newDescribeSchedule,
    DescribeScheduleResponse (DescribeScheduleResponse'),
    newDescribeScheduleResponse,

    -- ** CreateMultiplexProgram
    CreateMultiplexProgram' (CreateMultiplexProgram''),
    newCreateMultiplexProgram',
    CreateMultiplexProgramResponse (CreateMultiplexProgramResponse'),
    newCreateMultiplexProgramResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** TransferInputDevice
    TransferInputDevice' (TransferInputDevice''),
    newTransferInputDevice',
    TransferInputDeviceResponse (TransferInputDeviceResponse'),
    newTransferInputDeviceResponse,

    -- ** ListMultiplexPrograms (Paginated)
    ListMultiplexPrograms (ListMultiplexPrograms'),
    newListMultiplexPrograms,
    ListMultiplexProgramsResponse (ListMultiplexProgramsResponse'),
    newListMultiplexProgramsResponse,

    -- ** DescribeMultiplex
    DescribeMultiplex (DescribeMultiplex'),
    newDescribeMultiplex,
    DescribeMultiplexResponse (DescribeMultiplexResponse'),
    newDescribeMultiplexResponse,

    -- ** BatchDelete
    BatchDelete' (BatchDelete''),
    newBatchDelete',
    BatchDeleteResponse (BatchDeleteResponse'),
    newBatchDeleteResponse,

    -- ** CreateInput
    CreateInput' (CreateInput''),
    newCreateInput',
    CreateInputResponse (CreateInputResponse'),
    newCreateInputResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel' (UpdateChannel''),
    newUpdateChannel',
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- * Types

    -- ** AacCodingMode
    AacCodingMode (..),

    -- ** AacInputType
    AacInputType (..),

    -- ** AacProfile
    AacProfile (..),

    -- ** AacRateControlMode
    AacRateControlMode (..),

    -- ** AacRawFormat
    AacRawFormat (..),

    -- ** AacSpec
    AacSpec (..),

    -- ** AacVbrQuality
    AacVbrQuality (..),

    -- ** Ac3BitstreamMode
    Ac3BitstreamMode (..),

    -- ** Ac3CodingMode
    Ac3CodingMode (..),

    -- ** Ac3DrcProfile
    Ac3DrcProfile (..),

    -- ** Ac3LfeFilter
    Ac3LfeFilter (..),

    -- ** Ac3MetadataControl
    Ac3MetadataControl (..),

    -- ** AcceptHeader
    AcceptHeader (..),

    -- ** AfdSignaling
    AfdSignaling (..),

    -- ** AudioDescriptionAudioTypeControl
    AudioDescriptionAudioTypeControl (..),

    -- ** AudioDescriptionLanguageCodeControl
    AudioDescriptionLanguageCodeControl (..),

    -- ** AudioLanguageSelectionPolicy
    AudioLanguageSelectionPolicy (..),

    -- ** AudioNormalizationAlgorithm
    AudioNormalizationAlgorithm (..),

    -- ** AudioNormalizationAlgorithmControl
    AudioNormalizationAlgorithmControl (..),

    -- ** AudioOnlyHlsSegmentType
    AudioOnlyHlsSegmentType (..),

    -- ** AudioOnlyHlsTrackType
    AudioOnlyHlsTrackType (..),

    -- ** AudioType
    AudioType (..),

    -- ** AuthenticationScheme
    AuthenticationScheme (..),

    -- ** AvailBlankingState
    AvailBlankingState (..),

    -- ** BlackoutSlateNetworkEndBlackout
    BlackoutSlateNetworkEndBlackout (..),

    -- ** BlackoutSlateState
    BlackoutSlateState (..),

    -- ** BurnInAlignment
    BurnInAlignment (..),

    -- ** BurnInBackgroundColor
    BurnInBackgroundColor (..),

    -- ** BurnInFontColor
    BurnInFontColor (..),

    -- ** BurnInOutlineColor
    BurnInOutlineColor (..),

    -- ** BurnInShadowColor
    BurnInShadowColor (..),

    -- ** BurnInTeletextGridControl
    BurnInTeletextGridControl (..),

    -- ** CdiInputResolution
    CdiInputResolution (..),

    -- ** ChannelClass
    ChannelClass (..),

    -- ** ChannelState
    ChannelState (..),

    -- ** ContentType
    ContentType (..),

    -- ** DeviceSettingsSyncState
    DeviceSettingsSyncState (..),

    -- ** DeviceUpdateStatus
    DeviceUpdateStatus (..),

    -- ** DvbSdtOutputSdt
    DvbSdtOutputSdt (..),

    -- ** DvbSubDestinationAlignment
    DvbSubDestinationAlignment (..),

    -- ** DvbSubDestinationBackgroundColor
    DvbSubDestinationBackgroundColor (..),

    -- ** DvbSubDestinationFontColor
    DvbSubDestinationFontColor (..),

    -- ** DvbSubDestinationOutlineColor
    DvbSubDestinationOutlineColor (..),

    -- ** DvbSubDestinationShadowColor
    DvbSubDestinationShadowColor (..),

    -- ** DvbSubDestinationTeletextGridControl
    DvbSubDestinationTeletextGridControl (..),

    -- ** Eac3AttenuationControl
    Eac3AttenuationControl (..),

    -- ** Eac3BitstreamMode
    Eac3BitstreamMode (..),

    -- ** Eac3CodingMode
    Eac3CodingMode (..),

    -- ** Eac3DcFilter
    Eac3DcFilter (..),

    -- ** Eac3DrcLine
    Eac3DrcLine (..),

    -- ** Eac3DrcRf
    Eac3DrcRf (..),

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

    -- ** EbuTtDDestinationStyleControl
    EbuTtDDestinationStyleControl (..),

    -- ** EbuTtDFillLineGapControl
    EbuTtDFillLineGapControl (..),

    -- ** EmbeddedConvert608To708
    EmbeddedConvert608To708 (..),

    -- ** EmbeddedScte20Detection
    EmbeddedScte20Detection (..),

    -- ** FeatureActivationsInputPrepareScheduleActions
    FeatureActivationsInputPrepareScheduleActions (..),

    -- ** FecOutputIncludeFec
    FecOutputIncludeFec (..),

    -- ** FixedAfd
    FixedAfd (..),

    -- ** Fmp4NielsenId3Behavior
    Fmp4NielsenId3Behavior (..),

    -- ** Fmp4TimedMetadataBehavior
    Fmp4TimedMetadataBehavior (..),

    -- ** FollowPoint
    FollowPoint (..),

    -- ** FrameCaptureIntervalUnit
    FrameCaptureIntervalUnit (..),

    -- ** GlobalConfigurationInputEndAction
    GlobalConfigurationInputEndAction (..),

    -- ** GlobalConfigurationLowFramerateInputs
    GlobalConfigurationLowFramerateInputs (..),

    -- ** GlobalConfigurationOutputLockingMode
    GlobalConfigurationOutputLockingMode (..),

    -- ** GlobalConfigurationOutputTimingSource
    GlobalConfigurationOutputTimingSource (..),

    -- ** H264AdaptiveQuantization
    H264AdaptiveQuantization (..),

    -- ** H264ColorMetadata
    H264ColorMetadata (..),

    -- ** H264EntropyEncoding
    H264EntropyEncoding (..),

    -- ** H264FlickerAq
    H264FlickerAq (..),

    -- ** H264ForceFieldPictures
    H264ForceFieldPictures (..),

    -- ** H264FramerateControl
    H264FramerateControl (..),

    -- ** H264GopBReference
    H264GopBReference (..),

    -- ** H264GopSizeUnits
    H264GopSizeUnits (..),

    -- ** H264Level
    H264Level (..),

    -- ** H264LookAheadRateControl
    H264LookAheadRateControl (..),

    -- ** H264ParControl
    H264ParControl (..),

    -- ** H264Profile
    H264Profile (..),

    -- ** H264QualityLevel
    H264QualityLevel (..),

    -- ** H264RateControlMode
    H264RateControlMode (..),

    -- ** H264ScanType
    H264ScanType (..),

    -- ** H264SceneChangeDetect
    H264SceneChangeDetect (..),

    -- ** H264SpatialAq
    H264SpatialAq (..),

    -- ** H264SubGopLength
    H264SubGopLength (..),

    -- ** H264Syntax
    H264Syntax (..),

    -- ** H264TemporalAq
    H264TemporalAq (..),

    -- ** H264TimecodeInsertionBehavior
    H264TimecodeInsertionBehavior (..),

    -- ** H265AdaptiveQuantization
    H265AdaptiveQuantization (..),

    -- ** H265AlternativeTransferFunction
    H265AlternativeTransferFunction (..),

    -- ** H265ColorMetadata
    H265ColorMetadata (..),

    -- ** H265FlickerAq
    H265FlickerAq (..),

    -- ** H265GopSizeUnits
    H265GopSizeUnits (..),

    -- ** H265Level
    H265Level (..),

    -- ** H265LookAheadRateControl
    H265LookAheadRateControl (..),

    -- ** H265Profile
    H265Profile (..),

    -- ** H265RateControlMode
    H265RateControlMode (..),

    -- ** H265ScanType
    H265ScanType (..),

    -- ** H265SceneChangeDetect
    H265SceneChangeDetect (..),

    -- ** H265Tier
    H265Tier (..),

    -- ** H265TimecodeInsertionBehavior
    H265TimecodeInsertionBehavior (..),

    -- ** HlsAdMarkers
    HlsAdMarkers (..),

    -- ** HlsAkamaiHttpTransferMode
    HlsAkamaiHttpTransferMode (..),

    -- ** HlsCaptionLanguageSetting
    HlsCaptionLanguageSetting (..),

    -- ** HlsClientCache
    HlsClientCache (..),

    -- ** HlsCodecSpecification
    HlsCodecSpecification (..),

    -- ** HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- ** HlsDiscontinuityTags
    HlsDiscontinuityTags (..),

    -- ** HlsEncryptionType
    HlsEncryptionType (..),

    -- ** HlsH265PackagingType
    HlsH265PackagingType (..),

    -- ** HlsId3SegmentTaggingState
    HlsId3SegmentTaggingState (..),

    -- ** HlsIncompleteSegmentBehavior
    HlsIncompleteSegmentBehavior (..),

    -- ** HlsIvInManifest
    HlsIvInManifest (..),

    -- ** HlsIvSource
    HlsIvSource (..),

    -- ** HlsManifestCompression
    HlsManifestCompression (..),

    -- ** HlsManifestDurationFormat
    HlsManifestDurationFormat (..),

    -- ** HlsMediaStoreStorageClass
    HlsMediaStoreStorageClass (..),

    -- ** HlsMode
    HlsMode (..),

    -- ** HlsOutputSelection
    HlsOutputSelection (..),

    -- ** HlsProgramDateTime
    HlsProgramDateTime (..),

    -- ** HlsRedundantManifest
    HlsRedundantManifest (..),

    -- ** HlsSegmentationMode
    HlsSegmentationMode (..),

    -- ** HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- ** HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- ** HlsTsFileMode
    HlsTsFileMode (..),

    -- ** HlsWebdavHttpTransferMode
    HlsWebdavHttpTransferMode (..),

    -- ** IFrameOnlyPlaylistType
    IFrameOnlyPlaylistType (..),

    -- ** InputClass
    InputClass (..),

    -- ** InputCodec
    InputCodec (..),

    -- ** InputDeblockFilter
    InputDeblockFilter (..),

    -- ** InputDenoiseFilter
    InputDenoiseFilter (..),

    -- ** InputDeviceActiveInput
    InputDeviceActiveInput (..),

    -- ** InputDeviceConfiguredInput
    InputDeviceConfiguredInput (..),

    -- ** InputDeviceConnectionState
    InputDeviceConnectionState (..),

    -- ** InputDeviceIpScheme
    InputDeviceIpScheme (..),

    -- ** InputDeviceScanType
    InputDeviceScanType (..),

    -- ** InputDeviceState
    InputDeviceState (..),

    -- ** InputDeviceTransferType
    InputDeviceTransferType (..),

    -- ** InputDeviceType
    InputDeviceType (..),

    -- ** InputFilter
    InputFilter (..),

    -- ** InputLossActionForHlsOut
    InputLossActionForHlsOut (..),

    -- ** InputLossActionForMsSmoothOut
    InputLossActionForMsSmoothOut (..),

    -- ** InputLossActionForRtmpOut
    InputLossActionForRtmpOut (..),

    -- ** InputLossActionForUdpOut
    InputLossActionForUdpOut (..),

    -- ** InputLossImageType
    InputLossImageType (..),

    -- ** InputMaximumBitrate
    InputMaximumBitrate (..),

    -- ** InputPreference
    InputPreference (..),

    -- ** InputResolution
    InputResolution (..),

    -- ** InputSecurityGroupState
    InputSecurityGroupState (..),

    -- ** InputSourceEndBehavior
    InputSourceEndBehavior (..),

    -- ** InputSourceType
    InputSourceType (..),

    -- ** InputState
    InputState (..),

    -- ** InputTimecodeSource
    InputTimecodeSource (..),

    -- ** InputType
    InputType (..),

    -- ** LastFrameClippingBehavior
    LastFrameClippingBehavior (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** M2tsAbsentInputAudioBehavior
    M2tsAbsentInputAudioBehavior (..),

    -- ** M2tsArib
    M2tsArib (..),

    -- ** M2tsAribCaptionsPidControl
    M2tsAribCaptionsPidControl (..),

    -- ** M2tsAudioBufferModel
    M2tsAudioBufferModel (..),

    -- ** M2tsAudioInterval
    M2tsAudioInterval (..),

    -- ** M2tsAudioStreamType
    M2tsAudioStreamType (..),

    -- ** M2tsBufferModel
    M2tsBufferModel (..),

    -- ** M2tsCcDescriptor
    M2tsCcDescriptor (..),

    -- ** M2tsEbifControl
    M2tsEbifControl (..),

    -- ** M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- ** M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- ** M2tsKlv
    M2tsKlv (..),

    -- ** M2tsNielsenId3Behavior
    M2tsNielsenId3Behavior (..),

    -- ** M2tsPcrControl
    M2tsPcrControl (..),

    -- ** M2tsRateMode
    M2tsRateMode (..),

    -- ** M2tsScte35Control
    M2tsScte35Control (..),

    -- ** M2tsSegmentationMarkers
    M2tsSegmentationMarkers (..),

    -- ** M2tsSegmentationStyle
    M2tsSegmentationStyle (..),

    -- ** M2tsTimedMetadataBehavior
    M2tsTimedMetadataBehavior (..),

    -- ** M3u8NielsenId3Behavior
    M3u8NielsenId3Behavior (..),

    -- ** M3u8PcrControl
    M3u8PcrControl (..),

    -- ** M3u8Scte35Behavior
    M3u8Scte35Behavior (..),

    -- ** M3u8TimedMetadataBehavior
    M3u8TimedMetadataBehavior (..),

    -- ** Mp2CodingMode
    Mp2CodingMode (..),

    -- ** Mpeg2AdaptiveQuantization
    Mpeg2AdaptiveQuantization (..),

    -- ** Mpeg2ColorMetadata
    Mpeg2ColorMetadata (..),

    -- ** Mpeg2ColorSpace
    Mpeg2ColorSpace (..),

    -- ** Mpeg2DisplayRatio
    Mpeg2DisplayRatio (..),

    -- ** Mpeg2GopSizeUnits
    Mpeg2GopSizeUnits (..),

    -- ** Mpeg2ScanType
    Mpeg2ScanType (..),

    -- ** Mpeg2SubGopLength
    Mpeg2SubGopLength (..),

    -- ** Mpeg2TimecodeInsertionBehavior
    Mpeg2TimecodeInsertionBehavior (..),

    -- ** MsSmoothH265PackagingType
    MsSmoothH265PackagingType (..),

    -- ** MultiplexState
    MultiplexState (..),

    -- ** NetworkInputServerValidation
    NetworkInputServerValidation (..),

    -- ** NielsenPcmToId3TaggingState
    NielsenPcmToId3TaggingState (..),

    -- ** OfferingDurationUnits
    OfferingDurationUnits (..),

    -- ** OfferingType
    OfferingType (..),

    -- ** PipelineId
    PipelineId (..),

    -- ** PreferredChannelPipeline
    PreferredChannelPipeline (..),

    -- ** ReservationCodec
    ReservationCodec (..),

    -- ** ReservationMaximumBitrate
    ReservationMaximumBitrate (..),

    -- ** ReservationMaximumFramerate
    ReservationMaximumFramerate (..),

    -- ** ReservationResolution
    ReservationResolution (..),

    -- ** ReservationResourceType
    ReservationResourceType (..),

    -- ** ReservationSpecialFeature
    ReservationSpecialFeature (..),

    -- ** ReservationState
    ReservationState (..),

    -- ** ReservationVideoQuality
    ReservationVideoQuality (..),

    -- ** RtmpAdMarkers
    RtmpAdMarkers (..),

    -- ** RtmpCacheFullBehavior
    RtmpCacheFullBehavior (..),

    -- ** RtmpCaptionData
    RtmpCaptionData (..),

    -- ** RtmpOutputCertificateMode
    RtmpOutputCertificateMode (..),

    -- ** Scte20Convert608To708
    Scte20Convert608To708 (..),

    -- ** Scte35AposNoRegionalBlackoutBehavior
    Scte35AposNoRegionalBlackoutBehavior (..),

    -- ** Scte35AposWebDeliveryAllowedBehavior
    Scte35AposWebDeliveryAllowedBehavior (..),

    -- ** Scte35ArchiveAllowedFlag
    Scte35ArchiveAllowedFlag (..),

    -- ** Scte35DeviceRestrictions
    Scte35DeviceRestrictions (..),

    -- ** Scte35NoRegionalBlackoutFlag
    Scte35NoRegionalBlackoutFlag (..),

    -- ** Scte35SegmentationCancelIndicator
    Scte35SegmentationCancelIndicator (..),

    -- ** Scte35SpliceInsertNoRegionalBlackoutBehavior
    Scte35SpliceInsertNoRegionalBlackoutBehavior (..),

    -- ** Scte35SpliceInsertWebDeliveryAllowedBehavior
    Scte35SpliceInsertWebDeliveryAllowedBehavior (..),

    -- ** Scte35WebDeliveryAllowedFlag
    Scte35WebDeliveryAllowedFlag (..),

    -- ** SmoothGroupAudioOnlyTimecodeControl
    SmoothGroupAudioOnlyTimecodeControl (..),

    -- ** SmoothGroupCertificateMode
    SmoothGroupCertificateMode (..),

    -- ** SmoothGroupEventIdMode
    SmoothGroupEventIdMode (..),

    -- ** SmoothGroupEventStopBehavior
    SmoothGroupEventStopBehavior (..),

    -- ** SmoothGroupSegmentationMode
    SmoothGroupSegmentationMode (..),

    -- ** SmoothGroupSparseTrackType
    SmoothGroupSparseTrackType (..),

    -- ** SmoothGroupStreamManifestBehavior
    SmoothGroupStreamManifestBehavior (..),

    -- ** SmoothGroupTimestampOffsetMode
    SmoothGroupTimestampOffsetMode (..),

    -- ** Smpte2038DataPreference
    Smpte2038DataPreference (..),

    -- ** TemporalFilterPostFilterSharpening
    TemporalFilterPostFilterSharpening (..),

    -- ** TemporalFilterStrength
    TemporalFilterStrength (..),

    -- ** TimecodeConfigSource
    TimecodeConfigSource (..),

    -- ** TtmlDestinationStyleControl
    TtmlDestinationStyleControl (..),

    -- ** UdpTimedMetadataId3Frame
    UdpTimedMetadataId3Frame (..),

    -- ** VideoDescriptionRespondToAfd
    VideoDescriptionRespondToAfd (..),

    -- ** VideoDescriptionScalingBehavior
    VideoDescriptionScalingBehavior (..),

    -- ** VideoSelectorColorSpace
    VideoSelectorColorSpace (..),

    -- ** VideoSelectorColorSpaceUsage
    VideoSelectorColorSpaceUsage (..),

    -- ** WavCodingMode
    WavCodingMode (..),

    -- ** AacSettings
    AacSettings (AacSettings'),
    newAacSettings,

    -- ** Ac3Settings
    Ac3Settings (Ac3Settings'),
    newAc3Settings,

    -- ** AncillarySourceSettings
    AncillarySourceSettings (AncillarySourceSettings'),
    newAncillarySourceSettings,

    -- ** ArchiveContainerSettings
    ArchiveContainerSettings (ArchiveContainerSettings'),
    newArchiveContainerSettings,

    -- ** ArchiveGroupSettings
    ArchiveGroupSettings (ArchiveGroupSettings'),
    newArchiveGroupSettings,

    -- ** ArchiveOutputSettings
    ArchiveOutputSettings (ArchiveOutputSettings'),
    newArchiveOutputSettings,

    -- ** AribDestinationSettings
    AribDestinationSettings (AribDestinationSettings'),
    newAribDestinationSettings,

    -- ** AribSourceSettings
    AribSourceSettings (AribSourceSettings'),
    newAribSourceSettings,

    -- ** AudioChannelMapping
    AudioChannelMapping (AudioChannelMapping'),
    newAudioChannelMapping,

    -- ** AudioCodecSettings
    AudioCodecSettings (AudioCodecSettings'),
    newAudioCodecSettings,

    -- ** AudioDescription
    AudioDescription (AudioDescription'),
    newAudioDescription,

    -- ** AudioLanguageSelection
    AudioLanguageSelection (AudioLanguageSelection'),
    newAudioLanguageSelection,

    -- ** AudioNormalizationSettings
    AudioNormalizationSettings (AudioNormalizationSettings'),
    newAudioNormalizationSettings,

    -- ** AudioOnlyHlsSettings
    AudioOnlyHlsSettings (AudioOnlyHlsSettings'),
    newAudioOnlyHlsSettings,

    -- ** AudioPidSelection
    AudioPidSelection (AudioPidSelection'),
    newAudioPidSelection,

    -- ** AudioSelector
    AudioSelector (AudioSelector'),
    newAudioSelector,

    -- ** AudioSelectorSettings
    AudioSelectorSettings (AudioSelectorSettings'),
    newAudioSelectorSettings,

    -- ** AudioSilenceFailoverSettings
    AudioSilenceFailoverSettings (AudioSilenceFailoverSettings'),
    newAudioSilenceFailoverSettings,

    -- ** AudioTrack
    AudioTrack (AudioTrack'),
    newAudioTrack,

    -- ** AudioTrackSelection
    AudioTrackSelection (AudioTrackSelection'),
    newAudioTrackSelection,

    -- ** AutomaticInputFailoverSettings
    AutomaticInputFailoverSettings (AutomaticInputFailoverSettings'),
    newAutomaticInputFailoverSettings,

    -- ** AvailBlanking
    AvailBlanking (AvailBlanking'),
    newAvailBlanking,

    -- ** AvailConfiguration
    AvailConfiguration (AvailConfiguration'),
    newAvailConfiguration,

    -- ** AvailSettings
    AvailSettings (AvailSettings'),
    newAvailSettings,

    -- ** BatchFailedResultModel
    BatchFailedResultModel (BatchFailedResultModel'),
    newBatchFailedResultModel,

    -- ** BatchScheduleActionCreateRequest
    BatchScheduleActionCreateRequest (BatchScheduleActionCreateRequest'),
    newBatchScheduleActionCreateRequest,

    -- ** BatchScheduleActionCreateResult
    BatchScheduleActionCreateResult (BatchScheduleActionCreateResult'),
    newBatchScheduleActionCreateResult,

    -- ** BatchScheduleActionDeleteRequest
    BatchScheduleActionDeleteRequest (BatchScheduleActionDeleteRequest'),
    newBatchScheduleActionDeleteRequest,

    -- ** BatchScheduleActionDeleteResult
    BatchScheduleActionDeleteResult (BatchScheduleActionDeleteResult'),
    newBatchScheduleActionDeleteResult,

    -- ** BatchSuccessfulResultModel
    BatchSuccessfulResultModel (BatchSuccessfulResultModel'),
    newBatchSuccessfulResultModel,

    -- ** BlackoutSlate
    BlackoutSlate (BlackoutSlate'),
    newBlackoutSlate,

    -- ** BurnInDestinationSettings
    BurnInDestinationSettings (BurnInDestinationSettings'),
    newBurnInDestinationSettings,

    -- ** CaptionDescription
    CaptionDescription (CaptionDescription'),
    newCaptionDescription,

    -- ** CaptionDestinationSettings
    CaptionDestinationSettings (CaptionDestinationSettings'),
    newCaptionDestinationSettings,

    -- ** CaptionLanguageMapping
    CaptionLanguageMapping (CaptionLanguageMapping'),
    newCaptionLanguageMapping,

    -- ** CaptionSelector
    CaptionSelector (CaptionSelector'),
    newCaptionSelector,

    -- ** CaptionSelectorSettings
    CaptionSelectorSettings (CaptionSelectorSettings'),
    newCaptionSelectorSettings,

    -- ** CdiInputSpecification
    CdiInputSpecification (CdiInputSpecification'),
    newCdiInputSpecification,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelEgressEndpoint
    ChannelEgressEndpoint (ChannelEgressEndpoint'),
    newChannelEgressEndpoint,

    -- ** ChannelSummary
    ChannelSummary (ChannelSummary'),
    newChannelSummary,

    -- ** ColorSpacePassthroughSettings
    ColorSpacePassthroughSettings (ColorSpacePassthroughSettings'),
    newColorSpacePassthroughSettings,

    -- ** DvbNitSettings
    DvbNitSettings (DvbNitSettings'),
    newDvbNitSettings,

    -- ** DvbSdtSettings
    DvbSdtSettings (DvbSdtSettings'),
    newDvbSdtSettings,

    -- ** DvbSubDestinationSettings
    DvbSubDestinationSettings (DvbSubDestinationSettings'),
    newDvbSubDestinationSettings,

    -- ** DvbSubSourceSettings
    DvbSubSourceSettings (DvbSubSourceSettings'),
    newDvbSubSourceSettings,

    -- ** DvbTdtSettings
    DvbTdtSettings (DvbTdtSettings'),
    newDvbTdtSettings,

    -- ** Eac3Settings
    Eac3Settings (Eac3Settings'),
    newEac3Settings,

    -- ** EbuTtDDestinationSettings
    EbuTtDDestinationSettings (EbuTtDDestinationSettings'),
    newEbuTtDDestinationSettings,

    -- ** EmbeddedDestinationSettings
    EmbeddedDestinationSettings (EmbeddedDestinationSettings'),
    newEmbeddedDestinationSettings,

    -- ** EmbeddedPlusScte20DestinationSettings
    EmbeddedPlusScte20DestinationSettings (EmbeddedPlusScte20DestinationSettings'),
    newEmbeddedPlusScte20DestinationSettings,

    -- ** EmbeddedSourceSettings
    EmbeddedSourceSettings (EmbeddedSourceSettings'),
    newEmbeddedSourceSettings,

    -- ** EncoderSettings
    EncoderSettings (EncoderSettings'),
    newEncoderSettings,

    -- ** FailoverCondition
    FailoverCondition (FailoverCondition'),
    newFailoverCondition,

    -- ** FailoverConditionSettings
    FailoverConditionSettings (FailoverConditionSettings'),
    newFailoverConditionSettings,

    -- ** FeatureActivations
    FeatureActivations (FeatureActivations'),
    newFeatureActivations,

    -- ** FecOutputSettings
    FecOutputSettings (FecOutputSettings'),
    newFecOutputSettings,

    -- ** FixedModeScheduleActionStartSettings
    FixedModeScheduleActionStartSettings (FixedModeScheduleActionStartSettings'),
    newFixedModeScheduleActionStartSettings,

    -- ** Fmp4HlsSettings
    Fmp4HlsSettings (Fmp4HlsSettings'),
    newFmp4HlsSettings,

    -- ** FollowModeScheduleActionStartSettings
    FollowModeScheduleActionStartSettings (FollowModeScheduleActionStartSettings'),
    newFollowModeScheduleActionStartSettings,

    -- ** FrameCaptureGroupSettings
    FrameCaptureGroupSettings (FrameCaptureGroupSettings'),
    newFrameCaptureGroupSettings,

    -- ** FrameCaptureHlsSettings
    FrameCaptureHlsSettings (FrameCaptureHlsSettings'),
    newFrameCaptureHlsSettings,

    -- ** FrameCaptureOutputSettings
    FrameCaptureOutputSettings (FrameCaptureOutputSettings'),
    newFrameCaptureOutputSettings,

    -- ** FrameCaptureSettings
    FrameCaptureSettings (FrameCaptureSettings'),
    newFrameCaptureSettings,

    -- ** GlobalConfiguration
    GlobalConfiguration (GlobalConfiguration'),
    newGlobalConfiguration,

    -- ** H264ColorSpaceSettings
    H264ColorSpaceSettings (H264ColorSpaceSettings'),
    newH264ColorSpaceSettings,

    -- ** H264FilterSettings
    H264FilterSettings (H264FilterSettings'),
    newH264FilterSettings,

    -- ** H264Settings
    H264Settings (H264Settings'),
    newH264Settings,

    -- ** H265ColorSpaceSettings
    H265ColorSpaceSettings (H265ColorSpaceSettings'),
    newH265ColorSpaceSettings,

    -- ** H265FilterSettings
    H265FilterSettings (H265FilterSettings'),
    newH265FilterSettings,

    -- ** H265Settings
    H265Settings (H265Settings'),
    newH265Settings,

    -- ** Hdr10Settings
    Hdr10Settings (Hdr10Settings'),
    newHdr10Settings,

    -- ** HlsAkamaiSettings
    HlsAkamaiSettings (HlsAkamaiSettings'),
    newHlsAkamaiSettings,

    -- ** HlsBasicPutSettings
    HlsBasicPutSettings (HlsBasicPutSettings'),
    newHlsBasicPutSettings,

    -- ** HlsCdnSettings
    HlsCdnSettings (HlsCdnSettings'),
    newHlsCdnSettings,

    -- ** HlsGroupSettings
    HlsGroupSettings (HlsGroupSettings'),
    newHlsGroupSettings,

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    HlsId3SegmentTaggingScheduleActionSettings (HlsId3SegmentTaggingScheduleActionSettings'),
    newHlsId3SegmentTaggingScheduleActionSettings,

    -- ** HlsInputSettings
    HlsInputSettings (HlsInputSettings'),
    newHlsInputSettings,

    -- ** HlsMediaStoreSettings
    HlsMediaStoreSettings (HlsMediaStoreSettings'),
    newHlsMediaStoreSettings,

    -- ** HlsOutputSettings
    HlsOutputSettings (HlsOutputSettings'),
    newHlsOutputSettings,

    -- ** HlsSettings
    HlsSettings (HlsSettings'),
    newHlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings (HlsTimedMetadataScheduleActionSettings'),
    newHlsTimedMetadataScheduleActionSettings,

    -- ** HlsWebdavSettings
    HlsWebdavSettings (HlsWebdavSettings'),
    newHlsWebdavSettings,

    -- ** ImmediateModeScheduleActionStartSettings
    ImmediateModeScheduleActionStartSettings (ImmediateModeScheduleActionStartSettings'),
    newImmediateModeScheduleActionStartSettings,

    -- ** Input
    Input (Input'),
    newInput,

    -- ** InputAttachment
    InputAttachment (InputAttachment'),
    newInputAttachment,

    -- ** InputChannelLevel
    InputChannelLevel (InputChannelLevel'),
    newInputChannelLevel,

    -- ** InputClippingSettings
    InputClippingSettings (InputClippingSettings'),
    newInputClippingSettings,

    -- ** InputDestination
    InputDestination (InputDestination'),
    newInputDestination,

    -- ** InputDestinationRequest
    InputDestinationRequest (InputDestinationRequest'),
    newInputDestinationRequest,

    -- ** InputDestinationVpc
    InputDestinationVpc (InputDestinationVpc'),
    newInputDestinationVpc,

    -- ** InputDeviceConfigurableSettings
    InputDeviceConfigurableSettings (InputDeviceConfigurableSettings'),
    newInputDeviceConfigurableSettings,

    -- ** InputDeviceHdSettings
    InputDeviceHdSettings (InputDeviceHdSettings'),
    newInputDeviceHdSettings,

    -- ** InputDeviceNetworkSettings
    InputDeviceNetworkSettings (InputDeviceNetworkSettings'),
    newInputDeviceNetworkSettings,

    -- ** InputDeviceRequest
    InputDeviceRequest (InputDeviceRequest'),
    newInputDeviceRequest,

    -- ** InputDeviceSettings
    InputDeviceSettings (InputDeviceSettings'),
    newInputDeviceSettings,

    -- ** InputDeviceSummary
    InputDeviceSummary (InputDeviceSummary'),
    newInputDeviceSummary,

    -- ** InputDeviceUhdSettings
    InputDeviceUhdSettings (InputDeviceUhdSettings'),
    newInputDeviceUhdSettings,

    -- ** InputLocation
    InputLocation (InputLocation'),
    newInputLocation,

    -- ** InputLossBehavior
    InputLossBehavior (InputLossBehavior'),
    newInputLossBehavior,

    -- ** InputLossFailoverSettings
    InputLossFailoverSettings (InputLossFailoverSettings'),
    newInputLossFailoverSettings,

    -- ** InputPrepareScheduleActionSettings
    InputPrepareScheduleActionSettings (InputPrepareScheduleActionSettings'),
    newInputPrepareScheduleActionSettings,

    -- ** InputSecurityGroup
    InputSecurityGroup (InputSecurityGroup'),
    newInputSecurityGroup,

    -- ** InputSettings
    InputSettings (InputSettings'),
    newInputSettings,

    -- ** InputSource
    InputSource (InputSource'),
    newInputSource,

    -- ** InputSourceRequest
    InputSourceRequest (InputSourceRequest'),
    newInputSourceRequest,

    -- ** InputSpecification
    InputSpecification (InputSpecification'),
    newInputSpecification,

    -- ** InputSwitchScheduleActionSettings
    InputSwitchScheduleActionSettings (InputSwitchScheduleActionSettings'),
    newInputSwitchScheduleActionSettings,

    -- ** InputVpcRequest
    InputVpcRequest (InputVpcRequest'),
    newInputVpcRequest,

    -- ** InputWhitelistRule
    InputWhitelistRule (InputWhitelistRule'),
    newInputWhitelistRule,

    -- ** InputWhitelistRuleCidr
    InputWhitelistRuleCidr (InputWhitelistRuleCidr'),
    newInputWhitelistRuleCidr,

    -- ** KeyProviderSettings
    KeyProviderSettings (KeyProviderSettings'),
    newKeyProviderSettings,

    -- ** M2tsSettings
    M2tsSettings (M2tsSettings'),
    newM2tsSettings,

    -- ** M3u8Settings
    M3u8Settings (M3u8Settings'),
    newM3u8Settings,

    -- ** MediaConnectFlow
    MediaConnectFlow (MediaConnectFlow'),
    newMediaConnectFlow,

    -- ** MediaConnectFlowRequest
    MediaConnectFlowRequest (MediaConnectFlowRequest'),
    newMediaConnectFlowRequest,

    -- ** MediaPackageGroupSettings
    MediaPackageGroupSettings (MediaPackageGroupSettings'),
    newMediaPackageGroupSettings,

    -- ** MediaPackageOutputDestinationSettings
    MediaPackageOutputDestinationSettings (MediaPackageOutputDestinationSettings'),
    newMediaPackageOutputDestinationSettings,

    -- ** MediaPackageOutputSettings
    MediaPackageOutputSettings (MediaPackageOutputSettings'),
    newMediaPackageOutputSettings,

    -- ** Mp2Settings
    Mp2Settings (Mp2Settings'),
    newMp2Settings,

    -- ** Mpeg2FilterSettings
    Mpeg2FilterSettings (Mpeg2FilterSettings'),
    newMpeg2FilterSettings,

    -- ** Mpeg2Settings
    Mpeg2Settings (Mpeg2Settings'),
    newMpeg2Settings,

    -- ** MsSmoothGroupSettings
    MsSmoothGroupSettings (MsSmoothGroupSettings'),
    newMsSmoothGroupSettings,

    -- ** MsSmoothOutputSettings
    MsSmoothOutputSettings (MsSmoothOutputSettings'),
    newMsSmoothOutputSettings,

    -- ** Multiplex
    Multiplex (Multiplex'),
    newMultiplex,

    -- ** MultiplexGroupSettings
    MultiplexGroupSettings (MultiplexGroupSettings'),
    newMultiplexGroupSettings,

    -- ** MultiplexMediaConnectOutputDestinationSettings
    MultiplexMediaConnectOutputDestinationSettings (MultiplexMediaConnectOutputDestinationSettings'),
    newMultiplexMediaConnectOutputDestinationSettings,

    -- ** MultiplexOutputDestination
    MultiplexOutputDestination (MultiplexOutputDestination'),
    newMultiplexOutputDestination,

    -- ** MultiplexOutputSettings
    MultiplexOutputSettings (MultiplexOutputSettings'),
    newMultiplexOutputSettings,

    -- ** MultiplexProgram
    MultiplexProgram (MultiplexProgram'),
    newMultiplexProgram,

    -- ** MultiplexProgramChannelDestinationSettings
    MultiplexProgramChannelDestinationSettings (MultiplexProgramChannelDestinationSettings'),
    newMultiplexProgramChannelDestinationSettings,

    -- ** MultiplexProgramPacketIdentifiersMap
    MultiplexProgramPacketIdentifiersMap (MultiplexProgramPacketIdentifiersMap'),
    newMultiplexProgramPacketIdentifiersMap,

    -- ** MultiplexProgramPipelineDetail
    MultiplexProgramPipelineDetail (MultiplexProgramPipelineDetail'),
    newMultiplexProgramPipelineDetail,

    -- ** MultiplexProgramServiceDescriptor
    MultiplexProgramServiceDescriptor (MultiplexProgramServiceDescriptor'),
    newMultiplexProgramServiceDescriptor,

    -- ** MultiplexProgramSettings
    MultiplexProgramSettings (MultiplexProgramSettings'),
    newMultiplexProgramSettings,

    -- ** MultiplexProgramSummary
    MultiplexProgramSummary (MultiplexProgramSummary'),
    newMultiplexProgramSummary,

    -- ** MultiplexSettings
    MultiplexSettings (MultiplexSettings'),
    newMultiplexSettings,

    -- ** MultiplexSettingsSummary
    MultiplexSettingsSummary (MultiplexSettingsSummary'),
    newMultiplexSettingsSummary,

    -- ** MultiplexStatmuxVideoSettings
    MultiplexStatmuxVideoSettings (MultiplexStatmuxVideoSettings'),
    newMultiplexStatmuxVideoSettings,

    -- ** MultiplexSummary
    MultiplexSummary (MultiplexSummary'),
    newMultiplexSummary,

    -- ** MultiplexVideoSettings
    MultiplexVideoSettings (MultiplexVideoSettings'),
    newMultiplexVideoSettings,

    -- ** NetworkInputSettings
    NetworkInputSettings (NetworkInputSettings'),
    newNetworkInputSettings,

    -- ** NielsenConfiguration
    NielsenConfiguration (NielsenConfiguration'),
    newNielsenConfiguration,

    -- ** Offering
    Offering (Offering'),
    newOffering,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** OutputDestination
    OutputDestination (OutputDestination'),
    newOutputDestination,

    -- ** OutputDestinationSettings
    OutputDestinationSettings (OutputDestinationSettings'),
    newOutputDestinationSettings,

    -- ** OutputGroup
    OutputGroup (OutputGroup'),
    newOutputGroup,

    -- ** OutputGroupSettings
    OutputGroupSettings (OutputGroupSettings'),
    newOutputGroupSettings,

    -- ** OutputLocationRef
    OutputLocationRef (OutputLocationRef'),
    newOutputLocationRef,

    -- ** OutputSettings
    OutputSettings (OutputSettings'),
    newOutputSettings,

    -- ** PassThroughSettings
    PassThroughSettings (PassThroughSettings'),
    newPassThroughSettings,

    -- ** PauseStateScheduleActionSettings
    PauseStateScheduleActionSettings (PauseStateScheduleActionSettings'),
    newPauseStateScheduleActionSettings,

    -- ** PipelineDetail
    PipelineDetail (PipelineDetail'),
    newPipelineDetail,

    -- ** PipelinePauseStateSettings
    PipelinePauseStateSettings (PipelinePauseStateSettings'),
    newPipelinePauseStateSettings,

    -- ** RawSettings
    RawSettings (RawSettings'),
    newRawSettings,

    -- ** Rec601Settings
    Rec601Settings (Rec601Settings'),
    newRec601Settings,

    -- ** Rec709Settings
    Rec709Settings (Rec709Settings'),
    newRec709Settings,

    -- ** RemixSettings
    RemixSettings (RemixSettings'),
    newRemixSettings,

    -- ** Reservation
    Reservation (Reservation'),
    newReservation,

    -- ** ReservationResourceSpecification
    ReservationResourceSpecification (ReservationResourceSpecification'),
    newReservationResourceSpecification,

    -- ** RtmpCaptionInfoDestinationSettings
    RtmpCaptionInfoDestinationSettings (RtmpCaptionInfoDestinationSettings'),
    newRtmpCaptionInfoDestinationSettings,

    -- ** RtmpGroupSettings
    RtmpGroupSettings (RtmpGroupSettings'),
    newRtmpGroupSettings,

    -- ** RtmpOutputSettings
    RtmpOutputSettings (RtmpOutputSettings'),
    newRtmpOutputSettings,

    -- ** ScheduleAction
    ScheduleAction (ScheduleAction'),
    newScheduleAction,

    -- ** ScheduleActionSettings
    ScheduleActionSettings (ScheduleActionSettings'),
    newScheduleActionSettings,

    -- ** ScheduleActionStartSettings
    ScheduleActionStartSettings (ScheduleActionStartSettings'),
    newScheduleActionStartSettings,

    -- ** Scte20PlusEmbeddedDestinationSettings
    Scte20PlusEmbeddedDestinationSettings (Scte20PlusEmbeddedDestinationSettings'),
    newScte20PlusEmbeddedDestinationSettings,

    -- ** Scte20SourceSettings
    Scte20SourceSettings (Scte20SourceSettings'),
    newScte20SourceSettings,

    -- ** Scte27DestinationSettings
    Scte27DestinationSettings (Scte27DestinationSettings'),
    newScte27DestinationSettings,

    -- ** Scte27SourceSettings
    Scte27SourceSettings (Scte27SourceSettings'),
    newScte27SourceSettings,

    -- ** Scte35DeliveryRestrictions
    Scte35DeliveryRestrictions (Scte35DeliveryRestrictions'),
    newScte35DeliveryRestrictions,

    -- ** Scte35Descriptor
    Scte35Descriptor (Scte35Descriptor'),
    newScte35Descriptor,

    -- ** Scte35DescriptorSettings
    Scte35DescriptorSettings (Scte35DescriptorSettings'),
    newScte35DescriptorSettings,

    -- ** Scte35ReturnToNetworkScheduleActionSettings
    Scte35ReturnToNetworkScheduleActionSettings (Scte35ReturnToNetworkScheduleActionSettings'),
    newScte35ReturnToNetworkScheduleActionSettings,

    -- ** Scte35SegmentationDescriptor
    Scte35SegmentationDescriptor (Scte35SegmentationDescriptor'),
    newScte35SegmentationDescriptor,

    -- ** Scte35SpliceInsert
    Scte35SpliceInsert (Scte35SpliceInsert'),
    newScte35SpliceInsert,

    -- ** Scte35SpliceInsertScheduleActionSettings
    Scte35SpliceInsertScheduleActionSettings (Scte35SpliceInsertScheduleActionSettings'),
    newScte35SpliceInsertScheduleActionSettings,

    -- ** Scte35TimeSignalApos
    Scte35TimeSignalApos (Scte35TimeSignalApos'),
    newScte35TimeSignalApos,

    -- ** Scte35TimeSignalScheduleActionSettings
    Scte35TimeSignalScheduleActionSettings (Scte35TimeSignalScheduleActionSettings'),
    newScte35TimeSignalScheduleActionSettings,

    -- ** SmpteTtDestinationSettings
    SmpteTtDestinationSettings (SmpteTtDestinationSettings'),
    newSmpteTtDestinationSettings,

    -- ** StandardHlsSettings
    StandardHlsSettings (StandardHlsSettings'),
    newStandardHlsSettings,

    -- ** StartTimecode
    StartTimecode (StartTimecode'),
    newStartTimecode,

    -- ** StaticImageActivateScheduleActionSettings
    StaticImageActivateScheduleActionSettings (StaticImageActivateScheduleActionSettings'),
    newStaticImageActivateScheduleActionSettings,

    -- ** StaticImageDeactivateScheduleActionSettings
    StaticImageDeactivateScheduleActionSettings (StaticImageDeactivateScheduleActionSettings'),
    newStaticImageDeactivateScheduleActionSettings,

    -- ** StaticKeySettings
    StaticKeySettings (StaticKeySettings'),
    newStaticKeySettings,

    -- ** StopTimecode
    StopTimecode (StopTimecode'),
    newStopTimecode,

    -- ** TeletextDestinationSettings
    TeletextDestinationSettings (TeletextDestinationSettings'),
    newTeletextDestinationSettings,

    -- ** TeletextSourceSettings
    TeletextSourceSettings (TeletextSourceSettings'),
    newTeletextSourceSettings,

    -- ** TemporalFilterSettings
    TemporalFilterSettings (TemporalFilterSettings'),
    newTemporalFilterSettings,

    -- ** TimecodeConfig
    TimecodeConfig (TimecodeConfig'),
    newTimecodeConfig,

    -- ** TransferringInputDeviceSummary
    TransferringInputDeviceSummary (TransferringInputDeviceSummary'),
    newTransferringInputDeviceSummary,

    -- ** TtmlDestinationSettings
    TtmlDestinationSettings (TtmlDestinationSettings'),
    newTtmlDestinationSettings,

    -- ** UdpContainerSettings
    UdpContainerSettings (UdpContainerSettings'),
    newUdpContainerSettings,

    -- ** UdpGroupSettings
    UdpGroupSettings (UdpGroupSettings'),
    newUdpGroupSettings,

    -- ** UdpOutputSettings
    UdpOutputSettings (UdpOutputSettings'),
    newUdpOutputSettings,

    -- ** VideoBlackFailoverSettings
    VideoBlackFailoverSettings (VideoBlackFailoverSettings'),
    newVideoBlackFailoverSettings,

    -- ** VideoCodecSettings
    VideoCodecSettings (VideoCodecSettings'),
    newVideoCodecSettings,

    -- ** VideoDescription
    VideoDescription (VideoDescription'),
    newVideoDescription,

    -- ** VideoSelector
    VideoSelector (VideoSelector'),
    newVideoSelector,

    -- ** VideoSelectorPid
    VideoSelectorPid (VideoSelectorPid'),
    newVideoSelectorPid,

    -- ** VideoSelectorProgramId
    VideoSelectorProgramId (VideoSelectorProgramId'),
    newVideoSelectorProgramId,

    -- ** VideoSelectorSettings
    VideoSelectorSettings (VideoSelectorSettings'),
    newVideoSelectorSettings,

    -- ** VpcOutputSettings
    VpcOutputSettings (VpcOutputSettings'),
    newVpcOutputSettings,

    -- ** WavSettings
    WavSettings (WavSettings'),
    newWavSettings,

    -- ** WebvttDestinationSettings
    WebvttDestinationSettings (WebvttDestinationSettings'),
    newWebvttDestinationSettings,
  )
where

import Network.AWS.MediaLive.AcceptInputDeviceTransfer
import Network.AWS.MediaLive.BatchDelete
import Network.AWS.MediaLive.BatchStart
import Network.AWS.MediaLive.BatchStop
import Network.AWS.MediaLive.BatchUpdateSchedule
import Network.AWS.MediaLive.CancelInputDeviceTransfer
import Network.AWS.MediaLive.CreateChannel
import Network.AWS.MediaLive.CreateInput
import Network.AWS.MediaLive.CreateInputSecurityGroup
import Network.AWS.MediaLive.CreateMultiplex
import Network.AWS.MediaLive.CreateMultiplexProgram
import Network.AWS.MediaLive.CreatePartnerInput
import Network.AWS.MediaLive.CreateTags
import Network.AWS.MediaLive.DeleteChannel
import Network.AWS.MediaLive.DeleteInput
import Network.AWS.MediaLive.DeleteInputSecurityGroup
import Network.AWS.MediaLive.DeleteMultiplex
import Network.AWS.MediaLive.DeleteMultiplexProgram
import Network.AWS.MediaLive.DeleteReservation
import Network.AWS.MediaLive.DeleteSchedule
import Network.AWS.MediaLive.DeleteTags
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeInputDevice
import Network.AWS.MediaLive.DescribeInputDeviceThumbnail
import Network.AWS.MediaLive.DescribeInputSecurityGroup
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.DescribeMultiplexProgram
import Network.AWS.MediaLive.DescribeOffering
import Network.AWS.MediaLive.DescribeReservation
import Network.AWS.MediaLive.DescribeSchedule
import Network.AWS.MediaLive.Lens
import Network.AWS.MediaLive.ListChannels
import Network.AWS.MediaLive.ListInputDeviceTransfers
import Network.AWS.MediaLive.ListInputDevices
import Network.AWS.MediaLive.ListInputSecurityGroups
import Network.AWS.MediaLive.ListInputs
import Network.AWS.MediaLive.ListMultiplexPrograms
import Network.AWS.MediaLive.ListMultiplexes
import Network.AWS.MediaLive.ListOfferings
import Network.AWS.MediaLive.ListReservations
import Network.AWS.MediaLive.ListTagsForResource
import Network.AWS.MediaLive.PurchaseOffering
import Network.AWS.MediaLive.RejectInputDeviceTransfer
import Network.AWS.MediaLive.StartChannel
import Network.AWS.MediaLive.StartMultiplex
import Network.AWS.MediaLive.StopChannel
import Network.AWS.MediaLive.StopMultiplex
import Network.AWS.MediaLive.TransferInputDevice
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.UpdateChannel
import Network.AWS.MediaLive.UpdateChannelClass
import Network.AWS.MediaLive.UpdateInput
import Network.AWS.MediaLive.UpdateInputDevice
import Network.AWS.MediaLive.UpdateInputSecurityGroup
import Network.AWS.MediaLive.UpdateMultiplex
import Network.AWS.MediaLive.UpdateMultiplexProgram
import Network.AWS.MediaLive.UpdateReservation
import Network.AWS.MediaLive.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaLive'.

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
