{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaLive
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- API for AWS Elemental MediaLive
module Amazonka.MediaLive
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadGatewayException
    _BadGatewayException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** GatewayTimeoutException
    _GatewayTimeoutException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- * Waiters
    -- $waiters

    -- ** ChannelCreated
    newChannelCreated,

    -- ** ChannelDeleted
    newChannelDeleted,

    -- ** ChannelRunning
    newChannelRunning,

    -- ** ChannelStopped
    newChannelStopped,

    -- ** InputAttached
    newInputAttached,

    -- ** InputDeleted
    newInputDeleted,

    -- ** InputDetached
    newInputDetached,

    -- ** MultiplexCreated
    newMultiplexCreated,

    -- ** MultiplexDeleted
    newMultiplexDeleted,

    -- ** MultiplexRunning
    newMultiplexRunning,

    -- ** MultiplexStopped
    newMultiplexStopped,

    -- * Operations
    -- $operations

    -- ** AcceptInputDeviceTransfer
    AcceptInputDeviceTransfer (AcceptInputDeviceTransfer'),
    newAcceptInputDeviceTransfer,
    AcceptInputDeviceTransferResponse (AcceptInputDeviceTransferResponse'),
    newAcceptInputDeviceTransferResponse,

    -- ** BatchDelete
    BatchDelete' (BatchDelete''),
    newBatchDelete',
    BatchDeleteResponse (BatchDeleteResponse'),
    newBatchDeleteResponse,

    -- ** BatchStart
    BatchStart' (BatchStart''),
    newBatchStart',
    BatchStartResponse (BatchStartResponse'),
    newBatchStartResponse,

    -- ** BatchStop
    BatchStop' (BatchStop''),
    newBatchStop',
    BatchStopResponse (BatchStopResponse'),
    newBatchStopResponse,

    -- ** BatchUpdateSchedule
    BatchUpdateSchedule (BatchUpdateSchedule'),
    newBatchUpdateSchedule,
    BatchUpdateScheduleResponse (BatchUpdateScheduleResponse'),
    newBatchUpdateScheduleResponse,

    -- ** CancelInputDeviceTransfer
    CancelInputDeviceTransfer (CancelInputDeviceTransfer'),
    newCancelInputDeviceTransfer,
    CancelInputDeviceTransferResponse (CancelInputDeviceTransferResponse'),
    newCancelInputDeviceTransferResponse,

    -- ** ClaimDevice
    ClaimDevice (ClaimDevice'),
    newClaimDevice,
    ClaimDeviceResponse (ClaimDeviceResponse'),
    newClaimDeviceResponse,

    -- ** CreateChannel
    CreateChannel' (CreateChannel''),
    newCreateChannel',
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateInput
    CreateInput' (CreateInput''),
    newCreateInput',
    CreateInputResponse (CreateInputResponse'),
    newCreateInputResponse,

    -- ** CreateInputSecurityGroup
    CreateInputSecurityGroup (CreateInputSecurityGroup'),
    newCreateInputSecurityGroup,
    CreateInputSecurityGroupResponse (CreateInputSecurityGroupResponse'),
    newCreateInputSecurityGroupResponse,

    -- ** CreateMultiplex
    CreateMultiplex' (CreateMultiplex''),
    newCreateMultiplex',
    CreateMultiplexResponse (CreateMultiplexResponse'),
    newCreateMultiplexResponse,

    -- ** CreateMultiplexProgram
    CreateMultiplexProgram' (CreateMultiplexProgram''),
    newCreateMultiplexProgram',
    CreateMultiplexProgramResponse (CreateMultiplexProgramResponse'),
    newCreateMultiplexProgramResponse,

    -- ** CreatePartnerInput
    CreatePartnerInput' (CreatePartnerInput''),
    newCreatePartnerInput',
    CreatePartnerInputResponse (CreatePartnerInputResponse'),
    newCreatePartnerInputResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteInput
    DeleteInput (DeleteInput'),
    newDeleteInput,
    DeleteInputResponse (DeleteInputResponse'),
    newDeleteInputResponse,

    -- ** DeleteInputSecurityGroup
    DeleteInputSecurityGroup (DeleteInputSecurityGroup'),
    newDeleteInputSecurityGroup,
    DeleteInputSecurityGroupResponse (DeleteInputSecurityGroupResponse'),
    newDeleteInputSecurityGroupResponse,

    -- ** DeleteMultiplex
    DeleteMultiplex (DeleteMultiplex'),
    newDeleteMultiplex,
    DeleteMultiplexResponse (DeleteMultiplexResponse'),
    newDeleteMultiplexResponse,

    -- ** DeleteMultiplexProgram
    DeleteMultiplexProgram (DeleteMultiplexProgram'),
    newDeleteMultiplexProgram,
    DeleteMultiplexProgramResponse (DeleteMultiplexProgramResponse'),
    newDeleteMultiplexProgramResponse,

    -- ** DeleteReservation
    DeleteReservation (DeleteReservation'),
    newDeleteReservation,
    DeleteReservationResponse (DeleteReservationResponse'),
    newDeleteReservationResponse,

    -- ** DeleteSchedule
    DeleteSchedule (DeleteSchedule'),
    newDeleteSchedule,
    DeleteScheduleResponse (DeleteScheduleResponse'),
    newDeleteScheduleResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeInput
    DescribeInput (DescribeInput'),
    newDescribeInput,
    DescribeInputResponse (DescribeInputResponse'),
    newDescribeInputResponse,

    -- ** DescribeInputDevice
    DescribeInputDevice (DescribeInputDevice'),
    newDescribeInputDevice,
    DescribeInputDeviceResponse (DescribeInputDeviceResponse'),
    newDescribeInputDeviceResponse,

    -- ** DescribeInputDeviceThumbnail
    DescribeInputDeviceThumbnail (DescribeInputDeviceThumbnail'),
    newDescribeInputDeviceThumbnail,
    DescribeInputDeviceThumbnailResponse (DescribeInputDeviceThumbnailResponse'),
    newDescribeInputDeviceThumbnailResponse,

    -- ** DescribeInputSecurityGroup
    DescribeInputSecurityGroup (DescribeInputSecurityGroup'),
    newDescribeInputSecurityGroup,
    DescribeInputSecurityGroupResponse (DescribeInputSecurityGroupResponse'),
    newDescribeInputSecurityGroupResponse,

    -- ** DescribeMultiplex
    DescribeMultiplex (DescribeMultiplex'),
    newDescribeMultiplex,
    DescribeMultiplexResponse (DescribeMultiplexResponse'),
    newDescribeMultiplexResponse,

    -- ** DescribeMultiplexProgram
    DescribeMultiplexProgram (DescribeMultiplexProgram'),
    newDescribeMultiplexProgram,
    DescribeMultiplexProgramResponse (DescribeMultiplexProgramResponse'),
    newDescribeMultiplexProgramResponse,

    -- ** DescribeOffering
    DescribeOffering (DescribeOffering'),
    newDescribeOffering,
    DescribeOfferingResponse (DescribeOfferingResponse'),
    newDescribeOfferingResponse,

    -- ** DescribeReservation
    DescribeReservation (DescribeReservation'),
    newDescribeReservation,
    DescribeReservationResponse (DescribeReservationResponse'),
    newDescribeReservationResponse,

    -- ** DescribeSchedule (Paginated)
    DescribeSchedule (DescribeSchedule'),
    newDescribeSchedule,
    DescribeScheduleResponse (DescribeScheduleResponse'),
    newDescribeScheduleResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListInputDeviceTransfers (Paginated)
    ListInputDeviceTransfers (ListInputDeviceTransfers'),
    newListInputDeviceTransfers,
    ListInputDeviceTransfersResponse (ListInputDeviceTransfersResponse'),
    newListInputDeviceTransfersResponse,

    -- ** ListInputDevices (Paginated)
    ListInputDevices (ListInputDevices'),
    newListInputDevices,
    ListInputDevicesResponse (ListInputDevicesResponse'),
    newListInputDevicesResponse,

    -- ** ListInputSecurityGroups (Paginated)
    ListInputSecurityGroups (ListInputSecurityGroups'),
    newListInputSecurityGroups,
    ListInputSecurityGroupsResponse (ListInputSecurityGroupsResponse'),
    newListInputSecurityGroupsResponse,

    -- ** ListInputs (Paginated)
    ListInputs (ListInputs'),
    newListInputs,
    ListInputsResponse (ListInputsResponse'),
    newListInputsResponse,

    -- ** ListMultiplexPrograms (Paginated)
    ListMultiplexPrograms (ListMultiplexPrograms'),
    newListMultiplexPrograms,
    ListMultiplexProgramsResponse (ListMultiplexProgramsResponse'),
    newListMultiplexProgramsResponse,

    -- ** ListMultiplexes (Paginated)
    ListMultiplexes (ListMultiplexes'),
    newListMultiplexes,
    ListMultiplexesResponse (ListMultiplexesResponse'),
    newListMultiplexesResponse,

    -- ** ListOfferings (Paginated)
    ListOfferings (ListOfferings'),
    newListOfferings,
    ListOfferingsResponse (ListOfferingsResponse'),
    newListOfferingsResponse,

    -- ** ListReservations (Paginated)
    ListReservations (ListReservations'),
    newListReservations,
    ListReservationsResponse (ListReservationsResponse'),
    newListReservationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PurchaseOffering
    PurchaseOffering' (PurchaseOffering''),
    newPurchaseOffering',
    PurchaseOfferingResponse (PurchaseOfferingResponse'),
    newPurchaseOfferingResponse,

    -- ** RebootInputDevice
    RebootInputDevice' (RebootInputDevice''),
    newRebootInputDevice',
    RebootInputDeviceResponse (RebootInputDeviceResponse'),
    newRebootInputDeviceResponse,

    -- ** RejectInputDeviceTransfer
    RejectInputDeviceTransfer (RejectInputDeviceTransfer'),
    newRejectInputDeviceTransfer,
    RejectInputDeviceTransferResponse (RejectInputDeviceTransferResponse'),
    newRejectInputDeviceTransferResponse,

    -- ** StartChannel
    StartChannel (StartChannel'),
    newStartChannel,
    StartChannelResponse (StartChannelResponse'),
    newStartChannelResponse,

    -- ** StartInputDeviceMaintenanceWindow
    StartInputDeviceMaintenanceWindow (StartInputDeviceMaintenanceWindow'),
    newStartInputDeviceMaintenanceWindow,
    StartInputDeviceMaintenanceWindowResponse (StartInputDeviceMaintenanceWindowResponse'),
    newStartInputDeviceMaintenanceWindowResponse,

    -- ** StartMultiplex
    StartMultiplex (StartMultiplex'),
    newStartMultiplex,
    StartMultiplexResponse (StartMultiplexResponse'),
    newStartMultiplexResponse,

    -- ** StopChannel
    StopChannel (StopChannel'),
    newStopChannel,
    StopChannelResponse (StopChannelResponse'),
    newStopChannelResponse,

    -- ** StopMultiplex
    StopMultiplex (StopMultiplex'),
    newStopMultiplex,
    StopMultiplexResponse (StopMultiplexResponse'),
    newStopMultiplexResponse,

    -- ** TransferInputDevice
    TransferInputDevice' (TransferInputDevice''),
    newTransferInputDevice',
    TransferInputDeviceResponse (TransferInputDeviceResponse'),
    newTransferInputDeviceResponse,

    -- ** UpdateChannel
    UpdateChannel' (UpdateChannel''),
    newUpdateChannel',
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** UpdateChannelClass
    UpdateChannelClass' (UpdateChannelClass''),
    newUpdateChannelClass',
    UpdateChannelClassResponse (UpdateChannelClassResponse'),
    newUpdateChannelClassResponse,

    -- ** UpdateInput
    UpdateInput' (UpdateInput''),
    newUpdateInput',
    UpdateInputResponse (UpdateInputResponse'),
    newUpdateInputResponse,

    -- ** UpdateInputDevice
    UpdateInputDevice' (UpdateInputDevice''),
    newUpdateInputDevice',
    UpdateInputDeviceResponse (UpdateInputDeviceResponse'),
    newUpdateInputDeviceResponse,

    -- ** UpdateInputSecurityGroup
    UpdateInputSecurityGroup (UpdateInputSecurityGroup'),
    newUpdateInputSecurityGroup,
    UpdateInputSecurityGroupResponse (UpdateInputSecurityGroupResponse'),
    newUpdateInputSecurityGroupResponse,

    -- ** UpdateMultiplex
    UpdateMultiplex' (UpdateMultiplex''),
    newUpdateMultiplex',
    UpdateMultiplexResponse (UpdateMultiplexResponse'),
    newUpdateMultiplexResponse,

    -- ** UpdateMultiplexProgram
    UpdateMultiplexProgram' (UpdateMultiplexProgram''),
    newUpdateMultiplexProgram',
    UpdateMultiplexProgramResponse (UpdateMultiplexProgramResponse'),
    newUpdateMultiplexProgramResponse,

    -- ** UpdateReservation
    UpdateReservation' (UpdateReservation''),
    newUpdateReservation',
    UpdateReservationResponse (UpdateReservationResponse'),
    newUpdateReservationResponse,

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

    -- ** AccessibilityType
    AccessibilityType (..),

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

    -- ** DvbSubOcrLanguage
    DvbSubOcrLanguage (..),

    -- ** Eac3AtmosCodingMode
    Eac3AtmosCodingMode (..),

    -- ** Eac3AtmosDrcLine
    Eac3AtmosDrcLine (..),

    -- ** Eac3AtmosDrcRf
    Eac3AtmosDrcRf (..),

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

    -- ** HlsProgramDateTimeClock
    HlsProgramDateTimeClock (..),

    -- ** HlsRedundantManifest
    HlsRedundantManifest (..),

    -- ** HlsScte35SourceType
    HlsScte35SourceType (..),

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

    -- ** MaintenanceDay
    MaintenanceDay (..),

    -- ** MotionGraphicsInsertion
    MotionGraphicsInsertion (..),

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

    -- ** NielsenWatermarksCbetStepaside
    NielsenWatermarksCbetStepaside (..),

    -- ** NielsenWatermarksDistributionTypes
    NielsenWatermarksDistributionTypes (..),

    -- ** OfferingDurationUnits
    OfferingDurationUnits (..),

    -- ** OfferingType
    OfferingType (..),

    -- ** PipelineId
    PipelineId (..),

    -- ** PreferredChannelPipeline
    PreferredChannelPipeline (..),

    -- ** RebootInputDeviceForce
    RebootInputDeviceForce (..),

    -- ** ReservationAutomaticRenewal
    ReservationAutomaticRenewal (..),

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

    -- ** S3CannedAcl
    S3CannedAcl (..),

    -- ** Scte20Convert608To708
    Scte20Convert608To708 (..),

    -- ** Scte27OcrLanguage
    Scte27OcrLanguage (..),

    -- ** Scte35AposNoRegionalBlackoutBehavior
    Scte35AposNoRegionalBlackoutBehavior (..),

    -- ** Scte35AposWebDeliveryAllowedBehavior
    Scte35AposWebDeliveryAllowedBehavior (..),

    -- ** Scte35ArchiveAllowedFlag
    Scte35ArchiveAllowedFlag (..),

    -- ** Scte35DeviceRestrictions
    Scte35DeviceRestrictions (..),

    -- ** Scte35InputMode
    Scte35InputMode (..),

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

    -- ** WebvttDestinationStyleControl
    WebvttDestinationStyleControl (..),

    -- ** AacSettings
    AacSettings (AacSettings'),
    newAacSettings,

    -- ** Ac3Settings
    Ac3Settings (Ac3Settings'),
    newAc3Settings,

    -- ** AncillarySourceSettings
    AncillarySourceSettings (AncillarySourceSettings'),
    newAncillarySourceSettings,

    -- ** ArchiveCdnSettings
    ArchiveCdnSettings (ArchiveCdnSettings'),
    newArchiveCdnSettings,

    -- ** ArchiveContainerSettings
    ArchiveContainerSettings (ArchiveContainerSettings'),
    newArchiveContainerSettings,

    -- ** ArchiveGroupSettings
    ArchiveGroupSettings (ArchiveGroupSettings'),
    newArchiveGroupSettings,

    -- ** ArchiveOutputSettings
    ArchiveOutputSettings (ArchiveOutputSettings'),
    newArchiveOutputSettings,

    -- ** ArchiveS3Settings
    ArchiveS3Settings (ArchiveS3Settings'),
    newArchiveS3Settings,

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

    -- ** AudioHlsRenditionSelection
    AudioHlsRenditionSelection (AudioHlsRenditionSelection'),
    newAudioHlsRenditionSelection,

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

    -- ** AudioWatermarkSettings
    AudioWatermarkSettings (AudioWatermarkSettings'),
    newAudioWatermarkSettings,

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

    -- ** CaptionRectangle
    CaptionRectangle (CaptionRectangle'),
    newCaptionRectangle,

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

    -- ** DolbyVision81Settings
    DolbyVision81Settings (DolbyVision81Settings'),
    newDolbyVision81Settings,

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

    -- ** Eac3AtmosSettings
    Eac3AtmosSettings (Eac3AtmosSettings'),
    newEac3AtmosSettings,

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

    -- ** Esam
    Esam (Esam'),
    newEsam,

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

    -- ** FrameCaptureCdnSettings
    FrameCaptureCdnSettings (FrameCaptureCdnSettings'),
    newFrameCaptureCdnSettings,

    -- ** FrameCaptureGroupSettings
    FrameCaptureGroupSettings (FrameCaptureGroupSettings'),
    newFrameCaptureGroupSettings,

    -- ** FrameCaptureHlsSettings
    FrameCaptureHlsSettings (FrameCaptureHlsSettings'),
    newFrameCaptureHlsSettings,

    -- ** FrameCaptureOutputSettings
    FrameCaptureOutputSettings (FrameCaptureOutputSettings'),
    newFrameCaptureOutputSettings,

    -- ** FrameCaptureS3Settings
    FrameCaptureS3Settings (FrameCaptureS3Settings'),
    newFrameCaptureS3Settings,

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

    -- ** HlsS3Settings
    HlsS3Settings (HlsS3Settings'),
    newHlsS3Settings,

    -- ** HlsSettings
    HlsSettings (HlsSettings'),
    newHlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings (HlsTimedMetadataScheduleActionSettings'),
    newHlsTimedMetadataScheduleActionSettings,

    -- ** HlsWebdavSettings
    HlsWebdavSettings (HlsWebdavSettings'),
    newHlsWebdavSettings,

    -- ** HtmlMotionGraphicsSettings
    HtmlMotionGraphicsSettings (HtmlMotionGraphicsSettings'),
    newHtmlMotionGraphicsSettings,

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

    -- ** MaintenanceCreateSettings
    MaintenanceCreateSettings (MaintenanceCreateSettings'),
    newMaintenanceCreateSettings,

    -- ** MaintenanceStatus
    MaintenanceStatus (MaintenanceStatus'),
    newMaintenanceStatus,

    -- ** MaintenanceUpdateSettings
    MaintenanceUpdateSettings (MaintenanceUpdateSettings'),
    newMaintenanceUpdateSettings,

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

    -- ** MotionGraphicsActivateScheduleActionSettings
    MotionGraphicsActivateScheduleActionSettings (MotionGraphicsActivateScheduleActionSettings'),
    newMotionGraphicsActivateScheduleActionSettings,

    -- ** MotionGraphicsConfiguration
    MotionGraphicsConfiguration (MotionGraphicsConfiguration'),
    newMotionGraphicsConfiguration,

    -- ** MotionGraphicsDeactivateScheduleActionSettings
    MotionGraphicsDeactivateScheduleActionSettings (MotionGraphicsDeactivateScheduleActionSettings'),
    newMotionGraphicsDeactivateScheduleActionSettings,

    -- ** MotionGraphicsSettings
    MotionGraphicsSettings (MotionGraphicsSettings'),
    newMotionGraphicsSettings,

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

    -- ** NielsenCBET
    NielsenCBET (NielsenCBET'),
    newNielsenCBET,

    -- ** NielsenConfiguration
    NielsenConfiguration (NielsenConfiguration'),
    newNielsenConfiguration,

    -- ** NielsenNaesIiNw
    NielsenNaesIiNw (NielsenNaesIiNw'),
    newNielsenNaesIiNw,

    -- ** NielsenWatermarksSettings
    NielsenWatermarksSettings (NielsenWatermarksSettings'),
    newNielsenWatermarksSettings,

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

    -- ** RenewalSettings
    RenewalSettings (RenewalSettings'),
    newRenewalSettings,

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

    -- ** Scte35InputScheduleActionSettings
    Scte35InputScheduleActionSettings (Scte35InputScheduleActionSettings'),
    newScte35InputScheduleActionSettings,

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

    -- ** VideoSelectorColorSpaceSettings
    VideoSelectorColorSpaceSettings (VideoSelectorColorSpaceSettings'),
    newVideoSelectorColorSpaceSettings,

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

    -- ** VpcOutputSettingsDescription
    VpcOutputSettingsDescription (VpcOutputSettingsDescription'),
    newVpcOutputSettingsDescription,

    -- ** WavSettings
    WavSettings (WavSettings'),
    newWavSettings,

    -- ** WebvttDestinationSettings
    WebvttDestinationSettings (WebvttDestinationSettings'),
    newWebvttDestinationSettings,
  )
where

import Amazonka.MediaLive.AcceptInputDeviceTransfer
import Amazonka.MediaLive.BatchDelete
import Amazonka.MediaLive.BatchStart
import Amazonka.MediaLive.BatchStop
import Amazonka.MediaLive.BatchUpdateSchedule
import Amazonka.MediaLive.CancelInputDeviceTransfer
import Amazonka.MediaLive.ClaimDevice
import Amazonka.MediaLive.CreateChannel
import Amazonka.MediaLive.CreateInput
import Amazonka.MediaLive.CreateInputSecurityGroup
import Amazonka.MediaLive.CreateMultiplex
import Amazonka.MediaLive.CreateMultiplexProgram
import Amazonka.MediaLive.CreatePartnerInput
import Amazonka.MediaLive.CreateTags
import Amazonka.MediaLive.DeleteChannel
import Amazonka.MediaLive.DeleteInput
import Amazonka.MediaLive.DeleteInputSecurityGroup
import Amazonka.MediaLive.DeleteMultiplex
import Amazonka.MediaLive.DeleteMultiplexProgram
import Amazonka.MediaLive.DeleteReservation
import Amazonka.MediaLive.DeleteSchedule
import Amazonka.MediaLive.DeleteTags
import Amazonka.MediaLive.DescribeChannel
import Amazonka.MediaLive.DescribeInput
import Amazonka.MediaLive.DescribeInputDevice
import Amazonka.MediaLive.DescribeInputDeviceThumbnail
import Amazonka.MediaLive.DescribeInputSecurityGroup
import Amazonka.MediaLive.DescribeMultiplex
import Amazonka.MediaLive.DescribeMultiplexProgram
import Amazonka.MediaLive.DescribeOffering
import Amazonka.MediaLive.DescribeReservation
import Amazonka.MediaLive.DescribeSchedule
import Amazonka.MediaLive.Lens
import Amazonka.MediaLive.ListChannels
import Amazonka.MediaLive.ListInputDeviceTransfers
import Amazonka.MediaLive.ListInputDevices
import Amazonka.MediaLive.ListInputSecurityGroups
import Amazonka.MediaLive.ListInputs
import Amazonka.MediaLive.ListMultiplexPrograms
import Amazonka.MediaLive.ListMultiplexes
import Amazonka.MediaLive.ListOfferings
import Amazonka.MediaLive.ListReservations
import Amazonka.MediaLive.ListTagsForResource
import Amazonka.MediaLive.PurchaseOffering
import Amazonka.MediaLive.RebootInputDevice
import Amazonka.MediaLive.RejectInputDeviceTransfer
import Amazonka.MediaLive.StartChannel
import Amazonka.MediaLive.StartInputDeviceMaintenanceWindow
import Amazonka.MediaLive.StartMultiplex
import Amazonka.MediaLive.StopChannel
import Amazonka.MediaLive.StopMultiplex
import Amazonka.MediaLive.TransferInputDevice
import Amazonka.MediaLive.Types
import Amazonka.MediaLive.UpdateChannel
import Amazonka.MediaLive.UpdateChannelClass
import Amazonka.MediaLive.UpdateInput
import Amazonka.MediaLive.UpdateInputDevice
import Amazonka.MediaLive.UpdateInputSecurityGroup
import Amazonka.MediaLive.UpdateMultiplex
import Amazonka.MediaLive.UpdateMultiplexProgram
import Amazonka.MediaLive.UpdateReservation
import Amazonka.MediaLive.Waiters

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
