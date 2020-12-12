{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
  ( -- * Service configuration
    mediaLiveService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** ChannelRunning
    mkChannelRunning,

    -- ** InputAttached
    mkInputAttached,

    -- ** MultiplexRunning
    mkMultiplexRunning,

    -- ** MultiplexDeleted
    mkMultiplexDeleted,

    -- ** InputDetached
    mkInputDetached,

    -- ** InputDeleted
    mkInputDeleted,

    -- ** ChannelStopped
    mkChannelStopped,

    -- ** MultiplexCreated
    mkMultiplexCreated,

    -- ** ChannelCreated
    mkChannelCreated,

    -- ** ChannelDeleted
    mkChannelDeleted,

    -- ** MultiplexStopped
    mkMultiplexStopped,

    -- * Operations
    -- $operations

    -- ** UpdateChannelClass
    module Network.AWS.MediaLive.UpdateChannelClass,

    -- ** ListMultiplexes (Paginated)
    module Network.AWS.MediaLive.ListMultiplexes,

    -- ** BatchStart
    module Network.AWS.MediaLive.BatchStart,

    -- ** CreateMultiplex
    module Network.AWS.MediaLive.CreateMultiplex,

    -- ** ListInputDeviceTransfers (Paginated)
    module Network.AWS.MediaLive.ListInputDeviceTransfers,

    -- ** ListInputDevices (Paginated)
    module Network.AWS.MediaLive.ListInputDevices,

    -- ** ListInputs (Paginated)
    module Network.AWS.MediaLive.ListInputs,

    -- ** DescribeInputDeviceThumbnail
    module Network.AWS.MediaLive.DescribeInputDeviceThumbnail,

    -- ** ListChannels (Paginated)
    module Network.AWS.MediaLive.ListChannels,

    -- ** DescribeInputSecurityGroup
    module Network.AWS.MediaLive.DescribeInputSecurityGroup,

    -- ** CreateInput
    module Network.AWS.MediaLive.CreateInput,

    -- ** ListTagsForResource
    module Network.AWS.MediaLive.ListTagsForResource,

    -- ** DeleteChannel
    module Network.AWS.MediaLive.DeleteChannel,

    -- ** UpdateChannel
    module Network.AWS.MediaLive.UpdateChannel,

    -- ** AcceptInputDeviceTransfer
    module Network.AWS.MediaLive.AcceptInputDeviceTransfer,

    -- ** DescribeReservation
    module Network.AWS.MediaLive.DescribeReservation,

    -- ** CreateTags
    module Network.AWS.MediaLive.CreateTags,

    -- ** StopMultiplex
    module Network.AWS.MediaLive.StopMultiplex,

    -- ** DeleteTags
    module Network.AWS.MediaLive.DeleteTags,

    -- ** CreateInputSecurityGroup
    module Network.AWS.MediaLive.CreateInputSecurityGroup,

    -- ** StartChannel
    module Network.AWS.MediaLive.StartChannel,

    -- ** CancelInputDeviceTransfer
    module Network.AWS.MediaLive.CancelInputDeviceTransfer,

    -- ** ListInputSecurityGroups (Paginated)
    module Network.AWS.MediaLive.ListInputSecurityGroups,

    -- ** DeleteReservation
    module Network.AWS.MediaLive.DeleteReservation,

    -- ** UpdateReservation
    module Network.AWS.MediaLive.UpdateReservation,

    -- ** BatchStop
    module Network.AWS.MediaLive.BatchStop,

    -- ** DeleteSchedule
    module Network.AWS.MediaLive.DeleteSchedule,

    -- ** CreateChannel
    module Network.AWS.MediaLive.CreateChannel,

    -- ** DeleteInput
    module Network.AWS.MediaLive.DeleteInput,

    -- ** UpdateInput
    module Network.AWS.MediaLive.UpdateInput,

    -- ** UpdateInputDevice
    module Network.AWS.MediaLive.UpdateInputDevice,

    -- ** RejectInputDeviceTransfer
    module Network.AWS.MediaLive.RejectInputDeviceTransfer,

    -- ** DescribeOffering
    module Network.AWS.MediaLive.DescribeOffering,

    -- ** TransferInputDevice
    module Network.AWS.MediaLive.TransferInputDevice,

    -- ** DeleteMultiplexProgram
    module Network.AWS.MediaLive.DeleteMultiplexProgram,

    -- ** UpdateMultiplexProgram
    module Network.AWS.MediaLive.UpdateMultiplexProgram,

    -- ** BatchDelete
    module Network.AWS.MediaLive.BatchDelete,

    -- ** ListMultiplexPrograms (Paginated)
    module Network.AWS.MediaLive.ListMultiplexPrograms,

    -- ** DescribeMultiplex
    module Network.AWS.MediaLive.DescribeMultiplex,

    -- ** BatchUpdateSchedule
    module Network.AWS.MediaLive.BatchUpdateSchedule,

    -- ** CreateMultiplexProgram
    module Network.AWS.MediaLive.CreateMultiplexProgram,

    -- ** DescribeSchedule (Paginated)
    module Network.AWS.MediaLive.DescribeSchedule,

    -- ** StartMultiplex
    module Network.AWS.MediaLive.StartMultiplex,

    -- ** StopChannel
    module Network.AWS.MediaLive.StopChannel,

    -- ** DescribeInput
    module Network.AWS.MediaLive.DescribeInput,

    -- ** PurchaseOffering
    module Network.AWS.MediaLive.PurchaseOffering,

    -- ** DescribeInputDevice
    module Network.AWS.MediaLive.DescribeInputDevice,

    -- ** DescribeChannel
    module Network.AWS.MediaLive.DescribeChannel,

    -- ** UpdateInputSecurityGroup
    module Network.AWS.MediaLive.UpdateInputSecurityGroup,

    -- ** DeleteInputSecurityGroup
    module Network.AWS.MediaLive.DeleteInputSecurityGroup,

    -- ** ListReservations (Paginated)
    module Network.AWS.MediaLive.ListReservations,

    -- ** DeleteMultiplex
    module Network.AWS.MediaLive.DeleteMultiplex,

    -- ** UpdateMultiplex
    module Network.AWS.MediaLive.UpdateMultiplex,

    -- ** DescribeMultiplexProgram
    module Network.AWS.MediaLive.DescribeMultiplexProgram,

    -- ** ListOfferings (Paginated)
    module Network.AWS.MediaLive.ListOfferings,

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

    -- ** HlsAkamaiHTTPTransferMode
    HlsAkamaiHTTPTransferMode (..),

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

    -- ** HlsWebdavHTTPTransferMode
    HlsWebdavHTTPTransferMode (..),

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

    -- ** InputDeviceIPScheme
    InputDeviceIPScheme (..),

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

    -- ** M2tsCCDescriptor
    M2tsCCDescriptor (..),

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
    AacSettings (..),
    mkAacSettings,
    aRawFormat,
    aCodingMode,
    aProfile,
    aRateControlMode,
    aSampleRate,
    aSpec,
    aBitrate,
    aVbrQuality,
    aInputType,

    -- ** Ac3Settings
    Ac3Settings (..),
    mkAc3Settings,
    asLfeFilter,
    asMetadataControl,
    asBitstreamMode,
    asCodingMode,
    asBitrate,
    asDialnorm,
    asDrcProfile,

    -- ** AncillarySourceSettings
    AncillarySourceSettings (..),
    mkAncillarySourceSettings,
    assSourceAncillaryChannelNumber,

    -- ** ArchiveContainerSettings
    ArchiveContainerSettings (..),
    mkArchiveContainerSettings,
    acsM2tsSettings,
    acsRawSettings,

    -- ** ArchiveGroupSettings
    ArchiveGroupSettings (..),
    mkArchiveGroupSettings,
    agsRolloverInterval,
    agsDestination,

    -- ** ArchiveOutputSettings
    ArchiveOutputSettings (..),
    mkArchiveOutputSettings,
    aosExtension,
    aosNameModifier,
    aosContainerSettings,

    -- ** AribDestinationSettings
    AribDestinationSettings (..),
    mkAribDestinationSettings,

    -- ** AribSourceSettings
    AribSourceSettings (..),
    mkAribSourceSettings,

    -- ** AudioChannelMapping
    AudioChannelMapping (..),
    mkAudioChannelMapping,
    acmOutputChannel,
    acmInputChannelLevels,

    -- ** AudioCodecSettings
    AudioCodecSettings (..),
    mkAudioCodecSettings,
    acsPassThroughSettings,
    acsAc3Settings,
    acsMp2Settings,
    acsWavSettings,
    acsAacSettings,
    acsEac3Settings,

    -- ** AudioDescription
    AudioDescription (..),
    mkAudioDescription,
    adLanguageCode,
    adAudioType,
    adAudioNormalizationSettings,
    adLanguageCodeControl,
    adCodecSettings,
    adStreamName,
    adRemixSettings,
    adAudioTypeControl,
    adAudioSelectorName,
    adName,

    -- ** AudioLanguageSelection
    AudioLanguageSelection (..),
    mkAudioLanguageSelection,
    alsLanguageSelectionPolicy,
    alsLanguageCode,

    -- ** AudioNormalizationSettings
    AudioNormalizationSettings (..),
    mkAudioNormalizationSettings,
    ansAlgorithmControl,
    ansTargetLkfs,
    ansAlgorithm,

    -- ** AudioOnlyHlsSettings
    AudioOnlyHlsSettings (..),
    mkAudioOnlyHlsSettings,
    aohsAudioOnlyImage,
    aohsSegmentType,
    aohsAudioGroupId,
    aohsAudioTrackType,

    -- ** AudioPidSelection
    AudioPidSelection (..),
    mkAudioPidSelection,
    apsPid,

    -- ** AudioSelector
    AudioSelector (..),
    mkAudioSelector,
    asSelectorSettings,
    asName,

    -- ** AudioSelectorSettings
    AudioSelectorSettings (..),
    mkAudioSelectorSettings,
    assAudioLanguageSelection,
    assAudioTrackSelection,
    assAudioPidSelection,

    -- ** AudioTrack
    AudioTrack (..),
    mkAudioTrack,
    atTrack,

    -- ** AudioTrackSelection
    AudioTrackSelection (..),
    mkAudioTrackSelection,
    atsTracks,

    -- ** AutomaticInputFailoverSettings
    AutomaticInputFailoverSettings (..),
    mkAutomaticInputFailoverSettings,
    aifsFailoverConditions,
    aifsErrorClearTimeMsec,
    aifsInputPreference,
    aifsSecondaryInputId,

    -- ** AvailBlanking
    AvailBlanking (..),
    mkAvailBlanking,
    abState,
    abAvailBlankingImage,

    -- ** AvailConfiguration
    AvailConfiguration (..),
    mkAvailConfiguration,
    acAvailSettings,

    -- ** AvailSettings
    AvailSettings (..),
    mkAvailSettings,
    asScte35SpliceInsert,
    asScte35TimeSignalApos,

    -- ** BatchFailedResultModel
    BatchFailedResultModel (..),
    mkBatchFailedResultModel,
    bfrmARN,
    bfrmId,
    bfrmCode,
    bfrmMessage,

    -- ** BatchScheduleActionCreateRequest
    BatchScheduleActionCreateRequest (..),
    mkBatchScheduleActionCreateRequest,
    bsacrScheduleActions,

    -- ** BatchScheduleActionCreateResult
    BatchScheduleActionCreateResult (..),
    mkBatchScheduleActionCreateResult,
    bScheduleActions,

    -- ** BatchScheduleActionDeleteRequest
    BatchScheduleActionDeleteRequest (..),
    mkBatchScheduleActionDeleteRequest,
    bsadrActionNames,

    -- ** BatchScheduleActionDeleteResult
    BatchScheduleActionDeleteResult (..),
    mkBatchScheduleActionDeleteResult,
    bsadrScheduleActions,

    -- ** BatchSuccessfulResultModel
    BatchSuccessfulResultModel (..),
    mkBatchSuccessfulResultModel,
    bsrmState,
    bsrmARN,
    bsrmId,

    -- ** BlackoutSlate
    BlackoutSlate (..),
    mkBlackoutSlate,
    bsNetworkEndBlackoutImage,
    bsState,
    bsNetworkEndBlackout,
    bsNetworkId,
    bsBlackoutSlateImage,

    -- ** BurnInDestinationSettings
    BurnInDestinationSettings (..),
    mkBurnInDestinationSettings,
    bidsBackgroundOpacity,
    bidsFontOpacity,
    bidsShadowYOffset,
    bidsFontResolution,
    bidsYPosition,
    bidsBackgroundColor,
    bidsShadowXOffset,
    bidsFontSize,
    bidsXPosition,
    bidsAlignment,
    bidsShadowOpacity,
    bidsTeletextGridControl,
    bidsOutlineColor,
    bidsOutlineSize,
    bidsFont,
    bidsShadowColor,
    bidsFontColor,

    -- ** CaptionDescription
    CaptionDescription (..),
    mkCaptionDescription,
    cdLanguageCode,
    cdDestinationSettings,
    cdLanguageDescription,
    cdCaptionSelectorName,
    cdName,

    -- ** CaptionDestinationSettings
    CaptionDestinationSettings (..),
    mkCaptionDestinationSettings,
    cdsTeletextDestinationSettings,
    cdsEbuTtDDestinationSettings,
    cdsRtmpCaptionInfoDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsScte27DestinationSettings,
    cdsTtmlDestinationSettings,
    cdsScte20PlusEmbeddedDestinationSettings,
    cdsEmbeddedPlusScte20DestinationSettings,
    cdsSmpteTtDestinationSettings,
    cdsWebvttDestinationSettings,
    cdsEmbeddedDestinationSettings,
    cdsBurnInDestinationSettings,
    cdsAribDestinationSettings,

    -- ** CaptionLanguageMapping
    CaptionLanguageMapping (..),
    mkCaptionLanguageMapping,
    clmLanguageCode,
    clmLanguageDescription,
    clmCaptionChannel,

    -- ** CaptionSelector
    CaptionSelector (..),
    mkCaptionSelector,
    cLanguageCode,
    cSelectorSettings,
    cName,

    -- ** CaptionSelectorSettings
    CaptionSelectorSettings (..),
    mkCaptionSelectorSettings,
    cssTeletextSourceSettings,
    cssAribSourceSettings,
    cssScte27SourceSettings,
    cssDvbSubSourceSettings,
    cssAncillarySourceSettings,
    cssScte20SourceSettings,
    cssEmbeddedSourceSettings,

    -- ** CdiInputSpecification
    CdiInputSpecification (..),
    mkCdiInputSpecification,
    cisResolution,

    -- ** Channel
    Channel (..),
    mkChannel,
    chaState,
    chaLogLevel,
    chaARN,
    chaPipelinesRunningCount,
    chaPipelineDetails,
    chaInputSpecification,
    chaInputAttachments,
    chaDestinations,
    chaName,
    chaCdiInputSpecification,
    chaId,
    chaChannelClass,
    chaEgressEndpoints,
    chaTags,
    chaEncoderSettings,
    chaRoleARN,

    -- ** ChannelEgressEndpoint
    ChannelEgressEndpoint (..),
    mkChannelEgressEndpoint,
    ceeSourceIP,

    -- ** ChannelSummary
    ChannelSummary (..),
    mkChannelSummary,
    csState,
    csLogLevel,
    csARN,
    csPipelinesRunningCount,
    csInputSpecification,
    csInputAttachments,
    csDestinations,
    csName,
    csCdiInputSpecification,
    csId,
    csChannelClass,
    csEgressEndpoints,
    csTags,
    csRoleARN,

    -- ** ColorSpacePassthroughSettings
    ColorSpacePassthroughSettings (..),
    mkColorSpacePassthroughSettings,

    -- ** DvbNitSettings
    DvbNitSettings (..),
    mkDvbNitSettings,
    dnsRepInterval,
    dnsNetworkName,
    dnsNetworkId,

    -- ** DvbSdtSettings
    DvbSdtSettings (..),
    mkDvbSdtSettings,
    dssRepInterval,
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
    dsdsAlignment,
    dsdsShadowOpacity,
    dsdsTeletextGridControl,
    dsdsOutlineColor,
    dsdsOutlineSize,
    dsdsFont,
    dsdsShadowColor,
    dsdsFontColor,

    -- ** DvbSubSourceSettings
    DvbSubSourceSettings (..),
    mkDvbSubSourceSettings,
    dsssPid,

    -- ** DvbTdtSettings
    DvbTdtSettings (..),
    mkDvbTdtSettings,
    dtsRepInterval,

    -- ** Eac3Settings
    Eac3Settings (..),
    mkEac3Settings,
    esStereoDownmix,
    esLoRoCenterMixLevel,
    esLtRtCenterMixLevel,
    esLfeFilter,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esLoRoSurroundMixLevel,
    esSurroundMode,
    esAttenuationControl,
    esPassthroughControl,
    esBitstreamMode,
    esLfeControl,
    esCodingMode,
    esDrcLine,
    esDrcRf,
    esDcFilter,
    esBitrate,
    esPhaseControl,
    esSurroundExMode,
    esDialnorm,

    -- ** EbuTtDDestinationSettings
    EbuTtDDestinationSettings (..),
    mkEbuTtDDestinationSettings,
    etddsFillLineGap,
    etddsFontFamily,
    etddsStyleControl,

    -- ** EmbeddedDestinationSettings
    EmbeddedDestinationSettings (..),
    mkEmbeddedDestinationSettings,

    -- ** EmbeddedPlusScte20DestinationSettings
    EmbeddedPlusScte20DestinationSettings (..),
    mkEmbeddedPlusScte20DestinationSettings,

    -- ** EmbeddedSourceSettings
    EmbeddedSourceSettings (..),
    mkEmbeddedSourceSettings,
    essConvert608To708,
    essScte20Detection,
    essSource608TrackNumber,
    essSource608ChannelNumber,

    -- ** EncoderSettings
    EncoderSettings (..),
    mkEncoderSettings,
    esCaptionDescriptions,
    esAvailConfiguration,
    esFeatureActivations,
    esNielsenConfiguration,
    esAvailBlanking,
    esGlobalConfiguration,
    esBlackoutSlate,
    esVideoDescriptions,
    esAudioDescriptions,
    esOutputGroups,
    esTimecodeConfig,

    -- ** FailoverCondition
    FailoverCondition (..),
    mkFailoverCondition,
    fcFailoverConditionSettings,

    -- ** FailoverConditionSettings
    FailoverConditionSettings (..),
    mkFailoverConditionSettings,
    fcsInputLossSettings,

    -- ** FeatureActivations
    FeatureActivations (..),
    mkFeatureActivations,
    faInputPrepareScheduleActions,

    -- ** FecOutputSettings
    FecOutputSettings (..),
    mkFecOutputSettings,
    fosRowLength,
    fosIncludeFec,
    fosColumnDepth,

    -- ** FixedModeScheduleActionStartSettings
    FixedModeScheduleActionStartSettings (..),
    mkFixedModeScheduleActionStartSettings,
    fmsassTime,

    -- ** Fmp4HlsSettings
    Fmp4HlsSettings (..),
    mkFmp4HlsSettings,
    fhsNielsenId3Behavior,
    fhsAudioRenditionSets,
    fhsTimedMetadataBehavior,

    -- ** FollowModeScheduleActionStartSettings
    FollowModeScheduleActionStartSettings (..),
    mkFollowModeScheduleActionStartSettings,
    fmsassReferenceActionName,
    fmsassFollowPoint,

    -- ** FrameCaptureGroupSettings
    FrameCaptureGroupSettings (..),
    mkFrameCaptureGroupSettings,
    fcgsDestination,

    -- ** FrameCaptureOutputSettings
    FrameCaptureOutputSettings (..),
    mkFrameCaptureOutputSettings,
    fcosNameModifier,

    -- ** FrameCaptureSettings
    FrameCaptureSettings (..),
    mkFrameCaptureSettings,
    fcsCaptureIntervalUnits,
    fcsCaptureInterval,

    -- ** GlobalConfiguration
    GlobalConfiguration (..),
    mkGlobalConfiguration,
    gcOutputLockingMode,
    gcInputLossBehavior,
    gcInitialAudioGain,
    gcSupportLowFramerateInputs,
    gcInputEndAction,
    gcOutputTimingSource,

    -- ** H264ColorSpaceSettings
    H264ColorSpaceSettings (..),
    mkH264ColorSpaceSettings,
    hRec709Settings,
    hRec601Settings,
    hColorSpacePassthroughSettings,

    -- ** H264FilterSettings
    H264FilterSettings (..),
    mkH264FilterSettings,
    hTemporalFilterSettings,

    -- ** H264Settings
    H264Settings (..),
    mkH264Settings,
    hTemporalAq,
    hSceneChangeDetect,
    hScanType,
    hTimecodeInsertion,
    hParNumerator,
    hAfdSignaling,
    hGopSize,
    hGopSizeUnits,
    hSubgopLength,
    hQualityLevel,
    hSlices,
    hProfile,
    hRateControlMode,
    hMinIInterval,
    hQvbrQualityLevel,
    hColorSpaceSettings,
    hParControl,
    hFlickerAq,
    hBufSize,
    hSpatialAq,
    hGopNumBFrames,
    hFixedAfd,
    hSoftness,
    hFilterSettings,
    hBitrate,
    hFramerateDenominator,
    hForceFieldPictures,
    hEntropyEncoding,
    hFramerateControl,
    hColorMetadata,
    hLookAheadRateControl,
    hAdaptiveQuantization,
    hFramerateNumerator,
    hLevel,
    hGopBReference,
    hMaxBitrate,
    hSyntax,
    hBufFillPct,
    hGopClosedCadence,
    hNumRefFrames,
    hParDenominator,

    -- ** H265ColorSpaceSettings
    H265ColorSpaceSettings (..),
    mkH265ColorSpaceSettings,
    hcssHdr10Settings,
    hcssRec709Settings,
    hcssRec601Settings,
    hcssColorSpacePassthroughSettings,

    -- ** H265FilterSettings
    H265FilterSettings (..),
    mkH265FilterSettings,
    hfsTemporalFilterSettings,

    -- ** H265Settings
    H265Settings (..),
    mkH265Settings,
    hsSceneChangeDetect,
    hsScanType,
    hsTimecodeInsertion,
    hsParNumerator,
    hsAfdSignaling,
    hsGopSize,
    hsGopSizeUnits,
    hsSlices,
    hsProfile,
    hsAlternativeTransferFunction,
    hsRateControlMode,
    hsMinIInterval,
    hsQvbrQualityLevel,
    hsColorSpaceSettings,
    hsFlickerAq,
    hsBufSize,
    hsTier,
    hsFixedAfd,
    hsFilterSettings,
    hsBitrate,
    hsColorMetadata,
    hsLookAheadRateControl,
    hsAdaptiveQuantization,
    hsLevel,
    hsMaxBitrate,
    hsGopClosedCadence,
    hsParDenominator,
    hsFramerateNumerator,
    hsFramerateDenominator,

    -- ** Hdr10Settings
    Hdr10Settings (..),
    mkHdr10Settings,
    hsMaxFall,
    hsMaxCll,

    -- ** HlsAkamaiSettings
    HlsAkamaiSettings (..),
    mkHlsAkamaiSettings,
    hasHTTPTransferMode,
    hasNumRetries,
    hasToken,
    hasConnectionRetryInterval,
    hasFilecacheDuration,
    hasRestartDelay,
    hasSalt,

    -- ** HlsBasicPutSettings
    HlsBasicPutSettings (..),
    mkHlsBasicPutSettings,
    hbpsNumRetries,
    hbpsConnectionRetryInterval,
    hbpsFilecacheDuration,
    hbpsRestartDelay,

    -- ** HlsCdnSettings
    HlsCdnSettings (..),
    mkHlsCdnSettings,
    hcsHlsAkamaiSettings,
    hcsHlsMediaStoreSettings,
    hcsHlsBasicPutSettings,
    hcsHlsWebdavSettings,

    -- ** HlsGroupSettings
    HlsGroupSettings (..),
    mkHlsGroupSettings,
    hgsDirectoryStructure,
    hgsEncryptionType,
    hgsTimedMetadataId3Period,
    hgsIvInManifest,
    hgsDiscontinuityTags,
    hgsTsFileMode,
    hgsMinSegmentLength,
    hgsIFrameOnlyPlaylists,
    hgsProgramDateTime,
    hgsIndexNSegments,
    hgsProgramDateTimePeriod,
    hgsCodecSpecification,
    hgsHlsCdnSettings,
    hgsCaptionLanguageMappings,
    hgsInputLossAction,
    hgsMode,
    hgsKeyProviderSettings,
    hgsIncompleteSegmentBehavior,
    hgsConstantIv,
    hgsBaseURLManifest,
    hgsAdMarkers,
    hgsKeyFormat,
    hgsSegmentLength,
    hgsHlsId3SegmentTagging,
    hgsTimedMetadataId3Frame,
    hgsBaseURLContent,
    hgsOutputSelection,
    hgsCaptionLanguageSetting,
    hgsSegmentsPerSubdirectory,
    hgsManifestDurationFormat,
    hgsIvSource,
    hgsSegmentationMode,
    hgsKeyFormatVersions,
    hgsClientCache,
    hgsTimestampDeltaMilliseconds,
    hgsBaseURLManifest1,
    hgsRedundantManifest,
    hgsStreamInfResolution,
    hgsKeepSegments,
    hgsBaseURLContent1,
    hgsManifestCompression,
    hgsDestination,

    -- ** HlsId3SegmentTaggingScheduleActionSettings
    HlsId3SegmentTaggingScheduleActionSettings (..),
    mkHlsId3SegmentTaggingScheduleActionSettings,
    histsasTag,

    -- ** HlsInputSettings
    HlsInputSettings (..),
    mkHlsInputSettings,
    hisBufferSegments,
    hisRetries,
    hisRetryInterval,
    hisBandwidth,

    -- ** HlsMediaStoreSettings
    HlsMediaStoreSettings (..),
    mkHlsMediaStoreSettings,
    hmssNumRetries,
    hmssConnectionRetryInterval,
    hmssFilecacheDuration,
    hmssMediaStoreStorageClass,
    hmssRestartDelay,

    -- ** HlsOutputSettings
    HlsOutputSettings (..),
    mkHlsOutputSettings,
    hosH265PackagingType,
    hosSegmentModifier,
    hosNameModifier,
    hosHlsSettings,

    -- ** HlsSettings
    HlsSettings (..),
    mkHlsSettings,
    hsFmp4HlsSettings,
    hsAudioOnlyHlsSettings,
    hsStandardHlsSettings,

    -- ** HlsTimedMetadataScheduleActionSettings
    HlsTimedMetadataScheduleActionSettings (..),
    mkHlsTimedMetadataScheduleActionSettings,
    htmsasId3,

    -- ** HlsWebdavSettings
    HlsWebdavSettings (..),
    mkHlsWebdavSettings,
    hwsHTTPTransferMode,
    hwsNumRetries,
    hwsConnectionRetryInterval,
    hwsFilecacheDuration,
    hwsRestartDelay,

    -- ** ImmediateModeScheduleActionStartSettings
    ImmediateModeScheduleActionStartSettings (..),
    mkImmediateModeScheduleActionStartSettings,

    -- ** Input
    Input (..),
    mkInput,
    iState,
    iSecurityGroups,
    iARN,
    iInputDevices,
    iSources,
    iDestinations,
    iName,
    iAttachedChannels,
    iId,
    iInputClass,
    iType,
    iMediaConnectFlows,
    iInputSourceType,
    iTags,
    iRoleARN,

    -- ** InputAttachment
    InputAttachment (..),
    mkInputAttachment,
    iaInputAttachmentName,
    iaInputId,
    iaAutomaticInputFailoverSettings,
    iaInputSettings,

    -- ** InputChannelLevel
    InputChannelLevel (..),
    mkInputChannelLevel,
    iclInputChannel,
    iclGain,

    -- ** InputClippingSettings
    InputClippingSettings (..),
    mkInputClippingSettings,
    icsStopTimecode,
    icsStartTimecode,
    icsInputTimecodeSource,

    -- ** InputDestination
    InputDestination (..),
    mkInputDestination,
    idURL,
    idIP,
    idVPC,
    idPort,

    -- ** InputDestinationRequest
    InputDestinationRequest (..),
    mkInputDestinationRequest,
    idrStreamName,

    -- ** InputDestinationVPC
    InputDestinationVPC (..),
    mkInputDestinationVPC,
    idvNetworkInterfaceId,
    idvAvailabilityZone,

    -- ** InputDeviceConfigurableSettings
    InputDeviceConfigurableSettings (..),
    mkInputDeviceConfigurableSettings,
    idcsConfiguredInput,
    idcsMaxBitrate,

    -- ** InputDeviceHdSettings
    InputDeviceHdSettings (..),
    mkInputDeviceHdSettings,
    idhsFramerate,
    idhsScanType,
    idhsDeviceState,
    idhsHeight,
    idhsActiveInput,
    idhsWidth,
    idhsConfiguredInput,
    idhsMaxBitrate,

    -- ** InputDeviceNetworkSettings
    InputDeviceNetworkSettings (..),
    mkInputDeviceNetworkSettings,
    idnsIPAddress,
    idnsGateway,
    idnsDNSAddresses,
    idnsIPScheme,
    idnsSubnetMask,

    -- ** InputDeviceRequest
    InputDeviceRequest (..),
    mkInputDeviceRequest,
    idrId,

    -- ** InputDeviceSettings
    InputDeviceSettings (..),
    mkInputDeviceSettings,
    idssId,

    -- ** InputDeviceSummary
    InputDeviceSummary (..),
    mkInputDeviceSummary,
    idsARN,
    idsMACAddress,
    idsHdDeviceSettings,
    idsName,
    idsId,
    idsDeviceUpdateStatus,
    idsDeviceSettingsSyncState,
    idsType,
    idsSerialNumber,
    idsNetworkSettings,
    idsConnectionState,

    -- ** InputLocation
    InputLocation (..),
    mkInputLocation,
    ilUsername,
    ilPasswordParam,
    ilURI,

    -- ** InputLossBehavior
    InputLossBehavior (..),
    mkInputLossBehavior,
    ilbInputLossImageColor,
    ilbBlackFrameMsec,
    ilbRepeatFrameMsec,
    ilbInputLossImageType,
    ilbInputLossImageSlate,

    -- ** InputLossFailoverSettings
    InputLossFailoverSettings (..),
    mkInputLossFailoverSettings,
    ilfsInputLossThresholdMsec,

    -- ** InputPrepareScheduleActionSettings
    InputPrepareScheduleActionSettings (..),
    mkInputPrepareScheduleActionSettings,
    ipsasInputAttachmentNameReference,
    ipsasInputClippingSettings,
    ipsasURLPath,

    -- ** InputSecurityGroup
    InputSecurityGroup (..),
    mkInputSecurityGroup,
    isgState,
    isgARN,
    isgInputs,
    isgId,
    isgWhitelistRules,
    isgTags,

    -- ** InputSettings
    InputSettings (..),
    mkInputSettings,
    isVideoSelector,
    isSmpte2038DataPreference,
    isNetworkInputSettings,
    isAudioSelectors,
    isDeblockFilter,
    isDenoiseFilter,
    isFilterStrength,
    isCaptionSelectors,
    isInputFilter,
    isSourceEndBehavior,

    -- ** InputSource
    InputSource (..),
    mkInputSource,
    isURL,
    isUsername,
    isPasswordParam,

    -- ** InputSourceRequest
    InputSourceRequest (..),
    mkInputSourceRequest,
    isrURL,
    isrUsername,
    isrPasswordParam,

    -- ** InputSpecification
    InputSpecification (..),
    mkInputSpecification,
    isResolution,
    isCodec,
    isMaximumBitrate,

    -- ** InputSwitchScheduleActionSettings
    InputSwitchScheduleActionSettings (..),
    mkInputSwitchScheduleActionSettings,
    issasInputClippingSettings,
    issasURLPath,
    issasInputAttachmentNameReference,

    -- ** InputVPCRequest
    InputVPCRequest (..),
    mkInputVPCRequest,
    ivrSecurityGroupIds,
    ivrSubnetIds,

    -- ** InputWhitelistRule
    InputWhitelistRule (..),
    mkInputWhitelistRule,
    iwrCidr,

    -- ** InputWhitelistRuleCidr
    InputWhitelistRuleCidr (..),
    mkInputWhitelistRuleCidr,
    iwrcCidr,

    -- ** KeyProviderSettings
    KeyProviderSettings (..),
    mkKeyProviderSettings,
    kpsStaticKeySettings,

    -- ** M2tsSettings
    M2tsSettings (..),
    mkM2tsSettings,
    mPmtPid,
    mEtvSignalPid,
    mVideoPid,
    mNielsenId3Behavior,
    mBufferModel,
    mScte35Pid,
    mTransportStreamId,
    mProgramNum,
    mFragmentTime,
    mTimedMetadataBehavior,
    mCCDescriptor,
    mPmtInterval,
    mDvbSdtSettings,
    mEcmPid,
    mNullPacketBitrate,
    mAudioBufferModel,
    mTimedMetadataPid,
    mKlv,
    mAudioFramesPerPes,
    mPcrPeriod,
    mPcrPid,
    mSegmentationMarkers,
    mAribCaptionsPidControl,
    mKlvDataPids,
    mEbpLookaheadMs,
    mDvbSubPids,
    mScte27Pids,
    mPatInterval,
    mAudioStreamType,
    mEsRateInPes,
    mEtvPlatformPid,
    mBitrate,
    mScte35Control,
    mAudioPids,
    mDvbTeletextPid,
    mEbif,
    mArib,
    mAribCaptionsPid,
    mAbsentInputAudioBehavior,
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
    mssPmtPid,
    mssVideoPid,
    mssNielsenId3Behavior,
    mssScte35Pid,
    mssTransportStreamId,
    mssProgramNum,
    mssTimedMetadataBehavior,
    mssPmtInterval,
    mssEcmPid,
    mssTimedMetadataPid,
    mssAudioFramesPerPes,
    mssPcrPeriod,
    mssPcrPid,
    mssPatInterval,
    mssAudioPids,
    mssScte35Behavior,
    mssPcrControl,

    -- ** MediaConnectFlow
    MediaConnectFlow (..),
    mkMediaConnectFlow,
    mcfFlowARN,

    -- ** MediaConnectFlowRequest
    MediaConnectFlowRequest (..),
    mkMediaConnectFlowRequest,
    mcfrFlowARN,

    -- ** MediaPackageGroupSettings
    MediaPackageGroupSettings (..),
    mkMediaPackageGroupSettings,
    mpgsDestination,

    -- ** MediaPackageOutputDestinationSettings
    MediaPackageOutputDestinationSettings (..),
    mkMediaPackageOutputDestinationSettings,
    mpodsChannelId,

    -- ** MediaPackageOutputSettings
    MediaPackageOutputSettings (..),
    mkMediaPackageOutputSettings,

    -- ** Mp2Settings
    Mp2Settings (..),
    mkMp2Settings,
    msCodingMode,
    msSampleRate,
    msBitrate,

    -- ** Mpeg2FilterSettings
    Mpeg2FilterSettings (..),
    mkMpeg2FilterSettings,
    mfsTemporalFilterSettings,

    -- ** Mpeg2Settings
    Mpeg2Settings (..),
    mkMpeg2Settings,
    msScanType,
    msTimecodeInsertion,
    msAfdSignaling,
    msGopSize,
    msGopSizeUnits,
    msSubgopLength,
    msDisplayAspectRatio,
    msGopNumBFrames,
    msFixedAfd,
    msFilterSettings,
    msColorMetadata,
    msAdaptiveQuantization,
    msGopClosedCadence,
    msColorSpace,
    msFramerateNumerator,
    msFramerateDenominator,

    -- ** MsSmoothGroupSettings
    MsSmoothGroupSettings (..),
    mkMsSmoothGroupSettings,
    msgsFragmentLength,
    msgsStreamManifestBehavior,
    msgsSendDelayMs,
    msgsEventStopBehavior,
    msgsTimestampOffsetMode,
    msgsNumRetries,
    msgsAcquisitionPointId,
    msgsInputLossAction,
    msgsTimestampOffset,
    msgsCertificateMode,
    msgsSparseTrackType,
    msgsConnectionRetryInterval,
    msgsFilecacheDuration,
    msgsRestartDelay,
    msgsEventIdMode,
    msgsAudioOnlyTimecodeControl,
    msgsSegmentationMode,
    msgsEventId,
    msgsDestination,

    -- ** MsSmoothOutputSettings
    MsSmoothOutputSettings (..),
    mkMsSmoothOutputSettings,
    msosH265PackagingType,
    msosNameModifier,

    -- ** Multiplex
    Multiplex (..),
    mkMultiplex,
    mState,
    mARN,
    mPipelinesRunningCount,
    mAvailabilityZones,
    mProgramCount,
    mDestinations,
    mName,
    mId,
    mMultiplexSettings,
    mTags,

    -- ** MultiplexGroupSettings
    MultiplexGroupSettings (..),
    mkMultiplexGroupSettings,

    -- ** MultiplexMediaConnectOutputDestinationSettings
    MultiplexMediaConnectOutputDestinationSettings (..),
    mkMultiplexMediaConnectOutputDestinationSettings,
    mmcodsEntitlementARN,

    -- ** MultiplexOutputDestination
    MultiplexOutputDestination (..),
    mkMultiplexOutputDestination,
    modMediaConnectSettings,

    -- ** MultiplexOutputSettings
    MultiplexOutputSettings (..),
    mkMultiplexOutputSettings,
    mosDestination,

    -- ** MultiplexProgram
    MultiplexProgram (..),
    mkMultiplexProgram,
    mpPacketIdentifiersMap,
    mpPipelineDetails,
    mpProgramName,
    mpChannelId,
    mpMultiplexProgramSettings,

    -- ** MultiplexProgramChannelDestinationSettings
    MultiplexProgramChannelDestinationSettings (..),
    mkMultiplexProgramChannelDestinationSettings,
    mpcdsMultiplexId,
    mpcdsProgramName,

    -- ** MultiplexProgramPacketIdentifiersMap
    MultiplexProgramPacketIdentifiersMap (..),
    mkMultiplexProgramPacketIdentifiersMap,
    mppimPmtPid,
    mppimEtvSignalPid,
    mppimVideoPid,
    mppimScte35Pid,
    mppimPrivateMetadataPid,
    mppimTimedMetadataPid,
    mppimPcrPid,
    mppimKlvDataPids,
    mppimDvbSubPids,
    mppimScte27Pids,
    mppimEtvPlatformPid,
    mppimAudioPids,
    mppimDvbTeletextPid,

    -- ** MultiplexProgramPipelineDetail
    MultiplexProgramPipelineDetail (..),
    mkMultiplexProgramPipelineDetail,
    mppdPipelineId,
    mppdActiveChannelPipeline,

    -- ** MultiplexProgramServiceDescriptor
    MultiplexProgramServiceDescriptor (..),
    mkMultiplexProgramServiceDescriptor,
    mpsdProviderName,
    mpsdServiceName,

    -- ** MultiplexProgramSettings
    MultiplexProgramSettings (..),
    mkMultiplexProgramSettings,
    mpsPreferredChannelPipeline,
    mpsVideoSettings,
    mpsServiceDescriptor,
    mpsProgramNumber,

    -- ** MultiplexProgramSummary
    MultiplexProgramSummary (..),
    mkMultiplexProgramSummary,
    mpsProgramName,
    mpsChannelId,

    -- ** MultiplexSettings
    MultiplexSettings (..),
    mkMultiplexSettings,
    msMaximumVideoBufferDelayMilliseconds,
    msTransportStreamReservedBitrate,
    msTransportStreamBitrate,
    msTransportStreamId,

    -- ** MultiplexSettingsSummary
    MultiplexSettingsSummary (..),
    mkMultiplexSettingsSummary,
    mssTransportStreamBitrate,

    -- ** MultiplexStatmuxVideoSettings
    MultiplexStatmuxVideoSettings (..),
    mkMultiplexStatmuxVideoSettings,
    msvsPriority,
    msvsMinimumBitrate,
    msvsMaximumBitrate,

    -- ** MultiplexSummary
    MultiplexSummary (..),
    mkMultiplexSummary,
    msState,
    msARN,
    msPipelinesRunningCount,
    msAvailabilityZones,
    msProgramCount,
    msName,
    msId,
    msMultiplexSettings,
    msTags,

    -- ** MultiplexVideoSettings
    MultiplexVideoSettings (..),
    mkMultiplexVideoSettings,
    mvsStatmuxSettings,
    mvsConstantBitrate,

    -- ** NetworkInputSettings
    NetworkInputSettings (..),
    mkNetworkInputSettings,
    nisHlsInputSettings,
    nisServerValidation,

    -- ** NielsenConfiguration
    NielsenConfiguration (..),
    mkNielsenConfiguration,
    ncDistributorId,
    ncNielsenPcmToId3Tagging,

    -- ** Offering
    Offering (..),
    mkOffering,
    oResourceSpecification,
    oCurrencyCode,
    oARN,
    oOfferingId,
    oRegion,
    oOfferingType,
    oUsagePrice,
    oFixedPrice,
    oDurationUnits,
    oOfferingDescription,
    oDuration,

    -- ** Output
    Output (..),
    mkOutput,
    oCaptionDescriptionNames,
    oVideoDescriptionName,
    oOutputName,
    oAudioDescriptionNames,
    oOutputSettings,

    -- ** OutputDestination
    OutputDestination (..),
    mkOutputDestination,
    odSettings,
    odMediaPackageSettings,
    odId,
    odMultiplexSettings,

    -- ** OutputDestinationSettings
    OutputDestinationSettings (..),
    mkOutputDestinationSettings,
    odsURL,
    odsUsername,
    odsPasswordParam,
    odsStreamName,

    -- ** OutputGroup
    OutputGroup (..),
    mkOutputGroup,
    ogName,
    ogOutputs,
    ogOutputGroupSettings,

    -- ** OutputGroupSettings
    OutputGroupSettings (..),
    mkOutputGroupSettings,
    ogsMediaPackageGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsRtmpGroupSettings,
    ogsMultiplexGroupSettings,
    ogsHlsGroupSettings,
    ogsArchiveGroupSettings,
    ogsUdpGroupSettings,
    ogsFrameCaptureGroupSettings,

    -- ** OutputLocationRef
    OutputLocationRef (..),
    mkOutputLocationRef,
    olrDestinationRefId,

    -- ** OutputSettings
    OutputSettings (..),
    mkOutputSettings,
    osMultiplexOutputSettings,
    osArchiveOutputSettings,
    osRtmpOutputSettings,
    osMediaPackageOutputSettings,
    osHlsOutputSettings,
    osFrameCaptureOutputSettings,
    osUdpOutputSettings,
    osMsSmoothOutputSettings,

    -- ** PassThroughSettings
    PassThroughSettings (..),
    mkPassThroughSettings,

    -- ** PauseStateScheduleActionSettings
    PauseStateScheduleActionSettings (..),
    mkPauseStateScheduleActionSettings,
    pssasPipelines,

    -- ** PipelineDetail
    PipelineDetail (..),
    mkPipelineDetail,
    pdPipelineId,
    pdActiveInputSwitchActionName,
    pdActiveInputAttachmentName,

    -- ** PipelinePauseStateSettings
    PipelinePauseStateSettings (..),
    mkPipelinePauseStateSettings,
    ppssPipelineId,

    -- ** RawSettings
    RawSettings (..),
    mkRawSettings,

    -- ** Rec601Settings
    Rec601Settings (..),
    mkRec601Settings,

    -- ** Rec709Settings
    Rec709Settings (..),
    mkRec709Settings,

    -- ** RemixSettings
    RemixSettings (..),
    mkRemixSettings,
    rsChannelsIn,
    rsChannelsOut,
    rsChannelMappings,

    -- ** Reservation
    Reservation (..),
    mkReservation,
    rState,
    rResourceSpecification,
    rCurrencyCode,
    rARN,
    rStart,
    rCount,
    rEnd,
    rName,
    rReservationId,
    rOfferingId,
    rRegion,
    rOfferingType,
    rUsagePrice,
    rFixedPrice,
    rDurationUnits,
    rOfferingDescription,
    rDuration,
    rTags,

    -- ** ReservationResourceSpecification
    ReservationResourceSpecification (..),
    mkReservationResourceSpecification,
    rrsVideoQuality,
    rrsMaximumFramerate,
    rrsResourceType,
    rrsResolution,
    rrsCodec,
    rrsSpecialFeature,
    rrsChannelClass,
    rrsMaximumBitrate,

    -- ** RtmpCaptionInfoDestinationSettings
    RtmpCaptionInfoDestinationSettings (..),
    mkRtmpCaptionInfoDestinationSettings,

    -- ** RtmpGroupSettings
    RtmpGroupSettings (..),
    mkRtmpGroupSettings,
    rgsInputLossAction,
    rgsCaptionData,
    rgsAdMarkers,
    rgsRestartDelay,
    rgsAuthenticationScheme,
    rgsCacheLength,
    rgsCacheFullBehavior,

    -- ** RtmpOutputSettings
    RtmpOutputSettings (..),
    mkRtmpOutputSettings,
    rosNumRetries,
    rosCertificateMode,
    rosConnectionRetryInterval,
    rosDestination,

    -- ** ScheduleAction
    ScheduleAction (..),
    mkScheduleAction,
    saActionName,
    saScheduleActionStartSettings,
    saScheduleActionSettings,

    -- ** ScheduleActionSettings
    ScheduleActionSettings (..),
    mkScheduleActionSettings,
    sasStaticImageDeactivateSettings,
    sasScte35SpliceInsertSettings,
    sasStaticImageActivateSettings,
    sasScte35TimeSignalSettings,
    sasInputPrepareSettings,
    sasHlsId3SegmentTaggingSettings,
    sasScte35ReturnToNetworkSettings,
    sasPauseStateSettings,
    sasHlsTimedMetadataSettings,
    sasInputSwitchSettings,

    -- ** ScheduleActionStartSettings
    ScheduleActionStartSettings (..),
    mkScheduleActionStartSettings,
    sassImmediateModeScheduleActionStartSettings,
    sassFollowModeScheduleActionStartSettings,
    sassFixedModeScheduleActionStartSettings,

    -- ** Scte20PlusEmbeddedDestinationSettings
    Scte20PlusEmbeddedDestinationSettings (..),
    mkScte20PlusEmbeddedDestinationSettings,

    -- ** Scte20SourceSettings
    Scte20SourceSettings (..),
    mkScte20SourceSettings,
    sssConvert608To708,
    sssSource608ChannelNumber,

    -- ** Scte27DestinationSettings
    Scte27DestinationSettings (..),
    mkScte27DestinationSettings,

    -- ** Scte27SourceSettings
    Scte27SourceSettings (..),
    mkScte27SourceSettings,
    sssPid,

    -- ** Scte35DeliveryRestrictions
    Scte35DeliveryRestrictions (..),
    mkScte35DeliveryRestrictions,
    sdrDeviceRestrictions,
    sdrArchiveAllowedFlag,
    sdrWebDeliveryAllowedFlag,
    sdrNoRegionalBlackoutFlag,

    -- ** Scte35Descriptor
    Scte35Descriptor (..),
    mkScte35Descriptor,
    sdScte35DescriptorSettings,

    -- ** Scte35DescriptorSettings
    Scte35DescriptorSettings (..),
    mkScte35DescriptorSettings,
    sdsSegmentationDescriptorScte35DescriptorSettings,

    -- ** Scte35ReturnToNetworkScheduleActionSettings
    Scte35ReturnToNetworkScheduleActionSettings (..),
    mkScte35ReturnToNetworkScheduleActionSettings,
    srtnsasSpliceEventId,

    -- ** Scte35SegmentationDescriptor
    Scte35SegmentationDescriptor (..),
    mkScte35SegmentationDescriptor,
    ssdSegmentationUpidType,
    ssdSegmentsExpected,
    ssdSubSegmentsExpected,
    ssdSegmentNum,
    ssdSegmentationDuration,
    ssdSegmentationTypeId,
    ssdDeliveryRestrictions,
    ssdSegmentationUpid,
    ssdSubSegmentNum,
    ssdSegmentationEventId,
    ssdSegmentationCancelIndicator,

    -- ** Scte35SpliceInsert
    Scte35SpliceInsert (..),
    mkScte35SpliceInsert,
    ssiWebDeliveryAllowedFlag,
    ssiAdAvailOffset,
    ssiNoRegionalBlackoutFlag,

    -- ** Scte35SpliceInsertScheduleActionSettings
    Scte35SpliceInsertScheduleActionSettings (..),
    mkScte35SpliceInsertScheduleActionSettings,
    ssisasDuration,
    ssisasSpliceEventId,

    -- ** Scte35TimeSignalApos
    Scte35TimeSignalApos (..),
    mkScte35TimeSignalApos,
    stsaWebDeliveryAllowedFlag,
    stsaAdAvailOffset,
    stsaNoRegionalBlackoutFlag,

    -- ** Scte35TimeSignalScheduleActionSettings
    Scte35TimeSignalScheduleActionSettings (..),
    mkScte35TimeSignalScheduleActionSettings,
    stssasScte35Descriptors,

    -- ** SmpteTtDestinationSettings
    SmpteTtDestinationSettings (..),
    mkSmpteTtDestinationSettings,

    -- ** StandardHlsSettings
    StandardHlsSettings (..),
    mkStandardHlsSettings,
    shsAudioRenditionSets,
    shsM3u8Settings,

    -- ** StartTimecode
    StartTimecode (..),
    mkStartTimecode,
    sTimecode,

    -- ** StaticImageActivateScheduleActionSettings
    StaticImageActivateScheduleActionSettings (..),
    mkStaticImageActivateScheduleActionSettings,
    siasasImageX,
    siasasHeight,
    siasasFadeOut,
    siasasWidth,
    siasasOpacity,
    siasasLayer,
    siasasDuration,
    siasasImageY,
    siasasFadeIn,
    siasasImage,

    -- ** StaticImageDeactivateScheduleActionSettings
    StaticImageDeactivateScheduleActionSettings (..),
    mkStaticImageDeactivateScheduleActionSettings,
    sidsasFadeOut,
    sidsasLayer,

    -- ** StaticKeySettings
    StaticKeySettings (..),
    mkStaticKeySettings,
    sksKeyProviderServer,
    sksStaticKeyValue,

    -- ** StopTimecode
    StopTimecode (..),
    mkStopTimecode,
    stLastFrameClippingBehavior,
    stTimecode,

    -- ** TeletextDestinationSettings
    TeletextDestinationSettings (..),
    mkTeletextDestinationSettings,

    -- ** TeletextSourceSettings
    TeletextSourceSettings (..),
    mkTeletextSourceSettings,
    tssPageNumber,

    -- ** TemporalFilterSettings
    TemporalFilterSettings (..),
    mkTemporalFilterSettings,
    tfsStrength,
    tfsPostFilterSharpening,

    -- ** TimecodeConfig
    TimecodeConfig (..),
    mkTimecodeConfig,
    tcSyncThreshold,
    tcSource,

    -- ** TransferringInputDeviceSummary
    TransferringInputDeviceSummary (..),
    mkTransferringInputDeviceSummary,
    tidsTransferType,
    tidsId,
    tidsTargetCustomerId,
    tidsMessage,

    -- ** TtmlDestinationSettings
    TtmlDestinationSettings (..),
    mkTtmlDestinationSettings,
    tdsStyleControl,

    -- ** UdpContainerSettings
    UdpContainerSettings (..),
    mkUdpContainerSettings,
    ucsM2tsSettings,

    -- ** UdpGroupSettings
    UdpGroupSettings (..),
    mkUdpGroupSettings,
    ugsTimedMetadataId3Period,
    ugsInputLossAction,
    ugsTimedMetadataId3Frame,

    -- ** UdpOutputSettings
    UdpOutputSettings (..),
    mkUdpOutputSettings,
    uosFecOutputSettings,
    uosBufferMsec,
    uosDestination,
    uosContainerSettings,

    -- ** VideoCodecSettings
    VideoCodecSettings (..),
    mkVideoCodecSettings,
    vcsFrameCaptureSettings,
    vcsH265Settings,
    vcsH264Settings,
    vcsMpeg2Settings,

    -- ** VideoDescription
    VideoDescription (..),
    mkVideoDescription,
    vdHeight,
    vdSharpness,
    vdWidth,
    vdScalingBehavior,
    vdRespondToAfd,
    vdCodecSettings,
    vdName,

    -- ** VideoSelector
    VideoSelector (..),
    mkVideoSelector,
    vsSelectorSettings,
    vsColorSpaceUsage,
    vsColorSpace,

    -- ** VideoSelectorPid
    VideoSelectorPid (..),
    mkVideoSelectorPid,
    vspPid,

    -- ** VideoSelectorProgramId
    VideoSelectorProgramId (..),
    mkVideoSelectorProgramId,
    vspiProgramId,

    -- ** VideoSelectorSettings
    VideoSelectorSettings (..),
    mkVideoSelectorSettings,
    vssVideoSelectorProgramId,
    vssVideoSelectorPid,

    -- ** WavSettings
    WavSettings (..),
    mkWavSettings,
    wsBitDepth,
    wsCodingMode,
    wsSampleRate,

    -- ** WebvttDestinationSettings
    WebvttDestinationSettings (..),
    mkWebvttDestinationSettings,

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
import qualified Network.AWS.Prelude as Lude

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
