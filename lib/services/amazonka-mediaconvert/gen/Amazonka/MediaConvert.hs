{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaConvert
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-08-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elemental MediaConvert
module Amazonka.MediaConvert
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateCertificate
    AssociateCertificate (AssociateCertificate'),
    newAssociateCertificate,
    AssociateCertificateResponse (AssociateCertificateResponse'),
    newAssociateCertificateResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** CreateJobTemplate
    CreateJobTemplate (CreateJobTemplate'),
    newCreateJobTemplate,
    CreateJobTemplateResponse (CreateJobTemplateResponse'),
    newCreateJobTemplateResponse,

    -- ** CreatePreset
    CreatePreset (CreatePreset'),
    newCreatePreset,
    CreatePresetResponse (CreatePresetResponse'),
    newCreatePresetResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** DeleteJobTemplate
    DeleteJobTemplate (DeleteJobTemplate'),
    newDeleteJobTemplate,
    DeleteJobTemplateResponse (DeleteJobTemplateResponse'),
    newDeleteJobTemplateResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeletePreset
    DeletePreset (DeletePreset'),
    newDeletePreset,
    DeletePresetResponse (DeletePresetResponse'),
    newDeletePresetResponse,

    -- ** DeleteQueue
    DeleteQueue (DeleteQueue'),
    newDeleteQueue,
    DeleteQueueResponse (DeleteQueueResponse'),
    newDeleteQueueResponse,

    -- ** DescribeEndpoints (Paginated)
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** DisassociateCertificate
    DisassociateCertificate (DisassociateCertificate'),
    newDisassociateCertificate,
    DisassociateCertificateResponse (DisassociateCertificateResponse'),
    newDisassociateCertificateResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetJobTemplate
    GetJobTemplate (GetJobTemplate'),
    newGetJobTemplate,
    GetJobTemplateResponse (GetJobTemplateResponse'),
    newGetJobTemplateResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** GetPreset
    GetPreset (GetPreset'),
    newGetPreset,
    GetPresetResponse (GetPresetResponse'),
    newGetPresetResponse,

    -- ** GetQueue
    GetQueue (GetQueue'),
    newGetQueue,
    GetQueueResponse (GetQueueResponse'),
    newGetQueueResponse,

    -- ** ListJobTemplates (Paginated)
    ListJobTemplates (ListJobTemplates'),
    newListJobTemplates,
    ListJobTemplatesResponse (ListJobTemplatesResponse'),
    newListJobTemplatesResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListPresets (Paginated)
    ListPresets (ListPresets'),
    newListPresets,
    ListPresetsResponse (ListPresetsResponse'),
    newListPresetsResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateJobTemplate
    UpdateJobTemplate (UpdateJobTemplate'),
    newUpdateJobTemplate,
    UpdateJobTemplateResponse (UpdateJobTemplateResponse'),
    newUpdateJobTemplateResponse,

    -- ** UpdatePreset
    UpdatePreset (UpdatePreset'),
    newUpdatePreset,
    UpdatePresetResponse (UpdatePresetResponse'),
    newUpdatePresetResponse,

    -- ** UpdateQueue
    UpdateQueue (UpdateQueue'),
    newUpdateQueue,
    UpdateQueueResponse (UpdateQueueResponse'),
    newUpdateQueueResponse,

    -- * Types

    -- ** AacAudioDescriptionBroadcasterMix
    AacAudioDescriptionBroadcasterMix (..),

    -- ** AacCodecProfile
    AacCodecProfile (..),

    -- ** AacCodingMode
    AacCodingMode (..),

    -- ** AacRateControlMode
    AacRateControlMode (..),

    -- ** AacRawFormat
    AacRawFormat (..),

    -- ** AacSpecification
    AacSpecification (..),

    -- ** AacVbrQuality
    AacVbrQuality (..),

    -- ** Ac3BitstreamMode
    Ac3BitstreamMode (..),

    -- ** Ac3CodingMode
    Ac3CodingMode (..),

    -- ** Ac3DynamicRangeCompressionLine
    Ac3DynamicRangeCompressionLine (..),

    -- ** Ac3DynamicRangeCompressionProfile
    Ac3DynamicRangeCompressionProfile (..),

    -- ** Ac3DynamicRangeCompressionRf
    Ac3DynamicRangeCompressionRf (..),

    -- ** Ac3LfeFilter
    Ac3LfeFilter (..),

    -- ** Ac3MetadataControl
    Ac3MetadataControl (..),

    -- ** AccelerationMode
    AccelerationMode (..),

    -- ** AccelerationStatus
    AccelerationStatus (..),

    -- ** AfdSignaling
    AfdSignaling (..),

    -- ** AlphaBehavior
    AlphaBehavior (..),

    -- ** AncillaryConvert608To708
    AncillaryConvert608To708 (..),

    -- ** AncillaryTerminateCaptions
    AncillaryTerminateCaptions (..),

    -- ** AntiAlias
    AntiAlias (..),

    -- ** AudioChannelTag
    AudioChannelTag (..),

    -- ** AudioCodec
    AudioCodec (..),

    -- ** AudioDefaultSelection
    AudioDefaultSelection (..),

    -- ** AudioDurationCorrection
    AudioDurationCorrection (..),

    -- ** AudioLanguageCodeControl
    AudioLanguageCodeControl (..),

    -- ** AudioNormalizationAlgorithm
    AudioNormalizationAlgorithm (..),

    -- ** AudioNormalizationAlgorithmControl
    AudioNormalizationAlgorithmControl (..),

    -- ** AudioNormalizationLoudnessLogging
    AudioNormalizationLoudnessLogging (..),

    -- ** AudioNormalizationPeakCalculation
    AudioNormalizationPeakCalculation (..),

    -- ** AudioSelectorType
    AudioSelectorType (..),

    -- ** AudioTypeControl
    AudioTypeControl (..),

    -- ** Av1AdaptiveQuantization
    Av1AdaptiveQuantization (..),

    -- ** Av1BitDepth
    Av1BitDepth (..),

    -- ** Av1FramerateControl
    Av1FramerateControl (..),

    -- ** Av1FramerateConversionAlgorithm
    Av1FramerateConversionAlgorithm (..),

    -- ** Av1RateControlMode
    Av1RateControlMode (..),

    -- ** Av1SpatialAdaptiveQuantization
    Av1SpatialAdaptiveQuantization (..),

    -- ** AvcIntraClass
    AvcIntraClass (..),

    -- ** AvcIntraFramerateControl
    AvcIntraFramerateControl (..),

    -- ** AvcIntraFramerateConversionAlgorithm
    AvcIntraFramerateConversionAlgorithm (..),

    -- ** AvcIntraInterlaceMode
    AvcIntraInterlaceMode (..),

    -- ** AvcIntraScanTypeConversionMode
    AvcIntraScanTypeConversionMode (..),

    -- ** AvcIntraSlowPal
    AvcIntraSlowPal (..),

    -- ** AvcIntraTelecine
    AvcIntraTelecine (..),

    -- ** AvcIntraUhdQualityTuningLevel
    AvcIntraUhdQualityTuningLevel (..),

    -- ** BillingTagsSource
    BillingTagsSource (..),

    -- ** BurnInSubtitleStylePassthrough
    BurnInSubtitleStylePassthrough (..),

    -- ** BurninSubtitleAlignment
    BurninSubtitleAlignment (..),

    -- ** BurninSubtitleApplyFontColor
    BurninSubtitleApplyFontColor (..),

    -- ** BurninSubtitleBackgroundColor
    BurninSubtitleBackgroundColor (..),

    -- ** BurninSubtitleFallbackFont
    BurninSubtitleFallbackFont (..),

    -- ** BurninSubtitleFontColor
    BurninSubtitleFontColor (..),

    -- ** BurninSubtitleOutlineColor
    BurninSubtitleOutlineColor (..),

    -- ** BurninSubtitleShadowColor
    BurninSubtitleShadowColor (..),

    -- ** BurninSubtitleTeletextSpacing
    BurninSubtitleTeletextSpacing (..),

    -- ** CaptionDestinationType
    CaptionDestinationType (..),

    -- ** CaptionSourceType
    CaptionSourceType (..),

    -- ** CmafClientCache
    CmafClientCache (..),

    -- ** CmafCodecSpecification
    CmafCodecSpecification (..),

    -- ** CmafEncryptionType
    CmafEncryptionType (..),

    -- ** CmafImageBasedTrickPlay
    CmafImageBasedTrickPlay (..),

    -- ** CmafInitializationVectorInManifest
    CmafInitializationVectorInManifest (..),

    -- ** CmafIntervalCadence
    CmafIntervalCadence (..),

    -- ** CmafKeyProviderType
    CmafKeyProviderType (..),

    -- ** CmafManifestCompression
    CmafManifestCompression (..),

    -- ** CmafManifestDurationFormat
    CmafManifestDurationFormat (..),

    -- ** CmafMpdManifestBandwidthType
    CmafMpdManifestBandwidthType (..),

    -- ** CmafMpdProfile
    CmafMpdProfile (..),

    -- ** CmafPtsOffsetHandlingForBFrames
    CmafPtsOffsetHandlingForBFrames (..),

    -- ** CmafSegmentControl
    CmafSegmentControl (..),

    -- ** CmafSegmentLengthControl
    CmafSegmentLengthControl (..),

    -- ** CmafStreamInfResolution
    CmafStreamInfResolution (..),

    -- ** CmafTargetDurationCompatibilityMode
    CmafTargetDurationCompatibilityMode (..),

    -- ** CmafVideoCompositionOffsets
    CmafVideoCompositionOffsets (..),

    -- ** CmafWriteDASHManifest
    CmafWriteDASHManifest (..),

    -- ** CmafWriteHLSManifest
    CmafWriteHLSManifest (..),

    -- ** CmafWriteSegmentTimelineInRepresentation
    CmafWriteSegmentTimelineInRepresentation (..),

    -- ** CmfcAudioDuration
    CmfcAudioDuration (..),

    -- ** CmfcAudioTrackType
    CmfcAudioTrackType (..),

    -- ** CmfcDescriptiveVideoServiceFlag
    CmfcDescriptiveVideoServiceFlag (..),

    -- ** CmfcIFrameOnlyManifest
    CmfcIFrameOnlyManifest (..),

    -- ** CmfcKlvMetadata
    CmfcKlvMetadata (..),

    -- ** CmfcManifestMetadataSignaling
    CmfcManifestMetadataSignaling (..),

    -- ** CmfcScte35Esam
    CmfcScte35Esam (..),

    -- ** CmfcScte35Source
    CmfcScte35Source (..),

    -- ** CmfcTimedMetadata
    CmfcTimedMetadata (..),

    -- ** CmfcTimedMetadataBoxVersion
    CmfcTimedMetadataBoxVersion (..),

    -- ** ColorMetadata
    ColorMetadata (..),

    -- ** ColorSpace
    ColorSpace (..),

    -- ** ColorSpaceConversion
    ColorSpaceConversion (..),

    -- ** ColorSpaceUsage
    ColorSpaceUsage (..),

    -- ** Commitment
    Commitment (..),

    -- ** ContainerType
    ContainerType (..),

    -- ** CopyProtectionAction
    CopyProtectionAction (..),

    -- ** DashIsoGroupAudioChannelConfigSchemeIdUri
    DashIsoGroupAudioChannelConfigSchemeIdUri (..),

    -- ** DashIsoHbbtvCompliance
    DashIsoHbbtvCompliance (..),

    -- ** DashIsoImageBasedTrickPlay
    DashIsoImageBasedTrickPlay (..),

    -- ** DashIsoIntervalCadence
    DashIsoIntervalCadence (..),

    -- ** DashIsoMpdManifestBandwidthType
    DashIsoMpdManifestBandwidthType (..),

    -- ** DashIsoMpdProfile
    DashIsoMpdProfile (..),

    -- ** DashIsoPlaybackDeviceCompatibility
    DashIsoPlaybackDeviceCompatibility (..),

    -- ** DashIsoPtsOffsetHandlingForBFrames
    DashIsoPtsOffsetHandlingForBFrames (..),

    -- ** DashIsoSegmentControl
    DashIsoSegmentControl (..),

    -- ** DashIsoSegmentLengthControl
    DashIsoSegmentLengthControl (..),

    -- ** DashIsoVideoCompositionOffsets
    DashIsoVideoCompositionOffsets (..),

    -- ** DashIsoWriteSegmentTimelineInRepresentation
    DashIsoWriteSegmentTimelineInRepresentation (..),

    -- ** DecryptionMode
    DecryptionMode (..),

    -- ** DeinterlaceAlgorithm
    DeinterlaceAlgorithm (..),

    -- ** DeinterlacerControl
    DeinterlacerControl (..),

    -- ** DeinterlacerMode
    DeinterlacerMode (..),

    -- ** DescribeEndpointsMode
    DescribeEndpointsMode (..),

    -- ** DolbyVisionLevel6Mode
    DolbyVisionLevel6Mode (..),

    -- ** DolbyVisionMapping
    DolbyVisionMapping (..),

    -- ** DolbyVisionProfile
    DolbyVisionProfile (..),

    -- ** DropFrameTimecode
    DropFrameTimecode (..),

    -- ** DvbSubSubtitleFallbackFont
    DvbSubSubtitleFallbackFont (..),

    -- ** DvbSubtitleAlignment
    DvbSubtitleAlignment (..),

    -- ** DvbSubtitleApplyFontColor
    DvbSubtitleApplyFontColor (..),

    -- ** DvbSubtitleBackgroundColor
    DvbSubtitleBackgroundColor (..),

    -- ** DvbSubtitleFontColor
    DvbSubtitleFontColor (..),

    -- ** DvbSubtitleOutlineColor
    DvbSubtitleOutlineColor (..),

    -- ** DvbSubtitleShadowColor
    DvbSubtitleShadowColor (..),

    -- ** DvbSubtitleStylePassthrough
    DvbSubtitleStylePassthrough (..),

    -- ** DvbSubtitleTeletextSpacing
    DvbSubtitleTeletextSpacing (..),

    -- ** DvbSubtitlingType
    DvbSubtitlingType (..),

    -- ** DvbddsHandling
    DvbddsHandling (..),

    -- ** Eac3AtmosBitstreamMode
    Eac3AtmosBitstreamMode (..),

    -- ** Eac3AtmosCodingMode
    Eac3AtmosCodingMode (..),

    -- ** Eac3AtmosDialogueIntelligence
    Eac3AtmosDialogueIntelligence (..),

    -- ** Eac3AtmosDownmixControl
    Eac3AtmosDownmixControl (..),

    -- ** Eac3AtmosDynamicRangeCompressionLine
    Eac3AtmosDynamicRangeCompressionLine (..),

    -- ** Eac3AtmosDynamicRangeCompressionRf
    Eac3AtmosDynamicRangeCompressionRf (..),

    -- ** Eac3AtmosDynamicRangeControl
    Eac3AtmosDynamicRangeControl (..),

    -- ** Eac3AtmosMeteringMode
    Eac3AtmosMeteringMode (..),

    -- ** Eac3AtmosStereoDownmix
    Eac3AtmosStereoDownmix (..),

    -- ** Eac3AtmosSurroundExMode
    Eac3AtmosSurroundExMode (..),

    -- ** Eac3AttenuationControl
    Eac3AttenuationControl (..),

    -- ** Eac3BitstreamMode
    Eac3BitstreamMode (..),

    -- ** Eac3CodingMode
    Eac3CodingMode (..),

    -- ** Eac3DcFilter
    Eac3DcFilter (..),

    -- ** Eac3DynamicRangeCompressionLine
    Eac3DynamicRangeCompressionLine (..),

    -- ** Eac3DynamicRangeCompressionRf
    Eac3DynamicRangeCompressionRf (..),

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

    -- ** EmbeddedConvert608To708
    EmbeddedConvert608To708 (..),

    -- ** EmbeddedTerminateCaptions
    EmbeddedTerminateCaptions (..),

    -- ** EmbeddedTimecodeOverride
    EmbeddedTimecodeOverride (..),

    -- ** F4vMoovPlacement
    F4vMoovPlacement (..),

    -- ** FileSourceConvert608To708
    FileSourceConvert608To708 (..),

    -- ** FileSourceTimeDeltaUnits
    FileSourceTimeDeltaUnits (..),

    -- ** FontScript
    FontScript (..),

    -- ** H264AdaptiveQuantization
    H264AdaptiveQuantization (..),

    -- ** H264CodecLevel
    H264CodecLevel (..),

    -- ** H264CodecProfile
    H264CodecProfile (..),

    -- ** H264DynamicSubGop
    H264DynamicSubGop (..),

    -- ** H264EntropyEncoding
    H264EntropyEncoding (..),

    -- ** H264FieldEncoding
    H264FieldEncoding (..),

    -- ** H264FlickerAdaptiveQuantization
    H264FlickerAdaptiveQuantization (..),

    -- ** H264FramerateControl
    H264FramerateControl (..),

    -- ** H264FramerateConversionAlgorithm
    H264FramerateConversionAlgorithm (..),

    -- ** H264GopBReference
    H264GopBReference (..),

    -- ** H264GopSizeUnits
    H264GopSizeUnits (..),

    -- ** H264InterlaceMode
    H264InterlaceMode (..),

    -- ** H264ParControl
    H264ParControl (..),

    -- ** H264QualityTuningLevel
    H264QualityTuningLevel (..),

    -- ** H264RateControlMode
    H264RateControlMode (..),

    -- ** H264RepeatPps
    H264RepeatPps (..),

    -- ** H264ScanTypeConversionMode
    H264ScanTypeConversionMode (..),

    -- ** H264SceneChangeDetect
    H264SceneChangeDetect (..),

    -- ** H264SlowPal
    H264SlowPal (..),

    -- ** H264SpatialAdaptiveQuantization
    H264SpatialAdaptiveQuantization (..),

    -- ** H264Syntax
    H264Syntax (..),

    -- ** H264Telecine
    H264Telecine (..),

    -- ** H264TemporalAdaptiveQuantization
    H264TemporalAdaptiveQuantization (..),

    -- ** H264UnregisteredSeiTimecode
    H264UnregisteredSeiTimecode (..),

    -- ** H265AdaptiveQuantization
    H265AdaptiveQuantization (..),

    -- ** H265AlternateTransferFunctionSei
    H265AlternateTransferFunctionSei (..),

    -- ** H265CodecLevel
    H265CodecLevel (..),

    -- ** H265CodecProfile
    H265CodecProfile (..),

    -- ** H265DynamicSubGop
    H265DynamicSubGop (..),

    -- ** H265FlickerAdaptiveQuantization
    H265FlickerAdaptiveQuantization (..),

    -- ** H265FramerateControl
    H265FramerateControl (..),

    -- ** H265FramerateConversionAlgorithm
    H265FramerateConversionAlgorithm (..),

    -- ** H265GopBReference
    H265GopBReference (..),

    -- ** H265GopSizeUnits
    H265GopSizeUnits (..),

    -- ** H265InterlaceMode
    H265InterlaceMode (..),

    -- ** H265ParControl
    H265ParControl (..),

    -- ** H265QualityTuningLevel
    H265QualityTuningLevel (..),

    -- ** H265RateControlMode
    H265RateControlMode (..),

    -- ** H265SampleAdaptiveOffsetFilterMode
    H265SampleAdaptiveOffsetFilterMode (..),

    -- ** H265ScanTypeConversionMode
    H265ScanTypeConversionMode (..),

    -- ** H265SceneChangeDetect
    H265SceneChangeDetect (..),

    -- ** H265SlowPal
    H265SlowPal (..),

    -- ** H265SpatialAdaptiveQuantization
    H265SpatialAdaptiveQuantization (..),

    -- ** H265Telecine
    H265Telecine (..),

    -- ** H265TemporalAdaptiveQuantization
    H265TemporalAdaptiveQuantization (..),

    -- ** H265TemporalIds
    H265TemporalIds (..),

    -- ** H265Tiles
    H265Tiles (..),

    -- ** H265UnregisteredSeiTimecode
    H265UnregisteredSeiTimecode (..),

    -- ** H265WriteMp4PackagingType
    H265WriteMp4PackagingType (..),

    -- ** HlsAdMarkers
    HlsAdMarkers (..),

    -- ** HlsAudioOnlyContainer
    HlsAudioOnlyContainer (..),

    -- ** HlsAudioOnlyHeader
    HlsAudioOnlyHeader (..),

    -- ** HlsAudioTrackType
    HlsAudioTrackType (..),

    -- ** HlsCaptionLanguageSetting
    HlsCaptionLanguageSetting (..),

    -- ** HlsCaptionSegmentLengthControl
    HlsCaptionSegmentLengthControl (..),

    -- ** HlsClientCache
    HlsClientCache (..),

    -- ** HlsCodecSpecification
    HlsCodecSpecification (..),

    -- ** HlsDescriptiveVideoServiceFlag
    HlsDescriptiveVideoServiceFlag (..),

    -- ** HlsDirectoryStructure
    HlsDirectoryStructure (..),

    -- ** HlsEncryptionType
    HlsEncryptionType (..),

    -- ** HlsIFrameOnlyManifest
    HlsIFrameOnlyManifest (..),

    -- ** HlsImageBasedTrickPlay
    HlsImageBasedTrickPlay (..),

    -- ** HlsInitializationVectorInManifest
    HlsInitializationVectorInManifest (..),

    -- ** HlsIntervalCadence
    HlsIntervalCadence (..),

    -- ** HlsKeyProviderType
    HlsKeyProviderType (..),

    -- ** HlsManifestCompression
    HlsManifestCompression (..),

    -- ** HlsManifestDurationFormat
    HlsManifestDurationFormat (..),

    -- ** HlsOfflineEncrypted
    HlsOfflineEncrypted (..),

    -- ** HlsOutputSelection
    HlsOutputSelection (..),

    -- ** HlsProgramDateTime
    HlsProgramDateTime (..),

    -- ** HlsSegmentControl
    HlsSegmentControl (..),

    -- ** HlsSegmentLengthControl
    HlsSegmentLengthControl (..),

    -- ** HlsStreamInfResolution
    HlsStreamInfResolution (..),

    -- ** HlsTargetDurationCompatibilityMode
    HlsTargetDurationCompatibilityMode (..),

    -- ** HlsTimedMetadataId3Frame
    HlsTimedMetadataId3Frame (..),

    -- ** ImscAccessibilitySubs
    ImscAccessibilitySubs (..),

    -- ** ImscStylePassthrough
    ImscStylePassthrough (..),

    -- ** InputDeblockFilter
    InputDeblockFilter (..),

    -- ** InputDenoiseFilter
    InputDenoiseFilter (..),

    -- ** InputFilterEnable
    InputFilterEnable (..),

    -- ** InputPolicy
    InputPolicy (..),

    -- ** InputPsiControl
    InputPsiControl (..),

    -- ** InputRotate
    InputRotate (..),

    -- ** InputSampleRange
    InputSampleRange (..),

    -- ** InputScanType
    InputScanType (..),

    -- ** InputTimecodeSource
    InputTimecodeSource (..),

    -- ** JobPhase
    JobPhase (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobTemplateListBy
    JobTemplateListBy (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** M2tsAudioBufferModel
    M2tsAudioBufferModel (..),

    -- ** M2tsAudioDuration
    M2tsAudioDuration (..),

    -- ** M2tsBufferModel
    M2tsBufferModel (..),

    -- ** M2tsDataPtsControl
    M2tsDataPtsControl (..),

    -- ** M2tsEbpAudioInterval
    M2tsEbpAudioInterval (..),

    -- ** M2tsEbpPlacement
    M2tsEbpPlacement (..),

    -- ** M2tsEsRateInPes
    M2tsEsRateInPes (..),

    -- ** M2tsForceTsVideoEbpOrder
    M2tsForceTsVideoEbpOrder (..),

    -- ** M2tsKlvMetadata
    M2tsKlvMetadata (..),

    -- ** M2tsNielsenId3
    M2tsNielsenId3 (..),

    -- ** M2tsPcrControl
    M2tsPcrControl (..),

    -- ** M2tsRateMode
    M2tsRateMode (..),

    -- ** M2tsScte35Source
    M2tsScte35Source (..),

    -- ** M2tsSegmentationMarkers
    M2tsSegmentationMarkers (..),

    -- ** M2tsSegmentationStyle
    M2tsSegmentationStyle (..),

    -- ** M3u8AudioDuration
    M3u8AudioDuration (..),

    -- ** M3u8DataPtsControl
    M3u8DataPtsControl (..),

    -- ** M3u8NielsenId3
    M3u8NielsenId3 (..),

    -- ** M3u8PcrControl
    M3u8PcrControl (..),

    -- ** M3u8Scte35Source
    M3u8Scte35Source (..),

    -- ** MotionImageInsertionMode
    MotionImageInsertionMode (..),

    -- ** MotionImagePlayback
    MotionImagePlayback (..),

    -- ** MovClapAtom
    MovClapAtom (..),

    -- ** MovCslgAtom
    MovCslgAtom (..),

    -- ** MovMpeg2FourCCControl
    MovMpeg2FourCCControl (..),

    -- ** MovPaddingControl
    MovPaddingControl (..),

    -- ** MovReference
    MovReference (..),

    -- ** Mp3RateControlMode
    Mp3RateControlMode (..),

    -- ** Mp4CslgAtom
    Mp4CslgAtom (..),

    -- ** Mp4FreeSpaceBox
    Mp4FreeSpaceBox (..),

    -- ** Mp4MoovPlacement
    Mp4MoovPlacement (..),

    -- ** MpdAccessibilityCaptionHints
    MpdAccessibilityCaptionHints (..),

    -- ** MpdAudioDuration
    MpdAudioDuration (..),

    -- ** MpdCaptionContainerType
    MpdCaptionContainerType (..),

    -- ** MpdKlvMetadata
    MpdKlvMetadata (..),

    -- ** MpdManifestMetadataSignaling
    MpdManifestMetadataSignaling (..),

    -- ** MpdScte35Esam
    MpdScte35Esam (..),

    -- ** MpdScte35Source
    MpdScte35Source (..),

    -- ** MpdTimedMetadata
    MpdTimedMetadata (..),

    -- ** MpdTimedMetadataBoxVersion
    MpdTimedMetadataBoxVersion (..),

    -- ** Mpeg2AdaptiveQuantization
    Mpeg2AdaptiveQuantization (..),

    -- ** Mpeg2CodecLevel
    Mpeg2CodecLevel (..),

    -- ** Mpeg2CodecProfile
    Mpeg2CodecProfile (..),

    -- ** Mpeg2DynamicSubGop
    Mpeg2DynamicSubGop (..),

    -- ** Mpeg2FramerateControl
    Mpeg2FramerateControl (..),

    -- ** Mpeg2FramerateConversionAlgorithm
    Mpeg2FramerateConversionAlgorithm (..),

    -- ** Mpeg2GopSizeUnits
    Mpeg2GopSizeUnits (..),

    -- ** Mpeg2InterlaceMode
    Mpeg2InterlaceMode (..),

    -- ** Mpeg2IntraDcPrecision
    Mpeg2IntraDcPrecision (..),

    -- ** Mpeg2ParControl
    Mpeg2ParControl (..),

    -- ** Mpeg2QualityTuningLevel
    Mpeg2QualityTuningLevel (..),

    -- ** Mpeg2RateControlMode
    Mpeg2RateControlMode (..),

    -- ** Mpeg2ScanTypeConversionMode
    Mpeg2ScanTypeConversionMode (..),

    -- ** Mpeg2SceneChangeDetect
    Mpeg2SceneChangeDetect (..),

    -- ** Mpeg2SlowPal
    Mpeg2SlowPal (..),

    -- ** Mpeg2SpatialAdaptiveQuantization
    Mpeg2SpatialAdaptiveQuantization (..),

    -- ** Mpeg2Syntax
    Mpeg2Syntax (..),

    -- ** Mpeg2Telecine
    Mpeg2Telecine (..),

    -- ** Mpeg2TemporalAdaptiveQuantization
    Mpeg2TemporalAdaptiveQuantization (..),

    -- ** MsSmoothAudioDeduplication
    MsSmoothAudioDeduplication (..),

    -- ** MsSmoothFragmentLengthControl
    MsSmoothFragmentLengthControl (..),

    -- ** MsSmoothManifestEncoding
    MsSmoothManifestEncoding (..),

    -- ** MxfAfdSignaling
    MxfAfdSignaling (..),

    -- ** MxfProfile
    MxfProfile (..),

    -- ** MxfXavcDurationMode
    MxfXavcDurationMode (..),

    -- ** NielsenActiveWatermarkProcessType
    NielsenActiveWatermarkProcessType (..),

    -- ** NielsenSourceWatermarkStatusType
    NielsenSourceWatermarkStatusType (..),

    -- ** NielsenUniqueTicPerAudioTrackType
    NielsenUniqueTicPerAudioTrackType (..),

    -- ** NoiseFilterPostTemporalSharpening
    NoiseFilterPostTemporalSharpening (..),

    -- ** NoiseFilterPostTemporalSharpeningStrength
    NoiseFilterPostTemporalSharpeningStrength (..),

    -- ** NoiseReducerFilter
    NoiseReducerFilter (..),

    -- ** Order
    Order (..),

    -- ** OutputGroupType
    OutputGroupType (..),

    -- ** OutputSdt
    OutputSdt (..),

    -- ** PadVideo
    PadVideo (..),

    -- ** PresetListBy
    PresetListBy (..),

    -- ** PricingPlan
    PricingPlan (..),

    -- ** ProresChromaSampling
    ProresChromaSampling (..),

    -- ** ProresCodecProfile
    ProresCodecProfile (..),

    -- ** ProresFramerateControl
    ProresFramerateControl (..),

    -- ** ProresFramerateConversionAlgorithm
    ProresFramerateConversionAlgorithm (..),

    -- ** ProresInterlaceMode
    ProresInterlaceMode (..),

    -- ** ProresParControl
    ProresParControl (..),

    -- ** ProresScanTypeConversionMode
    ProresScanTypeConversionMode (..),

    -- ** ProresSlowPal
    ProresSlowPal (..),

    -- ** ProresTelecine
    ProresTelecine (..),

    -- ** QueueListBy
    QueueListBy (..),

    -- ** QueueStatus
    QueueStatus (..),

    -- ** RenewalType
    RenewalType (..),

    -- ** RequiredFlag
    RequiredFlag (..),

    -- ** ReservationPlanStatus
    ReservationPlanStatus (..),

    -- ** RespondToAfd
    RespondToAfd (..),

    -- ** RuleType
    RuleType (..),

    -- ** S3ObjectCannedAcl
    S3ObjectCannedAcl (..),

    -- ** S3ServerSideEncryptionType
    S3ServerSideEncryptionType (..),

    -- ** SampleRangeConversion
    SampleRangeConversion (..),

    -- ** ScalingBehavior
    ScalingBehavior (..),

    -- ** SccDestinationFramerate
    SccDestinationFramerate (..),

    -- ** SimulateReservedQueue
    SimulateReservedQueue (..),

    -- ** SrtStylePassthrough
    SrtStylePassthrough (..),

    -- ** StatusUpdateInterval
    StatusUpdateInterval (..),

    -- ** TeletextPageType
    TeletextPageType (..),

    -- ** TimecodeBurninPosition
    TimecodeBurninPosition (..),

    -- ** TimecodeSource
    TimecodeSource (..),

    -- ** TimedMetadata
    TimedMetadata (..),

    -- ** TtmlStylePassthrough
    TtmlStylePassthrough (..),

    -- ** Type
    Type (..),

    -- ** Vc3Class
    Vc3Class (..),

    -- ** Vc3FramerateControl
    Vc3FramerateControl (..),

    -- ** Vc3FramerateConversionAlgorithm
    Vc3FramerateConversionAlgorithm (..),

    -- ** Vc3InterlaceMode
    Vc3InterlaceMode (..),

    -- ** Vc3ScanTypeConversionMode
    Vc3ScanTypeConversionMode (..),

    -- ** Vc3SlowPal
    Vc3SlowPal (..),

    -- ** Vc3Telecine
    Vc3Telecine (..),

    -- ** VchipAction
    VchipAction (..),

    -- ** VideoCodec
    VideoCodec (..),

    -- ** VideoTimecodeInsertion
    VideoTimecodeInsertion (..),

    -- ** Vp8FramerateControl
    Vp8FramerateControl (..),

    -- ** Vp8FramerateConversionAlgorithm
    Vp8FramerateConversionAlgorithm (..),

    -- ** Vp8ParControl
    Vp8ParControl (..),

    -- ** Vp8QualityTuningLevel
    Vp8QualityTuningLevel (..),

    -- ** Vp8RateControlMode
    Vp8RateControlMode (..),

    -- ** Vp9FramerateControl
    Vp9FramerateControl (..),

    -- ** Vp9FramerateConversionAlgorithm
    Vp9FramerateConversionAlgorithm (..),

    -- ** Vp9ParControl
    Vp9ParControl (..),

    -- ** Vp9QualityTuningLevel
    Vp9QualityTuningLevel (..),

    -- ** Vp9RateControlMode
    Vp9RateControlMode (..),

    -- ** WatermarkingStrength
    WatermarkingStrength (..),

    -- ** WavFormat
    WavFormat (..),

    -- ** WebvttAccessibilitySubs
    WebvttAccessibilitySubs (..),

    -- ** WebvttStylePassthrough
    WebvttStylePassthrough (..),

    -- ** Xavc4kIntraCbgProfileClass
    Xavc4kIntraCbgProfileClass (..),

    -- ** Xavc4kIntraVbrProfileClass
    Xavc4kIntraVbrProfileClass (..),

    -- ** Xavc4kProfileBitrateClass
    Xavc4kProfileBitrateClass (..),

    -- ** Xavc4kProfileCodecProfile
    Xavc4kProfileCodecProfile (..),

    -- ** Xavc4kProfileQualityTuningLevel
    Xavc4kProfileQualityTuningLevel (..),

    -- ** XavcAdaptiveQuantization
    XavcAdaptiveQuantization (..),

    -- ** XavcEntropyEncoding
    XavcEntropyEncoding (..),

    -- ** XavcFlickerAdaptiveQuantization
    XavcFlickerAdaptiveQuantization (..),

    -- ** XavcFramerateControl
    XavcFramerateControl (..),

    -- ** XavcFramerateConversionAlgorithm
    XavcFramerateConversionAlgorithm (..),

    -- ** XavcGopBReference
    XavcGopBReference (..),

    -- ** XavcHdIntraCbgProfileClass
    XavcHdIntraCbgProfileClass (..),

    -- ** XavcHdProfileBitrateClass
    XavcHdProfileBitrateClass (..),

    -- ** XavcHdProfileQualityTuningLevel
    XavcHdProfileQualityTuningLevel (..),

    -- ** XavcHdProfileTelecine
    XavcHdProfileTelecine (..),

    -- ** XavcInterlaceMode
    XavcInterlaceMode (..),

    -- ** XavcProfile
    XavcProfile (..),

    -- ** XavcSlowPal
    XavcSlowPal (..),

    -- ** XavcSpatialAdaptiveQuantization
    XavcSpatialAdaptiveQuantization (..),

    -- ** XavcTemporalAdaptiveQuantization
    XavcTemporalAdaptiveQuantization (..),

    -- ** AacSettings
    AacSettings (AacSettings'),
    newAacSettings,

    -- ** Ac3Settings
    Ac3Settings (Ac3Settings'),
    newAc3Settings,

    -- ** AccelerationSettings
    AccelerationSettings (AccelerationSettings'),
    newAccelerationSettings,

    -- ** AiffSettings
    AiffSettings (AiffSettings'),
    newAiffSettings,

    -- ** AllowedRenditionSize
    AllowedRenditionSize (AllowedRenditionSize'),
    newAllowedRenditionSize,

    -- ** AncillarySourceSettings
    AncillarySourceSettings (AncillarySourceSettings'),
    newAncillarySourceSettings,

    -- ** AudioChannelTaggingSettings
    AudioChannelTaggingSettings (AudioChannelTaggingSettings'),
    newAudioChannelTaggingSettings,

    -- ** AudioCodecSettings
    AudioCodecSettings (AudioCodecSettings'),
    newAudioCodecSettings,

    -- ** AudioDescription
    AudioDescription (AudioDescription'),
    newAudioDescription,

    -- ** AudioNormalizationSettings
    AudioNormalizationSettings (AudioNormalizationSettings'),
    newAudioNormalizationSettings,

    -- ** AudioSelector
    AudioSelector (AudioSelector'),
    newAudioSelector,

    -- ** AudioSelectorGroup
    AudioSelectorGroup (AudioSelectorGroup'),
    newAudioSelectorGroup,

    -- ** AutomatedAbrRule
    AutomatedAbrRule (AutomatedAbrRule'),
    newAutomatedAbrRule,

    -- ** AutomatedAbrSettings
    AutomatedAbrSettings (AutomatedAbrSettings'),
    newAutomatedAbrSettings,

    -- ** AutomatedEncodingSettings
    AutomatedEncodingSettings (AutomatedEncodingSettings'),
    newAutomatedEncodingSettings,

    -- ** Av1QvbrSettings
    Av1QvbrSettings (Av1QvbrSettings'),
    newAv1QvbrSettings,

    -- ** Av1Settings
    Av1Settings (Av1Settings'),
    newAv1Settings,

    -- ** AvailBlanking
    AvailBlanking (AvailBlanking'),
    newAvailBlanking,

    -- ** AvcIntraSettings
    AvcIntraSettings (AvcIntraSettings'),
    newAvcIntraSettings,

    -- ** AvcIntraUhdSettings
    AvcIntraUhdSettings (AvcIntraUhdSettings'),
    newAvcIntraUhdSettings,

    -- ** BurninDestinationSettings
    BurninDestinationSettings (BurninDestinationSettings'),
    newBurninDestinationSettings,

    -- ** CaptionDescription
    CaptionDescription (CaptionDescription'),
    newCaptionDescription,

    -- ** CaptionDescriptionPreset
    CaptionDescriptionPreset (CaptionDescriptionPreset'),
    newCaptionDescriptionPreset,

    -- ** CaptionDestinationSettings
    CaptionDestinationSettings (CaptionDestinationSettings'),
    newCaptionDestinationSettings,

    -- ** CaptionSelector
    CaptionSelector (CaptionSelector'),
    newCaptionSelector,

    -- ** CaptionSourceFramerate
    CaptionSourceFramerate (CaptionSourceFramerate'),
    newCaptionSourceFramerate,

    -- ** CaptionSourceSettings
    CaptionSourceSettings (CaptionSourceSettings'),
    newCaptionSourceSettings,

    -- ** ChannelMapping
    ChannelMapping (ChannelMapping'),
    newChannelMapping,

    -- ** CmafAdditionalManifest
    CmafAdditionalManifest (CmafAdditionalManifest'),
    newCmafAdditionalManifest,

    -- ** CmafEncryptionSettings
    CmafEncryptionSettings (CmafEncryptionSettings'),
    newCmafEncryptionSettings,

    -- ** CmafGroupSettings
    CmafGroupSettings (CmafGroupSettings'),
    newCmafGroupSettings,

    -- ** CmafImageBasedTrickPlaySettings
    CmafImageBasedTrickPlaySettings (CmafImageBasedTrickPlaySettings'),
    newCmafImageBasedTrickPlaySettings,

    -- ** CmfcSettings
    CmfcSettings (CmfcSettings'),
    newCmfcSettings,

    -- ** ColorCorrector
    ColorCorrector (ColorCorrector'),
    newColorCorrector,

    -- ** ContainerSettings
    ContainerSettings (ContainerSettings'),
    newContainerSettings,

    -- ** DashAdditionalManifest
    DashAdditionalManifest (DashAdditionalManifest'),
    newDashAdditionalManifest,

    -- ** DashIsoEncryptionSettings
    DashIsoEncryptionSettings (DashIsoEncryptionSettings'),
    newDashIsoEncryptionSettings,

    -- ** DashIsoGroupSettings
    DashIsoGroupSettings (DashIsoGroupSettings'),
    newDashIsoGroupSettings,

    -- ** DashIsoImageBasedTrickPlaySettings
    DashIsoImageBasedTrickPlaySettings (DashIsoImageBasedTrickPlaySettings'),
    newDashIsoImageBasedTrickPlaySettings,

    -- ** Deinterlacer
    Deinterlacer (Deinterlacer'),
    newDeinterlacer,

    -- ** DestinationSettings
    DestinationSettings (DestinationSettings'),
    newDestinationSettings,

    -- ** DolbyVision
    DolbyVision (DolbyVision'),
    newDolbyVision,

    -- ** DolbyVisionLevel6Metadata
    DolbyVisionLevel6Metadata (DolbyVisionLevel6Metadata'),
    newDolbyVisionLevel6Metadata,

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

    -- ** EmbeddedDestinationSettings
    EmbeddedDestinationSettings (EmbeddedDestinationSettings'),
    newEmbeddedDestinationSettings,

    -- ** EmbeddedSourceSettings
    EmbeddedSourceSettings (EmbeddedSourceSettings'),
    newEmbeddedSourceSettings,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EsamManifestConfirmConditionNotification
    EsamManifestConfirmConditionNotification (EsamManifestConfirmConditionNotification'),
    newEsamManifestConfirmConditionNotification,

    -- ** EsamSettings
    EsamSettings (EsamSettings'),
    newEsamSettings,

    -- ** EsamSignalProcessingNotification
    EsamSignalProcessingNotification (EsamSignalProcessingNotification'),
    newEsamSignalProcessingNotification,

    -- ** ExtendedDataServices
    ExtendedDataServices (ExtendedDataServices'),
    newExtendedDataServices,

    -- ** F4vSettings
    F4vSettings (F4vSettings'),
    newF4vSettings,

    -- ** FileGroupSettings
    FileGroupSettings (FileGroupSettings'),
    newFileGroupSettings,

    -- ** FileSourceSettings
    FileSourceSettings (FileSourceSettings'),
    newFileSourceSettings,

    -- ** ForceIncludeRenditionSize
    ForceIncludeRenditionSize (ForceIncludeRenditionSize'),
    newForceIncludeRenditionSize,

    -- ** FrameCaptureSettings
    FrameCaptureSettings (FrameCaptureSettings'),
    newFrameCaptureSettings,

    -- ** H264QvbrSettings
    H264QvbrSettings (H264QvbrSettings'),
    newH264QvbrSettings,

    -- ** H264Settings
    H264Settings (H264Settings'),
    newH264Settings,

    -- ** H265QvbrSettings
    H265QvbrSettings (H265QvbrSettings'),
    newH265QvbrSettings,

    -- ** H265Settings
    H265Settings (H265Settings'),
    newH265Settings,

    -- ** Hdr10Metadata
    Hdr10Metadata (Hdr10Metadata'),
    newHdr10Metadata,

    -- ** Hdr10Plus
    Hdr10Plus (Hdr10Plus'),
    newHdr10Plus,

    -- ** HlsAdditionalManifest
    HlsAdditionalManifest (HlsAdditionalManifest'),
    newHlsAdditionalManifest,

    -- ** HlsCaptionLanguageMapping
    HlsCaptionLanguageMapping (HlsCaptionLanguageMapping'),
    newHlsCaptionLanguageMapping,

    -- ** HlsEncryptionSettings
    HlsEncryptionSettings (HlsEncryptionSettings'),
    newHlsEncryptionSettings,

    -- ** HlsGroupSettings
    HlsGroupSettings (HlsGroupSettings'),
    newHlsGroupSettings,

    -- ** HlsImageBasedTrickPlaySettings
    HlsImageBasedTrickPlaySettings (HlsImageBasedTrickPlaySettings'),
    newHlsImageBasedTrickPlaySettings,

    -- ** HlsRenditionGroupSettings
    HlsRenditionGroupSettings (HlsRenditionGroupSettings'),
    newHlsRenditionGroupSettings,

    -- ** HlsSettings
    HlsSettings (HlsSettings'),
    newHlsSettings,

    -- ** HopDestination
    HopDestination (HopDestination'),
    newHopDestination,

    -- ** Id3Insertion
    Id3Insertion (Id3Insertion'),
    newId3Insertion,

    -- ** ImageInserter
    ImageInserter (ImageInserter'),
    newImageInserter,

    -- ** ImscDestinationSettings
    ImscDestinationSettings (ImscDestinationSettings'),
    newImscDestinationSettings,

    -- ** Input
    Input (Input'),
    newInput,

    -- ** InputClipping
    InputClipping (InputClipping'),
    newInputClipping,

    -- ** InputDecryptionSettings
    InputDecryptionSettings (InputDecryptionSettings'),
    newInputDecryptionSettings,

    -- ** InputTemplate
    InputTemplate (InputTemplate'),
    newInputTemplate,

    -- ** InputVideoGenerator
    InputVideoGenerator (InputVideoGenerator'),
    newInputVideoGenerator,

    -- ** InsertableImage
    InsertableImage (InsertableImage'),
    newInsertableImage,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobMessages
    JobMessages (JobMessages'),
    newJobMessages,

    -- ** JobSettings
    JobSettings (JobSettings'),
    newJobSettings,

    -- ** JobTemplate
    JobTemplate (JobTemplate'),
    newJobTemplate,

    -- ** JobTemplateSettings
    JobTemplateSettings (JobTemplateSettings'),
    newJobTemplateSettings,

    -- ** KantarWatermarkSettings
    KantarWatermarkSettings (KantarWatermarkSettings'),
    newKantarWatermarkSettings,

    -- ** M2tsScte35Esam
    M2tsScte35Esam (M2tsScte35Esam'),
    newM2tsScte35Esam,

    -- ** M2tsSettings
    M2tsSettings (M2tsSettings'),
    newM2tsSettings,

    -- ** M3u8Settings
    M3u8Settings (M3u8Settings'),
    newM3u8Settings,

    -- ** MinBottomRenditionSize
    MinBottomRenditionSize (MinBottomRenditionSize'),
    newMinBottomRenditionSize,

    -- ** MinTopRenditionSize
    MinTopRenditionSize (MinTopRenditionSize'),
    newMinTopRenditionSize,

    -- ** MotionImageInserter
    MotionImageInserter (MotionImageInserter'),
    newMotionImageInserter,

    -- ** MotionImageInsertionFramerate
    MotionImageInsertionFramerate (MotionImageInsertionFramerate'),
    newMotionImageInsertionFramerate,

    -- ** MotionImageInsertionOffset
    MotionImageInsertionOffset (MotionImageInsertionOffset'),
    newMotionImageInsertionOffset,

    -- ** MovSettings
    MovSettings (MovSettings'),
    newMovSettings,

    -- ** Mp2Settings
    Mp2Settings (Mp2Settings'),
    newMp2Settings,

    -- ** Mp3Settings
    Mp3Settings (Mp3Settings'),
    newMp3Settings,

    -- ** Mp4Settings
    Mp4Settings (Mp4Settings'),
    newMp4Settings,

    -- ** MpdSettings
    MpdSettings (MpdSettings'),
    newMpdSettings,

    -- ** Mpeg2Settings
    Mpeg2Settings (Mpeg2Settings'),
    newMpeg2Settings,

    -- ** MsSmoothAdditionalManifest
    MsSmoothAdditionalManifest (MsSmoothAdditionalManifest'),
    newMsSmoothAdditionalManifest,

    -- ** MsSmoothEncryptionSettings
    MsSmoothEncryptionSettings (MsSmoothEncryptionSettings'),
    newMsSmoothEncryptionSettings,

    -- ** MsSmoothGroupSettings
    MsSmoothGroupSettings (MsSmoothGroupSettings'),
    newMsSmoothGroupSettings,

    -- ** MxfSettings
    MxfSettings (MxfSettings'),
    newMxfSettings,

    -- ** MxfXavcProfileSettings
    MxfXavcProfileSettings (MxfXavcProfileSettings'),
    newMxfXavcProfileSettings,

    -- ** NexGuardFileMarkerSettings
    NexGuardFileMarkerSettings (NexGuardFileMarkerSettings'),
    newNexGuardFileMarkerSettings,

    -- ** NielsenConfiguration
    NielsenConfiguration (NielsenConfiguration'),
    newNielsenConfiguration,

    -- ** NielsenNonLinearWatermarkSettings
    NielsenNonLinearWatermarkSettings (NielsenNonLinearWatermarkSettings'),
    newNielsenNonLinearWatermarkSettings,

    -- ** NoiseReducer
    NoiseReducer (NoiseReducer'),
    newNoiseReducer,

    -- ** NoiseReducerFilterSettings
    NoiseReducerFilterSettings (NoiseReducerFilterSettings'),
    newNoiseReducerFilterSettings,

    -- ** NoiseReducerSpatialFilterSettings
    NoiseReducerSpatialFilterSettings (NoiseReducerSpatialFilterSettings'),
    newNoiseReducerSpatialFilterSettings,

    -- ** NoiseReducerTemporalFilterSettings
    NoiseReducerTemporalFilterSettings (NoiseReducerTemporalFilterSettings'),
    newNoiseReducerTemporalFilterSettings,

    -- ** OpusSettings
    OpusSettings (OpusSettings'),
    newOpusSettings,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** OutputChannelMapping
    OutputChannelMapping (OutputChannelMapping'),
    newOutputChannelMapping,

    -- ** OutputDetail
    OutputDetail (OutputDetail'),
    newOutputDetail,

    -- ** OutputGroup
    OutputGroup (OutputGroup'),
    newOutputGroup,

    -- ** OutputGroupDetail
    OutputGroupDetail (OutputGroupDetail'),
    newOutputGroupDetail,

    -- ** OutputGroupSettings
    OutputGroupSettings (OutputGroupSettings'),
    newOutputGroupSettings,

    -- ** OutputSettings
    OutputSettings (OutputSettings'),
    newOutputSettings,

    -- ** PartnerWatermarking
    PartnerWatermarking (PartnerWatermarking'),
    newPartnerWatermarking,

    -- ** Policy
    Policy (Policy'),
    newPolicy,

    -- ** Preset
    Preset (Preset'),
    newPreset,

    -- ** PresetSettings
    PresetSettings (PresetSettings'),
    newPresetSettings,

    -- ** ProresSettings
    ProresSettings (ProresSettings'),
    newProresSettings,

    -- ** Queue
    Queue (Queue'),
    newQueue,

    -- ** QueueTransition
    QueueTransition (QueueTransition'),
    newQueueTransition,

    -- ** Rectangle
    Rectangle (Rectangle'),
    newRectangle,

    -- ** RemixSettings
    RemixSettings (RemixSettings'),
    newRemixSettings,

    -- ** ReservationPlan
    ReservationPlan (ReservationPlan'),
    newReservationPlan,

    -- ** ReservationPlanSettings
    ReservationPlanSettings (ReservationPlanSettings'),
    newReservationPlanSettings,

    -- ** ResourceTags
    ResourceTags (ResourceTags'),
    newResourceTags,

    -- ** S3DestinationAccessControl
    S3DestinationAccessControl (S3DestinationAccessControl'),
    newS3DestinationAccessControl,

    -- ** S3DestinationSettings
    S3DestinationSettings (S3DestinationSettings'),
    newS3DestinationSettings,

    -- ** S3EncryptionSettings
    S3EncryptionSettings (S3EncryptionSettings'),
    newS3EncryptionSettings,

    -- ** SccDestinationSettings
    SccDestinationSettings (SccDestinationSettings'),
    newSccDestinationSettings,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (SpekeKeyProvider'),
    newSpekeKeyProvider,

    -- ** SpekeKeyProviderCmaf
    SpekeKeyProviderCmaf (SpekeKeyProviderCmaf'),
    newSpekeKeyProviderCmaf,

    -- ** SrtDestinationSettings
    SrtDestinationSettings (SrtDestinationSettings'),
    newSrtDestinationSettings,

    -- ** StaticKeyProvider
    StaticKeyProvider (StaticKeyProvider'),
    newStaticKeyProvider,

    -- ** TeletextDestinationSettings
    TeletextDestinationSettings (TeletextDestinationSettings'),
    newTeletextDestinationSettings,

    -- ** TeletextSourceSettings
    TeletextSourceSettings (TeletextSourceSettings'),
    newTeletextSourceSettings,

    -- ** TimecodeBurnin
    TimecodeBurnin (TimecodeBurnin'),
    newTimecodeBurnin,

    -- ** TimecodeConfig
    TimecodeConfig (TimecodeConfig'),
    newTimecodeConfig,

    -- ** TimedMetadataInsertion
    TimedMetadataInsertion (TimedMetadataInsertion'),
    newTimedMetadataInsertion,

    -- ** Timing
    Timing (Timing'),
    newTiming,

    -- ** TrackSourceSettings
    TrackSourceSettings (TrackSourceSettings'),
    newTrackSourceSettings,

    -- ** TtmlDestinationSettings
    TtmlDestinationSettings (TtmlDestinationSettings'),
    newTtmlDestinationSettings,

    -- ** Vc3Settings
    Vc3Settings (Vc3Settings'),
    newVc3Settings,

    -- ** VideoCodecSettings
    VideoCodecSettings (VideoCodecSettings'),
    newVideoCodecSettings,

    -- ** VideoDescription
    VideoDescription (VideoDescription'),
    newVideoDescription,

    -- ** VideoDetail
    VideoDetail (VideoDetail'),
    newVideoDetail,

    -- ** VideoPreprocessor
    VideoPreprocessor (VideoPreprocessor'),
    newVideoPreprocessor,

    -- ** VideoSelector
    VideoSelector (VideoSelector'),
    newVideoSelector,

    -- ** VorbisSettings
    VorbisSettings (VorbisSettings'),
    newVorbisSettings,

    -- ** Vp8Settings
    Vp8Settings (Vp8Settings'),
    newVp8Settings,

    -- ** Vp9Settings
    Vp9Settings (Vp9Settings'),
    newVp9Settings,

    -- ** WavSettings
    WavSettings (WavSettings'),
    newWavSettings,

    -- ** WebvttDestinationSettings
    WebvttDestinationSettings (WebvttDestinationSettings'),
    newWebvttDestinationSettings,

    -- ** WebvttHlsSourceSettings
    WebvttHlsSourceSettings (WebvttHlsSourceSettings'),
    newWebvttHlsSourceSettings,

    -- ** Xavc4kIntraCbgProfileSettings
    Xavc4kIntraCbgProfileSettings (Xavc4kIntraCbgProfileSettings'),
    newXavc4kIntraCbgProfileSettings,

    -- ** Xavc4kIntraVbrProfileSettings
    Xavc4kIntraVbrProfileSettings (Xavc4kIntraVbrProfileSettings'),
    newXavc4kIntraVbrProfileSettings,

    -- ** Xavc4kProfileSettings
    Xavc4kProfileSettings (Xavc4kProfileSettings'),
    newXavc4kProfileSettings,

    -- ** XavcHdIntraCbgProfileSettings
    XavcHdIntraCbgProfileSettings (XavcHdIntraCbgProfileSettings'),
    newXavcHdIntraCbgProfileSettings,

    -- ** XavcHdProfileSettings
    XavcHdProfileSettings (XavcHdProfileSettings'),
    newXavcHdProfileSettings,

    -- ** XavcSettings
    XavcSettings (XavcSettings'),
    newXavcSettings,
  )
where

import Amazonka.MediaConvert.AssociateCertificate
import Amazonka.MediaConvert.CancelJob
import Amazonka.MediaConvert.CreateJob
import Amazonka.MediaConvert.CreateJobTemplate
import Amazonka.MediaConvert.CreatePreset
import Amazonka.MediaConvert.CreateQueue
import Amazonka.MediaConvert.DeleteJobTemplate
import Amazonka.MediaConvert.DeletePolicy
import Amazonka.MediaConvert.DeletePreset
import Amazonka.MediaConvert.DeleteQueue
import Amazonka.MediaConvert.DescribeEndpoints
import Amazonka.MediaConvert.DisassociateCertificate
import Amazonka.MediaConvert.GetJob
import Amazonka.MediaConvert.GetJobTemplate
import Amazonka.MediaConvert.GetPolicy
import Amazonka.MediaConvert.GetPreset
import Amazonka.MediaConvert.GetQueue
import Amazonka.MediaConvert.Lens
import Amazonka.MediaConvert.ListJobTemplates
import Amazonka.MediaConvert.ListJobs
import Amazonka.MediaConvert.ListPresets
import Amazonka.MediaConvert.ListQueues
import Amazonka.MediaConvert.ListTagsForResource
import Amazonka.MediaConvert.PutPolicy
import Amazonka.MediaConvert.TagResource
import Amazonka.MediaConvert.Types
import Amazonka.MediaConvert.UntagResource
import Amazonka.MediaConvert.UpdateJobTemplate
import Amazonka.MediaConvert.UpdatePreset
import Amazonka.MediaConvert.UpdateQueue
import Amazonka.MediaConvert.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaConvert'.

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
