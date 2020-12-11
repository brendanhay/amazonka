-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types
  ( -- * Service configuration
    elasticTranscoderService,

    -- * Errors

    -- * Artwork
    Artwork (..),
    mkArtwork,
    aSizingPolicy,
    aAlbumArtFormat,
    aMaxHeight,
    aInputKey,
    aPaddingPolicy,
    aEncryption,
    aMaxWidth,

    -- * AudioCodecOptions
    AudioCodecOptions (..),
    mkAudioCodecOptions,
    acoSigned,
    acoBitDepth,
    acoProfile,
    acoBitOrder,

    -- * AudioParameters
    AudioParameters (..),
    mkAudioParameters,
    apChannels,
    apCodec,
    apAudioPackingMode,
    apSampleRate,
    apBitRate,
    apCodecOptions,

    -- * CaptionFormat
    CaptionFormat (..),
    mkCaptionFormat,
    cfPattern,
    cfFormat,
    cfEncryption,

    -- * CaptionSource
    CaptionSource (..),
    mkCaptionSource,
    csTimeOffset,
    csEncryption,
    csKey,
    csLanguage,
    csLabel,

    -- * Captions
    Captions (..),
    mkCaptions,
    cMergePolicy,
    cCaptionSources,
    cCaptionFormats,

    -- * Clip
    Clip (..),
    mkClip,
    cTimeSpan,

    -- * CreateJobOutput
    CreateJobOutput (..),
    mkCreateJobOutput,
    cjoThumbnailPattern,
    cjoCaptions,
    cjoPresetId,
    cjoComposition,
    cjoAlbumArt,
    cjoWatermarks,
    cjoEncryption,
    cjoKey,
    cjoSegmentDuration,
    cjoThumbnailEncryption,
    cjoRotate,

    -- * CreateJobPlaylist
    CreateJobPlaylist (..),
    mkCreateJobPlaylist,
    cjpPlayReadyDrm,
    cjpFormat,
    cjpOutputKeys,
    cjpName,
    cjpHlsContentProtection,

    -- * DetectedProperties
    DetectedProperties (..),
    mkDetectedProperties,
    dpHeight,
    dpFrameRate,
    dpFileSize,
    dpWidth,
    dpDurationMillis,

    -- * Encryption
    Encryption (..),
    mkEncryption,
    eMode,
    eKeyMD5,
    eKey,
    eInitializationVector,

    -- * HlsContentProtection
    HlsContentProtection (..),
    mkHlsContentProtection,
    hcpKeyMD5,
    hcpKeyStoragePolicy,
    hcpKey,
    hcpMethod,
    hcpInitializationVector,
    hcpLicenseAcquisitionURL,

    -- * InputCaptions
    InputCaptions (..),
    mkInputCaptions,
    icMergePolicy,
    icCaptionSources,

    -- * Job'
    Job' (..),
    mkJob',
    jStatus,
    jPipelineId,
    jARN,
    jInputs,
    jInput,
    jUserMetadata,
    jOutputs,
    jOutput,
    jId,
    jPlaylists,
    jOutputKeyPrefix,
    jTiming,

    -- * JobAlbumArt
    JobAlbumArt (..),
    mkJobAlbumArt,
    jaaMergePolicy,
    jaaArtwork,

    -- * JobInput
    JobInput (..),
    mkJobInput,
    jiFrameRate,
    jiResolution,
    jiAspectRatio,
    jiTimeSpan,
    jiEncryption,
    jiKey,
    jiDetectedProperties,
    jiContainer,
    jiInterlaced,
    jiInputCaptions,

    -- * JobOutput
    JobOutput (..),
    mkJobOutput,
    joAppliedColorSpaceConversion,
    joThumbnailPattern,
    joStatus,
    joHeight,
    joFrameRate,
    joCaptions,
    joPresetId,
    joComposition,
    joAlbumArt,
    joFileSize,
    joWatermarks,
    joWidth,
    joEncryption,
    joKey,
    joStatusDetail,
    joId,
    joSegmentDuration,
    joDurationMillis,
    joThumbnailEncryption,
    joDuration,
    joRotate,

    -- * JobWatermark
    JobWatermark (..),
    mkJobWatermark,
    jwPresetWatermarkId,
    jwInputKey,
    jwEncryption,

    -- * Notifications
    Notifications (..),
    mkNotifications,
    nError,
    nWarning,
    nProgressing,
    nCompleted,

    -- * Permission
    Permission (..),
    mkPermission,
    pAccess,
    pGranteeType,
    pGrantee,

    -- * Pipeline
    Pipeline (..),
    mkPipeline,
    pipStatus,
    pipARN,
    pipInputBucket,
    pipContentConfig,
    pipOutputBucket,
    pipRole,
    pipName,
    pipAWSKMSKeyARN,
    pipId,
    pipNotifications,
    pipThumbnailConfig,

    -- * PipelineOutputConfig
    PipelineOutputConfig (..),
    mkPipelineOutputConfig,
    pocBucket,
    pocStorageClass,
    pocPermissions,

    -- * PlayReadyDrm
    PlayReadyDrm (..),
    mkPlayReadyDrm,
    prdKeyId,
    prdFormat,
    prdKeyMD5,
    prdKey,
    prdInitializationVector,
    prdLicenseAcquisitionURL,

    -- * Playlist
    Playlist (..),
    mkPlaylist,
    pStatus,
    pPlayReadyDrm,
    pFormat,
    pOutputKeys,
    pName,
    pStatusDetail,
    pHlsContentProtection,

    -- * Preset
    Preset (..),
    mkPreset,
    preARN,
    preVideo,
    preThumbnails,
    preName,
    preContainer,
    preId,
    preType,
    preDescription,
    preAudio,

    -- * PresetWatermark
    PresetWatermark (..),
    mkPresetWatermark,
    pwVerticalAlign,
    pwSizingPolicy,
    pwHorizontalOffset,
    pwMaxHeight,
    pwOpacity,
    pwVerticalOffset,
    pwMaxWidth,
    pwId,
    pwHorizontalAlign,
    pwTarget,

    -- * Thumbnails
    Thumbnails (..),
    mkThumbnails,
    tSizingPolicy,
    tFormat,
    tMaxHeight,
    tResolution,
    tAspectRatio,
    tPaddingPolicy,
    tInterval,
    tMaxWidth,

    -- * TimeSpan
    TimeSpan (..),
    mkTimeSpan,
    tsStartTime,
    tsDuration,

    -- * Timing
    Timing (..),
    mkTiming,
    tSubmitTimeMillis,
    tFinishTimeMillis,
    tStartTimeMillis,

    -- * VideoParameters
    VideoParameters (..),
    mkVideoParameters,
    vpKeyframesMaxDist,
    vpFrameRate,
    vpSizingPolicy,
    vpMaxFrameRate,
    vpMaxHeight,
    vpWatermarks,
    vpDisplayAspectRatio,
    vpResolution,
    vpCodec,
    vpAspectRatio,
    vpPaddingPolicy,
    vpMaxWidth,
    vpBitRate,
    vpFixedGOP,
    vpCodecOptions,

    -- * Warning
    Warning (..),
    mkWarning,
    wCode,
    wMessage,
  )
where

import Network.AWS.ElasticTranscoder.Types.Artwork
import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
import Network.AWS.ElasticTranscoder.Types.AudioParameters
import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.CreateJobOutput
import Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
import Network.AWS.ElasticTranscoder.Types.DetectedProperties
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.InputCaptions
import Network.AWS.ElasticTranscoder.Types.Job'
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobInput
import Network.AWS.ElasticTranscoder.Types.JobOutput
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import Network.AWS.ElasticTranscoder.Types.Notifications
import Network.AWS.ElasticTranscoder.Types.Permission
import Network.AWS.ElasticTranscoder.Types.Pipeline
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import Network.AWS.ElasticTranscoder.Types.Playlist
import Network.AWS.ElasticTranscoder.Types.Preset
import Network.AWS.ElasticTranscoder.Types.PresetWatermark
import Network.AWS.ElasticTranscoder.Types.Thumbnails
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import Network.AWS.ElasticTranscoder.Types.Timing
import Network.AWS.ElasticTranscoder.Types.VideoParameters
import Network.AWS.ElasticTranscoder.Types.Warning
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-09-25@ of the Amazon Elastic Transcoder SDK configuration.
elasticTranscoderService :: Lude.Service
elasticTranscoderService =
  Lude.Service
    { Lude._svcAbbrev = "ElasticTranscoder",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elastictranscoder",
      Lude._svcVersion = "2012-09-25",
      Lude._svcEndpoint = Lude.defaultEndpoint elasticTranscoderService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ElasticTranscoder",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
