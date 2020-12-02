{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types
  ( -- * Service Configuration
    elasticTranscoder,

    -- * Errors

    -- * Artwork
    Artwork,
    artwork,
    aSizingPolicy,
    aAlbumArtFormat,
    aMaxHeight,
    aInputKey,
    aPaddingPolicy,
    aEncryption,
    aMaxWidth,

    -- * AudioCodecOptions
    AudioCodecOptions,
    audioCodecOptions,
    acoSigned,
    acoBitDepth,
    acoProfile,
    acoBitOrder,

    -- * AudioParameters
    AudioParameters,
    audioParameters,
    apChannels,
    apCodec,
    apAudioPackingMode,
    apSampleRate,
    apBitRate,
    apCodecOptions,

    -- * CaptionFormat
    CaptionFormat,
    captionFormat,
    cfPattern,
    cfFormat,
    cfEncryption,

    -- * CaptionSource
    CaptionSource,
    captionSource,
    csTimeOffset,
    csEncryption,
    csKey,
    csLanguage,
    csLabel,

    -- * Captions
    Captions,
    captions,
    cMergePolicy,
    cCaptionSources,
    cCaptionFormats,

    -- * Clip
    Clip,
    clip,
    cTimeSpan,

    -- * CreateJobOutput
    CreateJobOutput,
    createJobOutput,
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
    CreateJobPlaylist,
    createJobPlaylist,
    cjpPlayReadyDrm,
    cjpFormat,
    cjpOutputKeys,
    cjpName,
    cjpHlsContentProtection,

    -- * DetectedProperties
    DetectedProperties,
    detectedProperties,
    dpHeight,
    dpFrameRate,
    dpFileSize,
    dpWidth,
    dpDurationMillis,

    -- * Encryption
    Encryption,
    encryption,
    eMode,
    eKeyMD5,
    eKey,
    eInitializationVector,

    -- * HlsContentProtection
    HlsContentProtection,
    hlsContentProtection,
    hcpKeyMD5,
    hcpKeyStoragePolicy,
    hcpKey,
    hcpMethod,
    hcpInitializationVector,
    hcpLicenseAcquisitionURL,

    -- * InputCaptions
    InputCaptions,
    inputCaptions,
    icMergePolicy,
    icCaptionSources,

    -- * Job'
    Job',
    job',
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
    JobAlbumArt,
    jobAlbumArt,
    jaaMergePolicy,
    jaaArtwork,

    -- * JobInput
    JobInput,
    jobInput,
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
    JobOutput,
    jobOutput,
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
    JobWatermark,
    jobWatermark,
    jwPresetWatermarkId,
    jwInputKey,
    jwEncryption,

    -- * Notifications
    Notifications,
    notifications,
    nError,
    nWarning,
    nProgressing,
    nCompleted,

    -- * Permission
    Permission,
    permission,
    pAccess,
    pGranteeType,
    pGrantee,

    -- * Pipeline
    Pipeline,
    pipeline,
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
    PipelineOutputConfig,
    pipelineOutputConfig,
    pocBucket,
    pocStorageClass,
    pocPermissions,

    -- * PlayReadyDrm
    PlayReadyDrm,
    playReadyDrm,
    prdKeyId,
    prdFormat,
    prdKeyMD5,
    prdKey,
    prdInitializationVector,
    prdLicenseAcquisitionURL,

    -- * Playlist
    Playlist,
    playlist,
    pStatus,
    pPlayReadyDrm,
    pFormat,
    pOutputKeys,
    pName,
    pStatusDetail,
    pHlsContentProtection,

    -- * Preset
    Preset,
    preset,
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
    PresetWatermark,
    presetWatermark,
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
    Thumbnails,
    thumbnails,
    tSizingPolicy,
    tFormat,
    tMaxHeight,
    tResolution,
    tAspectRatio,
    tPaddingPolicy,
    tInterval,
    tMaxWidth,

    -- * TimeSpan
    TimeSpan,
    timeSpan,
    tsStartTime,
    tsDuration,

    -- * Timing
    Timing,
    timing,
    tSubmitTimeMillis,
    tFinishTimeMillis,
    tStartTimeMillis,

    -- * VideoParameters
    VideoParameters,
    videoParameters,
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
    Warning,
    warning,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-09-25@ of the Amazon Elastic Transcoder SDK configuration.
elasticTranscoder :: Service
elasticTranscoder =
  Service
    { _svcAbbrev = "ElasticTranscoder",
      _svcSigner = v4,
      _svcPrefix = "elastictranscoder",
      _svcVersion = "2012-09-25",
      _svcEndpoint = defaultEndpoint elasticTranscoder,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ElasticTranscoder",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
