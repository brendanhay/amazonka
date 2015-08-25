{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types
    (
    -- * Service Configuration
      elasticTranscoder

    -- * Errors
    , _ValidationException
    , _IncompatibleVersionException
    , _AccessDeniedException
    , _InternalServiceException
    , _ResourceNotFoundException
    , _ResourceInUseException
    , _LimitExceededException

    -- * Artwork
    , Artwork
    , artwork
    , aSizingPolicy
    , aMaxHeight
    , aAlbumArtFormat
    , aInputKey
    , aPaddingPolicy
    , aEncryption
    , aMaxWidth

    -- * AudioCodecOptions
    , AudioCodecOptions
    , audioCodecOptions
    , acoBitDepth
    , acoSigned
    , acoProfile
    , acoBitOrder

    -- * AudioParameters
    , AudioParameters
    , audioParameters
    , apChannels
    , apCodec
    , apAudioPackingMode
    , apSampleRate
    , apBitRate
    , apCodecOptions

    -- * CaptionFormat
    , CaptionFormat
    , captionFormat
    , cfPattern
    , cfFormat
    , cfEncryption

    -- * CaptionSource
    , CaptionSource
    , captionSource
    , csTimeOffset
    , csKey
    , csEncryption
    , csLanguage
    , csLabel

    -- * Captions
    , Captions
    , captions
    , cMergePolicy
    , cCaptionSources
    , cCaptionFormats

    -- * Clip
    , Clip
    , clip
    , cTimeSpan

    -- * CreateJobOutput
    , CreateJobOutput
    , createJobOutput
    , cjoThumbnailPattern
    , cjoCaptions
    , cjoPresetId
    , cjoComposition
    , cjoAlbumArt
    , cjoWatermarks
    , cjoKey
    , cjoEncryption
    , cjoSegmentDuration
    , cjoThumbnailEncryption
    , cjoRotate

    -- * CreateJobPlaylist
    , CreateJobPlaylist
    , createJobPlaylist
    , cjpPlayReadyDrm
    , cjpOutputKeys
    , cjpFormat
    , cjpName
    , cjpHlsContentProtection

    -- * DetectedProperties
    , DetectedProperties
    , detectedProperties
    , dpHeight
    , dpFrameRate
    , dpFileSize
    , dpWidth
    , dpDurationMillis

    -- * Encryption
    , Encryption
    , encryption
    , eKeyMD5
    , eMode
    , eKey
    , eInitializationVector

    -- * HlsContentProtection
    , HlsContentProtection
    , hlsContentProtection
    , hcpKeyMD5
    , hcpKeyStoragePolicy
    , hcpKey
    , hcpMethod
    , hcpLicenseAcquisitionURL
    , hcpInitializationVector

    -- * Job'
    , Job'
    , job'
    , jStatus
    , jPipelineId
    , jARN
    , jInput
    , jOutputs
    , jUserMetadata
    , jOutput
    , jId
    , jPlaylists
    , jTiming
    , jOutputKeyPrefix

    -- * JobAlbumArt
    , JobAlbumArt
    , jobAlbumArt
    , jaaMergePolicy
    , jaaArtwork

    -- * JobInput
    , JobInput
    , jobInput
    , jiFrameRate
    , jiResolution
    , jiAspectRatio
    , jiKey
    , jiDetectedProperties
    , jiEncryption
    , jiContainer
    , jiInterlaced

    -- * JobOutput
    , JobOutput
    , jobOutput
    , joAppliedColorSpaceConversion
    , joStatus
    , joThumbnailPattern
    , joHeight
    , joFrameRate
    , joCaptions
    , joPresetId
    , joComposition
    , joAlbumArt
    , joFileSize
    , joWatermarks
    , joWidth
    , joKey
    , joEncryption
    , joId
    , joSegmentDuration
    , joStatusDetail
    , joDurationMillis
    , joThumbnailEncryption
    , joDuration
    , joRotate

    -- * JobWatermark
    , JobWatermark
    , jobWatermark
    , jwPresetWatermarkId
    , jwInputKey
    , jwEncryption

    -- * Notifications
    , Notifications
    , notifications
    , nError
    , nWarning
    , nCompleted
    , nProgressing

    -- * Permission
    , Permission
    , permission
    , pAccess
    , pGranteeType
    , pGrantee

    -- * Pipeline
    , Pipeline
    , pipeline
    , pipStatus
    , pipARN
    , pipInputBucket
    , pipContentConfig
    , pipOutputBucket
    , pipRole
    , pipName
    , pipAWSKMSKeyARN
    , pipId
    , pipThumbnailConfig
    , pipNotifications

    -- * PipelineOutputConfig
    , PipelineOutputConfig
    , pipelineOutputConfig
    , pocBucket
    , pocStorageClass
    , pocPermissions

    -- * PlayReadyDrm
    , PlayReadyDrm
    , playReadyDrm
    , prdKeyId
    , prdKeyMD5
    , prdFormat
    , prdKey
    , prdLicenseAcquisitionURL
    , prdInitializationVector

    -- * Playlist
    , Playlist
    , playlist
    , pPlayReadyDrm
    , pStatus
    , pOutputKeys
    , pFormat
    , pName
    , pHlsContentProtection
    , pStatusDetail

    -- * Preset
    , Preset
    , preset
    , preARN
    , preVideo
    , preName
    , preThumbnails
    , preContainer
    , preId
    , preType
    , preAudio
    , preDescription

    -- * PresetWatermark
    , PresetWatermark
    , presetWatermark
    , pwVerticalAlign
    , pwSizingPolicy
    , pwMaxHeight
    , pwHorizontalOffset
    , pwOpacity
    , pwVerticalOffset
    , pwMaxWidth
    , pwId
    , pwHorizontalAlign
    , pwTarget

    -- * Thumbnails
    , Thumbnails
    , thumbnails
    , tSizingPolicy
    , tFormat
    , tMaxHeight
    , tResolution
    , tPaddingPolicy
    , tAspectRatio
    , tInterval
    , tMaxWidth

    -- * TimeSpan
    , TimeSpan
    , timeSpan
    , tsStartTime
    , tsDuration

    -- * Timing
    , Timing
    , timing
    , tSubmitTimeMillis
    , tFinishTimeMillis
    , tStartTimeMillis

    -- * VideoParameters
    , VideoParameters
    , videoParameters
    , vpKeyframesMaxDist
    , vpFrameRate
    , vpSizingPolicy
    , vpMaxFrameRate
    , vpMaxHeight
    , vpDisplayAspectRatio
    , vpWatermarks
    , vpCodec
    , vpResolution
    , vpPaddingPolicy
    , vpAspectRatio
    , vpMaxWidth
    , vpBitRate
    , vpFixedGOP
    , vpCodecOptions

    -- * Warning
    , Warning
    , warning
    , wCode
    , wMessage
    ) where

import           Network.AWS.ElasticTranscoder.Types.Product
import           Network.AWS.ElasticTranscoder.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2012-09-25' of the Amazon Elastic Transcoder SDK configuration.
elasticTranscoder :: Service
elasticTranscoder =
    Service
    { _svcAbbrev = "ElasticTranscoder"
    , _svcSigner = v4
    , _svcPrefix = "elastictranscoder"
    , _svcVersion = "2012-09-25"
    , _svcEndpoint = defaultEndpoint elasticTranscoder
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | One or more required parameter values were not provided in the request.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException =
    _ServiceError . hasStatus 400 . hasCode "ValidationException"

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersionException =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersionException"

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException =
    _ServiceError . hasStatus 403 . hasCode "AccessDeniedException"

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException = _ServiceError . hasCode "InternalServiceException"

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
    _ServiceError . hasStatus 409 . hasCode "ResourceInUseException"

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 429 . hasCode "LimitExceededException"
