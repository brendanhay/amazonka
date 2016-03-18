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
    , _IncompatibleVersionException
    , _ValidationException
    , _AccessDeniedException
    , _InternalServiceException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * Artwork
    , Artwork
    , artwork
    , aSizingPolicy
    , aAlbumArtFormat
    , aMaxHeight
    , aInputKey
    , aPaddingPolicy
    , aEncryption
    , aMaxWidth

    -- * AudioCodecOptions
    , AudioCodecOptions
    , audioCodecOptions
    , acoSigned
    , acoBitDepth
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
    , csEncryption
    , csKey
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
    , cjoEncryption
    , cjoKey
    , cjoSegmentDuration
    , cjoThumbnailEncryption
    , cjoRotate

    -- * CreateJobPlaylist
    , CreateJobPlaylist
    , createJobPlaylist
    , cjpPlayReadyDrm
    , cjpFormat
    , cjpOutputKeys
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
    , eMode
    , eKeyMD5
    , eKey
    , eInitializationVector

    -- * HlsContentProtection
    , HlsContentProtection
    , hlsContentProtection
    , hcpKeyMD5
    , hcpKeyStoragePolicy
    , hcpKey
    , hcpMethod
    , hcpInitializationVector
    , hcpLicenseAcquisitionURL

    -- * Job'
    , Job'
    , job'
    , jStatus
    , jPipelineId
    , jARN
    , jInput
    , jUserMetadata
    , jOutputs
    , jOutput
    , jId
    , jPlaylists
    , jOutputKeyPrefix
    , jTiming

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
    , jiEncryption
    , jiKey
    , jiDetectedProperties
    , jiContainer
    , jiInterlaced

    -- * JobOutput
    , JobOutput
    , jobOutput
    , joAppliedColorSpaceConversion
    , joThumbnailPattern
    , joStatus
    , joHeight
    , joFrameRate
    , joCaptions
    , joPresetId
    , joComposition
    , joAlbumArt
    , joFileSize
    , joWatermarks
    , joWidth
    , joEncryption
    , joKey
    , joStatusDetail
    , joId
    , joSegmentDuration
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
    , nProgressing
    , nCompleted

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
    , pipNotifications
    , pipThumbnailConfig

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
    , prdFormat
    , prdKeyMD5
    , prdKey
    , prdInitializationVector
    , prdLicenseAcquisitionURL

    -- * Playlist
    , Playlist
    , playlist
    , pStatus
    , pPlayReadyDrm
    , pFormat
    , pOutputKeys
    , pName
    , pStatusDetail
    , pHlsContentProtection

    -- * Preset
    , Preset
    , preset
    , preARN
    , preVideo
    , preThumbnails
    , preName
    , preContainer
    , preId
    , preType
    , preDescription
    , preAudio

    -- * PresetWatermark
    , PresetWatermark
    , presetWatermark
    , pwVerticalAlign
    , pwSizingPolicy
    , pwHorizontalOffset
    , pwMaxHeight
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
    , tAspectRatio
    , tPaddingPolicy
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
    , vpWatermarks
    , vpDisplayAspectRatio
    , vpResolution
    , vpCodec
    , vpAspectRatio
    , vpPaddingPolicy
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
import           Network.AWS.Lens
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersionException =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersionException"

-- | One or more required parameter values were not provided in the request.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException =
    _ServiceError . hasStatus 400 . hasCode "ValidationException"

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

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 429 . hasCode "LimitExceededException"

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
    _ServiceError . hasStatus 409 . hasCode "ResourceInUseException"
