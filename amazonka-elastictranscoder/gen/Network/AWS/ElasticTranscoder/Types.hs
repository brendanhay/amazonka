{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types
    (
    -- * Service
      ElasticTranscoder

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
    , artSizingPolicy
    , artMaxHeight
    , artAlbumArtFormat
    , artInputKey
    , artPaddingPolicy
    , artEncryption
    , artMaxWidth

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
    , capMergePolicy
    , capCaptionSources
    , capCaptionFormats

    -- * Clip
    , Clip
    , clip
    , cliTimeSpan

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
    , encKeyMD5
    , encMode
    , encKey
    , encInitializationVector

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
    , notError
    , notWarning
    , notCompleted
    , notProgressing

    -- * Permission
    , Permission
    , permission
    , perAccess
    , perGranteeType
    , perGrantee

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
    , plaPlayReadyDrm
    , plaStatus
    , plaOutputKeys
    , plaFormat
    , plaName
    , plaHlsContentProtection
    , plaStatusDetail

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
    , thuSizingPolicy
    , thuFormat
    , thuMaxHeight
    , thuResolution
    , thuPaddingPolicy
    , thuAspectRatio
    , thuInterval
    , thuMaxWidth

    -- * TimeSpan
    , TimeSpan
    , timeSpan
    , tsStartTime
    , tsDuration

    -- * Timing
    , Timing
    , timing
    , timSubmitTimeMillis
    , timFinishTimeMillis
    , timStartTimeMillis

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
    , warCode
    , warMessage
    ) where

import           Network.AWS.ElasticTranscoder.Types.Product
import           Network.AWS.ElasticTranscoder.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-09-25@ of the Amazon Elastic Transcoder SDK.
data ElasticTranscoder

instance AWSService ElasticTranscoder where
    type Sg ElasticTranscoder = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "ElasticTranscoder"
            , _svcPrefix = "elastictranscoder"
            , _svcVersion = "2012-09-25"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
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
_ValidationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ValidationException =
    _ServiceError . hasStatus 400 . hasCode "ValidationException"

-- | Prism for IncompatibleVersionException' errors.
_IncompatibleVersionException :: AWSError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersionException =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersionException"

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: AWSError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException =
    _ServiceError . hasStatus 403 . hasCode "AccessDeniedException"

-- | Elastic Transcoder encountered an unexpected exception while trying to
-- fulfill the request.
_InternalServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServiceException = _ServiceError . hasCode "InternalServiceException"

-- | The requested resource does not exist or is not available. For example,
-- the pipeline to which you\'re trying to add a job doesn\'t exist or is
-- still being created.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | The resource you are attempting to change is in use. For example, you
-- are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
    _ServiceError . hasStatus 409 . hasCode "ResourceInUseException"

-- | Too many operations for a given AWS account. For example, the number of
-- pipelines exceeds the maximum allowed.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 429 . hasCode "LimitExceededException"
