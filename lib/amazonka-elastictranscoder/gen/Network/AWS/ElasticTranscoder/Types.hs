-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _IncompatibleVersionException
    , _ValidationException
    , _AccessDeniedException
    , _InternalServiceException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ThumbnailPattern
    , ThumbnailPattern (..)

    -- * AudioBitDepth
    , AudioBitDepth (..)

    -- * Digits
    , Digits (..)

    -- * PlayReadyDrm
    , PlayReadyDrm (..)
    , mkPlayReadyDrm
    , prdFormat
    , prdInitializationVector
    , prdKey
    , prdKeyId
    , prdKeyMd5
    , prdLicenseAcquisitionUrl

    -- * VideoBitRate
    , VideoBitRate (..)

    -- * AudioSigned
    , AudioSigned (..)

    -- * KeyframesMaxDist
    , KeyframesMaxDist (..)

    -- * VideoCodec
    , VideoCodec (..)

    -- * WatermarkKey
    , WatermarkKey (..)

    -- * VerticalAlign
    , VerticalAlign (..)

    -- * PipelineOutputConfig
    , PipelineOutputConfig (..)
    , mkPipelineOutputConfig
    , pocBucket
    , pocPermissions
    , pocStorageClass

    -- * FrameRate
    , FrameRate (..)

    -- * KeyArn
    , KeyArn (..)

    -- * NonEmptyBase64EncodedString
    , NonEmptyBase64EncodedString (..)

    -- * HlsContentProtectionMethod
    , HlsContentProtectionMethod (..)

    -- * CodecOption
    , CodecOption (..)

    -- * CaptionFormatPattern
    , CaptionFormatPattern (..)

    -- * CreateJobPlaylist
    , CreateJobPlaylist (..)
    , mkCreateJobPlaylist
    , cjpFormat
    , cjpHlsContentProtection
    , cjpName
    , cjpOutputKeys
    , cjpPlayReadyDrm

    -- * Captions
    , Captions (..)
    , mkCaptions
    , cCaptionFormats
    , cCaptionSources
    , cMergePolicy

    -- * PresetType
    , PresetType (..)

    -- * Time
    , Time (..)

    -- * PlaylistFormat
    , PlaylistFormat (..)

    -- * MergePolicy
    , MergePolicy (..)

    -- * PresetWatermarkId
    , PresetWatermarkId (..)

    -- * AudioCodecOptions
    , AudioCodecOptions (..)
    , mkAudioCodecOptions
    , acoBitDepth
    , acoBitOrder
    , acoProfile
    , acoSigned

    -- * JobOutput
    , JobOutput (..)
    , mkJobOutput
    , joAlbumArt
    , joAppliedColorSpaceConversion
    , joCaptions
    , joComposition
    , joDuration
    , joDurationMillis
    , joEncryption
    , joFileSize
    , joFrameRate
    , joHeight
    , joId
    , joKey
    , joPresetId
    , joRotate
    , joSegmentDuration
    , joStatus
    , joStatusDetail
    , joThumbnailEncryption
    , joThumbnailPattern
    , joWatermarks
    , joWidth

    -- * TimeOffset
    , TimeOffset (..)

    -- * SizingPolicy
    , SizingPolicy (..)

    -- * MaxFrameRate
    , MaxFrameRate (..)

    -- * CaptionSource
    , CaptionSource (..)
    , mkCaptionSource
    , csEncryption
    , csKey
    , csLabel
    , csLanguage
    , csTimeOffset

    -- * AccessControl
    , AccessControl (..)

    -- * CaptionFormatFormat
    , CaptionFormatFormat (..)

    -- * GranteeType
    , GranteeType (..)

    -- * Resolution
    , Resolution (..)

    -- * Ascending
    , Ascending (..)

    -- * KeyStoragePolicy
    , KeyStoragePolicy (..)

    -- * CaptionMergePolicy
    , CaptionMergePolicy (..)

    -- * Artwork
    , Artwork (..)
    , mkArtwork
    , aAlbumArtFormat
    , aEncryption
    , aInputKey
    , aMaxHeight
    , aMaxWidth
    , aPaddingPolicy
    , aSizingPolicy

    -- * PipelineStatus
    , PipelineStatus (..)

    -- * DigitsOrAuto
    , DigitsOrAuto (..)

    -- * AspectRatio
    , AspectRatio (..)

    -- * JpgOrPng
    , JpgOrPng (..)

    -- * BucketName
    , BucketName (..)

    -- * Opacity
    , Opacity (..)

    -- * TimeSpan
    , TimeSpan (..)
    , mkTimeSpan
    , tsDuration
    , tsStartTime

    -- * PaddingPolicy
    , PaddingPolicy (..)

    -- * AudioPackingMode
    , AudioPackingMode (..)

    -- * EncryptionMode
    , EncryptionMode (..)

    -- * CreateJobOutput
    , CreateJobOutput (..)
    , mkCreateJobOutput
    , cjoAlbumArt
    , cjoCaptions
    , cjoComposition
    , cjoEncryption
    , cjoKey
    , cjoPresetId
    , cjoRotate
    , cjoSegmentDuration
    , cjoThumbnailEncryption
    , cjoThumbnailPattern
    , cjoWatermarks

    -- * AudioParameters
    , AudioParameters (..)
    , mkAudioParameters
    , apAudioPackingMode
    , apBitRate
    , apChannels
    , apCodec
    , apCodecOptions
    , apSampleRate

    -- * Role
    , Role (..)

    -- * Thumbnails
    , Thumbnails (..)
    , mkThumbnails
    , tAspectRatio
    , tFormat
    , tInterval
    , tMaxHeight
    , tMaxWidth
    , tPaddingPolicy
    , tResolution
    , tSizingPolicy

    -- * Encryption
    , Encryption (..)
    , mkEncryption
    , eInitializationVector
    , eKey
    , eKeyMd5
    , eMode

    -- * JobAlbumArt
    , JobAlbumArt (..)
    , mkJobAlbumArt
    , jaaArtwork
    , jaaMergePolicy

    -- * SnsTopic
    , SnsTopic (..)

    -- * Job'
    , Job' (..)
    , mkJob'
    , jArn
    , jId
    , jInput
    , jInputs
    , jOutput
    , jOutputKeyPrefix
    , jOutputs
    , jPipelineId
    , jPlaylists
    , jStatus
    , jTiming
    , jUserMetadata

    -- * Key
    , Key (..)

    -- * DetectedProperties
    , DetectedProperties (..)
    , mkDetectedProperties
    , dpDurationMillis
    , dpFileSize
    , dpFrameRate
    , dpHeight
    , dpWidth

    -- * StorageClass
    , StorageClass (..)

    -- * Name
    , Name (..)

    -- * JobWatermark
    , JobWatermark (..)
    , mkJobWatermark
    , jwEncryption
    , jwInputKey
    , jwPresetWatermarkId

    -- * ZeroTo512String
    , ZeroTo512String (..)

    -- * Pipeline
    , Pipeline (..)
    , mkPipeline
    , pfArn
    , pfAwsKmsKeyArn
    , pfContentConfig
    , pfId
    , pfInputBucket
    , pfName
    , pfNotifications
    , pfOutputBucket
    , pfRole
    , pfStatus
    , pfThumbnailConfig

    -- * Warning
    , Warning (..)
    , mkWarning
    , wCode
    , wMessage

    -- * PresetContainer
    , PresetContainer (..)

    -- * PixelsOrPercent
    , PixelsOrPercent (..)

    -- * Preset
    , Preset (..)
    , mkPreset
    , pgArn
    , pgAudio
    , pgContainer
    , pgDescription
    , pgId
    , pgName
    , pgThumbnails
    , pgType
    , pgVideo

    -- * Id
    , Id (..)

    -- * CaptionFormat
    , CaptionFormat (..)
    , mkCaptionFormat
    , cfEncryption
    , cfFormat
    , cfPattern

    -- * HlsContentProtection
    , HlsContentProtection (..)
    , mkHlsContentProtection
    , hcpInitializationVector
    , hcpKey
    , hcpKeyMd5
    , hcpKeyStoragePolicy
    , hcpLicenseAcquisitionUrl
    , hcpMethod

    -- * LongKey
    , LongKey (..)

    -- * PresetWatermark
    , PresetWatermark (..)
    , mkPresetWatermark
    , pwHorizontalAlign
    , pwHorizontalOffset
    , pwId
    , pwMaxHeight
    , pwMaxWidth
    , pwOpacity
    , pwSizingPolicy
    , pwTarget
    , pwVerticalAlign
    , pwVerticalOffset

    -- * Base64EncodedString
    , Base64EncodedString (..)

    -- * Timing
    , Timing (..)
    , mkTiming
    , tFinishTimeMillis
    , tStartTimeMillis
    , tSubmitTimeMillis

    -- * JobContainer
    , JobContainer (..)

    -- * JobStatus
    , JobStatus (..)

    -- * Permission
    , Permission (..)
    , mkPermission
    , pAccess
    , pGrantee
    , pGranteeType

    -- * Interlaced
    , Interlaced (..)

    -- * HorizontalAlign
    , HorizontalAlign (..)

    -- * InputCaptions
    , InputCaptions (..)
    , mkInputCaptions
    , icCaptionSources
    , icMergePolicy

    -- * VideoParameters
    , VideoParameters (..)
    , mkVideoParameters
    , vpAspectRatio
    , vpBitRate
    , vpCodec
    , vpCodecOptions
    , vpDisplayAspectRatio
    , vpFixedGOP
    , vpFrameRate
    , vpKeyframesMaxDist
    , vpMaxFrameRate
    , vpMaxHeight
    , vpMaxWidth
    , vpPaddingPolicy
    , vpResolution
    , vpSizingPolicy
    , vpWatermarks

    -- * Grantee
    , Grantee (..)

    -- * Description
    , Description (..)

    -- * Playlist
    , Playlist (..)
    , mkPlaylist
    , pFormat
    , pHlsContentProtection
    , pName
    , pOutputKeys
    , pPlayReadyDrm
    , pStatus
    , pStatusDetail

    -- * FixedGOP
    , FixedGOP (..)

    -- * Notifications
    , Notifications (..)
    , mkNotifications
    , nCompleted
    , nError
    , nProgressing
    , nWarning

    -- * Clip
    , Clip (..)
    , mkClip
    , cTimeSpan

    -- * Rotate
    , Rotate (..)

    -- * JobInput
    , JobInput (..)
    , mkJobInput
    , jiAspectRatio
    , jiContainer
    , jiDetectedProperties
    , jiEncryption
    , jiFrameRate
    , jiInputCaptions
    , jiInterlaced
    , jiKey
    , jiResolution
    , jiTimeSpan

    -- * Filename
    , Filename (..)

    -- * Target
    , Target (..)

    -- * Format
    , Format (..)

    -- * InitializationVector
    , InitializationVector (..)

    -- * KeyId
    , KeyId (..)

    -- * KeyMd5
    , KeyMd5 (..)

    -- * LicenseAcquisitionUrl
    , LicenseAcquisitionUrl (..)

    -- * NextPageToken
    , NextPageToken (..)

    -- * Bucket
    , Bucket (..)

    -- * AwsKmsKeyArn
    , AwsKmsKeyArn (..)

    -- * InputBucket
    , InputBucket (..)

    -- * PageToken
    , PageToken (..)

    -- * PipelineId
    , PipelineId (..)

    -- * OutputKeyPrefix
    , OutputKeyPrefix (..)

    -- * BitOrder
    , BitOrder (..)

    -- * Profile
    , Profile (..)

    -- * PresetId
    , PresetId (..)

    -- * SegmentDuration
    , SegmentDuration (..)

    -- * Status
    , Status (..)

    -- * StatusDetail
    , StatusDetail (..)

    -- * Container
    , Container (..)

    -- * Label
    , Label (..)

    -- * Language
    , Language (..)

    -- * AlbumArtFormat
    , AlbumArtFormat (..)

    -- * MaxHeight
    , MaxHeight (..)

    -- * MaxWidth
    , MaxWidth (..)

    -- * BitRate
    , BitRate (..)

    -- * Channels
    , Channels (..)

    -- * Codec
    , Codec (..)

    -- * SampleRate
    , SampleRate (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ElasticTranscoder.Types.ThumbnailPattern
  
import Network.AWS.ElasticTranscoder.Types.AudioBitDepth
  
  
import Network.AWS.ElasticTranscoder.Types.Digits
  
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
  
  
import Network.AWS.ElasticTranscoder.Types.VideoBitRate
  
import Network.AWS.ElasticTranscoder.Types.AudioSigned
  
  
import Network.AWS.ElasticTranscoder.Types.KeyframesMaxDist
  
import Network.AWS.ElasticTranscoder.Types.VideoCodec
  
import Network.AWS.ElasticTranscoder.Types.WatermarkKey
  
import Network.AWS.ElasticTranscoder.Types.VerticalAlign
  
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
  
import Network.AWS.ElasticTranscoder.Types.FrameRate
  
import Network.AWS.ElasticTranscoder.Types.KeyArn
  
import Network.AWS.ElasticTranscoder.Types.NonEmptyBase64EncodedString
  
import Network.AWS.ElasticTranscoder.Types.HlsContentProtectionMethod
  
import Network.AWS.ElasticTranscoder.Types.CodecOption
  
import Network.AWS.ElasticTranscoder.Types.CaptionFormatPattern
  
import Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
  
import Network.AWS.ElasticTranscoder.Types.Captions
  
import Network.AWS.ElasticTranscoder.Types.PresetType
  
import Network.AWS.ElasticTranscoder.Types.Time
  
import Network.AWS.ElasticTranscoder.Types.PlaylistFormat
  
import Network.AWS.ElasticTranscoder.Types.MergePolicy
  
import Network.AWS.ElasticTranscoder.Types.PresetWatermarkId
  
import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
  
import Network.AWS.ElasticTranscoder.Types.JobOutput
  
import Network.AWS.ElasticTranscoder.Types.TimeOffset
  
import Network.AWS.ElasticTranscoder.Types.SizingPolicy
  
import Network.AWS.ElasticTranscoder.Types.MaxFrameRate
  
import Network.AWS.ElasticTranscoder.Types.CaptionSource
  
import Network.AWS.ElasticTranscoder.Types.AccessControl
  
import Network.AWS.ElasticTranscoder.Types.CaptionFormatFormat
  
import Network.AWS.ElasticTranscoder.Types.GranteeType
  
import Network.AWS.ElasticTranscoder.Types.Resolution
  
import Network.AWS.ElasticTranscoder.Types.Ascending
  
import Network.AWS.ElasticTranscoder.Types.KeyStoragePolicy
  
import Network.AWS.ElasticTranscoder.Types.CaptionMergePolicy
  
import Network.AWS.ElasticTranscoder.Types.Artwork
  
import Network.AWS.ElasticTranscoder.Types.PipelineStatus
  
import Network.AWS.ElasticTranscoder.Types.DigitsOrAuto
  
import Network.AWS.ElasticTranscoder.Types.AspectRatio
  
import Network.AWS.ElasticTranscoder.Types.JpgOrPng
  
import Network.AWS.ElasticTranscoder.Types.BucketName
  
import Network.AWS.ElasticTranscoder.Types.Opacity
  
import Network.AWS.ElasticTranscoder.Types.TimeSpan
  
import Network.AWS.ElasticTranscoder.Types.PaddingPolicy
  
import Network.AWS.ElasticTranscoder.Types.AudioPackingMode
  
import Network.AWS.ElasticTranscoder.Types.EncryptionMode
  
import Network.AWS.ElasticTranscoder.Types.CreateJobOutput
  
import Network.AWS.ElasticTranscoder.Types.AudioParameters
  
import Network.AWS.ElasticTranscoder.Types.Role
  
import Network.AWS.ElasticTranscoder.Types.Thumbnails
  
import Network.AWS.ElasticTranscoder.Types.Encryption
  
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
  
import Network.AWS.ElasticTranscoder.Types.SnsTopic
  
import Network.AWS.ElasticTranscoder.Types.Job'
  
import Network.AWS.ElasticTranscoder.Types.Key
  
import Network.AWS.ElasticTranscoder.Types.DetectedProperties
  
import Network.AWS.ElasticTranscoder.Types.StorageClass
  
import Network.AWS.ElasticTranscoder.Types.Name
  
import Network.AWS.ElasticTranscoder.Types.JobWatermark
  
import Network.AWS.ElasticTranscoder.Types.ZeroTo512String
  
import Network.AWS.ElasticTranscoder.Types.Pipeline
  
import Network.AWS.ElasticTranscoder.Types.Warning
  
import Network.AWS.ElasticTranscoder.Types.PresetContainer
  
import Network.AWS.ElasticTranscoder.Types.PixelsOrPercent
  
import Network.AWS.ElasticTranscoder.Types.Preset
  
import Network.AWS.ElasticTranscoder.Types.Id
  
import Network.AWS.ElasticTranscoder.Types.CaptionFormat
  
import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
  
import Network.AWS.ElasticTranscoder.Types.LongKey
  
  
import Network.AWS.ElasticTranscoder.Types.PresetWatermark
  
import Network.AWS.ElasticTranscoder.Types.Base64EncodedString
  
import Network.AWS.ElasticTranscoder.Types.Timing
  
import Network.AWS.ElasticTranscoder.Types.JobContainer
  
import Network.AWS.ElasticTranscoder.Types.JobStatus
  
import Network.AWS.ElasticTranscoder.Types.Permission
  
import Network.AWS.ElasticTranscoder.Types.Interlaced
  
import Network.AWS.ElasticTranscoder.Types.HorizontalAlign
  
import Network.AWS.ElasticTranscoder.Types.InputCaptions
  
import Network.AWS.ElasticTranscoder.Types.VideoParameters
  
import Network.AWS.ElasticTranscoder.Types.Grantee
  
import Network.AWS.ElasticTranscoder.Types.Description
  
import Network.AWS.ElasticTranscoder.Types.Playlist
  
import Network.AWS.ElasticTranscoder.Types.FixedGOP
  
import Network.AWS.ElasticTranscoder.Types.Notifications
  
import Network.AWS.ElasticTranscoder.Types.Clip
  
  
import Network.AWS.ElasticTranscoder.Types.Rotate
  
import Network.AWS.ElasticTranscoder.Types.JobInput
  
import Network.AWS.ElasticTranscoder.Types.Filename
  
  
  
import Network.AWS.ElasticTranscoder.Types.Target
  
import Network.AWS.ElasticTranscoder.Types.Format
  
import Network.AWS.ElasticTranscoder.Types.InitializationVector
  
import Network.AWS.ElasticTranscoder.Types.KeyId
  
import Network.AWS.ElasticTranscoder.Types.KeyMd5
  
import Network.AWS.ElasticTranscoder.Types.LicenseAcquisitionUrl
  
import Network.AWS.ElasticTranscoder.Types.NextPageToken
  
import Network.AWS.ElasticTranscoder.Types.Bucket
  
import Network.AWS.ElasticTranscoder.Types.AwsKmsKeyArn
  
import Network.AWS.ElasticTranscoder.Types.InputBucket
  
import Network.AWS.ElasticTranscoder.Types.PageToken
  
import Network.AWS.ElasticTranscoder.Types.PipelineId
  
import Network.AWS.ElasticTranscoder.Types.OutputKeyPrefix
  
import Network.AWS.ElasticTranscoder.Types.BitOrder
  
import Network.AWS.ElasticTranscoder.Types.Profile
  
import Network.AWS.ElasticTranscoder.Types.PresetId
  
import Network.AWS.ElasticTranscoder.Types.SegmentDuration
  
import Network.AWS.ElasticTranscoder.Types.Status
  
import Network.AWS.ElasticTranscoder.Types.StatusDetail
  
import Network.AWS.ElasticTranscoder.Types.Container
  
import Network.AWS.ElasticTranscoder.Types.Label
  
import Network.AWS.ElasticTranscoder.Types.Language
  
import Network.AWS.ElasticTranscoder.Types.AlbumArtFormat
  
import Network.AWS.ElasticTranscoder.Types.MaxHeight
  
import Network.AWS.ElasticTranscoder.Types.MaxWidth
  
import Network.AWS.ElasticTranscoder.Types.BitRate
  
import Network.AWS.ElasticTranscoder.Types.Channels
  
import Network.AWS.ElasticTranscoder.Types.Codec
  
import Network.AWS.ElasticTranscoder.Types.SampleRate
  

-- | API version @2012-09-25@ of the Amazon Elastic Transcoder SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ElasticTranscoder",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "elastictranscoder",
                 Core._svcVersion = "2012-09-25", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ElasticTranscoder",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Prism for 'IncompatibleVersionException' errors.
_IncompatibleVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleVersionException
  = Core._MatchServiceError mkServiceConfig
      "IncompatibleVersionException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IncompatibleVersionException #-}
{-# DEPRECATED _IncompatibleVersionException "Use generic-lens or generic-optics instead"  #-}

-- | One or more required parameter values were not provided in the request.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | General authentication failure. The request was not signed correctly.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | Elastic Transcoder encountered an unexpected exception while trying to fulfill the request.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The requested resource does not exist or is not available. For example, the pipeline to which you're trying to add a job doesn't exist or is still being created.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Too many operations for a given AWS account. For example, the number of pipelines exceeds the maximum allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The resource you are attempting to change is in use. For example, you are attempting to delete a pipeline that is currently in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
