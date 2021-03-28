{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Elastic Transcoder Service__ 
--
-- The AWS Elastic Transcoder Service.
module Network.AWS.ElasticTranscoder
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** IncompatibleVersionException
    , _IncompatibleVersionException

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** JobComplete
    , mkJobComplete

    -- * Operations
    -- $operations

    -- ** DeletePreset 
    , module Network.AWS.ElasticTranscoder.DeletePreset

    -- ** UpdatePipelineStatus 
    , module Network.AWS.ElasticTranscoder.UpdatePipelineStatus

    -- ** ListJobsByPipeline (Paginated)
    , module Network.AWS.ElasticTranscoder.ListJobsByPipeline

    -- ** UpdatePipeline 
    , module Network.AWS.ElasticTranscoder.UpdatePipeline

    -- ** DeletePipeline 
    , module Network.AWS.ElasticTranscoder.DeletePipeline

    -- ** CreateJob 
    , module Network.AWS.ElasticTranscoder.CreateJob

    -- ** ListPipelines (Paginated)
    , module Network.AWS.ElasticTranscoder.ListPipelines

    -- ** CreatePreset 
    , module Network.AWS.ElasticTranscoder.CreatePreset

    -- ** ListPresets (Paginated)
    , module Network.AWS.ElasticTranscoder.ListPresets

    -- ** ReadPreset 
    , module Network.AWS.ElasticTranscoder.ReadPreset

    -- ** ReadJob 
    , module Network.AWS.ElasticTranscoder.ReadJob

    -- ** UpdatePipelineNotifications 
    , module Network.AWS.ElasticTranscoder.UpdatePipelineNotifications

    -- ** ReadPipeline 
    , module Network.AWS.ElasticTranscoder.ReadPipeline

    -- ** CreatePipeline 
    , module Network.AWS.ElasticTranscoder.CreatePipeline

    -- ** ListJobsByStatus (Paginated)
    , module Network.AWS.ElasticTranscoder.ListJobsByStatus

    -- ** CancelJob 
    , module Network.AWS.ElasticTranscoder.CancelJob

    -- * Types

    -- ** ThumbnailPattern
    , ThumbnailPattern (..)

    -- ** AudioBitDepth
    , AudioBitDepth (..)

    -- ** Digits
    , Digits (..)

    -- ** PlayReadyDrm
    , PlayReadyDrm (..)
    , mkPlayReadyDrm
    , prdFormat
    , prdInitializationVector
    , prdKey
    , prdKeyId
    , prdKeyMd5
    , prdLicenseAcquisitionUrl

    -- ** VideoBitRate
    , VideoBitRate (..)

    -- ** AudioSigned
    , AudioSigned (..)

    -- ** KeyframesMaxDist
    , KeyframesMaxDist (..)

    -- ** VideoCodec
    , VideoCodec (..)

    -- ** WatermarkKey
    , WatermarkKey (..)

    -- ** VerticalAlign
    , VerticalAlign (..)

    -- ** PipelineOutputConfig
    , PipelineOutputConfig (..)
    , mkPipelineOutputConfig
    , pocBucket
    , pocPermissions
    , pocStorageClass

    -- ** FrameRate
    , FrameRate (..)

    -- ** KeyArn
    , KeyArn (..)

    -- ** NonEmptyBase64EncodedString
    , NonEmptyBase64EncodedString (..)

    -- ** HlsContentProtectionMethod
    , HlsContentProtectionMethod (..)

    -- ** CodecOption
    , CodecOption (..)

    -- ** CaptionFormatPattern
    , CaptionFormatPattern (..)

    -- ** CreateJobPlaylist
    , CreateJobPlaylist (..)
    , mkCreateJobPlaylist
    , cjpFormat
    , cjpHlsContentProtection
    , cjpName
    , cjpOutputKeys
    , cjpPlayReadyDrm

    -- ** Captions
    , Captions (..)
    , mkCaptions
    , cCaptionFormats
    , cCaptionSources
    , cMergePolicy

    -- ** PresetType
    , PresetType (..)

    -- ** Time
    , Time (..)

    -- ** PlaylistFormat
    , PlaylistFormat (..)

    -- ** MergePolicy
    , MergePolicy (..)

    -- ** PresetWatermarkId
    , PresetWatermarkId (..)

    -- ** AudioCodecOptions
    , AudioCodecOptions (..)
    , mkAudioCodecOptions
    , acoBitDepth
    , acoBitOrder
    , acoProfile
    , acoSigned

    -- ** JobOutput
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

    -- ** TimeOffset
    , TimeOffset (..)

    -- ** SizingPolicy
    , SizingPolicy (..)

    -- ** MaxFrameRate
    , MaxFrameRate (..)

    -- ** CaptionSource
    , CaptionSource (..)
    , mkCaptionSource
    , csEncryption
    , csKey
    , csLabel
    , csLanguage
    , csTimeOffset

    -- ** AccessControl
    , AccessControl (..)

    -- ** CaptionFormatFormat
    , CaptionFormatFormat (..)

    -- ** GranteeType
    , GranteeType (..)

    -- ** Resolution
    , Resolution (..)

    -- ** Ascending
    , Ascending (..)

    -- ** KeyStoragePolicy
    , KeyStoragePolicy (..)

    -- ** CaptionMergePolicy
    , CaptionMergePolicy (..)

    -- ** Artwork
    , Artwork (..)
    , mkArtwork
    , aAlbumArtFormat
    , aEncryption
    , aInputKey
    , aMaxHeight
    , aMaxWidth
    , aPaddingPolicy
    , aSizingPolicy

    -- ** PipelineStatus
    , PipelineStatus (..)

    -- ** DigitsOrAuto
    , DigitsOrAuto (..)

    -- ** AspectRatio
    , AspectRatio (..)

    -- ** JpgOrPng
    , JpgOrPng (..)

    -- ** BucketName
    , BucketName (..)

    -- ** Opacity
    , Opacity (..)

    -- ** TimeSpan
    , TimeSpan (..)
    , mkTimeSpan
    , tsDuration
    , tsStartTime

    -- ** PaddingPolicy
    , PaddingPolicy (..)

    -- ** AudioPackingMode
    , AudioPackingMode (..)

    -- ** EncryptionMode
    , EncryptionMode (..)

    -- ** CreateJobOutput
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

    -- ** AudioParameters
    , AudioParameters (..)
    , mkAudioParameters
    , apAudioPackingMode
    , apBitRate
    , apChannels
    , apCodec
    , apCodecOptions
    , apSampleRate

    -- ** Role
    , Role (..)

    -- ** Thumbnails
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

    -- ** Encryption
    , Encryption (..)
    , mkEncryption
    , eInitializationVector
    , eKey
    , eKeyMd5
    , eMode

    -- ** JobAlbumArt
    , JobAlbumArt (..)
    , mkJobAlbumArt
    , jaaArtwork
    , jaaMergePolicy

    -- ** SnsTopic
    , SnsTopic (..)

    -- ** Job'
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

    -- ** Key
    , Key (..)

    -- ** DetectedProperties
    , DetectedProperties (..)
    , mkDetectedProperties
    , dpDurationMillis
    , dpFileSize
    , dpFrameRate
    , dpHeight
    , dpWidth

    -- ** StorageClass
    , StorageClass (..)

    -- ** Name
    , Name (..)

    -- ** JobWatermark
    , JobWatermark (..)
    , mkJobWatermark
    , jwEncryption
    , jwInputKey
    , jwPresetWatermarkId

    -- ** ZeroTo512String
    , ZeroTo512String (..)

    -- ** Pipeline
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

    -- ** Warning
    , Warning (..)
    , mkWarning
    , wCode
    , wMessage

    -- ** PresetContainer
    , PresetContainer (..)

    -- ** PixelsOrPercent
    , PixelsOrPercent (..)

    -- ** Preset
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

    -- ** Id
    , Id (..)

    -- ** CaptionFormat
    , CaptionFormat (..)
    , mkCaptionFormat
    , cfEncryption
    , cfFormat
    , cfPattern

    -- ** HlsContentProtection
    , HlsContentProtection (..)
    , mkHlsContentProtection
    , hcpInitializationVector
    , hcpKey
    , hcpKeyMd5
    , hcpKeyStoragePolicy
    , hcpLicenseAcquisitionUrl
    , hcpMethod

    -- ** LongKey
    , LongKey (..)

    -- ** PresetWatermark
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

    -- ** Base64EncodedString
    , Base64EncodedString (..)

    -- ** Timing
    , Timing (..)
    , mkTiming
    , tFinishTimeMillis
    , tStartTimeMillis
    , tSubmitTimeMillis

    -- ** JobContainer
    , JobContainer (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** Permission
    , Permission (..)
    , mkPermission
    , pAccess
    , pGrantee
    , pGranteeType

    -- ** Interlaced
    , Interlaced (..)

    -- ** HorizontalAlign
    , HorizontalAlign (..)

    -- ** InputCaptions
    , InputCaptions (..)
    , mkInputCaptions
    , icCaptionSources
    , icMergePolicy

    -- ** VideoParameters
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

    -- ** Grantee
    , Grantee (..)

    -- ** Description
    , Description (..)

    -- ** Playlist
    , Playlist (..)
    , mkPlaylist
    , pFormat
    , pHlsContentProtection
    , pName
    , pOutputKeys
    , pPlayReadyDrm
    , pStatus
    , pStatusDetail

    -- ** FixedGOP
    , FixedGOP (..)

    -- ** Notifications
    , Notifications (..)
    , mkNotifications
    , nCompleted
    , nError
    , nProgressing
    , nWarning

    -- ** Clip
    , Clip (..)
    , mkClip
    , cTimeSpan

    -- ** Rotate
    , Rotate (..)

    -- ** JobInput
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

    -- ** Filename
    , Filename (..)

    -- ** Target
    , Target (..)

    -- ** Format
    , Format (..)

    -- ** InitializationVector
    , InitializationVector (..)

    -- ** KeyId
    , KeyId (..)

    -- ** KeyMd5
    , KeyMd5 (..)

    -- ** LicenseAcquisitionUrl
    , LicenseAcquisitionUrl (..)

    -- ** NextPageToken
    , NextPageToken (..)

    -- ** Bucket
    , Bucket (..)

    -- ** AwsKmsKeyArn
    , AwsKmsKeyArn (..)

    -- ** InputBucket
    , InputBucket (..)

    -- ** PageToken
    , PageToken (..)

    -- ** PipelineId
    , PipelineId (..)

    -- ** OutputKeyPrefix
    , OutputKeyPrefix (..)

    -- ** BitOrder
    , BitOrder (..)

    -- ** Profile
    , Profile (..)

    -- ** PresetId
    , PresetId (..)

    -- ** SegmentDuration
    , SegmentDuration (..)

    -- ** Status
    , Status (..)

    -- ** StatusDetail
    , StatusDetail (..)

    -- ** Container
    , Container (..)

    -- ** Label
    , Label (..)

    -- ** Language
    , Language (..)

    -- ** AlbumArtFormat
    , AlbumArtFormat (..)

    -- ** MaxHeight
    , MaxHeight (..)

    -- ** MaxWidth
    , MaxWidth (..)

    -- ** BitRate
    , BitRate (..)

    -- ** Channels
    , Channels (..)

    -- ** Codec
    , Codec (..)

    -- ** SampleRate
    , SampleRate (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Waiters
import Network.AWS.ElasticTranscoder.DeletePreset
import Network.AWS.ElasticTranscoder.UpdatePipelineStatus
import Network.AWS.ElasticTranscoder.ListJobsByPipeline
import Network.AWS.ElasticTranscoder.UpdatePipeline
import Network.AWS.ElasticTranscoder.DeletePipeline
import Network.AWS.ElasticTranscoder.CreateJob
import Network.AWS.ElasticTranscoder.ListPipelines
import Network.AWS.ElasticTranscoder.CreatePreset
import Network.AWS.ElasticTranscoder.ListPresets
import Network.AWS.ElasticTranscoder.ReadPreset
import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
import Network.AWS.ElasticTranscoder.ReadPipeline
import Network.AWS.ElasticTranscoder.CreatePipeline
import Network.AWS.ElasticTranscoder.ListJobsByStatus
import Network.AWS.ElasticTranscoder.CancelJob
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElasticTranscoder'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
