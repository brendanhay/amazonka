{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Elastic Transcoder Service__
--
-- The AWS Elastic Transcoder Service.
--
module Network.AWS.ElasticTranscoder
    (
    -- * Service Configuration
      elasticTranscoder

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
    , jobComplete

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

    -- ** Artwork
    , Artwork
    , artwork
    , aSizingPolicy
    , aAlbumArtFormat
    , aMaxHeight
    , aInputKey
    , aPaddingPolicy
    , aEncryption
    , aMaxWidth

    -- ** AudioCodecOptions
    , AudioCodecOptions
    , audioCodecOptions
    , acoSigned
    , acoBitDepth
    , acoProfile
    , acoBitOrder

    -- ** AudioParameters
    , AudioParameters
    , audioParameters
    , apChannels
    , apCodec
    , apAudioPackingMode
    , apSampleRate
    , apBitRate
    , apCodecOptions

    -- ** CaptionFormat
    , CaptionFormat
    , captionFormat
    , cfPattern
    , cfFormat
    , cfEncryption

    -- ** CaptionSource
    , CaptionSource
    , captionSource
    , csTimeOffset
    , csEncryption
    , csKey
    , csLanguage
    , csLabel

    -- ** Captions
    , Captions
    , captions
    , cMergePolicy
    , cCaptionSources
    , cCaptionFormats

    -- ** Clip
    , Clip
    , clip
    , cTimeSpan

    -- ** CreateJobOutput
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

    -- ** CreateJobPlaylist
    , CreateJobPlaylist
    , createJobPlaylist
    , cjpPlayReadyDrm
    , cjpFormat
    , cjpOutputKeys
    , cjpName
    , cjpHlsContentProtection

    -- ** DetectedProperties
    , DetectedProperties
    , detectedProperties
    , dpHeight
    , dpFrameRate
    , dpFileSize
    , dpWidth
    , dpDurationMillis

    -- ** Encryption
    , Encryption
    , encryption
    , eMode
    , eKeyMD5
    , eKey
    , eInitializationVector

    -- ** HlsContentProtection
    , HlsContentProtection
    , hlsContentProtection
    , hcpKeyMD5
    , hcpKeyStoragePolicy
    , hcpKey
    , hcpMethod
    , hcpInitializationVector
    , hcpLicenseAcquisitionURL

    -- ** InputCaptions
    , InputCaptions
    , inputCaptions
    , icMergePolicy
    , icCaptionSources

    -- ** Job'
    , Job'
    , job'
    , jStatus
    , jPipelineId
    , jARN
    , jInputs
    , jInput
    , jUserMetadata
    , jOutputs
    , jOutput
    , jId
    , jPlaylists
    , jOutputKeyPrefix
    , jTiming

    -- ** JobAlbumArt
    , JobAlbumArt
    , jobAlbumArt
    , jaaMergePolicy
    , jaaArtwork

    -- ** JobInput
    , JobInput
    , jobInput
    , jiFrameRate
    , jiResolution
    , jiAspectRatio
    , jiTimeSpan
    , jiEncryption
    , jiKey
    , jiDetectedProperties
    , jiContainer
    , jiInterlaced
    , jiInputCaptions

    -- ** JobOutput
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

    -- ** JobWatermark
    , JobWatermark
    , jobWatermark
    , jwPresetWatermarkId
    , jwInputKey
    , jwEncryption

    -- ** Notifications
    , Notifications
    , notifications
    , nError
    , nWarning
    , nProgressing
    , nCompleted

    -- ** Permission
    , Permission
    , permission
    , pAccess
    , pGranteeType
    , pGrantee

    -- ** Pipeline
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

    -- ** PipelineOutputConfig
    , PipelineOutputConfig
    , pipelineOutputConfig
    , pocBucket
    , pocStorageClass
    , pocPermissions

    -- ** PlayReadyDrm
    , PlayReadyDrm
    , playReadyDrm
    , prdKeyId
    , prdFormat
    , prdKeyMD5
    , prdKey
    , prdInitializationVector
    , prdLicenseAcquisitionURL

    -- ** Playlist
    , Playlist
    , playlist
    , pStatus
    , pPlayReadyDrm
    , pFormat
    , pOutputKeys
    , pName
    , pStatusDetail
    , pHlsContentProtection

    -- ** Preset
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

    -- ** PresetWatermark
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

    -- ** Thumbnails
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

    -- ** TimeSpan
    , TimeSpan
    , timeSpan
    , tsStartTime
    , tsDuration

    -- ** Timing
    , Timing
    , timing
    , tSubmitTimeMillis
    , tFinishTimeMillis
    , tStartTimeMillis

    -- ** VideoParameters
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

    -- ** Warning
    , Warning
    , warning
    , wCode
    , wMessage
    ) where

import Network.AWS.ElasticTranscoder.CancelJob
import Network.AWS.ElasticTranscoder.CreateJob
import Network.AWS.ElasticTranscoder.CreatePipeline
import Network.AWS.ElasticTranscoder.CreatePreset
import Network.AWS.ElasticTranscoder.DeletePipeline
import Network.AWS.ElasticTranscoder.DeletePreset
import Network.AWS.ElasticTranscoder.ListJobsByPipeline
import Network.AWS.ElasticTranscoder.ListJobsByStatus
import Network.AWS.ElasticTranscoder.ListPipelines
import Network.AWS.ElasticTranscoder.ListPresets
import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.ReadPipeline
import Network.AWS.ElasticTranscoder.ReadPreset
import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.UpdatePipeline
import Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
import Network.AWS.ElasticTranscoder.UpdatePipelineStatus
import Network.AWS.ElasticTranscoder.Waiters

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
