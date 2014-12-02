{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ElasticTranscoder.Types
    (
    -- * Service
      ElasticTranscoder
    -- ** Error
    , JSONError

    -- * PipelineOutputConfig
    , PipelineOutputConfig
    , pipelineOutputConfig
    , pocBucket
    , pocPermissions
    , pocStorageClass

    -- * CreateJobPlaylist
    , CreateJobPlaylist
    , createJobPlaylist
    , cjpFormat
    , cjpName
    , cjpOutputKeys

    -- * Captions
    , Captions
    , captions
    , cCaptionFormats
    , cCaptionSources
    , cMergePolicy

    -- * AudioCodecOptions
    , AudioCodecOptions
    , audioCodecOptions
    , acoProfile

    -- * JobOutput
    , JobOutput
    , jobOutput
    , joAlbumArt
    , joCaptions
    , joComposition
    , joDuration
    , joEncryption
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

    -- * Job'
    , Job'
    , job
    , jArn
    , jId
    , jInput
    , jOutput
    , jOutputKeyPrefix
    , jOutputs
    , jPipelineId
    , jPlaylists
    , jStatus

    -- * CaptionSource
    , CaptionSource
    , captionSource
    , csEncryption
    , csKey
    , csLabel
    , csLanguage
    , csTimeOffset

    -- * Artwork
    , Artwork
    , artwork
    , aAlbumArtFormat
    , aEncryption
    , aInputKey
    , aMaxHeight
    , aMaxWidth
    , aPaddingPolicy
    , aSizingPolicy

    -- * TimeSpan
    , TimeSpan
    , timeSpan
    , tsDuration
    , tsStartTime

    -- * CreateJobOutput
    , CreateJobOutput
    , createJobOutput
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
    , AudioParameters
    , audioParameters
    , apBitRate
    , apChannels
    , apCodec
    , apCodecOptions
    , apSampleRate

    -- * Thumbnails
    , Thumbnails
    , thumbnails
    , tAspectRatio
    , tFormat
    , tInterval
    , tMaxHeight
    , tMaxWidth
    , tPaddingPolicy
    , tResolution
    , tSizingPolicy

    -- * Encryption
    , Encryption
    , encryption
    , eInitializationVector
    , eKey
    , eKeyMd5
    , eMode

    -- * JobAlbumArt
    , JobAlbumArt
    , jobAlbumArt
    , jaaArtwork
    , jaaMergePolicy

    -- * JobWatermark
    , JobWatermark
    , jobWatermark
    , jwEncryption
    , jwInputKey
    , jwPresetWatermarkId

    -- * Pipeline
    , Pipeline
    , pipeline
    , pArn
    , pAwsKmsKeyArn
    , pContentConfig
    , pId
    , pInputBucket
    , pName
    , pNotifications
    , pOutputBucket
    , pRole
    , pStatus
    , pThumbnailConfig

    -- * Preset
    , Preset
    , preset
    , p1Arn
    , p1Audio
    , p1Container
    , p1Description
    , p1Id
    , p1Name
    , p1Thumbnails
    , p1Type
    , p1Video

    -- * CaptionFormat
    , CaptionFormat
    , captionFormat
    , cfEncryption
    , cfFormat
    , cfPattern

    -- * PresetWatermark
    , PresetWatermark
    , presetWatermark
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

    -- * Permission
    , Permission
    , permission
    , pAccess
    , pGrantee
    , pGranteeType

    -- * VideoParameters
    , VideoParameters
    , videoParameters
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

    -- * Playlist
    , Playlist
    , playlist
    , p2Format
    , p2Name
    , p2OutputKeys
    , p2Status
    , p2StatusDetail

    -- * Notifications
    , Notifications
    , notifications
    , nCompleted
    , nError
    , nProgressing
    , nWarning

    -- * Clip
    , Clip
    , clip
    , cTimeSpan

    -- * JobInput
    , JobInput
    , jobInput
    , jiAspectRatio
    , jiContainer
    , jiEncryption
    , jiFrameRate
    , jiInterlaced
    , jiKey
    , jiResolution
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-09-25@ of the Amazon Elastic Transcoder service.
data ElasticTranscoder

instance AWSService ElasticTranscoder where
    type Sg ElasticTranscoder = V4
    type Er ElasticTranscoder = JSONError

    service = service'
      where
        service' :: Service ElasticTranscoder
        service' = Service
              { _svcAbbrev       = "ElasticTranscoder"
              , _svcPrefix       = "elastictranscoder"
              , _svcVersion      = "2012-09-25"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcDelay        = Exp 0.05 2 5
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Status
              -> JSONError
              -> Bool
        retry (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | s == 400  = "ThrottlingException" == e -- Throttling
            | otherwise = False

data PipelineOutputConfig = PipelineOutputConfig
    { _pocBucket       :: Maybe Text
    , _pocPermissions  :: List "Permissions" Permission
    , _pocStorageClass :: Maybe Text
    } deriving (Eq, Show)

-- | 'PipelineOutputConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pocBucket' @::@ 'Maybe' 'Text'
--
-- * 'pocPermissions' @::@ ['Permission']
--
-- * 'pocStorageClass' @::@ 'Maybe' 'Text'
--
pipelineOutputConfig :: PipelineOutputConfig
pipelineOutputConfig = PipelineOutputConfig
    { _pocBucket       = Nothing
    , _pocStorageClass = Nothing
    , _pocPermissions  = mempty
    }

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. Specify this value when all of the following are true:  You
-- want to save transcoded files, thumbnails (if any), and playlists (if any)
-- together in one bucket. You do not want to specify the users or groups who
-- have access to the transcoded files, thumbnails, and playlists. You do not
-- want to specify the permissions that Elastic Transcoder grants to the files.
-- You want to associate the transcoded files and thumbnails with the Amazon S3
-- Standard storage class.  If you want to save transcoded files and playlists
-- in one bucket and thumbnails in another bucket, specify which users can
-- access the transcoded files or the permissions the users have, or change the
-- Amazon S3 storage class, omit OutputBucket and specify values for 'ContentConfig' and 'ThumbnailConfig' instead.
pocBucket :: Lens' PipelineOutputConfig (Maybe Text)
pocBucket = lens _pocBucket (\s a -> s { _pocBucket = a })

-- | Optional. The 'Permissions' object specifies which users and/or predefined
-- Amazon S3 groups you want to have access to transcoded files and playlists,
-- and the type of access you want them to have. You can grant permissions to a
-- maximum of 30 users and/or predefined Amazon S3 groups.
--
-- If you include 'Permissions', Elastic Transcoder grants only the permissions
-- that you specify. It does not grant full permissions to the owner of the role
-- specified by 'Role'. If you want that user to have full control, you must
-- explicitly grant full control to the user.
--
-- If you omit 'Permissions', Elastic Transcoder grants full control over the
-- transcoded files and playlists to the owner of the role specified by 'Role',
-- and grants no other permissions to any other user or group.
pocPermissions :: Lens' PipelineOutputConfig [Permission]
pocPermissions = lens _pocPermissions (\s a -> s { _pocPermissions = a }) . _List

-- | The Amazon S3 storage class, 'Standard' or 'ReducedRedundancy', that you want
-- Elastic Transcoder to assign to the video files and playlists that it stores
-- in your Amazon S3 bucket.
pocStorageClass :: Lens' PipelineOutputConfig (Maybe Text)
pocStorageClass = lens _pocStorageClass (\s a -> s { _pocStorageClass = a })

instance FromJSON PipelineOutputConfig where
    parseJSON = withObject "PipelineOutputConfig" $ \o -> PipelineOutputConfig
        <$> o .:? "Bucket"
        <*> o .:? "Permissions" .!= mempty
        <*> o .:? "StorageClass"

instance ToJSON PipelineOutputConfig where
    toJSON PipelineOutputConfig{..} = object
        [ "Bucket"       .= _pocBucket
        , "StorageClass" .= _pocStorageClass
        , "Permissions"  .= _pocPermissions
        ]

data CreateJobPlaylist = CreateJobPlaylist
    { _cjpFormat     :: Maybe Text
    , _cjpName       :: Maybe Text
    , _cjpOutputKeys :: List "OutputKeys" Text
    } deriving (Eq, Ord, Show)

-- | 'CreateJobPlaylist' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjpFormat' @::@ 'Maybe' 'Text'
--
-- * 'cjpName' @::@ 'Maybe' 'Text'
--
-- * 'cjpOutputKeys' @::@ ['Text']
--
createJobPlaylist :: CreateJobPlaylist
createJobPlaylist = CreateJobPlaylist
    { _cjpName       = Nothing
    , _cjpFormat     = Nothing
    , _cjpOutputKeys = mempty
    }

-- | The format of the output playlist. Valid formats include 'HLSv3', 'HLSv4', and 'Smooth'.
cjpFormat :: Lens' CreateJobPlaylist (Maybe Text)
cjpFormat = lens _cjpFormat (\s a -> s { _cjpFormat = a })

-- | The name that you want Elastic Transcoder to assign to the master playlist,
-- for example, nyc-vacation.m3u8. If the name includes a '/' character, the
-- section of the name before the last '/' must be identical for all 'Name' objects.
-- If you create more than one master playlist, the values of all 'Name' objects
-- must be unique.
--
-- Note: Elastic Transcoder automatically appends the relevant file extension
-- to the file name ('.m3u8' for 'HLSv3' and 'HLSv4' playlists, and '.ism' and '.ismc' for 'Smooth' playlists). If you include a file extension in 'Name', the file name
-- will have two extensions.
cjpName :: Lens' CreateJobPlaylist (Maybe Text)
cjpName = lens _cjpName (\s a -> s { _cjpName = a })

-- | For each output in this job that you want to include in a master playlist,
-- the value of the 'Outputs:Key' object.
--
-- If your output is not 'HLS' or does not have a segment duration set, the
-- name of the output file is a concatenation of 'OutputKeyPrefix' and 'Outputs:Key':
--
-- OutputKeyPrefix'Outputs:Key'
--
-- If your output is 'HLSv3' and has a segment duration set, or is not included
-- in a playlist, Elastic Transcoder creates an output playlist file with a file
-- extension of '.m3u8', and a series of '.ts' files that include a five-digit
-- sequential counter beginning with 00000:
--
-- OutputKeyPrefix'Outputs:Key'.m3u8
--
-- OutputKeyPrefix'Outputs:Key'00000.ts
--
-- If your output is 'HLSv4', has a segment duration set, and is included in an 'HLSv4' playlist, Elastic Transcoder creates an output playlist file with a
-- file extension of '_v4.m3u8'. If the output is video, Elastic Transcoder also
-- creates an output file with an extension of '_iframe.m3u8':
--
-- OutputKeyPrefix'Outputs:Key'_v4.m3u8
--
-- OutputKeyPrefix'Outputs:Key'_iframe.m3u8
--
-- OutputKeyPrefix'Outputs:Key'.ts
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name. If you include a file extension in Output Key, the file name
-- will have two extensions.
--
-- If you include more than one output in a playlist, any segment duration
-- settings, clip settings, or caption settings must be the same for all outputs
-- in the playlist. For 'Smooth' playlists, the 'Audio:Profile', 'Video:Profile', and 'Video:FrameRate' to 'Video:KeyframesMaxDist' ratio must be the same for all outputs.
cjpOutputKeys :: Lens' CreateJobPlaylist [Text]
cjpOutputKeys = lens _cjpOutputKeys (\s a -> s { _cjpOutputKeys = a }) . _List

instance FromJSON CreateJobPlaylist where
    parseJSON = withObject "CreateJobPlaylist" $ \o -> CreateJobPlaylist
        <$> o .:? "Format"
        <*> o .:? "Name"
        <*> o .:? "OutputKeys" .!= mempty

instance ToJSON CreateJobPlaylist where
    toJSON CreateJobPlaylist{..} = object
        [ "Name"       .= _cjpName
        , "Format"     .= _cjpFormat
        , "OutputKeys" .= _cjpOutputKeys
        ]

data Captions = Captions
    { _cCaptionFormats :: List "CaptionFormats" CaptionFormat
    , _cCaptionSources :: List "CaptionSources" CaptionSource
    , _cMergePolicy    :: Maybe Text
    } deriving (Eq, Show)

-- | 'Captions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cCaptionFormats' @::@ ['CaptionFormat']
--
-- * 'cCaptionSources' @::@ ['CaptionSource']
--
-- * 'cMergePolicy' @::@ 'Maybe' 'Text'
--
captions :: Captions
captions = Captions
    { _cMergePolicy    = Nothing
    , _cCaptionSources = mempty
    , _cCaptionFormats = mempty
    }

-- | The array of file formats for the output captions. If you leave this value
-- blank, Elastic Transcoder returns an error.
cCaptionFormats :: Lens' Captions [CaptionFormat]
cCaptionFormats = lens _cCaptionFormats (\s a -> s { _cCaptionFormats = a }) . _List

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave 'CaptionSources' blank.
cCaptionSources :: Lens' Captions [CaptionSource]
cCaptionSources = lens _cCaptionSources (\s a -> s { _cCaptionSources = a }) . _List

-- | A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions.
--
-- MergeOverride: Elastic Transcoder transcodes both embedded and sidecar
-- captions into outputs. If captions for a language are embedded in the input
-- file and also appear in a sidecar file, Elastic Transcoder uses the sidecar
-- captions and ignores the embedded captions for that language.
--
-- MergeRetain: Elastic Transcoder transcodes both embedded and sidecar
-- captions into outputs. If captions for a language are embedded in the input
-- file and also appear in a sidecar file, Elastic Transcoder uses the embedded
-- captions and ignores the sidecar captions for that language. If 'CaptionSources'
-- is empty, Elastic Transcoder omits all sidecar captions from the output
-- files.
--
-- Override: Elastic Transcoder transcodes only the sidecar captions that you
-- specify in 'CaptionSources'.
--
-- 'MergePolicy' cannot be null.
cMergePolicy :: Lens' Captions (Maybe Text)
cMergePolicy = lens _cMergePolicy (\s a -> s { _cMergePolicy = a })

instance FromJSON Captions where
    parseJSON = withObject "Captions" $ \o -> Captions
        <$> o .:? "CaptionFormats" .!= mempty
        <*> o .:? "CaptionSources" .!= mempty
        <*> o .:? "MergePolicy"

instance ToJSON Captions where
    toJSON Captions{..} = object
        [ "MergePolicy"    .= _cMergePolicy
        , "CaptionSources" .= _cCaptionSources
        , "CaptionFormats" .= _cCaptionFormats
        ]

newtype AudioCodecOptions = AudioCodecOptions
    { _acoProfile :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'AudioCodecOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoProfile' @::@ 'Maybe' 'Text'
--
audioCodecOptions :: AudioCodecOptions
audioCodecOptions = AudioCodecOptions
    { _acoProfile = Nothing
    }

-- | You can only choose an audio profile when you specify AAC for the value of
-- Audio:Codec.
--
-- Specify the AAC profile for the output file. Elastic Transcoder supports the
-- following profiles:
--
-- 'auto': If you specify 'auto', Elastic Transcoder will select the profile
-- based on the bit rate selected for the output file.  'AAC-LC': The most common
-- AAC profile. Use for bit rates larger than 64 kbps.  'HE-AAC': Not supported on
-- some older players and devices. Use for bit rates between 40 and 80 kbps.  'HE-AACv2': Not supported on some players and devices. Use for bit rates less than 48
-- kbps.  All outputs in a 'Smooth' playlist must have the same value for 'Profile'.
--
-- If you created any presets before AAC profiles were added, Elastic
-- Transcoder automatically updated your presets to use AAC-LC. You can change
-- the value as required.
--
acoProfile :: Lens' AudioCodecOptions (Maybe Text)
acoProfile = lens _acoProfile (\s a -> s { _acoProfile = a })

instance FromJSON AudioCodecOptions where
    parseJSON = withObject "AudioCodecOptions" $ \o -> AudioCodecOptions
        <$> o .:? "Profile"

instance ToJSON AudioCodecOptions where
    toJSON AudioCodecOptions{..} = object
        [ "Profile" .= _acoProfile
        ]

data JobOutput = JobOutput
    { _joAlbumArt            :: Maybe JobAlbumArt
    , _joCaptions            :: Maybe Captions
    , _joComposition         :: List "Composition" Clip
    , _joDuration            :: Maybe Integer
    , _joEncryption          :: Maybe Encryption
    , _joHeight              :: Maybe Int
    , _joId                  :: Maybe Text
    , _joKey                 :: Maybe Text
    , _joPresetId            :: Maybe Text
    , _joRotate              :: Maybe Text
    , _joSegmentDuration     :: Maybe Text
    , _joStatus              :: Maybe Text
    , _joStatusDetail        :: Maybe Text
    , _joThumbnailEncryption :: Maybe Encryption
    , _joThumbnailPattern    :: Maybe Text
    , _joWatermarks          :: List "Watermarks" JobWatermark
    , _joWidth               :: Maybe Int
    } deriving (Eq, Show)

-- | 'JobOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'joAlbumArt' @::@ 'Maybe' 'JobAlbumArt'
--
-- * 'joCaptions' @::@ 'Maybe' 'Captions'
--
-- * 'joComposition' @::@ ['Clip']
--
-- * 'joDuration' @::@ 'Maybe' 'Integer'
--
-- * 'joEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'joHeight' @::@ 'Maybe' 'Int'
--
-- * 'joId' @::@ 'Maybe' 'Text'
--
-- * 'joKey' @::@ 'Maybe' 'Text'
--
-- * 'joPresetId' @::@ 'Maybe' 'Text'
--
-- * 'joRotate' @::@ 'Maybe' 'Text'
--
-- * 'joSegmentDuration' @::@ 'Maybe' 'Text'
--
-- * 'joStatus' @::@ 'Maybe' 'Text'
--
-- * 'joStatusDetail' @::@ 'Maybe' 'Text'
--
-- * 'joThumbnailEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'joThumbnailPattern' @::@ 'Maybe' 'Text'
--
-- * 'joWatermarks' @::@ ['JobWatermark']
--
-- * 'joWidth' @::@ 'Maybe' 'Int'
--
jobOutput :: JobOutput
jobOutput = JobOutput
    { _joId                  = Nothing
    , _joKey                 = Nothing
    , _joThumbnailPattern    = Nothing
    , _joThumbnailEncryption = Nothing
    , _joRotate              = Nothing
    , _joPresetId            = Nothing
    , _joSegmentDuration     = Nothing
    , _joStatus              = Nothing
    , _joStatusDetail        = Nothing
    , _joDuration            = Nothing
    , _joWidth               = Nothing
    , _joHeight              = Nothing
    , _joWatermarks          = mempty
    , _joAlbumArt            = Nothing
    , _joComposition         = mempty
    , _joCaptions            = Nothing
    , _joEncryption          = Nothing
    }

-- | The album art to be associated with the output file, if any.
joAlbumArt :: Lens' JobOutput (Maybe JobAlbumArt)
joAlbumArt = lens _joAlbumArt (\s a -> s { _joAlbumArt = a })

-- | You can configure Elastic Transcoder to transcode captions, or subtitles,
-- from one format to another. All captions must be in UTF-8. Elastic Transcoder
-- supports two types of captions:
--
-- Embedded: Embedded captions are included in the same file as the audio and
-- video. Elastic Transcoder supports only one embedded caption per language, to
-- a maximum of 300 embedded captions per file.
--
-- Valid input values include: 'CEA-608 (EIA-608', first non-empty channel only), 'CEA-708 (EIA-708', first non-empty channel only), and 'mov-text'
--
-- Valid outputs include: 'mov-text'
--
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
-- Sidecar: Sidecar captions are kept in a separate metadata file from the
-- audio and video data. Sidecar captions require a player that is capable of
-- understanding the relationship between the video file and the sidecar file.
-- Elastic Transcoder supports only one sidecar caption per language, to a
-- maximum of 20 sidecar captions per file.
--
-- Valid input values include: 'dfxp' (first div element only), 'ebu-tt', 'scc', 'smpt',
-- 'srt', 'ttml' (first div element only), and 'webvtt'
--
-- Valid outputs include: 'dfxp' (first div element only), 'scc', 'srt', and 'webvtt'.
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your
-- output format.
--
-- Elastic Transcoder does not support OCR (Optical Character Recognition),
-- does not accept pictures as a valid input for captions, and is not available
-- for audio-only transcoding. Elastic Transcoder does not preserve text
-- formatting (for example, italics) during the transcoding process.
--
-- To remove captions or leave the captions empty, set 'Captions' to null. To
-- pass through existing captions unchanged, set the 'MergePolicy' to 'MergeRetain',
-- and pass in a null 'CaptionSources' array.
--
-- For more information on embedded files, see the Subtitles Wikipedia page.
--
-- For more information on sidecar files, see the Extensible Metadata Platform
-- and Sidecar file Wikipedia pages.
joCaptions :: Lens' JobOutput (Maybe Captions)
joCaptions = lens _joCaptions (\s a -> s { _joCaptions = a })

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make up
-- an output file. For the current release, you can only specify settings for a
-- single clip per output file. The Composition object cannot be null.
joComposition :: Lens' JobOutput [Clip]
joComposition = lens _joComposition (\s a -> s { _joComposition = a }) . _List

-- | Duration of the output file, in seconds.
joDuration :: Lens' JobOutput (Maybe Integer)
joDuration = lens _joDuration (\s a -> s { _joDuration = a })

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your output files. If you choose to use encryption, you must specify a mode
-- to use. If you choose not to use encryption, Elastic Transcoder will write an
-- unencrypted file to your Amazon S3 bucket.
joEncryption :: Lens' JobOutput (Maybe Encryption)
joEncryption = lens _joEncryption (\s a -> s { _joEncryption = a })

-- | Height of the output file, in pixels.
joHeight :: Lens' JobOutput (Maybe Int)
joHeight = lens _joHeight (\s a -> s { _joHeight = a })

-- | A sequential counter, starting with 1, that identifies an output among the
-- outputs from the current job. In the Output syntax, this value is always 1.
joId :: Lens' JobOutput (Maybe Text)
joId = lens _joId (\s a -> s { _joId = a })

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file
-- in the Amazon S3 bucket specified by the 'OutputBucket' object in the pipeline
-- that is specified by the pipeline ID.
joKey :: Lens' JobOutput (Maybe Text)
joKey = lens _joKey (\s a -> s { _joKey = a })

-- | The value of the 'Id' object for the preset that you want to use for this job.
-- The preset determines the audio, video, and thumbnail settings that Elastic
-- Transcoder uses for transcoding. To use a preset that you created, specify
-- the preset ID that Elastic Transcoder returned in the response when you
-- created the preset. You can also use the Elastic Transcoder system presets,
-- which you can get with 'ListPresets'.
joPresetId :: Lens' JobOutput (Maybe Text)
joPresetId = lens _joPresetId (\s a -> s { _joPresetId = a })

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values:
--
-- 'auto', '0', '90', '180', '270'
--
-- The value 'auto' generally works only if the file that you're transcoding
-- contains rotation metadata.
joRotate :: Lens' JobOutput (Maybe Text)
joRotate = lens _joRotate (\s a -> s { _joRotate = a })

-- | (Outputs in Fragmented MP4 or MPEG-TS format only.If you specify a preset in 'PresetId' for which the value of 'Container' is 'fmp4' (Fragmented MP4) or 'ts' (MPEG-TS), 'SegmentDuration' is the target maximum duration of each segment in seconds. For 'HLSv3' format
-- playlists, each media segment is stored in a separate '.ts' file. For 'HLSv4' and 'Smooth' playlists, all media segments for an output are stored in a single
-- file. Each segment is approximately the length of the 'SegmentDuration', though
-- individual segments might be shorter or longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the video
-- is not evenly divisible by 'SegmentDuration', the duration of the last segment
-- is the remainder of total length/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output 'HLS'
-- output that you specify in OutputKeys. To add an output to the master
-- playlist for this job, include it in the 'OutputKeys' of the associated
-- playlist.
joSegmentDuration :: Lens' JobOutput (Maybe Text)
joSegmentDuration =
    lens _joSegmentDuration (\s a -> s { _joSegmentDuration = a })

-- | The status of one output in a job. If you specified only one output for the
-- job, 'Outputs:Status' is always the same as 'Job:Status'. If you specified more
-- than one output:   'Job:Status' and 'Outputs:Status' for all of the outputs is
-- Submitted until Elastic Transcoder starts to process the first output. When
-- Elastic Transcoder starts to process the first output, 'Outputs:Status' for
-- that output and 'Job:Status' both change to Progressing. For each output, the
-- value of 'Outputs:Status' remains Submitted until Elastic Transcoder starts to
-- process the output. Job:Status remains Progressing until all of the outputs
-- reach a terminal status, either Complete or Error. When all of the outputs
-- reach a terminal status, 'Job:Status' changes to Complete only if 'Outputs:Status'
-- for all of the outputs is 'Complete'. If 'Outputs:Status' for one or more
-- outputs is 'Error', the terminal status for 'Job:Status' is also 'Error'.  The
-- value of 'Status' is one of the following: 'Submitted', 'Progressing', 'Complete', 'Canceled', or 'Error'.
joStatus :: Lens' JobOutput (Maybe Text)
joStatus = lens _joStatus (\s a -> s { _joStatus = a })

-- | Information that further explains 'Status'.
joStatusDetail :: Lens' JobOutput (Maybe Text)
joStatusDetail = lens _joStatusDetail (\s a -> s { _joStatusDetail = a })

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your thumbnail.
joThumbnailEncryption :: Lens' JobOutput (Maybe Encryption)
joThumbnailEncryption =
    lens _joThumbnailEncryption (\s a -> s { _joThumbnailEncryption = a })

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and,
-- if so, how you want Elastic Transcoder to name the files.
--
-- If you don't want Elastic Transcoder to create thumbnails, specify "".
--
-- If you do want Elastic Transcoder to create thumbnails, specify the
-- information that you want to include in the file name for each thumbnail. You
-- can specify the following values in any sequence:
--
-- '{count}' (Required): If you want to create thumbnails, you must include '{count}' in the 'ThumbnailPattern' object. Wherever you specify '{count}', Elastic
-- Transcoder adds a five-digit sequence number (beginning with 00001) to
-- thumbnail file names. The number indicates where a given thumbnail appears in
-- the sequence of thumbnails for a transcoded file.
--
-- If you specify a literal value and/or '{resolution}' but you omit '{count}',
-- Elastic Transcoder returns a validation error and does not create the job.
-- Literal values (Optional): You can specify literal values anywhere in the 'ThumbnailPattern' object. For example, you can include them as a file name prefix or as a
-- delimiter between '{resolution}' and '{count}'.
--
-- '{resolution}' (Optional): If you want Elastic Transcoder to include the
-- resolution in the file name, include '{resolution}' in the 'ThumbnailPattern'
-- object.
--
-- When creating thumbnails, Elastic Transcoder automatically saves the files
-- in the format (.jpg or .png) that appears in the preset that you specified in
-- the 'PresetID' value of 'CreateJobOutput'. Elastic Transcoder also appends the
-- applicable file name extension.
joThumbnailPattern :: Lens' JobOutput (Maybe Text)
joThumbnailPattern =
    lens _joThumbnailPattern (\s a -> s { _joThumbnailPattern = a })

-- | Information about the watermarks that you want Elastic Transcoder to add to
-- the video during transcoding. You can specify up to four watermarks for each
-- output. Settings for each watermark must be defined in the preset that you
-- specify in 'Preset' for the current output.
--
-- Watermarks are added to the output video in the sequence in which you list
-- them in the job output—the first watermark in the list is added to the output
-- video first, the second watermark in the list is added next, and so on. As a
-- result, if the settings in a preset cause Elastic Transcoder to place all
-- watermarks in the same location, the second watermark that you add will cover
-- the first one, the third one will cover the second, and the fourth one will
-- cover the third.
joWatermarks :: Lens' JobOutput [JobWatermark]
joWatermarks = lens _joWatermarks (\s a -> s { _joWatermarks = a }) . _List

-- | Specifies the width of the output file in pixels.
joWidth :: Lens' JobOutput (Maybe Int)
joWidth = lens _joWidth (\s a -> s { _joWidth = a })

instance FromJSON JobOutput where
    parseJSON = withObject "JobOutput" $ \o -> JobOutput
        <$> o .:? "AlbumArt"
        <*> o .:? "Captions"
        <*> o .:? "Composition" .!= mempty
        <*> o .:? "Duration"
        <*> o .:? "Encryption"
        <*> o .:? "Height"
        <*> o .:? "Id"
        <*> o .:? "Key"
        <*> o .:? "PresetId"
        <*> o .:? "Rotate"
        <*> o .:? "SegmentDuration"
        <*> o .:? "Status"
        <*> o .:? "StatusDetail"
        <*> o .:? "ThumbnailEncryption"
        <*> o .:? "ThumbnailPattern"
        <*> o .:? "Watermarks" .!= mempty
        <*> o .:? "Width"

instance ToJSON JobOutput where
    toJSON JobOutput{..} = object
        [ "Id"                  .= _joId
        , "Key"                 .= _joKey
        , "ThumbnailPattern"    .= _joThumbnailPattern
        , "ThumbnailEncryption" .= _joThumbnailEncryption
        , "Rotate"              .= _joRotate
        , "PresetId"            .= _joPresetId
        , "SegmentDuration"     .= _joSegmentDuration
        , "Status"              .= _joStatus
        , "StatusDetail"        .= _joStatusDetail
        , "Duration"            .= _joDuration
        , "Width"               .= _joWidth
        , "Height"              .= _joHeight
        , "Watermarks"          .= _joWatermarks
        , "AlbumArt"            .= _joAlbumArt
        , "Composition"         .= _joComposition
        , "Captions"            .= _joCaptions
        , "Encryption"          .= _joEncryption
        ]

data Job' = Job'
    { _jArn             :: Maybe Text
    , _jId              :: Maybe Text
    , _jInput           :: Maybe JobInput
    , _jOutput          :: Maybe JobOutput
    , _jOutputKeyPrefix :: Maybe Text
    , _jOutputs         :: List "Outputs" JobOutput
    , _jPipelineId      :: Maybe Text
    , _jPlaylists       :: List "Playlists" Playlist
    , _jStatus          :: Maybe Text
    } deriving (Eq, Show)

-- | 'Job'' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jArn' @::@ 'Maybe' 'Text'
--
-- * 'jId' @::@ 'Maybe' 'Text'
--
-- * 'jInput' @::@ 'Maybe' 'JobInput'
--
-- * 'jOutput' @::@ 'Maybe' 'JobOutput'
--
-- * 'jOutputKeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'jOutputs' @::@ ['JobOutput']
--
-- * 'jPipelineId' @::@ 'Maybe' 'Text'
--
-- * 'jPlaylists' @::@ ['Playlist']
--
-- * 'jStatus' @::@ 'Maybe' 'Text'
--
job :: Job'
job = Job'
    { _jId              = Nothing
    , _jArn             = Nothing
    , _jPipelineId      = Nothing
    , _jInput           = Nothing
    , _jOutput          = Nothing
    , _jOutputs         = mempty
    , _jOutputKeyPrefix = Nothing
    , _jPlaylists       = mempty
    , _jStatus          = Nothing
    }

-- | The Amazon Resource Name (ARN) for the job.
jArn :: Lens' Job' (Maybe Text)
jArn = lens _jArn (\s a -> s { _jArn = a })

-- | The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
jId :: Lens' Job' (Maybe Text)
jId = lens _jId (\s a -> s { _jId = a })

-- | A section of the request or response body that provides information about
-- the file that is being transcoded.
jInput :: Lens' Job' (Maybe JobInput)
jInput = lens _jInput (\s a -> s { _jInput = a })

-- | If you specified one output for a job, information about that output. If you
-- specified multiple outputs for a job, the Output object lists information
-- about the first output. This duplicates the information that is listed for
-- the first output in the Outputs object.
--
-- Outputs recommended instead. A section of the request or response body that
-- provides information about the transcoded (target) file.
jOutput :: Lens' Job' (Maybe JobOutput)
jOutput = lens _jOutput (\s a -> s { _jOutput = a })

-- | The value, if any, that you want Elastic Transcoder to prepend to the names
-- of all files that this job creates, including output files, thumbnails, and
-- playlists. We recommend that you add a / or some other delimiter to the end
-- of the 'OutputKeyPrefix'.
jOutputKeyPrefix :: Lens' Job' (Maybe Text)
jOutputKeyPrefix = lens _jOutputKeyPrefix (\s a -> s { _jOutputKeyPrefix = a })

-- | Information about the output files. We recommend that you use the 'Outputs'
-- syntax for all jobs, even when you want Elastic Transcoder to transcode a
-- file into only one format. Do not use both the 'Outputs' and 'Output' syntaxes in
-- the same request. You can create a maximum of 30 outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder creates
-- the files for each output in the order in which you specify them in the job.
jOutputs :: Lens' Job' [JobOutput]
jOutputs = lens _jOutputs (\s a -> s { _jOutputs = a }) . _List

-- | The 'Id' of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the Amazon
-- S3 bucket from which Elastic Transcoder gets the files to transcode and the
-- bucket into which Elastic Transcoder puts the transcoded files.
jPipelineId :: Lens' Job' (Maybe Text)
jPipelineId = lens _jPipelineId (\s a -> s { _jPipelineId = a })

-- | Outputs in Fragmented MP4 or MPEG-TS format only.If you specify a preset in 'PresetId' for which the value of 'Container' is fmp4 (Fragmented MP4) or ts (MPEG-TS), 'Playlists' contains information about the master playlists that you want Elastic
-- Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
jPlaylists :: Lens' Job' [Playlist]
jPlaylists = lens _jPlaylists (\s a -> s { _jPlaylists = a }) . _List

-- | The status of the job: 'Submitted', 'Progressing', 'Complete', 'Canceled', or 'Error'.
jStatus :: Lens' Job' (Maybe Text)
jStatus = lens _jStatus (\s a -> s { _jStatus = a })

instance FromJSON Job' where
    parseJSON = withObject "Job'" $ \o -> Job'
        <$> o .:? "Arn"
        <*> o .:? "Id"
        <*> o .:? "Input"
        <*> o .:? "Output"
        <*> o .:? "OutputKeyPrefix"
        <*> o .:? "Outputs" .!= mempty
        <*> o .:? "PipelineId"
        <*> o .:? "Playlists" .!= mempty
        <*> o .:? "Status"

instance ToJSON Job' where
    toJSON Job'{..} = object
        [ "Id"              .= _jId
        , "Arn"             .= _jArn
        , "PipelineId"      .= _jPipelineId
        , "Input"           .= _jInput
        , "Output"          .= _jOutput
        , "Outputs"         .= _jOutputs
        , "OutputKeyPrefix" .= _jOutputKeyPrefix
        , "Playlists"       .= _jPlaylists
        , "Status"          .= _jStatus
        ]

data CaptionSource = CaptionSource
    { _csEncryption :: Maybe Encryption
    , _csKey        :: Maybe Text
    , _csLabel      :: Maybe Text
    , _csLanguage   :: Maybe Text
    , _csTimeOffset :: Maybe Text
    } deriving (Eq, Show)

-- | 'CaptionSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'csKey' @::@ 'Maybe' 'Text'
--
-- * 'csLabel' @::@ 'Maybe' 'Text'
--
-- * 'csLanguage' @::@ 'Maybe' 'Text'
--
-- * 'csTimeOffset' @::@ 'Maybe' 'Text'
--
captionSource :: CaptionSource
captionSource = CaptionSource
    { _csKey        = Nothing
    , _csLanguage   = Nothing
    , _csTimeOffset = Nothing
    , _csLabel      = Nothing
    , _csEncryption = Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your caption sources.
csEncryption :: Lens' CaptionSource (Maybe Encryption)
csEncryption = lens _csEncryption (\s a -> s { _csEncryption = a })

-- | The name of the sidecar caption file that you want Elastic Transcoder to
-- include in the output file.
csKey :: Lens' CaptionSource (Maybe Text)
csKey = lens _csKey (\s a -> s { _csKey = a })

-- | The label of the caption shown in the player when choosing a language. We
-- recommend that you put the caption language name here, in the language of the
-- captions.
csLabel :: Lens' CaptionSource (Maybe Text)
csLabel = lens _csLabel (\s a -> s { _csLabel = a })

-- | A string that specifies the language of the caption. Specify this as one of:
--
-- 2-character ISO 639-1 code
--
-- 3-character ISO 639-2 code
--
-- For more information on ISO language codes and language names, see the List
-- of ISO 639-1 codes.
csLanguage :: Lens' CaptionSource (Maybe Text)
csLanguage = lens _csLanguage (\s a -> s { _csLanguage = a })

-- | For clip generation or captions that do not start at the same time as the
-- associated video file, the 'TimeOffset' tells Elastic Transcoder how much of
-- the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
csTimeOffset :: Lens' CaptionSource (Maybe Text)
csTimeOffset = lens _csTimeOffset (\s a -> s { _csTimeOffset = a })

instance FromJSON CaptionSource where
    parseJSON = withObject "CaptionSource" $ \o -> CaptionSource
        <$> o .:? "Encryption"
        <*> o .:? "Key"
        <*> o .:? "Label"
        <*> o .:? "Language"
        <*> o .:? "TimeOffset"

instance ToJSON CaptionSource where
    toJSON CaptionSource{..} = object
        [ "Key"        .= _csKey
        , "Language"   .= _csLanguage
        , "TimeOffset" .= _csTimeOffset
        , "Label"      .= _csLabel
        , "Encryption" .= _csEncryption
        ]

data Artwork = Artwork
    { _aAlbumArtFormat :: Maybe Text
    , _aEncryption     :: Maybe Encryption
    , _aInputKey       :: Maybe Text
    , _aMaxHeight      :: Maybe Text
    , _aMaxWidth       :: Maybe Text
    , _aPaddingPolicy  :: Maybe Text
    , _aSizingPolicy   :: Maybe Text
    } deriving (Eq, Show)

-- | 'Artwork' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAlbumArtFormat' @::@ 'Maybe' 'Text'
--
-- * 'aEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'aInputKey' @::@ 'Maybe' 'Text'
--
-- * 'aMaxHeight' @::@ 'Maybe' 'Text'
--
-- * 'aMaxWidth' @::@ 'Maybe' 'Text'
--
-- * 'aPaddingPolicy' @::@ 'Maybe' 'Text'
--
-- * 'aSizingPolicy' @::@ 'Maybe' 'Text'
--
artwork :: Artwork
artwork = Artwork
    { _aInputKey       = Nothing
    , _aMaxWidth       = Nothing
    , _aMaxHeight      = Nothing
    , _aSizingPolicy   = Nothing
    , _aPaddingPolicy  = Nothing
    , _aAlbumArtFormat = Nothing
    , _aEncryption     = Nothing
    }

-- | The format of album art, if any. Valid formats are '.jpg' and '.png'.
aAlbumArtFormat :: Lens' Artwork (Maybe Text)
aAlbumArtFormat = lens _aAlbumArtFormat (\s a -> s { _aAlbumArtFormat = a })

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your artwork.
aEncryption :: Lens' Artwork (Maybe Encryption)
aEncryption = lens _aEncryption (\s a -> s { _aEncryption = a })

-- | The name of the file to be used as album art. To determine which Amazon S3
-- bucket contains the specified file, Elastic Transcoder checks the pipeline
-- specified by 'PipelineId'; the 'InputBucket' object in that pipeline identifies
-- the bucket.
--
-- If the file name includes a prefix, for example, 'cooking/pie.jpg', include
-- the prefix in the key. If the file isn't in the specified bucket, Elastic
-- Transcoder returns an error.
aInputKey :: Lens' Artwork (Maybe Text)
aInputKey = lens _aInputKey (\s a -> s { _aInputKey = a })

-- | The maximum height of the output album art in pixels. If you specify 'auto',
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 3072, inclusive.
aMaxHeight :: Lens' Artwork (Maybe Text)
aMaxHeight = lens _aMaxHeight (\s a -> s { _aMaxHeight = a })

-- | The maximum width of the output album art in pixels. If you specify 'auto',
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 4096, inclusive.
aMaxWidth :: Lens' Artwork (Maybe Text)
aMaxWidth = lens _aMaxWidth (\s a -> s { _aMaxWidth = a })

-- | When you set 'PaddingPolicy' to 'Pad', Elastic Transcoder may add white bars to
-- the top and bottom and/or left and right sides of the output album art to
-- make the total size of the output art match the values that you specified for 'MaxWidth' and 'MaxHeight'.
aPaddingPolicy :: Lens' Artwork (Maybe Text)
aPaddingPolicy = lens _aPaddingPolicy (\s a -> s { _aPaddingPolicy = a })

-- | Specify one of the following values to control scaling of the output album
-- art:
--
-- 'Fit:' Elastic Transcoder scales the output art so it matches the value
-- that you specified in either 'MaxWidth' or 'MaxHeight' without exceeding the
-- other value.  'Fill:' Elastic Transcoder scales the output art so it matches
-- the value that you specified in either 'MaxWidth' or 'MaxHeight' and matches or
-- exceeds the other value. Elastic Transcoder centers the output art and then
-- crops it in the dimension (if any) that exceeds the maximum value.   'Stretch:'
-- Elastic Transcoder stretches the output art to match the values that you
-- specified for 'MaxWidth' and 'MaxHeight'. If the relative proportions of the
-- input art and the output art are different, the output art will be distorted.
-- 'Keep:' Elastic Transcoder does not scale the output art. If either dimension
-- of the input art exceeds the values that you specified for 'MaxWidth' and 'MaxHeight', Elastic Transcoder crops the output art.  'ShrinkToFit:' Elastic Transcoder
-- scales the output art down so that its dimensions match the values that you
-- specified for at least one of 'MaxWidth' and 'MaxHeight' without exceeding either
-- value. If you specify this option, Elastic Transcoder does not scale the art
-- up.  'ShrinkToFill' Elastic Transcoder scales the output art down so that its
-- dimensions match the values that you specified for at least one of 'MaxWidth'
-- and 'MaxHeight' without dropping below either value. If you specify this
-- option, Elastic Transcoder does not scale the art up.
aSizingPolicy :: Lens' Artwork (Maybe Text)
aSizingPolicy = lens _aSizingPolicy (\s a -> s { _aSizingPolicy = a })

instance FromJSON Artwork where
    parseJSON = withObject "Artwork" $ \o -> Artwork
        <$> o .:? "AlbumArtFormat"
        <*> o .:? "Encryption"
        <*> o .:? "InputKey"
        <*> o .:? "MaxHeight"
        <*> o .:? "MaxWidth"
        <*> o .:? "PaddingPolicy"
        <*> o .:? "SizingPolicy"

instance ToJSON Artwork where
    toJSON Artwork{..} = object
        [ "InputKey"       .= _aInputKey
        , "MaxWidth"       .= _aMaxWidth
        , "MaxHeight"      .= _aMaxHeight
        , "SizingPolicy"   .= _aSizingPolicy
        , "PaddingPolicy"  .= _aPaddingPolicy
        , "AlbumArtFormat" .= _aAlbumArtFormat
        , "Encryption"     .= _aEncryption
        ]

data TimeSpan = TimeSpan
    { _tsDuration  :: Maybe Text
    , _tsStartTime :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'TimeSpan' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tsDuration' @::@ 'Maybe' 'Text'
--
-- * 'tsStartTime' @::@ 'Maybe' 'Text'
--
timeSpan :: TimeSpan
timeSpan = TimeSpan
    { _tsStartTime = Nothing
    , _tsDuration  = Nothing
    }

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
-- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum
-- value: 86399.999). If you don't specify a value, Elastic Transcoder creates
-- an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file, Elastic
-- Transcoder transcodes the file and returns a warning message.
tsDuration :: Lens' TimeSpan (Maybe Text)
tsDuration = lens _tsDuration (\s a -> s { _tsDuration = a })

-- | The place in the input file where you want a clip to start. The format can be
-- either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a
-- second) or sssss.SSS (maximum value: 86399.999). If you don't specify a
-- value, Elastic Transcoder starts at the beginning of the input file.
tsStartTime :: Lens' TimeSpan (Maybe Text)
tsStartTime = lens _tsStartTime (\s a -> s { _tsStartTime = a })

instance FromJSON TimeSpan where
    parseJSON = withObject "TimeSpan" $ \o -> TimeSpan
        <$> o .:? "Duration"
        <*> o .:? "StartTime"

instance ToJSON TimeSpan where
    toJSON TimeSpan{..} = object
        [ "StartTime" .= _tsStartTime
        , "Duration"  .= _tsDuration
        ]

data CreateJobOutput = CreateJobOutput
    { _cjoAlbumArt            :: Maybe JobAlbumArt
    , _cjoCaptions            :: Maybe Captions
    , _cjoComposition         :: List "Composition" Clip
    , _cjoEncryption          :: Maybe Encryption
    , _cjoKey                 :: Maybe Text
    , _cjoPresetId            :: Maybe Text
    , _cjoRotate              :: Maybe Text
    , _cjoSegmentDuration     :: Maybe Text
    , _cjoThumbnailEncryption :: Maybe Encryption
    , _cjoThumbnailPattern    :: Maybe Text
    , _cjoWatermarks          :: List "Watermarks" JobWatermark
    } deriving (Eq, Show)

-- | 'CreateJobOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjoAlbumArt' @::@ 'Maybe' 'JobAlbumArt'
--
-- * 'cjoCaptions' @::@ 'Maybe' 'Captions'
--
-- * 'cjoComposition' @::@ ['Clip']
--
-- * 'cjoEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'cjoKey' @::@ 'Maybe' 'Text'
--
-- * 'cjoPresetId' @::@ 'Maybe' 'Text'
--
-- * 'cjoRotate' @::@ 'Maybe' 'Text'
--
-- * 'cjoSegmentDuration' @::@ 'Maybe' 'Text'
--
-- * 'cjoThumbnailEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'cjoThumbnailPattern' @::@ 'Maybe' 'Text'
--
-- * 'cjoWatermarks' @::@ ['JobWatermark']
--
createJobOutput :: CreateJobOutput
createJobOutput = CreateJobOutput
    { _cjoKey                 = Nothing
    , _cjoThumbnailPattern    = Nothing
    , _cjoThumbnailEncryption = Nothing
    , _cjoRotate              = Nothing
    , _cjoPresetId            = Nothing
    , _cjoSegmentDuration     = Nothing
    , _cjoWatermarks          = mempty
    , _cjoAlbumArt            = Nothing
    , _cjoComposition         = mempty
    , _cjoCaptions            = Nothing
    , _cjoEncryption          = Nothing
    }

-- | Information about the album art that you want Elastic Transcoder to add to
-- the file during transcoding. You can specify up to twenty album artworks for
-- each output. Settings for each artwork must be defined in the job for the
-- current output.
cjoAlbumArt :: Lens' CreateJobOutput (Maybe JobAlbumArt)
cjoAlbumArt = lens _cjoAlbumArt (\s a -> s { _cjoAlbumArt = a })

-- | You can configure Elastic Transcoder to transcode captions, or subtitles,
-- from one format to another. All captions must be in UTF-8. Elastic Transcoder
-- supports two types of captions:
--
-- Embedded: Embedded captions are included in the same file as the audio and
-- video. Elastic Transcoder supports only one embedded caption per language, to
-- a maximum of 300 embedded captions per file.
--
-- Valid input values include: 'CEA-608 (EIA-608', first non-empty channel only), 'CEA-708 (EIA-708', first non-empty channel only), and 'mov-text'
--
-- Valid outputs include: 'mov-text'
--
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
-- Sidecar: Sidecar captions are kept in a separate metadata file from the
-- audio and video data. Sidecar captions require a player that is capable of
-- understanding the relationship between the video file and the sidecar file.
-- Elastic Transcoder supports only one sidecar caption per language, to a
-- maximum of 20 sidecar captions per file.
--
-- Valid input values include: 'dfxp' (first div element only), 'ebu-tt', 'scc', 'smpt',
-- 'srt', 'ttml' (first div element only), and 'webvtt'
--
-- Valid outputs include: 'dfxp' (first div element only), 'scc', 'srt', and 'webvtt'.
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your
-- output format.
--
-- Elastic Transcoder does not support OCR (Optical Character Recognition),
-- does not accept pictures as a valid input for captions, and is not available
-- for audio-only transcoding. Elastic Transcoder does not preserve text
-- formatting (for example, italics) during the transcoding process.
--
-- To remove captions or leave the captions empty, set 'Captions' to null. To
-- pass through existing captions unchanged, set the 'MergePolicy' to 'MergeRetain',
-- and pass in a null 'CaptionSources' array.
--
-- For more information on embedded files, see the Subtitles Wikipedia page.
--
-- For more information on sidecar files, see the Extensible Metadata Platform
-- and Sidecar file Wikipedia pages.
cjoCaptions :: Lens' CreateJobOutput (Maybe Captions)
cjoCaptions = lens _cjoCaptions (\s a -> s { _cjoCaptions = a })

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make up
-- an output file. For the current release, you can only specify settings for a
-- single clip per output file. The Composition object cannot be null.
cjoComposition :: Lens' CreateJobOutput [Clip]
cjoComposition = lens _cjoComposition (\s a -> s { _cjoComposition = a }) . _List

-- | You can specify encryption settings for any output files that you want to use
-- for a transcoding job. This includes the output file and any watermarks,
-- thumbnails, album art, or captions that you want to use. You must specify
-- encryption settings for each file individually.
cjoEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoEncryption = lens _cjoEncryption (\s a -> s { _cjoEncryption = a })

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file
-- in the Amazon S3 bucket specified by the 'OutputBucket' object in the pipeline
-- that is specified by the pipeline ID. If a file with the specified name
-- already exists in the output bucket, the job fails.
cjoKey :: Lens' CreateJobOutput (Maybe Text)
cjoKey = lens _cjoKey (\s a -> s { _cjoKey = a })

-- | The 'Id' of the preset to use for this job. The preset determines the audio,
-- video, and thumbnail settings that Elastic Transcoder uses for transcoding.
cjoPresetId :: Lens' CreateJobOutput (Maybe Text)
cjoPresetId = lens _cjoPresetId (\s a -> s { _cjoPresetId = a })

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values: 'auto', '0', '90', '180', '270'. The value 'auto' generally works only if the file that
-- you're transcoding contains rotation metadata.
cjoRotate :: Lens' CreateJobOutput (Maybe Text)
cjoRotate = lens _cjoRotate (\s a -> s { _cjoRotate = a })

-- | (Outputs in Fragmented MP4 or MPEG-TS format only.If you specify a preset in 'PresetId' for which the value of 'Container' is 'fmp4' (Fragmented MP4) or 'ts' (MPEG-TS), 'SegmentDuration' is the target maximum duration of each segment in seconds. For 'HLSv3' format
-- playlists, each media segment is stored in a separate '.ts' file. For 'HLSv4' and 'Smooth' playlists, all media segments for an output are stored in a single
-- file. Each segment is approximately the length of the 'SegmentDuration', though
-- individual segments might be shorter or longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the video
-- is not evenly divisible by 'SegmentDuration', the duration of the last segment
-- is the remainder of total length/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output 'HLS'
-- output that you specify in OutputKeys. To add an output to the master
-- playlist for this job, include it in the 'OutputKeys' of the associated
-- playlist.
cjoSegmentDuration :: Lens' CreateJobOutput (Maybe Text)
cjoSegmentDuration =
    lens _cjoSegmentDuration (\s a -> s { _cjoSegmentDuration = a })

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your thumbnail.
cjoThumbnailEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoThumbnailEncryption =
    lens _cjoThumbnailEncryption (\s a -> s { _cjoThumbnailEncryption = a })

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and,
-- if so, how you want Elastic Transcoder to name the files.
--
-- If you don't want Elastic Transcoder to create thumbnails, specify "".
--
-- If you do want Elastic Transcoder to create thumbnails, specify the
-- information that you want to include in the file name for each thumbnail. You
-- can specify the following values in any sequence:
--
-- '{count}' (Required): If you want to create thumbnails, you must include '{count}' in the 'ThumbnailPattern' object. Wherever you specify '{count}', Elastic
-- Transcoder adds a five-digit sequence number (beginning with 00001) to
-- thumbnail file names. The number indicates where a given thumbnail appears in
-- the sequence of thumbnails for a transcoded file.
--
-- If you specify a literal value and/or '{resolution}' but you omit '{count}',
-- Elastic Transcoder returns a validation error and does not create the job.
-- Literal values (Optional): You can specify literal values anywhere in the 'ThumbnailPattern' object. For example, you can include them as a file name prefix or as a
-- delimiter between '{resolution}' and '{count}'.
--
-- '{resolution}' (Optional): If you want Elastic Transcoder to include the
-- resolution in the file name, include '{resolution}' in the 'ThumbnailPattern'
-- object.
--
-- When creating thumbnails, Elastic Transcoder automatically saves the files
-- in the format (.jpg or .png) that appears in the preset that you specified in
-- the 'PresetID' value of 'CreateJobOutput'. Elastic Transcoder also appends the
-- applicable file name extension.
cjoThumbnailPattern :: Lens' CreateJobOutput (Maybe Text)
cjoThumbnailPattern =
    lens _cjoThumbnailPattern (\s a -> s { _cjoThumbnailPattern = a })

-- | Information about the watermarks that you want Elastic Transcoder to add to
-- the video during transcoding. You can specify up to four watermarks for each
-- output. Settings for each watermark must be defined in the preset for the
-- current output.
cjoWatermarks :: Lens' CreateJobOutput [JobWatermark]
cjoWatermarks = lens _cjoWatermarks (\s a -> s { _cjoWatermarks = a }) . _List

instance FromJSON CreateJobOutput where
    parseJSON = withObject "CreateJobOutput" $ \o -> CreateJobOutput
        <$> o .:? "AlbumArt"
        <*> o .:? "Captions"
        <*> o .:? "Composition" .!= mempty
        <*> o .:? "Encryption"
        <*> o .:? "Key"
        <*> o .:? "PresetId"
        <*> o .:? "Rotate"
        <*> o .:? "SegmentDuration"
        <*> o .:? "ThumbnailEncryption"
        <*> o .:? "ThumbnailPattern"
        <*> o .:? "Watermarks" .!= mempty

instance ToJSON CreateJobOutput where
    toJSON CreateJobOutput{..} = object
        [ "Key"                 .= _cjoKey
        , "ThumbnailPattern"    .= _cjoThumbnailPattern
        , "ThumbnailEncryption" .= _cjoThumbnailEncryption
        , "Rotate"              .= _cjoRotate
        , "PresetId"            .= _cjoPresetId
        , "SegmentDuration"     .= _cjoSegmentDuration
        , "Watermarks"          .= _cjoWatermarks
        , "AlbumArt"            .= _cjoAlbumArt
        , "Composition"         .= _cjoComposition
        , "Captions"            .= _cjoCaptions
        , "Encryption"          .= _cjoEncryption
        ]

data AudioParameters = AudioParameters
    { _apBitRate      :: Maybe Text
    , _apChannels     :: Maybe Text
    , _apCodec        :: Maybe Text
    , _apCodecOptions :: Maybe AudioCodecOptions
    , _apSampleRate   :: Maybe Text
    } deriving (Eq, Show)

-- | 'AudioParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apBitRate' @::@ 'Maybe' 'Text'
--
-- * 'apChannels' @::@ 'Maybe' 'Text'
--
-- * 'apCodec' @::@ 'Maybe' 'Text'
--
-- * 'apCodecOptions' @::@ 'Maybe' 'AudioCodecOptions'
--
-- * 'apSampleRate' @::@ 'Maybe' 'Text'
--
audioParameters :: AudioParameters
audioParameters = AudioParameters
    { _apCodec        = Nothing
    , _apSampleRate   = Nothing
    , _apBitRate      = Nothing
    , _apChannels     = Nothing
    , _apCodecOptions = Nothing
    }

-- | The bit rate of the audio stream in the output file, in kilobits/second.
-- Enter an integer between 64 and 320, inclusive.
apBitRate :: Lens' AudioParameters (Maybe Text)
apBitRate = lens _apBitRate (\s a -> s { _apBitRate = a })

-- | The number of audio channels in the output file. Valid values include:
--
-- 'auto', '0', '1', '2'
--
-- If you specify 'auto', Elastic Transcoder automatically detects the number of
-- channels in the input file.
apChannels :: Lens' AudioParameters (Maybe Text)
apChannels = lens _apChannels (\s a -> s { _apChannels = a })

-- | The audio codec for the output file. Valid values include 'aac', 'mp3', and 'vorbis'
-- .
apCodec :: Lens' AudioParameters (Maybe Text)
apCodec = lens _apCodec (\s a -> s { _apCodec = a })

-- | If you specified 'AAC' for 'Audio:Codec', this is the 'AAC' compression profile to
-- use. Valid values include:
--
-- 'auto', 'AAC-LC', 'HE-AAC', 'HE-AACv2'
--
-- If you specify 'auto', Elastic Transcoder chooses a profile based on the bit
-- rate of the output file.
apCodecOptions :: Lens' AudioParameters (Maybe AudioCodecOptions)
apCodecOptions = lens _apCodecOptions (\s a -> s { _apCodecOptions = a })

-- | The sample rate of the audio stream in the output file, in Hertz. Valid
-- values include:
--
-- 'auto', '22050', '32000', '44100', '48000', '96000'
--
-- If you specify 'auto', Elastic Transcoder automatically detects the sample
-- rate.
apSampleRate :: Lens' AudioParameters (Maybe Text)
apSampleRate = lens _apSampleRate (\s a -> s { _apSampleRate = a })

instance FromJSON AudioParameters where
    parseJSON = withObject "AudioParameters" $ \o -> AudioParameters
        <$> o .:? "BitRate"
        <*> o .:? "Channels"
        <*> o .:? "Codec"
        <*> o .:? "CodecOptions"
        <*> o .:? "SampleRate"

instance ToJSON AudioParameters where
    toJSON AudioParameters{..} = object
        [ "Codec"        .= _apCodec
        , "SampleRate"   .= _apSampleRate
        , "BitRate"      .= _apBitRate
        , "Channels"     .= _apChannels
        , "CodecOptions" .= _apCodecOptions
        ]

data Thumbnails = Thumbnails
    { _tAspectRatio   :: Maybe Text
    , _tFormat        :: Maybe Text
    , _tInterval      :: Maybe Text
    , _tMaxHeight     :: Maybe Text
    , _tMaxWidth      :: Maybe Text
    , _tPaddingPolicy :: Maybe Text
    , _tResolution    :: Maybe Text
    , _tSizingPolicy  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Thumbnails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tAspectRatio' @::@ 'Maybe' 'Text'
--
-- * 'tFormat' @::@ 'Maybe' 'Text'
--
-- * 'tInterval' @::@ 'Maybe' 'Text'
--
-- * 'tMaxHeight' @::@ 'Maybe' 'Text'
--
-- * 'tMaxWidth' @::@ 'Maybe' 'Text'
--
-- * 'tPaddingPolicy' @::@ 'Maybe' 'Text'
--
-- * 'tResolution' @::@ 'Maybe' 'Text'
--
-- * 'tSizingPolicy' @::@ 'Maybe' 'Text'
--
thumbnails :: Thumbnails
thumbnails = Thumbnails
    { _tFormat        = Nothing
    , _tInterval      = Nothing
    , _tResolution    = Nothing
    , _tAspectRatio   = Nothing
    , _tMaxWidth      = Nothing
    , _tMaxHeight     = Nothing
    , _tSizingPolicy  = Nothing
    , _tPaddingPolicy = Nothing
    }

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values 'MaxWidth', 'MaxHeight', 'SizingPolicy', and 'PaddingPolicy'
-- instead of 'Resolution' and 'AspectRatio'. The two groups of settings are
-- mutually exclusive. Do not use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
--
-- 'auto', '1:1', '4:3', '3:2', '16:9'
--
-- If you specify 'auto', Elastic Transcoder tries to preserve the aspect ratio
-- of the video in the output file.
tAspectRatio :: Lens' Thumbnails (Maybe Text)
tAspectRatio = lens _tAspectRatio (\s a -> s { _tAspectRatio = a })

-- | The format of thumbnails, if any. Valid values are 'jpg' and 'png'.
--
-- You specify whether you want Elastic Transcoder to create thumbnails when
-- you create a job.
tFormat :: Lens' Thumbnails (Maybe Text)
tFormat = lens _tFormat (\s a -> s { _tFormat = a })

-- | The approximate number of seconds between thumbnails. Specify an integer
-- value.
tInterval :: Lens' Thumbnails (Maybe Text)
tInterval = lens _tInterval (\s a -> s { _tInterval = a })

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 3072.
tMaxHeight :: Lens' Thumbnails (Maybe Text)
tMaxHeight = lens _tMaxHeight (\s a -> s { _tMaxHeight = a })

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 4096.
tMaxWidth :: Lens' Thumbnails (Maybe Text)
tMaxWidth = lens _tMaxWidth (\s a -> s { _tMaxWidth = a })

-- | When you set 'PaddingPolicy' to 'Pad', Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of thumbnails to make the
-- total size of the thumbnails match the values that you specified for
-- thumbnail 'MaxWidth' and 'MaxHeight' settings.
tPaddingPolicy :: Lens' Thumbnails (Maybe Text)
tPaddingPolicy = lens _tPaddingPolicy (\s a -> s { _tPaddingPolicy = a })

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values 'MaxWidth', 'MaxHeight', 'SizingPolicy', and 'PaddingPolicy'
-- instead of 'Resolution' and 'AspectRatio'. The two groups of settings are
-- mutually exclusive. Do not use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in the
-- format '/width/ x '/height/ where both values are even integers. The values cannot
-- exceed the width and height that you specified in the 'Video:Resolution' object.
tResolution :: Lens' Thumbnails (Maybe Text)
tResolution = lens _tResolution (\s a -> s { _tResolution = a })

-- | Specify one of the following values to control scaling of thumbnails:
--
-- 'Fit': Elastic Transcoder scales thumbnails so they match the value that
-- you specified in thumbnail MaxWidth or MaxHeight settings without exceeding
-- the other value.   'Fill': Elastic Transcoder scales thumbnails so they match
-- the value that you specified in thumbnail 'MaxWidth' or 'MaxHeight' settings and
-- matches or exceeds the other value. Elastic Transcoder centers the image in
-- thumbnails and then crops in the dimension (if any) that exceeds the maximum
-- value.  'Stretch': Elastic Transcoder stretches thumbnails to match the values
-- that you specified for thumbnail 'MaxWidth' and 'MaxHeight' settings. If the
-- relative proportions of the input video and thumbnails are different, the
-- thumbnails will be distorted.  'Keep': Elastic Transcoder does not scale
-- thumbnails. If either dimension of the input video exceeds the values that
-- you specified for thumbnail 'MaxWidth' and 'MaxHeight' settings, Elastic
-- Transcoder crops the thumbnails.  'ShrinkToFit': Elastic Transcoder scales
-- thumbnails down so that their dimensions match the values that you specified
-- for at least one of thumbnail 'MaxWidth' and 'MaxHeight' without exceeding either
-- value. If you specify this option, Elastic Transcoder does not scale
-- thumbnails up.  'ShrinkToFill': Elastic Transcoder scales thumbnails down so
-- that their dimensions match the values that you specified for at least one of 'MaxWidth' and 'MaxHeight' without dropping below either value. If you specify
-- this option, Elastic Transcoder does not scale thumbnails up.
tSizingPolicy :: Lens' Thumbnails (Maybe Text)
tSizingPolicy = lens _tSizingPolicy (\s a -> s { _tSizingPolicy = a })

instance FromJSON Thumbnails where
    parseJSON = withObject "Thumbnails" $ \o -> Thumbnails
        <$> o .:? "AspectRatio"
        <*> o .:? "Format"
        <*> o .:? "Interval"
        <*> o .:? "MaxHeight"
        <*> o .:? "MaxWidth"
        <*> o .:? "PaddingPolicy"
        <*> o .:? "Resolution"
        <*> o .:? "SizingPolicy"

instance ToJSON Thumbnails where
    toJSON Thumbnails{..} = object
        [ "Format"        .= _tFormat
        , "Interval"      .= _tInterval
        , "Resolution"    .= _tResolution
        , "AspectRatio"   .= _tAspectRatio
        , "MaxWidth"      .= _tMaxWidth
        , "MaxHeight"     .= _tMaxHeight
        , "SizingPolicy"  .= _tSizingPolicy
        , "PaddingPolicy" .= _tPaddingPolicy
        ]

data Encryption = Encryption
    { _eInitializationVector :: Maybe Text
    , _eKey                  :: Maybe Text
    , _eKeyMd5               :: Maybe Text
    , _eMode                 :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Encryption' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eInitializationVector' @::@ 'Maybe' 'Text'
--
-- * 'eKey' @::@ 'Maybe' 'Text'
--
-- * 'eKeyMd5' @::@ 'Maybe' 'Text'
--
-- * 'eMode' @::@ 'Maybe' 'Text'
--
encryption :: Encryption
encryption = Encryption
    { _eMode                 = Nothing
    , _eKey                  = Nothing
    , _eKeyMd5               = Nothing
    , _eInitializationVector = Nothing
    }

-- | The series of random bits created by a random bit generator, unique for every
-- encryption operation, that you used to encrypt your input files or that you
-- want Elastic Transcoder to use to encrypt your output files. The
-- initialization vector must be base64-encoded, and it must be exactly 16 bytes
-- long before being base64-encoded.
eInitializationVector :: Lens' Encryption (Maybe Text)
eInitializationVector =
    lens _eInitializationVector (\s a -> s { _eInitializationVector = a })

-- | The data encryption key that you want Elastic Transcoder to use to encrypt
-- your output file, or that was used to encrypt your input file. The key must
-- be base64-encoded and it must be one of the following bit lengths before
-- being base64-encoded:
--
-- '128', '192', or '256'.
--
-- The key must also be encrypted by using the Amazon Key Management Service.
eKey :: Lens' Encryption (Maybe Text)
eKey = lens _eKey (\s a -> s { _eKey = a })

-- | The MD5 digest of the key that you used to encrypt your input file, or that
-- you want Elastic Transcoder to use to encrypt your output file. Elastic
-- Transcoder uses the key digest as a checksum to make sure your key was not
-- corrupted in transit. The key MD5 must be base64-encoded, and it must be
-- exactly 16 bytes long before being base64-encoded.
eKeyMd5 :: Lens' Encryption (Maybe Text)
eKeyMd5 = lens _eKeyMd5 (\s a -> s { _eKeyMd5 = a })

-- | The specific server-side encryption mode that you want Elastic Transcoder to
-- use when decrypting your input files or encrypting your output files. Elastic
-- Transcoder supports the following options:
--
-- S3: Amazon S3 creates and manages the keys used for encrypting your files.
--
-- S3-AWS-KMS: Amazon S3 calls the Amazon Key Management Service, which creates
-- and manages the keys that are used for encrypting your files. If you specify 'S3-AWS-KMS' and you don't want to use the default key, you must add the AWS-KMS key that
-- you want to use to your pipeline.
--
-- AES-CBC-PKCS7: A padded cipher-block mode of operation originally used for
-- HLS files.
--
-- AES-CTR: AES Counter Mode.
--
-- AES-GCM: AES Galois Counter Mode, a mode of operation that is an
-- authenticated encryption format, meaning that a file, key, or initialization
-- vector that has been tampered with will fail the decryption process.
--
-- For all three AES options, you must provide the following settings, which
-- must be base64-encoded:
--
-- Key
--
-- Key MD5
--
-- Initialization Vector
--
-- For the AES modes, your private encryption keys and your unencrypted data
-- are never stored by AWS; therefore, it is important that you safely manage
-- your encryption keys. If you lose them, you won't be able to unencrypt your
-- data.
--
eMode :: Lens' Encryption (Maybe Text)
eMode = lens _eMode (\s a -> s { _eMode = a })

instance FromJSON Encryption where
    parseJSON = withObject "Encryption" $ \o -> Encryption
        <$> o .:? "InitializationVector"
        <*> o .:? "Key"
        <*> o .:? "KeyMd5"
        <*> o .:? "Mode"

instance ToJSON Encryption where
    toJSON Encryption{..} = object
        [ "Mode"                 .= _eMode
        , "Key"                  .= _eKey
        , "KeyMd5"               .= _eKeyMd5
        , "InitializationVector" .= _eInitializationVector
        ]

data JobAlbumArt = JobAlbumArt
    { _jaaArtwork     :: List "Artwork" Artwork
    , _jaaMergePolicy :: Maybe Text
    } deriving (Eq, Show)

-- | 'JobAlbumArt' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jaaArtwork' @::@ ['Artwork']
--
-- * 'jaaMergePolicy' @::@ 'Maybe' 'Text'
--
jobAlbumArt :: JobAlbumArt
jobAlbumArt = JobAlbumArt
    { _jaaMergePolicy = Nothing
    , _jaaArtwork     = mempty
    }

-- | The file to be used as album art. There can be multiple artworks associated
-- with an audio file, to a maximum of 20. Valid formats are '.jpg' and '.png'
jaaArtwork :: Lens' JobAlbumArt [Artwork]
jaaArtwork = lens _jaaArtwork (\s a -> s { _jaaArtwork = a }) . _List

-- | A policy that determines how Elastic Transcoder will handle the existence of
-- multiple album artwork files.
--
-- 'Replace:' The specified album art will replace any existing album art.  'Prepend:' The specified album art will be placed in front of any existing album art.  'Append:' The specified album art will be placed after any existing album art.  'Fallback:' If the original input file contains artwork, Elastic Transcoder will use
-- that artwork for the output. If the original input does not contain artwork,
-- Elastic Transcoder will use the specified album art file.
jaaMergePolicy :: Lens' JobAlbumArt (Maybe Text)
jaaMergePolicy = lens _jaaMergePolicy (\s a -> s { _jaaMergePolicy = a })

instance FromJSON JobAlbumArt where
    parseJSON = withObject "JobAlbumArt" $ \o -> JobAlbumArt
        <$> o .:? "Artwork" .!= mempty
        <*> o .:? "MergePolicy"

instance ToJSON JobAlbumArt where
    toJSON JobAlbumArt{..} = object
        [ "MergePolicy" .= _jaaMergePolicy
        , "Artwork"     .= _jaaArtwork
        ]

data JobWatermark = JobWatermark
    { _jwEncryption        :: Maybe Encryption
    , _jwInputKey          :: Maybe Text
    , _jwPresetWatermarkId :: Maybe Text
    } deriving (Eq, Show)

-- | 'JobWatermark' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jwEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'jwInputKey' @::@ 'Maybe' 'Text'
--
-- * 'jwPresetWatermarkId' @::@ 'Maybe' 'Text'
--
jobWatermark :: JobWatermark
jobWatermark = JobWatermark
    { _jwPresetWatermarkId = Nothing
    , _jwInputKey          = Nothing
    , _jwEncryption        = Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your watermarks.
jwEncryption :: Lens' JobWatermark (Maybe Encryption)
jwEncryption = lens _jwEncryption (\s a -> s { _jwEncryption = a })

-- | The name of the .png or .jpg file that you want to use for the watermark. To
-- determine which Amazon S3 bucket contains the specified file, Elastic
-- Transcoder checks the pipeline specified by 'Pipeline'; the 'Input Bucket' object
-- in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, logos/128x64.png, include
-- the prefix in the key. If the file isn't in the specified bucket, Elastic
-- Transcoder returns an error.
jwInputKey :: Lens' JobWatermark (Maybe Text)
jwInputKey = lens _jwInputKey (\s a -> s { _jwInputKey = a })

-- | The ID of the watermark settings that Elastic Transcoder uses to add
-- watermarks to the video during transcoding. The settings are in the preset
-- specified by Preset for the current output. In that preset, the value of
-- Watermarks Id tells Elastic Transcoder which settings to use.
jwPresetWatermarkId :: Lens' JobWatermark (Maybe Text)
jwPresetWatermarkId =
    lens _jwPresetWatermarkId (\s a -> s { _jwPresetWatermarkId = a })

instance FromJSON JobWatermark where
    parseJSON = withObject "JobWatermark" $ \o -> JobWatermark
        <$> o .:? "Encryption"
        <*> o .:? "InputKey"
        <*> o .:? "PresetWatermarkId"

instance ToJSON JobWatermark where
    toJSON JobWatermark{..} = object
        [ "PresetWatermarkId" .= _jwPresetWatermarkId
        , "InputKey"          .= _jwInputKey
        , "Encryption"        .= _jwEncryption
        ]

data Pipeline = Pipeline
    { _pArn             :: Maybe Text
    , _pAwsKmsKeyArn    :: Maybe Text
    , _pContentConfig   :: Maybe PipelineOutputConfig
    , _pId              :: Maybe Text
    , _pInputBucket     :: Maybe Text
    , _pName            :: Maybe Text
    , _pNotifications   :: Maybe Notifications
    , _pOutputBucket    :: Maybe Text
    , _pRole            :: Maybe Text
    , _pStatus          :: Maybe Text
    , _pThumbnailConfig :: Maybe PipelineOutputConfig
    } deriving (Eq, Show)

-- | 'Pipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pArn' @::@ 'Maybe' 'Text'
--
-- * 'pAwsKmsKeyArn' @::@ 'Maybe' 'Text'
--
-- * 'pContentConfig' @::@ 'Maybe' 'PipelineOutputConfig'
--
-- * 'pId' @::@ 'Maybe' 'Text'
--
-- * 'pInputBucket' @::@ 'Maybe' 'Text'
--
-- * 'pName' @::@ 'Maybe' 'Text'
--
-- * 'pNotifications' @::@ 'Maybe' 'Notifications'
--
-- * 'pOutputBucket' @::@ 'Maybe' 'Text'
--
-- * 'pRole' @::@ 'Maybe' 'Text'
--
-- * 'pStatus' @::@ 'Maybe' 'Text'
--
-- * 'pThumbnailConfig' @::@ 'Maybe' 'PipelineOutputConfig'
--
pipeline :: Pipeline
pipeline = Pipeline
    { _pId              = Nothing
    , _pArn             = Nothing
    , _pName            = Nothing
    , _pStatus          = Nothing
    , _pInputBucket     = Nothing
    , _pOutputBucket    = Nothing
    , _pRole            = Nothing
    , _pAwsKmsKeyArn    = Nothing
    , _pNotifications   = Nothing
    , _pContentConfig   = Nothing
    , _pThumbnailConfig = Nothing
    }

-- | The Amazon Resource Name (ARN) for the pipeline.
pArn :: Lens' Pipeline (Maybe Text)
pArn = lens _pArn (\s a -> s { _pArn = a })

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this
-- pipeline.
--
-- If you use either 'S3' or 'S3-AWS-KMS' as your 'Encryption:Mode', you don't need
-- to provide a key with your job because a default key, known as an AWS-KMS
-- key, is created for you automatically. You need to provide an AWS-KMS key
-- only if you want to use a non-default AWS-KMS key, or if you are using an 'Encryption:Mode' of 'AES-PKCS7', 'AES-CTR', or 'AES-GCM'.
pAwsKmsKeyArn :: Lens' Pipeline (Maybe Text)
pAwsKmsKeyArn = lens _pAwsKmsKeyArn (\s a -> s { _pAwsKmsKeyArn = a })

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder
-- to save transcoded files and playlists. Either you specify both 'ContentConfig'
-- and 'ThumbnailConfig', or you specify 'OutputBucket'.
--
-- Bucket: The Amazon S3 bucket in which you want Elastic Transcoder to save
-- transcoded files and playlists.  Permissions: A list of the users and/or
-- predefined Amazon S3 groups you want to have access to transcoded files and
-- playlists, and the type of access that you want them to have.  GranteeType:
-- The type of value that appears in the 'Grantee' object:   'Canonical': Either the
-- canonical user ID for an AWS account or an origin access identity for an
-- Amazon CloudFront distribution.  'Email': The registered email address of an
-- AWS account.  'Group': One of the following predefined Amazon S3 groups: 'AllUsers', 'AuthenticatedUsers', or 'LogDelivery'.    'Grantee': The AWS user or group that
-- you want to have access to transcoded files and playlists.  'Access': The
-- permission that you want to give to the AWS user that is listed in 'Grantee'.
-- Valid values include:   'READ': The grantee can read the objects and metadata
-- for objects that Elastic Transcoder adds to the Amazon S3 bucket.  'READ_ACP':
-- The grantee can read the object ACL for objects that Elastic Transcoder adds
-- to the Amazon S3 bucket.  'WRITE_ACP': The grantee can write the ACL for the
-- objects that Elastic Transcoder adds to the Amazon S3 bucket.  'FULL_CONTROL':
-- The grantee has 'READ', 'READ_ACP', and 'WRITE_ACP' permissions for the objects
-- that Elastic Transcoder adds to the Amazon S3 bucket.      StorageClass: The
-- Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic
-- Transcoder to assign to the video files and playlists that it stores in your
-- Amazon S3 bucket.
pContentConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pContentConfig = lens _pContentConfig (\s a -> s { _pContentConfig = a })

-- | The identifier for the pipeline. You use this value to identify the pipeline
-- in which you want to perform a variety of operations, such as creating a job
-- or a preset.
pId :: Lens' Pipeline (Maybe Text)
pId = lens _pId (\s a -> s { _pId = a })

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for
-- transcoding and the graphics files, if any, that you want to use for
-- watermarks.
pInputBucket :: Lens' Pipeline (Maybe Text)
pInputBucket = lens _pInputBucket (\s a -> s { _pInputBucket = a })

-- | The name of the pipeline. We recommend that the name be unique within the AWS
-- account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
pName :: Lens' Pipeline (Maybe Text)
pName = lens _pName (\s a -> s { _pName = a })

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in the
-- Amazon SNS console.   Progressing (optional): The Amazon Simple Notification
-- Service (Amazon SNS) topic that you want to notify when Elastic Transcoder
-- has started to process the job.  Completed (optional): The Amazon SNS topic
-- that you want to notify when Elastic Transcoder has finished processing the
-- job.  Warning (optional): The Amazon SNS topic that you want to notify when
-- Elastic Transcoder encounters a warning condition.  Error (optional): The
-- Amazon SNS topic that you want to notify when Elastic Transcoder encounters
-- an error condition.
pNotifications :: Lens' Pipeline (Maybe Notifications)
pNotifications = lens _pNotifications (\s a -> s { _pNotifications = a })

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
-- files, thumbnails, and playlists. Either you specify this value, or you
-- specify both 'ContentConfig' and 'ThumbnailConfig'.
pOutputBucket :: Lens' Pipeline (Maybe Text)
pOutputBucket = lens _pOutputBucket (\s a -> s { _pOutputBucket = a })

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses
-- to transcode jobs for this pipeline.
pRole :: Lens' Pipeline (Maybe Text)
pRole = lens _pRole (\s a -> s { _pRole = a })

-- | The current status of the pipeline:
--
-- 'Active': The pipeline is processing jobs.  'Paused': The pipeline is not
-- currently processing jobs.
pStatus :: Lens' Pipeline (Maybe Text)
pStatus = lens _pStatus (\s a -> s { _pStatus = a })

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder
-- to save thumbnail files. Either you specify both 'ContentConfig' and 'ThumbnailConfig', or you specify 'OutputBucket'.
--
-- 'Bucket': The Amazon S3 bucket in which you want Elastic Transcoder to save
-- thumbnail files.   'Permissions': A list of the users and/or predefined Amazon
-- S3 groups you want to have access to thumbnail files, and the type of access
-- that you want them to have.  GranteeType: The type of value that appears in
-- the Grantee object:   'Canonical': Either the canonical user ID for an AWS
-- account or an origin access identity for an Amazon CloudFront distribution. A
-- canonical user ID is not the same as an AWS account number.   'Email': The
-- registered email address of an AWS account.  'Group': One of the following
-- predefined Amazon S3 groups: 'AllUsers', 'AuthenticatedUsers', or 'LogDelivery'.    'Grantee': The AWS user or group that you want to have access to thumbnail
-- files. Access: The permission that you want to give to the AWS user that is
-- listed in Grantee. Valid values include:   'READ': The grantee can read the
-- thumbnails and metadata for thumbnails that Elastic Transcoder adds to the
-- Amazon S3 bucket.  'READ_ACP': The grantee can read the object ACL for
-- thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.  'WRITE_ACP':
-- The grantee can write the ACL for the thumbnails that Elastic Transcoder adds
-- to the Amazon S3 bucket.  'FULL_CONTROL': The grantee has READ, READ_ACP, and
-- WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the
-- Amazon S3 bucket.      'StorageClass': The Amazon S3 storage class, 'Standard' or 'ReducedRedundancy', that you want Elastic Transcoder to assign to the
-- thumbnails that it stores in your Amazon S3 bucket.
pThumbnailConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pThumbnailConfig = lens _pThumbnailConfig (\s a -> s { _pThumbnailConfig = a })

instance FromJSON Pipeline where
    parseJSON = withObject "Pipeline" $ \o -> Pipeline
        <$> o .:? "Arn"
        <*> o .:? "AwsKmsKeyArn"
        <*> o .:? "ContentConfig"
        <*> o .:? "Id"
        <*> o .:? "InputBucket"
        <*> o .:? "Name"
        <*> o .:? "Notifications"
        <*> o .:? "OutputBucket"
        <*> o .:? "Role"
        <*> o .:? "Status"
        <*> o .:? "ThumbnailConfig"

instance ToJSON Pipeline where
    toJSON Pipeline{..} = object
        [ "Id"              .= _pId
        , "Arn"             .= _pArn
        , "Name"            .= _pName
        , "Status"          .= _pStatus
        , "InputBucket"     .= _pInputBucket
        , "OutputBucket"    .= _pOutputBucket
        , "Role"            .= _pRole
        , "AwsKmsKeyArn"    .= _pAwsKmsKeyArn
        , "Notifications"   .= _pNotifications
        , "ContentConfig"   .= _pContentConfig
        , "ThumbnailConfig" .= _pThumbnailConfig
        ]

data Preset = Preset
    { _p1Arn         :: Maybe Text
    , _p1Audio       :: Maybe AudioParameters
    , _p1Container   :: Maybe Text
    , _p1Description :: Maybe Text
    , _p1Id          :: Maybe Text
    , _p1Name        :: Maybe Text
    , _p1Thumbnails  :: Maybe Thumbnails
    , _p1Type        :: Maybe Text
    , _p1Video       :: Maybe VideoParameters
    } deriving (Eq, Show)

-- | 'Preset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'p1Arn' @::@ 'Maybe' 'Text'
--
-- * 'p1Audio' @::@ 'Maybe' 'AudioParameters'
--
-- * 'p1Container' @::@ 'Maybe' 'Text'
--
-- * 'p1Description' @::@ 'Maybe' 'Text'
--
-- * 'p1Id' @::@ 'Maybe' 'Text'
--
-- * 'p1Name' @::@ 'Maybe' 'Text'
--
-- * 'p1Thumbnails' @::@ 'Maybe' 'Thumbnails'
--
-- * 'p1Type' @::@ 'Maybe' 'Text'
--
-- * 'p1Video' @::@ 'Maybe' 'VideoParameters'
--
preset :: Preset
preset = Preset
    { _p1Id          = Nothing
    , _p1Arn         = Nothing
    , _p1Name        = Nothing
    , _p1Description = Nothing
    , _p1Container   = Nothing
    , _p1Audio       = Nothing
    , _p1Video       = Nothing
    , _p1Thumbnails  = Nothing
    , _p1Type        = Nothing
    }

-- | The Amazon Resource Name (ARN) for the preset.
p1Arn :: Lens' Preset (Maybe Text)
p1Arn = lens _p1Arn (\s a -> s { _p1Arn = a })

-- | A section of the response body that provides information about the audio
-- preset values.
p1Audio :: Lens' Preset (Maybe AudioParameters)
p1Audio = lens _p1Audio (\s a -> s { _p1Audio = a })

-- | The container type for the output file. Valid values include 'fmp4', 'mp3', 'mp4', 'ogg', 'ts', and 'webm'.
p1Container :: Lens' Preset (Maybe Text)
p1Container = lens _p1Container (\s a -> s { _p1Container = a })

-- | A description of the preset.
p1Description :: Lens' Preset (Maybe Text)
p1Description = lens _p1Description (\s a -> s { _p1Description = a })

-- | Identifier for the new preset. You use this value to get settings for the
-- preset or to delete it.
p1Id :: Lens' Preset (Maybe Text)
p1Id = lens _p1Id (\s a -> s { _p1Id = a })

-- | The name of the preset.
p1Name :: Lens' Preset (Maybe Text)
p1Name = lens _p1Name (\s a -> s { _p1Name = a })

-- | A section of the response body that provides information about the thumbnail
-- preset values, if any.
p1Thumbnails :: Lens' Preset (Maybe Thumbnails)
p1Thumbnails = lens _p1Thumbnails (\s a -> s { _p1Thumbnails = a })

-- | Whether the preset is a default preset provided by Elastic Transcoder ('System') or a preset that you have defined (
-- 'Custom').
p1Type :: Lens' Preset (Maybe Text)
p1Type = lens _p1Type (\s a -> s { _p1Type = a })

-- | A section of the response body that provides information about the video
-- preset values.
p1Video :: Lens' Preset (Maybe VideoParameters)
p1Video = lens _p1Video (\s a -> s { _p1Video = a })

instance FromJSON Preset where
    parseJSON = withObject "Preset" $ \o -> Preset
        <$> o .:? "Arn"
        <*> o .:? "Audio"
        <*> o .:? "Container"
        <*> o .:? "Description"
        <*> o .:? "Id"
        <*> o .:? "Name"
        <*> o .:? "Thumbnails"
        <*> o .:? "Type"
        <*> o .:? "Video"

instance ToJSON Preset where
    toJSON Preset{..} = object
        [ "Id"          .= _p1Id
        , "Arn"         .= _p1Arn
        , "Name"        .= _p1Name
        , "Description" .= _p1Description
        , "Container"   .= _p1Container
        , "Audio"       .= _p1Audio
        , "Video"       .= _p1Video
        , "Thumbnails"  .= _p1Thumbnails
        , "Type"        .= _p1Type
        ]

data CaptionFormat = CaptionFormat
    { _cfEncryption :: Maybe Encryption
    , _cfFormat     :: Maybe Text
    , _cfPattern    :: Maybe Text
    } deriving (Eq, Show)

-- | 'CaptionFormat' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'cfFormat' @::@ 'Maybe' 'Text'
--
-- * 'cfPattern' @::@ 'Maybe' 'Text'
--
captionFormat :: CaptionFormat
captionFormat = CaptionFormat
    { _cfFormat     = Nothing
    , _cfPattern    = Nothing
    , _cfEncryption = Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to
-- your caption formats.
cfEncryption :: Lens' CaptionFormat (Maybe Encryption)
cfEncryption = lens _cfEncryption (\s a -> s { _cfEncryption = a })

-- | The format you specify determines whether Elastic Transcoder generates an
-- embedded or sidecar caption for this output.
--
-- Valid Embedded Caption Formats:
--
-- For MP3: None
--
-- For MP4: mov-text
--
-- For MPEG-TS: None
--
-- For ogg: None
--
-- For webm: None
--
-- Valid Sidecar Caption Formats: Elastic Transcoder supports dfxp (first
-- div element only), scc, srt, and webvtt. If you want ttml or smpte-tt
-- compatible captions, specify dfxp as your output format.
--
-- For FMP4: dfxp
--
-- Non-FMP4 outputs: All sidecar types
--
-- 'fmp4' captions have an extension of '.ismt'
--
--
cfFormat :: Lens' CaptionFormat (Maybe Text)
cfFormat = lens _cfFormat (\s a -> s { _cfFormat = a })

-- | The prefix for caption filenames, in the form /description/-'{language}', where:
--
-- /description/ is a description of the video.  '{language}' is a literal value
-- that Elastic Transcoder replaces with the two- or three-letter code for the
-- language of the caption in the output file names.  If you don't include '{language}' in the file name pattern, Elastic Transcoder automatically appends "'{language}'" to the value that you specify for the description. In addition, Elastic
-- Transcoder automatically appends the count to the end of the segment files.
--
-- For example, suppose you're transcoding into srt format. When you enter
-- "Sydney-{language}-sunrise", and the language of the captions is English
-- (en), the name of the first caption file will be Sydney-en-sunrise00000.srt.
cfPattern :: Lens' CaptionFormat (Maybe Text)
cfPattern = lens _cfPattern (\s a -> s { _cfPattern = a })

instance FromJSON CaptionFormat where
    parseJSON = withObject "CaptionFormat" $ \o -> CaptionFormat
        <$> o .:? "Encryption"
        <*> o .:? "Format"
        <*> o .:? "Pattern"

instance ToJSON CaptionFormat where
    toJSON CaptionFormat{..} = object
        [ "Format"     .= _cfFormat
        , "Pattern"    .= _cfPattern
        , "Encryption" .= _cfEncryption
        ]

data PresetWatermark = PresetWatermark
    { _pwHorizontalAlign  :: Maybe Text
    , _pwHorizontalOffset :: Maybe Text
    , _pwId               :: Maybe Text
    , _pwMaxHeight        :: Maybe Text
    , _pwMaxWidth         :: Maybe Text
    , _pwOpacity          :: Maybe Text
    , _pwSizingPolicy     :: Maybe Text
    , _pwTarget           :: Maybe Text
    , _pwVerticalAlign    :: Maybe Text
    , _pwVerticalOffset   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'PresetWatermark' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pwHorizontalAlign' @::@ 'Maybe' 'Text'
--
-- * 'pwHorizontalOffset' @::@ 'Maybe' 'Text'
--
-- * 'pwId' @::@ 'Maybe' 'Text'
--
-- * 'pwMaxHeight' @::@ 'Maybe' 'Text'
--
-- * 'pwMaxWidth' @::@ 'Maybe' 'Text'
--
-- * 'pwOpacity' @::@ 'Maybe' 'Text'
--
-- * 'pwSizingPolicy' @::@ 'Maybe' 'Text'
--
-- * 'pwTarget' @::@ 'Maybe' 'Text'
--
-- * 'pwVerticalAlign' @::@ 'Maybe' 'Text'
--
-- * 'pwVerticalOffset' @::@ 'Maybe' 'Text'
--
presetWatermark :: PresetWatermark
presetWatermark = PresetWatermark
    { _pwId               = Nothing
    , _pwMaxWidth         = Nothing
    , _pwMaxHeight        = Nothing
    , _pwSizingPolicy     = Nothing
    , _pwHorizontalAlign  = Nothing
    , _pwHorizontalOffset = Nothing
    , _pwVerticalAlign    = Nothing
    , _pwVerticalOffset   = Nothing
    , _pwOpacity          = Nothing
    , _pwTarget           = Nothing
    }

-- | The horizontal position of the watermark unless you specify a non-zero value
-- for 'HorizontalOffset':   Left: The left edge of the watermark is aligned with
-- the left border of the video.  Right: The right edge of the watermark is
-- aligned with the right border of the video.  Center: The watermark is
-- centered between the left and right borders.
pwHorizontalAlign :: Lens' PresetWatermark (Maybe Text)
pwHorizontalAlign =
    lens _pwHorizontalAlign (\s a -> s { _pwHorizontalAlign = a })

-- | The amount by which you want the horizontal position of the watermark to be
-- offset from the position specified by HorizontalAlign:  number of pixels
-- (px): The minimum value is 0 pixels, and the maximum value is the value of
-- MaxWidth. integer percentage (%): The range of valid values is 0 to 100. For
-- example, if you specify Left for 'HorizontalAlign' and 5px for 'HorizontalOffset', the left side of the watermark appears 5 pixels from the left border of the output video.
--
--
-- 'HorizontalOffset' is only valid when the value of 'HorizontalAlign' is 'Left' or 'Right'. If you specify an offset that causes the watermark to extend beyond the
-- left or right border and Elastic Transcoder has not added black bars, the
-- watermark is cropped. If Elastic Transcoder has added black bars, the
-- watermark extends into the black bars. If the watermark extends beyond the
-- black bars, it is cropped.
--
-- Use the value of 'Target' to specify whether you want to include the black
-- bars that are added by Elastic Transcoder, if any, in the offset calculation.
pwHorizontalOffset :: Lens' PresetWatermark (Maybe Text)
pwHorizontalOffset =
    lens _pwHorizontalOffset (\s a -> s { _pwHorizontalOffset = a })

-- | A unique identifier for the settings for one watermark. The value of 'Id' can
-- be up to 40 characters long.
pwId :: Lens' PresetWatermark (Maybe Text)
pwId = lens _pwId (\s a -> s { _pwId = a })

-- | The maximum height of the watermark in one of the following formats:  number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is the
-- value of 'MaxHeight'. integer percentage (%): The range of valid values is 0 to
-- 100. Use the value of 'Target' to specify whether you want Elastic Transcoder
-- to include the black bars that are added by Elastic Transcoder, if any, in
-- the calculation.  If you specify the value in pixels, it must be less than or
-- equal to the value of 'MaxHeight'.
pwMaxHeight :: Lens' PresetWatermark (Maybe Text)
pwMaxHeight = lens _pwMaxHeight (\s a -> s { _pwMaxHeight = a })

-- | The maximum width of the watermark in one of the following formats:  number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is the
-- value of 'MaxWidth'. integer percentage (%): The range of valid values is 0 to
-- 100. Use the value of 'Target' to specify whether you want Elastic Transcoder
-- to include the black bars that are added by Elastic Transcoder, if any, in
-- the calculation. If you specify the value in pixels, it must be less than or
-- equal to the value of 'MaxWidth'.
pwMaxWidth :: Lens' PresetWatermark (Maybe Text)
pwMaxWidth = lens _pwMaxWidth (\s a -> s { _pwMaxWidth = a })

-- | A percentage that indicates how much you want a watermark to obscure the
-- video in the location where it appears. Valid values are 0 (the watermark is
-- invisible) to 100 (the watermark completely obscures the video in the
-- specified location). The datatype of 'Opacity' is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a
-- transparent .png, the transparent portion of the video appears as if you had
-- specified a value of 0 for 'Opacity'. The .jpg file format doesn't support
-- transparency.
pwOpacity :: Lens' PresetWatermark (Maybe Text)
pwOpacity = lens _pwOpacity (\s a -> s { _pwOpacity = a })

-- | A value that controls scaling of the watermark:   Fit: Elastic Transcoder
-- scales the watermark so it matches the value that you specified in either 'MaxWidth' or 'MaxHeight' without exceeding the other value.  Stretch: Elastic Transcoder
-- stretches the watermark to match the values that you specified for 'MaxWidth'
-- and 'MaxHeight'. If the relative proportions of the watermark and the values of 'MaxWidth' and 'MaxHeight' are different, the watermark will be distorted.  ShrinkToFit
-- : Elastic Transcoder scales the watermark down so that its dimensions match
-- the values that you specified for at least one of 'MaxWidth' and 'MaxHeight'
-- without exceeding either value. If you specify this option, Elastic
-- Transcoder does not scale the watermark up.
pwSizingPolicy :: Lens' PresetWatermark (Maybe Text)
pwSizingPolicy = lens _pwSizingPolicy (\s a -> s { _pwSizingPolicy = a })

-- | A value that determines how Elastic Transcoder interprets values that you
-- specified for 'HorizontalOffset', 'VerticalOffset', 'MaxWidth', and 'MaxHeight':   Content
-- : 'HorizontalOffset' and 'VerticalOffset' values are calculated based on the
-- borders of the video excluding black bars added by Elastic Transcoder, if
-- any. In addition, 'MaxWidth' and 'MaxHeight', if specified as a percentage, are
-- calculated based on the borders of the video excluding black bars added by
-- Elastic Transcoder, if any.  Frame: 'HorizontalOffset' and 'VerticalOffset'
-- values are calculated based on the borders of the video including black bars
-- added by Elastic Transcoder, if any. In addition, 'MaxWidth' and 'MaxHeight', if
-- specified as a percentage, are calculated based on the borders of the video
-- including black bars added by Elastic Transcoder, if any.
pwTarget :: Lens' PresetWatermark (Maybe Text)
pwTarget = lens _pwTarget (\s a -> s { _pwTarget = a })

-- | The vertical position of the watermark unless you specify a non-zero value
-- for 'VerticalOffset':   Top: The top edge of the watermark is aligned with the
-- top border of the video.  Bottom: The bottom edge of the watermark is aligned
-- with the bottom border of the video.  Center: The watermark is centered
-- between the top and bottom borders.
pwVerticalAlign :: Lens' PresetWatermark (Maybe Text)
pwVerticalAlign = lens _pwVerticalAlign (\s a -> s { _pwVerticalAlign = a })

-- | 'VerticalOffset' The amount by which you want the vertical position of the
-- watermark to be offset from the position specified by VerticalAlign: number
-- of pixels (px): The minimum value is 0 pixels, and the maximum value is the
-- value of 'MaxHeight'. integer percentage (%): The range of valid values is 0 to
-- 100.  For example, if you specify 'Top' for 'VerticalAlign' and '5px' for 'VerticalOffset', the top of the watermark appears 5 pixels from the top border of the output
-- video.
--
-- 'VerticalOffset' is only valid when the value of VerticalAlign is Top or
-- Bottom.
--
-- If you specify an offset that causes the watermark to extend beyond the top
-- or bottom border and Elastic Transcoder has not added black bars, the
-- watermark is cropped. If Elastic Transcoder has added black bars, the
-- watermark extends into the black bars. If the watermark extends beyond the
-- black bars, it is cropped.
--
-- Use the value of 'Target' to specify whether you want Elastic Transcoder to
-- include the black bars that are added by Elastic Transcoder, if any, in the
-- offset calculation.
pwVerticalOffset :: Lens' PresetWatermark (Maybe Text)
pwVerticalOffset = lens _pwVerticalOffset (\s a -> s { _pwVerticalOffset = a })

instance FromJSON PresetWatermark where
    parseJSON = withObject "PresetWatermark" $ \o -> PresetWatermark
        <$> o .:? "HorizontalAlign"
        <*> o .:? "HorizontalOffset"
        <*> o .:? "Id"
        <*> o .:? "MaxHeight"
        <*> o .:? "MaxWidth"
        <*> o .:? "Opacity"
        <*> o .:? "SizingPolicy"
        <*> o .:? "Target"
        <*> o .:? "VerticalAlign"
        <*> o .:? "VerticalOffset"

instance ToJSON PresetWatermark where
    toJSON PresetWatermark{..} = object
        [ "Id"               .= _pwId
        , "MaxWidth"         .= _pwMaxWidth
        , "MaxHeight"        .= _pwMaxHeight
        , "SizingPolicy"     .= _pwSizingPolicy
        , "HorizontalAlign"  .= _pwHorizontalAlign
        , "HorizontalOffset" .= _pwHorizontalOffset
        , "VerticalAlign"    .= _pwVerticalAlign
        , "VerticalOffset"   .= _pwVerticalOffset
        , "Opacity"          .= _pwOpacity
        , "Target"           .= _pwTarget
        ]

data Permission = Permission
    { _pAccess      :: List "Access" Text
    , _pGrantee     :: Maybe Text
    , _pGranteeType :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Permission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAccess' @::@ ['Text']
--
-- * 'pGrantee' @::@ 'Maybe' 'Text'
--
-- * 'pGranteeType' @::@ 'Maybe' 'Text'
--
permission :: Permission
permission = Permission
    { _pGranteeType = Nothing
    , _pGrantee     = Nothing
    , _pAccess      = mempty
    }

-- | The permission that you want to give to the AWS user that is listed in
-- Grantee. Valid values include:   'READ': The grantee can read the thumbnails
-- and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3
-- bucket.  'READ_ACP': The grantee can read the object ACL for thumbnails that
-- Elastic Transcoder adds to the Amazon S3 bucket.  'WRITE_ACP': The grantee can
-- write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket.  'FULL_CONTROL': The grantee has READ, READ_ACP, and WRITE_ACP
-- permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3
-- bucket.
pAccess :: Lens' Permission [Text]
pAccess = lens _pAccess (\s a -> s { _pAccess = a }) . _List

-- | The AWS user or group that you want to have access to transcoded files and
-- playlists. To identify the user or group, you can specify the canonical user
-- ID for an AWS account, an origin access identity for a CloudFront
-- distribution, the registered email address of an AWS account, or a predefined
-- Amazon S3 group.
pGrantee :: Lens' Permission (Maybe Text)
pGrantee = lens _pGrantee (\s a -> s { _pGrantee = a })

-- | The type of value that appears in the Grantee object:   'Canonical': Either the
-- canonical user ID for an AWS account or an origin access identity for an
-- Amazon CloudFront distribution. A canonical user ID is not the same as an AWS
-- account number.   'Email': The registered email address of an AWS account.  'Group': One of the following predefined Amazon S3 groups: 'AllUsers', 'AuthenticatedUsers', or 'LogDelivery'.
pGranteeType :: Lens' Permission (Maybe Text)
pGranteeType = lens _pGranteeType (\s a -> s { _pGranteeType = a })

instance FromJSON Permission where
    parseJSON = withObject "Permission" $ \o -> Permission
        <$> o .:? "Access" .!= mempty
        <*> o .:? "Grantee"
        <*> o .:? "GranteeType"

instance ToJSON Permission where
    toJSON Permission{..} = object
        [ "GranteeType" .= _pGranteeType
        , "Grantee"     .= _pGrantee
        , "Access"      .= _pAccess
        ]

data VideoParameters = VideoParameters
    { _vpAspectRatio        :: Maybe Text
    , _vpBitRate            :: Maybe Text
    , _vpCodec              :: Maybe Text
    , _vpCodecOptions       :: Map Text Text
    , _vpDisplayAspectRatio :: Maybe Text
    , _vpFixedGOP           :: Maybe Text
    , _vpFrameRate          :: Maybe Text
    , _vpKeyframesMaxDist   :: Maybe Text
    , _vpMaxFrameRate       :: Maybe Text
    , _vpMaxHeight          :: Maybe Text
    , _vpMaxWidth           :: Maybe Text
    , _vpPaddingPolicy      :: Maybe Text
    , _vpResolution         :: Maybe Text
    , _vpSizingPolicy       :: Maybe Text
    , _vpWatermarks         :: List "Watermarks" PresetWatermark
    } deriving (Eq, Show)

-- | 'VideoParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpAspectRatio' @::@ 'Maybe' 'Text'
--
-- * 'vpBitRate' @::@ 'Maybe' 'Text'
--
-- * 'vpCodec' @::@ 'Maybe' 'Text'
--
-- * 'vpCodecOptions' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'vpDisplayAspectRatio' @::@ 'Maybe' 'Text'
--
-- * 'vpFixedGOP' @::@ 'Maybe' 'Text'
--
-- * 'vpFrameRate' @::@ 'Maybe' 'Text'
--
-- * 'vpKeyframesMaxDist' @::@ 'Maybe' 'Text'
--
-- * 'vpMaxFrameRate' @::@ 'Maybe' 'Text'
--
-- * 'vpMaxHeight' @::@ 'Maybe' 'Text'
--
-- * 'vpMaxWidth' @::@ 'Maybe' 'Text'
--
-- * 'vpPaddingPolicy' @::@ 'Maybe' 'Text'
--
-- * 'vpResolution' @::@ 'Maybe' 'Text'
--
-- * 'vpSizingPolicy' @::@ 'Maybe' 'Text'
--
-- * 'vpWatermarks' @::@ ['PresetWatermark']
--
videoParameters :: VideoParameters
videoParameters = VideoParameters
    { _vpCodec              = Nothing
    , _vpCodecOptions       = mempty
    , _vpKeyframesMaxDist   = Nothing
    , _vpFixedGOP           = Nothing
    , _vpBitRate            = Nothing
    , _vpFrameRate          = Nothing
    , _vpMaxFrameRate       = Nothing
    , _vpResolution         = Nothing
    , _vpAspectRatio        = Nothing
    , _vpMaxWidth           = Nothing
    , _vpMaxHeight          = Nothing
    , _vpDisplayAspectRatio = Nothing
    , _vpSizingPolicy       = Nothing
    , _vpPaddingPolicy      = Nothing
    , _vpWatermarks         = mempty
    }

-- | To better control resolution and aspect ratio of output videos, we recommend
-- that you use the values 'MaxWidth', 'MaxHeight', 'SizingPolicy', 'PaddingPolicy', and 'DisplayAspectRatio' instead of 'Resolution' and 'AspectRatio'. The two groups of
-- settings are mutually exclusive. Do not use them together.
--
-- The display aspect ratio of the video in the output file. Valid values
-- include:
--
-- 'auto', '1:1', '4:3', '3:2', '16:9'
--
-- If you specify 'auto', Elastic Transcoder tries to preserve the aspect ratio
-- of the input file.
--
-- If you specify an aspect ratio for the output file that differs from aspect
-- ratio of the input file, Elastic Transcoder adds pillarboxing (black bars on
-- the sides) or letterboxing (black bars on the top and bottom) to maintain the
-- aspect ratio of the active region of the video.
vpAspectRatio :: Lens' VideoParameters (Maybe Text)
vpAspectRatio = lens _vpAspectRatio (\s a -> s { _vpAspectRatio = a })

-- | The bit rate of the video stream in the output file, in kilobits/second.
-- Valid values depend on the values of 'Level' and 'Profile'. If you specify 'auto',
-- Elastic Transcoder uses the detected bit rate of the input source. If you
-- specify a value other than 'auto', we recommend that you specify a value less
-- than or equal to the maximum H.264-compliant value listed for your level and
-- profile:
--
-- /Level - Maximum video bit rate in kilobits/second (baseline and mainProfile) : maximum video bit rate in kilobits/second (high Profile)/
--
-- 1 - 64 : 80 1b - 128 : 160 1.1 - 192 : 240 1.2 - 384 : 480 1.3 - 768 : 960 2 - 2000 : 2500
-- 3 - 10000 : 12500 3.1 - 14000 : 17500 3.2 - 20000 : 25000 4 - 20000 : 25000 4.1 - 50000 : 62500
--
vpBitRate :: Lens' VideoParameters (Maybe Text)
vpBitRate = lens _vpBitRate (\s a -> s { _vpBitRate = a })

-- | The video codec for the output file. Valid values include 'H.264' and 'vp8'. You
-- can only specify 'vp8' when the container type is 'webm'.
vpCodec :: Lens' VideoParameters (Maybe Text)
vpCodec = lens _vpCodec (\s a -> s { _vpCodec = a })

-- | Profile
--
-- The H.264 profile that you want to use for the output file. Elastic
-- Transcoder supports the following profiles:
--
-- 'baseline': The profile most commonly used for videoconferencing and for
-- mobile applications.  'main': The profile used for standard-definition digital
-- TV broadcasts.  'high': The profile used for high-definition digital TV
-- broadcasts and for Blu-ray discs.   Level (H.264 Only)
--
-- The H.264 level that you want to use for the output file. Elastic Transcoder
-- supports the following levels:
--
-- '1', '1b', '1.1', '1.2', '1.3', '2', '2.1', '2.2', '3', '3.1', '3.2', '4', '4.1'
--
-- MaxReferenceFrames (H.264 Only)
--
-- Applicable only when the value of Video:Codec is H.264. The maximum number
-- of previously decoded frames to use as a reference for decoding future
-- frames. Valid values are integers 0 through 16, but we recommend that you not
-- use a value greater than the following:
--
-- 'Min(Floor(Maximum decoded picture buffer in macroblocks * 256 / (Width inpixels * Height in pixels)), 16)'
--
-- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth and
-- MaxHeight, or Resolution. /Maximum decoded picture buffer in macroblocks/
-- depends on the value of the 'Level' object. See the list below. (A macroblock
-- is a block of pixels measuring 16x16.)
--
-- 1 - 396 1b - 396 1.1 - 900 1.2 - 2376 1.3 - 2376 2 - 2376 2.1 - 4752 2.2 -
-- 8100 3 - 8100 3.1 - 18000 3.2 - 20480 4 - 32768 4.1 - 32768   MaxBitRate
--
-- The maximum number of bits per second in a video buffer; the size of the
-- buffer is specified by 'BufferSize'. Specify a value between 16 and 62,500. You
-- can reduce the bandwidth required to stream a video by reducing the maximum
-- bit rate, but this also reduces the quality of the video.
--
-- BufferSize
--
-- The maximum number of bits in any x seconds of the output video. This window
-- is commonly 10 seconds, the standard segment duration when you're using FMP4
-- or MPEG-TS for the container type of the output video. Specify an integer
-- greater than 0. If you specify 'MaxBitRate' and omit 'BufferSize', Elastic
-- Transcoder sets 'BufferSize' to 10 times the value of 'MaxBitRate'.
vpCodecOptions :: Lens' VideoParameters (HashMap Text Text)
vpCodecOptions = lens _vpCodecOptions (\s a -> s { _vpCodecOptions = a }) . _Map

-- | The value that Elastic Transcoder adds to the metadata in the output file.
vpDisplayAspectRatio :: Lens' VideoParameters (Maybe Text)
vpDisplayAspectRatio =
    lens _vpDisplayAspectRatio (\s a -> s { _vpDisplayAspectRatio = a })

-- | Whether to use a fixed value for 'FixedGOP'. Valid values are 'true' and 'false':
--
-- 'true': Elastic Transcoder uses the value of 'KeyframesMaxDist' for the
-- distance between key frames (the number of frames in a group of pictures, or
-- GOP).  'false': The distance between key frames can vary.  'FixedGOP' must be set
-- to 'true' for 'fmp4' containers.
--
vpFixedGOP :: Lens' VideoParameters (Maybe Text)
vpFixedGOP = lens _vpFixedGOP (\s a -> s { _vpFixedGOP = a })

-- | The frames per second for the video stream in the output file. Valid values
-- include:
--
-- 'auto', '10', '15', '23.97', '24', '25', '29.97', '30', '60'
--
-- If you specify 'auto', Elastic Transcoder uses the detected frame rate of the
-- input source. If you specify a frame rate, we recommend that you perform the
-- following calculation:
--
-- 'Frame rate = maximum recommended decoding speed in luma samples/second /(width in pixels * height in pixels)'
--
-- where:
--
-- /width in pixels/ and /height in pixels/ represent the Resolution of the
-- output video.  /maximum recommended decoding speed in Luma samples/second/ is
-- less than or equal to the maximum value listed in the following table, based
-- on the value that you specified for Level.  The maximum recommended decoding
-- speed in Luma samples/second for each level is described in the following
-- list (/Level - Decoding speed/):
--
-- 1 - 380160 1b - 380160 1.1 - 76800 1.2 - 1536000 1.3 - 3041280 2 - 3041280 2.1 - 5068800
-- 2.2 - 5184000 3 - 10368000 3.1 - 27648000 3.2 - 55296000 4 - 62914560 4.1 -
-- 62914560
vpFrameRate :: Lens' VideoParameters (Maybe Text)
vpFrameRate = lens _vpFrameRate (\s a -> s { _vpFrameRate = a })

-- | The maximum number of frames between key frames. Key frames are fully encoded
-- frames; the frames between key frames are encoded based, in part, on the
-- content of the key frames. The value is an integer formatted as a string;
-- valid values are between 1 (every frame is a key frame) and 100000,
-- inclusive. A higher value results in higher compression but may also
-- discernibly decrease video quality.
--
-- For 'Smooth' outputs, the 'FrameRate' must have a constant ratio to the 'KeyframesMaxDist'. This allows 'Smooth' playlists to switch between different quality levels
-- while the file is being played.
--
-- For example, an input file can have a 'FrameRate' of 30 with a 'KeyframesMaxDist'
-- of 90. The output file then needs to have a ratio of 1:3. Valid outputs
-- would have 'FrameRate' of 30, 25, and 10, and 'KeyframesMaxDist' of 90, 75, and
-- 30, respectively.
--
-- Alternately, this can be achieved by setting 'FrameRate' to auto and having
-- the same values for 'MaxFrameRate' and 'KeyframesMaxDist'.
vpKeyframesMaxDist :: Lens' VideoParameters (Maybe Text)
vpKeyframesMaxDist =
    lens _vpKeyframesMaxDist (\s a -> s { _vpKeyframesMaxDist = a })

-- | If you specify 'auto' for 'FrameRate', Elastic Transcoder uses the frame rate of
-- the input video for the frame rate of the output video. Specify the maximum
-- frame rate that you want Elastic Transcoder to use when the frame rate of the
-- input video is greater than the desired maximum frame rate of the output
-- video. Valid values include: '10', '15', '23.97', '24', '25', '29.97', '30', '60'.
vpMaxFrameRate :: Lens' VideoParameters (Maybe Text)
vpMaxFrameRate = lens _vpMaxFrameRate (\s a -> s { _vpMaxFrameRate = a })

-- | The maximum height of the output video in pixels. If you specify 'auto',
-- Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 96 and 3072.
vpMaxHeight :: Lens' VideoParameters (Maybe Text)
vpMaxHeight = lens _vpMaxHeight (\s a -> s { _vpMaxHeight = a })

-- | The maximum width of the output video in pixels. If you specify 'auto',
-- Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 128 and 4096.
vpMaxWidth :: Lens' VideoParameters (Maybe Text)
vpMaxWidth = lens _vpMaxWidth (\s a -> s { _vpMaxWidth = a })

-- | When you set 'PaddingPolicy' to 'Pad', Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of the output video to make
-- the total size of the output video match the values that you specified for 'MaxWidth' and 'MaxHeight'.
vpPaddingPolicy :: Lens' VideoParameters (Maybe Text)
vpPaddingPolicy = lens _vpPaddingPolicy (\s a -> s { _vpPaddingPolicy = a })

-- | To better control resolution and aspect ratio of output videos, we recommend
-- that you use the values 'MaxWidth', 'MaxHeight', 'SizingPolicy', 'PaddingPolicy', and 'DisplayAspectRatio' instead of 'Resolution' and 'AspectRatio'. The two groups of
-- settings are mutually exclusive. Do not use them together.
--
-- The width and height of the video in the output file, in pixels. Valid
-- values are 'auto' and /width/ x /height/:
--
-- 'auto': Elastic Transcoder attempts to preserve the width and height of the
-- input file, subject to the following rules.  '/width/ x /height/: The width and
-- height of the output video in pixels.  Note the following about specifying
-- the width and height:
--
-- The width must be an even integer between 128 and 4096, inclusive. The
-- height must be an even integer between 96 and 3072, inclusive. If you specify
-- a resolution that is less than the resolution of the input file, Elastic
-- Transcoder rescales the output file to the lower resolution. If you specify a
-- resolution that is greater than the resolution of the input file, Elastic
-- Transcoder rescales the output to the higher resolution. We recommend that
-- you specify a resolution for which the product of width and height is less
-- than or equal to the applicable value in the following list (/List - Max widthx height value/):  1 - 25344 1b - 25344 1.1 - 101376 1.2 - 101376 1.3 - 101376
-- 2 - 101376 2.1 - 202752 2.2 - 404720 3 - 404720 3.1 - 921600 3.2 - 1310720 4
-- - 2097152 4.1 - 2097152
vpResolution :: Lens' VideoParameters (Maybe Text)
vpResolution = lens _vpResolution (\s a -> s { _vpResolution = a })

-- | Specify one of the following values to control scaling of the output video:
--
-- 'Fit': Elastic Transcoder scales the output video so it matches the value
-- that you specified in either 'MaxWidth' or 'MaxHeight' without exceeding the
-- other value.  'Fill': Elastic Transcoder scales the output video so it matches
-- the value that you specified in either 'MaxWidth' or 'MaxHeight' and matches or
-- exceeds the other value. Elastic Transcoder centers the output video and then
-- crops it in the dimension (if any) that exceeds the maximum value.  'Stretch':
-- Elastic Transcoder stretches the output video to match the values that you
-- specified for 'MaxWidth' and 'MaxHeight'. If the relative proportions of the
-- input video and the output video are different, the output video will be
-- distorted.  'Keep': Elastic Transcoder does not scale the output video. If
-- either dimension of the input video exceeds the values that you specified for 'MaxWidth' and 'MaxHeight', Elastic Transcoder crops the output video.  'ShrinkToFit': Elastic Transcoder scales the output video down so that its dimensions
-- match the values that you specified for at least one of 'MaxWidth' and 'MaxHeight'
-- without exceeding either value. If you specify this option, Elastic
-- Transcoder does not scale the video up.  'ShrinkToFill': Elastic Transcoder
-- scales the output video down so that its dimensions match the values that you
-- specified for at least one of 'MaxWidth' and 'MaxHeight' without dropping below
-- either value. If you specify this option, Elastic Transcoder does not scale
-- the video up.
vpSizingPolicy :: Lens' VideoParameters (Maybe Text)
vpSizingPolicy = lens _vpSizingPolicy (\s a -> s { _vpSizingPolicy = a })

-- | Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks appear
-- in the specified size and location, and with the specified opacity for the
-- duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark
-- that is not rectangular, use the .png format, which supports transparency.
--
-- When you create a job that uses this preset, you specify the .png or .jpg
-- graphics that you want Elastic Transcoder to include in the transcoded
-- videos. You can specify fewer graphics in the job than you specify watermark
-- settings in the preset, which allows you to use the same preset for up to
-- four watermarks that have different dimensions.
vpWatermarks :: Lens' VideoParameters [PresetWatermark]
vpWatermarks = lens _vpWatermarks (\s a -> s { _vpWatermarks = a }) . _List

instance FromJSON VideoParameters where
    parseJSON = withObject "VideoParameters" $ \o -> VideoParameters
        <$> o .:? "AspectRatio"
        <*> o .:? "BitRate"
        <*> o .:? "Codec"
        <*> o .:? "CodecOptions" .!= mempty
        <*> o .:? "DisplayAspectRatio"
        <*> o .:? "FixedGOP"
        <*> o .:? "FrameRate"
        <*> o .:? "KeyframesMaxDist"
        <*> o .:? "MaxFrameRate"
        <*> o .:? "MaxHeight"
        <*> o .:? "MaxWidth"
        <*> o .:? "PaddingPolicy"
        <*> o .:? "Resolution"
        <*> o .:? "SizingPolicy"
        <*> o .:? "Watermarks" .!= mempty

instance ToJSON VideoParameters where
    toJSON VideoParameters{..} = object
        [ "Codec"              .= _vpCodec
        , "CodecOptions"       .= _vpCodecOptions
        , "KeyframesMaxDist"   .= _vpKeyframesMaxDist
        , "FixedGOP"           .= _vpFixedGOP
        , "BitRate"            .= _vpBitRate
        , "FrameRate"          .= _vpFrameRate
        , "MaxFrameRate"       .= _vpMaxFrameRate
        , "Resolution"         .= _vpResolution
        , "AspectRatio"        .= _vpAspectRatio
        , "MaxWidth"           .= _vpMaxWidth
        , "MaxHeight"          .= _vpMaxHeight
        , "DisplayAspectRatio" .= _vpDisplayAspectRatio
        , "SizingPolicy"       .= _vpSizingPolicy
        , "PaddingPolicy"      .= _vpPaddingPolicy
        , "Watermarks"         .= _vpWatermarks
        ]

data Playlist = Playlist
    { _p2Format       :: Maybe Text
    , _p2Name         :: Maybe Text
    , _p2OutputKeys   :: List "OutputKeys" Text
    , _p2Status       :: Maybe Text
    , _p2StatusDetail :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Playlist' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'p2Format' @::@ 'Maybe' 'Text'
--
-- * 'p2Name' @::@ 'Maybe' 'Text'
--
-- * 'p2OutputKeys' @::@ ['Text']
--
-- * 'p2Status' @::@ 'Maybe' 'Text'
--
-- * 'p2StatusDetail' @::@ 'Maybe' 'Text'
--
playlist :: Playlist
playlist = Playlist
    { _p2Name         = Nothing
    , _p2Format       = Nothing
    , _p2OutputKeys   = mempty
    , _p2Status       = Nothing
    , _p2StatusDetail = Nothing
    }

-- | The format of the output playlist. Valid formats include 'HLSv3', 'HLSv4', and 'Smooth'.
p2Format :: Lens' Playlist (Maybe Text)
p2Format = lens _p2Format (\s a -> s { _p2Format = a })

-- | The name that you want Elastic Transcoder to assign to the master playlist,
-- for example, nyc-vacation.m3u8. If the name includes a '/' character, the
-- section of the name before the last '/' must be identical for all 'Name' objects.
-- If you create more than one master playlist, the values of all 'Name' objects
-- must be unique.
--
-- Note: Elastic Transcoder automatically appends the relevant file extension
-- to the file name ('.m3u8' for 'HLSv3' and 'HLSv4' playlists, and '.ism' and '.ismc' for 'Smooth' playlists). If you include a file extension in 'Name', the file name
-- will have two extensions.
p2Name :: Lens' Playlist (Maybe Text)
p2Name = lens _p2Name (\s a -> s { _p2Name = a })

-- | For each output in this job that you want to include in a master playlist,
-- the value of the Outputs:Key object.
--
-- If your output is not 'HLS' or does not have a segment duration set, the
-- name of the output file is a concatenation of 'OutputKeyPrefix' and 'Outputs:Key':
--
-- OutputKeyPrefix'Outputs:Key'
--
-- If your output is 'HLSv3' and has a segment duration set, or is not included
-- in a playlist, Elastic Transcoder creates an output playlist file with a file
-- extension of '.m3u8', and a series of '.ts' files that include a five-digit
-- sequential counter beginning with 00000:
--
-- OutputKeyPrefix'Outputs:Key'.m3u8
--
-- OutputKeyPrefix'Outputs:Key'00000.ts
--
-- If your output is 'HLSv4', has a segment duration set, and is included in an 'HLSv4' playlist, Elastic Transcoder creates an output playlist file with a
-- file extension of '_v4.m3u8'. If the output is video, Elastic Transcoder also
-- creates an output file with an extension of '_iframe.m3u8':
--
-- OutputKeyPrefix'Outputs:Key'_v4.m3u8
--
-- OutputKeyPrefix'Outputs:Key'_iframe.m3u8
--
-- OutputKeyPrefix'Outputs:Key'.ts
--
-- Elastic Transcoder automatically appends the relevant file extension to
-- the file name. If you include a file extension in Output Key, the file name
-- will have two extensions.
--
-- If you include more than one output in a playlist, any segment duration
-- settings, clip settings, or caption settings must be the same for all outputs
-- in the playlist. For 'Smooth' playlists, the 'Audio:Profile', 'Video:Profile', and 'Video:FrameRate' to 'Video:KeyframesMaxDist' ratio must be the same for all outputs.
p2OutputKeys :: Lens' Playlist [Text]
p2OutputKeys = lens _p2OutputKeys (\s a -> s { _p2OutputKeys = a }) . _List

-- | The status of the job with which the playlist is associated.
p2Status :: Lens' Playlist (Maybe Text)
p2Status = lens _p2Status (\s a -> s { _p2Status = a })

-- | Information that further explains the status.
p2StatusDetail :: Lens' Playlist (Maybe Text)
p2StatusDetail = lens _p2StatusDetail (\s a -> s { _p2StatusDetail = a })

instance FromJSON Playlist where
    parseJSON = withObject "Playlist" $ \o -> Playlist
        <$> o .:? "Format"
        <*> o .:? "Name"
        <*> o .:? "OutputKeys" .!= mempty
        <*> o .:? "Status"
        <*> o .:? "StatusDetail"

instance ToJSON Playlist where
    toJSON Playlist{..} = object
        [ "Name"         .= _p2Name
        , "Format"       .= _p2Format
        , "OutputKeys"   .= _p2OutputKeys
        , "Status"       .= _p2Status
        , "StatusDetail" .= _p2StatusDetail
        ]

data Notifications = Notifications
    { _nCompleted   :: Maybe Text
    , _nError       :: Maybe Text
    , _nProgressing :: Maybe Text
    , _nWarning     :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Notifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nCompleted' @::@ 'Maybe' 'Text'
--
-- * 'nError' @::@ 'Maybe' 'Text'
--
-- * 'nProgressing' @::@ 'Maybe' 'Text'
--
-- * 'nWarning' @::@ 'Maybe' 'Text'
--
notifications :: Notifications
notifications = Notifications
    { _nProgressing = Nothing
    , _nCompleted   = Nothing
    , _nWarning     = Nothing
    , _nError       = Nothing
    }

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
nCompleted :: Lens' Notifications (Maybe Text)
nCompleted = lens _nCompleted (\s a -> s { _nCompleted = a })

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
nError :: Lens' Notifications (Maybe Text)
nError = lens _nError (\s a -> s { _nError = a })

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify when Elastic Transcoder has started to process the job.
nProgressing :: Lens' Notifications (Maybe Text)
nProgressing = lens _nProgressing (\s a -> s { _nProgressing = a })

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
nWarning :: Lens' Notifications (Maybe Text)
nWarning = lens _nWarning (\s a -> s { _nWarning = a })

instance FromJSON Notifications where
    parseJSON = withObject "Notifications" $ \o -> Notifications
        <$> o .:? "Completed"
        <*> o .:? "Error"
        <*> o .:? "Progressing"
        <*> o .:? "Warning"

instance ToJSON Notifications where
    toJSON Notifications{..} = object
        [ "Progressing" .= _nProgressing
        , "Completed"   .= _nCompleted
        , "Warning"     .= _nWarning
        , "Error"       .= _nError
        ]

newtype Clip = Clip
    { _cTimeSpan :: Maybe TimeSpan
    } deriving (Eq, Show)

-- | 'Clip' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cTimeSpan' @::@ 'Maybe' 'TimeSpan'
--
clip :: Clip
clip = Clip
    { _cTimeSpan = Nothing
    }

-- | Settings that determine when a clip begins and how long it lasts.
cTimeSpan :: Lens' Clip (Maybe TimeSpan)
cTimeSpan = lens _cTimeSpan (\s a -> s { _cTimeSpan = a })

instance FromJSON Clip where
    parseJSON = withObject "Clip" $ \o -> Clip
        <$> o .:? "TimeSpan"

instance ToJSON Clip where
    toJSON Clip{..} = object
        [ "TimeSpan" .= _cTimeSpan
        ]

data JobInput = JobInput
    { _jiAspectRatio :: Maybe Text
    , _jiContainer   :: Maybe Text
    , _jiEncryption  :: Maybe Encryption
    , _jiFrameRate   :: Maybe Text
    , _jiInterlaced  :: Maybe Text
    , _jiKey         :: Maybe Text
    , _jiResolution  :: Maybe Text
    } deriving (Eq, Show)

-- | 'JobInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jiAspectRatio' @::@ 'Maybe' 'Text'
--
-- * 'jiContainer' @::@ 'Maybe' 'Text'
--
-- * 'jiEncryption' @::@ 'Maybe' 'Encryption'
--
-- * 'jiFrameRate' @::@ 'Maybe' 'Text'
--
-- * 'jiInterlaced' @::@ 'Maybe' 'Text'
--
-- * 'jiKey' @::@ 'Maybe' 'Text'
--
-- * 'jiResolution' @::@ 'Maybe' 'Text'
--
jobInput :: JobInput
jobInput = JobInput
    { _jiKey         = Nothing
    , _jiFrameRate   = Nothing
    , _jiResolution  = Nothing
    , _jiAspectRatio = Nothing
    , _jiInterlaced  = Nothing
    , _jiContainer   = Nothing
    , _jiEncryption  = Nothing
    }

-- | The aspect ratio of the input file. If you want Elastic Transcoder to
-- automatically detect the aspect ratio of the input file, specify 'auto'. If you
-- want to specify the aspect ratio for the output file, enter one of the
-- following values:
--
-- '1:1', '4:3', '3:2', '16:9'
--
-- If you specify a value other than 'auto', Elastic Transcoder disables
-- automatic detection of the aspect ratio.
jiAspectRatio :: Lens' JobInput (Maybe Text)
jiAspectRatio = lens _jiAspectRatio (\s a -> s { _jiAspectRatio = a })

-- | The container type for the input file. If you want Elastic Transcoder to
-- automatically detect the container type of the input file, specify 'auto'. If
-- you want to specify the container type for the input file, enter one of the
-- following values:
--
-- '3gp', 'aac', 'asf', 'avi', 'divx', 'flv', 'm4a', 'mkv', 'mov', 'mp3', 'mp4', 'mpeg', 'mpeg-ps', 'mpeg-ts', 'mxf', 'ogg', 'vob', 'wav', 'webm'
jiContainer :: Lens' JobInput (Maybe Text)
jiContainer = lens _jiContainer (\s a -> s { _jiContainer = a })

-- | The encryption settings, if any, that are used for decrypting your input
-- files. If your input file is encrypted, you must specify the mode that
-- Elastic Transcoder will use to decrypt your file.
jiEncryption :: Lens' JobInput (Maybe Encryption)
jiEncryption = lens _jiEncryption (\s a -> s { _jiEncryption = a })

-- | The frame rate of the input file. If you want Elastic Transcoder to
-- automatically detect the frame rate of the input file, specify 'auto'. If you
-- want to specify the frame rate for the input file, enter one of the following
-- values:
--
-- '10', '15', '23.97', '24', '25', '29.97', '30', '60'
--
-- If you specify a value other than 'auto', Elastic Transcoder disables
-- automatic detection of the frame rate.
jiFrameRate :: Lens' JobInput (Maybe Text)
jiFrameRate = lens _jiFrameRate (\s a -> s { _jiFrameRate = a })

-- | Whether the input file is interlaced. If you want Elastic Transcoder to
-- automatically detect whether the input file is interlaced, specify 'auto'. If
-- you want to specify whether the input file is interlaced, enter one of the
-- following values:
--
-- 'true', 'false'
--
-- If you specify a value other than 'auto', Elastic Transcoder disables
-- automatic detection of interlacing.
jiInterlaced :: Lens' JobInput (Maybe Text)
jiInterlaced = lens _jiInterlaced (\s a -> s { _jiInterlaced = a })

-- | The name of the file to transcode. Elsewhere in the body of the JSON block
-- is the the ID of the pipeline to use for processing the job. The 'InputBucket'
-- object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to
-- get the file from.
--
-- If the file name includes a prefix, such as 'cooking/lasagna.mpg', include the
-- prefix in the key. If the file isn't in the specified bucket, Elastic
-- Transcoder returns an error.
jiKey :: Lens' JobInput (Maybe Text)
jiKey = lens _jiKey (\s a -> s { _jiKey = a })

-- | This value must be 'auto', which causes Elastic Transcoder to automatically
-- detect the resolution of the input file.
jiResolution :: Lens' JobInput (Maybe Text)
jiResolution = lens _jiResolution (\s a -> s { _jiResolution = a })

instance FromJSON JobInput where
    parseJSON = withObject "JobInput" $ \o -> JobInput
        <$> o .:? "AspectRatio"
        <*> o .:? "Container"
        <*> o .:? "Encryption"
        <*> o .:? "FrameRate"
        <*> o .:? "Interlaced"
        <*> o .:? "Key"
        <*> o .:? "Resolution"

instance ToJSON JobInput where
    toJSON JobInput{..} = object
        [ "Key"         .= _jiKey
        , "FrameRate"   .= _jiFrameRate
        , "Resolution"  .= _jiResolution
        , "AspectRatio" .= _jiAspectRatio
        , "Interlaced"  .= _jiInterlaced
        , "Container"   .= _jiContainer
        , "Encryption"  .= _jiEncryption
        ]
