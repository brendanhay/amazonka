{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Transcoder lets you convert media files that you have stored
-- in Amazon Simple Storage Service (Amazon S3) into media files in the
-- formats required by consumer playback devices. For example, you can convert
-- large, high-quality digital media files into formats that users can play
-- back on mobile devices, tablets, web browsers, and connected televisions.
module Network.AWS.ElasticTranscoder.V2012_09_25.Types
    (
    -- * Service
      ElasticTranscoder
    -- ** Errors
    , Er (..)

    -- * AudioCodecOptions
    , AudioCodecOptions (..)
    , acoProfile

    -- * Clip
    , Clip (..)
    , dTimeSpan

    -- * Artwork
    , Artwork (..)
    , bInputKey
    , bMaxWidth
    , bMaxHeight
    , bSizingPolicy
    , bPaddingPolicy
    , bAlbumArtFormat

    -- * AudioParameters
    , AudioParameters (..)
    , apCodec
    , apSampleRate
    , apBitRate
    , apChannels
    , apCodecOptions

    -- * CaptionFormat
    , CaptionFormat (..)
    , cgFormat
    , cgPattern

    -- * CaptionSource
    , CaptionSource (..)
    , ctKey
    , ctLanguage
    , ctTimeOffset
    , ctLabel

    -- * Captions
    , Captions (..)
    , eMergePolicy
    , eCaptionSources
    , eCaptionFormats

    -- * CreateJobOutput
    , CreateJobOutput (..)
    , cjoKey
    , cjoThumbnailPattern
    , cjoRotate
    , cjoPresetId
    , cjoSegmentDuration
    , cjoWatermarks
    , cjoAlbumArt
    , cjoComposition
    , cjoCaptions

    -- * CreateJobPlaylist
    , CreateJobPlaylist (..)
    , cjvName
    , cjvFormat
    , cjvOutputKeys

    -- * Job
    , Job (..)
    , jbId
    , jbArn
    , jbPipelineId
    , jbInput
    , jbOutput
    , jbOutputs
    , jbOutputKeyPrefix
    , jbPlaylists
    , jbStatus

    -- * JobAlbumArt
    , JobAlbumArt (..)
    , jaaMergePolicy
    , jaaArtwork

    -- * JobInput
    , JobInput (..)
    , jiKey
    , jiFrameRate
    , jiResolution
    , jiAspectRatio
    , jiInterlaced
    , jiContainer

    -- * JobOutput
    , JobOutput (..)
    , joId
    , joKey
    , joThumbnailPattern
    , joRotate
    , joPresetId
    , joSegmentDuration
    , joStatus
    , joStatusDetail
    , joDuration
    , joWidth
    , joHeight
    , joWatermarks
    , joAlbumArt
    , joComposition
    , joCaptions

    -- * JobWatermark
    , JobWatermark (..)
    , jxPresetWatermarkId
    , jxInputKey

    -- * Notifications
    , Notifications (..)
    , nsProgressing
    , nsCompleted
    , nsWarning
    , nsError

    -- * Permission
    , Permission (..)
    , pnGranteeType
    , pnGrantee
    , pnAccess

    -- * Pipeline
    , Pipeline (..)
    , peId
    , peArn
    , peName
    , peStatus
    , peInputBucket
    , peOutputBucket
    , peRole
    , peNotifications
    , peContentConfig
    , peThumbnailConfig

    -- * PipelineOutputConfig
    , PipelineOutputConfig (..)
    , pocBucket
    , pocStorageClass
    , pocPermissions

    -- * Playlist
    , Playlist (..)
    , puName
    , puFormat
    , puOutputKeys
    , puStatus
    , puStatusDetail

    -- * Preset
    , Preset (..)
    , ppuId
    , ppuArn
    , ppuName
    , ppuDescription
    , ppuContainer
    , ppuAudio
    , ppuVideo
    , ppuThumbnails
    , ppuType

    -- * PresetWatermark
    , PresetWatermark (..)
    , ppkId
    , ppkMaxWidth
    , ppkMaxHeight
    , ppkSizingPolicy
    , ppkHorizontalAlign
    , ppkHorizontalOffset
    , ppkVerticalAlign
    , ppkVerticalOffset
    , ppkOpacity
    , ppkTarget

    -- * Thumbnails
    , Thumbnails (..)
    , ttsFormat
    , ttsInterval
    , ttsResolution
    , ttsAspectRatio
    , ttsMaxWidth
    , ttsMaxHeight
    , ttsSizingPolicy
    , ttsPaddingPolicy

    -- * TimeSpan
    , TimeSpan (..)
    , tsStartTime
    , tsDuration

    -- * VideoParameters
    , VideoParameters (..)
    , vpCodec
    , vpCodecOptions
    , vpKeyframesMaxDist
    , vpFixedGOP
    , vpBitRate
    , vpFrameRate
    , vpMaxFrameRate
    , vpResolution
    , vpAspectRatio
    , vpMaxWidth
    , vpMaxHeight
    , vpDisplayAspectRatio
    , vpSizingPolicy
    , vpPaddingPolicy
    , vpWatermarks

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-09-25@) of the
-- @Amazon Elastic Transcoder@ service.
data ElasticTranscoder deriving (Typeable)

instance AWSService ElasticTranscoder where
    type Sg ElasticTranscoder = V4
    data Er ElasticTranscoder
        = AccessDeniedException
        | ElasticTranscoderClient HttpException
        | ElasticTranscoderSerializer String
        | ElasticTranscoderService String
        | IncompatibleVersionException
        | InternalServiceException
        | LimitExceededException
        | ResourceInUseException
        | ResourceNotFoundException
        | ValidationException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elastictranscoder"
        , _svcVersion  = "2012-09-25"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ElasticTranscoder)
deriving instance Generic (Er ElasticTranscoder)

instance AWSError (Er ElasticTranscoder) where
    awsError = const "ElasticTranscoderError"

instance AWSServiceError (Er ElasticTranscoder) where
    serviceError    = ElasticTranscoderService
    clientError     = ElasticTranscoderClient
    serializerError = ElasticTranscoderSerializer

instance Exception (Er ElasticTranscoder)

-- | If you specified AAC for Audio:Codec, this is the AAC compression profile
-- to use. Valid values include: auto, AAC-LC, HE-AAC, HE-AACv2 If you specify
-- auto, Elastic Transcoder chooses a profile based on the bit rate of the
-- output file.
newtype AudioCodecOptions = AudioCodecOptions
    { _acoProfile :: Maybe Text
      -- ^ You can only choose an audio profile when you specify AAC for the
      -- value of Audio:Codec. Specify the AAC profile for the output
      -- file. Elastic Transcoder supports the following profiles: auto:
      -- If you specify auto, Elastic Transcoder will select the profile
      -- based on the bit rate selected for the output file. AAC-LC: The
      -- most common AAC profile. Use for bitrates larger than 64 kbps.
      -- HE-AAC: Not supported on some older players and devices. Use for
      -- bitrates between 40 and 80 kbps. HE-AACv2: Not supported on some
      -- players and devices. Use for bitrates less than 48 kbps. If you
      -- created any presets before AAC profiles were added, Elastic
      -- Transcoder automatically updated your presets to use AAC-LC. You
      -- can change the value as required.
    } deriving (Show, Generic)

-- | You can only choose an audio profile when you specify AAC for the value of
-- Audio:Codec. Specify the AAC profile for the output file. Elastic
-- Transcoder supports the following profiles: auto: If you specify auto,
-- Elastic Transcoder will select the profile based on the bit rate selected
-- for the output file. AAC-LC: The most common AAC profile. Use for bitrates
-- larger than 64 kbps. HE-AAC: Not supported on some older players and
-- devices. Use for bitrates between 40 and 80 kbps. HE-AACv2: Not supported
-- on some players and devices. Use for bitrates less than 48 kbps. If you
-- created any presets before AAC profiles were added, Elastic Transcoder
-- automatically updated your presets to use AAC-LC. You can change the value
-- as required.
acoProfile
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AudioCodecOptions
    -> f AudioCodecOptions
acoProfile f x =
    (\y -> x { _acoProfile = y })
       <$> f (_acoProfile x)
{-# INLINE acoProfile #-}

instance FromJSON AudioCodecOptions

instance ToJSON AudioCodecOptions

-- | Settings for one clip in a composition. All jobs in a playlist must have
-- the same clip settings.
newtype Clip = Clip
    { _dTimeSpan :: Maybe TimeSpan
      -- ^ Settings that determine when a clip begins and how long it lasts.
    } deriving (Show, Generic)

-- | Settings that determine when a clip begins and how long it lasts.
dTimeSpan
    :: Functor f
    => (Maybe TimeSpan
    -> f (Maybe TimeSpan))
    -> Clip
    -> f Clip
dTimeSpan f x =
    (\y -> x { _dTimeSpan = y })
       <$> f (_dTimeSpan x)
{-# INLINE dTimeSpan #-}

instance FromJSON Clip

instance ToJSON Clip

-- | The file to be used as album art. There can be multiple artworks associated
-- with an audio file, to a maximum of 20. To remove artwork or leave the
-- artwork empty, you can either set Artwork to null, or set the Merge Policy
-- to "Replace" and use an empty Artwork array. To pass through existing
-- artwork unchanged, set the Merge Policy to "Prepend", "Append", or
-- "Fallback", and use an empty Artwork array.
data Artwork = Artwork
    { _bInputKey :: Maybe Text
      -- ^ The name of the file to be used as album art. To determine which
      -- Amazon S3 bucket contains the specified file, Elastic Transcoder
      -- checks the pipeline specified by PipelineId; the InputBucket
      -- object in that pipeline identifies the bucket. If the file name
      -- includes a prefix, for example, cooking/pie.jpg, include the
      -- prefix in the key. If the file isn't in the specified bucket,
      -- Elastic Transcoder returns an error.
    , _bMaxWidth :: Maybe Text
      -- ^ The maximum width of the output album art in pixels. If you
      -- specify auto, Elastic Transcoder uses 600 as the default value.
      -- If you specify a numeric value, enter an even integer between 32
      -- and 4096, inclusive.
    , _bMaxHeight :: Maybe Text
      -- ^ The maximum height of the output album art in pixels. If you
      -- specify auto, Elastic Transcoder uses 600 as the default value.
      -- If you specify a numeric value, enter an even integer between 32
      -- and 3072, inclusive.
    , _bSizingPolicy :: Maybe Text
      -- ^ Specify one of the following values to control scaling of the
      -- output album art: Fit: Elastic Transcoder scales the output art
      -- so it matches the value that you specified in either MaxWidth or
      -- MaxHeight without exceeding the other value. Fill: Elastic
      -- Transcoder scales the output art so it matches the value that you
      -- specified in either MaxWidth or MaxHeight and matches or exceeds
      -- the other value. Elastic Transcoder centers the output art and
      -- then crops it in the dimension (if any) that exceeds the maximum
      -- value. Stretch: Elastic Transcoder stretches the output art to
      -- match the values that you specified for MaxWidth and MaxHeight.
      -- If the relative proportions of the input art and the output art
      -- are different, the output art will be distorted. Keep: Elastic
      -- Transcoder does not scale the output art. If either dimension of
      -- the input art exceeds the values that you specified for MaxWidth
      -- and MaxHeight, Elastic Transcoder crops the output art.
      -- ShrinkToFit: Elastic Transcoder scales the output art down so
      -- that its dimensions match the values that you specified for at
      -- least one of MaxWidth and MaxHeight without exceeding either
      -- value. If you specify this option, Elastic Transcoder does not
      -- scale the art up. ShrinkToFill Elastic Transcoder scales the
      -- output art down so that its dimensions match the values that you
      -- specified for at least one of MaxWidth and MaxHeight without
      -- dropping below either value. If you specify this option, Elastic
      -- Transcoder does not scale the art up.
    , _bPaddingPolicy :: Maybe Text
      -- ^ When you set PaddingPolicy to Pad, Elastic Transcoder may add
      -- white bars to the top and bottom and/or left and right sides of
      -- the output album art to make the total size of the output art
      -- match the values that you specified for MaxWidth and MaxHeight.
    , _bAlbumArtFormat :: Maybe Text
      -- ^ The format of album art, if any. Valid formats are .jpg and .png.
    } deriving (Show, Generic)

-- | The name of the file to be used as album art. To determine which Amazon S3
-- bucket contains the specified file, Elastic Transcoder checks the pipeline
-- specified by PipelineId; the InputBucket object in that pipeline identifies
-- the bucket. If the file name includes a prefix, for example,
-- cooking/pie.jpg, include the prefix in the key. If the file isn't in the
-- specified bucket, Elastic Transcoder returns an error.
bInputKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bInputKey f x =
    (\y -> x { _bInputKey = y })
       <$> f (_bInputKey x)
{-# INLINE bInputKey #-}

-- | The maximum width of the output album art in pixels. If you specify auto,
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 4096, inclusive.
bMaxWidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bMaxWidth f x =
    (\y -> x { _bMaxWidth = y })
       <$> f (_bMaxWidth x)
{-# INLINE bMaxWidth #-}

-- | The maximum height of the output album art in pixels. If you specify auto,
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 3072, inclusive.
bMaxHeight
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bMaxHeight f x =
    (\y -> x { _bMaxHeight = y })
       <$> f (_bMaxHeight x)
{-# INLINE bMaxHeight #-}

-- | Specify one of the following values to control scaling of the output album
-- art: Fit: Elastic Transcoder scales the output art so it matches the value
-- that you specified in either MaxWidth or MaxHeight without exceeding the
-- other value. Fill: Elastic Transcoder scales the output art so it matches
-- the value that you specified in either MaxWidth or MaxHeight and matches or
-- exceeds the other value. Elastic Transcoder centers the output art and then
-- crops it in the dimension (if any) that exceeds the maximum value. Stretch:
-- Elastic Transcoder stretches the output art to match the values that you
-- specified for MaxWidth and MaxHeight. If the relative proportions of the
-- input art and the output art are different, the output art will be
-- distorted. Keep: Elastic Transcoder does not scale the output art. If
-- either dimension of the input art exceeds the values that you specified for
-- MaxWidth and MaxHeight, Elastic Transcoder crops the output art.
-- ShrinkToFit: Elastic Transcoder scales the output art down so that its
-- dimensions match the values that you specified for at least one of MaxWidth
-- and MaxHeight without exceeding either value. If you specify this option,
-- Elastic Transcoder does not scale the art up. ShrinkToFill Elastic
-- Transcoder scales the output art down so that its dimensions match the
-- values that you specified for at least one of MaxWidth and MaxHeight
-- without dropping below either value. If you specify this option, Elastic
-- Transcoder does not scale the art up.
bSizingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bSizingPolicy f x =
    (\y -> x { _bSizingPolicy = y })
       <$> f (_bSizingPolicy x)
{-# INLINE bSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add white bars to
-- the top and bottom and/or left and right sides of the output album art to
-- make the total size of the output art match the values that you specified
-- for MaxWidth and MaxHeight.
bPaddingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bPaddingPolicy f x =
    (\y -> x { _bPaddingPolicy = y })
       <$> f (_bPaddingPolicy x)
{-# INLINE bPaddingPolicy #-}

-- | The format of album art, if any. Valid formats are .jpg and .png.
bAlbumArtFormat
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Artwork
    -> f Artwork
bAlbumArtFormat f x =
    (\y -> x { _bAlbumArtFormat = y })
       <$> f (_bAlbumArtFormat x)
{-# INLINE bAlbumArtFormat #-}

instance FromJSON Artwork

instance ToJSON Artwork

-- | A section of the request body that specifies the audio parameters.
data AudioParameters = AudioParameters
    { _apCodec :: Maybe Text
      -- ^ The audio codec for the output file. Valid values include aac,
      -- mp3, and vorbis.
    , _apSampleRate :: Maybe Text
      -- ^ The sample rate of the audio stream in the output file, in Hertz.
      -- Valid values include: auto, 22050, 32000, 44100, 48000, 96000 If
      -- you specify auto, Elastic Transcoder automatically detects the
      -- sample rate.
    , _apBitRate :: Maybe Text
      -- ^ The bit rate of the audio stream in the output file, in
      -- kilobits/second. Enter an integer between 64 and 320, inclusive.
    , _apChannels :: Maybe Text
      -- ^ The number of audio channels in the output file. Valid values
      -- include: auto, 0, 1, 2 If you specify auto, Elastic Transcoder
      -- automatically detects the number of channels in the input file.
    , _apCodecOptions :: Maybe AudioCodecOptions
      -- ^ If you specified AAC for Audio:Codec, this is the AAC compression
      -- profile to use. Valid values include: auto, AAC-LC, HE-AAC,
      -- HE-AACv2 If you specify auto, Elastic Transcoder chooses a
      -- profile based on the bit rate of the output file.
    } deriving (Show, Generic)

-- | The audio codec for the output file. Valid values include aac, mp3, and
-- vorbis.
apCodec
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AudioParameters
    -> f AudioParameters
apCodec f x =
    (\y -> x { _apCodec = y })
       <$> f (_apCodec x)
{-# INLINE apCodec #-}

-- | The sample rate of the audio stream in the output file, in Hertz. Valid
-- values include: auto, 22050, 32000, 44100, 48000, 96000 If you specify
-- auto, Elastic Transcoder automatically detects the sample rate.
apSampleRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AudioParameters
    -> f AudioParameters
apSampleRate f x =
    (\y -> x { _apSampleRate = y })
       <$> f (_apSampleRate x)
{-# INLINE apSampleRate #-}

-- | The bit rate of the audio stream in the output file, in kilobits/second.
-- Enter an integer between 64 and 320, inclusive.
apBitRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AudioParameters
    -> f AudioParameters
apBitRate f x =
    (\y -> x { _apBitRate = y })
       <$> f (_apBitRate x)
{-# INLINE apBitRate #-}

-- | The number of audio channels in the output file. Valid values include:
-- auto, 0, 1, 2 If you specify auto, Elastic Transcoder automatically detects
-- the number of channels in the input file.
apChannels
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AudioParameters
    -> f AudioParameters
apChannels f x =
    (\y -> x { _apChannels = y })
       <$> f (_apChannels x)
{-# INLINE apChannels #-}

-- | If you specified AAC for Audio:Codec, this is the AAC compression profile
-- to use. Valid values include: auto, AAC-LC, HE-AAC, HE-AACv2 If you specify
-- auto, Elastic Transcoder chooses a profile based on the bit rate of the
-- output file.
apCodecOptions
    :: Functor f
    => (Maybe AudioCodecOptions
    -> f (Maybe AudioCodecOptions))
    -> AudioParameters
    -> f AudioParameters
apCodecOptions f x =
    (\y -> x { _apCodecOptions = y })
       <$> f (_apCodecOptions x)
{-# INLINE apCodecOptions #-}

instance FromJSON AudioParameters

instance ToJSON AudioParameters

-- | The file format of the output captions. If you leave this value blank,
-- Elastic Transcoder returns an error.
data CaptionFormat = CaptionFormat
    { _cgFormat :: Maybe Text
      -- ^ The format you specify determines whether Elastic Transcoder
      -- generates an embedded or sidecar caption for this output. Valid
      -- Embedded Caption Formats: For MP3: None For MP4: mov-text For
      -- MPEG-TS: None For ogg: None For webm: None Valid Sidecar Caption
      -- Formats: Elastic Transcoder supports dfxp (first div element
      -- only), scc, srt, and webvtt. There are no container restrictions
      -- on sidecar captions. If you want ttml or smpte-tt compatible
      -- captions, specify dfxp as your output format.
    , _cgPattern :: Maybe Text
      -- ^ The prefix for caption filenames, in the form
      -- description-{language}, where: description is a description of
      -- the video. {language} is a literal value that Elastic Transcoder
      -- replaces with the two- or three-letter code for the language of
      -- the caption in the output file names. If you don't include
      -- {language} in the file name pattern, Elastic Transcoder
      -- automatically appends "{language}" to the value that you specify
      -- for the description. In addition, Elastic Transcoder
      -- automatically appends the count to the end of the segment files.
      -- For example, suppose you're transcoding into srt format. When you
      -- enter "Sydney-{language}-sunrise", and the language of the
      -- captions is English (en), the name of the first caption file will
      -- be Sydney-en-sunrise00000.srt.
    } deriving (Show, Generic)

-- | The format you specify determines whether Elastic Transcoder generates an
-- embedded or sidecar caption for this output. Valid Embedded Caption
-- Formats: For MP3: None For MP4: mov-text For MPEG-TS: None For ogg: None
-- For webm: None Valid Sidecar Caption Formats: Elastic Transcoder supports
-- dfxp (first div element only), scc, srt, and webvtt. There are no container
-- restrictions on sidecar captions. If you want ttml or smpte-tt compatible
-- captions, specify dfxp as your output format.
cgFormat
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionFormat
    -> f CaptionFormat
cgFormat f x =
    (\y -> x { _cgFormat = y })
       <$> f (_cgFormat x)
{-# INLINE cgFormat #-}

-- | The prefix for caption filenames, in the form description-{language},
-- where: description is a description of the video. {language} is a literal
-- value that Elastic Transcoder replaces with the two- or three-letter code
-- for the language of the caption in the output file names. If you don't
-- include {language} in the file name pattern, Elastic Transcoder
-- automatically appends "{language}" to the value that you specify for the
-- description. In addition, Elastic Transcoder automatically appends the
-- count to the end of the segment files. For example, suppose you're
-- transcoding into srt format. When you enter "Sydney-{language}-sunrise",
-- and the language of the captions is English (en), the name of the first
-- caption file will be Sydney-en-sunrise00000.srt.
cgPattern
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionFormat
    -> f CaptionFormat
cgPattern f x =
    (\y -> x { _cgPattern = y })
       <$> f (_cgPattern x)
{-# INLINE cgPattern #-}

instance FromJSON CaptionFormat

instance ToJSON CaptionFormat

-- | A source file for the input sidecar captions used during the transcoding
-- process.
data CaptionSource = CaptionSource
    { _ctKey :: Maybe Text
      -- ^ The name of the sidecar caption file that you want Elastic
      -- Transcoder to include in the output file.
    , _ctLanguage :: Maybe Text
      -- ^ A string that specifies the language of the caption. Specify this
      -- as one of: 2-character ISO 639-1 code 3-character ISO 639-2 code
      -- For more information on ISO language codes and language names,
      -- see the List of ISO 639-1 codes.
    , _ctTimeOffset :: Maybe Text
      -- ^ For clip generation or captions that do not start at the same
      -- time as the associated video file, the TimeOffset tells Elastic
      -- Transcoder how much of the video to encode before including
      -- captions. Specify the TimeOffset in the form [+-]SS.sss or
      -- [+-]HH:mm:SS.ss.
    , _ctLabel :: Maybe Text
      -- ^ The label of the caption shown in the player when choosing a
      -- language. We recommend that you put the caption language name
      -- here, in the language of the captions.
    } deriving (Show, Generic)

-- | The name of the sidecar caption file that you want Elastic Transcoder to
-- include in the output file.
ctKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionSource
    -> f CaptionSource
ctKey f x =
    (\y -> x { _ctKey = y })
       <$> f (_ctKey x)
{-# INLINE ctKey #-}

-- | A string that specifies the language of the caption. Specify this as one
-- of: 2-character ISO 639-1 code 3-character ISO 639-2 code For more
-- information on ISO language codes and language names, see the List of ISO
-- 639-1 codes.
ctLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionSource
    -> f CaptionSource
ctLanguage f x =
    (\y -> x { _ctLanguage = y })
       <$> f (_ctLanguage x)
{-# INLINE ctLanguage #-}

-- | For clip generation or captions that do not start at the same time as the
-- associated video file, the TimeOffset tells Elastic Transcoder how much of
-- the video to encode before including captions. Specify the TimeOffset in
-- the form [+-]SS.sss or [+-]HH:mm:SS.ss.
ctTimeOffset
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionSource
    -> f CaptionSource
ctTimeOffset f x =
    (\y -> x { _ctTimeOffset = y })
       <$> f (_ctTimeOffset x)
{-# INLINE ctTimeOffset #-}

-- | The label of the caption shown in the player when choosing a language. We
-- recommend that you put the caption language name here, in the language of
-- the captions.
ctLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CaptionSource
    -> f CaptionSource
ctLabel f x =
    (\y -> x { _ctLabel = y })
       <$> f (_ctLabel x)
{-# INLINE ctLabel #-}

instance FromJSON CaptionSource

instance ToJSON CaptionSource

-- | You can configure Elastic Transcoder to transcode captions, or subtitles,
-- from one format to another. All captions must be in UTF-8. Elastic
-- Transcoder supports two types of captions: Embedded: Embedded captions are
-- included in the same file as the audio and video. Elastic Transcoder
-- supports only one embedded caption per language, to a maximum of 300
-- embedded captions per file. Valid input values include: CEA-608 (EIA-608,
-- first non-empty channel only), CEA-708 (EIA-708, first non-empty channel
-- only), and mov-text Valid outputs include: mov-text Elastic Transcoder
-- supports a maximum of one embedded format per output. Sidecar: Sidecar
-- captions are kept in a separate metadata file from the audio and video
-- data. Sidecar captions require a player that is capable of understanding
-- the relationship between the video file and the sidecar file. Elastic
-- Transcoder supports only one sidecar caption per language, to a maximum of
-- 20 sidecar captions per file. Valid input values include: dfxp (first div
-- element only), ebu-tt, scc, smpt, srt, ttml (first div element only), and
-- webvtt Valid outputs include: dfxp (first div element only), scc, srt, and
-- webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as
-- your output format. Elastic Transcoder does not support OCR (Optical
-- Character Recognition), does not accept pictures as a valid input for
-- captions, and is not available for audio-only transcoding. Elastic
-- Transcoder does not preserve text formatting (for example, italics) during
-- the transcoding process. To remove captions or leave the captions empty,
-- set Captions to null. To pass through existing captions unchanged, set the
-- MergePolicy to MergeRetain, and pass in a null CaptionSources array. For
-- more information on embedded files, see the Subtitles Wikipedia page. For
-- more information on sidecar files, see the Extensible Metadata Platform and
-- Sidecar file Wikipedia pages.
data Captions = Captions
    { _eMergePolicy :: Maybe Text
      -- ^ A policy that determines how Elastic Transcoder handles the
      -- existence of multiple captions. MergeOverride: Elastic Transcoder
      -- transcodes both embedded and sidecar captions into outputs. If
      -- captions for a language are embedded in the input file and also
      -- appear in a sidecar file, Elastic Transcoder uses the sidecar
      -- captions and ignores the embedded captions for that language.
      -- MergeRetain: Elastic Transcoder transcodes both embedded and
      -- sidecar captions into outputs. If captions for a language are
      -- embedded in the input file and also appear in a sidecar file,
      -- Elastic Transcoder uses the embedded captions and ignores the
      -- sidecar captions for that language. If CaptionSources is empty,
      -- Elastic Transcoder omits all sidecar captions from the output
      -- files. Override: Elastic Transcoder transcodes only the sidecar
      -- captions that you specify in CaptionSources. MergePolicy cannot
      -- be null.
    , _eCaptionSources :: [CaptionSource]
      -- ^ Source files for the input sidecar captions used during the
      -- transcoding process. To omit all sidecar captions, leave
      -- CaptionSources blank.
    , _eCaptionFormats :: [CaptionFormat]
      -- ^ The array of file formats for the output captions. If you leave
      -- this value blank, Elastic Transcoder returns an error.
    } deriving (Show, Generic)

-- | A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions. MergeOverride: Elastic Transcoder transcodes both
-- embedded and sidecar captions into outputs. If captions for a language are
-- embedded in the input file and also appear in a sidecar file, Elastic
-- Transcoder uses the sidecar captions and ignores the embedded captions for
-- that language. MergeRetain: Elastic Transcoder transcodes both embedded and
-- sidecar captions into outputs. If captions for a language are embedded in
-- the input file and also appear in a sidecar file, Elastic Transcoder uses
-- the embedded captions and ignores the sidecar captions for that language.
-- If CaptionSources is empty, Elastic Transcoder omits all sidecar captions
-- from the output files. Override: Elastic Transcoder transcodes only the
-- sidecar captions that you specify in CaptionSources. MergePolicy cannot be
-- null.
eMergePolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Captions
    -> f Captions
eMergePolicy f x =
    (\y -> x { _eMergePolicy = y })
       <$> f (_eMergePolicy x)
{-# INLINE eMergePolicy #-}

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave CaptionSources blank.
eCaptionSources
    :: Functor f
    => ([CaptionSource]
    -> f ([CaptionSource]))
    -> Captions
    -> f Captions
eCaptionSources f x =
    (\y -> x { _eCaptionSources = y })
       <$> f (_eCaptionSources x)
{-# INLINE eCaptionSources #-}

-- | The array of file formats for the output captions. If you leave this value
-- blank, Elastic Transcoder returns an error.
eCaptionFormats
    :: Functor f
    => ([CaptionFormat]
    -> f ([CaptionFormat]))
    -> Captions
    -> f Captions
eCaptionFormats f x =
    (\y -> x { _eCaptionFormats = y })
       <$> f (_eCaptionFormats x)
{-# INLINE eCaptionFormats #-}

instance FromJSON Captions

instance ToJSON Captions

-- | The CreateJobOutput structure.
data CreateJobOutput = CreateJobOutput
    { _cjoKey :: Maybe Text
      -- ^ The name to assign to the transcoded file. Elastic Transcoder
      -- saves the file in the Amazon S3 bucket specified by the
      -- OutputBucket object in the pipeline that is specified by the
      -- pipeline ID. If a file with the specified name already exists in
      -- the output bucket, the job fails.
    , _cjoThumbnailPattern :: Maybe Text
      -- ^ Whether you want Elastic Transcoder to create thumbnails for your
      -- videos and, if so, how you want Elastic Transcoder to name the
      -- files. If you don't want Elastic Transcoder to create thumbnails,
      -- specify "". If you do want Elastic Transcoder to create
      -- thumbnails, specify the information that you want to include in
      -- the file name for each thumbnail. You can specify the following
      -- values in any sequence: {count} (Required): If you want to create
      -- thumbnails, you must include {count} in the ThumbnailPattern
      -- object. Wherever you specify {count}, Elastic Transcoder adds a
      -- five-digit sequence number (beginning with 00001) to thumbnail
      -- file names. The number indicates where a given thumbnail appears
      -- in the sequence of thumbnails for a transcoded file. If you
      -- specify a literal value and/or {resolution} but you omit {count},
      -- Elastic Transcoder returns a validation error and does not create
      -- the job. Literal values (Optional): You can specify literal
      -- values anywhere in the ThumbnailPattern object. For example, you
      -- can include them as a file name prefix or as a delimiter between
      -- {resolution} and {count}. {resolution} (Optional): If you want
      -- Elastic Transcoder to include the resolution in the file name,
      -- include {resolution} in the ThumbnailPattern object. When
      -- creating thumbnails, Elastic Transcoder automatically saves the
      -- files in the format (.jpg or .png) that appears in the preset
      -- that you specified in the PresetID value of CreateJobOutput.
      -- Elastic Transcoder also appends the applicable file name
      -- extension.
    , _cjoRotate :: Maybe Text
      -- ^ The number of degrees clockwise by which you want Elastic
      -- Transcoder to rotate the output relative to the input. Enter one
      -- of the following values: auto, 0, 90, 180, 270. The value auto
      -- generally works only if the file that you're transcoding contains
      -- rotation metadata.
    , _cjoPresetId :: Maybe Text
      -- ^ The Id of the preset to use for this job. The preset determines
      -- the audio, video, and thumbnail settings that Elastic Transcoder
      -- uses for transcoding.
    , _cjoSegmentDuration :: Maybe Text
      -- ^ If you specify a preset in PresetId for which the value of
      -- Container is ts (MPEG-TS), SegmentDuration is the duration of
      -- each .ts file in seconds. The range of valid values is 1 to 60
      -- seconds.
    , _cjoWatermarks :: [JobWatermark]
      -- ^ Information about the watermarks that you want Elastic Transcoder
      -- to add to the video during transcoding. You can specify up to
      -- four watermarks for each output. Settings for each watermark must
      -- be defined in the preset for the current output.
    , _cjoAlbumArt :: Maybe JobAlbumArt
      -- ^ Information about the album art that you want Elastic Transcoder
      -- to add to the file during transcoding. You can specify up to
      -- twenty album artworks for each output. Settings for each artwork
      -- must be defined in the job for the current output.
    , _cjoComposition :: [Clip]
      -- ^ You can create an output file that contains an excerpt from the
      -- input file. This excerpt, called a clip, can come from the
      -- beginning, middle, or end of the file. The Composition object
      -- contains settings for the clips that make up an output file. For
      -- the current release, you can only specify settings for a single
      -- clip per output file. The Composition object cannot be null.
    , _cjoCaptions :: Maybe Captions
      -- ^ You can configure Elastic Transcoder to transcode captions, or
      -- subtitles, from one format to another. All captions must be in
      -- UTF-8. Elastic Transcoder supports two types of captions:
      -- Embedded: Embedded captions are included in the same file as the
      -- audio and video. Elastic Transcoder supports only one embedded
      -- caption per language, to a maximum of 300 embedded captions per
      -- file. Valid input values include: CEA-608 (EIA-608, first
      -- non-empty channel only), CEA-708 (EIA-708, first non-empty
      -- channel only), and mov-text Valid outputs include: mov-text
      -- Elastic Transcoder supports a maximum of one embedded format per
      -- output. Sidecar: Sidecar captions are kept in a separate metadata
      -- file from the audio and video data. Sidecar captions require a
      -- player that is capable of understanding the relationship between
      -- the video file and the sidecar file. Elastic Transcoder supports
      -- only one sidecar caption per language, to a maximum of 20 sidecar
      -- captions per file. Valid input values include: dfxp (first div
      -- element only), ebu-tt, scc, smpt, srt, ttml (first div element
      -- only), and webvtt Valid outputs include: dfxp (first div element
      -- only), scc, srt, and webvtt. If you want ttml or smpte-tt
      -- compatible captions, specify dfxp as your output format. Elastic
      -- Transcoder does not support OCR (Optical Character Recognition),
      -- does not accept pictures as a valid input for captions, and is
      -- not available for audio-only transcoding. Elastic Transcoder does
      -- not preserve text formatting (for example, italics) during the
      -- transcoding process. To remove captions or leave the captions
      -- empty, set Captions to null. To pass through existing captions
      -- unchanged, set the MergePolicy to MergeRetain, and pass in a null
      -- CaptionSources array. For more information on embedded files, see
      -- the Subtitles Wikipedia page. For more information on sidecar
      -- files, see the Extensible Metadata Platform and Sidecar file
      -- Wikipedia pages.
    } deriving (Show, Generic)

-- | The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the OutputBucket object in the
-- pipeline that is specified by the pipeline ID. If a file with the specified
-- name already exists in the output bucket, the job fails.
cjoKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoKey f x =
    (\y -> x { _cjoKey = y })
       <$> f (_cjoKey x)
{-# INLINE cjoKey #-}

-- | Whether you want Elastic Transcoder to create thumbnails for your videos
-- and, if so, how you want Elastic Transcoder to name the files. If you don't
-- want Elastic Transcoder to create thumbnails, specify "". If you do want
-- Elastic Transcoder to create thumbnails, specify the information that you
-- want to include in the file name for each thumbnail. You can specify the
-- following values in any sequence: {count} (Required): If you want to create
-- thumbnails, you must include {count} in the ThumbnailPattern object.
-- Wherever you specify {count}, Elastic Transcoder adds a five-digit sequence
-- number (beginning with 00001) to thumbnail file names. The number indicates
-- where a given thumbnail appears in the sequence of thumbnails for a
-- transcoded file. If you specify a literal value and/or {resolution} but you
-- omit {count}, Elastic Transcoder returns a validation error and does not
-- create the job. Literal values (Optional): You can specify literal values
-- anywhere in the ThumbnailPattern object. For example, you can include them
-- as a file name prefix or as a delimiter between {resolution} and {count}.
-- {resolution} (Optional): If you want Elastic Transcoder to include the
-- resolution in the file name, include {resolution} in the ThumbnailPattern
-- object. When creating thumbnails, Elastic Transcoder automatically saves
-- the files in the format (.jpg or .png) that appears in the preset that you
-- specified in the PresetID value of CreateJobOutput. Elastic Transcoder also
-- appends the applicable file name extension.
cjoThumbnailPattern
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoThumbnailPattern f x =
    (\y -> x { _cjoThumbnailPattern = y })
       <$> f (_cjoThumbnailPattern x)
{-# INLINE cjoThumbnailPattern #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values:
-- auto, 0, 90, 180, 270. The value auto generally works only if the file that
-- you're transcoding contains rotation metadata.
cjoRotate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoRotate f x =
    (\y -> x { _cjoRotate = y })
       <$> f (_cjoRotate x)
{-# INLINE cjoRotate #-}

-- | The Id of the preset to use for this job. The preset determines the audio,
-- video, and thumbnail settings that Elastic Transcoder uses for transcoding.
cjoPresetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoPresetId f x =
    (\y -> x { _cjoPresetId = y })
       <$> f (_cjoPresetId x)
{-# INLINE cjoPresetId #-}

-- | If you specify a preset in PresetId for which the value of Container is ts
-- (MPEG-TS), SegmentDuration is the duration of each .ts file in seconds. The
-- range of valid values is 1 to 60 seconds.
cjoSegmentDuration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoSegmentDuration f x =
    (\y -> x { _cjoSegmentDuration = y })
       <$> f (_cjoSegmentDuration x)
{-# INLINE cjoSegmentDuration #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to
-- the video during transcoding. You can specify up to four watermarks for
-- each output. Settings for each watermark must be defined in the preset for
-- the current output.
cjoWatermarks
    :: Functor f
    => ([JobWatermark]
    -> f ([JobWatermark]))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoWatermarks f x =
    (\y -> x { _cjoWatermarks = y })
       <$> f (_cjoWatermarks x)
{-# INLINE cjoWatermarks #-}

-- | Information about the album art that you want Elastic Transcoder to add to
-- the file during transcoding. You can specify up to twenty album artworks
-- for each output. Settings for each artwork must be defined in the job for
-- the current output.
cjoAlbumArt
    :: Functor f
    => (Maybe JobAlbumArt
    -> f (Maybe JobAlbumArt))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoAlbumArt f x =
    (\y -> x { _cjoAlbumArt = y })
       <$> f (_cjoAlbumArt x)
{-# INLINE cjoAlbumArt #-}

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make
-- up an output file. For the current release, you can only specify settings
-- for a single clip per output file. The Composition object cannot be null.
cjoComposition
    :: Functor f
    => ([Clip]
    -> f ([Clip]))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoComposition f x =
    (\y -> x { _cjoComposition = y })
       <$> f (_cjoComposition x)
{-# INLINE cjoComposition #-}

-- | You can configure Elastic Transcoder to transcode captions, or subtitles,
-- from one format to another. All captions must be in UTF-8. Elastic
-- Transcoder supports two types of captions: Embedded: Embedded captions are
-- included in the same file as the audio and video. Elastic Transcoder
-- supports only one embedded caption per language, to a maximum of 300
-- embedded captions per file. Valid input values include: CEA-608 (EIA-608,
-- first non-empty channel only), CEA-708 (EIA-708, first non-empty channel
-- only), and mov-text Valid outputs include: mov-text Elastic Transcoder
-- supports a maximum of one embedded format per output. Sidecar: Sidecar
-- captions are kept in a separate metadata file from the audio and video
-- data. Sidecar captions require a player that is capable of understanding
-- the relationship between the video file and the sidecar file. Elastic
-- Transcoder supports only one sidecar caption per language, to a maximum of
-- 20 sidecar captions per file. Valid input values include: dfxp (first div
-- element only), ebu-tt, scc, smpt, srt, ttml (first div element only), and
-- webvtt Valid outputs include: dfxp (first div element only), scc, srt, and
-- webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as
-- your output format. Elastic Transcoder does not support OCR (Optical
-- Character Recognition), does not accept pictures as a valid input for
-- captions, and is not available for audio-only transcoding. Elastic
-- Transcoder does not preserve text formatting (for example, italics) during
-- the transcoding process. To remove captions or leave the captions empty,
-- set Captions to null. To pass through existing captions unchanged, set the
-- MergePolicy to MergeRetain, and pass in a null CaptionSources array. For
-- more information on embedded files, see the Subtitles Wikipedia page. For
-- more information on sidecar files, see the Extensible Metadata Platform and
-- Sidecar file Wikipedia pages.
cjoCaptions
    :: Functor f
    => (Maybe Captions
    -> f (Maybe Captions))
    -> CreateJobOutput
    -> f CreateJobOutput
cjoCaptions f x =
    (\y -> x { _cjoCaptions = y })
       <$> f (_cjoCaptions x)
{-# INLINE cjoCaptions #-}

instance ToJSON CreateJobOutput

-- | Information about the master playlist.
data CreateJobPlaylist = CreateJobPlaylist
    { _cjvName :: Maybe Text
      -- ^ The name that you want Elastic Transcoder to assign to the master
      -- playlist, for example, nyc-vacation.m3u8. The name cannot include
      -- a / character. If you create more than one master playlist (not
      -- recommended), the values of all Name objects must be unique.
      -- Elastic Transcoder automatically appends .m3u8 to the file name.
      -- If you include .m3u8 in Name, it will appear twice in the file
      -- name.
    , _cjvFormat :: Maybe Text
      -- ^ This value must currently be HLSv3.
    , _cjvOutputKeys :: [Text]
      -- ^ For each output in this job that you want to include in a master
      -- playlist, the value of the Outputs:Key object. If you include
      -- more than one output in a playlist, the value of SegmentDuration
      -- for all of the outputs must be the same.
    } deriving (Show, Generic)

-- | The name that you want Elastic Transcoder to assign to the master playlist,
-- for example, nyc-vacation.m3u8. The name cannot include a / character. If
-- you create more than one master playlist (not recommended), the values of
-- all Name objects must be unique. Elastic Transcoder automatically appends
-- .m3u8 to the file name. If you include .m3u8 in Name, it will appear twice
-- in the file name.
cjvName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobPlaylist
    -> f CreateJobPlaylist
cjvName f x =
    (\y -> x { _cjvName = y })
       <$> f (_cjvName x)
{-# INLINE cjvName #-}

-- | This value must currently be HLSv3.
cjvFormat
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobPlaylist
    -> f CreateJobPlaylist
cjvFormat f x =
    (\y -> x { _cjvFormat = y })
       <$> f (_cjvFormat x)
{-# INLINE cjvFormat #-}

-- | For each output in this job that you want to include in a master playlist,
-- the value of the Outputs:Key object. If you include more than one output in
-- a playlist, the value of SegmentDuration for all of the outputs must be the
-- same.
cjvOutputKeys
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateJobPlaylist
    -> f CreateJobPlaylist
cjvOutputKeys f x =
    (\y -> x { _cjvOutputKeys = y })
       <$> f (_cjvOutputKeys x)
{-# INLINE cjvOutputKeys #-}

instance ToJSON CreateJobPlaylist

-- | A section of the response body that provides information about the job that
-- is created.
data Job = Job
    { _jbId :: Maybe Text
      -- ^ The identifier that Elastic Transcoder assigned to the job. You
      -- use this value to get settings for the job or to delete the job.
    , _jbArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for the job.
    , _jbPipelineId :: Maybe Text
      -- ^ The Id of the pipeline that you want Elastic Transcoder to use
      -- for transcoding. The pipeline determines several settings,
      -- including the Amazon S3 bucket from which Elastic Transcoder gets
      -- the files to transcode and the bucket into which Elastic
      -- Transcoder puts the transcoded files.
    , _jbInput :: Maybe JobInput
      -- ^ A section of the request or response body that provides
      -- information about the file that is being transcoded.
    , _jbOutput :: Maybe JobOutput
      -- ^ If you specified one output for a job, information about that
      -- output. If you specified multiple outputs for a job, the Output
      -- object lists information about the first output. This duplicates
      -- the information that is listed for the first output in the
      -- Outputs object. Outputs recommended instead. A section of the
      -- request or response body that provides information about the
      -- transcoded (target) file.
    , _jbOutputs :: [JobOutput]
      -- ^ Information about the output files. We recommend that you use the
      -- Outputs syntax for all jobs, even when you want Elastic
      -- Transcoder to transcode a file into only one format. Do not use
      -- both the Outputs and Output syntaxes in the same request. You can
      -- create a maximum of 30 outputs per job. If you specify more than
      -- one output for a job, Elastic Transcoder creates the files for
      -- each output in the order in which you specify them in the job.
    , _jbOutputKeyPrefix :: Maybe Text
      -- ^ The value, if any, that you want Elastic Transcoder to prepend to
      -- the names of all files that this job creates, including output
      -- files, thumbnails, and playlists. We recommend that you add a /
      -- or some other delimiter to the end of the OutputKeyPrefix.
    , _jbPlaylists :: [Playlist]
      -- ^ Outputs in MPEG-TS format only.If you specify a preset in
      -- PresetId for which the value of Container is ts (MPEG-TS),
      -- Playlists contains information about the master playlists that
      -- you want Elastic Transcoder to create. We recommend that you
      -- create only one master playlist. The maximum number of master
      -- playlists in a job is 30.
    , _jbStatus :: Maybe Text
      -- ^ The status of the job: Submitted, Progressing, Complete,
      -- Canceled, or Error.
    } deriving (Show, Generic)

-- | The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
jbId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Job
    -> f Job
jbId f x =
    (\y -> x { _jbId = y })
       <$> f (_jbId x)
{-# INLINE jbId #-}

-- | The Amazon Resource Name (ARN) for the job.
jbArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Job
    -> f Job
jbArn f x =
    (\y -> x { _jbArn = y })
       <$> f (_jbArn x)
{-# INLINE jbArn #-}

-- | The Id of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the Amazon
-- S3 bucket from which Elastic Transcoder gets the files to transcode and the
-- bucket into which Elastic Transcoder puts the transcoded files.
jbPipelineId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Job
    -> f Job
jbPipelineId f x =
    (\y -> x { _jbPipelineId = y })
       <$> f (_jbPipelineId x)
{-# INLINE jbPipelineId #-}

-- | A section of the request or response body that provides information about
-- the file that is being transcoded.
jbInput
    :: Functor f
    => (Maybe JobInput
    -> f (Maybe JobInput))
    -> Job
    -> f Job
jbInput f x =
    (\y -> x { _jbInput = y })
       <$> f (_jbInput x)
{-# INLINE jbInput #-}

-- | If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that is
-- listed for the first output in the Outputs object. Outputs recommended
-- instead. A section of the request or response body that provides
-- information about the transcoded (target) file.
jbOutput
    :: Functor f
    => (Maybe JobOutput
    -> f (Maybe JobOutput))
    -> Job
    -> f Job
jbOutput f x =
    (\y -> x { _jbOutput = y })
       <$> f (_jbOutput x)
{-# INLINE jbOutput #-}

-- | Information about the output files. We recommend that you use the Outputs
-- syntax for all jobs, even when you want Elastic Transcoder to transcode a
-- file into only one format. Do not use both the Outputs and Output syntaxes
-- in the same request. You can create a maximum of 30 outputs per job. If you
-- specify more than one output for a job, Elastic Transcoder creates the
-- files for each output in the order in which you specify them in the job.
jbOutputs
    :: Functor f
    => ([JobOutput]
    -> f ([JobOutput]))
    -> Job
    -> f Job
jbOutputs f x =
    (\y -> x { _jbOutputs = y })
       <$> f (_jbOutputs x)
{-# INLINE jbOutputs #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names
-- of all files that this job creates, including output files, thumbnails, and
-- playlists. We recommend that you add a / or some other delimiter to the end
-- of the OutputKeyPrefix.
jbOutputKeyPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Job
    -> f Job
jbOutputKeyPrefix f x =
    (\y -> x { _jbOutputKeyPrefix = y })
       <$> f (_jbOutputKeyPrefix x)
{-# INLINE jbOutputKeyPrefix #-}

-- | Outputs in MPEG-TS format only.If you specify a preset in PresetId for
-- which the value of Container is ts (MPEG-TS), Playlists contains
-- information about the master playlists that you want Elastic Transcoder to
-- create. We recommend that you create only one master playlist. The maximum
-- number of master playlists in a job is 30.
jbPlaylists
    :: Functor f
    => ([Playlist]
    -> f ([Playlist]))
    -> Job
    -> f Job
jbPlaylists f x =
    (\y -> x { _jbPlaylists = y })
       <$> f (_jbPlaylists x)
{-# INLINE jbPlaylists #-}

-- | The status of the job: Submitted, Progressing, Complete, Canceled, or
-- Error.
jbStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Job
    -> f Job
jbStatus f x =
    (\y -> x { _jbStatus = y })
       <$> f (_jbStatus x)
{-# INLINE jbStatus #-}

instance FromJSON Job

-- | Information about the album art that you want Elastic Transcoder to add to
-- the file during transcoding. You can specify up to twenty album artworks
-- for each output. Settings for each artwork must be defined in the job for
-- the current output.
data JobAlbumArt = JobAlbumArt
    { _jaaMergePolicy :: Maybe Text
      -- ^ A policy that determines how Elastic Transcoder will handle the
      -- existence of multiple album artwork files. Replace: The specified
      -- album art will replace any existing album art. Prepend: The
      -- specified album art will be placed in front of any existing album
      -- art. Append: The specified album art will be placed after any
      -- existing album art. Fallback: If the original input file contains
      -- artwork, Elastic Transcoder will use that artwork for the output.
      -- If the original input does not contain artwork, Elastic
      -- Transcoder will use the specified album art file.
    , _jaaArtwork :: [Artwork]
      -- ^ The file to be used as album art. There can be multiple artworks
      -- associated with an audio file, to a maximum of 20. Valid formats
      -- are .jpg and .png.
    } deriving (Show, Generic)

-- | A policy that determines how Elastic Transcoder will handle the existence
-- of multiple album artwork files. Replace: The specified album art will
-- replace any existing album art. Prepend: The specified album art will be
-- placed in front of any existing album art. Append: The specified album art
-- will be placed after any existing album art. Fallback: If the original
-- input file contains artwork, Elastic Transcoder will use that artwork for
-- the output. If the original input does not contain artwork, Elastic
-- Transcoder will use the specified album art file.
jaaMergePolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobAlbumArt
    -> f JobAlbumArt
jaaMergePolicy f x =
    (\y -> x { _jaaMergePolicy = y })
       <$> f (_jaaMergePolicy x)
{-# INLINE jaaMergePolicy #-}

-- | The file to be used as album art. There can be multiple artworks associated
-- with an audio file, to a maximum of 20. Valid formats are .jpg and .png.
jaaArtwork
    :: Functor f
    => ([Artwork]
    -> f ([Artwork]))
    -> JobAlbumArt
    -> f JobAlbumArt
jaaArtwork f x =
    (\y -> x { _jaaArtwork = y })
       <$> f (_jaaArtwork x)
{-# INLINE jaaArtwork #-}

instance FromJSON JobAlbumArt

instance ToJSON JobAlbumArt

-- | A section of the request body that provides information about the file that
-- is being transcoded.
data JobInput = JobInput
    { _jiKey :: Maybe Text
      -- ^ The name of the file to transcode. Elsewhere in the body of the
      -- JSON block is the the ID of the pipeline to use for processing
      -- the job. The InputBucket object in that pipeline tells Elastic
      -- Transcoder which Amazon S3 bucket to get the file from. If the
      -- file name includes a prefix, such as cooking/lasagna.mpg, include
      -- the prefix in the key. If the file isn't in the specified bucket,
      -- Elastic Transcoder returns an error.
    , _jiFrameRate :: Maybe Text
      -- ^ The frame rate of the input file. If you want Elastic Transcoder
      -- to automatically detect the frame rate of the input file, specify
      -- auto. If you want to specify the frame rate for the input file,
      -- enter one of the following values: 10, 15, 23.97, 24, 25, 29.97,
      -- 30, 60 If you specify a value other than auto, Elastic Transcoder
      -- disables automatic detection of the frame rate.
    , _jiResolution :: Maybe Text
      -- ^ This value must be auto, which causes Elastic Transcoder to
      -- automatically detect the resolution of the input file.
    , _jiAspectRatio :: Maybe Text
      -- ^ The aspect ratio of the input file. If you want Elastic
      -- Transcoder to automatically detect the aspect ratio of the input
      -- file, specify auto. If you want to specify the aspect ratio for
      -- the output file, enter one of the following values: 1:1, 4:3,
      -- 3:2, 16:9 If you specify a value other than auto, Elastic
      -- Transcoder disables automatic detection of the aspect ratio.
    , _jiInterlaced :: Maybe Text
      -- ^ Whether the input file is interlaced. If you want Elastic
      -- Transcoder to automatically detect whether the input file is
      -- interlaced, specify auto. If you want to specify whether the
      -- input file is interlaced, enter one of the following values:
      -- true, false If you specify a value other than auto, Elastic
      -- Transcoder disables automatic detection of interlacing.
    , _jiContainer :: Maybe Text
      -- ^ The container type for the input file. If you want Elastic
      -- Transcoder to automatically detect the container type of the
      -- input file, specify auto. If you want to specify the container
      -- type for the input file, enter one of the following values: 3gp,
      -- aac, asf, avi, divx, flv, m4a, mkv, mov, mp3, mp4, mpeg, mpeg-ps,
      -- mpeg-ts, mxf, ogg, vob, wav, webm.
    } deriving (Show, Generic)

-- | The name of the file to transcode. Elsewhere in the body of the JSON block
-- is the the ID of the pipeline to use for processing the job. The
-- InputBucket object in that pipeline tells Elastic Transcoder which Amazon
-- S3 bucket to get the file from. If the file name includes a prefix, such as
-- cooking/lasagna.mpg, include the prefix in the key. If the file isn't in
-- the specified bucket, Elastic Transcoder returns an error.
jiKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiKey f x =
    (\y -> x { _jiKey = y })
       <$> f (_jiKey x)
{-# INLINE jiKey #-}

-- | The frame rate of the input file. If you want Elastic Transcoder to
-- automatically detect the frame rate of the input file, specify auto. If you
-- want to specify the frame rate for the input file, enter one of the
-- following values: 10, 15, 23.97, 24, 25, 29.97, 30, 60 If you specify a
-- value other than auto, Elastic Transcoder disables automatic detection of
-- the frame rate.
jiFrameRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiFrameRate f x =
    (\y -> x { _jiFrameRate = y })
       <$> f (_jiFrameRate x)
{-# INLINE jiFrameRate #-}

-- | This value must be auto, which causes Elastic Transcoder to automatically
-- detect the resolution of the input file.
jiResolution
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiResolution f x =
    (\y -> x { _jiResolution = y })
       <$> f (_jiResolution x)
{-# INLINE jiResolution #-}

-- | The aspect ratio of the input file. If you want Elastic Transcoder to
-- automatically detect the aspect ratio of the input file, specify auto. If
-- you want to specify the aspect ratio for the output file, enter one of the
-- following values: 1:1, 4:3, 3:2, 16:9 If you specify a value other than
-- auto, Elastic Transcoder disables automatic detection of the aspect ratio.
jiAspectRatio
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiAspectRatio f x =
    (\y -> x { _jiAspectRatio = y })
       <$> f (_jiAspectRatio x)
{-# INLINE jiAspectRatio #-}

-- | Whether the input file is interlaced. If you want Elastic Transcoder to
-- automatically detect whether the input file is interlaced, specify auto. If
-- you want to specify whether the input file is interlaced, enter one of the
-- following values: true, false If you specify a value other than auto,
-- Elastic Transcoder disables automatic detection of interlacing.
jiInterlaced
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiInterlaced f x =
    (\y -> x { _jiInterlaced = y })
       <$> f (_jiInterlaced x)
{-# INLINE jiInterlaced #-}

-- | The container type for the input file. If you want Elastic Transcoder to
-- automatically detect the container type of the input file, specify auto. If
-- you want to specify the container type for the input file, enter one of the
-- following values: 3gp, aac, asf, avi, divx, flv, m4a, mkv, mov, mp3, mp4,
-- mpeg, mpeg-ps, mpeg-ts, mxf, ogg, vob, wav, webm.
jiContainer
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobInput
    -> f JobInput
jiContainer f x =
    (\y -> x { _jiContainer = y })
       <$> f (_jiContainer x)
{-# INLINE jiContainer #-}

instance FromJSON JobInput

instance ToJSON JobInput

-- | If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that is
-- listed for the first output in the Outputs object. Outputs recommended
-- instead. A section of the request or response body that provides
-- information about the transcoded (target) file.
data JobOutput = JobOutput
    { _joId :: Maybe Text
      -- ^ A sequential counter, starting with 1, that identifies an output
      -- among the outputs from the current job. In the Output syntax,
      -- this value is always 1.
    , _joKey :: Maybe Text
      -- ^ The name to assign to the transcoded file. Elastic Transcoder
      -- saves the file in the Amazon S3 bucket specified by the
      -- OutputBucket object in the pipeline that is specified by the
      -- pipeline ID.
    , _joThumbnailPattern :: Maybe Text
      -- ^ Whether you want Elastic Transcoder to create thumbnails for your
      -- videos and, if so, how you want Elastic Transcoder to name the
      -- files. If you don't want Elastic Transcoder to create thumbnails,
      -- specify "". If you do want Elastic Transcoder to create
      -- thumbnails, specify the information that you want to include in
      -- the file name for each thumbnail. You can specify the following
      -- values in any sequence: {count} (Required): If you want to create
      -- thumbnails, you must include {count} in the ThumbnailPattern
      -- object. Wherever you specify {count}, Elastic Transcoder adds a
      -- five-digit sequence number (beginning with 00001) to thumbnail
      -- file names. The number indicates where a given thumbnail appears
      -- in the sequence of thumbnails for a transcoded file. If you
      -- specify a literal value and/or {resolution} but you omit {count},
      -- Elastic Transcoder returns a validation error and does not create
      -- the job. Literal values (Optional): You can specify literal
      -- values anywhere in the ThumbnailPattern object. For example, you
      -- can include them as a file name prefix or as a delimiter between
      -- {resolution} and {count}. {resolution} (Optional): If you want
      -- Elastic Transcoder to include the resolution in the file name,
      -- include {resolution} in the ThumbnailPattern object. When
      -- creating thumbnails, Elastic Transcoder automatically saves the
      -- files in the format (.jpg or .png) that appears in the preset
      -- that you specified in the PresetID value of CreateJobOutput.
      -- Elastic Transcoder also appends the applicable file name
      -- extension.
    , _joRotate :: Maybe Text
      -- ^ The number of degrees clockwise by which you want Elastic
      -- Transcoder to rotate the output relative to the input. Enter one
      -- of the following values: auto, 0, 90, 180, 270 The value auto
      -- generally works only if the file that you're transcoding contains
      -- rotation metadata.
    , _joPresetId :: Maybe Text
      -- ^ The value of the Id object for the preset that you want to use
      -- for this job. The preset determines the audio, video, and
      -- thumbnail settings that Elastic Transcoder uses for transcoding.
      -- To use a preset that you created, specify the preset ID that
      -- Elastic Transcoder returned in the response when you created the
      -- preset. You can also use the Elastic Transcoder system presets,
      -- which you can get with ListPresets.
    , _joSegmentDuration :: Maybe Text
      -- ^ (Outputs in MPEG-TS format only.If you specify a preset in
      -- PresetId for which the value of Containeris ts (MPEG-TS),
      -- SegmentDuration is the maximum duration of each .ts file in
      -- seconds. The range of valid values is 1 to 60 seconds. If the
      -- duration of the video is not evenly divisible by SegmentDuration,
      -- the duration of the last segment is the remainder of total
      -- length/SegmentDuration. Elastic Transcoder creates an
      -- output-specific playlist for each output that you specify in
      -- OutputKeys. To add an output to the master playlist for this job,
      -- include it in OutputKeys.
    , _joStatus :: Maybe Text
      -- ^ The status of one output in a job. If you specified only one
      -- output for the job, Outputs:Status is always the same as
      -- Job:Status. If you specified more than one output: Job:Status and
      -- Outputs:Status for all of the outputs is Submitted until Elastic
      -- Transcoder starts to process the first output. When Elastic
      -- Transcoder starts to process the first output, Outputs:Status for
      -- that output and Job:Status both change to Progressing. For each
      -- output, the value of Outputs:Status remains Submitted until
      -- Elastic Transcoder starts to process the output. Job:Status
      -- remains Progressing until all of the outputs reach a terminal
      -- status, either Complete or Error. When all of the outputs reach a
      -- terminal status, Job:Status changes to Complete only if
      -- Outputs:Status for all of the outputs is Complete. If
      -- Outputs:Status for one or more outputs is Error, the terminal
      -- status for Job:Status is also Error. The value of Status is one
      -- of the following: Submitted, Progressing, Complete, Canceled, or
      -- Error.
    , _joStatusDetail :: Maybe Text
      -- ^ Information that further explains Status.
    , _joDuration :: Maybe Integer
      -- ^ Duration of the output file, in seconds.
    , _joWidth :: Maybe Integer
      -- ^ Specifies the width of the output file in pixels.
    , _joHeight :: Maybe Integer
      -- ^ Height of the output file, in pixels.
    , _joWatermarks :: [JobWatermark]
      -- ^ Information about the watermarks that you want Elastic Transcoder
      -- to add to the video during transcoding. You can specify up to
      -- four watermarks for each output. Settings for each watermark must
      -- be defined in the preset that you specify in Preset for the
      -- current output. Watermarks are added to the output video in the
      -- sequence in which you list them in the job output&#x2014;the
      -- first watermark in the list is added to the output video first,
      -- the second watermark in the list is added next, and so on. As a
      -- result, if the settings in a preset cause Elastic Transcoder to
      -- place all watermarks in the same location, the second watermark
      -- that you add will cover the first one, the third one will cover
      -- the second, and the fourth one will cover the third.
    , _joAlbumArt :: Maybe JobAlbumArt
      -- ^ The album art to be associated with the output file, if any.
    , _joComposition :: [Clip]
      -- ^ You can create an output file that contains an excerpt from the
      -- input file. This excerpt, called a clip, can come from the
      -- beginning, middle, or end of the file. The Composition object
      -- contains settings for the clips that make up an output file. For
      -- the current release, you can only specify settings for a single
      -- clip per output file. The Composition object cannot be null.
    , _joCaptions :: Maybe Captions
      -- ^ You can configure Elastic Transcoder to transcode captions, or
      -- subtitles, from one format to another. All captions must be in
      -- UTF-8. Elastic Transcoder supports two types of captions:
      -- Embedded: Embedded captions are included in the same file as the
      -- audio and video. Elastic Transcoder supports only one embedded
      -- caption per language, to a maximum of 300 embedded captions per
      -- file. Valid input values include: CEA-608 (EIA-608, first
      -- non-empty channel only), CEA-708 (EIA-708, first non-empty
      -- channel only), and mov-text Valid outputs include: mov-text
      -- Elastic Transcoder supports a maximum of one embedded format per
      -- output. Sidecar: Sidecar captions are kept in a separate metadata
      -- file from the audio and video data. Sidecar captions require a
      -- player that is capable of understanding the relationship between
      -- the video file and the sidecar file. Elastic Transcoder supports
      -- only one sidecar caption per language, to a maximum of 20 sidecar
      -- captions per file. Valid input values include: dfxp (first div
      -- element only), ebu-tt, scc, smpt, srt, ttml (first div element
      -- only), and webvtt Valid outputs include: dfxp (first div element
      -- only), scc, srt, and webvtt. If you want ttml or smpte-tt
      -- compatible captions, specify dfxp as your output format. Elastic
      -- Transcoder does not support OCR (Optical Character Recognition),
      -- does not accept pictures as a valid input for captions, and is
      -- not available for audio-only transcoding. Elastic Transcoder does
      -- not preserve text formatting (for example, italics) during the
      -- transcoding process. To remove captions or leave the captions
      -- empty, set Captions to null. To pass through existing captions
      -- unchanged, set the MergePolicy to MergeRetain, and pass in a null
      -- CaptionSources array. For more information on embedded files, see
      -- the Subtitles Wikipedia page. For more information on sidecar
      -- files, see the Extensible Metadata Platform and Sidecar file
      -- Wikipedia pages.
    } deriving (Show, Generic)

-- | A sequential counter, starting with 1, that identifies an output among the
-- outputs from the current job. In the Output syntax, this value is always 1.
joId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joId f x =
    (\y -> x { _joId = y })
       <$> f (_joId x)
{-# INLINE joId #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the OutputBucket object in the
-- pipeline that is specified by the pipeline ID.
joKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joKey f x =
    (\y -> x { _joKey = y })
       <$> f (_joKey x)
{-# INLINE joKey #-}

-- | Whether you want Elastic Transcoder to create thumbnails for your videos
-- and, if so, how you want Elastic Transcoder to name the files. If you don't
-- want Elastic Transcoder to create thumbnails, specify "". If you do want
-- Elastic Transcoder to create thumbnails, specify the information that you
-- want to include in the file name for each thumbnail. You can specify the
-- following values in any sequence: {count} (Required): If you want to create
-- thumbnails, you must include {count} in the ThumbnailPattern object.
-- Wherever you specify {count}, Elastic Transcoder adds a five-digit sequence
-- number (beginning with 00001) to thumbnail file names. The number indicates
-- where a given thumbnail appears in the sequence of thumbnails for a
-- transcoded file. If you specify a literal value and/or {resolution} but you
-- omit {count}, Elastic Transcoder returns a validation error and does not
-- create the job. Literal values (Optional): You can specify literal values
-- anywhere in the ThumbnailPattern object. For example, you can include them
-- as a file name prefix or as a delimiter between {resolution} and {count}.
-- {resolution} (Optional): If you want Elastic Transcoder to include the
-- resolution in the file name, include {resolution} in the ThumbnailPattern
-- object. When creating thumbnails, Elastic Transcoder automatically saves
-- the files in the format (.jpg or .png) that appears in the preset that you
-- specified in the PresetID value of CreateJobOutput. Elastic Transcoder also
-- appends the applicable file name extension.
joThumbnailPattern
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joThumbnailPattern f x =
    (\y -> x { _joThumbnailPattern = y })
       <$> f (_joThumbnailPattern x)
{-# INLINE joThumbnailPattern #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values:
-- auto, 0, 90, 180, 270 The value auto generally works only if the file that
-- you're transcoding contains rotation metadata.
joRotate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joRotate f x =
    (\y -> x { _joRotate = y })
       <$> f (_joRotate x)
{-# INLINE joRotate #-}

-- | The value of the Id object for the preset that you want to use for this
-- job. The preset determines the audio, video, and thumbnail settings that
-- Elastic Transcoder uses for transcoding. To use a preset that you created,
-- specify the preset ID that Elastic Transcoder returned in the response when
-- you created the preset. You can also use the Elastic Transcoder system
-- presets, which you can get with ListPresets.
joPresetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joPresetId f x =
    (\y -> x { _joPresetId = y })
       <$> f (_joPresetId x)
{-# INLINE joPresetId #-}

-- | (Outputs in MPEG-TS format only.If you specify a preset in PresetId for
-- which the value of Containeris ts (MPEG-TS), SegmentDuration is the maximum
-- duration of each .ts file in seconds. The range of valid values is 1 to 60
-- seconds. If the duration of the video is not evenly divisible by
-- SegmentDuration, the duration of the last segment is the remainder of total
-- length/SegmentDuration. Elastic Transcoder creates an output-specific
-- playlist for each output that you specify in OutputKeys. To add an output
-- to the master playlist for this job, include it in OutputKeys.
joSegmentDuration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joSegmentDuration f x =
    (\y -> x { _joSegmentDuration = y })
       <$> f (_joSegmentDuration x)
{-# INLINE joSegmentDuration #-}

-- | The status of one output in a job. If you specified only one output for the
-- job, Outputs:Status is always the same as Job:Status. If you specified more
-- than one output: Job:Status and Outputs:Status for all of the outputs is
-- Submitted until Elastic Transcoder starts to process the first output. When
-- Elastic Transcoder starts to process the first output, Outputs:Status for
-- that output and Job:Status both change to Progressing. For each output, the
-- value of Outputs:Status remains Submitted until Elastic Transcoder starts
-- to process the output. Job:Status remains Progressing until all of the
-- outputs reach a terminal status, either Complete or Error. When all of the
-- outputs reach a terminal status, Job:Status changes to Complete only if
-- Outputs:Status for all of the outputs is Complete. If Outputs:Status for
-- one or more outputs is Error, the terminal status for Job:Status is also
-- Error. The value of Status is one of the following: Submitted, Progressing,
-- Complete, Canceled, or Error.
joStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joStatus f x =
    (\y -> x { _joStatus = y })
       <$> f (_joStatus x)
{-# INLINE joStatus #-}

-- | Information that further explains Status.
joStatusDetail
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobOutput
    -> f JobOutput
joStatusDetail f x =
    (\y -> x { _joStatusDetail = y })
       <$> f (_joStatusDetail x)
{-# INLINE joStatusDetail #-}

-- | Duration of the output file, in seconds.
joDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> JobOutput
    -> f JobOutput
joDuration f x =
    (\y -> x { _joDuration = y })
       <$> f (_joDuration x)
{-# INLINE joDuration #-}

-- | Specifies the width of the output file in pixels.
joWidth
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> JobOutput
    -> f JobOutput
joWidth f x =
    (\y -> x { _joWidth = y })
       <$> f (_joWidth x)
{-# INLINE joWidth #-}

-- | Height of the output file, in pixels.
joHeight
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> JobOutput
    -> f JobOutput
joHeight f x =
    (\y -> x { _joHeight = y })
       <$> f (_joHeight x)
{-# INLINE joHeight #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to
-- the video during transcoding. You can specify up to four watermarks for
-- each output. Settings for each watermark must be defined in the preset that
-- you specify in Preset for the current output. Watermarks are added to the
-- output video in the sequence in which you list them in the job
-- output&#x2014;the first watermark in the list is added to the output video
-- first, the second watermark in the list is added next, and so on. As a
-- result, if the settings in a preset cause Elastic Transcoder to place all
-- watermarks in the same location, the second watermark that you add will
-- cover the first one, the third one will cover the second, and the fourth
-- one will cover the third.
joWatermarks
    :: Functor f
    => ([JobWatermark]
    -> f ([JobWatermark]))
    -> JobOutput
    -> f JobOutput
joWatermarks f x =
    (\y -> x { _joWatermarks = y })
       <$> f (_joWatermarks x)
{-# INLINE joWatermarks #-}

-- | The album art to be associated with the output file, if any.
joAlbumArt
    :: Functor f
    => (Maybe JobAlbumArt
    -> f (Maybe JobAlbumArt))
    -> JobOutput
    -> f JobOutput
joAlbumArt f x =
    (\y -> x { _joAlbumArt = y })
       <$> f (_joAlbumArt x)
{-# INLINE joAlbumArt #-}

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make
-- up an output file. For the current release, you can only specify settings
-- for a single clip per output file. The Composition object cannot be null.
joComposition
    :: Functor f
    => ([Clip]
    -> f ([Clip]))
    -> JobOutput
    -> f JobOutput
joComposition f x =
    (\y -> x { _joComposition = y })
       <$> f (_joComposition x)
{-# INLINE joComposition #-}

-- | You can configure Elastic Transcoder to transcode captions, or subtitles,
-- from one format to another. All captions must be in UTF-8. Elastic
-- Transcoder supports two types of captions: Embedded: Embedded captions are
-- included in the same file as the audio and video. Elastic Transcoder
-- supports only one embedded caption per language, to a maximum of 300
-- embedded captions per file. Valid input values include: CEA-608 (EIA-608,
-- first non-empty channel only), CEA-708 (EIA-708, first non-empty channel
-- only), and mov-text Valid outputs include: mov-text Elastic Transcoder
-- supports a maximum of one embedded format per output. Sidecar: Sidecar
-- captions are kept in a separate metadata file from the audio and video
-- data. Sidecar captions require a player that is capable of understanding
-- the relationship between the video file and the sidecar file. Elastic
-- Transcoder supports only one sidecar caption per language, to a maximum of
-- 20 sidecar captions per file. Valid input values include: dfxp (first div
-- element only), ebu-tt, scc, smpt, srt, ttml (first div element only), and
-- webvtt Valid outputs include: dfxp (first div element only), scc, srt, and
-- webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as
-- your output format. Elastic Transcoder does not support OCR (Optical
-- Character Recognition), does not accept pictures as a valid input for
-- captions, and is not available for audio-only transcoding. Elastic
-- Transcoder does not preserve text formatting (for example, italics) during
-- the transcoding process. To remove captions or leave the captions empty,
-- set Captions to null. To pass through existing captions unchanged, set the
-- MergePolicy to MergeRetain, and pass in a null CaptionSources array. For
-- more information on embedded files, see the Subtitles Wikipedia page. For
-- more information on sidecar files, see the Extensible Metadata Platform and
-- Sidecar file Wikipedia pages.
joCaptions
    :: Functor f
    => (Maybe Captions
    -> f (Maybe Captions))
    -> JobOutput
    -> f JobOutput
joCaptions f x =
    (\y -> x { _joCaptions = y })
       <$> f (_joCaptions x)
{-# INLINE joCaptions #-}

instance FromJSON JobOutput

instance ToJSON JobOutput

-- | Watermarks can be in .png or .jpg format. If you want to display a
-- watermark that is not rectangular, use the .png format, which supports
-- transparency.
data JobWatermark = JobWatermark
    { _jxPresetWatermarkId :: Maybe Text
      -- ^ The ID of the watermark settings that Elastic Transcoder uses to
      -- add watermarks to the video during transcoding. The settings are
      -- in the preset specified by Preset for the current output. In that
      -- preset, the value of Watermarks Id tells Elastic Transcoder which
      -- settings to use.
    , _jxInputKey :: Maybe Text
      -- ^ The name of the .png or .jpg file that you want to use for the
      -- watermark. To determine which Amazon S3 bucket contains the
      -- specified file, Elastic Transcoder checks the pipeline specified
      -- by Pipeline; the Input Bucket object in that pipeline identifies
      -- the bucket. If the file name includes a prefix, for example,
      -- logos/128x64.png, include the prefix in the key. If the file
      -- isn't in the specified bucket, Elastic Transcoder returns an
      -- error.
    } deriving (Show, Generic)

-- | The ID of the watermark settings that Elastic Transcoder uses to add
-- watermarks to the video during transcoding. The settings are in the preset
-- specified by Preset for the current output. In that preset, the value of
-- Watermarks Id tells Elastic Transcoder which settings to use.
jxPresetWatermarkId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobWatermark
    -> f JobWatermark
jxPresetWatermarkId f x =
    (\y -> x { _jxPresetWatermarkId = y })
       <$> f (_jxPresetWatermarkId x)
{-# INLINE jxPresetWatermarkId #-}

-- | The name of the .png or .jpg file that you want to use for the watermark.
-- To determine which Amazon S3 bucket contains the specified file, Elastic
-- Transcoder checks the pipeline specified by Pipeline; the Input Bucket
-- object in that pipeline identifies the bucket. If the file name includes a
-- prefix, for example, logos/128x64.png, include the prefix in the key. If
-- the file isn't in the specified bucket, Elastic Transcoder returns an
-- error.
jxInputKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> JobWatermark
    -> f JobWatermark
jxInputKey f x =
    (\y -> x { _jxInputKey = y })
       <$> f (_jxInputKey x)
{-# INLINE jxInputKey #-}

instance FromJSON JobWatermark

instance ToJSON JobWatermark

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify to report job status. To receive notifications, you must also
-- subscribe to the new topic in the Amazon SNS console. Progressing: The
-- topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic
-- that you want to notify when Elastic Transcoder has started to process a
-- job in this pipeline. This is the ARN that Amazon SNS returned when you
-- created the topic. For more information, see Create a Topic in the Amazon
-- Simple Notification Service Developer Guide. Completed: The topic ARN for
-- the Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing a job in this pipeline. This is the ARN that Amazon SNS
-- returned when you created the topic. Warning: The topic ARN for the Amazon
-- SNS topic that you want to notify when Elastic Transcoder encounters a
-- warning condition while processing a job in this pipeline. This is the ARN
-- that Amazon SNS returned when you created the topic. Error: The topic ARN
-- for the Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition while processing a job in this pipeline. This
-- is the ARN that Amazon SNS returned when you created the topic.
data Notifications = Notifications
    { _nsProgressing :: Maybe Text
      -- ^ The Amazon Simple Notification Service (Amazon SNS) topic that
      -- you want to notify when Elastic Transcoder has started to process
      -- the job.
    , _nsCompleted :: Maybe Text
      -- ^ The Amazon SNS topic that you want to notify when Elastic
      -- Transcoder has finished processing the job.
    , _nsWarning :: Maybe Text
      -- ^ The Amazon SNS topic that you want to notify when Elastic
      -- Transcoder encounters a warning condition.
    , _nsError :: Maybe Text
      -- ^ The Amazon SNS topic that you want to notify when Elastic
      -- Transcoder encounters an error condition.
    } deriving (Show, Generic)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify when Elastic Transcoder has started to process the job.
nsProgressing
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Notifications
    -> f Notifications
nsProgressing f x =
    (\y -> x { _nsProgressing = y })
       <$> f (_nsProgressing x)
{-# INLINE nsProgressing #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
nsCompleted
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Notifications
    -> f Notifications
nsCompleted f x =
    (\y -> x { _nsCompleted = y })
       <$> f (_nsCompleted x)
{-# INLINE nsCompleted #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
nsWarning
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Notifications
    -> f Notifications
nsWarning f x =
    (\y -> x { _nsWarning = y })
       <$> f (_nsWarning x)
{-# INLINE nsWarning #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
nsError
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Notifications
    -> f Notifications
nsError f x =
    (\y -> x { _nsError = y })
       <$> f (_nsError x)
{-# INLINE nsError #-}

instance FromJSON Notifications

instance ToJSON Notifications

-- | The Permission structure.
data Permission = Permission
    { _pnGranteeType :: Maybe Text
      -- ^ The type of value that appears in the Grantee object: Canonical:
      -- Either the canonical user ID for an AWS account or an origin
      -- access identity for an Amazon CloudFront distribution. A
      -- canonical user ID is not the same as an AWS account number.
      -- Email: The registered email address of an AWS account. Group: One
      -- of the following predefined Amazon S3 groups: AllUsers,
      -- AuthenticatedUsers, or LogDelivery.
    , _pnGrantee :: Maybe Text
      -- ^ The AWS user or group that you want to have access to transcoded
      -- files and playlists. To identify the user or group, you can
      -- specify the canonical user ID for an AWS account, an origin
      -- access identity for a CloudFront distribution, the registered
      -- email address of an AWS account, or a predefined Amazon S3 group.
    , _pnAccess :: [Text]
      -- ^ The permission that you want to give to the AWS user that is
      -- listed in Grantee. Valid values include: READ: The grantee can
      -- read the thumbnails and metadata for thumbnails that Elastic
      -- Transcoder adds to the Amazon S3 bucket. READ_ACP: The grantee
      -- can read the object ACL for thumbnails that Elastic Transcoder
      -- adds to the Amazon S3 bucket. WRITE_ACP: The grantee can write
      -- the ACL for the thumbnails that Elastic Transcoder adds to the
      -- Amazon S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP,
      -- and WRITE_ACP permissions for the thumbnails that Elastic
      -- Transcoder adds to the Amazon S3 bucket.
    } deriving (Show, Generic)

-- | The type of value that appears in the Grantee object: Canonical: Either the
-- canonical user ID for an AWS account or an origin access identity for an
-- Amazon CloudFront distribution. A canonical user ID is not the same as an
-- AWS account number. Email: The registered email address of an AWS account.
-- Group: One of the following predefined Amazon S3 groups: AllUsers,
-- AuthenticatedUsers, or LogDelivery.
pnGranteeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Permission
    -> f Permission
pnGranteeType f x =
    (\y -> x { _pnGranteeType = y })
       <$> f (_pnGranteeType x)
{-# INLINE pnGranteeType #-}

-- | The AWS user or group that you want to have access to transcoded files and
-- playlists. To identify the user or group, you can specify the canonical
-- user ID for an AWS account, an origin access identity for a CloudFront
-- distribution, the registered email address of an AWS account, or a
-- predefined Amazon S3 group.
pnGrantee
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Permission
    -> f Permission
pnGrantee f x =
    (\y -> x { _pnGrantee = y })
       <$> f (_pnGrantee x)
{-# INLINE pnGrantee #-}

-- | The permission that you want to give to the AWS user that is listed in
-- Grantee. Valid values include: READ: The grantee can read the thumbnails
-- and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3
-- bucket. READ_ACP: The grantee can read the object ACL for thumbnails that
-- Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The grantee can
-- write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and WRITE_ACP
-- permissions for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket.
pnAccess
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Permission
    -> f Permission
pnAccess f x =
    (\y -> x { _pnAccess = y })
       <$> f (_pnAccess x)
{-# INLINE pnAccess #-}

instance FromJSON Permission

instance ToJSON Permission

-- | A section of the response body that provides information about the pipeline
-- that is created.
data Pipeline = Pipeline
    { _peId :: Maybe Text
      -- ^ The identifier for the pipeline. You use this value to identify
      -- the pipeline in which you want to perform a variety of
      -- operations, such as creating a job or a preset.
    , _peArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for the pipeline.
    , _peName :: Maybe Text
      -- ^ The name of the pipeline. We recommend that the name be unique
      -- within the AWS account, but uniqueness is not enforced.
      -- Constraints: Maximum 40 characters.
    , _peStatus :: Maybe Text
      -- ^ The current status of the pipeline: Active: The pipeline is
      -- processing jobs. Paused: The pipeline is not currently processing
      -- jobs.
    , _peInputBucket :: Maybe Text
      -- ^ The Amazon S3 bucket from which Elastic Transcoder gets media
      -- files for transcoding and the graphics files, if any, that you
      -- want to use for watermarks.
    , _peOutputBucket :: Maybe Text
      -- ^ The Amazon S3 bucket in which you want Elastic Transcoder to save
      -- transcoded files, thumbnails, and playlists. Either you specify
      -- this value, or you specify both ContentConfig and
      -- ThumbnailConfig.
    , _peRole :: Maybe Text
      -- ^ The IAM Amazon Resource Name (ARN) for the role that Elastic
      -- Transcoder uses to transcode jobs for this pipeline.
    , _peNotifications :: Maybe Notifications
      -- ^ The Amazon Simple Notification Service (Amazon SNS) topic that
      -- you want to notify to report job status. To receive
      -- notifications, you must also subscribe to the new topic in the
      -- Amazon SNS console. Progressing (optional): The Amazon Simple
      -- Notification Service (Amazon SNS) topic that you want to notify
      -- when Elastic Transcoder has started to process the job. Completed
      -- (optional): The Amazon SNS topic that you want to notify when
      -- Elastic Transcoder has finished processing the job. Warning
      -- (optional): The Amazon SNS topic that you want to notify when
      -- Elastic Transcoder encounters a warning condition. Error
      -- (optional): The Amazon SNS topic that you want to notify when
      -- Elastic Transcoder encounters an error condition.
    , _peContentConfig :: Maybe PipelineOutputConfig
      -- ^ Information about the Amazon S3 bucket in which you want Elastic
      -- Transcoder to save transcoded files and playlists. Either you
      -- specify both ContentConfig and ThumbnailConfig, or you specify
      -- OutputBucket. Bucket: The Amazon S3 bucket in which you want
      -- Elastic Transcoder to save transcoded files and playlists.
      -- Permissions: A list of the users and/or predefined Amazon S3
      -- groups you want to have access to transcoded files and playlists,
      -- and the type of access that you want them to have. GranteeType:
      -- The type of value that appears in the Grantee object: Canonical:
      -- Either the canonical user ID for an AWS account or an origin
      -- access identity for an Amazon CloudFront distribution. Email: The
      -- registered email address of an AWS account. Group: One of the
      -- following predefined Amazon S3 groups: AllUsers,
      -- AuthenticatedUsers, or LogDelivery. Grantee: The AWS user or
      -- group that you want to have access to transcoded files and
      -- playlists. Access: The permission that you want to give to the
      -- AWS user that is listed in Grantee. Valid values include: READ:
      -- The grantee can read the objects and metadata for objects that
      -- Elastic Transcoder adds to the Amazon S3 bucket. READ_ACP: The
      -- grantee can read the object ACL for objects that Elastic
      -- Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The grantee
      -- can write the ACL for the objects that Elastic Transcoder adds to
      -- the Amazon S3 bucket. FULL_CONTROL: The grantee has READ,
      -- READ_ACP, and WRITE_ACP permissions for the objects that Elastic
      -- Transcoder adds to the Amazon S3 bucket. StorageClass: The Amazon
      -- S3 storage class, Standard or ReducedRedundancy, that you want
      -- Elastic Transcoder to assign to the video files and playlists
      -- that it stores in your Amazon S3 bucket.
    , _peThumbnailConfig :: Maybe PipelineOutputConfig
      -- ^ Information about the Amazon S3 bucket in which you want Elastic
      -- Transcoder to save thumbnail files. Either you specify both
      -- ContentConfig and ThumbnailConfig, or you specify OutputBucket.
      -- Bucket: The Amazon S3 bucket in which you want Elastic Transcoder
      -- to save thumbnail files. Permissions: A list of the users and/or
      -- predefined Amazon S3 groups you want to have access to thumbnail
      -- files, and the type of access that you want them to have.
      -- GranteeType: The type of value that appears in the Grantee
      -- object: Canonical: Either the canonical user ID for an AWS
      -- account or an origin access identity for an Amazon CloudFront
      -- distribution. A canonical user ID is not the same as an AWS
      -- account number. Email: The registered email address of an AWS
      -- account. Group: One of the following predefined Amazon S3 groups:
      -- AllUsers, AuthenticatedUsers, or LogDelivery. Grantee: The AWS
      -- user or group that you want to have access to thumbnail files.
      -- Access: The permission that you want to give to the AWS user that
      -- is listed in Grantee. Valid values include: READ: The grantee can
      -- read the thumbnails and metadata for thumbnails that Elastic
      -- Transcoder adds to the Amazon S3 bucket. READ_ACP: The grantee
      -- can read the object ACL for thumbnails that Elastic Transcoder
      -- adds to the Amazon S3 bucket. WRITE_ACP: The grantee can write
      -- the ACL for the thumbnails that Elastic Transcoder adds to the
      -- Amazon S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP,
      -- and WRITE_ACP permissions for the thumbnails that Elastic
      -- Transcoder adds to the Amazon S3 bucket. StorageClass: The Amazon
      -- S3 storage class, Standard or ReducedRedundancy, that you want
      -- Elastic Transcoder to assign to the thumbnails that it stores in
      -- your Amazon S3 bucket.
    } deriving (Show, Generic)

-- | The identifier for the pipeline. You use this value to identify the
-- pipeline in which you want to perform a variety of operations, such as
-- creating a job or a preset.
peId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peId f x =
    (\y -> x { _peId = y })
       <$> f (_peId x)
{-# INLINE peId #-}

-- | The Amazon Resource Name (ARN) for the pipeline.
peArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peArn f x =
    (\y -> x { _peArn = y })
       <$> f (_peArn x)
{-# INLINE peArn #-}

-- | The name of the pipeline. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced. Constraints: Maximum 40
-- characters.
peName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peName f x =
    (\y -> x { _peName = y })
       <$> f (_peName x)
{-# INLINE peName #-}

-- | The current status of the pipeline: Active: The pipeline is processing
-- jobs. Paused: The pipeline is not currently processing jobs.
peStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peStatus f x =
    (\y -> x { _peStatus = y })
       <$> f (_peStatus x)
{-# INLINE peStatus #-}

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for
-- transcoding and the graphics files, if any, that you want to use for
-- watermarks.
peInputBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peInputBucket f x =
    (\y -> x { _peInputBucket = y })
       <$> f (_peInputBucket x)
{-# INLINE peInputBucket #-}

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save
-- transcoded files, thumbnails, and playlists. Either you specify this value,
-- or you specify both ContentConfig and ThumbnailConfig.
peOutputBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peOutputBucket f x =
    (\y -> x { _peOutputBucket = y })
       <$> f (_peOutputBucket x)
{-# INLINE peOutputBucket #-}

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder
-- uses to transcode jobs for this pipeline.
peRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Pipeline
    -> f Pipeline
peRole f x =
    (\y -> x { _peRole = y })
       <$> f (_peRole x)
{-# INLINE peRole #-}

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify to report job status. To receive notifications, you must also
-- subscribe to the new topic in the Amazon SNS console. Progressing
-- (optional): The Amazon Simple Notification Service (Amazon SNS) topic that
-- you want to notify when Elastic Transcoder has started to process the job.
-- Completed (optional): The Amazon SNS topic that you want to notify when
-- Elastic Transcoder has finished processing the job. Warning (optional): The
-- Amazon SNS topic that you want to notify when Elastic Transcoder encounters
-- a warning condition. Error (optional): The Amazon SNS topic that you want
-- to notify when Elastic Transcoder encounters an error condition.
peNotifications
    :: Functor f
    => (Maybe Notifications
    -> f (Maybe Notifications))
    -> Pipeline
    -> f Pipeline
peNotifications f x =
    (\y -> x { _peNotifications = y })
       <$> f (_peNotifications x)
{-# INLINE peNotifications #-}

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder
-- to save transcoded files and playlists. Either you specify both
-- ContentConfig and ThumbnailConfig, or you specify OutputBucket. Bucket: The
-- Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
-- files and playlists. Permissions: A list of the users and/or predefined
-- Amazon S3 groups you want to have access to transcoded files and playlists,
-- and the type of access that you want them to have. GranteeType: The type of
-- value that appears in the Grantee object: Canonical: Either the canonical
-- user ID for an AWS account or an origin access identity for an Amazon
-- CloudFront distribution. Email: The registered email address of an AWS
-- account. Group: One of the following predefined Amazon S3 groups: AllUsers,
-- AuthenticatedUsers, or LogDelivery. Grantee: The AWS user or group that you
-- want to have access to transcoded files and playlists. Access: The
-- permission that you want to give to the AWS user that is listed in Grantee.
-- Valid values include: READ: The grantee can read the objects and metadata
-- for objects that Elastic Transcoder adds to the Amazon S3 bucket. READ_ACP:
-- The grantee can read the object ACL for objects that Elastic Transcoder
-- adds to the Amazon S3 bucket. WRITE_ACP: The grantee can write the ACL for
-- the objects that Elastic Transcoder adds to the Amazon S3 bucket.
-- FULL_CONTROL: The grantee has READ, READ_ACP, and WRITE_ACP permissions for
-- the objects that Elastic Transcoder adds to the Amazon S3 bucket.
-- StorageClass: The Amazon S3 storage class, Standard or ReducedRedundancy,
-- that you want Elastic Transcoder to assign to the video files and playlists
-- that it stores in your Amazon S3 bucket.
peContentConfig
    :: Functor f
    => (Maybe PipelineOutputConfig
    -> f (Maybe PipelineOutputConfig))
    -> Pipeline
    -> f Pipeline
peContentConfig f x =
    (\y -> x { _peContentConfig = y })
       <$> f (_peContentConfig x)
{-# INLINE peContentConfig #-}

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder
-- to save thumbnail files. Either you specify both ContentConfig and
-- ThumbnailConfig, or you specify OutputBucket. Bucket: The Amazon S3 bucket
-- in which you want Elastic Transcoder to save thumbnail files. Permissions:
-- A list of the users and/or predefined Amazon S3 groups you want to have
-- access to thumbnail files, and the type of access that you want them to
-- have. GranteeType: The type of value that appears in the Grantee object:
-- Canonical: Either the canonical user ID for an AWS account or an origin
-- access identity for an Amazon CloudFront distribution. A canonical user ID
-- is not the same as an AWS account number. Email: The registered email
-- address of an AWS account. Group: One of the following predefined Amazon S3
-- groups: AllUsers, AuthenticatedUsers, or LogDelivery. Grantee: The AWS user
-- or group that you want to have access to thumbnail files. Access: The
-- permission that you want to give to the AWS user that is listed in Grantee.
-- Valid values include: READ: The grantee can read the thumbnails and
-- metadata for thumbnails that Elastic Transcoder adds to the Amazon S3
-- bucket. READ_ACP: The grantee can read the object ACL for thumbnails that
-- Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The grantee can
-- write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and WRITE_ACP
-- permissions for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket. StorageClass: The Amazon S3 storage class, Standard or
-- ReducedRedundancy, that you want Elastic Transcoder to assign to the
-- thumbnails that it stores in your Amazon S3 bucket.
peThumbnailConfig
    :: Functor f
    => (Maybe PipelineOutputConfig
    -> f (Maybe PipelineOutputConfig))
    -> Pipeline
    -> f Pipeline
peThumbnailConfig f x =
    (\y -> x { _peThumbnailConfig = y })
       <$> f (_peThumbnailConfig x)
{-# INLINE peThumbnailConfig #-}

instance FromJSON Pipeline

-- | The optional ContentConfig object specifies information about the Amazon S3
-- bucket in which you want Elastic Transcoder to save transcoded files and
-- playlists: which bucket to use, which users you want to have access to the
-- files, the type of access you want users to have, and the storage class
-- that you want to assign to the files. If you specify values for
-- ContentConfig, you must also specify values for ThumbnailConfig. If you
-- specify values for ContentConfig and ThumbnailConfig, omit the OutputBucket
-- object. Bucket: The Amazon S3 bucket in which you want Elastic Transcoder
-- to save transcoded files and playlists. Permissions (Optional): The
-- Permissions object specifies which users you want to have access to
-- transcoded files and the type of access you want them to have. You can
-- grant permissions to a maximum of 30 users and/or predefined Amazon S3
-- groups. Grantee Type: Specify the type of value that appears in the Grantee
-- object: Canonical: The value in the Grantee object is either the canonical
-- user ID for an AWS account or an origin access identity for an Amazon
-- CloudFront distribution. For more information about canonical user IDs, see
-- Access Control List (ACL) Overview in the Amazon Simple Storage Service
-- Developer Guide. For more information about using CloudFront origin access
-- identities to require that users use CloudFront URLs instead of Amazon S3
-- URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon
-- S3 Content. A canonical user ID is not the same as an AWS account number.
-- Email: The value in the Grantee object is the registered email address of
-- an AWS account. Group: The value in the Grantee object is one of the
-- following predefined Amazon S3 groups: AllUsers, AuthenticatedUsers, or
-- LogDelivery. Grantee: The AWS user or group that you want to have access to
-- transcoded files and playlists. To identify the user or group, you can
-- specify the canonical user ID for an AWS account, an origin access identity
-- for a CloudFront distribution, the registered email address of an AWS
-- account, or a predefined Amazon S3 group Access: The permission that you
-- want to give to the AWS user that you specified in Grantee. Permissions are
-- granted on the files that Elastic Transcoder adds to the bucket, including
-- playlists and video files. Valid values include: READ: The grantee can read
-- the objects and metadata for objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. READ_ACP: The grantee can read the object ACL for objects
-- that Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The
-- grantee can write the ACL for the objects that Elastic Transcoder adds to
-- the Amazon S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and
-- WRITE_ACP permissions for the objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. StorageClass: The Amazon S3 storage class, Standard or
-- ReducedRedundancy, that you want Elastic Transcoder to assign to the video
-- files and playlists that it stores in your Amazon S3 bucket.
data PipelineOutputConfig = PipelineOutputConfig
    { _pocBucket :: Maybe Text
      -- ^ The Amazon S3 bucket in which you want Elastic Transcoder to save
      -- the transcoded files. Specify this value when all of the
      -- following are true: You want to save transcoded files, thumbnails
      -- (if any), and playlists (if any) together in one bucket. You do
      -- not want to specify the users or groups who have access to the
      -- transcoded files, thumbnails, and playlists. You do not want to
      -- specify the permissions that Elastic Transcoder grants to the
      -- files. You want to associate the transcoded files and thumbnails
      -- with the Amazon S3 Standard storage class. If you want to save
      -- transcoded files and playlists in one bucket and thumbnails in
      -- another bucket, specify which users can access the transcoded
      -- files or the permissions the users have, or change the Amazon S3
      -- storage class, omit OutputBucket and specify values for
      -- ContentConfig and ThumbnailConfig instead.
    , _pocStorageClass :: Maybe Text
      -- ^ The Amazon S3 storage class, Standard or ReducedRedundancy, that
      -- you want Elastic Transcoder to assign to the video files and
      -- playlists that it stores in your Amazon S3 bucket.
    , _pocPermissions :: [Permission]
      -- ^ Optional. The Permissions object specifies which users and/or
      -- predefined Amazon S3 groups you want to have access to transcoded
      -- files and playlists, and the type of access you want them to
      -- have. You can grant permissions to a maximum of 30 users and/or
      -- predefined Amazon S3 groups. If you include Permissions, Elastic
      -- Transcoder grants only the permissions that you specify. It does
      -- not grant full permissions to the owner of the role specified by
      -- Role. If you want that user to have full control, you must
      -- explicitly grant full control to the user. If you omit
      -- Permissions, Elastic Transcoder grants full control over the
      -- transcoded files and playlists to the owner of the role specified
      -- by Role, and grants no other permissions to any other user or
      -- group.
    } deriving (Show, Generic)

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. Specify this value when all of the following are true:
-- You want to save transcoded files, thumbnails (if any), and playlists (if
-- any) together in one bucket. You do not want to specify the users or groups
-- who have access to the transcoded files, thumbnails, and playlists. You do
-- not want to specify the permissions that Elastic Transcoder grants to the
-- files. You want to associate the transcoded files and thumbnails with the
-- Amazon S3 Standard storage class. If you want to save transcoded files and
-- playlists in one bucket and thumbnails in another bucket, specify which
-- users can access the transcoded files or the permissions the users have, or
-- change the Amazon S3 storage class, omit OutputBucket and specify values
-- for ContentConfig and ThumbnailConfig instead.
pocBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PipelineOutputConfig
    -> f PipelineOutputConfig
pocBucket f x =
    (\y -> x { _pocBucket = y })
       <$> f (_pocBucket x)
{-# INLINE pocBucket #-}

-- | The Amazon S3 storage class, Standard or ReducedRedundancy, that you want
-- Elastic Transcoder to assign to the video files and playlists that it
-- stores in your Amazon S3 bucket.
pocStorageClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PipelineOutputConfig
    -> f PipelineOutputConfig
pocStorageClass f x =
    (\y -> x { _pocStorageClass = y })
       <$> f (_pocStorageClass x)
{-# INLINE pocStorageClass #-}

-- | Optional. The Permissions object specifies which users and/or predefined
-- Amazon S3 groups you want to have access to transcoded files and playlists,
-- and the type of access you want them to have. You can grant permissions to
-- a maximum of 30 users and/or predefined Amazon S3 groups. If you include
-- Permissions, Elastic Transcoder grants only the permissions that you
-- specify. It does not grant full permissions to the owner of the role
-- specified by Role. If you want that user to have full control, you must
-- explicitly grant full control to the user. If you omit Permissions, Elastic
-- Transcoder grants full control over the transcoded files and playlists to
-- the owner of the role specified by Role, and grants no other permissions to
-- any other user or group.
pocPermissions
    :: Functor f
    => ([Permission]
    -> f ([Permission]))
    -> PipelineOutputConfig
    -> f PipelineOutputConfig
pocPermissions f x =
    (\y -> x { _pocPermissions = y })
       <$> f (_pocPermissions x)
{-# INLINE pocPermissions #-}

instance FromJSON PipelineOutputConfig

instance ToJSON PipelineOutputConfig

-- | Use Only for MPEG-TS Outputs. If you specify a preset for which the value
-- of Container is ts (MPEG-TS), Playlists contains information about the
-- master playlists that you want Elastic Transcoder to create. We recommend
-- that you create only one master playlist. The maximum number of master
-- playlists in a job is 30.
data Playlist = Playlist
    { _puName :: Maybe Text
      -- ^ The name that you want Elastic Transcoder to assign to the master
      -- playlist, for example, nyc-vacation.m3u8. The name cannot include
      -- a / character. If you create more than one master playlist (not
      -- recommended), the values of all Name objects must be unique.
      -- Note: Elastic Transcoder automatically appends .m3u8 to the file
      -- name. If you include .m3u8 in Name, it will appear twice in the
      -- file name.
    , _puFormat :: Maybe Text
      -- ^ This value must currently be HLSv3.
    , _puOutputKeys :: [Text]
      -- ^ For each output in this job that you want to include in a master
      -- playlist, the value of the Outputs:Key object. If you include
      -- more than one output in a playlist, the value of SegmentDuration
      -- for all of the outputs must be the same.
    , _puStatus :: Maybe Text
      -- ^ The status of the job with which the playlist is associated.
    , _puStatusDetail :: Maybe Text
      -- ^ Information that further explains the status.
    } deriving (Show, Generic)

-- | The name that you want Elastic Transcoder to assign to the master playlist,
-- for example, nyc-vacation.m3u8. The name cannot include a / character. If
-- you create more than one master playlist (not recommended), the values of
-- all Name objects must be unique. Note: Elastic Transcoder automatically
-- appends .m3u8 to the file name. If you include .m3u8 in Name, it will
-- appear twice in the file name.
puName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Playlist
    -> f Playlist
puName f x =
    (\y -> x { _puName = y })
       <$> f (_puName x)
{-# INLINE puName #-}

-- | This value must currently be HLSv3.
puFormat
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Playlist
    -> f Playlist
puFormat f x =
    (\y -> x { _puFormat = y })
       <$> f (_puFormat x)
{-# INLINE puFormat #-}

-- | For each output in this job that you want to include in a master playlist,
-- the value of the Outputs:Key object. If you include more than one output in
-- a playlist, the value of SegmentDuration for all of the outputs must be the
-- same.
puOutputKeys
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Playlist
    -> f Playlist
puOutputKeys f x =
    (\y -> x { _puOutputKeys = y })
       <$> f (_puOutputKeys x)
{-# INLINE puOutputKeys #-}

-- | The status of the job with which the playlist is associated.
puStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Playlist
    -> f Playlist
puStatus f x =
    (\y -> x { _puStatus = y })
       <$> f (_puStatus x)
{-# INLINE puStatus #-}

-- | Information that further explains the status.
puStatusDetail
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Playlist
    -> f Playlist
puStatusDetail f x =
    (\y -> x { _puStatusDetail = y })
       <$> f (_puStatusDetail x)
{-# INLINE puStatusDetail #-}

instance FromJSON Playlist

instance ToJSON Playlist

-- | A section of the response body that provides information about the preset
-- that is created.
data Preset = Preset
    { _ppuId :: Maybe Text
      -- ^ Identifier for the new preset. You use this value to get settings
      -- for the preset or to delete it.
    , _ppuArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for the preset.
    , _ppuName :: Maybe Text
      -- ^ The name of the preset.
    , _ppuDescription :: Maybe Text
      -- ^ A description of the preset.
    , _ppuContainer :: Maybe Text
      -- ^ The container type for the output file. Valid values include mp3,
      -- mp4, ogg, ts, and webm.
    , _ppuAudio :: Maybe AudioParameters
      -- ^ A section of the response body that provides information about
      -- the audio preset values.
    , _ppuVideo :: Maybe VideoParameters
      -- ^ A section of the response body that provides information about
      -- the video preset values.
    , _ppuThumbnails :: Maybe Thumbnails
      -- ^ A section of the response body that provides information about
      -- the thumbnail preset values, if any.
    , _ppuType :: Maybe Text
      -- ^ Whether the preset is a default preset provided by Elastic
      -- Transcoder (System) or a preset that you have defined (Custom).
    } deriving (Show, Generic)

-- | Identifier for the new preset. You use this value to get settings for the
-- preset or to delete it.
ppuId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuId f x =
    (\y -> x { _ppuId = y })
       <$> f (_ppuId x)
{-# INLINE ppuId #-}

-- | The Amazon Resource Name (ARN) for the preset.
ppuArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuArn f x =
    (\y -> x { _ppuArn = y })
       <$> f (_ppuArn x)
{-# INLINE ppuArn #-}

-- | The name of the preset.
ppuName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuName f x =
    (\y -> x { _ppuName = y })
       <$> f (_ppuName x)
{-# INLINE ppuName #-}

-- | A description of the preset.
ppuDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuDescription f x =
    (\y -> x { _ppuDescription = y })
       <$> f (_ppuDescription x)
{-# INLINE ppuDescription #-}

-- | The container type for the output file. Valid values include mp3, mp4, ogg,
-- ts, and webm.
ppuContainer
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuContainer f x =
    (\y -> x { _ppuContainer = y })
       <$> f (_ppuContainer x)
{-# INLINE ppuContainer #-}

-- | A section of the response body that provides information about the audio
-- preset values.
ppuAudio
    :: Functor f
    => (Maybe AudioParameters
    -> f (Maybe AudioParameters))
    -> Preset
    -> f Preset
ppuAudio f x =
    (\y -> x { _ppuAudio = y })
       <$> f (_ppuAudio x)
{-# INLINE ppuAudio #-}

-- | A section of the response body that provides information about the video
-- preset values.
ppuVideo
    :: Functor f
    => (Maybe VideoParameters
    -> f (Maybe VideoParameters))
    -> Preset
    -> f Preset
ppuVideo f x =
    (\y -> x { _ppuVideo = y })
       <$> f (_ppuVideo x)
{-# INLINE ppuVideo #-}

-- | A section of the response body that provides information about the
-- thumbnail preset values, if any.
ppuThumbnails
    :: Functor f
    => (Maybe Thumbnails
    -> f (Maybe Thumbnails))
    -> Preset
    -> f Preset
ppuThumbnails f x =
    (\y -> x { _ppuThumbnails = y })
       <$> f (_ppuThumbnails x)
{-# INLINE ppuThumbnails #-}

-- | Whether the preset is a default preset provided by Elastic Transcoder
-- (System) or a preset that you have defined (Custom).
ppuType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Preset
    -> f Preset
ppuType f x =
    (\y -> x { _ppuType = y })
       <$> f (_ppuType x)
{-# INLINE ppuType #-}

instance FromJSON Preset

-- | Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks
-- appear in the specified size and location, and with the specified opacity
-- for the duration of the transcoded video. Watermarks can be in .png or .jpg
-- format. If you want to display a watermark that is not rectangular, use the
-- .png format, which supports transparency. When you create a job that uses
-- this preset, you specify the .png or .jpg graphics that you want Elastic
-- Transcoder to include in the transcoded videos. You can specify fewer
-- graphics in the job than you specify watermark settings in the preset,
-- which allows you to use the same preset for up to four watermarks that have
-- different dimensions.
data PresetWatermark = PresetWatermark
    { _ppkId :: Maybe Text
      -- ^ A unique identifier for the settings for one watermark. The value
      -- of Id can be up to 40 characters long.
    , _ppkMaxWidth :: Maybe Text
      -- ^ The maximum width of the watermark in one of the following
      -- formats: number of pixels (px): The minimum value is 16 pixels,
      -- and the maximum value is the value of MaxWidth. integer
      -- percentage (%): The range of valid values is 0 to 100. Use the
      -- value of Target to specify whether you want Elastic Transcoder to
      -- include the black bars that are added by Elastic Transcoder, if
      -- any, in the calculation. If you specify the value in pixels, it
      -- must be less than or equal to the value of MaxWidth.
    , _ppkMaxHeight :: Maybe Text
      -- ^ The maximum height of the watermark in one of the following
      -- formats: number of pixels (px): The minimum value is 16 pixels,
      -- and the maximum value is the value of MaxHeight. integer
      -- percentage (%): The range of valid values is 0 to 100. Use the
      -- value of Target to specify whether you want Elastic Transcoder to
      -- include the black bars that are added by Elastic Transcoder, if
      -- any, in the calculation. If you specify the value in pixels, it
      -- must be less than or equal to the value of MaxHeight.
    , _ppkSizingPolicy :: Maybe Text
      -- ^ A value that controls scaling of the watermark: Fit: Elastic
      -- Transcoder scales the watermark so it matches the value that you
      -- specified in either MaxWidth or MaxHeight without exceeding the
      -- other value. Stretch: Elastic Transcoder stretches the watermark
      -- to match the values that you specified for MaxWidth and
      -- MaxHeight. If the relative proportions of the watermark and the
      -- values of MaxWidth and MaxHeight are different, the watermark
      -- will be distorted. ShrinkToFit: Elastic Transcoder scales the
      -- watermark down so that its dimensions match the values that you
      -- specified for at least one of MaxWidth and MaxHeight without
      -- exceeding either value. If you specify this option, Elastic
      -- Transcoder does not scale the watermark up.
    , _ppkHorizontalAlign :: Maybe Text
      -- ^ The horizontal position of the watermark unless you specify a
      -- non-zero value for HorizontalOffset: Left: The left edge of the
      -- watermark is aligned with the left border of the video. Right:
      -- The right edge of the watermark is aligned with the right border
      -- of the video. Center: The watermark is centered between the left
      -- and right borders.
    , _ppkHorizontalOffset :: Maybe Text
      -- ^ The amount by which you want the horizontal position of the
      -- watermark to be offset from the position specified by
      -- HorizontalAlign: number of pixels (px): The minimum value is 0
      -- pixels, and the maximum value is the value of MaxWidth. integer
      -- percentage (%): The range of valid values is 0 to 100. For
      -- example, if you specify Left for HorizontalAlign and 5px for
      -- HorizontalOffset, the left side of the watermark appears 5 pixels
      -- from the left border of the output video. HorizontalOffset is
      -- only valid when the value of HorizontalAlign is Left or Right. If
      -- you specify an offset that causes the watermark to extend beyond
      -- the left or right border and Elastic Transcoder has not added
      -- black bars, the watermark is cropped. If Elastic Transcoder has
      -- added black bars, the watermark extends into the black bars. If
      -- the watermark extends beyond the black bars, it is cropped. Use
      -- the value of Target to specify whether you want to include the
      -- black bars that are added by Elastic Transcoder, if any, in the
      -- offset calculation.
    , _ppkVerticalAlign :: Maybe Text
      -- ^ The vertical position of the watermark unless you specify a
      -- non-zero value for VerticalOffset: Top: The top edge of the
      -- watermark is aligned with the top border of the video. Bottom:
      -- The bottom edge of the watermark is aligned with the bottom
      -- border of the video. Center: The watermark is centered between
      -- the top and bottom borders.
    , _ppkVerticalOffset :: Maybe Text
      -- ^ VerticalOffset The amount by which you want the vertical position
      -- of the watermark to be offset from the position specified by
      -- VerticalAlign: number of pixels (px): The minimum value is 0
      -- pixels, and the maximum value is the value of MaxHeight. integer
      -- percentage (%): The range of valid values is 0 to 100. For
      -- example, if you specify Top for VerticalAlign and 5px for
      -- VerticalOffset, the top of the watermark appears 5 pixels from
      -- the top border of the output video. VerticalOffset is only valid
      -- when the value of VerticalAlign is Top or Bottom. If you specify
      -- an offset that causes the watermark to extend beyond the top or
      -- bottom border and Elastic Transcoder has not added black bars,
      -- the watermark is cropped. If Elastic Transcoder has added black
      -- bars, the watermark extends into the black bars. If the watermark
      -- extends beyond the black bars, it is cropped. Use the value of
      -- Target to specify whether you want Elastic Transcoder to include
      -- the black bars that are added by Elastic Transcoder, if any, in
      -- the offset calculation.
    , _ppkOpacity :: Maybe Text
      -- ^ A percentage that indicates how much you want a watermark to
      -- obscure the video in the location where it appears. Valid values
      -- are 0 (the watermark is invisible) to 100 (the watermark
      -- completely obscures the video in the specified location). The
      -- datatype of Opacity is float. Elastic Transcoder supports
      -- transparent .png graphics. If you use a transparent .png, the
      -- transparent portion of the video appears as if you had specified
      -- a value of 0 for Opacity. The .jpg file format doesn't support
      -- transparency.
    , _ppkTarget :: Maybe Text
      -- ^ A value that determines how Elastic Transcoder interprets values
      -- that you specified for HorizontalOffset, VerticalOffset,
      -- MaxWidth, and MaxHeight: Content: HorizontalOffset and
      -- VerticalOffset values are calculated based on the borders of the
      -- video excluding black bars added by Elastic Transcoder, if any.
      -- In addition, MaxWidth and MaxHeight, if specified as a
      -- percentage, are calculated based on the borders of the video
      -- excluding black bars added by Elastic Transcoder, if any. Frame:
      -- HorizontalOffset and VerticalOffset values are calculated based
      -- on the borders of the video including black bars added by Elastic
      -- Transcoder, if any. In addition, MaxWidth and MaxHeight, if
      -- specified as a percentage, are calculated based on the borders of
      -- the video including black bars added by Elastic Transcoder, if
      -- any.
    } deriving (Show, Generic)

-- | A unique identifier for the settings for one watermark. The value of Id can
-- be up to 40 characters long.
ppkId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkId f x =
    (\y -> x { _ppkId = y })
       <$> f (_ppkId x)
{-# INLINE ppkId #-}

-- | The maximum width of the watermark in one of the following formats: number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is
-- the value of MaxWidth. integer percentage (%): The range of valid values is
-- 0 to 100. Use the value of Target to specify whether you want Elastic
-- Transcoder to include the black bars that are added by Elastic Transcoder,
-- if any, in the calculation. If you specify the value in pixels, it must be
-- less than or equal to the value of MaxWidth.
ppkMaxWidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkMaxWidth f x =
    (\y -> x { _ppkMaxWidth = y })
       <$> f (_ppkMaxWidth x)
{-# INLINE ppkMaxWidth #-}

-- | The maximum height of the watermark in one of the following formats: number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is
-- the value of MaxHeight. integer percentage (%): The range of valid values
-- is 0 to 100. Use the value of Target to specify whether you want Elastic
-- Transcoder to include the black bars that are added by Elastic Transcoder,
-- if any, in the calculation. If you specify the value in pixels, it must be
-- less than or equal to the value of MaxHeight.
ppkMaxHeight
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkMaxHeight f x =
    (\y -> x { _ppkMaxHeight = y })
       <$> f (_ppkMaxHeight x)
{-# INLINE ppkMaxHeight #-}

-- | A value that controls scaling of the watermark: Fit: Elastic Transcoder
-- scales the watermark so it matches the value that you specified in either
-- MaxWidth or MaxHeight without exceeding the other value. Stretch: Elastic
-- Transcoder stretches the watermark to match the values that you specified
-- for MaxWidth and MaxHeight. If the relative proportions of the watermark
-- and the values of MaxWidth and MaxHeight are different, the watermark will
-- be distorted. ShrinkToFit: Elastic Transcoder scales the watermark down so
-- that its dimensions match the values that you specified for at least one of
-- MaxWidth and MaxHeight without exceeding either value. If you specify this
-- option, Elastic Transcoder does not scale the watermark up.
ppkSizingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkSizingPolicy f x =
    (\y -> x { _ppkSizingPolicy = y })
       <$> f (_ppkSizingPolicy x)
{-# INLINE ppkSizingPolicy #-}

-- | The horizontal position of the watermark unless you specify a non-zero
-- value for HorizontalOffset: Left: The left edge of the watermark is aligned
-- with the left border of the video. Right: The right edge of the watermark
-- is aligned with the right border of the video. Center: The watermark is
-- centered between the left and right borders.
ppkHorizontalAlign
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkHorizontalAlign f x =
    (\y -> x { _ppkHorizontalAlign = y })
       <$> f (_ppkHorizontalAlign x)
{-# INLINE ppkHorizontalAlign #-}

-- | The amount by which you want the horizontal position of the watermark to be
-- offset from the position specified by HorizontalAlign: number of pixels
-- (px): The minimum value is 0 pixels, and the maximum value is the value of
-- MaxWidth. integer percentage (%): The range of valid values is 0 to 100.
-- For example, if you specify Left for HorizontalAlign and 5px for
-- HorizontalOffset, the left side of the watermark appears 5 pixels from the
-- left border of the output video. HorizontalOffset is only valid when the
-- value of HorizontalAlign is Left or Right. If you specify an offset that
-- causes the watermark to extend beyond the left or right border and Elastic
-- Transcoder has not added black bars, the watermark is cropped. If Elastic
-- Transcoder has added black bars, the watermark extends into the black bars.
-- If the watermark extends beyond the black bars, it is cropped. Use the
-- value of Target to specify whether you want to include the black bars that
-- are added by Elastic Transcoder, if any, in the offset calculation.
ppkHorizontalOffset
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkHorizontalOffset f x =
    (\y -> x { _ppkHorizontalOffset = y })
       <$> f (_ppkHorizontalOffset x)
{-# INLINE ppkHorizontalOffset #-}

-- | The vertical position of the watermark unless you specify a non-zero value
-- for VerticalOffset: Top: The top edge of the watermark is aligned with the
-- top border of the video. Bottom: The bottom edge of the watermark is
-- aligned with the bottom border of the video. Center: The watermark is
-- centered between the top and bottom borders.
ppkVerticalAlign
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkVerticalAlign f x =
    (\y -> x { _ppkVerticalAlign = y })
       <$> f (_ppkVerticalAlign x)
{-# INLINE ppkVerticalAlign #-}

-- | VerticalOffset The amount by which you want the vertical position of the
-- watermark to be offset from the position specified by VerticalAlign: number
-- of pixels (px): The minimum value is 0 pixels, and the maximum value is the
-- value of MaxHeight. integer percentage (%): The range of valid values is 0
-- to 100. For example, if you specify Top for VerticalAlign and 5px for
-- VerticalOffset, the top of the watermark appears 5 pixels from the top
-- border of the output video. VerticalOffset is only valid when the value of
-- VerticalAlign is Top or Bottom. If you specify an offset that causes the
-- watermark to extend beyond the top or bottom border and Elastic Transcoder
-- has not added black bars, the watermark is cropped. If Elastic Transcoder
-- has added black bars, the watermark extends into the black bars. If the
-- watermark extends beyond the black bars, it is cropped. Use the value of
-- Target to specify whether you want Elastic Transcoder to include the black
-- bars that are added by Elastic Transcoder, if any, in the offset
-- calculation.
ppkVerticalOffset
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkVerticalOffset f x =
    (\y -> x { _ppkVerticalOffset = y })
       <$> f (_ppkVerticalOffset x)
{-# INLINE ppkVerticalOffset #-}

-- | A percentage that indicates how much you want a watermark to obscure the
-- video in the location where it appears. Valid values are 0 (the watermark
-- is invisible) to 100 (the watermark completely obscures the video in the
-- specified location). The datatype of Opacity is float. Elastic Transcoder
-- supports transparent .png graphics. If you use a transparent .png, the
-- transparent portion of the video appears as if you had specified a value of
-- 0 for Opacity. The .jpg file format doesn't support transparency.
ppkOpacity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkOpacity f x =
    (\y -> x { _ppkOpacity = y })
       <$> f (_ppkOpacity x)
{-# INLINE ppkOpacity #-}

-- | A value that determines how Elastic Transcoder interprets values that you
-- specified for HorizontalOffset, VerticalOffset, MaxWidth, and MaxHeight:
-- Content: HorizontalOffset and VerticalOffset values are calculated based on
-- the borders of the video excluding black bars added by Elastic Transcoder,
-- if any. In addition, MaxWidth and MaxHeight, if specified as a percentage,
-- are calculated based on the borders of the video excluding black bars added
-- by Elastic Transcoder, if any. Frame: HorizontalOffset and VerticalOffset
-- values are calculated based on the borders of the video including black
-- bars added by Elastic Transcoder, if any. In addition, MaxWidth and
-- MaxHeight, if specified as a percentage, are calculated based on the
-- borders of the video including black bars added by Elastic Transcoder, if
-- any.
ppkTarget
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PresetWatermark
    -> f PresetWatermark
ppkTarget f x =
    (\y -> x { _ppkTarget = y })
       <$> f (_ppkTarget x)
{-# INLINE ppkTarget #-}

instance FromJSON PresetWatermark

instance ToJSON PresetWatermark

-- | A section of the request body that specifies the thumbnail parameters, if
-- any.
data Thumbnails = Thumbnails
    { _ttsFormat :: Maybe Text
      -- ^ The format of thumbnails, if any. Valid values are jpg and png.
      -- You specify whether you want Elastic Transcoder to create
      -- thumbnails when you create a job.
    , _ttsInterval :: Maybe Text
      -- ^ The number of seconds between thumbnails. Specify an integer
      -- value.
    , _ttsResolution :: Maybe Text
      -- ^ To better control resolution and aspect ratio of thumbnails, we
      -- recommend that you use the values MaxWidth, MaxHeight,
      -- SizingPolicy, and PaddingPolicy instead of Resolution and
      -- AspectRatio. The two groups of settings are mutually exclusive.
      -- Do not use them together. The width and height of thumbnail files
      -- in pixels. Specify a value in the format width x height where
      -- both values are even integers. The values cannot exceed the width
      -- and height that you specified in the Video:Resolution object.
    , _ttsAspectRatio :: Maybe Text
      -- ^ To better control resolution and aspect ratio of thumbnails, we
      -- recommend that you use the values MaxWidth, MaxHeight,
      -- SizingPolicy, and PaddingPolicy instead of Resolution and
      -- AspectRatio. The two groups of settings are mutually exclusive.
      -- Do not use them together. The aspect ratio of thumbnails. Valid
      -- values include: auto, 1:1, 4:3, 3:2, 16:9 If you specify auto,
      -- Elastic Transcoder tries to preserve the aspect ratio of the
      -- video in the output file.
    , _ttsMaxWidth :: Maybe Text
      -- ^ The maximum width of thumbnails in pixels. If you specify auto,
      -- Elastic Transcoder uses 1920 (Full HD) as the default value. If
      -- you specify a numeric value, enter an even integer between 32 and
      -- 4096.
    , _ttsMaxHeight :: Maybe Text
      -- ^ The maximum height of thumbnails in pixels. If you specify auto,
      -- Elastic Transcoder uses 1080 (Full HD) as the default value. If
      -- you specify a numeric value, enter an even integer between 32 and
      -- 3072.
    , _ttsSizingPolicy :: Maybe Text
      -- ^ Specify one of the following values to control scaling of
      -- thumbnails: Fit: Elastic Transcoder scales thumbnails so they
      -- match the value that you specified in thumbnail MaxWidth or
      -- MaxHeight settings without exceeding the other value. Fill:
      -- Elastic Transcoder scales thumbnails so they match the value that
      -- you specified in thumbnail MaxWidth or MaxHeight settings and
      -- matches or exceeds the other value. Elastic Transcoder centers
      -- the image in thumbnails and then crops in the dimension (if any)
      -- that exceeds the maximum value. Stretch: Elastic Transcoder
      -- stretches thumbnails to match the values that you specified for
      -- thumbnail MaxWidth and MaxHeight settings. If the relative
      -- proportions of the input video and thumbnails are different, the
      -- thumbnails will be distorted. Keep: Elastic Transcoder does not
      -- scale thumbnails. If either dimension of the input video exceeds
      -- the values that you specified for thumbnail MaxWidth and
      -- MaxHeight settings, Elastic Transcoder crops the thumbnails.
      -- ShrinkToFit: Elastic Transcoder scales thumbnails down so that
      -- their dimensions match the values that you specified for at least
      -- one of thumbnail MaxWidth and MaxHeight without exceeding either
      -- value. If you specify this option, Elastic Transcoder does not
      -- scale thumbnails up. ShrinkToFill: Elastic Transcoder scales
      -- thumbnails down so that their dimensions match the values that
      -- you specified for at least one of MaxWidth and MaxHeight without
      -- dropping below either value. If you specify this option, Elastic
      -- Transcoder does not scale thumbnails up.
    , _ttsPaddingPolicy :: Maybe Text
      -- ^ When you set PaddingPolicy to Pad, Elastic Transcoder may add
      -- black bars to the top and bottom and/or left and right sides of
      -- thumbnails to make the total size of the thumbnails match the
      -- values that you specified for thumbnail MaxWidth and MaxHeight
      -- settings.
    } deriving (Show, Generic)

-- | The format of thumbnails, if any. Valid values are jpg and png. You specify
-- whether you want Elastic Transcoder to create thumbnails when you create a
-- job.
ttsFormat
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsFormat f x =
    (\y -> x { _ttsFormat = y })
       <$> f (_ttsFormat x)
{-# INLINE ttsFormat #-}

-- | The number of seconds between thumbnails. Specify an integer value.
ttsInterval
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsInterval f x =
    (\y -> x { _ttsInterval = y })
       <$> f (_ttsInterval x)
{-# INLINE ttsInterval #-}

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values MaxWidth, MaxHeight, SizingPolicy, and
-- PaddingPolicy instead of Resolution and AspectRatio. The two groups of
-- settings are mutually exclusive. Do not use them together. The width and
-- height of thumbnail files in pixels. Specify a value in the format width x
-- height where both values are even integers. The values cannot exceed the
-- width and height that you specified in the Video:Resolution object.
ttsResolution
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsResolution f x =
    (\y -> x { _ttsResolution = y })
       <$> f (_ttsResolution x)
{-# INLINE ttsResolution #-}

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values MaxWidth, MaxHeight, SizingPolicy, and
-- PaddingPolicy instead of Resolution and AspectRatio. The two groups of
-- settings are mutually exclusive. Do not use them together. The aspect ratio
-- of thumbnails. Valid values include: auto, 1:1, 4:3, 3:2, 16:9 If you
-- specify auto, Elastic Transcoder tries to preserve the aspect ratio of the
-- video in the output file.
ttsAspectRatio
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsAspectRatio f x =
    (\y -> x { _ttsAspectRatio = y })
       <$> f (_ttsAspectRatio x)
{-# INLINE ttsAspectRatio #-}

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
ttsMaxWidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsMaxWidth f x =
    (\y -> x { _ttsMaxWidth = y })
       <$> f (_ttsMaxWidth x)
{-# INLINE ttsMaxWidth #-}

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
ttsMaxHeight
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsMaxHeight f x =
    (\y -> x { _ttsMaxHeight = y })
       <$> f (_ttsMaxHeight x)
{-# INLINE ttsMaxHeight #-}

-- | Specify one of the following values to control scaling of thumbnails: Fit:
-- Elastic Transcoder scales thumbnails so they match the value that you
-- specified in thumbnail MaxWidth or MaxHeight settings without exceeding the
-- other value. Fill: Elastic Transcoder scales thumbnails so they match the
-- value that you specified in thumbnail MaxWidth or MaxHeight settings and
-- matches or exceeds the other value. Elastic Transcoder centers the image in
-- thumbnails and then crops in the dimension (if any) that exceeds the
-- maximum value. Stretch: Elastic Transcoder stretches thumbnails to match
-- the values that you specified for thumbnail MaxWidth and MaxHeight
-- settings. If the relative proportions of the input video and thumbnails are
-- different, the thumbnails will be distorted. Keep: Elastic Transcoder does
-- not scale thumbnails. If either dimension of the input video exceeds the
-- values that you specified for thumbnail MaxWidth and MaxHeight settings,
-- Elastic Transcoder crops the thumbnails. ShrinkToFit: Elastic Transcoder
-- scales thumbnails down so that their dimensions match the values that you
-- specified for at least one of thumbnail MaxWidth and MaxHeight without
-- exceeding either value. If you specify this option, Elastic Transcoder does
-- not scale thumbnails up. ShrinkToFill: Elastic Transcoder scales thumbnails
-- down so that their dimensions match the values that you specified for at
-- least one of MaxWidth and MaxHeight without dropping below either value. If
-- you specify this option, Elastic Transcoder does not scale thumbnails up.
ttsSizingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsSizingPolicy f x =
    (\y -> x { _ttsSizingPolicy = y })
       <$> f (_ttsSizingPolicy x)
{-# INLINE ttsSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of thumbnails to make the
-- total size of the thumbnails match the values that you specified for
-- thumbnail MaxWidth and MaxHeight settings.
ttsPaddingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Thumbnails
    -> f Thumbnails
ttsPaddingPolicy f x =
    (\y -> x { _ttsPaddingPolicy = y })
       <$> f (_ttsPaddingPolicy x)
{-# INLINE ttsPaddingPolicy #-}

instance FromJSON Thumbnails

instance ToJSON Thumbnails

-- | Settings that determine when a clip begins and how long it lasts.
data TimeSpan = TimeSpan
    { _tsStartTime :: Maybe Text
      -- ^ The place in the input file where you want a clip to start. The
      -- format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999;
      -- SSS is thousandths of a second) or sssss.SSS (maximum value:
      -- 86399.999). If you don't specify a value, Elastic Transcoder
      -- starts at the beginning of the input file.
    , _tsDuration :: Maybe Text
      -- ^ The duration of the clip. The format can be either HH:mm:ss.SSS
      -- (maximum value: 23:59:59.999; SSS is thousandths of a second) or
      -- sssss.SSS (maximum value: 86399.999). If you don't specify a
      -- value, Elastic Transcoder creates an output file from StartTime
      -- to the end of the file. If you specify a value longer than the
      -- duration of the input file, Elastic Transcoder transcodes the
      -- file and returns a warning message.
    } deriving (Show, Generic)

-- | The place in the input file where you want a clip to start. The format can
-- be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of
-- a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a
-- value, Elastic Transcoder starts at the beginning of the input file.
tsStartTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TimeSpan
    -> f TimeSpan
tsStartTime f x =
    (\y -> x { _tsStartTime = y })
       <$> f (_tsStartTime x)
{-# INLINE tsStartTime #-}

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
-- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum
-- value: 86399.999). If you don't specify a value, Elastic Transcoder creates
-- an output file from StartTime to the end of the file. If you specify a
-- value longer than the duration of the input file, Elastic Transcoder
-- transcodes the file and returns a warning message.
tsDuration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TimeSpan
    -> f TimeSpan
tsDuration f x =
    (\y -> x { _tsDuration = y })
       <$> f (_tsDuration x)
{-# INLINE tsDuration #-}

instance FromJSON TimeSpan

instance ToJSON TimeSpan

-- | A section of the request body that specifies the video parameters.
data VideoParameters = VideoParameters
    { _vpCodec :: Maybe Text
      -- ^ The video codec for the output file. Valid values include H.264
      -- and vp8. You can only specify vp8 when the container type is
      -- webm.
    , _vpCodecOptions :: Map Text Text
      -- ^ Profile The H.264 profile that you want to use for the output
      -- file. Elastic Transcoder supports the following profiles:
      -- baseline: The profile most commonly used for videoconferencing
      -- and for mobile applications. main: The profile used for
      -- standard-definition digital TV broadcasts. high: The profile used
      -- for high-definition digital TV broadcasts and for Blu-ray discs.
      -- Level (H.264 Only) The H.264 level that you want to use for the
      -- output file. Elastic Transcoder supports the following levels: 1,
      -- 1b, 1.1, 1.2, 1.3, 2, 2.1, 2.2, 3, 3.1, 3.2, 4, 4.1
      -- MaxReferenceFrames (H.264 Only) Applicable only when the value of
      -- Video:Codec is H.264. The maximum number of previously decoded
      -- frames to use as a reference for decoding future frames. Valid
      -- values are integers 0 through 16, but we recommend that you not
      -- use a value greater than the following: Min(Floor(Maximum decoded
      -- picture buffer in macroblocks * 256 / (Width in pixels * Height
      -- in pixels)), 16) where Width in pixels and Height in pixels
      -- represent either MaxWidth and MaxHeight, or Resolution. Maximum
      -- decoded picture buffer in macroblocks depends on the value of the
      -- Level object. See the list below. (A macroblock is a block of
      -- pixels measuring 16x16.) 1 - 396 1b - 396 1.1 - 900 1.2 - 2376
      -- 1.3 - 2376 2 - 2376 2.1 - 4752 2.2 - 8100 3 - 8100 3.1 - 18000
      -- 3.2 - 20480 4 - 32768 4.1 - 32768 MaxBitRate The maximum number
      -- of bits per second in a video buffer; the size of the buffer is
      -- specified by BufferSize. Specify a value between 16 and 62,500.
      -- You can reduce the bandwidth required to stream a video by
      -- reducing the maximum bit rate, but this also reduces the quality
      -- of the video. BufferSize The maximum number of bits in any x
      -- seconds of the output video. This window is commonly 10 seconds,
      -- the standard segment duration when you're using MPEG-TS for the
      -- container type of the output video. Specify an integer greater
      -- than 0. If you specify MaxBitRate and omit BufferSize, Elastic
      -- Transcoder sets BufferSize to 10 times the value of MaxBitRate.
    , _vpKeyframesMaxDist :: Maybe Text
      -- ^ The maximum number of frames between key frames. Key frames are
      -- fully encoded frames; the frames between key frames are encoded
      -- based, in part, on the content of the key frames. The value is an
      -- integer formatted as a string; valid values are between 1 (every
      -- frame is a key frame) and 100000, inclusive. A higher value
      -- results in higher compression but may also discernibly decrease
      -- video quality.
    , _vpFixedGOP :: Maybe Text
      -- ^ Whether to use a fixed value for FixedGOP. Valid values are true
      -- and false: true: Elastic Transcoder uses the value of
      -- KeyframesMaxDist for the distance between key frames (the number
      -- of frames in a group of pictures, or GOP). false: The distance
      -- between key frames can vary.
    , _vpBitRate :: Maybe Text
      -- ^ The bit rate of the video stream in the output file, in
      -- kilobits/second. Valid values depend on the values of Level and
      -- Profile. If you specify auto, Elastic Transcoder uses the
      -- detected bit rate of the input source. If you specify a value
      -- other than auto, we recommend that you specify a value less than
      -- or equal to the maximum H.264-compliant value listed for your
      -- level and profile: Level - Maximum video bit rate in
      -- kilobits/second (baseline and main Profile) : maximum video bit
      -- rate in kilobits/second (high Profile) 1 - 64 : 80 1b - 128 : 160
      -- 1.1 - 192 : 240 1.2 - 384 : 480 1.3 - 768 : 960 2 - 2000 : 2500 3
      -- - 10000 : 12500 3.1 - 14000 : 17500 3.2 - 20000 : 25000 4 - 20000
      -- : 25000 4.1 - 50000 : 62500.
    , _vpFrameRate :: Maybe Text
      -- ^ The frames per second for the video stream in the output file.
      -- Valid values include: auto, 10, 15, 23.97, 24, 25, 29.97, 30, 60
      -- If you specify auto, Elastic Transcoder uses the detected frame
      -- rate of the input source. If you specify a frame rate, we
      -- recommend that you perform the following calculation: Frame rate
      -- = maximum recommended decoding speed in luma samples/second /
      -- (width in pixels * height in pixels) where: width in pixels and
      -- height in pixels represent the Resolution of the output video.
      -- maximum recommended decoding speed in Luma samples/second is less
      -- than or equal to the maximum value listed in the following table,
      -- based on the value that you specified for Level. The maximum
      -- recommended decoding speed in Luma samples/second for each level
      -- is described in the following list (Level - Decoding speed): 1 -
      -- 380160 1b - 380160 1.1 - 76800 1.2 - 1536000 1.3 - 3041280 2 -
      -- 3041280 2.1 - 5068800 2.2 - 5184000 3 - 10368000 3.1 - 27648000
      -- 3.2 - 55296000 4 - 62914560 4.1 - 62914560.
    , _vpMaxFrameRate :: Maybe Text
      -- ^ If you specify auto for FrameRate, Elastic Transcoder uses the
      -- frame rate of the input video for the frame rate of the output
      -- video. Specify the maximum frame rate that you want Elastic
      -- Transcoder to use when the frame rate of the input video is
      -- greater than the desired maximum frame rate of the output video.
      -- Valid values include: 10, 15, 23.97, 24, 25, 29.97, 30, 60.
    , _vpResolution :: Maybe Text
      -- ^ To better control resolution and aspect ratio of output videos,
      -- we recommend that you use the values MaxWidth, MaxHeight,
      -- SizingPolicy, PaddingPolicy, and DisplayAspectRatio instead of
      -- Resolution and AspectRatio. The two groups of settings are
      -- mutually exclusive. Do not use them together. The width and
      -- height of the video in the output file, in pixels. Valid values
      -- are auto and width x height: auto: Elastic Transcoder attempts to
      -- preserve the width and height of the input file, subject to the
      -- following rules. width x height: The width and height of the
      -- output video in pixels. Note the following about specifying the
      -- width and height: The width must be an even integer between 128
      -- and 4096, inclusive. The height must be an even integer between
      -- 96 and 3072, inclusive. If you specify a resolution that is less
      -- than the resolution of the input file, Elastic Transcoder
      -- rescales the output file to the lower resolution. If you specify
      -- a resolution that is greater than the resolution of the input
      -- file, Elastic Transcoder rescales the output to the higher
      -- resolution. We recommend that you specify a resolution for which
      -- the product of width and height is less than or equal to the
      -- applicable value in the following list (List - Max width x height
      -- value): 1 - 25344 1b - 25344 1.1 - 101376 1.2 - 101376 1.3 -
      -- 101376 2 - 101376 2.1 - 202752 2.2 - 404720 3 - 404720 3.1 -
      -- 921600 3.2 - 1310720 4 - 2097152 4.1 - 2097152.
    , _vpAspectRatio :: Maybe Text
      -- ^ To better control resolution and aspect ratio of output videos,
      -- we recommend that you use the values MaxWidth, MaxHeight,
      -- SizingPolicy, PaddingPolicy, and DisplayAspectRatio instead of
      -- Resolution and AspectRatio. The two groups of settings are
      -- mutually exclusive. Do not use them together. The display aspect
      -- ratio of the video in the output file. Valid values include:
      -- auto, 1:1, 4:3, 3:2, 16:9 If you specify auto, Elastic Transcoder
      -- tries to preserve the aspect ratio of the input file. If you
      -- specify an aspect ratio for the output file that differs from
      -- aspect ratio of the input file, Elastic Transcoder adds
      -- pillarboxing (black bars on the sides) or letterboxing (black
      -- bars on the top and bottom) to maintain the aspect ratio of the
      -- active region of the video.
    , _vpMaxWidth :: Maybe Text
      -- ^ The maximum width of the output video in pixels. If you specify
      -- auto, Elastic Transcoder uses 1920 (Full HD) as the default
      -- value. If you specify a numeric value, enter an even integer
      -- between 128 and 4096.
    , _vpMaxHeight :: Maybe Text
      -- ^ The maximum height of the output video in pixels. If you specify
      -- auto, Elastic Transcoder uses 1080 (Full HD) as the default
      -- value. If you specify a numeric value, enter an even integer
      -- between 96 and 3072.
    , _vpDisplayAspectRatio :: Maybe Text
      -- ^ The value that Elastic Transcoder adds to the metadata in the
      -- output file.
    , _vpSizingPolicy :: Maybe Text
      -- ^ Specify one of the following values to control scaling of the
      -- output video: Fit: Elastic Transcoder scales the output video so
      -- it matches the value that you specified in either MaxWidth or
      -- MaxHeight without exceeding the other value. Fill: Elastic
      -- Transcoder scales the output video so it matches the value that
      -- you specified in either MaxWidth or MaxHeight and matches or
      -- exceeds the other value. Elastic Transcoder centers the output
      -- video and then crops it in the dimension (if any) that exceeds
      -- the maximum value. Stretch: Elastic Transcoder stretches the
      -- output video to match the values that you specified for MaxWidth
      -- and MaxHeight. If the relative proportions of the input video and
      -- the output video are different, the output video will be
      -- distorted. Keep: Elastic Transcoder does not scale the output
      -- video. If either dimension of the input video exceeds the values
      -- that you specified for MaxWidth and MaxHeight, Elastic Transcoder
      -- crops the output video. ShrinkToFit: Elastic Transcoder scales
      -- the output video down so that its dimensions match the values
      -- that you specified for at least one of MaxWidth and MaxHeight
      -- without exceeding either value. If you specify this option,
      -- Elastic Transcoder does not scale the video up. ShrinkToFill:
      -- Elastic Transcoder scales the output video down so that its
      -- dimensions match the values that you specified for at least one
      -- of MaxWidth and MaxHeight without dropping below either value. If
      -- you specify this option, Elastic Transcoder does not scale the
      -- video up.
    , _vpPaddingPolicy :: Maybe Text
      -- ^ When you set PaddingPolicy to Pad, Elastic Transcoder may add
      -- black bars to the top and bottom and/or left and right sides of
      -- the output video to make the total size of the output video match
      -- the values that you specified for MaxWidth and MaxHeight.
    , _vpWatermarks :: [PresetWatermark]
      -- ^ Settings for the size, location, and opacity of graphics that you
      -- want Elastic Transcoder to overlay over videos that are
      -- transcoded using this preset. You can specify settings for up to
      -- four watermarks. Watermarks appear in the specified size and
      -- location, and with the specified opacity for the duration of the
      -- transcoded video. Watermarks can be in .png or .jpg format. If
      -- you want to display a watermark that is not rectangular, use the
      -- .png format, which supports transparency. When you create a job
      -- that uses this preset, you specify the .png or .jpg graphics that
      -- you want Elastic Transcoder to include in the transcoded videos.
      -- You can specify fewer graphics in the job than you specify
      -- watermark settings in the preset, which allows you to use the
      -- same preset for up to four watermarks that have different
      -- dimensions.
    } deriving (Show, Generic)

-- | The video codec for the output file. Valid values include H.264 and vp8.
-- You can only specify vp8 when the container type is webm.
vpCodec
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpCodec f x =
    (\y -> x { _vpCodec = y })
       <$> f (_vpCodec x)
{-# INLINE vpCodec #-}

-- | Profile The H.264 profile that you want to use for the output file. Elastic
-- Transcoder supports the following profiles: baseline: The profile most
-- commonly used for videoconferencing and for mobile applications. main: The
-- profile used for standard-definition digital TV broadcasts. high: The
-- profile used for high-definition digital TV broadcasts and for Blu-ray
-- discs. Level (H.264 Only) The H.264 level that you want to use for the
-- output file. Elastic Transcoder supports the following levels: 1, 1b, 1.1,
-- 1.2, 1.3, 2, 2.1, 2.2, 3, 3.1, 3.2, 4, 4.1 MaxReferenceFrames (H.264 Only)
-- Applicable only when the value of Video:Codec is H.264. The maximum number
-- of previously decoded frames to use as a reference for decoding future
-- frames. Valid values are integers 0 through 16, but we recommend that you
-- not use a value greater than the following: Min(Floor(Maximum decoded
-- picture buffer in macroblocks * 256 / (Width in pixels * Height in
-- pixels)), 16) where Width in pixels and Height in pixels represent either
-- MaxWidth and MaxHeight, or Resolution. Maximum decoded picture buffer in
-- macroblocks depends on the value of the Level object. See the list below.
-- (A macroblock is a block of pixels measuring 16x16.) 1 - 396 1b - 396 1.1 -
-- 900 1.2 - 2376 1.3 - 2376 2 - 2376 2.1 - 4752 2.2 - 8100 3 - 8100 3.1 -
-- 18000 3.2 - 20480 4 - 32768 4.1 - 32768 MaxBitRate The maximum number of
-- bits per second in a video buffer; the size of the buffer is specified by
-- BufferSize. Specify a value between 16 and 62,500. You can reduce the
-- bandwidth required to stream a video by reducing the maximum bit rate, but
-- this also reduces the quality of the video. BufferSize The maximum number
-- of bits in any x seconds of the output video. This window is commonly 10
-- seconds, the standard segment duration when you're using MPEG-TS for the
-- container type of the output video. Specify an integer greater than 0. If
-- you specify MaxBitRate and omit BufferSize, Elastic Transcoder sets
-- BufferSize to 10 times the value of MaxBitRate.
vpCodecOptions
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> VideoParameters
    -> f VideoParameters
vpCodecOptions f x =
    (\y -> x { _vpCodecOptions = y })
       <$> f (_vpCodecOptions x)
{-# INLINE vpCodecOptions #-}

-- | The maximum number of frames between key frames. Key frames are fully
-- encoded frames; the frames between key frames are encoded based, in part,
-- on the content of the key frames. The value is an integer formatted as a
-- string; valid values are between 1 (every frame is a key frame) and 100000,
-- inclusive. A higher value results in higher compression but may also
-- discernibly decrease video quality.
vpKeyframesMaxDist
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpKeyframesMaxDist f x =
    (\y -> x { _vpKeyframesMaxDist = y })
       <$> f (_vpKeyframesMaxDist x)
{-# INLINE vpKeyframesMaxDist #-}

-- | Whether to use a fixed value for FixedGOP. Valid values are true and false:
-- true: Elastic Transcoder uses the value of KeyframesMaxDist for the
-- distance between key frames (the number of frames in a group of pictures,
-- or GOP). false: The distance between key frames can vary.
vpFixedGOP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpFixedGOP f x =
    (\y -> x { _vpFixedGOP = y })
       <$> f (_vpFixedGOP x)
{-# INLINE vpFixedGOP #-}

-- | The bit rate of the video stream in the output file, in kilobits/second.
-- Valid values depend on the values of Level and Profile. If you specify
-- auto, Elastic Transcoder uses the detected bit rate of the input source. If
-- you specify a value other than auto, we recommend that you specify a value
-- less than or equal to the maximum H.264-compliant value listed for your
-- level and profile: Level - Maximum video bit rate in kilobits/second
-- (baseline and main Profile) : maximum video bit rate in kilobits/second
-- (high Profile) 1 - 64 : 80 1b - 128 : 160 1.1 - 192 : 240 1.2 - 384 : 480
-- 1.3 - 768 : 960 2 - 2000 : 2500 3 - 10000 : 12500 3.1 - 14000 : 17500 3.2 -
-- 20000 : 25000 4 - 20000 : 25000 4.1 - 50000 : 62500.
vpBitRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpBitRate f x =
    (\y -> x { _vpBitRate = y })
       <$> f (_vpBitRate x)
{-# INLINE vpBitRate #-}

-- | The frames per second for the video stream in the output file. Valid values
-- include: auto, 10, 15, 23.97, 24, 25, 29.97, 30, 60 If you specify auto,
-- Elastic Transcoder uses the detected frame rate of the input source. If you
-- specify a frame rate, we recommend that you perform the following
-- calculation: Frame rate = maximum recommended decoding speed in luma
-- samples/second / (width in pixels * height in pixels) where: width in
-- pixels and height in pixels represent the Resolution of the output video.
-- maximum recommended decoding speed in Luma samples/second is less than or
-- equal to the maximum value listed in the following table, based on the
-- value that you specified for Level. The maximum recommended decoding speed
-- in Luma samples/second for each level is described in the following list
-- (Level - Decoding speed): 1 - 380160 1b - 380160 1.1 - 76800 1.2 - 1536000
-- 1.3 - 3041280 2 - 3041280 2.1 - 5068800 2.2 - 5184000 3 - 10368000 3.1 -
-- 27648000 3.2 - 55296000 4 - 62914560 4.1 - 62914560.
vpFrameRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpFrameRate f x =
    (\y -> x { _vpFrameRate = y })
       <$> f (_vpFrameRate x)
{-# INLINE vpFrameRate #-}

-- | If you specify auto for FrameRate, Elastic Transcoder uses the frame rate
-- of the input video for the frame rate of the output video. Specify the
-- maximum frame rate that you want Elastic Transcoder to use when the frame
-- rate of the input video is greater than the desired maximum frame rate of
-- the output video. Valid values include: 10, 15, 23.97, 24, 25, 29.97, 30,
-- 60.
vpMaxFrameRate
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpMaxFrameRate f x =
    (\y -> x { _vpMaxFrameRate = y })
       <$> f (_vpMaxFrameRate x)
{-# INLINE vpMaxFrameRate #-}

-- | To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values MaxWidth, MaxHeight, SizingPolicy,
-- PaddingPolicy, and DisplayAspectRatio instead of Resolution and
-- AspectRatio. The two groups of settings are mutually exclusive. Do not use
-- them together. The width and height of the video in the output file, in
-- pixels. Valid values are auto and width x height: auto: Elastic Transcoder
-- attempts to preserve the width and height of the input file, subject to the
-- following rules. width x height: The width and height of the output video
-- in pixels. Note the following about specifying the width and height: The
-- width must be an even integer between 128 and 4096, inclusive. The height
-- must be an even integer between 96 and 3072, inclusive. If you specify a
-- resolution that is less than the resolution of the input file, Elastic
-- Transcoder rescales the output file to the lower resolution. If you specify
-- a resolution that is greater than the resolution of the input file, Elastic
-- Transcoder rescales the output to the higher resolution. We recommend that
-- you specify a resolution for which the product of width and height is less
-- than or equal to the applicable value in the following list (List - Max
-- width x height value): 1 - 25344 1b - 25344 1.1 - 101376 1.2 - 101376 1.3 -
-- 101376 2 - 101376 2.1 - 202752 2.2 - 404720 3 - 404720 3.1 - 921600 3.2 -
-- 1310720 4 - 2097152 4.1 - 2097152.
vpResolution
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpResolution f x =
    (\y -> x { _vpResolution = y })
       <$> f (_vpResolution x)
{-# INLINE vpResolution #-}

-- | To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values MaxWidth, MaxHeight, SizingPolicy,
-- PaddingPolicy, and DisplayAspectRatio instead of Resolution and
-- AspectRatio. The two groups of settings are mutually exclusive. Do not use
-- them together. The display aspect ratio of the video in the output file.
-- Valid values include: auto, 1:1, 4:3, 3:2, 16:9 If you specify auto,
-- Elastic Transcoder tries to preserve the aspect ratio of the input file. If
-- you specify an aspect ratio for the output file that differs from aspect
-- ratio of the input file, Elastic Transcoder adds pillarboxing (black bars
-- on the sides) or letterboxing (black bars on the top and bottom) to
-- maintain the aspect ratio of the active region of the video.
vpAspectRatio
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpAspectRatio f x =
    (\y -> x { _vpAspectRatio = y })
       <$> f (_vpAspectRatio x)
{-# INLINE vpAspectRatio #-}

-- | The maximum width of the output video in pixels. If you specify auto,
-- Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify
-- a numeric value, enter an even integer between 128 and 4096.
vpMaxWidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpMaxWidth f x =
    (\y -> x { _vpMaxWidth = y })
       <$> f (_vpMaxWidth x)
{-# INLINE vpMaxWidth #-}

-- | The maximum height of the output video in pixels. If you specify auto,
-- Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify
-- a numeric value, enter an even integer between 96 and 3072.
vpMaxHeight
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpMaxHeight f x =
    (\y -> x { _vpMaxHeight = y })
       <$> f (_vpMaxHeight x)
{-# INLINE vpMaxHeight #-}

-- | The value that Elastic Transcoder adds to the metadata in the output file.
vpDisplayAspectRatio
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpDisplayAspectRatio f x =
    (\y -> x { _vpDisplayAspectRatio = y })
       <$> f (_vpDisplayAspectRatio x)
{-# INLINE vpDisplayAspectRatio #-}

-- | Specify one of the following values to control scaling of the output video:
-- Fit: Elastic Transcoder scales the output video so it matches the value
-- that you specified in either MaxWidth or MaxHeight without exceeding the
-- other value. Fill: Elastic Transcoder scales the output video so it matches
-- the value that you specified in either MaxWidth or MaxHeight and matches or
-- exceeds the other value. Elastic Transcoder centers the output video and
-- then crops it in the dimension (if any) that exceeds the maximum value.
-- Stretch: Elastic Transcoder stretches the output video to match the values
-- that you specified for MaxWidth and MaxHeight. If the relative proportions
-- of the input video and the output video are different, the output video
-- will be distorted. Keep: Elastic Transcoder does not scale the output
-- video. If either dimension of the input video exceeds the values that you
-- specified for MaxWidth and MaxHeight, Elastic Transcoder crops the output
-- video. ShrinkToFit: Elastic Transcoder scales the output video down so that
-- its dimensions match the values that you specified for at least one of
-- MaxWidth and MaxHeight without exceeding either value. If you specify this
-- option, Elastic Transcoder does not scale the video up. ShrinkToFill:
-- Elastic Transcoder scales the output video down so that its dimensions
-- match the values that you specified for at least one of MaxWidth and
-- MaxHeight without dropping below either value. If you specify this option,
-- Elastic Transcoder does not scale the video up.
vpSizingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpSizingPolicy f x =
    (\y -> x { _vpSizingPolicy = y })
       <$> f (_vpSizingPolicy x)
{-# INLINE vpSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of the output video to make
-- the total size of the output video match the values that you specified for
-- MaxWidth and MaxHeight.
vpPaddingPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VideoParameters
    -> f VideoParameters
vpPaddingPolicy f x =
    (\y -> x { _vpPaddingPolicy = y })
       <$> f (_vpPaddingPolicy x)
{-# INLINE vpPaddingPolicy #-}

-- | Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks
-- appear in the specified size and location, and with the specified opacity
-- for the duration of the transcoded video. Watermarks can be in .png or .jpg
-- format. If you want to display a watermark that is not rectangular, use the
-- .png format, which supports transparency. When you create a job that uses
-- this preset, you specify the .png or .jpg graphics that you want Elastic
-- Transcoder to include in the transcoded videos. You can specify fewer
-- graphics in the job than you specify watermark settings in the preset,
-- which allows you to use the same preset for up to four watermarks that have
-- different dimensions.
vpWatermarks
    :: Functor f
    => ([PresetWatermark]
    -> f ([PresetWatermark]))
    -> VideoParameters
    -> f VideoParameters
vpWatermarks f x =
    (\y -> x { _vpWatermarks = y })
       <$> f (_vpWatermarks x)
{-# INLINE vpWatermarks #-}

instance FromJSON VideoParameters

instance ToJSON VideoParameters
