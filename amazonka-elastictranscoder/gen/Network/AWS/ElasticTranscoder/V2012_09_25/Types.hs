{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AudioCodecOptions
    , mkAudioCodecOptions
    , acoProfile

    -- * Clip
    , Clip
    , mkClip
    , dTimeSpan

    -- * Artwork
    , Artwork
    , mkArtwork
    , bInputKey
    , bMaxWidth
    , bMaxHeight
    , bSizingPolicy
    , bPaddingPolicy
    , bAlbumArtFormat

    -- * AudioParameters
    , AudioParameters
    , mkAudioParameters
    , apCodec
    , apSampleRate
    , apBitRate
    , apChannels
    , apCodecOptions

    -- * CaptionFormat
    , CaptionFormat
    , mkCaptionFormat
    , cgFormat
    , cgPattern

    -- * CaptionSource
    , CaptionSource
    , mkCaptionSource
    , ctKey
    , ctLanguage
    , ctTimeOffset
    , ctLabel

    -- * Captions
    , Captions
    , mkCaptions
    , eMergePolicy
    , eCaptionSources
    , eCaptionFormats

    -- * CreateJobOutput
    , CreateJobOutput
    , mkCreateJobOutput
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
    , CreateJobPlaylist
    , mkCreateJobPlaylist
    , cjvName
    , cjvFormat
    , cjvOutputKeys

    -- * Job
    , Job
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
    , JobAlbumArt
    , mkJobAlbumArt
    , jaaMergePolicy
    , jaaArtwork

    -- * JobInput
    , JobInput
    , mkJobInput
    , jiKey
    , jiFrameRate
    , jiResolution
    , jiAspectRatio
    , jiInterlaced
    , jiContainer

    -- * JobOutput
    , JobOutput
    , mkJobOutput
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
    , JobWatermark
    , mkJobWatermark
    , jxPresetWatermarkId
    , jxInputKey

    -- * Notifications
    , Notifications
    , mkNotifications
    , nsProgressing
    , nsCompleted
    , nsWarning
    , nsError

    -- * Permission
    , Permission
    , mkPermission
    , pnGranteeType
    , pnGrantee
    , pnAccess

    -- * Pipeline
    , Pipeline
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
    , PipelineOutputConfig
    , mkPipelineOutputConfig
    , pocBucket
    , pocStorageClass
    , pocPermissions

    -- * Playlist
    , Playlist
    , mkPlaylist
    , puName
    , puFormat
    , puOutputKeys
    , puStatus
    , puStatusDetail

    -- * Preset
    , Preset
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
    , PresetWatermark
    , mkPresetWatermark
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
    , Thumbnails
    , mkThumbnails
    , ttsFormat
    , ttsInterval
    , ttsResolution
    , ttsAspectRatio
    , ttsMaxWidth
    , ttsMaxHeight
    , ttsSizingPolicy
    , ttsPaddingPolicy

    -- * TimeSpan
    , TimeSpan
    , mkTimeSpan
    , tsStartTime
    , tsDuration

    -- * VideoParameters
    , VideoParameters
    , mkVideoParameters
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
acoProfile :: Lens' AudioCodecOptions (Maybe Text)
acoProfile = lens _acoProfile (\s a -> s { _acoProfile = a })
{-# INLINE acoProfile #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AudioCodecOptions' data type to populate a request.
mkAudioCodecOptions :: AudioCodecOptions
mkAudioCodecOptions = AudioCodecOptions
    { _acoProfile = Nothing
    }
{-# INLINE mkAudioCodecOptions #-}

instance FromJSON AudioCodecOptions

instance ToJSON AudioCodecOptions

-- | Settings for one clip in a composition. All jobs in a playlist must have
-- the same clip settings.
newtype Clip = Clip
    { _dTimeSpan :: Maybe TimeSpan
      -- ^ Settings that determine when a clip begins and how long it lasts.
    } deriving (Show, Generic)

-- | Settings that determine when a clip begins and how long it lasts.
dTimeSpan :: Lens' Clip (Maybe TimeSpan)
dTimeSpan = lens _dTimeSpan (\s a -> s { _dTimeSpan = a })
{-# INLINE dTimeSpan #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Clip' data type to populate a request.
mkClip :: Clip
mkClip = Clip
    { _dTimeSpan = Nothing
    }
{-# INLINE mkClip #-}

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
bInputKey :: Lens' Artwork (Maybe Text)
bInputKey = lens _bInputKey (\s a -> s { _bInputKey = a })
{-# INLINE bInputKey #-}

-- | The maximum width of the output album art in pixels. If you specify auto,
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 4096, inclusive.
bMaxWidth :: Lens' Artwork (Maybe Text)
bMaxWidth = lens _bMaxWidth (\s a -> s { _bMaxWidth = a })
{-# INLINE bMaxWidth #-}

-- | The maximum height of the output album art in pixels. If you specify auto,
-- Elastic Transcoder uses 600 as the default value. If you specify a numeric
-- value, enter an even integer between 32 and 3072, inclusive.
bMaxHeight :: Lens' Artwork (Maybe Text)
bMaxHeight = lens _bMaxHeight (\s a -> s { _bMaxHeight = a })
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
bSizingPolicy :: Lens' Artwork (Maybe Text)
bSizingPolicy = lens _bSizingPolicy (\s a -> s { _bSizingPolicy = a })
{-# INLINE bSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add white bars to
-- the top and bottom and/or left and right sides of the output album art to
-- make the total size of the output art match the values that you specified
-- for MaxWidth and MaxHeight.
bPaddingPolicy :: Lens' Artwork (Maybe Text)
bPaddingPolicy = lens _bPaddingPolicy (\s a -> s { _bPaddingPolicy = a })
{-# INLINE bPaddingPolicy #-}

-- | The format of album art, if any. Valid formats are .jpg and .png.
bAlbumArtFormat :: Lens' Artwork (Maybe Text)
bAlbumArtFormat = lens _bAlbumArtFormat (\s a -> s { _bAlbumArtFormat = a })
{-# INLINE bAlbumArtFormat #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Artwork' data type to populate a request.
mkArtwork :: Artwork
mkArtwork = Artwork
    { _bInputKey = Nothing
    , _bMaxWidth = Nothing
    , _bMaxHeight = Nothing
    , _bSizingPolicy = Nothing
    , _bPaddingPolicy = Nothing
    , _bAlbumArtFormat = Nothing
    }
{-# INLINE mkArtwork #-}

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
apCodec :: Lens' AudioParameters (Maybe Text)
apCodec = lens _apCodec (\s a -> s { _apCodec = a })
{-# INLINE apCodec #-}

-- | The sample rate of the audio stream in the output file, in Hertz. Valid
-- values include: auto, 22050, 32000, 44100, 48000, 96000 If you specify
-- auto, Elastic Transcoder automatically detects the sample rate.
apSampleRate :: Lens' AudioParameters (Maybe Text)
apSampleRate = lens _apSampleRate (\s a -> s { _apSampleRate = a })
{-# INLINE apSampleRate #-}

-- | The bit rate of the audio stream in the output file, in kilobits/second.
-- Enter an integer between 64 and 320, inclusive.
apBitRate :: Lens' AudioParameters (Maybe Text)
apBitRate = lens _apBitRate (\s a -> s { _apBitRate = a })
{-# INLINE apBitRate #-}

-- | The number of audio channels in the output file. Valid values include:
-- auto, 0, 1, 2 If you specify auto, Elastic Transcoder automatically detects
-- the number of channels in the input file.
apChannels :: Lens' AudioParameters (Maybe Text)
apChannels = lens _apChannels (\s a -> s { _apChannels = a })
{-# INLINE apChannels #-}

-- | If you specified AAC for Audio:Codec, this is the AAC compression profile
-- to use. Valid values include: auto, AAC-LC, HE-AAC, HE-AACv2 If you specify
-- auto, Elastic Transcoder chooses a profile based on the bit rate of the
-- output file.
apCodecOptions :: Lens' AudioParameters (Maybe AudioCodecOptions)
apCodecOptions = lens _apCodecOptions (\s a -> s { _apCodecOptions = a })
{-# INLINE apCodecOptions #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AudioParameters' data type to populate a request.
mkAudioParameters :: AudioParameters
mkAudioParameters = AudioParameters
    { _apCodec = Nothing
    , _apSampleRate = Nothing
    , _apBitRate = Nothing
    , _apChannels = Nothing
    , _apCodecOptions = Nothing
    }
{-# INLINE mkAudioParameters #-}

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
cgFormat :: Lens' CaptionFormat (Maybe Text)
cgFormat = lens _cgFormat (\s a -> s { _cgFormat = a })
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
cgPattern :: Lens' CaptionFormat (Maybe Text)
cgPattern = lens _cgPattern (\s a -> s { _cgPattern = a })
{-# INLINE cgPattern #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CaptionFormat' data type to populate a request.
mkCaptionFormat :: CaptionFormat
mkCaptionFormat = CaptionFormat
    { _cgFormat = Nothing
    , _cgPattern = Nothing
    }
{-# INLINE mkCaptionFormat #-}

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
ctKey :: Lens' CaptionSource (Maybe Text)
ctKey = lens _ctKey (\s a -> s { _ctKey = a })
{-# INLINE ctKey #-}

-- | A string that specifies the language of the caption. Specify this as one
-- of: 2-character ISO 639-1 code 3-character ISO 639-2 code For more
-- information on ISO language codes and language names, see the List of ISO
-- 639-1 codes.
ctLanguage :: Lens' CaptionSource (Maybe Text)
ctLanguage = lens _ctLanguage (\s a -> s { _ctLanguage = a })
{-# INLINE ctLanguage #-}

-- | For clip generation or captions that do not start at the same time as the
-- associated video file, the TimeOffset tells Elastic Transcoder how much of
-- the video to encode before including captions. Specify the TimeOffset in
-- the form [+-]SS.sss or [+-]HH:mm:SS.ss.
ctTimeOffset :: Lens' CaptionSource (Maybe Text)
ctTimeOffset = lens _ctTimeOffset (\s a -> s { _ctTimeOffset = a })
{-# INLINE ctTimeOffset #-}

-- | The label of the caption shown in the player when choosing a language. We
-- recommend that you put the caption language name here, in the language of
-- the captions.
ctLabel :: Lens' CaptionSource (Maybe Text)
ctLabel = lens _ctLabel (\s a -> s { _ctLabel = a })
{-# INLINE ctLabel #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CaptionSource' data type to populate a request.
mkCaptionSource :: CaptionSource
mkCaptionSource = CaptionSource
    { _ctKey = Nothing
    , _ctLanguage = Nothing
    , _ctTimeOffset = Nothing
    , _ctLabel = Nothing
    }
{-# INLINE mkCaptionSource #-}

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
eMergePolicy :: Lens' Captions (Maybe Text)
eMergePolicy = lens _eMergePolicy (\s a -> s { _eMergePolicy = a })
{-# INLINE eMergePolicy #-}

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave CaptionSources blank.
eCaptionSources :: Lens' Captions ([CaptionSource])
eCaptionSources = lens _eCaptionSources (\s a -> s { _eCaptionSources = a })
{-# INLINE eCaptionSources #-}

-- | The array of file formats for the output captions. If you leave this value
-- blank, Elastic Transcoder returns an error.
eCaptionFormats :: Lens' Captions ([CaptionFormat])
eCaptionFormats = lens _eCaptionFormats (\s a -> s { _eCaptionFormats = a })
{-# INLINE eCaptionFormats #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Captions' data type to populate a request.
mkCaptions :: Captions
mkCaptions = Captions
    { _eMergePolicy = Nothing
    , _eCaptionSources = mempty
    , _eCaptionFormats = mempty
    }
{-# INLINE mkCaptions #-}

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
cjoKey :: Lens' CreateJobOutput (Maybe Text)
cjoKey = lens _cjoKey (\s a -> s { _cjoKey = a })
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
cjoThumbnailPattern :: Lens' CreateJobOutput (Maybe Text)
cjoThumbnailPattern = lens _cjoThumbnailPattern (\s a -> s { _cjoThumbnailPattern = a })
{-# INLINE cjoThumbnailPattern #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values:
-- auto, 0, 90, 180, 270. The value auto generally works only if the file that
-- you're transcoding contains rotation metadata.
cjoRotate :: Lens' CreateJobOutput (Maybe Text)
cjoRotate = lens _cjoRotate (\s a -> s { _cjoRotate = a })
{-# INLINE cjoRotate #-}

-- | The Id of the preset to use for this job. The preset determines the audio,
-- video, and thumbnail settings that Elastic Transcoder uses for transcoding.
cjoPresetId :: Lens' CreateJobOutput (Maybe Text)
cjoPresetId = lens _cjoPresetId (\s a -> s { _cjoPresetId = a })
{-# INLINE cjoPresetId #-}

-- | If you specify a preset in PresetId for which the value of Container is ts
-- (MPEG-TS), SegmentDuration is the duration of each .ts file in seconds. The
-- range of valid values is 1 to 60 seconds.
cjoSegmentDuration :: Lens' CreateJobOutput (Maybe Text)
cjoSegmentDuration = lens _cjoSegmentDuration (\s a -> s { _cjoSegmentDuration = a })
{-# INLINE cjoSegmentDuration #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to
-- the video during transcoding. You can specify up to four watermarks for
-- each output. Settings for each watermark must be defined in the preset for
-- the current output.
cjoWatermarks :: Lens' CreateJobOutput ([JobWatermark])
cjoWatermarks = lens _cjoWatermarks (\s a -> s { _cjoWatermarks = a })
{-# INLINE cjoWatermarks #-}

-- | Information about the album art that you want Elastic Transcoder to add to
-- the file during transcoding. You can specify up to twenty album artworks
-- for each output. Settings for each artwork must be defined in the job for
-- the current output.
cjoAlbumArt :: Lens' CreateJobOutput (Maybe JobAlbumArt)
cjoAlbumArt = lens _cjoAlbumArt (\s a -> s { _cjoAlbumArt = a })
{-# INLINE cjoAlbumArt #-}

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make
-- up an output file. For the current release, you can only specify settings
-- for a single clip per output file. The Composition object cannot be null.
cjoComposition :: Lens' CreateJobOutput ([Clip])
cjoComposition = lens _cjoComposition (\s a -> s { _cjoComposition = a })
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
cjoCaptions :: Lens' CreateJobOutput (Maybe Captions)
cjoCaptions = lens _cjoCaptions (\s a -> s { _cjoCaptions = a })
{-# INLINE cjoCaptions #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateJobOutput' data type to populate a request.
mkCreateJobOutput :: CreateJobOutput
mkCreateJobOutput = CreateJobOutput
    { _cjoKey = Nothing
    , _cjoThumbnailPattern = Nothing
    , _cjoRotate = Nothing
    , _cjoPresetId = Nothing
    , _cjoSegmentDuration = Nothing
    , _cjoWatermarks = mempty
    , _cjoAlbumArt = Nothing
    , _cjoComposition = mempty
    , _cjoCaptions = Nothing
    }
{-# INLINE mkCreateJobOutput #-}

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
cjvName :: Lens' CreateJobPlaylist (Maybe Text)
cjvName = lens _cjvName (\s a -> s { _cjvName = a })
{-# INLINE cjvName #-}

-- | This value must currently be HLSv3.
cjvFormat :: Lens' CreateJobPlaylist (Maybe Text)
cjvFormat = lens _cjvFormat (\s a -> s { _cjvFormat = a })
{-# INLINE cjvFormat #-}

-- | For each output in this job that you want to include in a master playlist,
-- the value of the Outputs:Key object. If you include more than one output in
-- a playlist, the value of SegmentDuration for all of the outputs must be the
-- same.
cjvOutputKeys :: Lens' CreateJobPlaylist ([Text])
cjvOutputKeys = lens _cjvOutputKeys (\s a -> s { _cjvOutputKeys = a })
{-# INLINE cjvOutputKeys #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateJobPlaylist' data type to populate a request.
mkCreateJobPlaylist :: CreateJobPlaylist
mkCreateJobPlaylist = CreateJobPlaylist
    { _cjvName = Nothing
    , _cjvFormat = Nothing
    , _cjvOutputKeys = mempty
    }
{-# INLINE mkCreateJobPlaylist #-}

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
jbId :: Lens' Job (Maybe Text)
jbId = lens _jbId (\s a -> s { _jbId = a })
{-# INLINE jbId #-}

-- | The Amazon Resource Name (ARN) for the job.
jbArn :: Lens' Job (Maybe Text)
jbArn = lens _jbArn (\s a -> s { _jbArn = a })
{-# INLINE jbArn #-}

-- | The Id of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the Amazon
-- S3 bucket from which Elastic Transcoder gets the files to transcode and the
-- bucket into which Elastic Transcoder puts the transcoded files.
jbPipelineId :: Lens' Job (Maybe Text)
jbPipelineId = lens _jbPipelineId (\s a -> s { _jbPipelineId = a })
{-# INLINE jbPipelineId #-}

-- | A section of the request or response body that provides information about
-- the file that is being transcoded.
jbInput :: Lens' Job (Maybe JobInput)
jbInput = lens _jbInput (\s a -> s { _jbInput = a })
{-# INLINE jbInput #-}

-- | If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that is
-- listed for the first output in the Outputs object. Outputs recommended
-- instead. A section of the request or response body that provides
-- information about the transcoded (target) file.
jbOutput :: Lens' Job (Maybe JobOutput)
jbOutput = lens _jbOutput (\s a -> s { _jbOutput = a })
{-# INLINE jbOutput #-}

-- | Information about the output files. We recommend that you use the Outputs
-- syntax for all jobs, even when you want Elastic Transcoder to transcode a
-- file into only one format. Do not use both the Outputs and Output syntaxes
-- in the same request. You can create a maximum of 30 outputs per job. If you
-- specify more than one output for a job, Elastic Transcoder creates the
-- files for each output in the order in which you specify them in the job.
jbOutputs :: Lens' Job ([JobOutput])
jbOutputs = lens _jbOutputs (\s a -> s { _jbOutputs = a })
{-# INLINE jbOutputs #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names
-- of all files that this job creates, including output files, thumbnails, and
-- playlists. We recommend that you add a / or some other delimiter to the end
-- of the OutputKeyPrefix.
jbOutputKeyPrefix :: Lens' Job (Maybe Text)
jbOutputKeyPrefix = lens _jbOutputKeyPrefix (\s a -> s { _jbOutputKeyPrefix = a })
{-# INLINE jbOutputKeyPrefix #-}

-- | Outputs in MPEG-TS format only.If you specify a preset in PresetId for
-- which the value of Container is ts (MPEG-TS), Playlists contains
-- information about the master playlists that you want Elastic Transcoder to
-- create. We recommend that you create only one master playlist. The maximum
-- number of master playlists in a job is 30.
jbPlaylists :: Lens' Job ([Playlist])
jbPlaylists = lens _jbPlaylists (\s a -> s { _jbPlaylists = a })
{-# INLINE jbPlaylists #-}

-- | The status of the job: Submitted, Progressing, Complete, Canceled, or
-- Error.
jbStatus :: Lens' Job (Maybe Text)
jbStatus = lens _jbStatus (\s a -> s { _jbStatus = a })
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
jaaMergePolicy :: Lens' JobAlbumArt (Maybe Text)
jaaMergePolicy = lens _jaaMergePolicy (\s a -> s { _jaaMergePolicy = a })
{-# INLINE jaaMergePolicy #-}

-- | The file to be used as album art. There can be multiple artworks associated
-- with an audio file, to a maximum of 20. Valid formats are .jpg and .png.
jaaArtwork :: Lens' JobAlbumArt ([Artwork])
jaaArtwork = lens _jaaArtwork (\s a -> s { _jaaArtwork = a })
{-# INLINE jaaArtwork #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobAlbumArt' data type to populate a request.
mkJobAlbumArt :: JobAlbumArt
mkJobAlbumArt = JobAlbumArt
    { _jaaMergePolicy = Nothing
    , _jaaArtwork = mempty
    }
{-# INLINE mkJobAlbumArt #-}

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
jiKey :: Lens' JobInput (Maybe Text)
jiKey = lens _jiKey (\s a -> s { _jiKey = a })
{-# INLINE jiKey #-}

-- | The frame rate of the input file. If you want Elastic Transcoder to
-- automatically detect the frame rate of the input file, specify auto. If you
-- want to specify the frame rate for the input file, enter one of the
-- following values: 10, 15, 23.97, 24, 25, 29.97, 30, 60 If you specify a
-- value other than auto, Elastic Transcoder disables automatic detection of
-- the frame rate.
jiFrameRate :: Lens' JobInput (Maybe Text)
jiFrameRate = lens _jiFrameRate (\s a -> s { _jiFrameRate = a })
{-# INLINE jiFrameRate #-}

-- | This value must be auto, which causes Elastic Transcoder to automatically
-- detect the resolution of the input file.
jiResolution :: Lens' JobInput (Maybe Text)
jiResolution = lens _jiResolution (\s a -> s { _jiResolution = a })
{-# INLINE jiResolution #-}

-- | The aspect ratio of the input file. If you want Elastic Transcoder to
-- automatically detect the aspect ratio of the input file, specify auto. If
-- you want to specify the aspect ratio for the output file, enter one of the
-- following values: 1:1, 4:3, 3:2, 16:9 If you specify a value other than
-- auto, Elastic Transcoder disables automatic detection of the aspect ratio.
jiAspectRatio :: Lens' JobInput (Maybe Text)
jiAspectRatio = lens _jiAspectRatio (\s a -> s { _jiAspectRatio = a })
{-# INLINE jiAspectRatio #-}

-- | Whether the input file is interlaced. If you want Elastic Transcoder to
-- automatically detect whether the input file is interlaced, specify auto. If
-- you want to specify whether the input file is interlaced, enter one of the
-- following values: true, false If you specify a value other than auto,
-- Elastic Transcoder disables automatic detection of interlacing.
jiInterlaced :: Lens' JobInput (Maybe Text)
jiInterlaced = lens _jiInterlaced (\s a -> s { _jiInterlaced = a })
{-# INLINE jiInterlaced #-}

-- | The container type for the input file. If you want Elastic Transcoder to
-- automatically detect the container type of the input file, specify auto. If
-- you want to specify the container type for the input file, enter one of the
-- following values: 3gp, aac, asf, avi, divx, flv, m4a, mkv, mov, mp3, mp4,
-- mpeg, mpeg-ps, mpeg-ts, mxf, ogg, vob, wav, webm.
jiContainer :: Lens' JobInput (Maybe Text)
jiContainer = lens _jiContainer (\s a -> s { _jiContainer = a })
{-# INLINE jiContainer #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobInput' data type to populate a request.
mkJobInput :: JobInput
mkJobInput = JobInput
    { _jiKey = Nothing
    , _jiFrameRate = Nothing
    , _jiResolution = Nothing
    , _jiAspectRatio = Nothing
    , _jiInterlaced = Nothing
    , _jiContainer = Nothing
    }
{-# INLINE mkJobInput #-}

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
joId :: Lens' JobOutput (Maybe Text)
joId = lens _joId (\s a -> s { _joId = a })
{-# INLINE joId #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the OutputBucket object in the
-- pipeline that is specified by the pipeline ID.
joKey :: Lens' JobOutput (Maybe Text)
joKey = lens _joKey (\s a -> s { _joKey = a })
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
joThumbnailPattern :: Lens' JobOutput (Maybe Text)
joThumbnailPattern = lens _joThumbnailPattern (\s a -> s { _joThumbnailPattern = a })
{-# INLINE joThumbnailPattern #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following values:
-- auto, 0, 90, 180, 270 The value auto generally works only if the file that
-- you're transcoding contains rotation metadata.
joRotate :: Lens' JobOutput (Maybe Text)
joRotate = lens _joRotate (\s a -> s { _joRotate = a })
{-# INLINE joRotate #-}

-- | The value of the Id object for the preset that you want to use for this
-- job. The preset determines the audio, video, and thumbnail settings that
-- Elastic Transcoder uses for transcoding. To use a preset that you created,
-- specify the preset ID that Elastic Transcoder returned in the response when
-- you created the preset. You can also use the Elastic Transcoder system
-- presets, which you can get with ListPresets.
joPresetId :: Lens' JobOutput (Maybe Text)
joPresetId = lens _joPresetId (\s a -> s { _joPresetId = a })
{-# INLINE joPresetId #-}

-- | (Outputs in MPEG-TS format only.If you specify a preset in PresetId for
-- which the value of Containeris ts (MPEG-TS), SegmentDuration is the maximum
-- duration of each .ts file in seconds. The range of valid values is 1 to 60
-- seconds. If the duration of the video is not evenly divisible by
-- SegmentDuration, the duration of the last segment is the remainder of total
-- length/SegmentDuration. Elastic Transcoder creates an output-specific
-- playlist for each output that you specify in OutputKeys. To add an output
-- to the master playlist for this job, include it in OutputKeys.
joSegmentDuration :: Lens' JobOutput (Maybe Text)
joSegmentDuration = lens _joSegmentDuration (\s a -> s { _joSegmentDuration = a })
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
joStatus :: Lens' JobOutput (Maybe Text)
joStatus = lens _joStatus (\s a -> s { _joStatus = a })
{-# INLINE joStatus #-}

-- | Information that further explains Status.
joStatusDetail :: Lens' JobOutput (Maybe Text)
joStatusDetail = lens _joStatusDetail (\s a -> s { _joStatusDetail = a })
{-# INLINE joStatusDetail #-}

-- | Duration of the output file, in seconds.
joDuration :: Lens' JobOutput (Maybe Integer)
joDuration = lens _joDuration (\s a -> s { _joDuration = a })
{-# INLINE joDuration #-}

-- | Specifies the width of the output file in pixels.
joWidth :: Lens' JobOutput (Maybe Integer)
joWidth = lens _joWidth (\s a -> s { _joWidth = a })
{-# INLINE joWidth #-}

-- | Height of the output file, in pixels.
joHeight :: Lens' JobOutput (Maybe Integer)
joHeight = lens _joHeight (\s a -> s { _joHeight = a })
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
joWatermarks :: Lens' JobOutput ([JobWatermark])
joWatermarks = lens _joWatermarks (\s a -> s { _joWatermarks = a })
{-# INLINE joWatermarks #-}

-- | The album art to be associated with the output file, if any.
joAlbumArt :: Lens' JobOutput (Maybe JobAlbumArt)
joAlbumArt = lens _joAlbumArt (\s a -> s { _joAlbumArt = a })
{-# INLINE joAlbumArt #-}

-- | You can create an output file that contains an excerpt from the input file.
-- This excerpt, called a clip, can come from the beginning, middle, or end of
-- the file. The Composition object contains settings for the clips that make
-- up an output file. For the current release, you can only specify settings
-- for a single clip per output file. The Composition object cannot be null.
joComposition :: Lens' JobOutput ([Clip])
joComposition = lens _joComposition (\s a -> s { _joComposition = a })
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
joCaptions :: Lens' JobOutput (Maybe Captions)
joCaptions = lens _joCaptions (\s a -> s { _joCaptions = a })
{-# INLINE joCaptions #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobOutput' data type to populate a request.
mkJobOutput :: JobOutput
mkJobOutput = JobOutput
    { _joId = Nothing
    , _joKey = Nothing
    , _joThumbnailPattern = Nothing
    , _joRotate = Nothing
    , _joPresetId = Nothing
    , _joSegmentDuration = Nothing
    , _joStatus = Nothing
    , _joStatusDetail = Nothing
    , _joDuration = Nothing
    , _joWidth = Nothing
    , _joHeight = Nothing
    , _joWatermarks = mempty
    , _joAlbumArt = Nothing
    , _joComposition = mempty
    , _joCaptions = Nothing
    }
{-# INLINE mkJobOutput #-}

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
jxPresetWatermarkId :: Lens' JobWatermark (Maybe Text)
jxPresetWatermarkId = lens _jxPresetWatermarkId (\s a -> s { _jxPresetWatermarkId = a })
{-# INLINE jxPresetWatermarkId #-}

-- | The name of the .png or .jpg file that you want to use for the watermark.
-- To determine which Amazon S3 bucket contains the specified file, Elastic
-- Transcoder checks the pipeline specified by Pipeline; the Input Bucket
-- object in that pipeline identifies the bucket. If the file name includes a
-- prefix, for example, logos/128x64.png, include the prefix in the key. If
-- the file isn't in the specified bucket, Elastic Transcoder returns an
-- error.
jxInputKey :: Lens' JobWatermark (Maybe Text)
jxInputKey = lens _jxInputKey (\s a -> s { _jxInputKey = a })
{-# INLINE jxInputKey #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'JobWatermark' data type to populate a request.
mkJobWatermark :: JobWatermark
mkJobWatermark = JobWatermark
    { _jxPresetWatermarkId = Nothing
    , _jxInputKey = Nothing
    }
{-# INLINE mkJobWatermark #-}

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
nsProgressing :: Lens' Notifications (Maybe Text)
nsProgressing = lens _nsProgressing (\s a -> s { _nsProgressing = a })
{-# INLINE nsProgressing #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing the job.
nsCompleted :: Lens' Notifications (Maybe Text)
nsCompleted = lens _nsCompleted (\s a -> s { _nsCompleted = a })
{-# INLINE nsCompleted #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition.
nsWarning :: Lens' Notifications (Maybe Text)
nsWarning = lens _nsWarning (\s a -> s { _nsWarning = a })
{-# INLINE nsWarning #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition.
nsError :: Lens' Notifications (Maybe Text)
nsError = lens _nsError (\s a -> s { _nsError = a })
{-# INLINE nsError #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Notifications' data type to populate a request.
mkNotifications :: Notifications
mkNotifications = Notifications
    { _nsProgressing = Nothing
    , _nsCompleted = Nothing
    , _nsWarning = Nothing
    , _nsError = Nothing
    }
{-# INLINE mkNotifications #-}

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
pnGranteeType :: Lens' Permission (Maybe Text)
pnGranteeType = lens _pnGranteeType (\s a -> s { _pnGranteeType = a })
{-# INLINE pnGranteeType #-}

-- | The AWS user or group that you want to have access to transcoded files and
-- playlists. To identify the user or group, you can specify the canonical
-- user ID for an AWS account, an origin access identity for a CloudFront
-- distribution, the registered email address of an AWS account, or a
-- predefined Amazon S3 group.
pnGrantee :: Lens' Permission (Maybe Text)
pnGrantee = lens _pnGrantee (\s a -> s { _pnGrantee = a })
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
pnAccess :: Lens' Permission ([Text])
pnAccess = lens _pnAccess (\s a -> s { _pnAccess = a })
{-# INLINE pnAccess #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Permission' data type to populate a request.
mkPermission :: Permission
mkPermission = Permission
    { _pnGranteeType = Nothing
    , _pnGrantee = Nothing
    , _pnAccess = mempty
    }
{-# INLINE mkPermission #-}

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
peId :: Lens' Pipeline (Maybe Text)
peId = lens _peId (\s a -> s { _peId = a })
{-# INLINE peId #-}

-- | The Amazon Resource Name (ARN) for the pipeline.
peArn :: Lens' Pipeline (Maybe Text)
peArn = lens _peArn (\s a -> s { _peArn = a })
{-# INLINE peArn #-}

-- | The name of the pipeline. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced. Constraints: Maximum 40
-- characters.
peName :: Lens' Pipeline (Maybe Text)
peName = lens _peName (\s a -> s { _peName = a })
{-# INLINE peName #-}

-- | The current status of the pipeline: Active: The pipeline is processing
-- jobs. Paused: The pipeline is not currently processing jobs.
peStatus :: Lens' Pipeline (Maybe Text)
peStatus = lens _peStatus (\s a -> s { _peStatus = a })
{-# INLINE peStatus #-}

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for
-- transcoding and the graphics files, if any, that you want to use for
-- watermarks.
peInputBucket :: Lens' Pipeline (Maybe Text)
peInputBucket = lens _peInputBucket (\s a -> s { _peInputBucket = a })
{-# INLINE peInputBucket #-}

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save
-- transcoded files, thumbnails, and playlists. Either you specify this value,
-- or you specify both ContentConfig and ThumbnailConfig.
peOutputBucket :: Lens' Pipeline (Maybe Text)
peOutputBucket = lens _peOutputBucket (\s a -> s { _peOutputBucket = a })
{-# INLINE peOutputBucket #-}

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder
-- uses to transcode jobs for this pipeline.
peRole :: Lens' Pipeline (Maybe Text)
peRole = lens _peRole (\s a -> s { _peRole = a })
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
peNotifications :: Lens' Pipeline (Maybe Notifications)
peNotifications = lens _peNotifications (\s a -> s { _peNotifications = a })
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
peContentConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
peContentConfig = lens _peContentConfig (\s a -> s { _peContentConfig = a })
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
peThumbnailConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
peThumbnailConfig = lens _peThumbnailConfig (\s a -> s { _peThumbnailConfig = a })
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
pocBucket :: Lens' PipelineOutputConfig (Maybe Text)
pocBucket = lens _pocBucket (\s a -> s { _pocBucket = a })
{-# INLINE pocBucket #-}

-- | The Amazon S3 storage class, Standard or ReducedRedundancy, that you want
-- Elastic Transcoder to assign to the video files and playlists that it
-- stores in your Amazon S3 bucket.
pocStorageClass :: Lens' PipelineOutputConfig (Maybe Text)
pocStorageClass = lens _pocStorageClass (\s a -> s { _pocStorageClass = a })
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
pocPermissions :: Lens' PipelineOutputConfig ([Permission])
pocPermissions = lens _pocPermissions (\s a -> s { _pocPermissions = a })
{-# INLINE pocPermissions #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PipelineOutputConfig' data type to populate a request.
mkPipelineOutputConfig :: PipelineOutputConfig
mkPipelineOutputConfig = PipelineOutputConfig
    { _pocBucket = Nothing
    , _pocStorageClass = Nothing
    , _pocPermissions = mempty
    }
{-# INLINE mkPipelineOutputConfig #-}

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
puName :: Lens' Playlist (Maybe Text)
puName = lens _puName (\s a -> s { _puName = a })
{-# INLINE puName #-}

-- | This value must currently be HLSv3.
puFormat :: Lens' Playlist (Maybe Text)
puFormat = lens _puFormat (\s a -> s { _puFormat = a })
{-# INLINE puFormat #-}

-- | For each output in this job that you want to include in a master playlist,
-- the value of the Outputs:Key object. If you include more than one output in
-- a playlist, the value of SegmentDuration for all of the outputs must be the
-- same.
puOutputKeys :: Lens' Playlist ([Text])
puOutputKeys = lens _puOutputKeys (\s a -> s { _puOutputKeys = a })
{-# INLINE puOutputKeys #-}

-- | The status of the job with which the playlist is associated.
puStatus :: Lens' Playlist (Maybe Text)
puStatus = lens _puStatus (\s a -> s { _puStatus = a })
{-# INLINE puStatus #-}

-- | Information that further explains the status.
puStatusDetail :: Lens' Playlist (Maybe Text)
puStatusDetail = lens _puStatusDetail (\s a -> s { _puStatusDetail = a })
{-# INLINE puStatusDetail #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Playlist' data type to populate a request.
mkPlaylist :: Playlist
mkPlaylist = Playlist
    { _puName = Nothing
    , _puFormat = Nothing
    , _puOutputKeys = mempty
    , _puStatus = Nothing
    , _puStatusDetail = Nothing
    }
{-# INLINE mkPlaylist #-}

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
ppuId :: Lens' Preset (Maybe Text)
ppuId = lens _ppuId (\s a -> s { _ppuId = a })
{-# INLINE ppuId #-}

-- | The Amazon Resource Name (ARN) for the preset.
ppuArn :: Lens' Preset (Maybe Text)
ppuArn = lens _ppuArn (\s a -> s { _ppuArn = a })
{-# INLINE ppuArn #-}

-- | The name of the preset.
ppuName :: Lens' Preset (Maybe Text)
ppuName = lens _ppuName (\s a -> s { _ppuName = a })
{-# INLINE ppuName #-}

-- | A description of the preset.
ppuDescription :: Lens' Preset (Maybe Text)
ppuDescription = lens _ppuDescription (\s a -> s { _ppuDescription = a })
{-# INLINE ppuDescription #-}

-- | The container type for the output file. Valid values include mp3, mp4, ogg,
-- ts, and webm.
ppuContainer :: Lens' Preset (Maybe Text)
ppuContainer = lens _ppuContainer (\s a -> s { _ppuContainer = a })
{-# INLINE ppuContainer #-}

-- | A section of the response body that provides information about the audio
-- preset values.
ppuAudio :: Lens' Preset (Maybe AudioParameters)
ppuAudio = lens _ppuAudio (\s a -> s { _ppuAudio = a })
{-# INLINE ppuAudio #-}

-- | A section of the response body that provides information about the video
-- preset values.
ppuVideo :: Lens' Preset (Maybe VideoParameters)
ppuVideo = lens _ppuVideo (\s a -> s { _ppuVideo = a })
{-# INLINE ppuVideo #-}

-- | A section of the response body that provides information about the
-- thumbnail preset values, if any.
ppuThumbnails :: Lens' Preset (Maybe Thumbnails)
ppuThumbnails = lens _ppuThumbnails (\s a -> s { _ppuThumbnails = a })
{-# INLINE ppuThumbnails #-}

-- | Whether the preset is a default preset provided by Elastic Transcoder
-- (System) or a preset that you have defined (Custom).
ppuType :: Lens' Preset (Maybe Text)
ppuType = lens _ppuType (\s a -> s { _ppuType = a })
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
ppkId :: Lens' PresetWatermark (Maybe Text)
ppkId = lens _ppkId (\s a -> s { _ppkId = a })
{-# INLINE ppkId #-}

-- | The maximum width of the watermark in one of the following formats: number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is
-- the value of MaxWidth. integer percentage (%): The range of valid values is
-- 0 to 100. Use the value of Target to specify whether you want Elastic
-- Transcoder to include the black bars that are added by Elastic Transcoder,
-- if any, in the calculation. If you specify the value in pixels, it must be
-- less than or equal to the value of MaxWidth.
ppkMaxWidth :: Lens' PresetWatermark (Maybe Text)
ppkMaxWidth = lens _ppkMaxWidth (\s a -> s { _ppkMaxWidth = a })
{-# INLINE ppkMaxWidth #-}

-- | The maximum height of the watermark in one of the following formats: number
-- of pixels (px): The minimum value is 16 pixels, and the maximum value is
-- the value of MaxHeight. integer percentage (%): The range of valid values
-- is 0 to 100. Use the value of Target to specify whether you want Elastic
-- Transcoder to include the black bars that are added by Elastic Transcoder,
-- if any, in the calculation. If you specify the value in pixels, it must be
-- less than or equal to the value of MaxHeight.
ppkMaxHeight :: Lens' PresetWatermark (Maybe Text)
ppkMaxHeight = lens _ppkMaxHeight (\s a -> s { _ppkMaxHeight = a })
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
ppkSizingPolicy :: Lens' PresetWatermark (Maybe Text)
ppkSizingPolicy = lens _ppkSizingPolicy (\s a -> s { _ppkSizingPolicy = a })
{-# INLINE ppkSizingPolicy #-}

-- | The horizontal position of the watermark unless you specify a non-zero
-- value for HorizontalOffset: Left: The left edge of the watermark is aligned
-- with the left border of the video. Right: The right edge of the watermark
-- is aligned with the right border of the video. Center: The watermark is
-- centered between the left and right borders.
ppkHorizontalAlign :: Lens' PresetWatermark (Maybe Text)
ppkHorizontalAlign = lens _ppkHorizontalAlign (\s a -> s { _ppkHorizontalAlign = a })
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
ppkHorizontalOffset :: Lens' PresetWatermark (Maybe Text)
ppkHorizontalOffset = lens _ppkHorizontalOffset (\s a -> s { _ppkHorizontalOffset = a })
{-# INLINE ppkHorizontalOffset #-}

-- | The vertical position of the watermark unless you specify a non-zero value
-- for VerticalOffset: Top: The top edge of the watermark is aligned with the
-- top border of the video. Bottom: The bottom edge of the watermark is
-- aligned with the bottom border of the video. Center: The watermark is
-- centered between the top and bottom borders.
ppkVerticalAlign :: Lens' PresetWatermark (Maybe Text)
ppkVerticalAlign = lens _ppkVerticalAlign (\s a -> s { _ppkVerticalAlign = a })
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
ppkVerticalOffset :: Lens' PresetWatermark (Maybe Text)
ppkVerticalOffset = lens _ppkVerticalOffset (\s a -> s { _ppkVerticalOffset = a })
{-# INLINE ppkVerticalOffset #-}

-- | A percentage that indicates how much you want a watermark to obscure the
-- video in the location where it appears. Valid values are 0 (the watermark
-- is invisible) to 100 (the watermark completely obscures the video in the
-- specified location). The datatype of Opacity is float. Elastic Transcoder
-- supports transparent .png graphics. If you use a transparent .png, the
-- transparent portion of the video appears as if you had specified a value of
-- 0 for Opacity. The .jpg file format doesn't support transparency.
ppkOpacity :: Lens' PresetWatermark (Maybe Text)
ppkOpacity = lens _ppkOpacity (\s a -> s { _ppkOpacity = a })
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
ppkTarget :: Lens' PresetWatermark (Maybe Text)
ppkTarget = lens _ppkTarget (\s a -> s { _ppkTarget = a })
{-# INLINE ppkTarget #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PresetWatermark' data type to populate a request.
mkPresetWatermark :: PresetWatermark
mkPresetWatermark = PresetWatermark
    { _ppkId = Nothing
    , _ppkMaxWidth = Nothing
    , _ppkMaxHeight = Nothing
    , _ppkSizingPolicy = Nothing
    , _ppkHorizontalAlign = Nothing
    , _ppkHorizontalOffset = Nothing
    , _ppkVerticalAlign = Nothing
    , _ppkVerticalOffset = Nothing
    , _ppkOpacity = Nothing
    , _ppkTarget = Nothing
    }
{-# INLINE mkPresetWatermark #-}

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
ttsFormat :: Lens' Thumbnails (Maybe Text)
ttsFormat = lens _ttsFormat (\s a -> s { _ttsFormat = a })
{-# INLINE ttsFormat #-}

-- | The number of seconds between thumbnails. Specify an integer value.
ttsInterval :: Lens' Thumbnails (Maybe Text)
ttsInterval = lens _ttsInterval (\s a -> s { _ttsInterval = a })
{-# INLINE ttsInterval #-}

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values MaxWidth, MaxHeight, SizingPolicy, and
-- PaddingPolicy instead of Resolution and AspectRatio. The two groups of
-- settings are mutually exclusive. Do not use them together. The width and
-- height of thumbnail files in pixels. Specify a value in the format width x
-- height where both values are even integers. The values cannot exceed the
-- width and height that you specified in the Video:Resolution object.
ttsResolution :: Lens' Thumbnails (Maybe Text)
ttsResolution = lens _ttsResolution (\s a -> s { _ttsResolution = a })
{-# INLINE ttsResolution #-}

-- | To better control resolution and aspect ratio of thumbnails, we recommend
-- that you use the values MaxWidth, MaxHeight, SizingPolicy, and
-- PaddingPolicy instead of Resolution and AspectRatio. The two groups of
-- settings are mutually exclusive. Do not use them together. The aspect ratio
-- of thumbnails. Valid values include: auto, 1:1, 4:3, 3:2, 16:9 If you
-- specify auto, Elastic Transcoder tries to preserve the aspect ratio of the
-- video in the output file.
ttsAspectRatio :: Lens' Thumbnails (Maybe Text)
ttsAspectRatio = lens _ttsAspectRatio (\s a -> s { _ttsAspectRatio = a })
{-# INLINE ttsAspectRatio #-}

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
ttsMaxWidth :: Lens' Thumbnails (Maybe Text)
ttsMaxWidth = lens _ttsMaxWidth (\s a -> s { _ttsMaxWidth = a })
{-# INLINE ttsMaxWidth #-}

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
ttsMaxHeight :: Lens' Thumbnails (Maybe Text)
ttsMaxHeight = lens _ttsMaxHeight (\s a -> s { _ttsMaxHeight = a })
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
ttsSizingPolicy :: Lens' Thumbnails (Maybe Text)
ttsSizingPolicy = lens _ttsSizingPolicy (\s a -> s { _ttsSizingPolicy = a })
{-# INLINE ttsSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of thumbnails to make the
-- total size of the thumbnails match the values that you specified for
-- thumbnail MaxWidth and MaxHeight settings.
ttsPaddingPolicy :: Lens' Thumbnails (Maybe Text)
ttsPaddingPolicy = lens _ttsPaddingPolicy (\s a -> s { _ttsPaddingPolicy = a })
{-# INLINE ttsPaddingPolicy #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Thumbnails' data type to populate a request.
mkThumbnails :: Thumbnails
mkThumbnails = Thumbnails
    { _ttsFormat = Nothing
    , _ttsInterval = Nothing
    , _ttsResolution = Nothing
    , _ttsAspectRatio = Nothing
    , _ttsMaxWidth = Nothing
    , _ttsMaxHeight = Nothing
    , _ttsSizingPolicy = Nothing
    , _ttsPaddingPolicy = Nothing
    }
{-# INLINE mkThumbnails #-}

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
tsStartTime :: Lens' TimeSpan (Maybe Text)
tsStartTime = lens _tsStartTime (\s a -> s { _tsStartTime = a })
{-# INLINE tsStartTime #-}

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
-- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum
-- value: 86399.999). If you don't specify a value, Elastic Transcoder creates
-- an output file from StartTime to the end of the file. If you specify a
-- value longer than the duration of the input file, Elastic Transcoder
-- transcodes the file and returns a warning message.
tsDuration :: Lens' TimeSpan (Maybe Text)
tsDuration = lens _tsDuration (\s a -> s { _tsDuration = a })
{-# INLINE tsDuration #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TimeSpan' data type to populate a request.
mkTimeSpan :: TimeSpan
mkTimeSpan = TimeSpan
    { _tsStartTime = Nothing
    , _tsDuration = Nothing
    }
{-# INLINE mkTimeSpan #-}

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
vpCodec :: Lens' VideoParameters (Maybe Text)
vpCodec = lens _vpCodec (\s a -> s { _vpCodec = a })
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
vpCodecOptions :: Lens' VideoParameters (Map Text Text)
vpCodecOptions = lens _vpCodecOptions (\s a -> s { _vpCodecOptions = a })
{-# INLINE vpCodecOptions #-}

-- | The maximum number of frames between key frames. Key frames are fully
-- encoded frames; the frames between key frames are encoded based, in part,
-- on the content of the key frames. The value is an integer formatted as a
-- string; valid values are between 1 (every frame is a key frame) and 100000,
-- inclusive. A higher value results in higher compression but may also
-- discernibly decrease video quality.
vpKeyframesMaxDist :: Lens' VideoParameters (Maybe Text)
vpKeyframesMaxDist = lens _vpKeyframesMaxDist (\s a -> s { _vpKeyframesMaxDist = a })
{-# INLINE vpKeyframesMaxDist #-}

-- | Whether to use a fixed value for FixedGOP. Valid values are true and false:
-- true: Elastic Transcoder uses the value of KeyframesMaxDist for the
-- distance between key frames (the number of frames in a group of pictures,
-- or GOP). false: The distance between key frames can vary.
vpFixedGOP :: Lens' VideoParameters (Maybe Text)
vpFixedGOP = lens _vpFixedGOP (\s a -> s { _vpFixedGOP = a })
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
vpBitRate :: Lens' VideoParameters (Maybe Text)
vpBitRate = lens _vpBitRate (\s a -> s { _vpBitRate = a })
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
vpFrameRate :: Lens' VideoParameters (Maybe Text)
vpFrameRate = lens _vpFrameRate (\s a -> s { _vpFrameRate = a })
{-# INLINE vpFrameRate #-}

-- | If you specify auto for FrameRate, Elastic Transcoder uses the frame rate
-- of the input video for the frame rate of the output video. Specify the
-- maximum frame rate that you want Elastic Transcoder to use when the frame
-- rate of the input video is greater than the desired maximum frame rate of
-- the output video. Valid values include: 10, 15, 23.97, 24, 25, 29.97, 30,
-- 60.
vpMaxFrameRate :: Lens' VideoParameters (Maybe Text)
vpMaxFrameRate = lens _vpMaxFrameRate (\s a -> s { _vpMaxFrameRate = a })
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
vpResolution :: Lens' VideoParameters (Maybe Text)
vpResolution = lens _vpResolution (\s a -> s { _vpResolution = a })
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
vpAspectRatio :: Lens' VideoParameters (Maybe Text)
vpAspectRatio = lens _vpAspectRatio (\s a -> s { _vpAspectRatio = a })
{-# INLINE vpAspectRatio #-}

-- | The maximum width of the output video in pixels. If you specify auto,
-- Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify
-- a numeric value, enter an even integer between 128 and 4096.
vpMaxWidth :: Lens' VideoParameters (Maybe Text)
vpMaxWidth = lens _vpMaxWidth (\s a -> s { _vpMaxWidth = a })
{-# INLINE vpMaxWidth #-}

-- | The maximum height of the output video in pixels. If you specify auto,
-- Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify
-- a numeric value, enter an even integer between 96 and 3072.
vpMaxHeight :: Lens' VideoParameters (Maybe Text)
vpMaxHeight = lens _vpMaxHeight (\s a -> s { _vpMaxHeight = a })
{-# INLINE vpMaxHeight #-}

-- | The value that Elastic Transcoder adds to the metadata in the output file.
vpDisplayAspectRatio :: Lens' VideoParameters (Maybe Text)
vpDisplayAspectRatio = lens _vpDisplayAspectRatio (\s a -> s { _vpDisplayAspectRatio = a })
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
vpSizingPolicy :: Lens' VideoParameters (Maybe Text)
vpSizingPolicy = lens _vpSizingPolicy (\s a -> s { _vpSizingPolicy = a })
{-# INLINE vpSizingPolicy #-}

-- | When you set PaddingPolicy to Pad, Elastic Transcoder may add black bars to
-- the top and bottom and/or left and right sides of the output video to make
-- the total size of the output video match the values that you specified for
-- MaxWidth and MaxHeight.
vpPaddingPolicy :: Lens' VideoParameters (Maybe Text)
vpPaddingPolicy = lens _vpPaddingPolicy (\s a -> s { _vpPaddingPolicy = a })
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
vpWatermarks :: Lens' VideoParameters ([PresetWatermark])
vpWatermarks = lens _vpWatermarks (\s a -> s { _vpWatermarks = a })
{-# INLINE vpWatermarks #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VideoParameters' data type to populate a request.
mkVideoParameters :: VideoParameters
mkVideoParameters = VideoParameters
    { _vpCodec = Nothing
    , _vpCodecOptions = mempty
    , _vpKeyframesMaxDist = Nothing
    , _vpFixedGOP = Nothing
    , _vpBitRate = Nothing
    , _vpFrameRate = Nothing
    , _vpMaxFrameRate = Nothing
    , _vpResolution = Nothing
    , _vpAspectRatio = Nothing
    , _vpMaxWidth = Nothing
    , _vpMaxHeight = Nothing
    , _vpDisplayAspectRatio = Nothing
    , _vpSizingPolicy = Nothing
    , _vpPaddingPolicy = Nothing
    , _vpWatermarks = mempty
    }
{-# INLINE mkVideoParameters #-}

instance FromJSON VideoParameters

instance ToJSON VideoParameters
