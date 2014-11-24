{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreatePreset operation creates a preset with settings that you specify.
-- Elastic Transcoder checks the CreatePreset settings to ensure that they
-- meet Elastic Transcoder requirements and to determine whether they comply
-- with H.264 standards. If your settings are not valid for Elastic
-- Transcoder, Elastic Transcoder returns an HTTP 400 response
-- (ValidationException) and does not create the preset. If the settings are
-- valid for Elastic Transcoder but aren't strictly compliant with the H.264
-- standard, Elastic Transcoder creates the preset and returns a warning
-- message in the response. This helps you determine whether your settings
-- comply with the H.264 standard while giving you greater flexibility with
-- respect to the video that Elastic Transcoder produces. Elastic Transcoder
-- uses the H.264 video-compression format. For more information, see the
-- International Telecommunication Union publication /Recommendation ITU-T
-- H.264: Advanced video coding for generic audiovisual services/.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CreatePreset.html>
module Network.AWS.ElasticTranscoder.CreatePreset
    (
    -- * Request
      CreatePreset
    -- ** Request constructor
    , createPreset
    -- ** Request lenses
    , cpAudio
    , cpContainer
    , cpDescription
    , cpName
    , cpThumbnails
    , cpVideo

    -- * Response
    , CreatePresetResponse
    -- ** Response constructor
    , createPresetResponse
    -- ** Response lenses
    , cprPreset
    , cprWarning
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

data CreatePreset = CreatePreset
    { _cpAudio       :: Maybe AudioParameters
    , _cpContainer   :: Text
    , _cpDescription :: Maybe Text
    , _cpName        :: Text
    , _cpThumbnails  :: Maybe Thumbnails
    , _cpVideo       :: Maybe VideoParameters
    } deriving (Eq, Show)

-- | 'CreatePreset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpAudio' @::@ 'Maybe' 'AudioParameters'
--
-- * 'cpContainer' @::@ 'Text'
--
-- * 'cpDescription' @::@ 'Maybe' 'Text'
--
-- * 'cpName' @::@ 'Text'
--
-- * 'cpThumbnails' @::@ 'Maybe' 'Thumbnails'
--
-- * 'cpVideo' @::@ 'Maybe' 'VideoParameters'
--
createPreset :: Text -- ^ 'cpName'
             -> Text -- ^ 'cpContainer'
             -> CreatePreset
createPreset p1 p2 = CreatePreset
    { _cpName        = p1
    , _cpContainer   = p2
    , _cpDescription = Nothing
    , _cpVideo       = Nothing
    , _cpAudio       = Nothing
    , _cpThumbnails  = Nothing
    }

-- | A section of the request body that specifies the audio parameters.
cpAudio :: Lens' CreatePreset (Maybe AudioParameters)
cpAudio = lens _cpAudio (\s a -> s { _cpAudio = a })

-- | The container type for the output file. Valid values include fmp4, mp3,
-- mp4, ogg, ts, and webm.
cpContainer :: Lens' CreatePreset Text
cpContainer = lens _cpContainer (\s a -> s { _cpContainer = a })

-- | A description of the preset.
cpDescription :: Lens' CreatePreset (Maybe Text)
cpDescription = lens _cpDescription (\s a -> s { _cpDescription = a })

-- | The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
cpName :: Lens' CreatePreset Text
cpName = lens _cpName (\s a -> s { _cpName = a })

-- | A section of the request body that specifies the thumbnail parameters, if
-- any.
cpThumbnails :: Lens' CreatePreset (Maybe Thumbnails)
cpThumbnails = lens _cpThumbnails (\s a -> s { _cpThumbnails = a })

-- | A section of the request body that specifies the video parameters.
cpVideo :: Lens' CreatePreset (Maybe VideoParameters)
cpVideo = lens _cpVideo (\s a -> s { _cpVideo = a })

data CreatePresetResponse = CreatePresetResponse
    { _cprPreset  :: Maybe Preset
    , _cprWarning :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreatePresetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprPreset' @::@ 'Maybe' 'Preset'
--
-- * 'cprWarning' @::@ 'Maybe' 'Text'
--
createPresetResponse :: CreatePresetResponse
createPresetResponse = CreatePresetResponse
    { _cprPreset  = Nothing
    , _cprWarning = Nothing
    }

-- | A section of the response body that provides information about the preset
-- that is created.
cprPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprPreset = lens _cprPreset (\s a -> s { _cprPreset = a })

-- | If the preset settings don't comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don't meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
cprWarning :: Lens' CreatePresetResponse (Maybe Text)
cprWarning = lens _cprWarning (\s a -> s { _cprWarning = a })

instance ToPath CreatePreset where
    toPath = const "/2012-09-25/presets"

instance ToQuery CreatePreset where
    toQuery = const mempty

instance ToHeaders CreatePreset

instance ToJSON CreatePreset where
    toJSON CreatePreset{..} = object
        [ "Name"        .= _cpName
        , "Description" .= _cpDescription
        , "Container"   .= _cpContainer
        , "Video"       .= _cpVideo
        , "Audio"       .= _cpAudio
        , "Thumbnails"  .= _cpThumbnails
        ]

instance AWSRequest CreatePreset where
    type Sv CreatePreset = ElasticTranscoder
    type Rs CreatePreset = CreatePresetResponse

    request  = post
    response = jsonResponse

instance FromJSON CreatePresetResponse where
    parseJSON = withObject "CreatePresetResponse" $ \o -> CreatePresetResponse
        <$> o .:? "Preset"
        <*> o .:? "Warning"
