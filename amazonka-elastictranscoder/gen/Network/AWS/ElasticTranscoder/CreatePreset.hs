{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The CreatePreset operation creates a preset with settings that you
-- specify.
--
-- Elastic Transcoder checks the CreatePreset settings to ensure that they
-- meet Elastic Transcoder requirements and to determine whether they
-- comply with H.264 standards. If your settings are not valid for Elastic
-- Transcoder, Elastic Transcoder returns an HTTP 400 response
-- (@ValidationException@) and does not create the preset. If the settings
-- are valid for Elastic Transcoder but aren\'t strictly compliant with the
-- H.264 standard, Elastic Transcoder creates the preset and returns a
-- warning message in the response. This helps you determine whether your
-- settings comply with the H.264 standard while giving you greater
-- flexibility with respect to the video that Elastic Transcoder produces.
--
-- Elastic Transcoder uses the H.264 video-compression format. For more
-- information, see the International Telecommunication Union publication
-- /Recommendation ITU-T H.264: Advanced video coding for generic
-- audiovisual services/.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CreatePreset.html>
module Network.AWS.ElasticTranscoder.CreatePreset
    (
    -- * Request
      CreatePreset
    -- ** Request constructor
    , createPreset
    -- ** Request lenses
    , cpVideo
    , cpThumbnails
    , cpAudio
    , cpDescription
    , cpName
    , cpContainer

    -- * Response
    , CreatePresetResponse
    -- ** Response constructor
    , createPresetResponse
    -- ** Response lenses
    , cprWarning
    , cprPreset
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPreset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpVideo'
--
-- * 'cpThumbnails'
--
-- * 'cpAudio'
--
-- * 'cpDescription'
--
-- * 'cpName'
--
-- * 'cpContainer'
data CreatePreset = CreatePreset'{_cpVideo :: Maybe VideoParameters, _cpThumbnails :: Maybe Thumbnails, _cpAudio :: Maybe AudioParameters, _cpDescription :: Maybe Text, _cpName :: Text, _cpContainer :: Text} deriving (Eq, Read, Show)

-- | 'CreatePreset' smart constructor.
createPreset :: Text -> Text -> CreatePreset
createPreset pName pContainer = CreatePreset'{_cpVideo = Nothing, _cpThumbnails = Nothing, _cpAudio = Nothing, _cpDescription = Nothing, _cpName = pName, _cpContainer = pContainer};

-- | A section of the request body that specifies the video parameters.
cpVideo :: Lens' CreatePreset (Maybe VideoParameters)
cpVideo = lens _cpVideo (\ s a -> s{_cpVideo = a});

-- | A section of the request body that specifies the thumbnail parameters,
-- if any.
cpThumbnails :: Lens' CreatePreset (Maybe Thumbnails)
cpThumbnails = lens _cpThumbnails (\ s a -> s{_cpThumbnails = a});

-- | A section of the request body that specifies the audio parameters.
cpAudio :: Lens' CreatePreset (Maybe AudioParameters)
cpAudio = lens _cpAudio (\ s a -> s{_cpAudio = a});

-- | A description of the preset.
cpDescription :: Lens' CreatePreset (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a});

-- | The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
cpName :: Lens' CreatePreset Text
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
cpContainer :: Lens' CreatePreset Text
cpContainer = lens _cpContainer (\ s a -> s{_cpContainer = a});

instance AWSRequest CreatePreset where
        type Sv CreatePreset = ElasticTranscoder
        type Rs CreatePreset = CreatePresetResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreatePresetResponse' <$>
                   (x .?> "Warning") <*> (x .?> "Preset"))

instance ToHeaders CreatePreset where
        toHeaders = const mempty

instance ToJSON CreatePreset where
        toJSON CreatePreset'{..}
          = object
              ["Video" .= _cpVideo, "Thumbnails" .= _cpThumbnails,
               "Audio" .= _cpAudio, "Description" .= _cpDescription,
               "Name" .= _cpName, "Container" .= _cpContainer]

instance ToPath CreatePreset where
        toPath = const "/2012-09-25/presets"

instance ToQuery CreatePreset where
        toQuery = const mempty

-- | /See:/ 'createPresetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprWarning'
--
-- * 'cprPreset'
data CreatePresetResponse = CreatePresetResponse'{_cprWarning :: Maybe Text, _cprPreset :: Maybe Preset} deriving (Eq, Read, Show)

-- | 'CreatePresetResponse' smart constructor.
createPresetResponse :: CreatePresetResponse
createPresetResponse = CreatePresetResponse'{_cprWarning = Nothing, _cprPreset = Nothing};

-- | If the preset settings don\'t comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don\'t meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
cprWarning :: Lens' CreatePresetResponse (Maybe Text)
cprWarning = lens _cprWarning (\ s a -> s{_cprWarning = a});

-- | A section of the response body that provides information about the
-- preset that is created.
cprPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprPreset = lens _cprPreset (\ s a -> s{_cprPreset = a});
