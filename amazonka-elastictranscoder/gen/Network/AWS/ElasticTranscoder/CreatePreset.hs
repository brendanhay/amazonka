{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The CreatePreset operation creates a preset with settings that you
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
    , cprqVideo
    , cprqThumbnails
    , cprqAudio
    , cprqDescription
    , cprqName
    , cprqContainer

    -- * Response
    , CreatePresetResponse
    -- ** Response constructor
    , createPresetResponse
    -- ** Response lenses
    , cprsWarning
    , cprsPreset
    , cprsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @CreatePresetRequest@ structure.
--
-- /See:/ 'createPreset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprqVideo'
--
-- * 'cprqThumbnails'
--
-- * 'cprqAudio'
--
-- * 'cprqDescription'
--
-- * 'cprqName'
--
-- * 'cprqContainer'
data CreatePreset = CreatePreset'
    { _cprqVideo       :: !(Maybe VideoParameters)
    , _cprqThumbnails  :: !(Maybe Thumbnails)
    , _cprqAudio       :: !(Maybe AudioParameters)
    , _cprqDescription :: !(Maybe Text)
    , _cprqName        :: !Text
    , _cprqContainer   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePreset' smart constructor.
createPreset :: Text -> Text -> CreatePreset
createPreset pName pContainer =
    CreatePreset'
    { _cprqVideo = Nothing
    , _cprqThumbnails = Nothing
    , _cprqAudio = Nothing
    , _cprqDescription = Nothing
    , _cprqName = pName
    , _cprqContainer = pContainer
    }

-- | A section of the request body that specifies the video parameters.
cprqVideo :: Lens' CreatePreset (Maybe VideoParameters)
cprqVideo = lens _cprqVideo (\ s a -> s{_cprqVideo = a});

-- | A section of the request body that specifies the thumbnail parameters,
-- if any.
cprqThumbnails :: Lens' CreatePreset (Maybe Thumbnails)
cprqThumbnails = lens _cprqThumbnails (\ s a -> s{_cprqThumbnails = a});

-- | A section of the request body that specifies the audio parameters.
cprqAudio :: Lens' CreatePreset (Maybe AudioParameters)
cprqAudio = lens _cprqAudio (\ s a -> s{_cprqAudio = a});

-- | A description of the preset.
cprqDescription :: Lens' CreatePreset (Maybe Text)
cprqDescription = lens _cprqDescription (\ s a -> s{_cprqDescription = a});

-- | The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
cprqName :: Lens' CreatePreset Text
cprqName = lens _cprqName (\ s a -> s{_cprqName = a});

-- | The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
cprqContainer :: Lens' CreatePreset Text
cprqContainer = lens _cprqContainer (\ s a -> s{_cprqContainer = a});

instance AWSRequest CreatePreset where
        type Sv CreatePreset = ElasticTranscoder
        type Rs CreatePreset = CreatePresetResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreatePresetResponse' <$>
                   (x .?> "Warning") <*> (x .?> "Preset") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreatePreset where
        toHeaders = const mempty

instance ToJSON CreatePreset where
        toJSON CreatePreset'{..}
          = object
              ["Video" .= _cprqVideo,
               "Thumbnails" .= _cprqThumbnails,
               "Audio" .= _cprqAudio,
               "Description" .= _cprqDescription,
               "Name" .= _cprqName, "Container" .= _cprqContainer]

instance ToPath CreatePreset where
        toPath = const "/2012-09-25/presets"

instance ToQuery CreatePreset where
        toQuery = const mempty

-- | The @CreatePresetResponse@ structure.
--
-- /See:/ 'createPresetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprsWarning'
--
-- * 'cprsPreset'
--
-- * 'cprsStatus'
data CreatePresetResponse = CreatePresetResponse'
    { _cprsWarning :: !(Maybe Text)
    , _cprsPreset  :: !(Maybe Preset)
    , _cprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePresetResponse' smart constructor.
createPresetResponse :: Int -> CreatePresetResponse
createPresetResponse pStatus =
    CreatePresetResponse'
    { _cprsWarning = Nothing
    , _cprsPreset = Nothing
    , _cprsStatus = pStatus
    }

-- | If the preset settings don\'t comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don\'t meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
cprsWarning :: Lens' CreatePresetResponse (Maybe Text)
cprsWarning = lens _cprsWarning (\ s a -> s{_cprsWarning = a});

-- | A section of the response body that provides information about the
-- preset that is created.
cprsPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprsPreset = lens _cprsPreset (\ s a -> s{_cprsPreset = a});

-- | FIXME: Undocumented member.
cprsStatus :: Lens' CreatePresetResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});
