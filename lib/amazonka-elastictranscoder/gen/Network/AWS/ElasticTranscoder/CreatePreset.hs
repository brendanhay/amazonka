{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePreset operation creates a preset with settings that you specify.
--
--
-- /Important:/ Elastic Transcoder checks the CreatePreset settings to ensure that they meet Elastic Transcoder requirements and to determine whether they comply with H.264 standards. If your settings are not valid for Elastic Transcoder, Elastic Transcoder returns an HTTP 400 response (@ValidationException@ ) and does not create the preset. If the settings are valid for Elastic Transcoder but aren't strictly compliant with the H.264 standard, Elastic Transcoder creates the preset and returns a warning message in the response. This helps you determine whether your settings comply with the H.264 standard while giving you greater flexibility with respect to the video that Elastic Transcoder produces.
--
-- Elastic Transcoder uses the H.264 video-compression format. For more information, see the International Telecommunication Union publication /Recommendation ITU-T H.264: Advanced video coding for generic audiovisual services/ .
--
module Network.AWS.ElasticTranscoder.CreatePreset
    (
    -- * Creating a Request
      createPreset
    , CreatePreset
    -- * Request Lenses
    , cpVideo
    , cpThumbnails
    , cpDescription
    , cpAudio
    , cpName
    , cpContainer

    -- * Destructuring the Response
    , createPresetResponse
    , CreatePresetResponse
    -- * Response Lenses
    , cprsWarning
    , cprsPreset
    , cprsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @CreatePresetRequest@ structure.
--
--
--
-- /See:/ 'createPreset' smart constructor.
data CreatePreset = CreatePreset'
  { _cpVideo       :: !(Maybe VideoParameters)
  , _cpThumbnails  :: !(Maybe Thumbnails)
  , _cpDescription :: !(Maybe Text)
  , _cpAudio       :: !(Maybe AudioParameters)
  , _cpName        :: !Text
  , _cpContainer   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpVideo' - A section of the request body that specifies the video parameters.
--
-- * 'cpThumbnails' - A section of the request body that specifies the thumbnail parameters, if any.
--
-- * 'cpDescription' - A description of the preset.
--
-- * 'cpAudio' - A section of the request body that specifies the audio parameters.
--
-- * 'cpName' - The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- * 'cpContainer' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
createPreset
    :: Text -- ^ 'cpName'
    -> Text -- ^ 'cpContainer'
    -> CreatePreset
createPreset pName_ pContainer_ =
  CreatePreset'
    { _cpVideo = Nothing
    , _cpThumbnails = Nothing
    , _cpDescription = Nothing
    , _cpAudio = Nothing
    , _cpName = pName_
    , _cpContainer = pContainer_
    }


-- | A section of the request body that specifies the video parameters.
cpVideo :: Lens' CreatePreset (Maybe VideoParameters)
cpVideo = lens _cpVideo (\ s a -> s{_cpVideo = a})

-- | A section of the request body that specifies the thumbnail parameters, if any.
cpThumbnails :: Lens' CreatePreset (Maybe Thumbnails)
cpThumbnails = lens _cpThumbnails (\ s a -> s{_cpThumbnails = a})

-- | A description of the preset.
cpDescription :: Lens' CreatePreset (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a})

-- | A section of the request body that specifies the audio parameters.
cpAudio :: Lens' CreatePreset (Maybe AudioParameters)
cpAudio = lens _cpAudio (\ s a -> s{_cpAudio = a})

-- | The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
cpName :: Lens' CreatePreset Text
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
cpContainer :: Lens' CreatePreset Text
cpContainer = lens _cpContainer (\ s a -> s{_cpContainer = a})

instance AWSRequest CreatePreset where
        type Rs CreatePreset = CreatePresetResponse
        request = postJSON elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 CreatePresetResponse' <$>
                   (x .?> "Warning") <*> (x .?> "Preset") <*>
                     (pure (fromEnum s)))

instance Hashable CreatePreset where

instance NFData CreatePreset where

instance ToHeaders CreatePreset where
        toHeaders = const mempty

instance ToJSON CreatePreset where
        toJSON CreatePreset'{..}
          = object
              (catMaybes
                 [("Video" .=) <$> _cpVideo,
                  ("Thumbnails" .=) <$> _cpThumbnails,
                  ("Description" .=) <$> _cpDescription,
                  ("Audio" .=) <$> _cpAudio, Just ("Name" .= _cpName),
                  Just ("Container" .= _cpContainer)])

instance ToPath CreatePreset where
        toPath = const "/2012-09-25/presets"

instance ToQuery CreatePreset where
        toQuery = const mempty

-- | The @CreatePresetResponse@ structure.
--
--
--
-- /See:/ 'createPresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { _cprsWarning        :: !(Maybe Text)
  , _cprsPreset         :: !(Maybe Preset)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePresetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsWarning' - If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
--
-- * 'cprsPreset' - A section of the response body that provides information about the preset that is created.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPresetResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePresetResponse
createPresetResponse pResponseStatus_ =
  CreatePresetResponse'
    { _cprsWarning = Nothing
    , _cprsPreset = Nothing
    , _cprsResponseStatus = pResponseStatus_
    }


-- | If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
cprsWarning :: Lens' CreatePresetResponse (Maybe Text)
cprsWarning = lens _cprsWarning (\ s a -> s{_cprsWarning = a})

-- | A section of the response body that provides information about the preset that is created.
cprsPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprsPreset = lens _cprsPreset (\ s a -> s{_cprsPreset = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePresetResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePresetResponse where
