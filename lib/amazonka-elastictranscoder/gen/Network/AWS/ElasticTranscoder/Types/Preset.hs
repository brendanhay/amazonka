{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Preset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Preset where

import Network.AWS.ElasticTranscoder.Types.AudioParameters
import Network.AWS.ElasticTranscoder.Types.Thumbnails
import Network.AWS.ElasticTranscoder.Types.VideoParameters
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.
--
--
--
-- /See:/ 'preset' smart constructor.
data Preset = Preset'
  { _preARN :: !(Maybe Text),
    _preVideo :: !(Maybe VideoParameters),
    _preThumbnails :: !(Maybe Thumbnails),
    _preName :: !(Maybe Text),
    _preContainer :: !(Maybe Text),
    _preId :: !(Maybe Text),
    _preType :: !(Maybe Text),
    _preDescription :: !(Maybe Text),
    _preAudio :: !(Maybe AudioParameters)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'preARN' - The Amazon Resource Name (ARN) for the preset.
--
-- * 'preVideo' - A section of the response body that provides information about the video preset values.
--
-- * 'preThumbnails' - A section of the response body that provides information about the thumbnail preset values, if any.
--
-- * 'preName' - The name of the preset.
--
-- * 'preContainer' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- * 'preId' - Identifier for the new preset. You use this value to get settings for the preset or to delete it.
--
-- * 'preType' - Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
--
-- * 'preDescription' - A description of the preset.
--
-- * 'preAudio' - A section of the response body that provides information about the audio preset values.
preset ::
  Preset
preset =
  Preset'
    { _preARN = Nothing,
      _preVideo = Nothing,
      _preThumbnails = Nothing,
      _preName = Nothing,
      _preContainer = Nothing,
      _preId = Nothing,
      _preType = Nothing,
      _preDescription = Nothing,
      _preAudio = Nothing
    }

-- | The Amazon Resource Name (ARN) for the preset.
preARN :: Lens' Preset (Maybe Text)
preARN = lens _preARN (\s a -> s {_preARN = a})

-- | A section of the response body that provides information about the video preset values.
preVideo :: Lens' Preset (Maybe VideoParameters)
preVideo = lens _preVideo (\s a -> s {_preVideo = a})

-- | A section of the response body that provides information about the thumbnail preset values, if any.
preThumbnails :: Lens' Preset (Maybe Thumbnails)
preThumbnails = lens _preThumbnails (\s a -> s {_preThumbnails = a})

-- | The name of the preset.
preName :: Lens' Preset (Maybe Text)
preName = lens _preName (\s a -> s {_preName = a})

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
preContainer :: Lens' Preset (Maybe Text)
preContainer = lens _preContainer (\s a -> s {_preContainer = a})

-- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
preId :: Lens' Preset (Maybe Text)
preId = lens _preId (\s a -> s {_preId = a})

-- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
preType :: Lens' Preset (Maybe Text)
preType = lens _preType (\s a -> s {_preType = a})

-- | A description of the preset.
preDescription :: Lens' Preset (Maybe Text)
preDescription = lens _preDescription (\s a -> s {_preDescription = a})

-- | A section of the response body that provides information about the audio preset values.
preAudio :: Lens' Preset (Maybe AudioParameters)
preAudio = lens _preAudio (\s a -> s {_preAudio = a})

instance FromJSON Preset where
  parseJSON =
    withObject
      "Preset"
      ( \x ->
          Preset'
            <$> (x .:? "Arn")
            <*> (x .:? "Video")
            <*> (x .:? "Thumbnails")
            <*> (x .:? "Name")
            <*> (x .:? "Container")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
            <*> (x .:? "Audio")
      )

instance Hashable Preset

instance NFData Preset
