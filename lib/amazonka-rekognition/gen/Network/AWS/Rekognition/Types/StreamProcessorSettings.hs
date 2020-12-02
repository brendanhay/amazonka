{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.FaceSearchSettings

-- | Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.
--
--
--
-- /See:/ 'streamProcessorSettings' smart constructor.
newtype StreamProcessorSettings = StreamProcessorSettings'
  { _spsFaceSearch ::
      Maybe FaceSearchSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamProcessorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsFaceSearch' - Face search settings to use on a streaming video.
streamProcessorSettings ::
  StreamProcessorSettings
streamProcessorSettings =
  StreamProcessorSettings' {_spsFaceSearch = Nothing}

-- | Face search settings to use on a streaming video.
spsFaceSearch :: Lens' StreamProcessorSettings (Maybe FaceSearchSettings)
spsFaceSearch = lens _spsFaceSearch (\s a -> s {_spsFaceSearch = a})

instance FromJSON StreamProcessorSettings where
  parseJSON =
    withObject
      "StreamProcessorSettings"
      (\x -> StreamProcessorSettings' <$> (x .:? "FaceSearch"))

instance Hashable StreamProcessorSettings

instance NFData StreamProcessorSettings

instance ToJSON StreamProcessorSettings where
  toJSON StreamProcessorSettings' {..} =
    object (catMaybes [("FaceSearch" .=) <$> _spsFaceSearch])
