{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'hlsTimedMetadataScheduleActionSettings' smart constructor.
newtype HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'
  { _htmsasId3 ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'HlsTimedMetadataScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'htmsasId3' - Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
hlsTimedMetadataScheduleActionSettings ::
  -- | 'htmsasId3'
  Text ->
  HlsTimedMetadataScheduleActionSettings
hlsTimedMetadataScheduleActionSettings pId3_ =
  HlsTimedMetadataScheduleActionSettings' {_htmsasId3 = pId3_}

-- | Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
htmsasId3 :: Lens' HlsTimedMetadataScheduleActionSettings Text
htmsasId3 = lens _htmsasId3 (\s a -> s {_htmsasId3 = a})

instance FromJSON HlsTimedMetadataScheduleActionSettings where
  parseJSON =
    withObject
      "HlsTimedMetadataScheduleActionSettings"
      (\x -> HlsTimedMetadataScheduleActionSettings' <$> (x .: "id3"))

instance Hashable HlsTimedMetadataScheduleActionSettings

instance NFData HlsTimedMetadataScheduleActionSettings

instance ToJSON HlsTimedMetadataScheduleActionSettings where
  toJSON HlsTimedMetadataScheduleActionSettings' {..} =
    object (catMaybes [Just ("id3" .= _htmsasId3)])
