{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264FilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import Network.AWS.Prelude

-- | H264 Filter Settings
--
-- /See:/ 'h264FilterSettings' smart constructor.
newtype H264FilterSettings = H264FilterSettings'
  { _hTemporalFilterSettings ::
      Maybe TemporalFilterSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264FilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hTemporalFilterSettings' - Undocumented member.
h264FilterSettings ::
  H264FilterSettings
h264FilterSettings =
  H264FilterSettings' {_hTemporalFilterSettings = Nothing}

-- | Undocumented member.
hTemporalFilterSettings :: Lens' H264FilterSettings (Maybe TemporalFilterSettings)
hTemporalFilterSettings = lens _hTemporalFilterSettings (\s a -> s {_hTemporalFilterSettings = a})

instance FromJSON H264FilterSettings where
  parseJSON =
    withObject
      "H264FilterSettings"
      (\x -> H264FilterSettings' <$> (x .:? "temporalFilterSettings"))

instance Hashable H264FilterSettings

instance NFData H264FilterSettings

instance ToJSON H264FilterSettings where
  toJSON H264FilterSettings' {..} =
    object
      ( catMaybes
          [("temporalFilterSettings" .=) <$> _hTemporalFilterSettings]
      )
