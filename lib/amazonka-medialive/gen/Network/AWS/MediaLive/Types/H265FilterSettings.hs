{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import Network.AWS.Prelude

-- | H265 Filter Settings
--
-- /See:/ 'h265FilterSettings' smart constructor.
newtype H265FilterSettings = H265FilterSettings'
  { _hfsTemporalFilterSettings ::
      Maybe TemporalFilterSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265FilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hfsTemporalFilterSettings' - Undocumented member.
h265FilterSettings ::
  H265FilterSettings
h265FilterSettings =
  H265FilterSettings' {_hfsTemporalFilterSettings = Nothing}

-- | Undocumented member.
hfsTemporalFilterSettings :: Lens' H265FilterSettings (Maybe TemporalFilterSettings)
hfsTemporalFilterSettings = lens _hfsTemporalFilterSettings (\s a -> s {_hfsTemporalFilterSettings = a})

instance FromJSON H265FilterSettings where
  parseJSON =
    withObject
      "H265FilterSettings"
      (\x -> H265FilterSettings' <$> (x .:? "temporalFilterSettings"))

instance Hashable H265FilterSettings

instance NFData H265FilterSettings

instance ToJSON H265FilterSettings where
  toJSON H265FilterSettings' {..} =
    object
      ( catMaybes
          [("temporalFilterSettings" .=) <$> _hfsTemporalFilterSettings]
      )
