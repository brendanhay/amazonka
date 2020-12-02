{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2FilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import Network.AWS.Prelude

-- | Mpeg2 Filter Settings
--
-- /See:/ 'mpeg2FilterSettings' smart constructor.
newtype Mpeg2FilterSettings = Mpeg2FilterSettings'
  { _mfsTemporalFilterSettings ::
      Maybe TemporalFilterSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mpeg2FilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfsTemporalFilterSettings' - Undocumented member.
mpeg2FilterSettings ::
  Mpeg2FilterSettings
mpeg2FilterSettings =
  Mpeg2FilterSettings' {_mfsTemporalFilterSettings = Nothing}

-- | Undocumented member.
mfsTemporalFilterSettings :: Lens' Mpeg2FilterSettings (Maybe TemporalFilterSettings)
mfsTemporalFilterSettings = lens _mfsTemporalFilterSettings (\s a -> s {_mfsTemporalFilterSettings = a})

instance FromJSON Mpeg2FilterSettings where
  parseJSON =
    withObject
      "Mpeg2FilterSettings"
      (\x -> Mpeg2FilterSettings' <$> (x .:? "temporalFilterSettings"))

instance Hashable Mpeg2FilterSettings

instance NFData Mpeg2FilterSettings

instance ToJSON Mpeg2FilterSettings where
  toJSON Mpeg2FilterSettings' {..} =
    object
      ( catMaybes
          [("temporalFilterSettings" .=) <$> _mfsTemporalFilterSettings]
      )
