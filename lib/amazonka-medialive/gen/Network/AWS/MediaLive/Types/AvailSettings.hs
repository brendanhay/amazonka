{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
import Network.AWS.Prelude

-- | Avail Settings
--
-- /See:/ 'availSettings' smart constructor.
data AvailSettings = AvailSettings'
  { _asScte35SpliceInsert ::
      !(Maybe Scte35SpliceInsert),
    _asScte35TimeSignalApos :: !(Maybe Scte35TimeSignalApos)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asScte35SpliceInsert' - Undocumented member.
--
-- * 'asScte35TimeSignalApos' - Undocumented member.
availSettings ::
  AvailSettings
availSettings =
  AvailSettings'
    { _asScte35SpliceInsert = Nothing,
      _asScte35TimeSignalApos = Nothing
    }

-- | Undocumented member.
asScte35SpliceInsert :: Lens' AvailSettings (Maybe Scte35SpliceInsert)
asScte35SpliceInsert = lens _asScte35SpliceInsert (\s a -> s {_asScte35SpliceInsert = a})

-- | Undocumented member.
asScte35TimeSignalApos :: Lens' AvailSettings (Maybe Scte35TimeSignalApos)
asScte35TimeSignalApos = lens _asScte35TimeSignalApos (\s a -> s {_asScte35TimeSignalApos = a})

instance FromJSON AvailSettings where
  parseJSON =
    withObject
      "AvailSettings"
      ( \x ->
          AvailSettings'
            <$> (x .:? "scte35SpliceInsert") <*> (x .:? "scte35TimeSignalApos")
      )

instance Hashable AvailSettings

instance NFData AvailSettings

instance ToJSON AvailSettings where
  toJSON AvailSettings' {..} =
    object
      ( catMaybes
          [ ("scte35SpliceInsert" .=) <$> _asScte35SpliceInsert,
            ("scte35TimeSignalApos" .=) <$> _asScte35TimeSignalApos
          ]
      )
