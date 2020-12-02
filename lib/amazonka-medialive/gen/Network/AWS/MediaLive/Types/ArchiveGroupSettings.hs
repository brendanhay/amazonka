{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.Prelude

-- | Archive Group Settings
--
-- /See:/ 'archiveGroupSettings' smart constructor.
data ArchiveGroupSettings = ArchiveGroupSettings'
  { _agsRolloverInterval ::
      !(Maybe Nat),
    _agsDestination :: !OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArchiveGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agsRolloverInterval' - Number of seconds to write to archive file before closing and starting a new one.
--
-- * 'agsDestination' - A directory and base filename where archive files should be written.
archiveGroupSettings ::
  -- | 'agsDestination'
  OutputLocationRef ->
  ArchiveGroupSettings
archiveGroupSettings pDestination_ =
  ArchiveGroupSettings'
    { _agsRolloverInterval = Nothing,
      _agsDestination = pDestination_
    }

-- | Number of seconds to write to archive file before closing and starting a new one.
agsRolloverInterval :: Lens' ArchiveGroupSettings (Maybe Natural)
agsRolloverInterval = lens _agsRolloverInterval (\s a -> s {_agsRolloverInterval = a}) . mapping _Nat

-- | A directory and base filename where archive files should be written.
agsDestination :: Lens' ArchiveGroupSettings OutputLocationRef
agsDestination = lens _agsDestination (\s a -> s {_agsDestination = a})

instance FromJSON ArchiveGroupSettings where
  parseJSON =
    withObject
      "ArchiveGroupSettings"
      ( \x ->
          ArchiveGroupSettings'
            <$> (x .:? "rolloverInterval") <*> (x .: "destination")
      )

instance Hashable ArchiveGroupSettings

instance NFData ArchiveGroupSettings

instance ToJSON ArchiveGroupSettings where
  toJSON ArchiveGroupSettings' {..} =
    object
      ( catMaybes
          [ ("rolloverInterval" .=) <$> _agsRolloverInterval,
            Just ("destination" .= _agsDestination)
          ]
      )
