{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfig where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TimecodeConfigSource
import Network.AWS.Prelude

-- | Timecode Config
--
-- /See:/ 'timecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { _tcSyncThreshold ::
      !(Maybe Nat),
    _tcSource :: !TimecodeConfigSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimecodeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcSyncThreshold' - Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
--
-- * 'tcSource' - Identifies the source for the timecode that will be associated with the events outputs. -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
timecodeConfig ::
  -- | 'tcSource'
  TimecodeConfigSource ->
  TimecodeConfig
timecodeConfig pSource_ =
  TimecodeConfig' {_tcSyncThreshold = Nothing, _tcSource = pSource_}

-- | Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
tcSyncThreshold :: Lens' TimecodeConfig (Maybe Natural)
tcSyncThreshold = lens _tcSyncThreshold (\s a -> s {_tcSyncThreshold = a}) . mapping _Nat

-- | Identifies the source for the timecode that will be associated with the events outputs. -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
tcSource :: Lens' TimecodeConfig TimecodeConfigSource
tcSource = lens _tcSource (\s a -> s {_tcSource = a})

instance FromJSON TimecodeConfig where
  parseJSON =
    withObject
      "TimecodeConfig"
      ( \x ->
          TimecodeConfig' <$> (x .:? "syncThreshold") <*> (x .: "source")
      )

instance Hashable TimecodeConfig

instance NFData TimecodeConfig

instance ToJSON TimecodeConfig where
  toJSON TimecodeConfig' {..} =
    object
      ( catMaybes
          [ ("syncThreshold" .=) <$> _tcSyncThreshold,
            Just ("source" .= _tcSource)
          ]
      )
