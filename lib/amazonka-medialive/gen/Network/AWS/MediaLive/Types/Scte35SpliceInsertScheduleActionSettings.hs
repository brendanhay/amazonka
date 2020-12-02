{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for a SCTE-35 splice_insert message.
--
-- /See:/ 'scte35SpliceInsertScheduleActionSettings' smart constructor.
data Scte35SpliceInsertScheduleActionSettings = Scte35SpliceInsertScheduleActionSettings'
  { _ssisasDuration ::
      !( Maybe
           Nat
       ),
    _ssisasSpliceEventId ::
      !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35SpliceInsertScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssisasDuration' - Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
--
-- * 'ssisasSpliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
scte35SpliceInsertScheduleActionSettings ::
  -- | 'ssisasSpliceEventId'
  Natural ->
  Scte35SpliceInsertScheduleActionSettings
scte35SpliceInsertScheduleActionSettings pSpliceEventId_ =
  Scte35SpliceInsertScheduleActionSettings'
    { _ssisasDuration =
        Nothing,
      _ssisasSpliceEventId = _Nat # pSpliceEventId_
    }

-- | Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
ssisasDuration :: Lens' Scte35SpliceInsertScheduleActionSettings (Maybe Natural)
ssisasDuration = lens _ssisasDuration (\s a -> s {_ssisasDuration = a}) . mapping _Nat

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
ssisasSpliceEventId :: Lens' Scte35SpliceInsertScheduleActionSettings Natural
ssisasSpliceEventId = lens _ssisasSpliceEventId (\s a -> s {_ssisasSpliceEventId = a}) . _Nat

instance FromJSON Scte35SpliceInsertScheduleActionSettings where
  parseJSON =
    withObject
      "Scte35SpliceInsertScheduleActionSettings"
      ( \x ->
          Scte35SpliceInsertScheduleActionSettings'
            <$> (x .:? "duration") <*> (x .: "spliceEventId")
      )

instance Hashable Scte35SpliceInsertScheduleActionSettings

instance NFData Scte35SpliceInsertScheduleActionSettings

instance ToJSON Scte35SpliceInsertScheduleActionSettings where
  toJSON Scte35SpliceInsertScheduleActionSettings' {..} =
    object
      ( catMaybes
          [ ("duration" .=) <$> _ssisasDuration,
            Just ("spliceEventId" .= _ssisasSpliceEventId)
          ]
      )
