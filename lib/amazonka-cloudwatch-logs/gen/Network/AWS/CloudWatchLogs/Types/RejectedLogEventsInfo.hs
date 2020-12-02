{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the rejected events.
--
--
--
-- /See:/ 'rejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
  { _rleiTooOldLogEventEndIndex ::
      !(Maybe Int),
    _rleiTooNewLogEventStartIndex :: !(Maybe Int),
    _rleiExpiredLogEventEndIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RejectedLogEventsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rleiTooOldLogEventEndIndex' - The log events that are too old.
--
-- * 'rleiTooNewLogEventStartIndex' - The log events that are too new.
--
-- * 'rleiExpiredLogEventEndIndex' - The expired log events.
rejectedLogEventsInfo ::
  RejectedLogEventsInfo
rejectedLogEventsInfo =
  RejectedLogEventsInfo'
    { _rleiTooOldLogEventEndIndex = Nothing,
      _rleiTooNewLogEventStartIndex = Nothing,
      _rleiExpiredLogEventEndIndex = Nothing
    }

-- | The log events that are too old.
rleiTooOldLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooOldLogEventEndIndex = lens _rleiTooOldLogEventEndIndex (\s a -> s {_rleiTooOldLogEventEndIndex = a})

-- | The log events that are too new.
rleiTooNewLogEventStartIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooNewLogEventStartIndex = lens _rleiTooNewLogEventStartIndex (\s a -> s {_rleiTooNewLogEventStartIndex = a})

-- | The expired log events.
rleiExpiredLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiExpiredLogEventEndIndex = lens _rleiExpiredLogEventEndIndex (\s a -> s {_rleiExpiredLogEventEndIndex = a})

instance FromJSON RejectedLogEventsInfo where
  parseJSON =
    withObject
      "RejectedLogEventsInfo"
      ( \x ->
          RejectedLogEventsInfo'
            <$> (x .:? "tooOldLogEventEndIndex")
            <*> (x .:? "tooNewLogEventStartIndex")
            <*> (x .:? "expiredLogEventEndIndex")
      )

instance Hashable RejectedLogEventsInfo

instance NFData RejectedLogEventsInfo
