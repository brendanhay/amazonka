{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimeoutType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimeoutType where

import Network.AWS.Prelude

data ActivityTaskTimeoutType
  = ATTTHeartbeat
  | ATTTScheduleToClose
  | ATTTScheduleToStart
  | ATTTStartToClose
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ActivityTaskTimeoutType where
  parser =
    takeLowerText >>= \case
      "heartbeat" -> pure ATTTHeartbeat
      "schedule_to_close" -> pure ATTTScheduleToClose
      "schedule_to_start" -> pure ATTTScheduleToStart
      "start_to_close" -> pure ATTTStartToClose
      e ->
        fromTextError $
          "Failure parsing ActivityTaskTimeoutType from value: '" <> e
            <> "'. Accepted values: heartbeat, schedule_to_close, schedule_to_start, start_to_close"

instance ToText ActivityTaskTimeoutType where
  toText = \case
    ATTTHeartbeat -> "HEARTBEAT"
    ATTTScheduleToClose -> "SCHEDULE_TO_CLOSE"
    ATTTScheduleToStart -> "SCHEDULE_TO_START"
    ATTTStartToClose -> "START_TO_CLOSE"

instance Hashable ActivityTaskTimeoutType

instance NFData ActivityTaskTimeoutType

instance ToByteString ActivityTaskTimeoutType

instance ToQuery ActivityTaskTimeoutType

instance ToHeader ActivityTaskTimeoutType

instance FromJSON ActivityTaskTimeoutType where
  parseJSON = parseJSONText "ActivityTaskTimeoutType"
