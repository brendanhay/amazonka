{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleStatus where

import Network.AWS.Prelude

data ScheduleStatus
  = SSFailed
  | SSPending
  | SSScheduled
  | SSStopped
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

instance FromText ScheduleStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure SSFailed
      "pending" -> pure SSPending
      "scheduled" -> pure SSScheduled
      "stopped" -> pure SSStopped
      e ->
        fromTextError $
          "Failure parsing ScheduleStatus from value: '" <> e
            <> "'. Accepted values: failed, pending, scheduled, stopped"

instance ToText ScheduleStatus where
  toText = \case
    SSFailed -> "Failed"
    SSPending -> "Pending"
    SSScheduled -> "Scheduled"
    SSStopped -> "Stopped"

instance Hashable ScheduleStatus

instance NFData ScheduleStatus

instance ToByteString ScheduleStatus

instance ToQuery ScheduleStatus

instance ToHeader ScheduleStatus

instance ToJSON ScheduleStatus where
  toJSON = toJSONText

instance FromJSON ScheduleStatus where
  parseJSON = parseJSONText "ScheduleStatus"
