{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayState where

import Network.AWS.Prelude

data ReplayState
  = Cancelled
  | Cancelling
  | Completed
  | Failed
  | Running
  | Starting
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

instance FromText ReplayState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "cancelling" -> pure Cancelling
      "completed" -> pure Completed
      "failed" -> pure Failed
      "running" -> pure Running
      "starting" -> pure Starting
      e ->
        fromTextError $
          "Failure parsing ReplayState from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, completed, failed, running, starting"

instance ToText ReplayState where
  toText = \case
    Cancelled -> "CANCELLED"
    Cancelling -> "CANCELLING"
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    Running -> "RUNNING"
    Starting -> "STARTING"

instance Hashable ReplayState

instance NFData ReplayState

instance ToByteString ReplayState

instance ToQuery ReplayState

instance ToHeader ReplayState

instance ToJSON ReplayState where
  toJSON = toJSONText

instance FromJSON ReplayState where
  parseJSON = parseJSONText "ReplayState"
