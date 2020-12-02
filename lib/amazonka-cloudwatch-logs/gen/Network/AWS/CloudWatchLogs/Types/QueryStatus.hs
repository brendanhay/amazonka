{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatus where

import Network.AWS.Prelude

data QueryStatus
  = Cancelled
  | Complete
  | Failed
  | Running
  | Scheduled
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

instance FromText QueryStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "complete" -> pure Complete
      "failed" -> pure Failed
      "running" -> pure Running
      "scheduled" -> pure Scheduled
      e ->
        fromTextError $
          "Failure parsing QueryStatus from value: '" <> e
            <> "'. Accepted values: cancelled, complete, failed, running, scheduled"

instance ToText QueryStatus where
  toText = \case
    Cancelled -> "Cancelled"
    Complete -> "Complete"
    Failed -> "Failed"
    Running -> "Running"
    Scheduled -> "Scheduled"

instance Hashable QueryStatus

instance NFData QueryStatus

instance ToByteString QueryStatus

instance ToQuery QueryStatus

instance ToHeader QueryStatus

instance ToJSON QueryStatus where
  toJSON = toJSONText

instance FromJSON QueryStatus where
  parseJSON = parseJSONText "QueryStatus"
