{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatus where

import Network.AWS.Prelude

data TaskStatus
  = TSCancelled
  | TSCancelling
  | TSCompleted
  | TSFailed
  | TSInProgress
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

instance FromText TaskStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure TSCancelled
      "cancelling" -> pure TSCancelling
      "completed" -> pure TSCompleted
      "failed" -> pure TSFailed
      "inprogress" -> pure TSInProgress
      e ->
        fromTextError $
          "Failure parsing TaskStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, completed, failed, inprogress"

instance ToText TaskStatus where
  toText = \case
    TSCancelled -> "Cancelled"
    TSCancelling -> "Cancelling"
    TSCompleted -> "Completed"
    TSFailed -> "Failed"
    TSInProgress -> "InProgress"

instance Hashable TaskStatus

instance NFData TaskStatus

instance ToByteString TaskStatus

instance ToQuery TaskStatus

instance ToHeader TaskStatus

instance ToJSON TaskStatus where
  toJSON = toJSONText

instance FromJSON TaskStatus where
  parseJSON = parseJSONText "TaskStatus"
