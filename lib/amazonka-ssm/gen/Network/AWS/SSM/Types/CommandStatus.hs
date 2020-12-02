{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandStatus where

import Network.AWS.Prelude

data CommandStatus
  = CSCancelled
  | CSCancelling
  | CSFailed
  | CSInProgress
  | CSPending
  | CSSuccess
  | CSTimedOut
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

instance FromText CommandStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure CSCancelled
      "cancelling" -> pure CSCancelling
      "failed" -> pure CSFailed
      "inprogress" -> pure CSInProgress
      "pending" -> pure CSPending
      "success" -> pure CSSuccess
      "timedout" -> pure CSTimedOut
      e ->
        fromTextError $
          "Failure parsing CommandStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout"

instance ToText CommandStatus where
  toText = \case
    CSCancelled -> "Cancelled"
    CSCancelling -> "Cancelling"
    CSFailed -> "Failed"
    CSInProgress -> "InProgress"
    CSPending -> "Pending"
    CSSuccess -> "Success"
    CSTimedOut -> "TimedOut"

instance Hashable CommandStatus

instance NFData CommandStatus

instance ToByteString CommandStatus

instance ToQuery CommandStatus

instance ToHeader CommandStatus

instance FromJSON CommandStatus where
  parseJSON = parseJSONText "CommandStatus"
