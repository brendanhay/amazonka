{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionStatus where

import Network.AWS.Prelude

data AutomationExecutionStatus
  = AESCancelled
  | AESCancelling
  | AESFailed
  | AESInProgress
  | AESPending
  | AESSuccess
  | AESTimedOut
  | AESWaiting
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

instance FromText AutomationExecutionStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure AESCancelled
      "cancelling" -> pure AESCancelling
      "failed" -> pure AESFailed
      "inprogress" -> pure AESInProgress
      "pending" -> pure AESPending
      "success" -> pure AESSuccess
      "timedout" -> pure AESTimedOut
      "waiting" -> pure AESWaiting
      e ->
        fromTextError $
          "Failure parsing AutomationExecutionStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout, waiting"

instance ToText AutomationExecutionStatus where
  toText = \case
    AESCancelled -> "Cancelled"
    AESCancelling -> "Cancelling"
    AESFailed -> "Failed"
    AESInProgress -> "InProgress"
    AESPending -> "Pending"
    AESSuccess -> "Success"
    AESTimedOut -> "TimedOut"
    AESWaiting -> "Waiting"

instance Hashable AutomationExecutionStatus

instance NFData AutomationExecutionStatus

instance ToByteString AutomationExecutionStatus

instance ToQuery AutomationExecutionStatus

instance ToHeader AutomationExecutionStatus

instance FromJSON AutomationExecutionStatus where
  parseJSON = parseJSONText "AutomationExecutionStatus"
