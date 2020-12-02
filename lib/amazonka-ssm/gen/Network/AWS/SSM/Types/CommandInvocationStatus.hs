{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandInvocationStatus where

import Network.AWS.Prelude

data CommandInvocationStatus
  = CISCancelled
  | CISCancelling
  | CISDelayed
  | CISFailed
  | CISInProgress
  | CISPending
  | CISSuccess
  | CISTimedOut
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

instance FromText CommandInvocationStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure CISCancelled
      "cancelling" -> pure CISCancelling
      "delayed" -> pure CISDelayed
      "failed" -> pure CISFailed
      "inprogress" -> pure CISInProgress
      "pending" -> pure CISPending
      "success" -> pure CISSuccess
      "timedout" -> pure CISTimedOut
      e ->
        fromTextError $
          "Failure parsing CommandInvocationStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, delayed, failed, inprogress, pending, success, timedout"

instance ToText CommandInvocationStatus where
  toText = \case
    CISCancelled -> "Cancelled"
    CISCancelling -> "Cancelling"
    CISDelayed -> "Delayed"
    CISFailed -> "Failed"
    CISInProgress -> "InProgress"
    CISPending -> "Pending"
    CISSuccess -> "Success"
    CISTimedOut -> "TimedOut"

instance Hashable CommandInvocationStatus

instance NFData CommandInvocationStatus

instance ToByteString CommandInvocationStatus

instance ToQuery CommandInvocationStatus

instance ToHeader CommandInvocationStatus

instance FromJSON CommandInvocationStatus where
  parseJSON = parseJSONText "CommandInvocationStatus"
