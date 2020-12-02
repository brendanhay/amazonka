{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepState where

import Network.AWS.Prelude

data StepState
  = SSCancelPending
  | SSCancelled
  | SSCompleted
  | SSFailed
  | SSInterrupted
  | SSPending
  | SSRunning
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

instance FromText StepState where
  parser =
    takeLowerText >>= \case
      "cancel_pending" -> pure SSCancelPending
      "cancelled" -> pure SSCancelled
      "completed" -> pure SSCompleted
      "failed" -> pure SSFailed
      "interrupted" -> pure SSInterrupted
      "pending" -> pure SSPending
      "running" -> pure SSRunning
      e ->
        fromTextError $
          "Failure parsing StepState from value: '" <> e
            <> "'. Accepted values: cancel_pending, cancelled, completed, failed, interrupted, pending, running"

instance ToText StepState where
  toText = \case
    SSCancelPending -> "CANCEL_PENDING"
    SSCancelled -> "CANCELLED"
    SSCompleted -> "COMPLETED"
    SSFailed -> "FAILED"
    SSInterrupted -> "INTERRUPTED"
    SSPending -> "PENDING"
    SSRunning -> "RUNNING"

instance Hashable StepState

instance NFData StepState

instance ToByteString StepState

instance ToQuery StepState

instance ToHeader StepState

instance ToJSON StepState where
  toJSON = toJSONText

instance FromJSON StepState where
  parseJSON = parseJSONText "StepState"
