{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionStatus where

import Network.AWS.Prelude

data ActionStatus
  = ExecutionFailure
  | ExecutionInProgress
  | ExecutionSuccess
  | Pending
  | ResetFailure
  | ResetInProgress
  | ReverseFailure
  | ReverseInProgress
  | ReverseSuccess
  | Standby
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

instance FromText ActionStatus where
  parser =
    takeLowerText >>= \case
      "execution_failure" -> pure ExecutionFailure
      "execution_in_progress" -> pure ExecutionInProgress
      "execution_success" -> pure ExecutionSuccess
      "pending" -> pure Pending
      "reset_failure" -> pure ResetFailure
      "reset_in_progress" -> pure ResetInProgress
      "reverse_failure" -> pure ReverseFailure
      "reverse_in_progress" -> pure ReverseInProgress
      "reverse_success" -> pure ReverseSuccess
      "standby" -> pure Standby
      e ->
        fromTextError $
          "Failure parsing ActionStatus from value: '" <> e
            <> "'. Accepted values: execution_failure, execution_in_progress, execution_success, pending, reset_failure, reset_in_progress, reverse_failure, reverse_in_progress, reverse_success, standby"

instance ToText ActionStatus where
  toText = \case
    ExecutionFailure -> "EXECUTION_FAILURE"
    ExecutionInProgress -> "EXECUTION_IN_PROGRESS"
    ExecutionSuccess -> "EXECUTION_SUCCESS"
    Pending -> "PENDING"
    ResetFailure -> "RESET_FAILURE"
    ResetInProgress -> "RESET_IN_PROGRESS"
    ReverseFailure -> "REVERSE_FAILURE"
    ReverseInProgress -> "REVERSE_IN_PROGRESS"
    ReverseSuccess -> "REVERSE_SUCCESS"
    Standby -> "STANDBY"

instance Hashable ActionStatus

instance NFData ActionStatus

instance ToByteString ActionStatus

instance ToQuery ActionStatus

instance ToHeader ActionStatus

instance FromJSON ActionStatus where
  parseJSON = parseJSONText "ActionStatus"
