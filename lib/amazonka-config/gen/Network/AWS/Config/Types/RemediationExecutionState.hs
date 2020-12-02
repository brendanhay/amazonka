{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionState where

import Network.AWS.Prelude

data RemediationExecutionState
  = RESFailed
  | RESInProgress
  | RESQueued
  | RESSucceeded
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

instance FromText RemediationExecutionState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure RESFailed
      "in_progress" -> pure RESInProgress
      "queued" -> pure RESQueued
      "succeeded" -> pure RESSucceeded
      e ->
        fromTextError $
          "Failure parsing RemediationExecutionState from value: '" <> e
            <> "'. Accepted values: failed, in_progress, queued, succeeded"

instance ToText RemediationExecutionState where
  toText = \case
    RESFailed -> "FAILED"
    RESInProgress -> "IN_PROGRESS"
    RESQueued -> "QUEUED"
    RESSucceeded -> "SUCCEEDED"

instance Hashable RemediationExecutionState

instance NFData RemediationExecutionState

instance ToByteString RemediationExecutionState

instance ToQuery RemediationExecutionState

instance ToHeader RemediationExecutionState

instance FromJSON RemediationExecutionState where
  parseJSON = parseJSONText "RemediationExecutionState"
