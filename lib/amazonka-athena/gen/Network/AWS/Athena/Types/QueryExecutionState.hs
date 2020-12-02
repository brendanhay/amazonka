{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionState where

import Network.AWS.Prelude

data QueryExecutionState
  = Cancelled
  | Failed
  | Queued
  | Running
  | Succeeded
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

instance FromText QueryExecutionState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "failed" -> pure Failed
      "queued" -> pure Queued
      "running" -> pure Running
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing QueryExecutionState from value: '" <> e
            <> "'. Accepted values: cancelled, failed, queued, running, succeeded"

instance ToText QueryExecutionState where
  toText = \case
    Cancelled -> "CANCELLED"
    Failed -> "FAILED"
    Queued -> "QUEUED"
    Running -> "RUNNING"
    Succeeded -> "SUCCEEDED"

instance Hashable QueryExecutionState

instance NFData QueryExecutionState

instance ToByteString QueryExecutionState

instance ToQuery QueryExecutionState

instance ToHeader QueryExecutionState

instance FromJSON QueryExecutionState where
  parseJSON = parseJSONText "QueryExecutionState"
