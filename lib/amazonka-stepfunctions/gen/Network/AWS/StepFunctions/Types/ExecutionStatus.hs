{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionStatus where

import Network.AWS.Prelude

data ExecutionStatus
  = Aborted
  | Failed
  | Running
  | Succeeded
  | TimedOut
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

instance FromText ExecutionStatus where
  parser =
    takeLowerText >>= \case
      "aborted" -> pure Aborted
      "failed" -> pure Failed
      "running" -> pure Running
      "succeeded" -> pure Succeeded
      "timed_out" -> pure TimedOut
      e ->
        fromTextError $
          "Failure parsing ExecutionStatus from value: '" <> e
            <> "'. Accepted values: aborted, failed, running, succeeded, timed_out"

instance ToText ExecutionStatus where
  toText = \case
    Aborted -> "ABORTED"
    Failed -> "FAILED"
    Running -> "RUNNING"
    Succeeded -> "SUCCEEDED"
    TimedOut -> "TIMED_OUT"

instance Hashable ExecutionStatus

instance NFData ExecutionStatus

instance ToByteString ExecutionStatus

instance ToQuery ExecutionStatus

instance ToHeader ExecutionStatus

instance ToJSON ExecutionStatus where
  toJSON = toJSONText

instance FromJSON ExecutionStatus where
  parseJSON = parseJSONText "ExecutionStatus"
