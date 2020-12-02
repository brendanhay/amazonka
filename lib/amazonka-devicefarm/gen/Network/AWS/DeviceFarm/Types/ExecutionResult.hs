{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionResult where

import Network.AWS.Prelude

data ExecutionResult
  = ERErrored
  | ERFailed
  | ERPassed
  | ERPending
  | ERSkipped
  | ERStopped
  | ERWarned
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

instance FromText ExecutionResult where
  parser =
    takeLowerText >>= \case
      "errored" -> pure ERErrored
      "failed" -> pure ERFailed
      "passed" -> pure ERPassed
      "pending" -> pure ERPending
      "skipped" -> pure ERSkipped
      "stopped" -> pure ERStopped
      "warned" -> pure ERWarned
      e ->
        fromTextError $
          "Failure parsing ExecutionResult from value: '" <> e
            <> "'. Accepted values: errored, failed, passed, pending, skipped, stopped, warned"

instance ToText ExecutionResult where
  toText = \case
    ERErrored -> "ERRORED"
    ERFailed -> "FAILED"
    ERPassed -> "PASSED"
    ERPending -> "PENDING"
    ERSkipped -> "SKIPPED"
    ERStopped -> "STOPPED"
    ERWarned -> "WARNED"

instance Hashable ExecutionResult

instance NFData ExecutionResult

instance ToByteString ExecutionResult

instance ToQuery ExecutionResult

instance ToHeader ExecutionResult

instance FromJSON ExecutionResult where
  parseJSON = parseJSONText "ExecutionResult"
