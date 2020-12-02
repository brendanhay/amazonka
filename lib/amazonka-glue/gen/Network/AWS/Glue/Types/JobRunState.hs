{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobRunState where

import Network.AWS.Prelude

data JobRunState
  = JRSFailed
  | JRSRunning
  | JRSStarting
  | JRSStopped
  | JRSStopping
  | JRSSucceeded
  | JRSTimeout
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

instance FromText JobRunState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure JRSFailed
      "running" -> pure JRSRunning
      "starting" -> pure JRSStarting
      "stopped" -> pure JRSStopped
      "stopping" -> pure JRSStopping
      "succeeded" -> pure JRSSucceeded
      "timeout" -> pure JRSTimeout
      e ->
        fromTextError $
          "Failure parsing JobRunState from value: '" <> e
            <> "'. Accepted values: failed, running, starting, stopped, stopping, succeeded, timeout"

instance ToText JobRunState where
  toText = \case
    JRSFailed -> "FAILED"
    JRSRunning -> "RUNNING"
    JRSStarting -> "STARTING"
    JRSStopped -> "STOPPED"
    JRSStopping -> "STOPPING"
    JRSSucceeded -> "SUCCEEDED"
    JRSTimeout -> "TIMEOUT"

instance Hashable JobRunState

instance NFData JobRunState

instance ToByteString JobRunState

instance ToQuery JobRunState

instance ToHeader JobRunState

instance ToJSON JobRunState where
  toJSON = toJSONText

instance FromJSON JobRunState where
  parseJSON = parseJSONText "JobRunState"
