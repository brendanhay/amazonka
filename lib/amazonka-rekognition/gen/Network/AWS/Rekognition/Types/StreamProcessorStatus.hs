{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorStatus where

import Network.AWS.Prelude

data StreamProcessorStatus
  = SPSFailed
  | SPSRunning
  | SPSStarting
  | SPSStopped
  | SPSStopping
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

instance FromText StreamProcessorStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure SPSFailed
      "running" -> pure SPSRunning
      "starting" -> pure SPSStarting
      "stopped" -> pure SPSStopped
      "stopping" -> pure SPSStopping
      e ->
        fromTextError $
          "Failure parsing StreamProcessorStatus from value: '" <> e
            <> "'. Accepted values: failed, running, starting, stopped, stopping"

instance ToText StreamProcessorStatus where
  toText = \case
    SPSFailed -> "FAILED"
    SPSRunning -> "RUNNING"
    SPSStarting -> "STARTING"
    SPSStopped -> "STOPPED"
    SPSStopping -> "STOPPING"

instance Hashable StreamProcessorStatus

instance NFData StreamProcessorStatus

instance ToByteString StreamProcessorStatus

instance ToQuery StreamProcessorStatus

instance ToHeader StreamProcessorStatus

instance FromJSON StreamProcessorStatus where
  parseJSON = parseJSONText "StreamProcessorStatus"
