{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationStatus where

import Network.AWS.Prelude

data StackSetOperationStatus
  = SSOSFailed
  | SSOSQueued
  | SSOSRunning
  | SSOSStopped
  | SSOSStopping
  | SSOSSucceeded
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

instance FromText StackSetOperationStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure SSOSFailed
      "queued" -> pure SSOSQueued
      "running" -> pure SSOSRunning
      "stopped" -> pure SSOSStopped
      "stopping" -> pure SSOSStopping
      "succeeded" -> pure SSOSSucceeded
      e ->
        fromTextError $
          "Failure parsing StackSetOperationStatus from value: '" <> e
            <> "'. Accepted values: failed, queued, running, stopped, stopping, succeeded"

instance ToText StackSetOperationStatus where
  toText = \case
    SSOSFailed -> "FAILED"
    SSOSQueued -> "QUEUED"
    SSOSRunning -> "RUNNING"
    SSOSStopped -> "STOPPED"
    SSOSStopping -> "STOPPING"
    SSOSSucceeded -> "SUCCEEDED"

instance Hashable StackSetOperationStatus

instance NFData StackSetOperationStatus

instance ToByteString StackSetOperationStatus

instance ToQuery StackSetOperationStatus

instance ToHeader StackSetOperationStatus

instance FromXML StackSetOperationStatus where
  parseXML = parseXMLText "StackSetOperationStatus"
