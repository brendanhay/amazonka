{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPluginStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandPluginStatus where

import Network.AWS.Prelude

data CommandPluginStatus
  = CPSCancelled
  | CPSFailed
  | CPSInProgress
  | CPSPending
  | CPSSuccess
  | CPSTimedOut
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

instance FromText CommandPluginStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure CPSCancelled
      "failed" -> pure CPSFailed
      "inprogress" -> pure CPSInProgress
      "pending" -> pure CPSPending
      "success" -> pure CPSSuccess
      "timedout" -> pure CPSTimedOut
      e ->
        fromTextError $
          "Failure parsing CommandPluginStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, inprogress, pending, success, timedout"

instance ToText CommandPluginStatus where
  toText = \case
    CPSCancelled -> "Cancelled"
    CPSFailed -> "Failed"
    CPSInProgress -> "InProgress"
    CPSPending -> "Pending"
    CPSSuccess -> "Success"
    CPSTimedOut -> "TimedOut"

instance Hashable CommandPluginStatus

instance NFData CommandPluginStatus

instance ToByteString CommandPluginStatus

instance ToQuery CommandPluginStatus

instance ToHeader CommandPluginStatus

instance FromJSON CommandPluginStatus where
  parseJSON = parseJSONText "CommandPluginStatus"
