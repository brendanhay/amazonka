{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionStatus where

import Network.AWS.Prelude

data UpdateActionStatus
  = Complete
  | InProgress
  | NotApplicable
  | NotApplied
  | Scheduled
  | Scheduling
  | Stopped
  | Stopping
  | WaitingToStart
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

instance FromText UpdateActionStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "in-progress" -> pure InProgress
      "not-applicable" -> pure NotApplicable
      "not-applied" -> pure NotApplied
      "scheduled" -> pure Scheduled
      "scheduling" -> pure Scheduling
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      "waiting-to-start" -> pure WaitingToStart
      e ->
        fromTextError $
          "Failure parsing UpdateActionStatus from value: '" <> e
            <> "'. Accepted values: complete, in-progress, not-applicable, not-applied, scheduled, scheduling, stopped, stopping, waiting-to-start"

instance ToText UpdateActionStatus where
  toText = \case
    Complete -> "complete"
    InProgress -> "in-progress"
    NotApplicable -> "not-applicable"
    NotApplied -> "not-applied"
    Scheduled -> "scheduled"
    Scheduling -> "scheduling"
    Stopped -> "stopped"
    Stopping -> "stopping"
    WaitingToStart -> "waiting-to-start"

instance Hashable UpdateActionStatus

instance NFData UpdateActionStatus

instance ToByteString UpdateActionStatus

instance ToQuery UpdateActionStatus

instance ToHeader UpdateActionStatus

instance FromXML UpdateActionStatus where
  parseXML = parseXMLText "UpdateActionStatus"
