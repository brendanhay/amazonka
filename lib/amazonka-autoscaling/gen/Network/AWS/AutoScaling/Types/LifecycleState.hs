{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleState where

import Network.AWS.Prelude

data LifecycleState
  = Detached
  | Detaching
  | EnteringStandby
  | InService
  | Pending
  | PendingProceed
  | PendingWait
  | Quarantined
  | Standby
  | Terminated
  | Terminating
  | TerminatingProceed
  | TerminatingWait
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

instance FromText LifecycleState where
  parser =
    takeLowerText >>= \case
      "detached" -> pure Detached
      "detaching" -> pure Detaching
      "enteringstandby" -> pure EnteringStandby
      "inservice" -> pure InService
      "pending" -> pure Pending
      "pending:proceed" -> pure PendingProceed
      "pending:wait" -> pure PendingWait
      "quarantined" -> pure Quarantined
      "standby" -> pure Standby
      "terminated" -> pure Terminated
      "terminating" -> pure Terminating
      "terminating:proceed" -> pure TerminatingProceed
      "terminating:wait" -> pure TerminatingWait
      e ->
        fromTextError $
          "Failure parsing LifecycleState from value: '" <> e
            <> "'. Accepted values: detached, detaching, enteringstandby, inservice, pending, pending:proceed, pending:wait, quarantined, standby, terminated, terminating, terminating:proceed, terminating:wait"

instance ToText LifecycleState where
  toText = \case
    Detached -> "Detached"
    Detaching -> "Detaching"
    EnteringStandby -> "EnteringStandby"
    InService -> "InService"
    Pending -> "Pending"
    PendingProceed -> "Pending:Proceed"
    PendingWait -> "Pending:Wait"
    Quarantined -> "Quarantined"
    Standby -> "Standby"
    Terminated -> "Terminated"
    Terminating -> "Terminating"
    TerminatingProceed -> "Terminating:Proceed"
    TerminatingWait -> "Terminating:Wait"

instance Hashable LifecycleState

instance NFData LifecycleState

instance ToByteString LifecycleState

instance ToQuery LifecycleState

instance ToHeader LifecycleState

instance FromXML LifecycleState where
  parseXML = parseXMLText "LifecycleState"
