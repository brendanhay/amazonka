{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.HSMStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.HSMStatus where

import Network.AWS.Prelude

data HSMStatus
  = HSDegraded
  | HSPending
  | HSRunning
  | HSSuspended
  | HSTerminated
  | HSTerminating
  | HSUpdating
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

instance FromText HSMStatus where
  parser =
    takeLowerText >>= \case
      "degraded" -> pure HSDegraded
      "pending" -> pure HSPending
      "running" -> pure HSRunning
      "suspended" -> pure HSSuspended
      "terminated" -> pure HSTerminated
      "terminating" -> pure HSTerminating
      "updating" -> pure HSUpdating
      e ->
        fromTextError $
          "Failure parsing HSMStatus from value: '" <> e
            <> "'. Accepted values: degraded, pending, running, suspended, terminated, terminating, updating"

instance ToText HSMStatus where
  toText = \case
    HSDegraded -> "DEGRADED"
    HSPending -> "PENDING"
    HSRunning -> "RUNNING"
    HSSuspended -> "SUSPENDED"
    HSTerminated -> "TERMINATED"
    HSTerminating -> "TERMINATING"
    HSUpdating -> "UPDATING"

instance Hashable HSMStatus

instance NFData HSMStatus

instance ToByteString HSMStatus

instance ToQuery HSMStatus

instance ToHeader HSMStatus

instance FromJSON HSMStatus where
  parseJSON = parseJSONText "HSMStatus"
