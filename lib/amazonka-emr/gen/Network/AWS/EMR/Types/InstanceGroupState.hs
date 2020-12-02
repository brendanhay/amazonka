{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupState where

import Network.AWS.Prelude

data InstanceGroupState
  = Arrested
  | Bootstrapping
  | Ended
  | Provisioning
  | Reconfiguring
  | Resizing
  | Running
  | ShuttingDown
  | Suspended
  | Terminated
  | Terminating
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

instance FromText InstanceGroupState where
  parser =
    takeLowerText >>= \case
      "arrested" -> pure Arrested
      "bootstrapping" -> pure Bootstrapping
      "ended" -> pure Ended
      "provisioning" -> pure Provisioning
      "reconfiguring" -> pure Reconfiguring
      "resizing" -> pure Resizing
      "running" -> pure Running
      "shutting_down" -> pure ShuttingDown
      "suspended" -> pure Suspended
      "terminated" -> pure Terminated
      "terminating" -> pure Terminating
      e ->
        fromTextError $
          "Failure parsing InstanceGroupState from value: '" <> e
            <> "'. Accepted values: arrested, bootstrapping, ended, provisioning, reconfiguring, resizing, running, shutting_down, suspended, terminated, terminating"

instance ToText InstanceGroupState where
  toText = \case
    Arrested -> "ARRESTED"
    Bootstrapping -> "BOOTSTRAPPING"
    Ended -> "ENDED"
    Provisioning -> "PROVISIONING"
    Reconfiguring -> "RECONFIGURING"
    Resizing -> "RESIZING"
    Running -> "RUNNING"
    ShuttingDown -> "SHUTTING_DOWN"
    Suspended -> "SUSPENDED"
    Terminated -> "TERMINATED"
    Terminating -> "TERMINATING"

instance Hashable InstanceGroupState

instance NFData InstanceGroupState

instance ToByteString InstanceGroupState

instance ToQuery InstanceGroupState

instance ToHeader InstanceGroupState

instance FromJSON InstanceGroupState where
  parseJSON = parseJSONText "InstanceGroupState"
