{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetState where

import Network.AWS.Prelude

data FleetState
  = Running
  | Starting
  | Stopped
  | Stopping
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

instance FromText FleetState where
  parser =
    takeLowerText >>= \case
      "running" -> pure Running
      "starting" -> pure Starting
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      e ->
        fromTextError $
          "Failure parsing FleetState from value: '" <> e
            <> "'. Accepted values: running, starting, stopped, stopping"

instance ToText FleetState where
  toText = \case
    Running -> "RUNNING"
    Starting -> "STARTING"
    Stopped -> "STOPPED"
    Stopping -> "STOPPING"

instance Hashable FleetState

instance NFData FleetState

instance ToByteString FleetState

instance ToQuery FleetState

instance ToHeader FleetState

instance FromJSON FleetState where
  parseJSON = parseJSONText "FleetState"
