{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetState where

import Network.AWS.Prelude

data InstanceFleetState
  = IFSBootstrapping
  | IFSProvisioning
  | IFSResizing
  | IFSRunning
  | IFSSuspended
  | IFSTerminated
  | IFSTerminating
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

instance FromText InstanceFleetState where
  parser =
    takeLowerText >>= \case
      "bootstrapping" -> pure IFSBootstrapping
      "provisioning" -> pure IFSProvisioning
      "resizing" -> pure IFSResizing
      "running" -> pure IFSRunning
      "suspended" -> pure IFSSuspended
      "terminated" -> pure IFSTerminated
      "terminating" -> pure IFSTerminating
      e ->
        fromTextError $
          "Failure parsing InstanceFleetState from value: '" <> e
            <> "'. Accepted values: bootstrapping, provisioning, resizing, running, suspended, terminated, terminating"

instance ToText InstanceFleetState where
  toText = \case
    IFSBootstrapping -> "BOOTSTRAPPING"
    IFSProvisioning -> "PROVISIONING"
    IFSResizing -> "RESIZING"
    IFSRunning -> "RUNNING"
    IFSSuspended -> "SUSPENDED"
    IFSTerminated -> "TERMINATED"
    IFSTerminating -> "TERMINATING"

instance Hashable InstanceFleetState

instance NFData InstanceFleetState

instance ToByteString InstanceFleetState

instance ToQuery InstanceFleetState

instance ToHeader InstanceFleetState

instance FromJSON InstanceFleetState where
  parseJSON = parseJSONText "InstanceFleetState"
