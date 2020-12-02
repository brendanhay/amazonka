{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceState where

import Network.AWS.Prelude

data InstanceState
  = ISAwaitingFulfillment
  | ISBootstrapping
  | ISProvisioning
  | ISRunning
  | ISTerminated
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

instance FromText InstanceState where
  parser =
    takeLowerText >>= \case
      "awaiting_fulfillment" -> pure ISAwaitingFulfillment
      "bootstrapping" -> pure ISBootstrapping
      "provisioning" -> pure ISProvisioning
      "running" -> pure ISRunning
      "terminated" -> pure ISTerminated
      e ->
        fromTextError $
          "Failure parsing InstanceState from value: '" <> e
            <> "'. Accepted values: awaiting_fulfillment, bootstrapping, provisioning, running, terminated"

instance ToText InstanceState where
  toText = \case
    ISAwaitingFulfillment -> "AWAITING_FULFILLMENT"
    ISBootstrapping -> "BOOTSTRAPPING"
    ISProvisioning -> "PROVISIONING"
    ISRunning -> "RUNNING"
    ISTerminated -> "TERMINATED"

instance Hashable InstanceState

instance NFData InstanceState

instance ToByteString InstanceState

instance ToQuery InstanceState

instance ToHeader InstanceState

instance ToJSON InstanceState where
  toJSON = toJSONText

instance FromJSON InstanceState where
  parseJSON = parseJSONText "InstanceState"
