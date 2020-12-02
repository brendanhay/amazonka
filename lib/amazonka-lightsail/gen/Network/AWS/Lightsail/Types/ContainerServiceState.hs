{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceState where

import Network.AWS.Prelude

data ContainerServiceState
  = Deleting
  | Disabled
  | Pending
  | Ready
  | Running
  | Updating
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

instance FromText ContainerServiceState where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure Deleting
      "disabled" -> pure Disabled
      "pending" -> pure Pending
      "ready" -> pure Ready
      "running" -> pure Running
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing ContainerServiceState from value: '" <> e
            <> "'. Accepted values: deleting, disabled, pending, ready, running, updating"

instance ToText ContainerServiceState where
  toText = \case
    Deleting -> "DELETING"
    Disabled -> "DISABLED"
    Pending -> "PENDING"
    Ready -> "READY"
    Running -> "RUNNING"
    Updating -> "UPDATING"

instance Hashable ContainerServiceState

instance NFData ContainerServiceState

instance ToByteString ContainerServiceState

instance ToQuery ContainerServiceState

instance ToHeader ContainerServiceState

instance FromJSON ContainerServiceState where
  parseJSON = parseJSONText "ContainerServiceState"
