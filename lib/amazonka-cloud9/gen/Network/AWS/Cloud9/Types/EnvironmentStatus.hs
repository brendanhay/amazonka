{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentStatus where

import Network.AWS.Prelude

data EnvironmentStatus
  = ESConnecting
  | ESCreating
  | ESDeleting
  | ESError'
  | ESReady
  | ESStopped
  | ESStopping
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

instance FromText EnvironmentStatus where
  parser =
    takeLowerText >>= \case
      "connecting" -> pure ESConnecting
      "creating" -> pure ESCreating
      "deleting" -> pure ESDeleting
      "error" -> pure ESError'
      "ready" -> pure ESReady
      "stopped" -> pure ESStopped
      "stopping" -> pure ESStopping
      e ->
        fromTextError $
          "Failure parsing EnvironmentStatus from value: '" <> e
            <> "'. Accepted values: connecting, creating, deleting, error, ready, stopped, stopping"

instance ToText EnvironmentStatus where
  toText = \case
    ESConnecting -> "connecting"
    ESCreating -> "creating"
    ESDeleting -> "deleting"
    ESError' -> "error"
    ESReady -> "ready"
    ESStopped -> "stopped"
    ESStopping -> "stopping"

instance Hashable EnvironmentStatus

instance NFData EnvironmentStatus

instance ToByteString EnvironmentStatus

instance ToQuery EnvironmentStatus

instance ToHeader EnvironmentStatus

instance FromJSON EnvironmentStatus where
  parseJSON = parseJSONText "EnvironmentStatus"
