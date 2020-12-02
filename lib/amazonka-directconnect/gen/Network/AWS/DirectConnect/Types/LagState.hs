{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.LagState where

import Network.AWS.Prelude

data LagState
  = LSAvailable
  | LSDeleted
  | LSDeleting
  | LSDown
  | LSPending
  | LSRequested
  | LSUnknown
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

instance FromText LagState where
  parser =
    takeLowerText >>= \case
      "available" -> pure LSAvailable
      "deleted" -> pure LSDeleted
      "deleting" -> pure LSDeleting
      "down" -> pure LSDown
      "pending" -> pure LSPending
      "requested" -> pure LSRequested
      "unknown" -> pure LSUnknown
      e ->
        fromTextError $
          "Failure parsing LagState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, down, pending, requested, unknown"

instance ToText LagState where
  toText = \case
    LSAvailable -> "available"
    LSDeleted -> "deleted"
    LSDeleting -> "deleting"
    LSDown -> "down"
    LSPending -> "pending"
    LSRequested -> "requested"
    LSUnknown -> "unknown"

instance Hashable LagState

instance NFData LagState

instance ToByteString LagState

instance ToQuery LagState

instance ToHeader LagState

instance FromJSON LagState where
  parseJSON = parseJSONText "LagState"
