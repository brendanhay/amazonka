{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.ConnectionState where

import Network.AWS.Prelude

data ConnectionState
  = CSAvailable
  | CSDeleted
  | CSDeleting
  | CSDown
  | CSOrdering
  | CSPending
  | CSRejected
  | CSRequested
  | CSUnknown
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

instance FromText ConnectionState where
  parser =
    takeLowerText >>= \case
      "available" -> pure CSAvailable
      "deleted" -> pure CSDeleted
      "deleting" -> pure CSDeleting
      "down" -> pure CSDown
      "ordering" -> pure CSOrdering
      "pending" -> pure CSPending
      "rejected" -> pure CSRejected
      "requested" -> pure CSRequested
      "unknown" -> pure CSUnknown
      e ->
        fromTextError $
          "Failure parsing ConnectionState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, down, ordering, pending, rejected, requested, unknown"

instance ToText ConnectionState where
  toText = \case
    CSAvailable -> "available"
    CSDeleted -> "deleted"
    CSDeleting -> "deleting"
    CSDown -> "down"
    CSOrdering -> "ordering"
    CSPending -> "pending"
    CSRejected -> "rejected"
    CSRequested -> "requested"
    CSUnknown -> "unknown"

instance Hashable ConnectionState

instance NFData ConnectionState

instance ToByteString ConnectionState

instance ToQuery ConnectionState

instance ToHeader ConnectionState

instance FromJSON ConnectionState where
  parseJSON = parseJSONText "ConnectionState"
