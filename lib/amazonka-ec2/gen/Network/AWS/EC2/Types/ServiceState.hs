{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ServiceState
  = SerAvailable
  | SerDeleted
  | SerDeleting
  | SerFailed
  | SerPending
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

instance FromText ServiceState where
  parser =
    takeLowerText >>= \case
      "available" -> pure SerAvailable
      "deleted" -> pure SerDeleted
      "deleting" -> pure SerDeleting
      "failed" -> pure SerFailed
      "pending" -> pure SerPending
      e ->
        fromTextError $
          "Failure parsing ServiceState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText ServiceState where
  toText = \case
    SerAvailable -> "Available"
    SerDeleted -> "Deleted"
    SerDeleting -> "Deleting"
    SerFailed -> "Failed"
    SerPending -> "Pending"

instance Hashable ServiceState

instance NFData ServiceState

instance ToByteString ServiceState

instance ToQuery ServiceState

instance ToHeader ServiceState

instance FromXML ServiceState where
  parseXML = parseXMLText "ServiceState"
