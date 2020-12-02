{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShipmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShipmentState where

import Network.AWS.Prelude

data ShipmentState
  = Received
  | Returned
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

instance FromText ShipmentState where
  parser =
    takeLowerText >>= \case
      "received" -> pure Received
      "returned" -> pure Returned
      e ->
        fromTextError $
          "Failure parsing ShipmentState from value: '" <> e
            <> "'. Accepted values: received, returned"

instance ToText ShipmentState where
  toText = \case
    Received -> "RECEIVED"
    Returned -> "RETURNED"

instance Hashable ShipmentState

instance NFData ShipmentState

instance ToByteString ShipmentState

instance ToQuery ShipmentState

instance ToHeader ShipmentState

instance ToJSON ShipmentState where
  toJSON = toJSONText
