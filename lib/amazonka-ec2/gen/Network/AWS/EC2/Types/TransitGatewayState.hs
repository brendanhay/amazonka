{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayState
  = TGSAvailable
  | TGSDeleted
  | TGSDeleting
  | TGSModifying
  | TGSPending
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

instance FromText TransitGatewayState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TGSAvailable
      "deleted" -> pure TGSDeleted
      "deleting" -> pure TGSDeleting
      "modifying" -> pure TGSModifying
      "pending" -> pure TGSPending
      e ->
        fromTextError $
          "Failure parsing TransitGatewayState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, modifying, pending"

instance ToText TransitGatewayState where
  toText = \case
    TGSAvailable -> "available"
    TGSDeleted -> "deleted"
    TGSDeleting -> "deleting"
    TGSModifying -> "modifying"
    TGSPending -> "pending"

instance Hashable TransitGatewayState

instance NFData TransitGatewayState

instance ToByteString TransitGatewayState

instance ToQuery TransitGatewayState

instance ToHeader TransitGatewayState

instance FromXML TransitGatewayState where
  parseXML = parseXMLText "TransitGatewayState"
