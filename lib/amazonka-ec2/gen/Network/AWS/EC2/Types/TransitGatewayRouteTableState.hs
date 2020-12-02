{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTableState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTableState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayRouteTableState
  = TGRTSAvailable
  | TGRTSDeleted
  | TGRTSDeleting
  | TGRTSPending
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

instance FromText TransitGatewayRouteTableState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TGRTSAvailable
      "deleted" -> pure TGRTSDeleted
      "deleting" -> pure TGRTSDeleting
      "pending" -> pure TGRTSPending
      e ->
        fromTextError $
          "Failure parsing TransitGatewayRouteTableState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText TransitGatewayRouteTableState where
  toText = \case
    TGRTSAvailable -> "available"
    TGRTSDeleted -> "deleted"
    TGRTSDeleting -> "deleting"
    TGRTSPending -> "pending"

instance Hashable TransitGatewayRouteTableState

instance NFData TransitGatewayRouteTableState

instance ToByteString TransitGatewayRouteTableState

instance ToQuery TransitGatewayRouteTableState

instance ToHeader TransitGatewayRouteTableState

instance FromXML TransitGatewayRouteTableState where
  parseXML = parseXMLText "TransitGatewayRouteTableState"
