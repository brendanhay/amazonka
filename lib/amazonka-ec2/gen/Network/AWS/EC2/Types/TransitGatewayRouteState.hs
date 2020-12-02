{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayRouteState
  = TGRSActive
  | TGRSBlackhole
  | TGRSDeleted
  | TGRSDeleting
  | TGRSPending
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

instance FromText TransitGatewayRouteState where
  parser =
    takeLowerText >>= \case
      "active" -> pure TGRSActive
      "blackhole" -> pure TGRSBlackhole
      "deleted" -> pure TGRSDeleted
      "deleting" -> pure TGRSDeleting
      "pending" -> pure TGRSPending
      e ->
        fromTextError $
          "Failure parsing TransitGatewayRouteState from value: '" <> e
            <> "'. Accepted values: active, blackhole, deleted, deleting, pending"

instance ToText TransitGatewayRouteState where
  toText = \case
    TGRSActive -> "active"
    TGRSBlackhole -> "blackhole"
    TGRSDeleted -> "deleted"
    TGRSDeleting -> "deleting"
    TGRSPending -> "pending"

instance Hashable TransitGatewayRouteState

instance NFData TransitGatewayRouteState

instance ToByteString TransitGatewayRouteState

instance ToQuery TransitGatewayRouteState

instance ToHeader TransitGatewayRouteState

instance FromXML TransitGatewayRouteState where
  parseXML = parseXMLText "TransitGatewayRouteState"
