{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayRouteType
  = TGRTPropagated
  | TGRTStatic
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

instance FromText TransitGatewayRouteType where
  parser =
    takeLowerText >>= \case
      "propagated" -> pure TGRTPropagated
      "static" -> pure TGRTStatic
      e ->
        fromTextError $
          "Failure parsing TransitGatewayRouteType from value: '" <> e
            <> "'. Accepted values: propagated, static"

instance ToText TransitGatewayRouteType where
  toText = \case
    TGRTPropagated -> "propagated"
    TGRTStatic -> "static"

instance Hashable TransitGatewayRouteType

instance NFData TransitGatewayRouteType

instance ToByteString TransitGatewayRouteType

instance ToQuery TransitGatewayRouteType

instance ToHeader TransitGatewayRouteType

instance FromXML TransitGatewayRouteType where
  parseXML = parseXMLText "TransitGatewayRouteType"
