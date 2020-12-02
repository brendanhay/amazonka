{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCEndpointType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCEndpointType
  = VETGateway
  | VETGatewayLoadBalancer
  | VETInterface
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

instance FromText VPCEndpointType where
  parser =
    takeLowerText >>= \case
      "gateway" -> pure VETGateway
      "gatewayloadbalancer" -> pure VETGatewayLoadBalancer
      "interface" -> pure VETInterface
      e ->
        fromTextError $
          "Failure parsing VPCEndpointType from value: '" <> e
            <> "'. Accepted values: gateway, gatewayloadbalancer, interface"

instance ToText VPCEndpointType where
  toText = \case
    VETGateway -> "Gateway"
    VETGatewayLoadBalancer -> "GatewayLoadBalancer"
    VETInterface -> "Interface"

instance Hashable VPCEndpointType

instance NFData VPCEndpointType

instance ToByteString VPCEndpointType

instance ToQuery VPCEndpointType

instance ToHeader VPCEndpointType

instance FromXML VPCEndpointType where
  parseXML = parseXMLText "VPCEndpointType"
