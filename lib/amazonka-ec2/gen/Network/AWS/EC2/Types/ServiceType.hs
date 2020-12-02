{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ServiceType
  = Gateway
  | GatewayLoadBalancer
  | Interface
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

instance FromText ServiceType where
  parser =
    takeLowerText >>= \case
      "gateway" -> pure Gateway
      "gatewayloadbalancer" -> pure GatewayLoadBalancer
      "interface" -> pure Interface
      e ->
        fromTextError $
          "Failure parsing ServiceType from value: '" <> e
            <> "'. Accepted values: gateway, gatewayloadbalancer, interface"

instance ToText ServiceType where
  toText = \case
    Gateway -> "Gateway"
    GatewayLoadBalancer -> "GatewayLoadBalancer"
    Interface -> "Interface"

instance Hashable ServiceType

instance NFData ServiceType

instance ToByteString ServiceType

instance ToQuery ServiceType

instance ToHeader ServiceType

instance FromXML ServiceType where
  parseXML = parseXMLText "ServiceType"
