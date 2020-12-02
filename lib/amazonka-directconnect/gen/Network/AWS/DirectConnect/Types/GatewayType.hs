{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.GatewayType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.GatewayType where

import Network.AWS.Prelude

data GatewayType
  = TransitGateway
  | VirtualPrivateGateway
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

instance FromText GatewayType where
  parser =
    takeLowerText >>= \case
      "transitgateway" -> pure TransitGateway
      "virtualprivategateway" -> pure VirtualPrivateGateway
      e ->
        fromTextError $
          "Failure parsing GatewayType from value: '" <> e
            <> "'. Accepted values: transitgateway, virtualprivategateway"

instance ToText GatewayType where
  toText = \case
    TransitGateway -> "transitGateway"
    VirtualPrivateGateway -> "virtualPrivateGateway"

instance Hashable GatewayType

instance NFData GatewayType

instance ToByteString GatewayType

instance ToQuery GatewayType

instance ToHeader GatewayType

instance FromJSON GatewayType where
  parseJSON = parseJSONText "GatewayType"
