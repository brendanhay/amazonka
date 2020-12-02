{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayAttachmentResourceType
  = DirectConnectGateway
  | Peering
  | TgwPeering
  | VPC
  | VPN
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

instance FromText TransitGatewayAttachmentResourceType where
  parser =
    takeLowerText >>= \case
      "direct-connect-gateway" -> pure DirectConnectGateway
      "peering" -> pure Peering
      "tgw-peering" -> pure TgwPeering
      "vpc" -> pure VPC
      "vpn" -> pure VPN
      e ->
        fromTextError $
          "Failure parsing TransitGatewayAttachmentResourceType from value: '" <> e
            <> "'. Accepted values: direct-connect-gateway, peering, tgw-peering, vpc, vpn"

instance ToText TransitGatewayAttachmentResourceType where
  toText = \case
    DirectConnectGateway -> "direct-connect-gateway"
    Peering -> "peering"
    TgwPeering -> "tgw-peering"
    VPC -> "vpc"
    VPN -> "vpn"

instance Hashable TransitGatewayAttachmentResourceType

instance NFData TransitGatewayAttachmentResourceType

instance ToByteString TransitGatewayAttachmentResourceType

instance ToQuery TransitGatewayAttachmentResourceType

instance ToHeader TransitGatewayAttachmentResourceType

instance FromXML TransitGatewayAttachmentResourceType where
  parseXML = parseXMLText "TransitGatewayAttachmentResourceType"
