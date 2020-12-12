{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
  ( TransitGatewayAttachmentResourceType
      ( TransitGatewayAttachmentResourceType',
        DirectConnectGateway,
        Peering,
        TgwPeering,
        VPC,
        VPN
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayAttachmentResourceType = TransitGatewayAttachmentResourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DirectConnectGateway :: TransitGatewayAttachmentResourceType
pattern DirectConnectGateway = TransitGatewayAttachmentResourceType' "direct-connect-gateway"

pattern Peering :: TransitGatewayAttachmentResourceType
pattern Peering = TransitGatewayAttachmentResourceType' "peering"

pattern TgwPeering :: TransitGatewayAttachmentResourceType
pattern TgwPeering = TransitGatewayAttachmentResourceType' "tgw-peering"

pattern VPC :: TransitGatewayAttachmentResourceType
pattern VPC = TransitGatewayAttachmentResourceType' "vpc"

pattern VPN :: TransitGatewayAttachmentResourceType
pattern VPN = TransitGatewayAttachmentResourceType' "vpn"

{-# COMPLETE
  DirectConnectGateway,
  Peering,
  TgwPeering,
  VPC,
  VPN,
  TransitGatewayAttachmentResourceType'
  #-}
