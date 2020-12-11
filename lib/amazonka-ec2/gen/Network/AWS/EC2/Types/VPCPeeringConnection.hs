-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnection
  ( VPCPeeringConnection (..),

    -- * Smart constructor
    mkVPCPeeringConnection,

    -- * Lenses
    vpcpcVPCPeeringConnectionId,
    vpcpcStatus,
    vpcpcAccepterVPCInfo,
    vpcpcRequesterVPCInfo,
    vpcpcExpirationTime,
    vpcpcTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
import Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC peering connection.
--
-- /See:/ 'mkVPCPeeringConnection' smart constructor.
data VPCPeeringConnection = VPCPeeringConnection'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe VPCPeeringConnectionStateReason,
    accepterVPCInfo ::
      Lude.Maybe VPCPeeringConnectionVPCInfo,
    requesterVPCInfo ::
      Lude.Maybe VPCPeeringConnectionVPCInfo,
    expirationTime :: Lude.Maybe Lude.ISO8601,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'accepterVPCInfo' - Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
-- * 'expirationTime' - The time that an unaccepted VPC peering connection will expire.
-- * 'requesterVPCInfo' - Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
-- * 'status' - The status of the VPC peering connection.
-- * 'tags' - Any tags assigned to the resource.
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
mkVPCPeeringConnection ::
  VPCPeeringConnection
mkVPCPeeringConnection =
  VPCPeeringConnection'
    { vpcPeeringConnectionId = Lude.Nothing,
      status = Lude.Nothing,
      accepterVPCInfo = Lude.Nothing,
      requesterVPCInfo = Lude.Nothing,
      expirationTime = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcVPCPeeringConnectionId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The status of the VPC peering connection.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcStatus :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionStateReason)
vpcpcStatus = Lens.lens (status :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionStateReason) (\s a -> s {status = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'accepterVPCInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcAccepterVPCInfo :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionVPCInfo)
vpcpcAccepterVPCInfo = Lens.lens (accepterVPCInfo :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionVPCInfo) (\s a -> s {accepterVPCInfo = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcAccepterVPCInfo "Use generic-lens or generic-optics with 'accepterVPCInfo' instead." #-}

-- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'requesterVPCInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcRequesterVPCInfo :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionVPCInfo)
vpcpcRequesterVPCInfo = Lens.lens (requesterVPCInfo :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionVPCInfo) (\s a -> s {requesterVPCInfo = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcRequesterVPCInfo "Use generic-lens or generic-optics with 'requesterVPCInfo' instead." #-}

-- | The time that an unaccepted VPC peering connection will expire.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcExpirationTime :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.ISO8601)
vpcpcExpirationTime = Lens.lens (expirationTime :: VPCPeeringConnection -> Lude.Maybe Lude.ISO8601) (\s a -> s {expirationTime = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcpcTags :: Lens.Lens' VPCPeeringConnection (Lude.Maybe [Tag])
vpcpcTags = Lens.lens (tags :: VPCPeeringConnection -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcpcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML VPCPeeringConnection where
  parseXML x =
    VPCPeeringConnection'
      Lude.<$> (x Lude..@? "vpcPeeringConnectionId")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "accepterVpcInfo")
      Lude.<*> (x Lude..@? "requesterVpcInfo")
      Lude.<*> (x Lude..@? "expirationTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
