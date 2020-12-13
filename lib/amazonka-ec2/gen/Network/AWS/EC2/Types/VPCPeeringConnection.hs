{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcAccepterVPCInfo,
    vpcRequesterVPCInfo,
    vpcExpirationTime,
    vpcTags,
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
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Lude.Maybe Lude.Text,
    -- | The status of the VPC peering connection.
    status :: Lude.Maybe VPCPeeringConnectionStateReason,
    -- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
    accepterVPCInfo :: Lude.Maybe VPCPeeringConnectionVPCInfo,
    -- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
    requesterVPCInfo :: Lude.Maybe VPCPeeringConnectionVPCInfo,
    -- | The time that an unaccepted VPC peering connection will expire.
    expirationTime :: Lude.Maybe Lude.DateTime,
    -- | Any tags assigned to the resource.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
-- * 'status' - The status of the VPC peering connection.
-- * 'accepterVPCInfo' - Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
-- * 'requesterVPCInfo' - Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
-- * 'expirationTime' - The time that an unaccepted VPC peering connection will expire.
-- * 'tags' - Any tags assigned to the resource.
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
vpcVPCPeeringConnectionId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The status of the VPC peering connection.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcStatus :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionStateReason)
vpcStatus = Lens.lens (status :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionStateReason) (\s a -> s {status = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'accepterVPCInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcAccepterVPCInfo :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionVPCInfo)
vpcAccepterVPCInfo = Lens.lens (accepterVPCInfo :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionVPCInfo) (\s a -> s {accepterVPCInfo = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcAccepterVPCInfo "Use generic-lens or generic-optics with 'accepterVPCInfo' instead." #-}

-- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'requesterVPCInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcRequesterVPCInfo :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionVPCInfo)
vpcRequesterVPCInfo = Lens.lens (requesterVPCInfo :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionVPCInfo) (\s a -> s {requesterVPCInfo = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcRequesterVPCInfo "Use generic-lens or generic-optics with 'requesterVPCInfo' instead." #-}

-- | The time that an unaccepted VPC peering connection will expire.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcExpirationTime :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.DateTime)
vpcExpirationTime = Lens.lens (expirationTime :: VPCPeeringConnection -> Lude.Maybe Lude.DateTime) (\s a -> s {expirationTime = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcTags :: Lens.Lens' VPCPeeringConnection (Lude.Maybe [Tag])
vpcTags = Lens.lens (tags :: VPCPeeringConnection -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
