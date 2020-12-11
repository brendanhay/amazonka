{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a VPC peering connection between two VPCs: a requester VPC that you own and an accepter VPC with which to create the connection. The accepter VPC can belong to another AWS account and can be in a different Region to the requester VPC. The requester VPC and accepter VPC cannot have overlapping CIDR blocks.
--
-- The owner of the accepter VPC must accept the peering request to activate the peering connection. The VPC peering connection request expires after 7 days, after which it cannot be accepted or rejected.
-- If you create a VPC peering connection request between VPCs with overlapping CIDR blocks, the VPC peering connection has a status of @failed@ .
module Network.AWS.EC2.CreateVPCPeeringConnection
  ( -- * Creating a request
    CreateVPCPeeringConnection (..),
    mkCreateVPCPeeringConnection,

    -- ** Request lenses
    cvpcPeerVPCId,
    cvpcVPCId,
    cvpcPeerOwnerId,
    cvpcTagSpecifications,
    cvpcPeerRegion,
    cvpcDryRun,

    -- * Destructuring the response
    CreateVPCPeeringConnectionResponse (..),
    mkCreateVPCPeeringConnectionResponse,

    -- ** Response lenses
    cvpcrsVPCPeeringConnection,
    cvpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVPCPeeringConnection' smart constructor.
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
  { peerVPCId ::
      Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    peerOwnerId :: Lude.Maybe Lude.Text,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    peerRegion :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'peerOwnerId' - The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
-- * 'peerRegion' - The Region code for the accepter VPC, if the accepter VPC is located in a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
-- * 'peerVPCId' - The ID of the VPC with which you are creating the VPC peering connection. You must specify this parameter in the request.
-- * 'tagSpecifications' - The tags to assign to the peering connection.
-- * 'vpcId' - The ID of the requester VPC. You must specify this parameter in the request.
mkCreateVPCPeeringConnection ::
  CreateVPCPeeringConnection
mkCreateVPCPeeringConnection =
  CreateVPCPeeringConnection'
    { peerVPCId = Lude.Nothing,
      vpcId = Lude.Nothing,
      peerOwnerId = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      peerRegion = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPC with which you are creating the VPC peering connection. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVPCId :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe Lude.Text)
cvpcPeerVPCId = Lens.lens (peerVPCId :: CreateVPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

-- | The ID of the requester VPC. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcVPCId :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe Lude.Text)
cvpcVPCId = Lens.lens (vpcId :: CreateVPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
--
-- /Note:/ Consider using 'peerOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerOwnerId :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe Lude.Text)
cvpcPeerOwnerId = Lens.lens (peerOwnerId :: CreateVPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {peerOwnerId = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcPeerOwnerId "Use generic-lens or generic-optics with 'peerOwnerId' instead." #-}

-- | The tags to assign to the peering connection.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcTagSpecifications :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe [TagSpecification])
cvpcTagSpecifications = Lens.lens (tagSpecifications :: CreateVPCPeeringConnection -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Region code for the accepter VPC, if the accepter VPC is located in a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerRegion :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe Lude.Text)
cvpcPeerRegion = Lens.lens (peerRegion :: CreateVPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {peerRegion = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcPeerRegion "Use generic-lens or generic-optics with 'peerRegion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcDryRun :: Lens.Lens' CreateVPCPeeringConnection (Lude.Maybe Lude.Bool)
cvpcDryRun = Lens.lens (dryRun :: CreateVPCPeeringConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPCPeeringConnection)
{-# DEPRECATED cvpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateVPCPeeringConnection where
  type
    Rs CreateVPCPeeringConnection =
      CreateVPCPeeringConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPCPeeringConnectionResponse'
            Lude.<$> (x Lude..@? "vpcPeeringConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCPeeringConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCPeeringConnection where
  toQuery CreateVPCPeeringConnection' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateVpcPeeringConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PeerVpcId" Lude.=: peerVPCId,
        "VpcId" Lude.=: vpcId,
        "PeerOwnerId" Lude.=: peerOwnerId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "PeerRegion" Lude.=: peerRegion,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateVPCPeeringConnectionResponse' smart constructor.
data CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
  { vpcPeeringConnection ::
      Lude.Maybe
        VPCPeeringConnection,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpcPeeringConnection' - Information about the VPC peering connection.
mkCreateVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCPeeringConnectionResponse
mkCreateVPCPeeringConnectionResponse pResponseStatus_ =
  CreateVPCPeeringConnectionResponse'
    { vpcPeeringConnection =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrsVPCPeeringConnection :: Lens.Lens' CreateVPCPeeringConnectionResponse (Lude.Maybe VPCPeeringConnection)
cvpcrsVPCPeeringConnection = Lens.lens (vpcPeeringConnection :: CreateVPCPeeringConnectionResponse -> Lude.Maybe VPCPeeringConnection) (\s a -> s {vpcPeeringConnection = a} :: CreateVPCPeeringConnectionResponse)
{-# DEPRECATED cvpcrsVPCPeeringConnection "Use generic-lens or generic-optics with 'vpcPeeringConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrsResponseStatus :: Lens.Lens' CreateVPCPeeringConnectionResponse Lude.Int
cvpcrsResponseStatus = Lens.lens (responseStatus :: CreateVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCPeeringConnectionResponse)
{-# DEPRECATED cvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
