{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCPeeringConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPC peering connection options on one side of a VPC peering connection. You can do the following:
--
--
--     * Enable/disable communication over the peering connection between an EC2-Classic instance that's linked to your VPC (using ClassicLink) and instances in the peer VPC.
--
--
--     * Enable/disable communication over the peering connection between instances in your VPC and an EC2-Classic instance that's linked to the peer VPC.
--
--
--     * Enable/disable the ability to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
--
--
-- If the peered VPCs are in the same AWS account, you can enable DNS resolution for queries from the local VPC. This ensures that queries from the local VPC resolve to private IP addresses in the peer VPC. This option is not available if the peered VPCs are in different AWS accounts or different Regions. For peered VPCs in different AWS accounts, each AWS account owner must initiate a separate request to modify the peering connection options. For inter-region peering connections, you must use the Region for the requester VPC to modify the requester VPC peering options and the Region for the accepter VPC to modify the accepter VPC peering options. To verify which VPCs are the accepter and the requester for a VPC peering connection, use the 'DescribeVpcPeeringConnections' command.
module Network.AWS.EC2.ModifyVPCPeeringConnectionOptions
  ( -- * Creating a request
    ModifyVPCPeeringConnectionOptions (..),
    mkModifyVPCPeeringConnectionOptions,

    -- ** Request lenses
    mvpcoVPCPeeringConnectionId,
    mvpcoRequesterPeeringConnectionOptions,
    mvpcoAccepterPeeringConnectionOptions,
    mvpcoDryRun,

    -- * Destructuring the response
    ModifyVPCPeeringConnectionOptionsResponse (..),
    mkModifyVPCPeeringConnectionOptionsResponse,

    -- ** Response lenses
    mvpcorsRequesterPeeringConnectionOptions,
    mvpcorsAccepterPeeringConnectionOptions,
    mvpcorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCPeeringConnectionOptions' smart constructor.
data ModifyVPCPeeringConnectionOptions = ModifyVPCPeeringConnectionOptions'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Lude.Text,
    -- | The VPC peering connection options for the requester VPC.
    requesterPeeringConnectionOptions :: Lude.Maybe PeeringConnectionOptionsRequest,
    -- | The VPC peering connection options for the accepter VPC.
    accepterPeeringConnectionOptions :: Lude.Maybe PeeringConnectionOptionsRequest,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCPeeringConnectionOptions' with the minimum fields required to make a request.
--
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
-- * 'requesterPeeringConnectionOptions' - The VPC peering connection options for the requester VPC.
-- * 'accepterPeeringConnectionOptions' - The VPC peering connection options for the accepter VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPCPeeringConnectionOptions ::
  -- | 'vpcPeeringConnectionId'
  Lude.Text ->
  ModifyVPCPeeringConnectionOptions
mkModifyVPCPeeringConnectionOptions pVPCPeeringConnectionId_ =
  ModifyVPCPeeringConnectionOptions'
    { vpcPeeringConnectionId =
        pVPCPeeringConnectionId_,
      requesterPeeringConnectionOptions = Lude.Nothing,
      accepterPeeringConnectionOptions = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoVPCPeeringConnectionId :: Lens.Lens' ModifyVPCPeeringConnectionOptions Lude.Text
mvpcoVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: ModifyVPCPeeringConnectionOptions -> Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: ModifyVPCPeeringConnectionOptions)
{-# DEPRECATED mvpcoVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The VPC peering connection options for the requester VPC.
--
-- /Note:/ Consider using 'requesterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoRequesterPeeringConnectionOptions :: Lens.Lens' ModifyVPCPeeringConnectionOptions (Lude.Maybe PeeringConnectionOptionsRequest)
mvpcoRequesterPeeringConnectionOptions = Lens.lens (requesterPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptions -> Lude.Maybe PeeringConnectionOptionsRequest) (\s a -> s {requesterPeeringConnectionOptions = a} :: ModifyVPCPeeringConnectionOptions)
{-# DEPRECATED mvpcoRequesterPeeringConnectionOptions "Use generic-lens or generic-optics with 'requesterPeeringConnectionOptions' instead." #-}

-- | The VPC peering connection options for the accepter VPC.
--
-- /Note:/ Consider using 'accepterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoAccepterPeeringConnectionOptions :: Lens.Lens' ModifyVPCPeeringConnectionOptions (Lude.Maybe PeeringConnectionOptionsRequest)
mvpcoAccepterPeeringConnectionOptions = Lens.lens (accepterPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptions -> Lude.Maybe PeeringConnectionOptionsRequest) (\s a -> s {accepterPeeringConnectionOptions = a} :: ModifyVPCPeeringConnectionOptions)
{-# DEPRECATED mvpcoAccepterPeeringConnectionOptions "Use generic-lens or generic-optics with 'accepterPeeringConnectionOptions' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoDryRun :: Lens.Lens' ModifyVPCPeeringConnectionOptions (Lude.Maybe Lude.Bool)
mvpcoDryRun = Lens.lens (dryRun :: ModifyVPCPeeringConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCPeeringConnectionOptions)
{-# DEPRECATED mvpcoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPCPeeringConnectionOptions where
  type
    Rs ModifyVPCPeeringConnectionOptions =
      ModifyVPCPeeringConnectionOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCPeeringConnectionOptionsResponse'
            Lude.<$> (x Lude..@? "requesterPeeringConnectionOptions")
            Lude.<*> (x Lude..@? "accepterPeeringConnectionOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCPeeringConnectionOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCPeeringConnectionOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCPeeringConnectionOptions where
  toQuery ModifyVPCPeeringConnectionOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpcPeeringConnectionOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "RequesterPeeringConnectionOptions"
          Lude.=: requesterPeeringConnectionOptions,
        "AccepterPeeringConnectionOptions"
          Lude.=: accepterPeeringConnectionOptions,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPCPeeringConnectionOptionsResponse' smart constructor.
data ModifyVPCPeeringConnectionOptionsResponse = ModifyVPCPeeringConnectionOptionsResponse'
  { -- | Information about the VPC peering connection options for the requester VPC.
    requesterPeeringConnectionOptions :: Lude.Maybe PeeringConnectionOptions,
    -- | Information about the VPC peering connection options for the accepter VPC.
    accepterPeeringConnectionOptions :: Lude.Maybe PeeringConnectionOptions,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCPeeringConnectionOptionsResponse' with the minimum fields required to make a request.
--
-- * 'requesterPeeringConnectionOptions' - Information about the VPC peering connection options for the requester VPC.
-- * 'accepterPeeringConnectionOptions' - Information about the VPC peering connection options for the accepter VPC.
-- * 'responseStatus' - The response status code.
mkModifyVPCPeeringConnectionOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCPeeringConnectionOptionsResponse
mkModifyVPCPeeringConnectionOptionsResponse pResponseStatus_ =
  ModifyVPCPeeringConnectionOptionsResponse'
    { requesterPeeringConnectionOptions =
        Lude.Nothing,
      accepterPeeringConnectionOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC peering connection options for the requester VPC.
--
-- /Note:/ Consider using 'requesterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorsRequesterPeeringConnectionOptions :: Lens.Lens' ModifyVPCPeeringConnectionOptionsResponse (Lude.Maybe PeeringConnectionOptions)
mvpcorsRequesterPeeringConnectionOptions = Lens.lens (requesterPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptionsResponse -> Lude.Maybe PeeringConnectionOptions) (\s a -> s {requesterPeeringConnectionOptions = a} :: ModifyVPCPeeringConnectionOptionsResponse)
{-# DEPRECATED mvpcorsRequesterPeeringConnectionOptions "Use generic-lens or generic-optics with 'requesterPeeringConnectionOptions' instead." #-}

-- | Information about the VPC peering connection options for the accepter VPC.
--
-- /Note:/ Consider using 'accepterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorsAccepterPeeringConnectionOptions :: Lens.Lens' ModifyVPCPeeringConnectionOptionsResponse (Lude.Maybe PeeringConnectionOptions)
mvpcorsAccepterPeeringConnectionOptions = Lens.lens (accepterPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptionsResponse -> Lude.Maybe PeeringConnectionOptions) (\s a -> s {accepterPeeringConnectionOptions = a} :: ModifyVPCPeeringConnectionOptionsResponse)
{-# DEPRECATED mvpcorsAccepterPeeringConnectionOptions "Use generic-lens or generic-optics with 'accepterPeeringConnectionOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorsResponseStatus :: Lens.Lens' ModifyVPCPeeringConnectionOptionsResponse Lude.Int
mvpcorsResponseStatus = Lens.lens (responseStatus :: ModifyVPCPeeringConnectionOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCPeeringConnectionOptionsResponse)
{-# DEPRECATED mvpcorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
