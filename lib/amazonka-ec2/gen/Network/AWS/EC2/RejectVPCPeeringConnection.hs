{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a VPC peering connection request. The VPC peering connection must be in the @pending-acceptance@ state. Use the 'DescribeVpcPeeringConnections' request to view your outstanding VPC peering connection requests. To delete an active VPC peering connection, or to delete a VPC peering connection request that you initiated, use 'DeleteVpcPeeringConnection' .
module Network.AWS.EC2.RejectVPCPeeringConnection
  ( -- * Creating a request
    RejectVPCPeeringConnection (..),
    mkRejectVPCPeeringConnection,

    -- ** Request lenses
    rvpcVPCPeeringConnectionId,
    rvpcDryRun,

    -- * Destructuring the response
    RejectVPCPeeringConnectionResponse (..),
    mkRejectVPCPeeringConnectionResponse,

    -- ** Response lenses
    rvpcrsReturn,
    rvpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectVPCPeeringConnection' smart constructor.
data RejectVPCPeeringConnection = RejectVPCPeeringConnection'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRejectVPCPeeringConnection ::
  -- | 'vpcPeeringConnectionId'
  Lude.Text ->
  RejectVPCPeeringConnection
mkRejectVPCPeeringConnection pVPCPeeringConnectionId_ =
  RejectVPCPeeringConnection'
    { vpcPeeringConnectionId =
        pVPCPeeringConnectionId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcVPCPeeringConnectionId :: Lens.Lens' RejectVPCPeeringConnection Lude.Text
rvpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: RejectVPCPeeringConnection -> Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: RejectVPCPeeringConnection)
{-# DEPRECATED rvpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcDryRun :: Lens.Lens' RejectVPCPeeringConnection (Lude.Maybe Lude.Bool)
rvpcDryRun = Lens.lens (dryRun :: RejectVPCPeeringConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RejectVPCPeeringConnection)
{-# DEPRECATED rvpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RejectVPCPeeringConnection where
  type
    Rs RejectVPCPeeringConnection =
      RejectVPCPeeringConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RejectVPCPeeringConnectionResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectVPCPeeringConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RejectVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectVPCPeeringConnection where
  toQuery RejectVPCPeeringConnection' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RejectVpcPeeringConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRejectVPCPeeringConnectionResponse' smart constructor.
data RejectVPCPeeringConnectionResponse = RejectVPCPeeringConnectionResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkRejectVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectVPCPeeringConnectionResponse
mkRejectVPCPeeringConnectionResponse pResponseStatus_ =
  RejectVPCPeeringConnectionResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcrsReturn :: Lens.Lens' RejectVPCPeeringConnectionResponse (Lude.Maybe Lude.Bool)
rvpcrsReturn = Lens.lens (return :: RejectVPCPeeringConnectionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: RejectVPCPeeringConnectionResponse)
{-# DEPRECATED rvpcrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcrsResponseStatus :: Lens.Lens' RejectVPCPeeringConnectionResponse Lude.Int
rvpcrsResponseStatus = Lens.lens (responseStatus :: RejectVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectVPCPeeringConnectionResponse)
{-# DEPRECATED rvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
