{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept a VPC peering connection request. To accept a request, the VPC peering connection must be in the @pending-acceptance@ state, and you must be the owner of the peer VPC. Use 'DescribeVpcPeeringConnections' to view your outstanding VPC peering connection requests.
--
-- For an inter-Region VPC peering connection request, you must accept the VPC peering connection in the Region of the accepter VPC.
module Network.AWS.EC2.AcceptVPCPeeringConnection
  ( -- * Creating a request
    AcceptVPCPeeringConnection (..),
    mkAcceptVPCPeeringConnection,

    -- ** Request lenses
    avpcVPCPeeringConnectionId,
    avpcDryRun,

    -- * Destructuring the response
    AcceptVPCPeeringConnectionResponse (..),
    mkAcceptVPCPeeringConnectionResponse,

    -- ** Response lenses
    avpcrsVPCPeeringConnection,
    avpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptVPCPeeringConnection' smart constructor.
data AcceptVPCPeeringConnection = AcceptVPCPeeringConnection'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'AcceptVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection. You must specify this parameter in the request.
mkAcceptVPCPeeringConnection ::
  AcceptVPCPeeringConnection
mkAcceptVPCPeeringConnection =
  AcceptVPCPeeringConnection'
    { vpcPeeringConnectionId =
        Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPC peering connection. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcVPCPeeringConnectionId :: Lens.Lens' AcceptVPCPeeringConnection (Lude.Maybe Lude.Text)
avpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: AcceptVPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: AcceptVPCPeeringConnection)
{-# DEPRECATED avpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcDryRun :: Lens.Lens' AcceptVPCPeeringConnection (Lude.Maybe Lude.Bool)
avpcDryRun = Lens.lens (dryRun :: AcceptVPCPeeringConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AcceptVPCPeeringConnection)
{-# DEPRECATED avpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AcceptVPCPeeringConnection where
  type
    Rs AcceptVPCPeeringConnection =
      AcceptVPCPeeringConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AcceptVPCPeeringConnectionResponse'
            Lude.<$> (x Lude..@? "vpcPeeringConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptVPCPeeringConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptVPCPeeringConnection where
  toQuery AcceptVPCPeeringConnection' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptVpcPeeringConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAcceptVPCPeeringConnectionResponse' smart constructor.
data AcceptVPCPeeringConnectionResponse = AcceptVPCPeeringConnectionResponse'
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

-- | Creates a value of 'AcceptVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpcPeeringConnection' - Information about the VPC peering connection.
mkAcceptVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptVPCPeeringConnectionResponse
mkAcceptVPCPeeringConnectionResponse pResponseStatus_ =
  AcceptVPCPeeringConnectionResponse'
    { vpcPeeringConnection =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcrsVPCPeeringConnection :: Lens.Lens' AcceptVPCPeeringConnectionResponse (Lude.Maybe VPCPeeringConnection)
avpcrsVPCPeeringConnection = Lens.lens (vpcPeeringConnection :: AcceptVPCPeeringConnectionResponse -> Lude.Maybe VPCPeeringConnection) (\s a -> s {vpcPeeringConnection = a} :: AcceptVPCPeeringConnectionResponse)
{-# DEPRECATED avpcrsVPCPeeringConnection "Use generic-lens or generic-optics with 'vpcPeeringConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcrsResponseStatus :: Lens.Lens' AcceptVPCPeeringConnectionResponse Lude.Int
avpcrsResponseStatus = Lens.lens (responseStatus :: AcceptVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptVPCPeeringConnectionResponse)
{-# DEPRECATED avpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
