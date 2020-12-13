{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPCEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more specified VPC endpoints. Deleting a gateway endpoint also deletes the endpoint routes in the route tables that were associated with the endpoint. Deleting an interface endpoint or a Gateway Load Balancer endpoint deletes the endpoint network interfaces. Gateway Load Balancer endpoints can only be deleted if the routes that are associated with the endpoint are deleted.
module Network.AWS.EC2.DeleteVPCEndpoints
  ( -- * Creating a request
    DeleteVPCEndpoints (..),
    mkDeleteVPCEndpoints,

    -- ** Request lenses
    dveVPCEndpointIds,
    dveDryRun,

    -- * Destructuring the response
    DeleteVPCEndpointsResponse (..),
    mkDeleteVPCEndpointsResponse,

    -- ** Response lenses
    dversUnsuccessful,
    dversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteVpcEndpoints.
--
-- /See:/ 'mkDeleteVPCEndpoints' smart constructor.
data DeleteVPCEndpoints = DeleteVPCEndpoints'
  { -- | One or more VPC endpoint IDs.
    vpcEndpointIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpoints' with the minimum fields required to make a request.
--
-- * 'vpcEndpointIds' - One or more VPC endpoint IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVPCEndpoints ::
  DeleteVPCEndpoints
mkDeleteVPCEndpoints =
  DeleteVPCEndpoints'
    { vpcEndpointIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | One or more VPC endpoint IDs.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveVPCEndpointIds :: Lens.Lens' DeleteVPCEndpoints [Lude.Text]
dveVPCEndpointIds = Lens.lens (vpcEndpointIds :: DeleteVPCEndpoints -> [Lude.Text]) (\s a -> s {vpcEndpointIds = a} :: DeleteVPCEndpoints)
{-# DEPRECATED dveVPCEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveDryRun :: Lens.Lens' DeleteVPCEndpoints (Lude.Maybe Lude.Bool)
dveDryRun = Lens.lens (dryRun :: DeleteVPCEndpoints -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPCEndpoints)
{-# DEPRECATED dveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteVPCEndpoints where
  type Rs DeleteVPCEndpoints = DeleteVPCEndpointsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteVPCEndpointsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCEndpoints where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPCEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCEndpoints where
  toQuery DeleteVPCEndpoints' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVpcEndpoints" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "VpcEndpointId" vpcEndpointIds,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of DeleteVpcEndpoints.
--
-- /See:/ 'mkDeleteVPCEndpointsResponse' smart constructor.
data DeleteVPCEndpointsResponse = DeleteVPCEndpointsResponse'
  { -- | Information about the VPC endpoints that were not successfully deleted.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the VPC endpoints that were not successfully deleted.
-- * 'responseStatus' - The response status code.
mkDeleteVPCEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCEndpointsResponse
mkDeleteVPCEndpointsResponse pResponseStatus_ =
  DeleteVPCEndpointsResponse'
    { unsuccessful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC endpoints that were not successfully deleted.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dversUnsuccessful :: Lens.Lens' DeleteVPCEndpointsResponse (Lude.Maybe [UnsuccessfulItem])
dversUnsuccessful = Lens.lens (unsuccessful :: DeleteVPCEndpointsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: DeleteVPCEndpointsResponse)
{-# DEPRECATED dversUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dversResponseStatus :: Lens.Lens' DeleteVPCEndpointsResponse Lude.Int
dversResponseStatus = Lens.lens (responseStatus :: DeleteVPCEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCEndpointsResponse)
{-# DEPRECATED dversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
