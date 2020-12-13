{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectVPCEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects one or more VPC endpoint connection requests to your VPC endpoint service.
module Network.AWS.EC2.RejectVPCEndpointConnections
  ( -- * Creating a request
    RejectVPCEndpointConnections (..),
    mkRejectVPCEndpointConnections,

    -- ** Request lenses
    rvecVPCEndpointIds,
    rvecServiceId,
    rvecDryRun,

    -- * Destructuring the response
    RejectVPCEndpointConnectionsResponse (..),
    mkRejectVPCEndpointConnectionsResponse,

    -- ** Response lenses
    rvecrsUnsuccessful,
    rvecrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectVPCEndpointConnections' smart constructor.
data RejectVPCEndpointConnections = RejectVPCEndpointConnections'
  { -- | The IDs of one or more VPC endpoints.
    vpcEndpointIds :: [Lude.Text],
    -- | The ID of the service.
    serviceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectVPCEndpointConnections' with the minimum fields required to make a request.
--
-- * 'vpcEndpointIds' - The IDs of one or more VPC endpoints.
-- * 'serviceId' - The ID of the service.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRejectVPCEndpointConnections ::
  -- | 'serviceId'
  Lude.Text ->
  RejectVPCEndpointConnections
mkRejectVPCEndpointConnections pServiceId_ =
  RejectVPCEndpointConnections'
    { vpcEndpointIds = Lude.mempty,
      serviceId = pServiceId_,
      dryRun = Lude.Nothing
    }

-- | The IDs of one or more VPC endpoints.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecVPCEndpointIds :: Lens.Lens' RejectVPCEndpointConnections [Lude.Text]
rvecVPCEndpointIds = Lens.lens (vpcEndpointIds :: RejectVPCEndpointConnections -> [Lude.Text]) (\s a -> s {vpcEndpointIds = a} :: RejectVPCEndpointConnections)
{-# DEPRECATED rvecVPCEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecServiceId :: Lens.Lens' RejectVPCEndpointConnections Lude.Text
rvecServiceId = Lens.lens (serviceId :: RejectVPCEndpointConnections -> Lude.Text) (\s a -> s {serviceId = a} :: RejectVPCEndpointConnections)
{-# DEPRECATED rvecServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecDryRun :: Lens.Lens' RejectVPCEndpointConnections (Lude.Maybe Lude.Bool)
rvecDryRun = Lens.lens (dryRun :: RejectVPCEndpointConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RejectVPCEndpointConnections)
{-# DEPRECATED rvecDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RejectVPCEndpointConnections where
  type
    Rs RejectVPCEndpointConnections =
      RejectVPCEndpointConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RejectVPCEndpointConnectionsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectVPCEndpointConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RejectVPCEndpointConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectVPCEndpointConnections where
  toQuery RejectVPCEndpointConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RejectVpcEndpointConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "VpcEndpointId" vpcEndpointIds,
        "ServiceId" Lude.=: serviceId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRejectVPCEndpointConnectionsResponse' smart constructor.
data RejectVPCEndpointConnectionsResponse = RejectVPCEndpointConnectionsResponse'
  { -- | Information about the endpoints that were not rejected, if applicable.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the endpoints that were not rejected, if applicable.
-- * 'responseStatus' - The response status code.
mkRejectVPCEndpointConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectVPCEndpointConnectionsResponse
mkRejectVPCEndpointConnectionsResponse pResponseStatus_ =
  RejectVPCEndpointConnectionsResponse'
    { unsuccessful =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the endpoints that were not rejected, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecrsUnsuccessful :: Lens.Lens' RejectVPCEndpointConnectionsResponse (Lude.Maybe [UnsuccessfulItem])
rvecrsUnsuccessful = Lens.lens (unsuccessful :: RejectVPCEndpointConnectionsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: RejectVPCEndpointConnectionsResponse)
{-# DEPRECATED rvecrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecrsResponseStatus :: Lens.Lens' RejectVPCEndpointConnectionsResponse Lude.Int
rvecrsResponseStatus = Lens.lens (responseStatus :: RejectVPCEndpointConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectVPCEndpointConnectionsResponse)
{-# DEPRECATED rvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
