{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptVPCEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts one or more interface VPC endpoint connection requests to your VPC endpoint service.
module Network.AWS.EC2.AcceptVPCEndpointConnections
  ( -- * Creating a request
    AcceptVPCEndpointConnections (..),
    mkAcceptVPCEndpointConnections,

    -- ** Request lenses
    avecVPCEndpointIds,
    avecServiceId,
    avecDryRun,

    -- * Destructuring the response
    AcceptVPCEndpointConnectionsResponse (..),
    mkAcceptVPCEndpointConnectionsResponse,

    -- ** Response lenses
    avecrsUnsuccessful,
    avecrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptVPCEndpointConnections' smart constructor.
data AcceptVPCEndpointConnections = AcceptVPCEndpointConnections'
  { -- | The IDs of one or more interface VPC endpoints.
    vpcEndpointIds :: [Lude.Text],
    -- | The ID of the VPC endpoint service.
    serviceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptVPCEndpointConnections' with the minimum fields required to make a request.
--
-- * 'vpcEndpointIds' - The IDs of one or more interface VPC endpoints.
-- * 'serviceId' - The ID of the VPC endpoint service.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAcceptVPCEndpointConnections ::
  -- | 'serviceId'
  Lude.Text ->
  AcceptVPCEndpointConnections
mkAcceptVPCEndpointConnections pServiceId_ =
  AcceptVPCEndpointConnections'
    { vpcEndpointIds = Lude.mempty,
      serviceId = pServiceId_,
      dryRun = Lude.Nothing
    }

-- | The IDs of one or more interface VPC endpoints.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecVPCEndpointIds :: Lens.Lens' AcceptVPCEndpointConnections [Lude.Text]
avecVPCEndpointIds = Lens.lens (vpcEndpointIds :: AcceptVPCEndpointConnections -> [Lude.Text]) (\s a -> s {vpcEndpointIds = a} :: AcceptVPCEndpointConnections)
{-# DEPRECATED avecVPCEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead." #-}

-- | The ID of the VPC endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecServiceId :: Lens.Lens' AcceptVPCEndpointConnections Lude.Text
avecServiceId = Lens.lens (serviceId :: AcceptVPCEndpointConnections -> Lude.Text) (\s a -> s {serviceId = a} :: AcceptVPCEndpointConnections)
{-# DEPRECATED avecServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecDryRun :: Lens.Lens' AcceptVPCEndpointConnections (Lude.Maybe Lude.Bool)
avecDryRun = Lens.lens (dryRun :: AcceptVPCEndpointConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AcceptVPCEndpointConnections)
{-# DEPRECATED avecDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AcceptVPCEndpointConnections where
  type
    Rs AcceptVPCEndpointConnections =
      AcceptVPCEndpointConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AcceptVPCEndpointConnectionsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptVPCEndpointConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptVPCEndpointConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptVPCEndpointConnections where
  toQuery AcceptVPCEndpointConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptVpcEndpointConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "VpcEndpointId" vpcEndpointIds,
        "ServiceId" Lude.=: serviceId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAcceptVPCEndpointConnectionsResponse' smart constructor.
data AcceptVPCEndpointConnectionsResponse = AcceptVPCEndpointConnectionsResponse'
  { -- | Information about the interface endpoints that were not accepted, if applicable.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the interface endpoints that were not accepted, if applicable.
-- * 'responseStatus' - The response status code.
mkAcceptVPCEndpointConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptVPCEndpointConnectionsResponse
mkAcceptVPCEndpointConnectionsResponse pResponseStatus_ =
  AcceptVPCEndpointConnectionsResponse'
    { unsuccessful =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the interface endpoints that were not accepted, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecrsUnsuccessful :: Lens.Lens' AcceptVPCEndpointConnectionsResponse (Lude.Maybe [UnsuccessfulItem])
avecrsUnsuccessful = Lens.lens (unsuccessful :: AcceptVPCEndpointConnectionsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: AcceptVPCEndpointConnectionsResponse)
{-# DEPRECATED avecrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecrsResponseStatus :: Lens.Lens' AcceptVPCEndpointConnectionsResponse Lude.Int
avecrsResponseStatus = Lens.lens (responseStatus :: AcceptVPCEndpointConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptVPCEndpointConnectionsResponse)
{-# DEPRECATED avecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
