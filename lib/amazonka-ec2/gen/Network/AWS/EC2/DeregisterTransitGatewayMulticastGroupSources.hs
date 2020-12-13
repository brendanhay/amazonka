{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified sources (network interfaces) from the transit gateway multicast group.
module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
  ( -- * Creating a request
    DeregisterTransitGatewayMulticastGroupSources (..),
    mkDeregisterTransitGatewayMulticastGroupSources,

    -- ** Request lenses
    dtgmgsNetworkInterfaceIds,
    dtgmgsTransitGatewayMulticastDomainId,
    dtgmgsGroupIPAddress,
    dtgmgsDryRun,

    -- * Destructuring the response
    DeregisterTransitGatewayMulticastGroupSourcesResponse (..),
    mkDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** Response lenses
    dtgmgsrsDeregisteredMulticastGroupSources,
    dtgmgsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSources' smart constructor.
data DeregisterTransitGatewayMulticastGroupSources = DeregisterTransitGatewayMulticastGroupSources'
  { -- | The IDs of the group sources' network interfaces.
    networkInterfaceIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Maybe Lude.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIPAddress :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTransitGatewayMulticastGroupSources' with the minimum fields required to make a request.
--
-- * 'networkInterfaceIds' - The IDs of the group sources' network interfaces.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeregisterTransitGatewayMulticastGroupSources ::
  DeregisterTransitGatewayMulticastGroupSources
mkDeregisterTransitGatewayMulticastGroupSources =
  DeregisterTransitGatewayMulticastGroupSources'
    { networkInterfaceIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      groupIPAddress = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the group sources' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Lude.Maybe [Lude.Text])
dtgmgsNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: DeregisterTransitGatewayMulticastGroupSources -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: DeregisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED dtgmgsNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Text)
dtgmgsTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: DeregisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: DeregisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED dtgmgsTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsGroupIPAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Text)
dtgmgsGroupIPAddress = Lens.lens (groupIPAddress :: DeregisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: DeregisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED dtgmgsGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Bool)
dtgmgsDryRun = Lens.lens (dryRun :: DeregisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeregisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED dtgmgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    DeregisterTransitGatewayMulticastGroupSources
  where
  type
    Rs DeregisterTransitGatewayMulticastGroupSources =
      DeregisterTransitGatewayMulticastGroupSourcesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeregisterTransitGatewayMulticastGroupSourcesResponse'
            Lude.<$> (x Lude..@? "deregisteredMulticastGroupSources")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DeregisterTransitGatewayMulticastGroupSources
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterTransitGatewayMulticastGroupSources where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterTransitGatewayMulticastGroupSources where
  toQuery DeregisterTransitGatewayMulticastGroupSources' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "DeregisterTransitGatewayMulticastGroupSources" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "NetworkInterfaceIds"
              Lude.<$> networkInterfaceIds
          ),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "GroupIpAddress" Lude.=: groupIPAddress,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupSourcesResponse = DeregisterTransitGatewayMulticastGroupSourcesResponse'
  { -- | Information about the deregistered group sources.
    deregisteredMulticastGroupSources :: Lude.Maybe TransitGatewayMulticastDeregisteredGroupSources,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTransitGatewayMulticastGroupSourcesResponse' with the minimum fields required to make a request.
--
-- * 'deregisteredMulticastGroupSources' - Information about the deregistered group sources.
-- * 'responseStatus' - The response status code.
mkDeregisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTransitGatewayMulticastGroupSourcesResponse
mkDeregisterTransitGatewayMulticastGroupSourcesResponse
  pResponseStatus_ =
    DeregisterTransitGatewayMulticastGroupSourcesResponse'
      { deregisteredMulticastGroupSources =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the deregistered group sources.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrsDeregisteredMulticastGroupSources :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse (Lude.Maybe TransitGatewayMulticastDeregisteredGroupSources)
dtgmgsrsDeregisteredMulticastGroupSources = Lens.lens (deregisteredMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> Lude.Maybe TransitGatewayMulticastDeregisteredGroupSources) (\s a -> s {deregisteredMulticastGroupSources = a} :: DeregisterTransitGatewayMulticastGroupSourcesResponse)
{-# DEPRECATED dtgmgsrsDeregisteredMulticastGroupSources "Use generic-lens or generic-optics with 'deregisteredMulticastGroupSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse Lude.Int
dtgmgsrsResponseStatus = Lens.lens (responseStatus :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTransitGatewayMulticastGroupSourcesResponse)
{-# DEPRECATED dtgmgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
