{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers sources (network interfaces) with the specified transit gateway multicast group.
--
-- A multicast source is a network interface attached to a supported instance that sends multicast traffic. For information about supported instances, see <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Considerations> in /Amazon VPC Transit Gateways/ .
-- After you add the source, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups> to verify that the source was added to the multicast group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
  ( -- * Creating a request
    RegisterTransitGatewayMulticastGroupSources (..),
    mkRegisterTransitGatewayMulticastGroupSources,

    -- ** Request lenses
    rtgmgsNetworkInterfaceIds,
    rtgmgsTransitGatewayMulticastDomainId,
    rtgmgsGroupIPAddress,
    rtgmgsDryRun,

    -- * Destructuring the response
    RegisterTransitGatewayMulticastGroupSourcesResponse (..),
    mkRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** Response lenses
    rtgmgsrsRegisteredMulticastGroupSources,
    rtgmgsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSources' smart constructor.
data RegisterTransitGatewayMulticastGroupSources = RegisterTransitGatewayMulticastGroupSources'
  { networkInterfaceIds ::
      Lude.Maybe
        [Lude.Text],
    transitGatewayMulticastDomainId ::
      Lude.Maybe
        Lude.Text,
    groupIPAddress ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupSources' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'networkInterfaceIds' - The group sources' network interface IDs to register with the transit gateway multicast group.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkRegisterTransitGatewayMulticastGroupSources ::
  RegisterTransitGatewayMulticastGroupSources
mkRegisterTransitGatewayMulticastGroupSources =
  RegisterTransitGatewayMulticastGroupSources'
    { networkInterfaceIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      groupIPAddress = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The group sources' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Lude.Maybe [Lude.Text])
rtgmgsNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: RegisterTransitGatewayMulticastGroupSources -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: RegisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED rtgmgsNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Text)
rtgmgsTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: RegisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: RegisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED rtgmgsTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsGroupIPAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Text)
rtgmgsGroupIPAddress = Lens.lens (groupIPAddress :: RegisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: RegisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED rtgmgsGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Lude.Maybe Lude.Bool)
rtgmgsDryRun = Lens.lens (dryRun :: RegisterTransitGatewayMulticastGroupSources -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RegisterTransitGatewayMulticastGroupSources)
{-# DEPRECATED rtgmgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    RegisterTransitGatewayMulticastGroupSources
  where
  type
    Rs RegisterTransitGatewayMulticastGroupSources =
      RegisterTransitGatewayMulticastGroupSourcesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupSourcesResponse'
            Lude.<$> (x Lude..@? "registeredMulticastGroupSources")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTransitGatewayMulticastGroupSources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterTransitGatewayMulticastGroupSources where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTransitGatewayMulticastGroupSources where
  toQuery RegisterTransitGatewayMulticastGroupSources' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RegisterTransitGatewayMulticastGroupSources" :: Lude.ByteString),
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

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupSourcesResponse = RegisterTransitGatewayMulticastGroupSourcesResponse'
  { registeredMulticastGroupSources ::
      Lude.Maybe
        TransitGatewayMulticastRegisteredGroupSources,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupSourcesResponse' with the minimum fields required to make a request.
--
-- * 'registeredMulticastGroupSources' - Information about the transit gateway multicast group sources.
-- * 'responseStatus' - The response status code.
mkRegisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTransitGatewayMulticastGroupSourcesResponse
mkRegisterTransitGatewayMulticastGroupSourcesResponse
  pResponseStatus_ =
    RegisterTransitGatewayMulticastGroupSourcesResponse'
      { registeredMulticastGroupSources =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the transit gateway multicast group sources.
--
-- /Note:/ Consider using 'registeredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrsRegisteredMulticastGroupSources :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse (Lude.Maybe TransitGatewayMulticastRegisteredGroupSources)
rtgmgsrsRegisteredMulticastGroupSources = Lens.lens (registeredMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> Lude.Maybe TransitGatewayMulticastRegisteredGroupSources) (\s a -> s {registeredMulticastGroupSources = a} :: RegisterTransitGatewayMulticastGroupSourcesResponse)
{-# DEPRECATED rtgmgsrsRegisteredMulticastGroupSources "Use generic-lens or generic-optics with 'registeredMulticastGroupSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse Lude.Int
rtgmgsrsResponseStatus = Lens.lens (responseStatus :: RegisterTransitGatewayMulticastGroupSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTransitGatewayMulticastGroupSourcesResponse)
{-# DEPRECATED rtgmgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
