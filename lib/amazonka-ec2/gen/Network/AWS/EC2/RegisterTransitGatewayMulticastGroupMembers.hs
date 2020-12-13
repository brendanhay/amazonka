{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers members (network interfaces) with the transit gateway multicast group. A member is a network interface associated with a supported EC2 instance that receives multicast traffic. For information about supported instances, see <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Consideration> in /Amazon VPC Transit Gateways/ .
--
-- After you add the members, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups> to verify that the members were added to the transit gateway multicast group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a request
    RegisterTransitGatewayMulticastGroupMembers (..),
    mkRegisterTransitGatewayMulticastGroupMembers,

    -- ** Request lenses
    rtgmgmNetworkInterfaceIds,
    rtgmgmTransitGatewayMulticastDomainId,
    rtgmgmGroupIPAddress,
    rtgmgmDryRun,

    -- * Destructuring the response
    RegisterTransitGatewayMulticastGroupMembersResponse (..),
    mkRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** Response lenses
    rtgmgmrsRegisteredMulticastGroupMembers,
    rtgmgmrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembers' smart constructor.
data RegisterTransitGatewayMulticastGroupMembers = RegisterTransitGatewayMulticastGroupMembers'
  { -- | The group members' network interface IDs to register with the transit gateway multicast group.
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

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupMembers' with the minimum fields required to make a request.
--
-- * 'networkInterfaceIds' - The group members' network interface IDs to register with the transit gateway multicast group.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRegisterTransitGatewayMulticastGroupMembers ::
  RegisterTransitGatewayMulticastGroupMembers
mkRegisterTransitGatewayMulticastGroupMembers =
  RegisterTransitGatewayMulticastGroupMembers'
    { networkInterfaceIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      groupIPAddress = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The group members' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Lude.Maybe [Lude.Text])
rtgmgmNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: RegisterTransitGatewayMulticastGroupMembers -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: RegisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED rtgmgmNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Text)
rtgmgmTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: RegisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: RegisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED rtgmgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmGroupIPAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Text)
rtgmgmGroupIPAddress = Lens.lens (groupIPAddress :: RegisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: RegisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED rtgmgmGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Bool)
rtgmgmDryRun = Lens.lens (dryRun :: RegisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RegisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED rtgmgmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    RegisterTransitGatewayMulticastGroupMembers
  where
  type
    Rs RegisterTransitGatewayMulticastGroupMembers =
      RegisterTransitGatewayMulticastGroupMembersResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupMembersResponse'
            Lude.<$> (x Lude..@? "registeredMulticastGroupMembers")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTransitGatewayMulticastGroupMembers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterTransitGatewayMulticastGroupMembers where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTransitGatewayMulticastGroupMembers where
  toQuery RegisterTransitGatewayMulticastGroupMembers' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RegisterTransitGatewayMulticastGroupMembers" :: Lude.ByteString),
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

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupMembersResponse = RegisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the registered transit gateway multicast group members.
    registeredMulticastGroupMembers :: Lude.Maybe TransitGatewayMulticastRegisteredGroupMembers,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTransitGatewayMulticastGroupMembersResponse' with the minimum fields required to make a request.
--
-- * 'registeredMulticastGroupMembers' - Information about the registered transit gateway multicast group members.
-- * 'responseStatus' - The response status code.
mkRegisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTransitGatewayMulticastGroupMembersResponse
mkRegisterTransitGatewayMulticastGroupMembersResponse
  pResponseStatus_ =
    RegisterTransitGatewayMulticastGroupMembersResponse'
      { registeredMulticastGroupMembers =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the registered transit gateway multicast group members.
--
-- /Note:/ Consider using 'registeredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrsRegisteredMulticastGroupMembers :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse (Lude.Maybe TransitGatewayMulticastRegisteredGroupMembers)
rtgmgmrsRegisteredMulticastGroupMembers = Lens.lens (registeredMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> Lude.Maybe TransitGatewayMulticastRegisteredGroupMembers) (\s a -> s {registeredMulticastGroupMembers = a} :: RegisterTransitGatewayMulticastGroupMembersResponse)
{-# DEPRECATED rtgmgmrsRegisteredMulticastGroupMembers "Use generic-lens or generic-optics with 'registeredMulticastGroupMembers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse Lude.Int
rtgmgmrsResponseStatus = Lens.lens (responseStatus :: RegisterTransitGatewayMulticastGroupMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTransitGatewayMulticastGroupMembersResponse)
{-# DEPRECATED rtgmgmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
