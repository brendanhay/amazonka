{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified members (network interfaces) from the transit gateway multicast group.
module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a request
    DeregisterTransitGatewayMulticastGroupMembers (..),
    mkDeregisterTransitGatewayMulticastGroupMembers,

    -- ** Request lenses
    dtgmgmNetworkInterfaceIds,
    dtgmgmTransitGatewayMulticastDomainId,
    dtgmgmGroupIPAddress,
    dtgmgmDryRun,

    -- * Destructuring the response
    DeregisterTransitGatewayMulticastGroupMembersResponse (..),
    mkDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** Response lenses
    dtgmgmrsDeregisteredMulticastGroupMembers,
    dtgmgmrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembers' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembers = DeregisterTransitGatewayMulticastGroupMembers'
  { -- | The IDs of the group members' network interfaces.
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

-- | Creates a value of 'DeregisterTransitGatewayMulticastGroupMembers' with the minimum fields required to make a request.
--
-- * 'networkInterfaceIds' - The IDs of the group members' network interfaces.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeregisterTransitGatewayMulticastGroupMembers ::
  DeregisterTransitGatewayMulticastGroupMembers
mkDeregisterTransitGatewayMulticastGroupMembers =
  DeregisterTransitGatewayMulticastGroupMembers'
    { networkInterfaceIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      groupIPAddress = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the group members' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Lude.Maybe [Lude.Text])
dtgmgmNetworkInterfaceIds = Lens.lens (networkInterfaceIds :: DeregisterTransitGatewayMulticastGroupMembers -> Lude.Maybe [Lude.Text]) (\s a -> s {networkInterfaceIds = a} :: DeregisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED dtgmgmNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Text)
dtgmgmTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: DeregisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: DeregisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED dtgmgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmGroupIPAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Text)
dtgmgmGroupIPAddress = Lens.lens (groupIPAddress :: DeregisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: DeregisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED dtgmgmGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Lude.Maybe Lude.Bool)
dtgmgmDryRun = Lens.lens (dryRun :: DeregisterTransitGatewayMulticastGroupMembers -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeregisterTransitGatewayMulticastGroupMembers)
{-# DEPRECATED dtgmgmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    DeregisterTransitGatewayMulticastGroupMembers
  where
  type
    Rs DeregisterTransitGatewayMulticastGroupMembers =
      DeregisterTransitGatewayMulticastGroupMembersResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeregisterTransitGatewayMulticastGroupMembersResponse'
            Lude.<$> (x Lude..@? "deregisteredMulticastGroupMembers")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DeregisterTransitGatewayMulticastGroupMembers
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterTransitGatewayMulticastGroupMembers where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterTransitGatewayMulticastGroupMembers where
  toQuery DeregisterTransitGatewayMulticastGroupMembers' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "DeregisterTransitGatewayMulticastGroupMembers" ::
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

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembersResponse = DeregisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the deregistered members.
    deregisteredMulticastGroupMembers :: Lude.Maybe TransitGatewayMulticastDeregisteredGroupMembers,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTransitGatewayMulticastGroupMembersResponse' with the minimum fields required to make a request.
--
-- * 'deregisteredMulticastGroupMembers' - Information about the deregistered members.
-- * 'responseStatus' - The response status code.
mkDeregisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTransitGatewayMulticastGroupMembersResponse
mkDeregisterTransitGatewayMulticastGroupMembersResponse
  pResponseStatus_ =
    DeregisterTransitGatewayMulticastGroupMembersResponse'
      { deregisteredMulticastGroupMembers =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the deregistered members.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrsDeregisteredMulticastGroupMembers :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse (Lude.Maybe TransitGatewayMulticastDeregisteredGroupMembers)
dtgmgmrsDeregisteredMulticastGroupMembers = Lens.lens (deregisteredMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> Lude.Maybe TransitGatewayMulticastDeregisteredGroupMembers) (\s a -> s {deregisteredMulticastGroupMembers = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)
{-# DEPRECATED dtgmgmrsDeregisteredMulticastGroupMembers "Use generic-lens or generic-optics with 'deregisteredMulticastGroupMembers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse Lude.Int
dtgmgmrsResponseStatus = Lens.lens (responseStatus :: DeregisterTransitGatewayMulticastGroupMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTransitGatewayMulticastGroupMembersResponse)
{-# DEPRECATED dtgmgmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
