{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnassignIPv6Addresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more IPv6 addresses from a network interface.
module Network.AWS.EC2.UnassignIPv6Addresses
  ( -- * Creating a request
    UnassignIPv6Addresses (..),
    mkUnassignIPv6Addresses,

    -- ** Request lenses
    uiaNetworkInterfaceId,
    uiaIPv6Addresses,

    -- * Destructuring the response
    UnassignIPv6AddressesResponse (..),
    mkUnassignIPv6AddressesResponse,

    -- ** Response lenses
    uiarsNetworkInterfaceId,
    uiarsUnassignedIPv6Addresses,
    uiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnassignIPv6Addresses' smart constructor.
data UnassignIPv6Addresses = UnassignIPv6Addresses'
  { -- | The ID of the network interface.
    networkInterfaceId :: Lude.Text,
    -- | The IPv6 addresses to unassign from the network interface.
    ipv6Addresses :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignIPv6Addresses' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'ipv6Addresses' - The IPv6 addresses to unassign from the network interface.
mkUnassignIPv6Addresses ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  UnassignIPv6Addresses
mkUnassignIPv6Addresses pNetworkInterfaceId_ =
  UnassignIPv6Addresses'
    { networkInterfaceId = pNetworkInterfaceId_,
      ipv6Addresses = Lude.mempty
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaNetworkInterfaceId :: Lens.Lens' UnassignIPv6Addresses Lude.Text
uiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: UnassignIPv6Addresses -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: UnassignIPv6Addresses)
{-# DEPRECATED uiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The IPv6 addresses to unassign from the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaIPv6Addresses :: Lens.Lens' UnassignIPv6Addresses [Lude.Text]
uiaIPv6Addresses = Lens.lens (ipv6Addresses :: UnassignIPv6Addresses -> [Lude.Text]) (\s a -> s {ipv6Addresses = a} :: UnassignIPv6Addresses)
{-# DEPRECATED uiaIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.AWSRequest UnassignIPv6Addresses where
  type Rs UnassignIPv6Addresses = UnassignIPv6AddressesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          UnassignIPv6AddressesResponse'
            Lude.<$> (x Lude..@? "networkInterfaceId")
            Lude.<*> ( x Lude..@? "unassignedIpv6Addresses" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnassignIPv6Addresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UnassignIPv6Addresses where
  toPath = Lude.const "/"

instance Lude.ToQuery UnassignIPv6Addresses where
  toQuery UnassignIPv6Addresses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UnassignIpv6Addresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        Lude.toQueryList "Ipv6Addresses" ipv6Addresses
      ]

-- | /See:/ 'mkUnassignIPv6AddressesResponse' smart constructor.
data UnassignIPv6AddressesResponse = UnassignIPv6AddressesResponse'
  { -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The IPv6 addresses that have been unassigned from the network interface.
    unassignedIPv6Addresses :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignIPv6AddressesResponse' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'unassignedIPv6Addresses' - The IPv6 addresses that have been unassigned from the network interface.
-- * 'responseStatus' - The response status code.
mkUnassignIPv6AddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnassignIPv6AddressesResponse
mkUnassignIPv6AddressesResponse pResponseStatus_ =
  UnassignIPv6AddressesResponse'
    { networkInterfaceId = Lude.Nothing,
      unassignedIPv6Addresses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarsNetworkInterfaceId :: Lens.Lens' UnassignIPv6AddressesResponse (Lude.Maybe Lude.Text)
uiarsNetworkInterfaceId = Lens.lens (networkInterfaceId :: UnassignIPv6AddressesResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: UnassignIPv6AddressesResponse)
{-# DEPRECATED uiarsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The IPv6 addresses that have been unassigned from the network interface.
--
-- /Note:/ Consider using 'unassignedIPv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarsUnassignedIPv6Addresses :: Lens.Lens' UnassignIPv6AddressesResponse (Lude.Maybe [Lude.Text])
uiarsUnassignedIPv6Addresses = Lens.lens (unassignedIPv6Addresses :: UnassignIPv6AddressesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {unassignedIPv6Addresses = a} :: UnassignIPv6AddressesResponse)
{-# DEPRECATED uiarsUnassignedIPv6Addresses "Use generic-lens or generic-optics with 'unassignedIPv6Addresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarsResponseStatus :: Lens.Lens' UnassignIPv6AddressesResponse Lude.Int
uiarsResponseStatus = Lens.lens (responseStatus :: UnassignIPv6AddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnassignIPv6AddressesResponse)
{-# DEPRECATED uiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
