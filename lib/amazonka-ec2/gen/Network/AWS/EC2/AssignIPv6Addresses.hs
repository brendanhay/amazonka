{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignIPv6Addresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more IPv6 addresses to the specified network interface. You can specify one or more specific IPv6 addresses, or you can specify the number of IPv6 addresses to be automatically assigned from within the subnet's IPv6 CIDR block range. You can assign as many IPv6 addresses to a network interface as you can assign private IPv4 addresses, and the limit varies per instance type. For information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per Network Interface Per Instance Type> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify either the IPv6 addresses or the IPv6 address count in the request.
module Network.AWS.EC2.AssignIPv6Addresses
  ( -- * Creating a request
    AssignIPv6Addresses (..),
    mkAssignIPv6Addresses,

    -- ** Request lenses
    aiaNetworkInterfaceId,
    aiaIPv6AddressCount,
    aiaIPv6Addresses,

    -- * Destructuring the response
    AssignIPv6AddressesResponse (..),
    mkAssignIPv6AddressesResponse,

    -- ** Response lenses
    aiarsNetworkInterfaceId,
    aiarsAssignedIPv6Addresses,
    aiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssignIPv6Addresses' smart constructor.
data AssignIPv6Addresses = AssignIPv6Addresses'
  { -- | The ID of the network interface.
    networkInterfaceId :: Lude.Text,
    -- | The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Lude.Maybe Lude.Int,
    -- | One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
    ipv6Addresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignIPv6Addresses' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'ipv6AddressCount' - The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
-- * 'ipv6Addresses' - One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
mkAssignIPv6Addresses ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  AssignIPv6Addresses
mkAssignIPv6Addresses pNetworkInterfaceId_ =
  AssignIPv6Addresses'
    { networkInterfaceId = pNetworkInterfaceId_,
      ipv6AddressCount = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaNetworkInterfaceId :: Lens.Lens' AssignIPv6Addresses Lude.Text
aiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: AssignIPv6Addresses -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: AssignIPv6Addresses)
{-# DEPRECATED aiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaIPv6AddressCount :: Lens.Lens' AssignIPv6Addresses (Lude.Maybe Lude.Int)
aiaIPv6AddressCount = Lens.lens (ipv6AddressCount :: AssignIPv6Addresses -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: AssignIPv6Addresses)
{-# DEPRECATED aiaIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaIPv6Addresses :: Lens.Lens' AssignIPv6Addresses (Lude.Maybe [Lude.Text])
aiaIPv6Addresses = Lens.lens (ipv6Addresses :: AssignIPv6Addresses -> Lude.Maybe [Lude.Text]) (\s a -> s {ipv6Addresses = a} :: AssignIPv6Addresses)
{-# DEPRECATED aiaIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.AWSRequest AssignIPv6Addresses where
  type Rs AssignIPv6Addresses = AssignIPv6AddressesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssignIPv6AddressesResponse'
            Lude.<$> (x Lude..@? "networkInterfaceId")
            Lude.<*> ( x Lude..@? "assignedIpv6Addresses" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssignIPv6Addresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssignIPv6Addresses where
  toPath = Lude.const "/"

instance Lude.ToQuery AssignIPv6Addresses where
  toQuery AssignIPv6Addresses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssignIpv6Addresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "Ipv6AddressCount" Lude.=: ipv6AddressCount,
        Lude.toQuery
          (Lude.toQueryList "Ipv6Addresses" Lude.<$> ipv6Addresses)
      ]

-- | /See:/ 'mkAssignIPv6AddressesResponse' smart constructor.
data AssignIPv6AddressesResponse = AssignIPv6AddressesResponse'
  { -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The IPv6 addresses assigned to the network interface.
    assignedIPv6Addresses :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignIPv6AddressesResponse' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'assignedIPv6Addresses' - The IPv6 addresses assigned to the network interface.
-- * 'responseStatus' - The response status code.
mkAssignIPv6AddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssignIPv6AddressesResponse
mkAssignIPv6AddressesResponse pResponseStatus_ =
  AssignIPv6AddressesResponse'
    { networkInterfaceId = Lude.Nothing,
      assignedIPv6Addresses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsNetworkInterfaceId :: Lens.Lens' AssignIPv6AddressesResponse (Lude.Maybe Lude.Text)
aiarsNetworkInterfaceId = Lens.lens (networkInterfaceId :: AssignIPv6AddressesResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: AssignIPv6AddressesResponse)
{-# DEPRECATED aiarsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The IPv6 addresses assigned to the network interface.
--
-- /Note:/ Consider using 'assignedIPv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsAssignedIPv6Addresses :: Lens.Lens' AssignIPv6AddressesResponse (Lude.Maybe [Lude.Text])
aiarsAssignedIPv6Addresses = Lens.lens (assignedIPv6Addresses :: AssignIPv6AddressesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {assignedIPv6Addresses = a} :: AssignIPv6AddressesResponse)
{-# DEPRECATED aiarsAssignedIPv6Addresses "Use generic-lens or generic-optics with 'assignedIPv6Addresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsResponseStatus :: Lens.Lens' AssignIPv6AddressesResponse Lude.Int
aiarsResponseStatus = Lens.lens (responseStatus :: AssignIPv6AddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssignIPv6AddressesResponse)
{-# DEPRECATED aiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
