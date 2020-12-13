{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignPrivateIPAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more secondary private IP addresses to the specified network interface.
--
-- You can specify one or more specific secondary IP addresses, or you can specify the number of secondary IP addresses to be automatically assigned within the subnet's CIDR block range. The number of secondary IP addresses that you can assign to an instance varies by instance type. For information about instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ . For more information about Elastic IP addresses, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
-- When you move a secondary private IP address to another network interface, any Elastic IP address that is associated with the IP address is also moved.
-- Remapping an IP address is an asynchronous operation. When you move an IP address from one network interface to another, check @network/interfaces/macs/mac/local-ipv4s@ in the instance metadata to confirm that the remapping is complete.
-- You must specify either the IP addresses or the IP address count in the request.
module Network.AWS.EC2.AssignPrivateIPAddresses
  ( -- * Creating a request
    AssignPrivateIPAddresses (..),
    mkAssignPrivateIPAddresses,

    -- ** Request lenses
    apiaPrivateIPAddresses,
    apiaNetworkInterfaceId,
    apiaAllowReassignment,
    apiaSecondaryPrivateIPAddressCount,

    -- * Destructuring the response
    AssignPrivateIPAddressesResponse (..),
    mkAssignPrivateIPAddressesResponse,

    -- ** Response lenses
    apiarsAssignedPrivateIPAddresses,
    apiarsNetworkInterfaceId,
    apiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for AssignPrivateIpAddresses.
--
-- /See:/ 'mkAssignPrivateIPAddresses' smart constructor.
data AssignPrivateIPAddresses = AssignPrivateIPAddresses'
  { -- | One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses.
    --
    -- If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
    privateIPAddresses :: Lude.Maybe [Lude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Text,
    -- | Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
    allowReassignment :: Lude.Maybe Lude.Bool,
    -- | The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
    secondaryPrivateIPAddressCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignPrivateIPAddresses' with the minimum fields required to make a request.
--
-- * 'privateIPAddresses' - One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses.
--
-- If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'allowReassignment' - Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
mkAssignPrivateIPAddresses ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  AssignPrivateIPAddresses
mkAssignPrivateIPAddresses pNetworkInterfaceId_ =
  AssignPrivateIPAddresses'
    { privateIPAddresses = Lude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_,
      allowReassignment = Lude.Nothing,
      secondaryPrivateIPAddressCount = Lude.Nothing
    }

-- | One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses.
--
-- If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaPrivateIPAddresses :: Lens.Lens' AssignPrivateIPAddresses (Lude.Maybe [Lude.Text])
apiaPrivateIPAddresses = Lens.lens (privateIPAddresses :: AssignPrivateIPAddresses -> Lude.Maybe [Lude.Text]) (\s a -> s {privateIPAddresses = a} :: AssignPrivateIPAddresses)
{-# DEPRECATED apiaPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaNetworkInterfaceId :: Lens.Lens' AssignPrivateIPAddresses Lude.Text
apiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: AssignPrivateIPAddresses -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: AssignPrivateIPAddresses)
{-# DEPRECATED apiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
--
-- /Note:/ Consider using 'allowReassignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaAllowReassignment :: Lens.Lens' AssignPrivateIPAddresses (Lude.Maybe Lude.Bool)
apiaAllowReassignment = Lens.lens (allowReassignment :: AssignPrivateIPAddresses -> Lude.Maybe Lude.Bool) (\s a -> s {allowReassignment = a} :: AssignPrivateIPAddresses)
{-# DEPRECATED apiaAllowReassignment "Use generic-lens or generic-optics with 'allowReassignment' instead." #-}

-- | The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaSecondaryPrivateIPAddressCount :: Lens.Lens' AssignPrivateIPAddresses (Lude.Maybe Lude.Int)
apiaSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: AssignPrivateIPAddresses -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: AssignPrivateIPAddresses)
{-# DEPRECATED apiaSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

instance Lude.AWSRequest AssignPrivateIPAddresses where
  type Rs AssignPrivateIPAddresses = AssignPrivateIPAddressesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssignPrivateIPAddressesResponse'
            Lude.<$> ( x Lude..@? "assignedPrivateIpAddressesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "networkInterfaceId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssignPrivateIPAddresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssignPrivateIPAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery AssignPrivateIPAddresses where
  toQuery AssignPrivateIPAddresses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssignPrivateIpAddresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "PrivateIpAddress" Lude.<$> privateIPAddresses),
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "AllowReassignment" Lude.=: allowReassignment,
        "SecondaryPrivateIpAddressCount"
          Lude.=: secondaryPrivateIPAddressCount
      ]

-- | /See:/ 'mkAssignPrivateIPAddressesResponse' smart constructor.
data AssignPrivateIPAddressesResponse = AssignPrivateIPAddressesResponse'
  { -- | The private IP addresses assigned to the network interface.
    assignedPrivateIPAddresses :: Lude.Maybe [AssignedPrivateIPAddress],
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignPrivateIPAddressesResponse' with the minimum fields required to make a request.
--
-- * 'assignedPrivateIPAddresses' - The private IP addresses assigned to the network interface.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'responseStatus' - The response status code.
mkAssignPrivateIPAddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssignPrivateIPAddressesResponse
mkAssignPrivateIPAddressesResponse pResponseStatus_ =
  AssignPrivateIPAddressesResponse'
    { assignedPrivateIPAddresses =
        Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The private IP addresses assigned to the network interface.
--
-- /Note:/ Consider using 'assignedPrivateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarsAssignedPrivateIPAddresses :: Lens.Lens' AssignPrivateIPAddressesResponse (Lude.Maybe [AssignedPrivateIPAddress])
apiarsAssignedPrivateIPAddresses = Lens.lens (assignedPrivateIPAddresses :: AssignPrivateIPAddressesResponse -> Lude.Maybe [AssignedPrivateIPAddress]) (\s a -> s {assignedPrivateIPAddresses = a} :: AssignPrivateIPAddressesResponse)
{-# DEPRECATED apiarsAssignedPrivateIPAddresses "Use generic-lens or generic-optics with 'assignedPrivateIPAddresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarsNetworkInterfaceId :: Lens.Lens' AssignPrivateIPAddressesResponse (Lude.Maybe Lude.Text)
apiarsNetworkInterfaceId = Lens.lens (networkInterfaceId :: AssignPrivateIPAddressesResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: AssignPrivateIPAddressesResponse)
{-# DEPRECATED apiarsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarsResponseStatus :: Lens.Lens' AssignPrivateIPAddressesResponse Lude.Int
apiarsResponseStatus = Lens.lens (responseStatus :: AssignPrivateIPAddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssignPrivateIPAddressesResponse)
{-# DEPRECATED apiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
