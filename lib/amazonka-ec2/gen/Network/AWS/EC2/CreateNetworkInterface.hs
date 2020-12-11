{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network interface in the specified subnet.
--
-- For more information about network interfaces, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNetworkInterface
  ( -- * Creating a request
    CreateNetworkInterface (..),
    mkCreateNetworkInterface,

    -- ** Request lenses
    cniGroups,
    cniPrivateIPAddresses,
    cniInterfaceType,
    cniTagSpecifications,
    cniIPv6AddressCount,
    cniPrivateIPAddress,
    cniSecondaryPrivateIPAddressCount,
    cniDescription,
    cniDryRun,
    cniIPv6Addresses,
    cniSubnetId,

    -- * Destructuring the response
    CreateNetworkInterfaceResponse (..),
    mkCreateNetworkInterfaceResponse,

    -- ** Response lenses
    cnirsNetworkInterface,
    cnirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateNetworkInterface.
--
-- /See:/ 'mkCreateNetworkInterface' smart constructor.
data CreateNetworkInterface = CreateNetworkInterface'
  { groups ::
      Lude.Maybe [Lude.Text],
    privateIPAddresses ::
      Lude.Maybe [PrivateIPAddressSpecification],
    interfaceType ::
      Lude.Maybe NetworkInterfaceCreationType,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    ipv6AddressCount :: Lude.Maybe Lude.Int,
    privateIPAddress :: Lude.Maybe Lude.Text,
    secondaryPrivateIPAddressCount ::
      Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    ipv6Addresses ::
      Lude.Maybe [InstanceIPv6Address],
    subnetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkInterface' with the minimum fields required to make a request.
--
-- * 'description' - A description for the network interface.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groups' - The IDs of one or more security groups.
-- * 'interfaceType' - Indicates the type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses. If your subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@ , you can specify @0@ to override this setting.
-- * 'ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
-- * 'privateIPAddress' - The primary private IPv4 address of the network interface. If you don't specify an IPv4 address, Amazon EC2 selects one for you from the subnet's IPv4 CIDR range. If you specify an IP address, you cannot indicate any IP addresses specified in @privateIpAddresses@ as primary (only one IP address can be designated as primary).
-- * 'privateIPAddresses' - One or more private IPv4 addresses.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses to assign to a network interface. When you specify a number of secondary IPv4 addresses, Amazon EC2 selects these IP addresses within the subnet's IPv4 CIDR range. You can't specify this option and specify more than one private IP address using @privateIpAddresses@ .
--
-- The number of IP addresses you can assign to a network interface varies by instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type> in the /Amazon Virtual Private Cloud User Guide/ .
-- * 'subnetId' - The ID of the subnet to associate with the network interface.
-- * 'tagSpecifications' - The tags to apply to the new network interface.
mkCreateNetworkInterface ::
  -- | 'subnetId'
  Lude.Text ->
  CreateNetworkInterface
mkCreateNetworkInterface pSubnetId_ =
  CreateNetworkInterface'
    { groups = Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      interfaceType = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      secondaryPrivateIPAddressCount = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      ipv6Addresses = Lude.Nothing,
      subnetId = pSubnetId_
    }

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniGroups :: Lens.Lens' CreateNetworkInterface (Lude.Maybe [Lude.Text])
cniGroups = Lens.lens (groups :: CreateNetworkInterface -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: CreateNetworkInterface)
{-# DEPRECATED cniGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniPrivateIPAddresses :: Lens.Lens' CreateNetworkInterface (Lude.Maybe [PrivateIPAddressSpecification])
cniPrivateIPAddresses = Lens.lens (privateIPAddresses :: CreateNetworkInterface -> Lude.Maybe [PrivateIPAddressSpecification]) (\s a -> s {privateIPAddresses = a} :: CreateNetworkInterface)
{-# DEPRECATED cniPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | Indicates the type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniInterfaceType :: Lens.Lens' CreateNetworkInterface (Lude.Maybe NetworkInterfaceCreationType)
cniInterfaceType = Lens.lens (interfaceType :: CreateNetworkInterface -> Lude.Maybe NetworkInterfaceCreationType) (\s a -> s {interfaceType = a} :: CreateNetworkInterface)
{-# DEPRECATED cniInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The tags to apply to the new network interface.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniTagSpecifications :: Lens.Lens' CreateNetworkInterface (Lude.Maybe [TagSpecification])
cniTagSpecifications = Lens.lens (tagSpecifications :: CreateNetworkInterface -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateNetworkInterface)
{-# DEPRECATED cniTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses. If your subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@ , you can specify @0@ to override this setting.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniIPv6AddressCount :: Lens.Lens' CreateNetworkInterface (Lude.Maybe Lude.Int)
cniIPv6AddressCount = Lens.lens (ipv6AddressCount :: CreateNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: CreateNetworkInterface)
{-# DEPRECATED cniIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The primary private IPv4 address of the network interface. If you don't specify an IPv4 address, Amazon EC2 selects one for you from the subnet's IPv4 CIDR range. If you specify an IP address, you cannot indicate any IP addresses specified in @privateIpAddresses@ as primary (only one IP address can be designated as primary).
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniPrivateIPAddress :: Lens.Lens' CreateNetworkInterface (Lude.Maybe Lude.Text)
cniPrivateIPAddress = Lens.lens (privateIPAddress :: CreateNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: CreateNetworkInterface)
{-# DEPRECATED cniPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The number of secondary private IPv4 addresses to assign to a network interface. When you specify a number of secondary IPv4 addresses, Amazon EC2 selects these IP addresses within the subnet's IPv4 CIDR range. You can't specify this option and specify more than one private IP address using @privateIpAddresses@ .
--
-- The number of IP addresses you can assign to a network interface varies by instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSecondaryPrivateIPAddressCount :: Lens.Lens' CreateNetworkInterface (Lude.Maybe Lude.Int)
cniSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: CreateNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: CreateNetworkInterface)
{-# DEPRECATED cniSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDescription :: Lens.Lens' CreateNetworkInterface (Lude.Maybe Lude.Text)
cniDescription = Lens.lens (description :: CreateNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateNetworkInterface)
{-# DEPRECATED cniDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDryRun :: Lens.Lens' CreateNetworkInterface (Lude.Maybe Lude.Bool)
cniDryRun = Lens.lens (dryRun :: CreateNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateNetworkInterface)
{-# DEPRECATED cniDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniIPv6Addresses :: Lens.Lens' CreateNetworkInterface (Lude.Maybe [InstanceIPv6Address])
cniIPv6Addresses = Lens.lens (ipv6Addresses :: CreateNetworkInterface -> Lude.Maybe [InstanceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: CreateNetworkInterface)
{-# DEPRECATED cniIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The ID of the subnet to associate with the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSubnetId :: Lens.Lens' CreateNetworkInterface Lude.Text
cniSubnetId = Lens.lens (subnetId :: CreateNetworkInterface -> Lude.Text) (\s a -> s {subnetId = a} :: CreateNetworkInterface)
{-# DEPRECATED cniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.AWSRequest CreateNetworkInterface where
  type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateNetworkInterfaceResponse'
            Lude.<$> (x Lude..@? "networkInterface")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNetworkInterface where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateNetworkInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkInterface where
  toQuery CreateNetworkInterface' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateNetworkInterface" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "SecurityGroupId" Lude.<$> groups),
        Lude.toQuery
          ( Lude.toQueryList "PrivateIpAddresses"
              Lude.<$> privateIPAddresses
          ),
        "InterfaceType" Lude.=: interfaceType,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Ipv6AddressCount" Lude.=: ipv6AddressCount,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "SecondaryPrivateIpAddressCount"
          Lude.=: secondaryPrivateIPAddressCount,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        Lude.toQuery
          (Lude.toQueryList "Ipv6Addresses" Lude.<$> ipv6Addresses),
        "SubnetId" Lude.=: subnetId
      ]

-- | Contains the output of CreateNetworkInterface.
--
-- /See:/ 'mkCreateNetworkInterfaceResponse' smart constructor.
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
  { networkInterface ::
      Lude.Maybe NetworkInterface,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkInterfaceResponse' with the minimum fields required to make a request.
--
-- * 'networkInterface' - Information about the network interface.
-- * 'responseStatus' - The response status code.
mkCreateNetworkInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNetworkInterfaceResponse
mkCreateNetworkInterfaceResponse pResponseStatus_ =
  CreateNetworkInterfaceResponse'
    { networkInterface = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the network interface.
--
-- /Note:/ Consider using 'networkInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirsNetworkInterface :: Lens.Lens' CreateNetworkInterfaceResponse (Lude.Maybe NetworkInterface)
cnirsNetworkInterface = Lens.lens (networkInterface :: CreateNetworkInterfaceResponse -> Lude.Maybe NetworkInterface) (\s a -> s {networkInterface = a} :: CreateNetworkInterfaceResponse)
{-# DEPRECATED cnirsNetworkInterface "Use generic-lens or generic-optics with 'networkInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirsResponseStatus :: Lens.Lens' CreateNetworkInterfaceResponse Lude.Int
cnirsResponseStatus = Lens.lens (responseStatus :: CreateNetworkInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNetworkInterfaceResponse)
{-# DEPRECATED cnirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
