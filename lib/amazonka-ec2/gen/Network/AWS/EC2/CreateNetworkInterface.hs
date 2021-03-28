{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateNetworkInterface (..)
    , mkCreateNetworkInterface
    -- ** Request lenses
    , cniSubnetId
    , cniDescription
    , cniDryRun
    , cniGroups
    , cniInterfaceType
    , cniIpv6AddressCount
    , cniIpv6Addresses
    , cniPrivateIpAddress
    , cniPrivateIpAddresses
    , cniSecondaryPrivateIpAddressCount
    , cniTagSpecifications

    -- * Destructuring the response
    , CreateNetworkInterfaceResponse (..)
    , mkCreateNetworkInterfaceResponse
    -- ** Response lenses
    , cnirrsNetworkInterface
    , cnirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateNetworkInterface.
--
-- /See:/ 'mkCreateNetworkInterface' smart constructor.
data CreateNetworkInterface = CreateNetworkInterface'
  { subnetId :: Types.SubnetId
    -- ^ The ID of the subnet to associate with the network interface.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the network interface.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of one or more security groups.
  , interfaceType :: Core.Maybe Types.NetworkInterfaceCreationType
    -- ^ Indicates the type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
  , ipv6AddressCount :: Core.Maybe Core.Int
    -- ^ The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses. If your subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@ , you can specify @0@ to override this setting.
  , ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address]
    -- ^ One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The primary private IPv4 address of the network interface. If you don't specify an IPv4 address, Amazon EC2 selects one for you from the subnet's IPv4 CIDR range. If you specify an IP address, you cannot indicate any IP addresses specified in @privateIpAddresses@ as primary (only one IP address can be designated as primary).
  , privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressSpecification]
    -- ^ One or more private IPv4 addresses.
  , secondaryPrivateIpAddressCount :: Core.Maybe Core.Int
    -- ^ The number of secondary private IPv4 addresses to assign to a network interface. When you specify a number of secondary IPv4 addresses, Amazon EC2 selects these IP addresses within the subnet's IPv4 CIDR range. You can't specify this option and specify more than one private IP address using @privateIpAddresses@ .
--
-- The number of IP addresses you can assign to a network interface varies by instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type> in the /Amazon Virtual Private Cloud User Guide/ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the new network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkInterface' value with any optional fields omitted.
mkCreateNetworkInterface
    :: Types.SubnetId -- ^ 'subnetId'
    -> CreateNetworkInterface
mkCreateNetworkInterface subnetId
  = CreateNetworkInterface'{subnetId, description = Core.Nothing,
                            dryRun = Core.Nothing, groups = Core.Nothing,
                            interfaceType = Core.Nothing, ipv6AddressCount = Core.Nothing,
                            ipv6Addresses = Core.Nothing, privateIpAddress = Core.Nothing,
                            privateIpAddresses = Core.Nothing,
                            secondaryPrivateIpAddressCount = Core.Nothing,
                            tagSpecifications = Core.Nothing}

-- | The ID of the subnet to associate with the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSubnetId :: Lens.Lens' CreateNetworkInterface Types.SubnetId
cniSubnetId = Lens.field @"subnetId"
{-# INLINEABLE cniSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDescription :: Lens.Lens' CreateNetworkInterface (Core.Maybe Core.Text)
cniDescription = Lens.field @"description"
{-# INLINEABLE cniDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDryRun :: Lens.Lens' CreateNetworkInterface (Core.Maybe Core.Bool)
cniDryRun = Lens.field @"dryRun"
{-# INLINEABLE cniDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniGroups :: Lens.Lens' CreateNetworkInterface (Core.Maybe [Types.SecurityGroupId])
cniGroups = Lens.field @"groups"
{-# INLINEABLE cniGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | Indicates the type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniInterfaceType :: Lens.Lens' CreateNetworkInterface (Core.Maybe Types.NetworkInterfaceCreationType)
cniInterfaceType = Lens.field @"interfaceType"
{-# INLINEABLE cniInterfaceType #-}
{-# DEPRECATED interfaceType "Use generic-lens or generic-optics with 'interfaceType' instead"  #-}

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses. If your subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@ , you can specify @0@ to override this setting.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniIpv6AddressCount :: Lens.Lens' CreateNetworkInterface (Core.Maybe Core.Int)
cniIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# INLINEABLE cniIpv6AddressCount #-}
{-# DEPRECATED ipv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead"  #-}

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniIpv6Addresses :: Lens.Lens' CreateNetworkInterface (Core.Maybe [Types.InstanceIpv6Address])
cniIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE cniIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The primary private IPv4 address of the network interface. If you don't specify an IPv4 address, Amazon EC2 selects one for you from the subnet's IPv4 CIDR range. If you specify an IP address, you cannot indicate any IP addresses specified in @privateIpAddresses@ as primary (only one IP address can be designated as primary).
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniPrivateIpAddress :: Lens.Lens' CreateNetworkInterface (Core.Maybe Core.Text)
cniPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE cniPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniPrivateIpAddresses :: Lens.Lens' CreateNetworkInterface (Core.Maybe [Types.PrivateIpAddressSpecification])
cniPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE cniPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

-- | The number of secondary private IPv4 addresses to assign to a network interface. When you specify a number of secondary IPv4 addresses, Amazon EC2 selects these IP addresses within the subnet's IPv4 CIDR range. You can't specify this option and specify more than one private IP address using @privateIpAddresses@ .
--
-- The number of IP addresses you can assign to a network interface varies by instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSecondaryPrivateIpAddressCount :: Lens.Lens' CreateNetworkInterface (Core.Maybe Core.Int)
cniSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# INLINEABLE cniSecondaryPrivateIpAddressCount #-}
{-# DEPRECATED secondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead"  #-}

-- | The tags to apply to the new network interface.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniTagSpecifications :: Lens.Lens' CreateNetworkInterface (Core.Maybe [Types.TagSpecification])
cniTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cniTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateNetworkInterface where
        toQuery CreateNetworkInterface{..}
          = Core.toQueryPair "Action" ("CreateNetworkInterface" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "SubnetId" subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId") groups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InterfaceType")
                interfaceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6AddressCount")
                ipv6AddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Ipv6Addresses")
                ipv6Addresses
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "PrivateIpAddresses")
                privateIpAddresses
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SecondaryPrivateIpAddressCount")
                secondaryPrivateIpAddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateNetworkInterface where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateNetworkInterface where
        type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateNetworkInterfaceResponse' Core.<$>
                   (x Core..@? "networkInterface") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateNetworkInterface.
--
-- /See:/ 'mkCreateNetworkInterfaceResponse' smart constructor.
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
  { networkInterface :: Core.Maybe Types.NetworkInterface
    -- ^ Information about the network interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateNetworkInterfaceResponse' value with any optional fields omitted.
mkCreateNetworkInterfaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNetworkInterfaceResponse
mkCreateNetworkInterfaceResponse responseStatus
  = CreateNetworkInterfaceResponse'{networkInterface = Core.Nothing,
                                    responseStatus}

-- | Information about the network interface.
--
-- /Note:/ Consider using 'networkInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirrsNetworkInterface :: Lens.Lens' CreateNetworkInterfaceResponse (Core.Maybe Types.NetworkInterface)
cnirrsNetworkInterface = Lens.field @"networkInterface"
{-# INLINEABLE cnirrsNetworkInterface #-}
{-# DEPRECATED networkInterface "Use generic-lens or generic-optics with 'networkInterface' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirrsResponseStatus :: Lens.Lens' CreateNetworkInterfaceResponse Core.Int
cnirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
