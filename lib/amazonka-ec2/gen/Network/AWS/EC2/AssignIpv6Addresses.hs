{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignIpv6Addresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more IPv6 addresses to the specified network interface. You can specify one or more specific IPv6 addresses, or you can specify the number of IPv6 addresses to be automatically assigned from within the subnet's IPv6 CIDR block range. You can assign as many IPv6 addresses to a network interface as you can assign private IPv4 addresses, and the limit varies per instance type. For information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per Network Interface Per Instance Type> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify either the IPv6 addresses or the IPv6 address count in the request.
module Network.AWS.EC2.AssignIpv6Addresses
    (
    -- * Creating a request
      AssignIpv6Addresses (..)
    , mkAssignIpv6Addresses
    -- ** Request lenses
    , aiaNetworkInterfaceId
    , aiaIpv6AddressCount
    , aiaIpv6Addresses

    -- * Destructuring the response
    , AssignIpv6AddressesResponse (..)
    , mkAssignIpv6AddressesResponse
    -- ** Response lenses
    , aiarrsAssignedIpv6Addresses
    , aiarrsNetworkInterfaceId
    , aiarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssignIpv6Addresses' smart constructor.
data AssignIpv6Addresses = AssignIpv6Addresses'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , ipv6AddressCount :: Core.Maybe Core.Int
    -- ^ The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
  , ipv6Addresses :: Core.Maybe [Core.Text]
    -- ^ One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignIpv6Addresses' value with any optional fields omitted.
mkAssignIpv6Addresses
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> AssignIpv6Addresses
mkAssignIpv6Addresses networkInterfaceId
  = AssignIpv6Addresses'{networkInterfaceId,
                         ipv6AddressCount = Core.Nothing, ipv6Addresses = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaNetworkInterfaceId :: Lens.Lens' AssignIpv6Addresses Types.NetworkInterfaceId
aiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE aiaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The number of IPv6 addresses to assign to the network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaIpv6AddressCount :: Lens.Lens' AssignIpv6Addresses (Core.Maybe Core.Int)
aiaIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# INLINEABLE aiaIpv6AddressCount #-}
{-# DEPRECATED ipv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead"  #-}

-- | One or more specific IPv6 addresses to be assigned to the network interface. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaIpv6Addresses :: Lens.Lens' AssignIpv6Addresses (Core.Maybe [Core.Text])
aiaIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE aiaIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

instance Core.ToQuery AssignIpv6Addresses where
        toQuery AssignIpv6Addresses{..}
          = Core.toQueryPair "Action" ("AssignIpv6Addresses" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6AddressCount")
                ipv6AddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Ipv6Addresses")
                ipv6Addresses

instance Core.ToHeaders AssignIpv6Addresses where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssignIpv6Addresses where
        type Rs AssignIpv6Addresses = AssignIpv6AddressesResponse
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
                 AssignIpv6AddressesResponse' Core.<$>
                   (x Core..@? "assignedIpv6Addresses" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "networkInterfaceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssignIpv6AddressesResponse' smart constructor.
data AssignIpv6AddressesResponse = AssignIpv6AddressesResponse'
  { assignedIpv6Addresses :: Core.Maybe [Core.Text]
    -- ^ The IPv6 addresses assigned to the network interface.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignIpv6AddressesResponse' value with any optional fields omitted.
mkAssignIpv6AddressesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssignIpv6AddressesResponse
mkAssignIpv6AddressesResponse responseStatus
  = AssignIpv6AddressesResponse'{assignedIpv6Addresses =
                                   Core.Nothing,
                                 networkInterfaceId = Core.Nothing, responseStatus}

-- | The IPv6 addresses assigned to the network interface.
--
-- /Note:/ Consider using 'assignedIpv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsAssignedIpv6Addresses :: Lens.Lens' AssignIpv6AddressesResponse (Core.Maybe [Core.Text])
aiarrsAssignedIpv6Addresses = Lens.field @"assignedIpv6Addresses"
{-# INLINEABLE aiarrsAssignedIpv6Addresses #-}
{-# DEPRECATED assignedIpv6Addresses "Use generic-lens or generic-optics with 'assignedIpv6Addresses' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsNetworkInterfaceId :: Lens.Lens' AssignIpv6AddressesResponse (Core.Maybe Core.Text)
aiarrsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE aiarrsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsResponseStatus :: Lens.Lens' AssignIpv6AddressesResponse Core.Int
aiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
