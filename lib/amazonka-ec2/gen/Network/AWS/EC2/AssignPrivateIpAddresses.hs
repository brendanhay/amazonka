{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignPrivateIpAddresses
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
module Network.AWS.EC2.AssignPrivateIpAddresses
    (
    -- * Creating a request
      AssignPrivateIpAddresses (..)
    , mkAssignPrivateIpAddresses
    -- ** Request lenses
    , apiaNetworkInterfaceId
    , apiaAllowReassignment
    , apiaPrivateIpAddresses
    , apiaSecondaryPrivateIpAddressCount

    -- * Destructuring the response
    , AssignPrivateIpAddressesResponse (..)
    , mkAssignPrivateIpAddressesResponse
    -- ** Response lenses
    , apiarrsAssignedPrivateIpAddresses
    , apiarrsNetworkInterfaceId
    , apiarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AssignPrivateIpAddresses.
--
-- /See:/ 'mkAssignPrivateIpAddresses' smart constructor.
data AssignPrivateIpAddresses = AssignPrivateIpAddresses'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , allowReassignment :: Core.Maybe Core.Bool
    -- ^ Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
  , privateIpAddresses :: Core.Maybe [Core.Text]
    -- ^ One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses.
--
-- If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
  , secondaryPrivateIpAddressCount :: Core.Maybe Core.Int
    -- ^ The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignPrivateIpAddresses' value with any optional fields omitted.
mkAssignPrivateIpAddresses
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> AssignPrivateIpAddresses
mkAssignPrivateIpAddresses networkInterfaceId
  = AssignPrivateIpAddresses'{networkInterfaceId,
                              allowReassignment = Core.Nothing,
                              privateIpAddresses = Core.Nothing,
                              secondaryPrivateIpAddressCount = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaNetworkInterfaceId :: Lens.Lens' AssignPrivateIpAddresses Types.NetworkInterfaceId
apiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE apiaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | Indicates whether to allow an IP address that is already assigned to another network interface or instance to be reassigned to the specified network interface.
--
-- /Note:/ Consider using 'allowReassignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaAllowReassignment :: Lens.Lens' AssignPrivateIpAddresses (Core.Maybe Core.Bool)
apiaAllowReassignment = Lens.field @"allowReassignment"
{-# INLINEABLE apiaAllowReassignment #-}
{-# DEPRECATED allowReassignment "Use generic-lens or generic-optics with 'allowReassignment' instead"  #-}

-- | One or more IP addresses to be assigned as a secondary private IP address to the network interface. You can't specify this parameter when also specifying a number of secondary IP addresses.
--
-- If you don't specify an IP address, Amazon EC2 automatically selects an IP address within the subnet range.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaPrivateIpAddresses :: Lens.Lens' AssignPrivateIpAddresses (Core.Maybe [Core.Text])
apiaPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE apiaPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

-- | The number of secondary IP addresses to assign to the network interface. You can't specify this parameter when also specifying private IP addresses.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaSecondaryPrivateIpAddressCount :: Lens.Lens' AssignPrivateIpAddresses (Core.Maybe Core.Int)
apiaSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# INLINEABLE apiaSecondaryPrivateIpAddressCount #-}
{-# DEPRECATED secondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead"  #-}

instance Core.ToQuery AssignPrivateIpAddresses where
        toQuery AssignPrivateIpAddresses{..}
          = Core.toQueryPair "Action"
              ("AssignPrivateIpAddresses" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllowReassignment")
                allowReassignment
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "PrivateIpAddress")
                privateIpAddresses
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SecondaryPrivateIpAddressCount")
                secondaryPrivateIpAddressCount

instance Core.ToHeaders AssignPrivateIpAddresses where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssignPrivateIpAddresses where
        type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse
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
                 AssignPrivateIpAddressesResponse' Core.<$>
                   (x Core..@? "assignedPrivateIpAddressesSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "networkInterfaceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssignPrivateIpAddressesResponse' smart constructor.
data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse'
  { assignedPrivateIpAddresses :: Core.Maybe [Types.AssignedPrivateIpAddress]
    -- ^ The private IP addresses assigned to the network interface.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignPrivateIpAddressesResponse' value with any optional fields omitted.
mkAssignPrivateIpAddressesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssignPrivateIpAddressesResponse
mkAssignPrivateIpAddressesResponse responseStatus
  = AssignPrivateIpAddressesResponse'{assignedPrivateIpAddresses =
                                        Core.Nothing,
                                      networkInterfaceId = Core.Nothing, responseStatus}

-- | The private IP addresses assigned to the network interface.
--
-- /Note:/ Consider using 'assignedPrivateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarrsAssignedPrivateIpAddresses :: Lens.Lens' AssignPrivateIpAddressesResponse (Core.Maybe [Types.AssignedPrivateIpAddress])
apiarrsAssignedPrivateIpAddresses = Lens.field @"assignedPrivateIpAddresses"
{-# INLINEABLE apiarrsAssignedPrivateIpAddresses #-}
{-# DEPRECATED assignedPrivateIpAddresses "Use generic-lens or generic-optics with 'assignedPrivateIpAddresses' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarrsNetworkInterfaceId :: Lens.Lens' AssignPrivateIpAddressesResponse (Core.Maybe Core.Text)
apiarrsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE apiarrsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiarrsResponseStatus :: Lens.Lens' AssignPrivateIpAddressesResponse Core.Int
apiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE apiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
