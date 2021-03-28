{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnassignIpv6Addresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more IPv6 addresses from a network interface.
module Network.AWS.EC2.UnassignIpv6Addresses
    (
    -- * Creating a request
      UnassignIpv6Addresses (..)
    , mkUnassignIpv6Addresses
    -- ** Request lenses
    , uiaIpv6Addresses
    , uiaNetworkInterfaceId

    -- * Destructuring the response
    , UnassignIpv6AddressesResponse (..)
    , mkUnassignIpv6AddressesResponse
    -- ** Response lenses
    , uiarrsNetworkInterfaceId
    , uiarrsUnassignedIpv6Addresses
    , uiarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnassignIpv6Addresses' smart constructor.
data UnassignIpv6Addresses = UnassignIpv6Addresses'
  { ipv6Addresses :: [Core.Text]
    -- ^ The IPv6 addresses to unassign from the network interface.
  , networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignIpv6Addresses' value with any optional fields omitted.
mkUnassignIpv6Addresses
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> UnassignIpv6Addresses
mkUnassignIpv6Addresses networkInterfaceId
  = UnassignIpv6Addresses'{ipv6Addresses = Core.mempty,
                           networkInterfaceId}

-- | The IPv6 addresses to unassign from the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaIpv6Addresses :: Lens.Lens' UnassignIpv6Addresses [Core.Text]
uiaIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE uiaIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaNetworkInterfaceId :: Lens.Lens' UnassignIpv6Addresses Types.NetworkInterfaceId
uiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE uiaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

instance Core.ToQuery UnassignIpv6Addresses where
        toQuery UnassignIpv6Addresses{..}
          = Core.toQueryPair "Action" ("UnassignIpv6Addresses" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "Ipv6Addresses" ipv6Addresses
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId

instance Core.ToHeaders UnassignIpv6Addresses where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UnassignIpv6Addresses where
        type Rs UnassignIpv6Addresses = UnassignIpv6AddressesResponse
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
                 UnassignIpv6AddressesResponse' Core.<$>
                   (x Core..@? "networkInterfaceId") Core.<*>
                     x Core..@? "unassignedIpv6Addresses" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnassignIpv6AddressesResponse' smart constructor.
data UnassignIpv6AddressesResponse = UnassignIpv6AddressesResponse'
  { networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , unassignedIpv6Addresses :: Core.Maybe [Core.Text]
    -- ^ The IPv6 addresses that have been unassigned from the network interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignIpv6AddressesResponse' value with any optional fields omitted.
mkUnassignIpv6AddressesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UnassignIpv6AddressesResponse
mkUnassignIpv6AddressesResponse responseStatus
  = UnassignIpv6AddressesResponse'{networkInterfaceId = Core.Nothing,
                                   unassignedIpv6Addresses = Core.Nothing, responseStatus}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarrsNetworkInterfaceId :: Lens.Lens' UnassignIpv6AddressesResponse (Core.Maybe Core.Text)
uiarrsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE uiarrsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The IPv6 addresses that have been unassigned from the network interface.
--
-- /Note:/ Consider using 'unassignedIpv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarrsUnassignedIpv6Addresses :: Lens.Lens' UnassignIpv6AddressesResponse (Core.Maybe [Core.Text])
uiarrsUnassignedIpv6Addresses = Lens.field @"unassignedIpv6Addresses"
{-# INLINEABLE uiarrsUnassignedIpv6Addresses #-}
{-# DEPRECATED unassignedIpv6Addresses "Use generic-lens or generic-optics with 'unassignedIpv6Addresses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiarrsResponseStatus :: Lens.Lens' UnassignIpv6AddressesResponse Core.Int
uiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
