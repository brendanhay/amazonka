{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses from a network interface.
module Network.AWS.EC2.UnassignPrivateIpAddresses
    (
    -- * Creating a request
      UnassignPrivateIpAddresses (..)
    , mkUnassignPrivateIpAddresses
    -- ** Request lenses
    , upiaNetworkInterfaceId
    , upiaPrivateIpAddresses

    -- * Destructuring the response
    , UnassignPrivateIpAddressesResponse (..)
    , mkUnassignPrivateIpAddressesResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for UnassignPrivateIpAddresses.
--
-- /See:/ 'mkUnassignPrivateIpAddresses' smart constructor.
data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , privateIpAddresses :: [Core.Text]
    -- ^ The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignPrivateIpAddresses' value with any optional fields omitted.
mkUnassignPrivateIpAddresses
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> UnassignPrivateIpAddresses
mkUnassignPrivateIpAddresses networkInterfaceId
  = UnassignPrivateIpAddresses'{networkInterfaceId,
                                privateIpAddresses = Core.mempty}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upiaNetworkInterfaceId :: Lens.Lens' UnassignPrivateIpAddresses Types.NetworkInterfaceId
upiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE upiaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upiaPrivateIpAddresses :: Lens.Lens' UnassignPrivateIpAddresses [Core.Text]
upiaPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE upiaPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

instance Core.ToQuery UnassignPrivateIpAddresses where
        toQuery UnassignPrivateIpAddresses{..}
          = Core.toQueryPair "Action"
              ("UnassignPrivateIpAddresses" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<> Core.toQueryList "PrivateIpAddress" privateIpAddresses

instance Core.ToHeaders UnassignPrivateIpAddresses where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UnassignPrivateIpAddresses where
        type Rs UnassignPrivateIpAddresses =
             UnassignPrivateIpAddressesResponse
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
          = Response.receiveNull UnassignPrivateIpAddressesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnassignPrivateIpAddressesResponse' smart constructor.
data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignPrivateIpAddressesResponse' value with any optional fields omitted.
mkUnassignPrivateIpAddressesResponse
    :: UnassignPrivateIpAddressesResponse
mkUnassignPrivateIpAddressesResponse
  = UnassignPrivateIpAddressesResponse'
