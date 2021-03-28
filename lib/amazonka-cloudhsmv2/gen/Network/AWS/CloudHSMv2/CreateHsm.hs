{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CreateHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateHsm
    (
    -- * Creating a request
      CreateHsm (..)
    , mkCreateHsm
    -- ** Request lenses
    , chClusterId
    , chAvailabilityZone
    , chIpAddress

    -- * Destructuring the response
    , CreateHsmResponse (..)
    , mkCreateHsmResponse
    -- ** Response lenses
    , chrrsHsm
    , chrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateHsm' smart constructor.
data CreateHsm = CreateHsm'
  { clusterId :: Types.ClusterId
    -- ^ The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
  , availabilityZone :: Types.ExternalAz
    -- ^ The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsm' value with any optional fields omitted.
mkCreateHsm
    :: Types.ClusterId -- ^ 'clusterId'
    -> Types.ExternalAz -- ^ 'availabilityZone'
    -> CreateHsm
mkCreateHsm clusterId availabilityZone
  = CreateHsm'{clusterId, availabilityZone, ipAddress = Core.Nothing}

-- | The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chClusterId :: Lens.Lens' CreateHsm Types.ClusterId
chClusterId = Lens.field @"clusterId"
{-# INLINEABLE chClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chAvailabilityZone :: Lens.Lens' CreateHsm Types.ExternalAz
chAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE chAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chIpAddress :: Lens.Lens' CreateHsm (Core.Maybe Types.IpAddress)
chIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE chIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

instance Core.ToQuery CreateHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateHsm where
        toHeaders CreateHsm{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.CreateHsm") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateHsm where
        toJSON CreateHsm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  Core.Just ("AvailabilityZone" Core..= availabilityZone),
                  ("IpAddress" Core..=) Core.<$> ipAddress])

instance Core.AWSRequest CreateHsm where
        type Rs CreateHsm = CreateHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateHsmResponse' Core.<$>
                   (x Core..:? "Hsm") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateHsmResponse' smart constructor.
data CreateHsmResponse = CreateHsmResponse'
  { hsm :: Core.Maybe Types.Hsm
    -- ^ Information about the HSM that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmResponse' value with any optional fields omitted.
mkCreateHsmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHsmResponse
mkCreateHsmResponse responseStatus
  = CreateHsmResponse'{hsm = Core.Nothing, responseStatus}

-- | Information about the HSM that was created.
--
-- /Note:/ Consider using 'hsm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsHsm :: Lens.Lens' CreateHsmResponse (Core.Maybe Types.Hsm)
chrrsHsm = Lens.field @"hsm"
{-# INLINEABLE chrrsHsm #-}
{-# DEPRECATED hsm "Use generic-lens or generic-optics with 'hsm' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsResponseStatus :: Lens.Lens' CreateHsmResponse Core.Int
chrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
