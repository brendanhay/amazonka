{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DeleteHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use 'DescribeClusters' .
module Network.AWS.CloudHSMv2.DeleteHsm
    (
    -- * Creating a request
      DeleteHsm (..)
    , mkDeleteHsm
    -- ** Request lenses
    , dhClusterId
    , dhEniId
    , dhEniIp
    , dhHsmId

    -- * Destructuring the response
    , DeleteHsmResponse (..)
    , mkDeleteHsmResponse
    -- ** Response lenses
    , dhrrsHsmId
    , dhrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteHsm' smart constructor.
data DeleteHsm = DeleteHsm'
  { clusterId :: Types.ClusterId
    -- ^ The identifier (ID) of the cluster that contains the HSM that you are deleting.
  , eniId :: Core.Maybe Types.EniId
    -- ^ The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
  , eniIp :: Core.Maybe Types.IpAddress
    -- ^ The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
  , hsmId :: Core.Maybe Types.HsmId
    -- ^ The identifier (ID) of the HSM that you are deleting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsm' value with any optional fields omitted.
mkDeleteHsm
    :: Types.ClusterId -- ^ 'clusterId'
    -> DeleteHsm
mkDeleteHsm clusterId
  = DeleteHsm'{clusterId, eniId = Core.Nothing, eniIp = Core.Nothing,
               hsmId = Core.Nothing}

-- | The identifier (ID) of the cluster that contains the HSM that you are deleting.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhClusterId :: Lens.Lens' DeleteHsm Types.ClusterId
dhClusterId = Lens.field @"clusterId"
{-# INLINEABLE dhClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhEniId :: Lens.Lens' DeleteHsm (Core.Maybe Types.EniId)
dhEniId = Lens.field @"eniId"
{-# INLINEABLE dhEniId #-}
{-# DEPRECATED eniId "Use generic-lens or generic-optics with 'eniId' instead"  #-}

-- | The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'eniIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhEniIp :: Lens.Lens' DeleteHsm (Core.Maybe Types.IpAddress)
dhEniIp = Lens.field @"eniIp"
{-# INLINEABLE dhEniIp #-}
{-# DEPRECATED eniIp "Use generic-lens or generic-optics with 'eniIp' instead"  #-}

-- | The identifier (ID) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHsmId :: Lens.Lens' DeleteHsm (Core.Maybe Types.HsmId)
dhHsmId = Lens.field @"hsmId"
{-# INLINEABLE dhHsmId #-}
{-# DEPRECATED hsmId "Use generic-lens or generic-optics with 'hsmId' instead"  #-}

instance Core.ToQuery DeleteHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHsm where
        toHeaders DeleteHsm{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.DeleteHsm") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteHsm where
        toJSON DeleteHsm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("EniId" Core..=) Core.<$> eniId, ("EniIp" Core..=) Core.<$> eniIp,
                  ("HsmId" Core..=) Core.<$> hsmId])

instance Core.AWSRequest DeleteHsm where
        type Rs DeleteHsm = DeleteHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteHsmResponse' Core.<$>
                   (x Core..:? "HsmId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { hsmId :: Core.Maybe Types.HsmId
    -- ^ The identifier (ID) of the HSM that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmResponse' value with any optional fields omitted.
mkDeleteHsmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteHsmResponse
mkDeleteHsmResponse responseStatus
  = DeleteHsmResponse'{hsmId = Core.Nothing, responseStatus}

-- | The identifier (ID) of the HSM that was deleted.
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsHsmId :: Lens.Lens' DeleteHsmResponse (Core.Maybe Types.HsmId)
dhrrsHsmId = Lens.field @"hsmId"
{-# INLINEABLE dhrrsHsmId #-}
{-# DEPRECATED hsmId "Use generic-lens or generic-optics with 'hsmId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DeleteHsmResponse Core.Int
dhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
