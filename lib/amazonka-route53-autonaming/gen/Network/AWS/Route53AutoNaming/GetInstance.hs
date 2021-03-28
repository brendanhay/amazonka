{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified instance.
module Network.AWS.Route53AutoNaming.GetInstance
    (
    -- * Creating a request
      GetInstance (..)
    , mkGetInstance
    -- ** Request lenses
    , giServiceId
    , giInstanceId

    -- * Destructuring the response
    , GetInstanceResponse (..)
    , mkGetInstanceResponse
    -- ** Response lenses
    , girrsInstance
    , girrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkGetInstance' smart constructor.
data GetInstance = GetInstance'
  { serviceId :: Types.ResourceId
    -- ^ The ID of the service that the instance is associated with.
  , instanceId :: Types.ResourceId
    -- ^ The ID of the instance that you want to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstance' value with any optional fields omitted.
mkGetInstance
    :: Types.ResourceId -- ^ 'serviceId'
    -> Types.ResourceId -- ^ 'instanceId'
    -> GetInstance
mkGetInstance serviceId instanceId
  = GetInstance'{serviceId, instanceId}

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giServiceId :: Lens.Lens' GetInstance Types.ResourceId
giServiceId = Lens.field @"serviceId"
{-# INLINEABLE giServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The ID of the instance that you want to get information about.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInstanceId :: Lens.Lens' GetInstance Types.ResourceId
giInstanceId = Lens.field @"instanceId"
{-# INLINEABLE giInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery GetInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstance where
        toHeaders GetInstance{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.GetInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstance where
        toJSON GetInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServiceId" Core..= serviceId),
                  Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest GetInstance where
        type Rs GetInstance = GetInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceResponse' Core.<$>
                   (x Core..:? "Instance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { instance' :: Core.Maybe Types.Instance
    -- ^ A complex type that contains information about a specified instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceResponse' value with any optional fields omitted.
mkGetInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceResponse
mkGetInstanceResponse responseStatus
  = GetInstanceResponse'{instance' = Core.Nothing, responseStatus}

-- | A complex type that contains information about a specified instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsInstance :: Lens.Lens' GetInstanceResponse (Core.Maybe Types.Instance)
girrsInstance = Lens.field @"instance'"
{-# INLINEABLE girrsInstance #-}
{-# DEPRECATED instance' "Use generic-lens or generic-optics with 'instance'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetInstanceResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE girrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
