{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a specific instance. Works on one instance at a time.
module Network.AWS.Lightsail.GetInstanceState
    (
    -- * Creating a request
      GetInstanceState (..)
    , mkGetInstanceState
    -- ** Request lenses
    , gisInstanceName

    -- * Destructuring the response
    , GetInstanceStateResponse (..)
    , mkGetInstanceStateResponse
    -- ** Response lenses
    , gisrgrsState
    , gisrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceState' smart constructor.
newtype GetInstanceState = GetInstanceState'
  { instanceName :: Types.ResourceName
    -- ^ The name of the instance to get state information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceState' value with any optional fields omitted.
mkGetInstanceState
    :: Types.ResourceName -- ^ 'instanceName'
    -> GetInstanceState
mkGetInstanceState instanceName = GetInstanceState'{instanceName}

-- | The name of the instance to get state information about.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceName :: Lens.Lens' GetInstanceState Types.ResourceName
gisInstanceName = Lens.field @"instanceName"
{-# INLINEABLE gisInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery GetInstanceState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstanceState where
        toHeaders GetInstanceState{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetInstanceState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstanceState where
        toJSON GetInstanceState{..}
          = Core.object
              (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetInstanceState where
        type Rs GetInstanceState = GetInstanceStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceStateResponse' Core.<$>
                   (x Core..:? "state") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInstanceStateResponse' smart constructor.
data GetInstanceStateResponse = GetInstanceStateResponse'
  { state :: Core.Maybe Types.InstanceState
    -- ^ The state of the instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceStateResponse' value with any optional fields omitted.
mkGetInstanceStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceStateResponse
mkGetInstanceStateResponse responseStatus
  = GetInstanceStateResponse'{state = Core.Nothing, responseStatus}

-- | The state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrgrsState :: Lens.Lens' GetInstanceStateResponse (Core.Maybe Types.InstanceState)
gisrgrsState = Lens.field @"state"
{-# INLINEABLE gisrgrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrgrsResponseStatus :: Lens.Lens' GetInstanceStateResponse Core.Int
gisrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gisrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
