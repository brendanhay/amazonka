{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified trigger.
module Network.AWS.Glue.StopTrigger
    (
    -- * Creating a request
      StopTrigger (..)
    , mkStopTrigger
    -- ** Request lenses
    , stName

    -- * Destructuring the response
    , StopTriggerResponse (..)
    , mkStopTriggerResponse
    -- ** Response lenses
    , strrsName
    , strrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopTrigger' smart constructor.
newtype StopTrigger = StopTrigger'
  { name :: Types.NameString
    -- ^ The name of the trigger to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrigger' value with any optional fields omitted.
mkStopTrigger
    :: Types.NameString -- ^ 'name'
    -> StopTrigger
mkStopTrigger name = StopTrigger'{name}

-- | The name of the trigger to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stName :: Lens.Lens' StopTrigger Types.NameString
stName = Lens.field @"name"
{-# INLINEABLE stName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopTrigger where
        toHeaders StopTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StopTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopTrigger where
        toJSON StopTrigger{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopTrigger where
        type Rs StopTrigger = StopTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopTriggerResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopTriggerResponse' smart constructor.
data StopTriggerResponse = StopTriggerResponse'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the trigger that was stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTriggerResponse' value with any optional fields omitted.
mkStopTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopTriggerResponse
mkStopTriggerResponse responseStatus
  = StopTriggerResponse'{name = Core.Nothing, responseStatus}

-- | The name of the trigger that was stopped.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strrsName :: Lens.Lens' StopTriggerResponse (Core.Maybe Types.Name)
strrsName = Lens.field @"name"
{-# INLINEABLE strrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strrsResponseStatus :: Lens.Lens' StopTriggerResponse Core.Int
strrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE strrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
