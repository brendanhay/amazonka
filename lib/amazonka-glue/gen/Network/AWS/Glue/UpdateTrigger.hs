{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
module Network.AWS.Glue.UpdateTrigger
    (
    -- * Creating a request
      UpdateTrigger (..)
    , mkUpdateTrigger
    -- ** Request lenses
    , utName
    , utTriggerUpdate

    -- * Destructuring the response
    , UpdateTriggerResponse (..)
    , mkUpdateTriggerResponse
    -- ** Response lenses
    , utrfrsTrigger
    , utrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { name :: Types.NameString
    -- ^ The name of the trigger to update.
  , triggerUpdate :: Types.TriggerUpdate
    -- ^ The new values with which to update the trigger.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrigger' value with any optional fields omitted.
mkUpdateTrigger
    :: Types.NameString -- ^ 'name'
    -> Types.TriggerUpdate -- ^ 'triggerUpdate'
    -> UpdateTrigger
mkUpdateTrigger name triggerUpdate
  = UpdateTrigger'{name, triggerUpdate}

-- | The name of the trigger to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utName :: Lens.Lens' UpdateTrigger Types.NameString
utName = Lens.field @"name"
{-# INLINEABLE utName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The new values with which to update the trigger.
--
-- /Note:/ Consider using 'triggerUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTriggerUpdate :: Lens.Lens' UpdateTrigger Types.TriggerUpdate
utTriggerUpdate = Lens.field @"triggerUpdate"
{-# INLINEABLE utTriggerUpdate #-}
{-# DEPRECATED triggerUpdate "Use generic-lens or generic-optics with 'triggerUpdate' instead"  #-}

instance Core.ToQuery UpdateTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTrigger where
        toHeaders UpdateTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTrigger where
        toJSON UpdateTrigger{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("TriggerUpdate" Core..= triggerUpdate)])

instance Core.AWSRequest UpdateTrigger where
        type Rs UpdateTrigger = UpdateTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTriggerResponse' Core.<$>
                   (x Core..:? "Trigger") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { trigger :: Core.Maybe Types.Trigger
    -- ^ The resulting trigger definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTriggerResponse' value with any optional fields omitted.
mkUpdateTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTriggerResponse
mkUpdateTriggerResponse responseStatus
  = UpdateTriggerResponse'{trigger = Core.Nothing, responseStatus}

-- | The resulting trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrfrsTrigger :: Lens.Lens' UpdateTriggerResponse (Core.Maybe Types.Trigger)
utrfrsTrigger = Lens.field @"trigger"
{-# INLINEABLE utrfrsTrigger #-}
{-# DEPRECATED trigger "Use generic-lens or generic-optics with 'trigger' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrfrsResponseStatus :: Lens.Lens' UpdateTriggerResponse Core.Int
utrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
