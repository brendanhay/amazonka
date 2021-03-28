{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Network.AWS.Glue.GetTrigger
    (
    -- * Creating a request
      GetTrigger (..)
    , mkGetTrigger
    -- ** Request lenses
    , gtName

    -- * Destructuring the response
    , GetTriggerResponse (..)
    , mkGetTriggerResponse
    -- ** Response lenses
    , gtrfrsTrigger
    , gtrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTrigger' smart constructor.
newtype GetTrigger = GetTrigger'
  { name :: Types.Name
    -- ^ The name of the trigger to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrigger' value with any optional fields omitted.
mkGetTrigger
    :: Types.Name -- ^ 'name'
    -> GetTrigger
mkGetTrigger name = GetTrigger'{name}

-- | The name of the trigger to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTrigger Types.Name
gtName = Lens.field @"name"
{-# INLINEABLE gtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTrigger where
        toHeaders GetTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTrigger where
        toJSON GetTrigger{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetTrigger where
        type Rs GetTrigger = GetTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTriggerResponse' Core.<$>
                   (x Core..:? "Trigger") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { trigger :: Core.Maybe Types.Trigger
    -- ^ The requested trigger definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTriggerResponse' value with any optional fields omitted.
mkGetTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTriggerResponse
mkGetTriggerResponse responseStatus
  = GetTriggerResponse'{trigger = Core.Nothing, responseStatus}

-- | The requested trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrfrsTrigger :: Lens.Lens' GetTriggerResponse (Core.Maybe Types.Trigger)
gtrfrsTrigger = Lens.field @"trigger"
{-# INLINEABLE gtrfrsTrigger #-}
{-# DEPRECATED trigger "Use generic-lens or generic-optics with 'trigger' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrfrsResponseStatus :: Lens.Lens' GetTriggerResponse Core.Int
gtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
