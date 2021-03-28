{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing trigger. See <https://docs.aws.amazon.com/glue/latest/dg/trigger-job.html Triggering Jobs> for information about how different types of trigger are started.
module Network.AWS.Glue.StartTrigger
    (
    -- * Creating a request
      StartTrigger (..)
    , mkStartTrigger
    -- ** Request lenses
    , stfName

    -- * Destructuring the response
    , StartTriggerResponse (..)
    , mkStartTriggerResponse
    -- ** Response lenses
    , strgrsName
    , strgrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartTrigger' smart constructor.
newtype StartTrigger = StartTrigger'
  { name :: Types.Name
    -- ^ The name of the trigger to start.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartTrigger' value with any optional fields omitted.
mkStartTrigger
    :: Types.Name -- ^ 'name'
    -> StartTrigger
mkStartTrigger name = StartTrigger'{name}

-- | The name of the trigger to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfName :: Lens.Lens' StartTrigger Types.Name
stfName = Lens.field @"name"
{-# INLINEABLE stfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StartTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartTrigger where
        toHeaders StartTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StartTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartTrigger where
        toJSON StartTrigger{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartTrigger where
        type Rs StartTrigger = StartTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartTriggerResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { name :: Core.Maybe Types.NameString
    -- ^ The name of the trigger that was started.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTriggerResponse' value with any optional fields omitted.
mkStartTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartTriggerResponse
mkStartTriggerResponse responseStatus
  = StartTriggerResponse'{name = Core.Nothing, responseStatus}

-- | The name of the trigger that was started.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strgrsName :: Lens.Lens' StartTriggerResponse (Core.Maybe Types.NameString)
strgrsName = Lens.field @"name"
{-# INLINEABLE strgrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strgrsResponseStatus :: Lens.Lens' StartTriggerResponse Core.Int
strgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE strgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
