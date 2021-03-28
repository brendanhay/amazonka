{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified trigger. If the trigger is not found, no exception is thrown.
module Network.AWS.Glue.DeleteTrigger
    (
    -- * Creating a request
      DeleteTrigger (..)
    , mkDeleteTrigger
    -- ** Request lenses
    , dtfName

    -- * Destructuring the response
    , DeleteTriggerResponse (..)
    , mkDeleteTriggerResponse
    -- ** Response lenses
    , dtrfrsName
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTrigger' smart constructor.
newtype DeleteTrigger = DeleteTrigger'
  { name :: Types.NameString
    -- ^ The name of the trigger to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrigger' value with any optional fields omitted.
mkDeleteTrigger
    :: Types.NameString -- ^ 'name'
    -> DeleteTrigger
mkDeleteTrigger name = DeleteTrigger'{name}

-- | The name of the trigger to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfName :: Lens.Lens' DeleteTrigger Types.NameString
dtfName = Lens.field @"name"
{-# INLINEABLE dtfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTrigger where
        toHeaders DeleteTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTrigger where
        toJSON DeleteTrigger{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteTrigger where
        type Rs DeleteTrigger = DeleteTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteTriggerResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTriggerResponse' smart constructor.
data DeleteTriggerResponse = DeleteTriggerResponse'
  { name :: Core.Maybe Types.NameString
    -- ^ The name of the trigger that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTriggerResponse' value with any optional fields omitted.
mkDeleteTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTriggerResponse
mkDeleteTriggerResponse responseStatus
  = DeleteTriggerResponse'{name = Core.Nothing, responseStatus}

-- | The name of the trigger that was deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsName :: Lens.Lens' DeleteTriggerResponse (Core.Maybe Types.NameString)
dtrfrsName = Lens.field @"name"
{-# INLINEABLE dtrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeleteTriggerResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
