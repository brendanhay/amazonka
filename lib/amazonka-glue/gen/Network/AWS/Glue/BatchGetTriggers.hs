{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of trigger names. After calling the @ListTriggers@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetTriggers
    (
    -- * Creating a request
      BatchGetTriggers (..)
    , mkBatchGetTriggers
    -- ** Request lenses
    , bgtTriggerNames

    -- * Destructuring the response
    , BatchGetTriggersResponse (..)
    , mkBatchGetTriggersResponse
    -- ** Response lenses
    , bgtrrsTriggers
    , bgtrrsTriggersNotFound
    , bgtrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetTriggers' smart constructor.
newtype BatchGetTriggers = BatchGetTriggers'
  { triggerNames :: [Types.NameString]
    -- ^ A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetTriggers' value with any optional fields omitted.
mkBatchGetTriggers
    :: BatchGetTriggers
mkBatchGetTriggers = BatchGetTriggers'{triggerNames = Core.mempty}

-- | A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
--
-- /Note:/ Consider using 'triggerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtTriggerNames :: Lens.Lens' BatchGetTriggers [Types.NameString]
bgtTriggerNames = Lens.field @"triggerNames"
{-# INLINEABLE bgtTriggerNames #-}
{-# DEPRECATED triggerNames "Use generic-lens or generic-optics with 'triggerNames' instead"  #-}

instance Core.ToQuery BatchGetTriggers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetTriggers where
        toHeaders BatchGetTriggers{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchGetTriggers") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetTriggers where
        toJSON BatchGetTriggers{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TriggerNames" Core..= triggerNames)])

instance Core.AWSRequest BatchGetTriggers where
        type Rs BatchGetTriggers = BatchGetTriggersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetTriggersResponse' Core.<$>
                   (x Core..:? "Triggers") Core.<*> x Core..:? "TriggersNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetTriggersResponse' smart constructor.
data BatchGetTriggersResponse = BatchGetTriggersResponse'
  { triggers :: Core.Maybe [Types.Trigger]
    -- ^ A list of trigger definitions.
  , triggersNotFound :: Core.Maybe [Types.NameString]
    -- ^ A list of names of triggers not found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetTriggersResponse' value with any optional fields omitted.
mkBatchGetTriggersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetTriggersResponse
mkBatchGetTriggersResponse responseStatus
  = BatchGetTriggersResponse'{triggers = Core.Nothing,
                              triggersNotFound = Core.Nothing, responseStatus}

-- | A list of trigger definitions.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsTriggers :: Lens.Lens' BatchGetTriggersResponse (Core.Maybe [Types.Trigger])
bgtrrsTriggers = Lens.field @"triggers"
{-# INLINEABLE bgtrrsTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

-- | A list of names of triggers not found.
--
-- /Note:/ Consider using 'triggersNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsTriggersNotFound :: Lens.Lens' BatchGetTriggersResponse (Core.Maybe [Types.NameString])
bgtrrsTriggersNotFound = Lens.field @"triggersNotFound"
{-# INLINEABLE bgtrrsTriggersNotFound #-}
{-# DEPRECATED triggersNotFound "Use generic-lens or generic-optics with 'triggersNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgtrrsResponseStatus :: Lens.Lens' BatchGetTriggersResponse Core.Int
bgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
