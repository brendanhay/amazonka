{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DeleteQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a queue you have created.
module Network.AWS.MediaConvert.DeleteQueue
    (
    -- * Creating a request
      DeleteQueue (..)
    , mkDeleteQueue
    -- ** Request lenses
    , dqName

    -- * Destructuring the response
    , DeleteQueueResponse (..)
    , mkDeleteQueueResponse
    -- ** Response lenses
    , dqrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { name :: Core.Text
    -- ^ The name of the queue that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueue' value with any optional fields omitted.
mkDeleteQueue
    :: Core.Text -- ^ 'name'
    -> DeleteQueue
mkDeleteQueue name = DeleteQueue'{name}

-- | The name of the queue that you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqName :: Lens.Lens' DeleteQueue Core.Text
dqName = Lens.field @"name"
{-# INLINEABLE dqName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteQueue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteQueue where
        toHeaders DeleteQueue{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteQueue where
        type Rs DeleteQueue = DeleteQueueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2017-08-29/queues/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteQueueResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteQueueResponse' smart constructor.
newtype DeleteQueueResponse = DeleteQueueResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueueResponse' value with any optional fields omitted.
mkDeleteQueueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteQueueResponse
mkDeleteQueueResponse responseStatus
  = DeleteQueueResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrrsResponseStatus :: Lens.Lens' DeleteQueueResponse Core.Int
dqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
