{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteGameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game session queue. Once a queue is successfully deleted, unfulfilled 'StartGameSessionPlacement' requests that reference the queue will fail. To delete a queue, specify the queue name.
--
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues> 
-- __Related operations__ 
--
--     * 'CreateGameSessionQueue' 
--
--
--     * 'DescribeGameSessionQueues' 
--
--
--     * 'UpdateGameSessionQueue' 
--
--
--     * 'DeleteGameSessionQueue' 
--
--
module Network.AWS.GameLift.DeleteGameSessionQueue
    (
    -- * Creating a request
      DeleteGameSessionQueue (..)
    , mkDeleteGameSessionQueue
    -- ** Request lenses
    , dgsqName

    -- * Destructuring the response
    , DeleteGameSessionQueueResponse (..)
    , mkDeleteGameSessionQueueResponse
    -- ** Response lenses
    , dgsqrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation. 
--
-- /See:/ 'mkDeleteGameSessionQueue' smart constructor.
newtype DeleteGameSessionQueue = DeleteGameSessionQueue'
  { name :: Types.Name
    -- ^ A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGameSessionQueue' value with any optional fields omitted.
mkDeleteGameSessionQueue
    :: Types.Name -- ^ 'name'
    -> DeleteGameSessionQueue
mkDeleteGameSessionQueue name = DeleteGameSessionQueue'{name}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqName :: Lens.Lens' DeleteGameSessionQueue Types.Name
dgsqName = Lens.field @"name"
{-# INLINEABLE dgsqName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteGameSessionQueue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGameSessionQueue where
        toHeaders DeleteGameSessionQueue{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteGameSessionQueue")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteGameSessionQueue where
        toJSON DeleteGameSessionQueue{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteGameSessionQueue where
        type Rs DeleteGameSessionQueue = DeleteGameSessionQueueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteGameSessionQueueResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGameSessionQueueResponse' smart constructor.
newtype DeleteGameSessionQueueResponse = DeleteGameSessionQueueResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGameSessionQueueResponse' value with any optional fields omitted.
mkDeleteGameSessionQueueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGameSessionQueueResponse
mkDeleteGameSessionQueueResponse responseStatus
  = DeleteGameSessionQueueResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrrsResponseStatus :: Lens.Lens' DeleteGameSessionQueueResponse Core.Int
dgsqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgsqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
