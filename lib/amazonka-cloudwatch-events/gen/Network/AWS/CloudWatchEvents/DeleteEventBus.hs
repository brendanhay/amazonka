{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteEventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified custom event bus or partner event bus. All rules associated with this event bus need to be deleted. You can't delete your account's default event bus.
module Network.AWS.CloudWatchEvents.DeleteEventBus
    (
    -- * Creating a request
      DeleteEventBus (..)
    , mkDeleteEventBus
    -- ** Request lenses
    , debsName

    -- * Destructuring the response
    , DeleteEventBusResponse (..)
    , mkDeleteEventBusResponse
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEventBus' smart constructor.
newtype DeleteEventBus = DeleteEventBus'
  { name :: Types.EventBusName
    -- ^ The name of the event bus to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventBus' value with any optional fields omitted.
mkDeleteEventBus
    :: Types.EventBusName -- ^ 'name'
    -> DeleteEventBus
mkDeleteEventBus name = DeleteEventBus'{name}

-- | The name of the event bus to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debsName :: Lens.Lens' DeleteEventBus Types.EventBusName
debsName = Lens.field @"name"
{-# INLINEABLE debsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteEventBus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEventBus where
        toHeaders DeleteEventBus{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DeleteEventBus") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteEventBus where
        toJSON DeleteEventBus{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteEventBus where
        type Rs DeleteEventBus = DeleteEventBusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteEventBusResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEventBusResponse' smart constructor.
data DeleteEventBusResponse = DeleteEventBusResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventBusResponse' value with any optional fields omitted.
mkDeleteEventBusResponse
    :: DeleteEventBusResponse
mkDeleteEventBusResponse = DeleteEventBusResponse'
