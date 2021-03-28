{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room by the room ARN.
module Network.AWS.AlexaBusiness.DeleteRoom
    (
    -- * Creating a request
      DeleteRoom (..)
    , mkDeleteRoom
    -- ** Request lenses
    , drRoomArn

    -- * Destructuring the response
    , DeleteRoomResponse (..)
    , mkDeleteRoomResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRoom' smart constructor.
newtype DeleteRoom = DeleteRoom'
  { roomArn :: Core.Maybe Types.RoomArn
    -- ^ The ARN of the room to delete. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoom' value with any optional fields omitted.
mkDeleteRoom
    :: DeleteRoom
mkDeleteRoom = DeleteRoom'{roomArn = Core.Nothing}

-- | The ARN of the room to delete. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRoomArn :: Lens.Lens' DeleteRoom (Core.Maybe Types.RoomArn)
drRoomArn = Lens.field @"roomArn"
{-# INLINEABLE drRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery DeleteRoom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRoom where
        toHeaders DeleteRoom{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteRoom") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRoom where
        toJSON DeleteRoom{..}
          = Core.object
              (Core.catMaybes [("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest DeleteRoom where
        type Rs DeleteRoom = DeleteRoomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteRoomResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRoomResponse' smart constructor.
newtype DeleteRoomResponse = DeleteRoomResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoomResponse' value with any optional fields omitted.
mkDeleteRoomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRoomResponse
mkDeleteRoomResponse responseStatus
  = DeleteRoomResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRoomResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
