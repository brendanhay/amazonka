{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete all schedule actions on a channel.
module Network.AWS.MediaLive.DeleteSchedule
    (
    -- * Creating a request
      DeleteSchedule (..)
    , mkDeleteSchedule
    -- ** Request lenses
    , dsChannelId

    -- * Destructuring the response
    , DeleteScheduleResponse (..)
    , mkDeleteScheduleResponse
    -- ** Response lenses
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteScheduleRequest
--
-- /See:/ 'mkDeleteSchedule' smart constructor.
newtype DeleteSchedule = DeleteSchedule'
  { channelId :: Core.Text
    -- ^ Id of the channel whose schedule is being deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchedule' value with any optional fields omitted.
mkDeleteSchedule
    :: Core.Text -- ^ 'channelId'
    -> DeleteSchedule
mkDeleteSchedule channelId = DeleteSchedule'{channelId}

-- | Id of the channel whose schedule is being deleted.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsChannelId :: Lens.Lens' DeleteSchedule Core.Text
dsChannelId = Lens.field @"channelId"
{-# INLINEABLE dsChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

instance Core.ToQuery DeleteSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSchedule where
        toHeaders DeleteSchedule{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteSchedule where
        type Rs DeleteSchedule = DeleteScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/prod/channels/" Core.<> Core.toText channelId Core.<>
                             "/schedule",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteScheduleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DeleteScheduleResponse
--
-- /See:/ 'mkDeleteScheduleResponse' smart constructor.
newtype DeleteScheduleResponse = DeleteScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduleResponse' value with any optional fields omitted.
mkDeleteScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteScheduleResponse
mkDeleteScheduleResponse responseStatus
  = DeleteScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteScheduleResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
