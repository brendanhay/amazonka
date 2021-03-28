{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchStop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops running resources
module Network.AWS.MediaLive.BatchStop
    (
    -- * Creating a request
      BatchStop (..)
    , mkBatchStop
    -- ** Request lenses
    , bsChannelIds
    , bsMultiplexIds

    -- * Destructuring the response
    , BatchStopResponse (..)
    , mkBatchStopResponse
    -- ** Response lenses
    , brsFailed
    , brsSuccessful
    , brsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to stop resources
--
-- /See:/ 'mkBatchStop' smart constructor.
data BatchStop = BatchStop'
  { channelIds :: Core.Maybe [Core.Text]
    -- ^ List of channel IDs
  , multiplexIds :: Core.Maybe [Core.Text]
    -- ^ List of multiplex IDs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStop' value with any optional fields omitted.
mkBatchStop
    :: BatchStop
mkBatchStop
  = BatchStop'{channelIds = Core.Nothing,
               multiplexIds = Core.Nothing}

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsChannelIds :: Lens.Lens' BatchStop (Core.Maybe [Core.Text])
bsChannelIds = Lens.field @"channelIds"
{-# INLINEABLE bsChannelIds #-}
{-# DEPRECATED channelIds "Use generic-lens or generic-optics with 'channelIds' instead"  #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsMultiplexIds :: Lens.Lens' BatchStop (Core.Maybe [Core.Text])
bsMultiplexIds = Lens.field @"multiplexIds"
{-# INLINEABLE bsMultiplexIds #-}
{-# DEPRECATED multiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead"  #-}

instance Core.ToQuery BatchStop where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchStop where
        toHeaders BatchStop{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchStop where
        toJSON BatchStop{..}
          = Core.object
              (Core.catMaybes
                 [("channelIds" Core..=) Core.<$> channelIds,
                  ("multiplexIds" Core..=) Core.<$> multiplexIds])

instance Core.AWSRequest BatchStop where
        type Rs BatchStop = BatchStopResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/prod/batch/stop",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchStopResponse' Core.<$>
                   (x Core..:? "failed") Core.<*> x Core..:? "successful" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for BatchStopResponse
--
-- /See:/ 'mkBatchStopResponse' smart constructor.
data BatchStopResponse = BatchStopResponse'
  { failed :: Core.Maybe [Types.BatchFailedResultModel]
    -- ^ List of failed operations
  , successful :: Core.Maybe [Types.BatchSuccessfulResultModel]
    -- ^ List of successful operations
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopResponse' value with any optional fields omitted.
mkBatchStopResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchStopResponse
mkBatchStopResponse responseStatus
  = BatchStopResponse'{failed = Core.Nothing,
                       successful = Core.Nothing, responseStatus}

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsFailed :: Lens.Lens' BatchStopResponse (Core.Maybe [Types.BatchFailedResultModel])
brsFailed = Lens.field @"failed"
{-# INLINEABLE brsFailed #-}
{-# DEPRECATED failed "Use generic-lens or generic-optics with 'failed' instead"  #-}

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsSuccessful :: Lens.Lens' BatchStopResponse (Core.Maybe [Types.BatchSuccessfulResultModel])
brsSuccessful = Lens.field @"successful"
{-# INLINEABLE brsSuccessful #-}
{-# DEPRECATED successful "Use generic-lens or generic-optics with 'successful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsResponseStatus :: Lens.Lens' BatchStopResponse Core.Int
brsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE brsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
