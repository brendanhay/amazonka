{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchStart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts existing resources
module Network.AWS.MediaLive.BatchStart
    (
    -- * Creating a request
      BatchStart (..)
    , mkBatchStart
    -- ** Request lenses
    , bChannelIds
    , bMultiplexIds

    -- * Destructuring the response
    , BatchStartResponse (..)
    , mkBatchStartResponse
    -- ** Response lenses
    , bsrrsFailed
    , bsrrsSuccessful
    , bsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to start resources
--
-- /See:/ 'mkBatchStart' smart constructor.
data BatchStart = BatchStart'
  { channelIds :: Core.Maybe [Core.Text]
    -- ^ List of channel IDs
  , multiplexIds :: Core.Maybe [Core.Text]
    -- ^ List of multiplex IDs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStart' value with any optional fields omitted.
mkBatchStart
    :: BatchStart
mkBatchStart
  = BatchStart'{channelIds = Core.Nothing,
                multiplexIds = Core.Nothing}

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bChannelIds :: Lens.Lens' BatchStart (Core.Maybe [Core.Text])
bChannelIds = Lens.field @"channelIds"
{-# INLINEABLE bChannelIds #-}
{-# DEPRECATED channelIds "Use generic-lens or generic-optics with 'channelIds' instead"  #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMultiplexIds :: Lens.Lens' BatchStart (Core.Maybe [Core.Text])
bMultiplexIds = Lens.field @"multiplexIds"
{-# INLINEABLE bMultiplexIds #-}
{-# DEPRECATED multiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead"  #-}

instance Core.ToQuery BatchStart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchStart where
        toHeaders BatchStart{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchStart where
        toJSON BatchStart{..}
          = Core.object
              (Core.catMaybes
                 [("channelIds" Core..=) Core.<$> channelIds,
                  ("multiplexIds" Core..=) Core.<$> multiplexIds])

instance Core.AWSRequest BatchStart where
        type Rs BatchStart = BatchStartResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/prod/batch/start",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchStartResponse' Core.<$>
                   (x Core..:? "failed") Core.<*> x Core..:? "successful" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for BatchStartResponse
--
-- /See:/ 'mkBatchStartResponse' smart constructor.
data BatchStartResponse = BatchStartResponse'
  { failed :: Core.Maybe [Types.BatchFailedResultModel]
    -- ^ List of failed operations
  , successful :: Core.Maybe [Types.BatchSuccessfulResultModel]
    -- ^ List of successful operations
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStartResponse' value with any optional fields omitted.
mkBatchStartResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchStartResponse
mkBatchStartResponse responseStatus
  = BatchStartResponse'{failed = Core.Nothing,
                        successful = Core.Nothing, responseStatus}

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsFailed :: Lens.Lens' BatchStartResponse (Core.Maybe [Types.BatchFailedResultModel])
bsrrsFailed = Lens.field @"failed"
{-# INLINEABLE bsrrsFailed #-}
{-# DEPRECATED failed "Use generic-lens or generic-optics with 'failed' instead"  #-}

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsSuccessful :: Lens.Lens' BatchStartResponse (Core.Maybe [Types.BatchSuccessfulResultModel])
bsrrsSuccessful = Lens.field @"successful"
{-# INLINEABLE bsrrsSuccessful #-}
{-# DEPRECATED successful "Use generic-lens or generic-optics with 'successful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsResponseStatus :: Lens.Lens' BatchStartResponse Core.Int
bsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
