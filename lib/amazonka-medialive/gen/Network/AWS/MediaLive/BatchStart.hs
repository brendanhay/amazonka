{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchStart (..),
    mkBatchStart,

    -- ** Request lenses
    bChannelIds,
    bMultiplexIds,

    -- * Destructuring the response
    BatchStartResponse (..),
    mkBatchStartResponse,

    -- ** Response lenses
    bsrrsFailed,
    bsrrsSuccessful,
    bsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to start resources
--
-- /See:/ 'mkBatchStart' smart constructor.
data BatchStart = BatchStart'
  { -- | List of channel IDs
    channelIds :: Core.Maybe [Core.Text],
    -- | List of multiplex IDs
    multiplexIds :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStart' value with any optional fields omitted.
mkBatchStart ::
  BatchStart
mkBatchStart =
  BatchStart'
    { channelIds = Core.Nothing,
      multiplexIds = Core.Nothing
    }

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bChannelIds :: Lens.Lens' BatchStart (Core.Maybe [Core.Text])
bChannelIds = Lens.field @"channelIds"
{-# DEPRECATED bChannelIds "Use generic-lens or generic-optics with 'channelIds' instead." #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMultiplexIds :: Lens.Lens' BatchStart (Core.Maybe [Core.Text])
bMultiplexIds = Lens.field @"multiplexIds"
{-# DEPRECATED bMultiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead." #-}

instance Core.FromJSON BatchStart where
  toJSON BatchStart {..} =
    Core.object
      ( Core.catMaybes
          [ ("channelIds" Core..=) Core.<$> channelIds,
            ("multiplexIds" Core..=) Core.<$> multiplexIds
          ]
      )

instance Core.AWSRequest BatchStart where
  type Rs BatchStart = BatchStartResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/prod/batch/start",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStartResponse'
            Core.<$> (x Core..:? "failed")
            Core.<*> (x Core..:? "successful")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for BatchStartResponse
--
-- /See:/ 'mkBatchStartResponse' smart constructor.
data BatchStartResponse = BatchStartResponse'
  { -- | List of failed operations
    failed :: Core.Maybe [Types.BatchFailedResultModel],
    -- | List of successful operations
    successful :: Core.Maybe [Types.BatchSuccessfulResultModel],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStartResponse' value with any optional fields omitted.
mkBatchStartResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchStartResponse
mkBatchStartResponse responseStatus =
  BatchStartResponse'
    { failed = Core.Nothing,
      successful = Core.Nothing,
      responseStatus
    }

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsFailed :: Lens.Lens' BatchStartResponse (Core.Maybe [Types.BatchFailedResultModel])
bsrrsFailed = Lens.field @"failed"
{-# DEPRECATED bsrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsSuccessful :: Lens.Lens' BatchStartResponse (Core.Maybe [Types.BatchSuccessfulResultModel])
bsrrsSuccessful = Lens.field @"successful"
{-# DEPRECATED bsrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsResponseStatus :: Lens.Lens' BatchStartResponse Core.Int
bsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
