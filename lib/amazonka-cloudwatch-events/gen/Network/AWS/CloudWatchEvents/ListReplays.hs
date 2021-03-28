{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListReplays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your replays. You can either list all the replays or you can provide a prefix to match to the replay names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListReplays
    (
    -- * Creating a request
      ListReplays (..)
    , mkListReplays
    -- ** Request lenses
    , lEventSourceArn
    , lLimit
    , lNamePrefix
    , lNextToken
    , lState

    -- * Destructuring the response
    , ListReplaysResponse (..)
    , mkListReplaysResponse
    -- ** Response lenses
    , lrsNextToken
    , lrsReplays
    , lrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListReplays' smart constructor.
data ListReplays = ListReplays'
  { eventSourceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the event source associated with the replay.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of replays to retrieve.
  , namePrefix :: Core.Maybe Types.ReplayName
    -- ^ A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to retrieve the next set of results.
  , state :: Core.Maybe Types.ReplayState
    -- ^ The state of the replay.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReplays' value with any optional fields omitted.
mkListReplays
    :: ListReplays
mkListReplays
  = ListReplays'{eventSourceArn = Core.Nothing, limit = Core.Nothing,
                 namePrefix = Core.Nothing, nextToken = Core.Nothing,
                 state = Core.Nothing}

-- | The ARN of the event source associated with the replay.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEventSourceArn :: Lens.Lens' ListReplays (Core.Maybe Types.Arn)
lEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE lEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | The maximum number of replays to retrieve.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLimit :: Lens.Lens' ListReplays (Core.Maybe Core.Natural)
lLimit = Lens.field @"limit"
{-# INLINEABLE lLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNamePrefix :: Lens.Lens' ListReplays (Core.Maybe Types.ReplayName)
lNamePrefix = Lens.field @"namePrefix"
{-# INLINEABLE lNamePrefix #-}
{-# DEPRECATED namePrefix "Use generic-lens or generic-optics with 'namePrefix' instead"  #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListReplays (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lState :: Lens.Lens' ListReplays (Core.Maybe Types.ReplayState)
lState = Lens.field @"state"
{-# INLINEABLE lState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.ToQuery ListReplays where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListReplays where
        toHeaders ListReplays{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.ListReplays") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListReplays where
        toJSON ListReplays{..}
          = Core.object
              (Core.catMaybes
                 [("EventSourceArn" Core..=) Core.<$> eventSourceArn,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NamePrefix" Core..=) Core.<$> namePrefix,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("State" Core..=) Core.<$> state])

instance Core.AWSRequest ListReplays where
        type Rs ListReplays = ListReplaysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListReplaysResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Replays" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListReplaysResponse' smart constructor.
data ListReplaysResponse = ListReplaysResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to retrieve the next set of results.
  , replays :: Core.Maybe [Types.Replay]
    -- ^ An array of @Replay@ objects that contain information about the replay.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListReplaysResponse' value with any optional fields omitted.
mkListReplaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListReplaysResponse
mkListReplaysResponse responseStatus
  = ListReplaysResponse'{nextToken = Core.Nothing,
                         replays = Core.Nothing, responseStatus}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListReplaysResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @Replay@ objects that contain information about the replay.
--
-- /Note:/ Consider using 'replays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsReplays :: Lens.Lens' ListReplaysResponse (Core.Maybe [Types.Replay])
lrsReplays = Lens.field @"replays"
{-# INLINEABLE lrsReplays #-}
{-# DEPRECATED replays "Use generic-lens or generic-optics with 'replays' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListReplaysResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
