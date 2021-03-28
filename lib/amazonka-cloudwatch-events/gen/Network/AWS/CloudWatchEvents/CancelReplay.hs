{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CancelReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified replay.
module Network.AWS.CloudWatchEvents.CancelReplay
    (
    -- * Creating a request
      CancelReplay (..)
    , mkCancelReplay
    -- ** Request lenses
    , crReplayName

    -- * Destructuring the response
    , CancelReplayResponse (..)
    , mkCancelReplayResponse
    -- ** Response lenses
    , crrrsReplayArn
    , crrrsState
    , crrrsStateReason
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelReplay' smart constructor.
newtype CancelReplay = CancelReplay'
  { replayName :: Types.ReplayName
    -- ^ The name of the replay to cancel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReplay' value with any optional fields omitted.
mkCancelReplay
    :: Types.ReplayName -- ^ 'replayName'
    -> CancelReplay
mkCancelReplay replayName = CancelReplay'{replayName}

-- | The name of the replay to cancel.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crReplayName :: Lens.Lens' CancelReplay Types.ReplayName
crReplayName = Lens.field @"replayName"
{-# INLINEABLE crReplayName #-}
{-# DEPRECATED replayName "Use generic-lens or generic-optics with 'replayName' instead"  #-}

instance Core.ToQuery CancelReplay where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelReplay where
        toHeaders CancelReplay{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.CancelReplay") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelReplay where
        toJSON CancelReplay{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ReplayName" Core..= replayName)])

instance Core.AWSRequest CancelReplay where
        type Rs CancelReplay = CancelReplayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelReplayResponse' Core.<$>
                   (x Core..:? "ReplayArn") Core.<*> x Core..:? "State" Core.<*>
                     x Core..:? "StateReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { replayArn :: Core.Maybe Types.ReplayArn
    -- ^ The ARN of the replay to cancel.
  , state :: Core.Maybe Types.ReplayState
    -- ^ The current state of the replay.
  , stateReason :: Core.Maybe Types.ReplayStateReason
    -- ^ The reason that the replay is in the current state.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReplayResponse' value with any optional fields omitted.
mkCancelReplayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelReplayResponse
mkCancelReplayResponse responseStatus
  = CancelReplayResponse'{replayArn = Core.Nothing,
                          state = Core.Nothing, stateReason = Core.Nothing, responseStatus}

-- | The ARN of the replay to cancel.
--
-- /Note:/ Consider using 'replayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsReplayArn :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayArn)
crrrsReplayArn = Lens.field @"replayArn"
{-# INLINEABLE crrrsReplayArn #-}
{-# DEPRECATED replayArn "Use generic-lens or generic-optics with 'replayArn' instead"  #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsState :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayState)
crrrsState = Lens.field @"state"
{-# INLINEABLE crrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsStateReason :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayStateReason)
crrrsStateReason = Lens.field @"stateReason"
{-# INLINEABLE crrrsStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CancelReplayResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
