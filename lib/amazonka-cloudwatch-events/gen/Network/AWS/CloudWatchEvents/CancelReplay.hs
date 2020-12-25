{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CancelReplay (..),
    mkCancelReplay,

    -- ** Request lenses
    crReplayName,

    -- * Destructuring the response
    CancelReplayResponse (..),
    mkCancelReplayResponse,

    -- ** Response lenses
    crrrsReplayArn,
    crrrsState,
    crrrsStateReason,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelReplay' smart constructor.
newtype CancelReplay = CancelReplay'
  { -- | The name of the replay to cancel.
    replayName :: Types.ReplayName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReplay' value with any optional fields omitted.
mkCancelReplay ::
  -- | 'replayName'
  Types.ReplayName ->
  CancelReplay
mkCancelReplay replayName = CancelReplay' {replayName}

-- | The name of the replay to cancel.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crReplayName :: Lens.Lens' CancelReplay Types.ReplayName
crReplayName = Lens.field @"replayName"
{-# DEPRECATED crReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

instance Core.FromJSON CancelReplay where
  toJSON CancelReplay {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ReplayName" Core..= replayName)])

instance Core.AWSRequest CancelReplay where
  type Rs CancelReplay = CancelReplayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.CancelReplay")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            Core.<$> (x Core..:? "ReplayArn")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { -- | The ARN of the replay to cancel.
    replayArn :: Core.Maybe Types.ReplayArn,
    -- | The current state of the replay.
    state :: Core.Maybe Types.ReplayState,
    -- | The reason that the replay is in the current state.
    stateReason :: Core.Maybe Types.ReplayStateReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReplayResponse' value with any optional fields omitted.
mkCancelReplayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelReplayResponse
mkCancelReplayResponse responseStatus =
  CancelReplayResponse'
    { replayArn = Core.Nothing,
      state = Core.Nothing,
      stateReason = Core.Nothing,
      responseStatus
    }

-- | The ARN of the replay to cancel.
--
-- /Note:/ Consider using 'replayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsReplayArn :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayArn)
crrrsReplayArn = Lens.field @"replayArn"
{-# DEPRECATED crrrsReplayArn "Use generic-lens or generic-optics with 'replayArn' instead." #-}

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsState :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayState)
crrrsState = Lens.field @"state"
{-# DEPRECATED crrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsStateReason :: Lens.Lens' CancelReplayResponse (Core.Maybe Types.ReplayStateReason)
crrrsStateReason = Lens.field @"stateReason"
{-# DEPRECATED crrrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CancelReplayResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
