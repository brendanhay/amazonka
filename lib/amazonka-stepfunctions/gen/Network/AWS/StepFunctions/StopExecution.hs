{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.StopExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.StopExecution
  ( -- * Creating a request
    StopExecution (..),
    mkStopExecution,

    -- ** Request lenses
    seExecutionArn,
    seCause,
    seError,

    -- * Destructuring the response
    StopExecutionResponse (..),
    mkStopExecutionResponse,

    -- ** Response lenses
    serrsStopDate,
    serrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkStopExecution' smart constructor.
data StopExecution = StopExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to stop.
    executionArn :: Types.Arn,
    -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.SensitiveCause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopExecution' value with any optional fields omitted.
mkStopExecution ::
  -- | 'executionArn'
  Types.Arn ->
  StopExecution
mkStopExecution executionArn =
  StopExecution'
    { executionArn,
      cause = Core.Nothing,
      error = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the execution to stop.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionArn :: Lens.Lens' StopExecution Types.Arn
seExecutionArn = Lens.field @"executionArn"
{-# DEPRECATED seExecutionArn "Use generic-lens or generic-optics with 'executionArn' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCause :: Lens.Lens' StopExecution (Core.Maybe Types.SensitiveCause)
seCause = Lens.field @"cause"
{-# DEPRECATED seCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seError :: Lens.Lens' StopExecution (Core.Maybe Types.SensitiveError)
seError = Lens.field @"error"
{-# DEPRECATED seError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON StopExecution where
  toJSON StopExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("executionArn" Core..= executionArn),
            ("cause" Core..=) Core.<$> cause,
            ("error" Core..=) Core.<$> error
          ]
      )

instance Core.AWSRequest StopExecution where
  type Rs StopExecution = StopExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.StopExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopExecutionResponse'
            Core.<$> (x Core..: "stopDate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { -- | The date the execution is stopped.
    stopDate :: Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopExecutionResponse' value with any optional fields omitted.
mkStopExecutionResponse ::
  -- | 'stopDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  StopExecutionResponse
mkStopExecutionResponse stopDate responseStatus =
  StopExecutionResponse' {stopDate, responseStatus}

-- | The date the execution is stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsStopDate :: Lens.Lens' StopExecutionResponse Core.NominalDiffTime
serrsStopDate = Lens.field @"stopDate"
{-# DEPRECATED serrsStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsResponseStatus :: Lens.Lens' StopExecutionResponse Core.Int
serrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED serrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
