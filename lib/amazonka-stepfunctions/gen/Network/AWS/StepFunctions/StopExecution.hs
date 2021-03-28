{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StopExecution (..)
    , mkStopExecution
    -- ** Request lenses
    , seExecutionArn
    , seCause
    , seError

    -- * Destructuring the response
    , StopExecutionResponse (..)
    , mkStopExecutionResponse
    -- ** Response lenses
    , serrsStopDate
    , serrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkStopExecution' smart constructor.
data StopExecution = StopExecution'
  { executionArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the execution to stop.
  , cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopExecution' value with any optional fields omitted.
mkStopExecution
    :: Types.Arn -- ^ 'executionArn'
    -> StopExecution
mkStopExecution executionArn
  = StopExecution'{executionArn, cause = Core.Nothing,
                   error = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the execution to stop.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionArn :: Lens.Lens' StopExecution Types.Arn
seExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE seExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCause :: Lens.Lens' StopExecution (Core.Maybe Types.SensitiveCause)
seCause = Lens.field @"cause"
{-# INLINEABLE seCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seError :: Lens.Lens' StopExecution (Core.Maybe Types.SensitiveError)
seError = Lens.field @"error"
{-# INLINEABLE seError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.ToQuery StopExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopExecution where
        toHeaders StopExecution{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.StopExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON StopExecution where
        toJSON StopExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("executionArn" Core..= executionArn),
                  ("cause" Core..=) Core.<$> cause,
                  ("error" Core..=) Core.<$> error])

instance Core.AWSRequest StopExecution where
        type Rs StopExecution = StopExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopExecutionResponse' Core.<$>
                   (x Core..: "stopDate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { stopDate :: Core.NominalDiffTime
    -- ^ The date the execution is stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopExecutionResponse' value with any optional fields omitted.
mkStopExecutionResponse
    :: Core.NominalDiffTime -- ^ 'stopDate'
    -> Core.Int -- ^ 'responseStatus'
    -> StopExecutionResponse
mkStopExecutionResponse stopDate responseStatus
  = StopExecutionResponse'{stopDate, responseStatus}

-- | The date the execution is stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsStopDate :: Lens.Lens' StopExecutionResponse Core.NominalDiffTime
serrsStopDate = Lens.field @"stopDate"
{-# INLINEABLE serrsStopDate #-}
{-# DEPRECATED stopDate "Use generic-lens or generic-optics with 'stopDate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
serrsResponseStatus :: Lens.Lens' StopExecutionResponse Core.Int
serrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE serrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
