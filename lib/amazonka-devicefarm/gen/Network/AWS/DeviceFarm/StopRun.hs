{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current test run. AWS Device Farm immediately stops the run on devices where tests have not started. You are not billed for these devices. On devices where tests have started executing, setup suite and teardown suite tests run to completion on those devices. You are billed for setup, teardown, and any tests that were in progress or already completed.
module Network.AWS.DeviceFarm.StopRun
    (
    -- * Creating a request
      StopRun (..)
    , mkStopRun
    -- ** Request lenses
    , srArn

    -- * Destructuring the response
    , StopRunResponse (..)
    , mkStopRunResponse
    -- ** Response lenses
    , srsRun
    , srsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to stop a specific run.
--
-- /See:/ 'mkStopRun' smart constructor.
newtype StopRun = StopRun'
  { arn :: Types.Arn
    -- ^ Represents the Amazon Resource Name (ARN) of the Device Farm run to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopRun' value with any optional fields omitted.
mkStopRun
    :: Types.Arn -- ^ 'arn'
    -> StopRun
mkStopRun arn = StopRun'{arn}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm run to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srArn :: Lens.Lens' StopRun Types.Arn
srArn = Lens.field @"arn"
{-# INLINEABLE srArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery StopRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopRun where
        toHeaders StopRun{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.StopRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopRun where
        toJSON StopRun{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest StopRun where
        type Rs StopRun = StopRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopRunResponse' Core.<$>
                   (x Core..:? "run") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the results of your stop run attempt.
--
-- /See:/ 'mkStopRunResponse' smart constructor.
data StopRunResponse = StopRunResponse'
  { run :: Core.Maybe Types.Run
    -- ^ The run that was stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopRunResponse' value with any optional fields omitted.
mkStopRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopRunResponse
mkStopRunResponse responseStatus
  = StopRunResponse'{run = Core.Nothing, responseStatus}

-- | The run that was stopped.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsRun :: Lens.Lens' StopRunResponse (Core.Maybe Types.Run)
srsRun = Lens.field @"run"
{-# INLINEABLE srsRun #-}
{-# DEPRECATED run "Use generic-lens or generic-optics with 'run' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopRunResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
