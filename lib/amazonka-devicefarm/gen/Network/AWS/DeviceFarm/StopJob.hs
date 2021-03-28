{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current job. AWS Device Farm immediately stops the job on the device where tests have not started. You are not billed for this device. On the device where tests have started, setup suite and teardown suite tests run to completion on the device. You are billed for setup, teardown, and any tests that were in progress or already completed.
module Network.AWS.DeviceFarm.StopJob
    (
    -- * Creating a request
      StopJob (..)
    , mkStopJob
    -- ** Request lenses
    , sjArn

    -- * Destructuring the response
    , StopJobResponse (..)
    , mkStopJobResponse
    -- ** Response lenses
    , sjrrsJob
    , sjrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopJob' smart constructor.
newtype StopJob = StopJob'
  { arn :: Types.AmazonResourceName
    -- ^ Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopJob' value with any optional fields omitted.
mkStopJob
    :: Types.AmazonResourceName -- ^ 'arn'
    -> StopJob
mkStopJob arn = StopJob'{arn}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjArn :: Lens.Lens' StopJob Types.AmazonResourceName
sjArn = Lens.field @"arn"
{-# INLINEABLE sjArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery StopJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopJob where
        toHeaders StopJob{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.StopJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopJob where
        toJSON StopJob{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest StopJob where
        type Rs StopJob = StopJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopJobResponse' Core.<$>
                   (x Core..:? "job") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { job :: Core.Maybe Types.Job
    -- ^ The job that was stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopJobResponse' value with any optional fields omitted.
mkStopJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopJobResponse
mkStopJobResponse responseStatus
  = StopJobResponse'{job = Core.Nothing, responseStatus}

-- | The job that was stopped.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsJob :: Lens.Lens' StopJobResponse (Core.Maybe Types.Job)
sjrrsJob = Lens.field @"job"
{-# INLINEABLE sjrrsJob #-}
{-# DEPRECATED job "Use generic-lens or generic-optics with 'job' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsResponseStatus :: Lens.Lens' StopJobResponse Core.Int
sjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
