{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.RunPipelineActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates the results of running a pipeline activity on a message payload.
module Network.AWS.IoTAnalytics.RunPipelineActivity
    (
    -- * Creating a request
      RunPipelineActivity (..)
    , mkRunPipelineActivity
    -- ** Request lenses
    , rpaPipelineActivity
    , rpaPayloads

    -- * Destructuring the response
    , RunPipelineActivityResponse (..)
    , mkRunPipelineActivityResponse
    -- ** Response lenses
    , rparrsLogResult
    , rparrsPayloads
    , rparrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRunPipelineActivity' smart constructor.
data RunPipelineActivity = RunPipelineActivity'
  { pipelineActivity :: Types.PipelineActivity
    -- ^ The pipeline activity that is run. This must not be a channel activity or a datastore activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a lambda activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
  , payloads :: Core.NonEmpty Core.Base64
    -- ^ The sample message payloads on which the pipeline activity is run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunPipelineActivity' value with any optional fields omitted.
mkRunPipelineActivity
    :: Types.PipelineActivity -- ^ 'pipelineActivity'
    -> Core.NonEmpty Core.Base64 -- ^ 'payloads'
    -> RunPipelineActivity
mkRunPipelineActivity pipelineActivity payloads
  = RunPipelineActivity'{pipelineActivity, payloads}

-- | The pipeline activity that is run. This must not be a channel activity or a datastore activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a lambda activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
--
-- /Note:/ Consider using 'pipelineActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpaPipelineActivity :: Lens.Lens' RunPipelineActivity Types.PipelineActivity
rpaPipelineActivity = Lens.field @"pipelineActivity"
{-# INLINEABLE rpaPipelineActivity #-}
{-# DEPRECATED pipelineActivity "Use generic-lens or generic-optics with 'pipelineActivity' instead"  #-}

-- | The sample message payloads on which the pipeline activity is run.
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpaPayloads :: Lens.Lens' RunPipelineActivity (Core.NonEmpty Core.Base64)
rpaPayloads = Lens.field @"payloads"
{-# INLINEABLE rpaPayloads #-}
{-# DEPRECATED payloads "Use generic-lens or generic-optics with 'payloads' instead"  #-}

instance Core.ToQuery RunPipelineActivity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RunPipelineActivity where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RunPipelineActivity where
        toJSON RunPipelineActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineActivity" Core..= pipelineActivity),
                  Core.Just ("payloads" Core..= payloads)])

instance Core.AWSRequest RunPipelineActivity where
        type Rs RunPipelineActivity = RunPipelineActivityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/pipelineactivities/run",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RunPipelineActivityResponse' Core.<$>
                   (x Core..:? "logResult") Core.<*> x Core..:? "payloads" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRunPipelineActivityResponse' smart constructor.
data RunPipelineActivityResponse = RunPipelineActivityResponse'
  { logResult :: Core.Maybe Types.LogResult
    -- ^ In case the pipeline activity fails, the log message that is generated.
  , payloads :: Core.Maybe (Core.NonEmpty Core.Base64)
    -- ^ The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunPipelineActivityResponse' value with any optional fields omitted.
mkRunPipelineActivityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RunPipelineActivityResponse
mkRunPipelineActivityResponse responseStatus
  = RunPipelineActivityResponse'{logResult = Core.Nothing,
                                 payloads = Core.Nothing, responseStatus}

-- | In case the pipeline activity fails, the log message that is generated.
--
-- /Note:/ Consider using 'logResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparrsLogResult :: Lens.Lens' RunPipelineActivityResponse (Core.Maybe Types.LogResult)
rparrsLogResult = Lens.field @"logResult"
{-# INLINEABLE rparrsLogResult #-}
{-# DEPRECATED logResult "Use generic-lens or generic-optics with 'logResult' instead"  #-}

-- | The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparrsPayloads :: Lens.Lens' RunPipelineActivityResponse (Core.Maybe (Core.NonEmpty Core.Base64))
rparrsPayloads = Lens.field @"payloads"
{-# INLINEABLE rparrsPayloads #-}
{-# DEPRECATED payloads "Use generic-lens or generic-optics with 'payloads' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparrsResponseStatus :: Lens.Lens' RunPipelineActivityResponse Core.Int
rparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
