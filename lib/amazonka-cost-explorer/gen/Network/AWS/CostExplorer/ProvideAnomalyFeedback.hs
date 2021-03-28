{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.ProvideAnomalyFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the feedback property of a given cost anomaly. 
module Network.AWS.CostExplorer.ProvideAnomalyFeedback
    (
    -- * Creating a request
      ProvideAnomalyFeedback (..)
    , mkProvideAnomalyFeedback
    -- ** Request lenses
    , pafAnomalyId
    , pafFeedback

    -- * Destructuring the response
    , ProvideAnomalyFeedbackResponse (..)
    , mkProvideAnomalyFeedbackResponse
    -- ** Response lenses
    , pafrrsAnomalyId
    , pafrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkProvideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { anomalyId :: Types.GenericString
    -- ^ A cost anomaly ID. 
  , feedback :: Types.AnomalyFeedbackType
    -- ^ Describes whether the cost anomaly was a planned activity or you considered it an anomaly. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvideAnomalyFeedback' value with any optional fields omitted.
mkProvideAnomalyFeedback
    :: Types.GenericString -- ^ 'anomalyId'
    -> Types.AnomalyFeedbackType -- ^ 'feedback'
    -> ProvideAnomalyFeedback
mkProvideAnomalyFeedback anomalyId feedback
  = ProvideAnomalyFeedback'{anomalyId, feedback}

-- | A cost anomaly ID. 
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafAnomalyId :: Lens.Lens' ProvideAnomalyFeedback Types.GenericString
pafAnomalyId = Lens.field @"anomalyId"
{-# INLINEABLE pafAnomalyId #-}
{-# DEPRECATED anomalyId "Use generic-lens or generic-optics with 'anomalyId' instead"  #-}

-- | Describes whether the cost anomaly was a planned activity or you considered it an anomaly. 
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafFeedback :: Lens.Lens' ProvideAnomalyFeedback Types.AnomalyFeedbackType
pafFeedback = Lens.field @"feedback"
{-# INLINEABLE pafFeedback #-}
{-# DEPRECATED feedback "Use generic-lens or generic-optics with 'feedback' instead"  #-}

instance Core.ToQuery ProvideAnomalyFeedback where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ProvideAnomalyFeedback where
        toHeaders ProvideAnomalyFeedback{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.ProvideAnomalyFeedback")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ProvideAnomalyFeedback where
        toJSON ProvideAnomalyFeedback{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AnomalyId" Core..= anomalyId),
                  Core.Just ("Feedback" Core..= feedback)])

instance Core.AWSRequest ProvideAnomalyFeedback where
        type Rs ProvideAnomalyFeedback = ProvideAnomalyFeedbackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ProvideAnomalyFeedbackResponse' Core.<$>
                   (x Core..: "AnomalyId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkProvideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { anomalyId :: Types.GenericString
    -- ^ The ID of the modified cost anomaly. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvideAnomalyFeedbackResponse' value with any optional fields omitted.
mkProvideAnomalyFeedbackResponse
    :: Types.GenericString -- ^ 'anomalyId'
    -> Core.Int -- ^ 'responseStatus'
    -> ProvideAnomalyFeedbackResponse
mkProvideAnomalyFeedbackResponse anomalyId responseStatus
  = ProvideAnomalyFeedbackResponse'{anomalyId, responseStatus}

-- | The ID of the modified cost anomaly. 
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrrsAnomalyId :: Lens.Lens' ProvideAnomalyFeedbackResponse Types.GenericString
pafrrsAnomalyId = Lens.field @"anomalyId"
{-# INLINEABLE pafrrsAnomalyId #-}
{-# DEPRECATED anomalyId "Use generic-lens or generic-optics with 'anomalyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrrsResponseStatus :: Lens.Lens' ProvideAnomalyFeedbackResponse Core.Int
pafrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pafrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
