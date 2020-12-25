{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ProvideAnomalyFeedback (..),
    mkProvideAnomalyFeedback,

    -- ** Request lenses
    pafAnomalyId,
    pafFeedback,

    -- * Destructuring the response
    ProvideAnomalyFeedbackResponse (..),
    mkProvideAnomalyFeedbackResponse,

    -- ** Response lenses
    pafrrsAnomalyId,
    pafrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkProvideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { -- | A cost anomaly ID.
    anomalyId :: Types.GenericString,
    -- | Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
    feedback :: Types.AnomalyFeedbackType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvideAnomalyFeedback' value with any optional fields omitted.
mkProvideAnomalyFeedback ::
  -- | 'anomalyId'
  Types.GenericString ->
  -- | 'feedback'
  Types.AnomalyFeedbackType ->
  ProvideAnomalyFeedback
mkProvideAnomalyFeedback anomalyId feedback =
  ProvideAnomalyFeedback' {anomalyId, feedback}

-- | A cost anomaly ID.
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafAnomalyId :: Lens.Lens' ProvideAnomalyFeedback Types.GenericString
pafAnomalyId = Lens.field @"anomalyId"
{-# DEPRECATED pafAnomalyId "Use generic-lens or generic-optics with 'anomalyId' instead." #-}

-- | Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafFeedback :: Lens.Lens' ProvideAnomalyFeedback Types.AnomalyFeedbackType
pafFeedback = Lens.field @"feedback"
{-# DEPRECATED pafFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

instance Core.FromJSON ProvideAnomalyFeedback where
  toJSON ProvideAnomalyFeedback {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AnomalyId" Core..= anomalyId),
            Core.Just ("Feedback" Core..= feedback)
          ]
      )

instance Core.AWSRequest ProvideAnomalyFeedback where
  type Rs ProvideAnomalyFeedback = ProvideAnomalyFeedbackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.ProvideAnomalyFeedback")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvideAnomalyFeedbackResponse'
            Core.<$> (x Core..: "AnomalyId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkProvideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { -- | The ID of the modified cost anomaly.
    anomalyId :: Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvideAnomalyFeedbackResponse' value with any optional fields omitted.
mkProvideAnomalyFeedbackResponse ::
  -- | 'anomalyId'
  Types.GenericString ->
  -- | 'responseStatus'
  Core.Int ->
  ProvideAnomalyFeedbackResponse
mkProvideAnomalyFeedbackResponse anomalyId responseStatus =
  ProvideAnomalyFeedbackResponse' {anomalyId, responseStatus}

-- | The ID of the modified cost anomaly.
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrrsAnomalyId :: Lens.Lens' ProvideAnomalyFeedbackResponse Types.GenericString
pafrrsAnomalyId = Lens.field @"anomalyId"
{-# DEPRECATED pafrrsAnomalyId "Use generic-lens or generic-optics with 'anomalyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrrsResponseStatus :: Lens.Lens' ProvideAnomalyFeedbackResponse Core.Int
pafrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pafrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
