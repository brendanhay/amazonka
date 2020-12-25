{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightEvent
  ( InsightEvent (..),

    -- * Smart constructor
    mkInsightEvent,

    -- * Lenses
    ieClientRequestImpactStatistics,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieSummary,
    ieTopAnomalousServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AnomalousService as Types
import qualified Network.AWS.XRay.Types.EventSummaryText as Types
import qualified Network.AWS.XRay.Types.RequestImpactStatistics as Types

-- | X-Ray reevaluates insights periodically until they are resolved, and records each intermediate state in an event. You can review incident events in the Impact Timeline on the Inspect page in the X-Ray console.
--
-- /See:/ 'mkInsightEvent' smart constructor.
data InsightEvent = InsightEvent'
  { -- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
    clientRequestImpactStatistics :: Core.Maybe Types.RequestImpactStatistics,
    -- | The time, in Unix seconds, at which the event was recorded.
    eventTime :: Core.Maybe Core.NominalDiffTime,
    -- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
    rootCauseServiceRequestImpactStatistics :: Core.Maybe Types.RequestImpactStatistics,
    -- | A brief description of the event.
    summary :: Core.Maybe Types.EventSummaryText,
    -- | The service during the event that is most impacted by the incident.
    topAnomalousServices :: Core.Maybe [Types.AnomalousService]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InsightEvent' value with any optional fields omitted.
mkInsightEvent ::
  InsightEvent
mkInsightEvent =
  InsightEvent'
    { clientRequestImpactStatistics = Core.Nothing,
      eventTime = Core.Nothing,
      rootCauseServiceRequestImpactStatistics = Core.Nothing,
      summary = Core.Nothing,
      topAnomalousServices = Core.Nothing
    }

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'clientRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieClientRequestImpactStatistics :: Lens.Lens' InsightEvent (Core.Maybe Types.RequestImpactStatistics)
ieClientRequestImpactStatistics = Lens.field @"clientRequestImpactStatistics"
{-# DEPRECATED ieClientRequestImpactStatistics "Use generic-lens or generic-optics with 'clientRequestImpactStatistics' instead." #-}

-- | The time, in Unix seconds, at which the event was recorded.
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieEventTime :: Lens.Lens' InsightEvent (Core.Maybe Core.NominalDiffTime)
ieEventTime = Lens.field @"eventTime"
{-# DEPRECATED ieEventTime "Use generic-lens or generic-optics with 'eventTime' instead." #-}

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'rootCauseServiceRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieRootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightEvent (Core.Maybe Types.RequestImpactStatistics)
ieRootCauseServiceRequestImpactStatistics = Lens.field @"rootCauseServiceRequestImpactStatistics"
{-# DEPRECATED ieRootCauseServiceRequestImpactStatistics "Use generic-lens or generic-optics with 'rootCauseServiceRequestImpactStatistics' instead." #-}

-- | A brief description of the event.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSummary :: Lens.Lens' InsightEvent (Core.Maybe Types.EventSummaryText)
ieSummary = Lens.field @"summary"
{-# DEPRECATED ieSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The service during the event that is most impacted by the incident.
--
-- /Note:/ Consider using 'topAnomalousServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTopAnomalousServices :: Lens.Lens' InsightEvent (Core.Maybe [Types.AnomalousService])
ieTopAnomalousServices = Lens.field @"topAnomalousServices"
{-# DEPRECATED ieTopAnomalousServices "Use generic-lens or generic-optics with 'topAnomalousServices' instead." #-}

instance Core.FromJSON InsightEvent where
  parseJSON =
    Core.withObject "InsightEvent" Core.$
      \x ->
        InsightEvent'
          Core.<$> (x Core..:? "ClientRequestImpactStatistics")
          Core.<*> (x Core..:? "EventTime")
          Core.<*> (x Core..:? "RootCauseServiceRequestImpactStatistics")
          Core.<*> (x Core..:? "Summary")
          Core.<*> (x Core..:? "TopAnomalousServices")
