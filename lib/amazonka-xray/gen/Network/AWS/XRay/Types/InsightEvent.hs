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
    ieSummary,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieTopAnomalousServices,
    ieClientRequestImpactStatistics,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.RequestImpactStatistics

-- | X-Ray reevaluates insights periodically until they are resolved, and records each intermediate state in an event. You can review incident events in the Impact Timeline on the Inspect page in the X-Ray console.
--
-- /See:/ 'mkInsightEvent' smart constructor.
data InsightEvent = InsightEvent'
  { -- | A brief description of the event.
    summary :: Lude.Maybe Lude.Text,
    -- | The time, in Unix seconds, at which the event was recorded.
    eventTime :: Lude.Maybe Lude.Timestamp,
    -- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
    rootCauseServiceRequestImpactStatistics :: Lude.Maybe RequestImpactStatistics,
    -- | The service during the event that is most impacted by the incident.
    topAnomalousServices :: Lude.Maybe [AnomalousService],
    -- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
    clientRequestImpactStatistics :: Lude.Maybe RequestImpactStatistics
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightEvent' with the minimum fields required to make a request.
--
-- * 'summary' - A brief description of the event.
-- * 'eventTime' - The time, in Unix seconds, at which the event was recorded.
-- * 'rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
-- * 'topAnomalousServices' - The service during the event that is most impacted by the incident.
-- * 'clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
mkInsightEvent ::
  InsightEvent
mkInsightEvent =
  InsightEvent'
    { summary = Lude.Nothing,
      eventTime = Lude.Nothing,
      rootCauseServiceRequestImpactStatistics = Lude.Nothing,
      topAnomalousServices = Lude.Nothing,
      clientRequestImpactStatistics = Lude.Nothing
    }

-- | A brief description of the event.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSummary :: Lens.Lens' InsightEvent (Lude.Maybe Lude.Text)
ieSummary = Lens.lens (summary :: InsightEvent -> Lude.Maybe Lude.Text) (\s a -> s {summary = a} :: InsightEvent)
{-# DEPRECATED ieSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The time, in Unix seconds, at which the event was recorded.
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieEventTime :: Lens.Lens' InsightEvent (Lude.Maybe Lude.Timestamp)
ieEventTime = Lens.lens (eventTime :: InsightEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventTime = a} :: InsightEvent)
{-# DEPRECATED ieEventTime "Use generic-lens or generic-optics with 'eventTime' instead." #-}

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'rootCauseServiceRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieRootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightEvent (Lude.Maybe RequestImpactStatistics)
ieRootCauseServiceRequestImpactStatistics = Lens.lens (rootCauseServiceRequestImpactStatistics :: InsightEvent -> Lude.Maybe RequestImpactStatistics) (\s a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightEvent)
{-# DEPRECATED ieRootCauseServiceRequestImpactStatistics "Use generic-lens or generic-optics with 'rootCauseServiceRequestImpactStatistics' instead." #-}

-- | The service during the event that is most impacted by the incident.
--
-- /Note:/ Consider using 'topAnomalousServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTopAnomalousServices :: Lens.Lens' InsightEvent (Lude.Maybe [AnomalousService])
ieTopAnomalousServices = Lens.lens (topAnomalousServices :: InsightEvent -> Lude.Maybe [AnomalousService]) (\s a -> s {topAnomalousServices = a} :: InsightEvent)
{-# DEPRECATED ieTopAnomalousServices "Use generic-lens or generic-optics with 'topAnomalousServices' instead." #-}

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'clientRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieClientRequestImpactStatistics :: Lens.Lens' InsightEvent (Lude.Maybe RequestImpactStatistics)
ieClientRequestImpactStatistics = Lens.lens (clientRequestImpactStatistics :: InsightEvent -> Lude.Maybe RequestImpactStatistics) (\s a -> s {clientRequestImpactStatistics = a} :: InsightEvent)
{-# DEPRECATED ieClientRequestImpactStatistics "Use generic-lens or generic-optics with 'clientRequestImpactStatistics' instead." #-}

instance Lude.FromJSON InsightEvent where
  parseJSON =
    Lude.withObject
      "InsightEvent"
      ( \x ->
          InsightEvent'
            Lude.<$> (x Lude..:? "Summary")
            Lude.<*> (x Lude..:? "EventTime")
            Lude.<*> (x Lude..:? "RootCauseServiceRequestImpactStatistics")
            Lude.<*> (x Lude..:? "TopAnomalousServices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ClientRequestImpactStatistics")
      )
