{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.RequestImpactStatistics

-- | X-Ray reevaluates insights periodically until they are resolved, and
-- records each intermediate state in an event. You can review incident
-- events in the Impact Timeline on the Inspect page in the X-Ray console.
--
-- /See:/ 'newInsightEvent' smart constructor.
data InsightEvent = InsightEvent'
  { -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | A brief description of the event.
    summary :: Core.Maybe Core.Text,
    -- | The service during the event that is most impacted by the incident.
    topAnomalousServices :: Core.Maybe [AnomalousService],
    -- | The time, in Unix seconds, at which the event was recorded.
    eventTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestImpactStatistics', 'insightEvent_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insightEvent_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'summary', 'insightEvent_summary' - A brief description of the event.
--
-- 'topAnomalousServices', 'insightEvent_topAnomalousServices' - The service during the event that is most impacted by the incident.
--
-- 'eventTime', 'insightEvent_eventTime' - The time, in Unix seconds, at which the event was recorded.
newInsightEvent ::
  InsightEvent
newInsightEvent =
  InsightEvent'
    { clientRequestImpactStatistics =
        Core.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Core.Nothing,
      summary = Core.Nothing,
      topAnomalousServices = Core.Nothing,
      eventTime = Core.Nothing
    }

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_clientRequestImpactStatistics :: Lens.Lens' InsightEvent (Core.Maybe RequestImpactStatistics)
insightEvent_clientRequestImpactStatistics = Lens.lens (\InsightEvent' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightEvent' {} a -> s {clientRequestImpactStatistics = a} :: InsightEvent)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightEvent (Core.Maybe RequestImpactStatistics)
insightEvent_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightEvent' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightEvent' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightEvent)

-- | A brief description of the event.
insightEvent_summary :: Lens.Lens' InsightEvent (Core.Maybe Core.Text)
insightEvent_summary = Lens.lens (\InsightEvent' {summary} -> summary) (\s@InsightEvent' {} a -> s {summary = a} :: InsightEvent)

-- | The service during the event that is most impacted by the incident.
insightEvent_topAnomalousServices :: Lens.Lens' InsightEvent (Core.Maybe [AnomalousService])
insightEvent_topAnomalousServices = Lens.lens (\InsightEvent' {topAnomalousServices} -> topAnomalousServices) (\s@InsightEvent' {} a -> s {topAnomalousServices = a} :: InsightEvent) Core.. Lens.mapping Lens._Coerce

-- | The time, in Unix seconds, at which the event was recorded.
insightEvent_eventTime :: Lens.Lens' InsightEvent (Core.Maybe Core.UTCTime)
insightEvent_eventTime = Lens.lens (\InsightEvent' {eventTime} -> eventTime) (\s@InsightEvent' {} a -> s {eventTime = a} :: InsightEvent) Core.. Lens.mapping Core._Time

instance Core.FromJSON InsightEvent where
  parseJSON =
    Core.withObject
      "InsightEvent"
      ( \x ->
          InsightEvent'
            Core.<$> (x Core..:? "ClientRequestImpactStatistics")
            Core.<*> ( x
                         Core..:? "RootCauseServiceRequestImpactStatistics"
                     )
            Core.<*> (x Core..:? "Summary")
            Core.<*> ( x Core..:? "TopAnomalousServices"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "EventTime")
      )

instance Core.Hashable InsightEvent

instance Core.NFData InsightEvent
