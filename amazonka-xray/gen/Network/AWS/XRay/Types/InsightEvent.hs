{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | A brief description of the event.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The service during the event that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    -- | The time, in Unix seconds, at which the event was recorded.
    eventTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      summary = Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      eventTime = Prelude.Nothing
    }

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_clientRequestImpactStatistics :: Lens.Lens' InsightEvent (Prelude.Maybe RequestImpactStatistics)
insightEvent_clientRequestImpactStatistics = Lens.lens (\InsightEvent' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightEvent' {} a -> s {clientRequestImpactStatistics = a} :: InsightEvent)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightEvent (Prelude.Maybe RequestImpactStatistics)
insightEvent_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightEvent' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightEvent' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightEvent)

-- | A brief description of the event.
insightEvent_summary :: Lens.Lens' InsightEvent (Prelude.Maybe Prelude.Text)
insightEvent_summary = Lens.lens (\InsightEvent' {summary} -> summary) (\s@InsightEvent' {} a -> s {summary = a} :: InsightEvent)

-- | The service during the event that is most impacted by the incident.
insightEvent_topAnomalousServices :: Lens.Lens' InsightEvent (Prelude.Maybe [AnomalousService])
insightEvent_topAnomalousServices = Lens.lens (\InsightEvent' {topAnomalousServices} -> topAnomalousServices) (\s@InsightEvent' {} a -> s {topAnomalousServices = a} :: InsightEvent) Prelude.. Lens.mapping Prelude._Coerce

-- | The time, in Unix seconds, at which the event was recorded.
insightEvent_eventTime :: Lens.Lens' InsightEvent (Prelude.Maybe Prelude.UTCTime)
insightEvent_eventTime = Lens.lens (\InsightEvent' {eventTime} -> eventTime) (\s@InsightEvent' {} a -> s {eventTime = a} :: InsightEvent) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON InsightEvent where
  parseJSON =
    Prelude.withObject
      "InsightEvent"
      ( \x ->
          InsightEvent'
            Prelude.<$> (x Prelude..:? "ClientRequestImpactStatistics")
            Prelude.<*> ( x
                            Prelude..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> (x Prelude..:? "Summary")
            Prelude.<*> ( x Prelude..:? "TopAnomalousServices"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "EventTime")
      )

instance Prelude.Hashable InsightEvent

instance Prelude.NFData InsightEvent
