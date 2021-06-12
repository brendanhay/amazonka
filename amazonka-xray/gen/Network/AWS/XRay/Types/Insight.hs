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
-- Module      : Network.AWS.XRay.Types.Insight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Insight where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ServiceId

-- | When fault rates go outside of the expected range, X-Ray creates an
-- insight. Insights tracks emergent issues within your applications.
--
-- /See:/ 'newInsight' smart constructor.
data Insight = Insight'
  { -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | The name of the group that the insight belongs to.
    groupName :: Core.Maybe Core.Text,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Core.Maybe Core.POSIX,
    rootCauseServiceId :: Core.Maybe ServiceId,
    -- | The current state of the insight.
    state :: Core.Maybe InsightState,
    -- | A brief description of the insight.
    summary :: Core.Maybe Core.Text,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Core.Maybe [AnomalousService],
    -- | The categories that label and describe the type of insight.
    categories :: Core.Maybe [InsightCategory],
    -- | The insights unique identifier.
    insightId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Insight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestImpactStatistics', 'insight_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insight_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'groupName', 'insight_groupName' - The name of the group that the insight belongs to.
--
-- 'startTime', 'insight_startTime' - The time, in Unix seconds, at which the insight began.
--
-- 'endTime', 'insight_endTime' - The time, in Unix seconds, at which the insight ended.
--
-- 'rootCauseServiceId', 'insight_rootCauseServiceId' - Undocumented member.
--
-- 'state', 'insight_state' - The current state of the insight.
--
-- 'summary', 'insight_summary' - A brief description of the insight.
--
-- 'topAnomalousServices', 'insight_topAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- 'categories', 'insight_categories' - The categories that label and describe the type of insight.
--
-- 'insightId', 'insight_insightId' - The insights unique identifier.
--
-- 'groupARN', 'insight_groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
newInsight ::
  Insight
newInsight =
  Insight'
    { clientRequestImpactStatistics =
        Core.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Core.Nothing,
      groupName = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      rootCauseServiceId = Core.Nothing,
      state = Core.Nothing,
      summary = Core.Nothing,
      topAnomalousServices = Core.Nothing,
      categories = Core.Nothing,
      insightId = Core.Nothing,
      groupARN = Core.Nothing
    }

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_clientRequestImpactStatistics :: Lens.Lens' Insight (Core.Maybe RequestImpactStatistics)
insight_clientRequestImpactStatistics = Lens.lens (\Insight' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@Insight' {} a -> s {clientRequestImpactStatistics = a} :: Insight)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_rootCauseServiceRequestImpactStatistics :: Lens.Lens' Insight (Core.Maybe RequestImpactStatistics)
insight_rootCauseServiceRequestImpactStatistics = Lens.lens (\Insight' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@Insight' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: Insight)

-- | The name of the group that the insight belongs to.
insight_groupName :: Lens.Lens' Insight (Core.Maybe Core.Text)
insight_groupName = Lens.lens (\Insight' {groupName} -> groupName) (\s@Insight' {} a -> s {groupName = a} :: Insight)

-- | The time, in Unix seconds, at which the insight began.
insight_startTime :: Lens.Lens' Insight (Core.Maybe Core.UTCTime)
insight_startTime = Lens.lens (\Insight' {startTime} -> startTime) (\s@Insight' {} a -> s {startTime = a} :: Insight) Core.. Lens.mapping Core._Time

-- | The time, in Unix seconds, at which the insight ended.
insight_endTime :: Lens.Lens' Insight (Core.Maybe Core.UTCTime)
insight_endTime = Lens.lens (\Insight' {endTime} -> endTime) (\s@Insight' {} a -> s {endTime = a} :: Insight) Core.. Lens.mapping Core._Time

-- | Undocumented member.
insight_rootCauseServiceId :: Lens.Lens' Insight (Core.Maybe ServiceId)
insight_rootCauseServiceId = Lens.lens (\Insight' {rootCauseServiceId} -> rootCauseServiceId) (\s@Insight' {} a -> s {rootCauseServiceId = a} :: Insight)

-- | The current state of the insight.
insight_state :: Lens.Lens' Insight (Core.Maybe InsightState)
insight_state = Lens.lens (\Insight' {state} -> state) (\s@Insight' {} a -> s {state = a} :: Insight)

-- | A brief description of the insight.
insight_summary :: Lens.Lens' Insight (Core.Maybe Core.Text)
insight_summary = Lens.lens (\Insight' {summary} -> summary) (\s@Insight' {} a -> s {summary = a} :: Insight)

-- | The service within the insight that is most impacted by the incident.
insight_topAnomalousServices :: Lens.Lens' Insight (Core.Maybe [AnomalousService])
insight_topAnomalousServices = Lens.lens (\Insight' {topAnomalousServices} -> topAnomalousServices) (\s@Insight' {} a -> s {topAnomalousServices = a} :: Insight) Core.. Lens.mapping Lens._Coerce

-- | The categories that label and describe the type of insight.
insight_categories :: Lens.Lens' Insight (Core.Maybe [InsightCategory])
insight_categories = Lens.lens (\Insight' {categories} -> categories) (\s@Insight' {} a -> s {categories = a} :: Insight) Core.. Lens.mapping Lens._Coerce

-- | The insights unique identifier.
insight_insightId :: Lens.Lens' Insight (Core.Maybe Core.Text)
insight_insightId = Lens.lens (\Insight' {insightId} -> insightId) (\s@Insight' {} a -> s {insightId = a} :: Insight)

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insight_groupARN :: Lens.Lens' Insight (Core.Maybe Core.Text)
insight_groupARN = Lens.lens (\Insight' {groupARN} -> groupARN) (\s@Insight' {} a -> s {groupARN = a} :: Insight)

instance Core.FromJSON Insight where
  parseJSON =
    Core.withObject
      "Insight"
      ( \x ->
          Insight'
            Core.<$> (x Core..:? "ClientRequestImpactStatistics")
            Core.<*> ( x
                         Core..:? "RootCauseServiceRequestImpactStatistics"
                     )
            Core.<*> (x Core..:? "GroupName")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "RootCauseServiceId")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Summary")
            Core.<*> ( x Core..:? "TopAnomalousServices"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Categories" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InsightId")
            Core.<*> (x Core..:? "GroupARN")
      )

instance Core.Hashable Insight

instance Core.NFData Insight
