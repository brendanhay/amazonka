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
import qualified Network.AWS.Prelude as Prelude
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
  { -- | A brief description of the insight.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The current state of the insight.
    state :: Prelude.Maybe InsightState,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The insights unique identifier.
    insightId :: Prelude.Maybe Prelude.Text,
    -- | The categories that label and describe the type of insight.
    categories :: Prelude.Maybe [InsightCategory],
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    rootCauseServiceId :: Prelude.Maybe ServiceId,
    -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the group that the insight belongs to.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Insight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'insight_summary' - A brief description of the insight.
--
-- 'state', 'insight_state' - The current state of the insight.
--
-- 'startTime', 'insight_startTime' - The time, in Unix seconds, at which the insight began.
--
-- 'insightId', 'insight_insightId' - The insights unique identifier.
--
-- 'categories', 'insight_categories' - The categories that label and describe the type of insight.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insight_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'topAnomalousServices', 'insight_topAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- 'rootCauseServiceId', 'insight_rootCauseServiceId' - Undocumented member.
--
-- 'clientRequestImpactStatistics', 'insight_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'endTime', 'insight_endTime' - The time, in Unix seconds, at which the insight ended.
--
-- 'groupARN', 'insight_groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- 'groupName', 'insight_groupName' - The name of the group that the insight belongs to.
newInsight ::
  Insight
newInsight =
  Insight'
    { summary = Prelude.Nothing,
      state = Prelude.Nothing,
      startTime = Prelude.Nothing,
      insightId = Prelude.Nothing,
      categories = Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      rootCauseServiceId = Prelude.Nothing,
      clientRequestImpactStatistics = Prelude.Nothing,
      endTime = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | A brief description of the insight.
insight_summary :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_summary = Lens.lens (\Insight' {summary} -> summary) (\s@Insight' {} a -> s {summary = a} :: Insight)

-- | The current state of the insight.
insight_state :: Lens.Lens' Insight (Prelude.Maybe InsightState)
insight_state = Lens.lens (\Insight' {state} -> state) (\s@Insight' {} a -> s {state = a} :: Insight)

-- | The time, in Unix seconds, at which the insight began.
insight_startTime :: Lens.Lens' Insight (Prelude.Maybe Prelude.UTCTime)
insight_startTime = Lens.lens (\Insight' {startTime} -> startTime) (\s@Insight' {} a -> s {startTime = a} :: Insight) Prelude.. Lens.mapping Core._Time

-- | The insights unique identifier.
insight_insightId :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_insightId = Lens.lens (\Insight' {insightId} -> insightId) (\s@Insight' {} a -> s {insightId = a} :: Insight)

-- | The categories that label and describe the type of insight.
insight_categories :: Lens.Lens' Insight (Prelude.Maybe [InsightCategory])
insight_categories = Lens.lens (\Insight' {categories} -> categories) (\s@Insight' {} a -> s {categories = a} :: Insight) Prelude.. Lens.mapping Lens.coerced

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_rootCauseServiceRequestImpactStatistics :: Lens.Lens' Insight (Prelude.Maybe RequestImpactStatistics)
insight_rootCauseServiceRequestImpactStatistics = Lens.lens (\Insight' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@Insight' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: Insight)

-- | The service within the insight that is most impacted by the incident.
insight_topAnomalousServices :: Lens.Lens' Insight (Prelude.Maybe [AnomalousService])
insight_topAnomalousServices = Lens.lens (\Insight' {topAnomalousServices} -> topAnomalousServices) (\s@Insight' {} a -> s {topAnomalousServices = a} :: Insight) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
insight_rootCauseServiceId :: Lens.Lens' Insight (Prelude.Maybe ServiceId)
insight_rootCauseServiceId = Lens.lens (\Insight' {rootCauseServiceId} -> rootCauseServiceId) (\s@Insight' {} a -> s {rootCauseServiceId = a} :: Insight)

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_clientRequestImpactStatistics :: Lens.Lens' Insight (Prelude.Maybe RequestImpactStatistics)
insight_clientRequestImpactStatistics = Lens.lens (\Insight' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@Insight' {} a -> s {clientRequestImpactStatistics = a} :: Insight)

-- | The time, in Unix seconds, at which the insight ended.
insight_endTime :: Lens.Lens' Insight (Prelude.Maybe Prelude.UTCTime)
insight_endTime = Lens.lens (\Insight' {endTime} -> endTime) (\s@Insight' {} a -> s {endTime = a} :: Insight) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insight_groupARN :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_groupARN = Lens.lens (\Insight' {groupARN} -> groupARN) (\s@Insight' {} a -> s {groupARN = a} :: Insight)

-- | The name of the group that the insight belongs to.
insight_groupName :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_groupName = Lens.lens (\Insight' {groupName} -> groupName) (\s@Insight' {} a -> s {groupName = a} :: Insight)

instance Core.FromJSON Insight where
  parseJSON =
    Core.withObject
      "Insight"
      ( \x ->
          Insight'
            Prelude.<$> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "InsightId")
            Prelude.<*> (x Core..:? "Categories" Core..!= Prelude.mempty)
            Prelude.<*> ( x
                            Core..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> ( x Core..:? "TopAnomalousServices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RootCauseServiceId")
            Prelude.<*> (x Core..:? "ClientRequestImpactStatistics")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "GroupARN")
            Prelude.<*> (x Core..:? "GroupName")
      )

instance Prelude.Hashable Insight

instance Prelude.NFData Insight
