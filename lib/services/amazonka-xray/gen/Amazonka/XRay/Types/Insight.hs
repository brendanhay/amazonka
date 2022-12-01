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
-- Module      : Amazonka.XRay.Types.Insight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Insight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.AnomalousService
import Amazonka.XRay.Types.InsightCategory
import Amazonka.XRay.Types.InsightState
import Amazonka.XRay.Types.RequestImpactStatistics
import Amazonka.XRay.Types.ServiceId

-- | When fault rates go outside of the expected range, X-Ray creates an
-- insight. Insights tracks emergent issues within your applications.
--
-- /See:/ 'newInsight' smart constructor.
data Insight = Insight'
  { rootCauseServiceId :: Prelude.Maybe ServiceId,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    -- | The insights unique identifier.
    insightId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the insight.
    state :: Prelude.Maybe InsightState,
    -- | A brief description of the insight.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The name of the group that the insight belongs to.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The categories that label and describe the type of insight.
    categories :: Prelude.Maybe [InsightCategory],
    -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Prelude.Maybe Core.POSIX
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
-- 'rootCauseServiceId', 'insight_rootCauseServiceId' - Undocumented member.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insight_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'topAnomalousServices', 'insight_topAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- 'insightId', 'insight_insightId' - The insights unique identifier.
--
-- 'state', 'insight_state' - The current state of the insight.
--
-- 'summary', 'insight_summary' - A brief description of the insight.
--
-- 'groupName', 'insight_groupName' - The name of the group that the insight belongs to.
--
-- 'endTime', 'insight_endTime' - The time, in Unix seconds, at which the insight ended.
--
-- 'categories', 'insight_categories' - The categories that label and describe the type of insight.
--
-- 'clientRequestImpactStatistics', 'insight_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'groupARN', 'insight_groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- 'startTime', 'insight_startTime' - The time, in Unix seconds, at which the insight began.
newInsight ::
  Insight
newInsight =
  Insight'
    { rootCauseServiceId = Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      insightId = Prelude.Nothing,
      state = Prelude.Nothing,
      summary = Prelude.Nothing,
      groupName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      categories = Prelude.Nothing,
      clientRequestImpactStatistics = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Undocumented member.
insight_rootCauseServiceId :: Lens.Lens' Insight (Prelude.Maybe ServiceId)
insight_rootCauseServiceId = Lens.lens (\Insight' {rootCauseServiceId} -> rootCauseServiceId) (\s@Insight' {} a -> s {rootCauseServiceId = a} :: Insight)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_rootCauseServiceRequestImpactStatistics :: Lens.Lens' Insight (Prelude.Maybe RequestImpactStatistics)
insight_rootCauseServiceRequestImpactStatistics = Lens.lens (\Insight' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@Insight' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: Insight)

-- | The service within the insight that is most impacted by the incident.
insight_topAnomalousServices :: Lens.Lens' Insight (Prelude.Maybe [AnomalousService])
insight_topAnomalousServices = Lens.lens (\Insight' {topAnomalousServices} -> topAnomalousServices) (\s@Insight' {} a -> s {topAnomalousServices = a} :: Insight) Prelude.. Lens.mapping Lens.coerced

-- | The insights unique identifier.
insight_insightId :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_insightId = Lens.lens (\Insight' {insightId} -> insightId) (\s@Insight' {} a -> s {insightId = a} :: Insight)

-- | The current state of the insight.
insight_state :: Lens.Lens' Insight (Prelude.Maybe InsightState)
insight_state = Lens.lens (\Insight' {state} -> state) (\s@Insight' {} a -> s {state = a} :: Insight)

-- | A brief description of the insight.
insight_summary :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_summary = Lens.lens (\Insight' {summary} -> summary) (\s@Insight' {} a -> s {summary = a} :: Insight)

-- | The name of the group that the insight belongs to.
insight_groupName :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_groupName = Lens.lens (\Insight' {groupName} -> groupName) (\s@Insight' {} a -> s {groupName = a} :: Insight)

-- | The time, in Unix seconds, at which the insight ended.
insight_endTime :: Lens.Lens' Insight (Prelude.Maybe Prelude.UTCTime)
insight_endTime = Lens.lens (\Insight' {endTime} -> endTime) (\s@Insight' {} a -> s {endTime = a} :: Insight) Prelude.. Lens.mapping Core._Time

-- | The categories that label and describe the type of insight.
insight_categories :: Lens.Lens' Insight (Prelude.Maybe [InsightCategory])
insight_categories = Lens.lens (\Insight' {categories} -> categories) (\s@Insight' {} a -> s {categories = a} :: Insight) Prelude.. Lens.mapping Lens.coerced

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insight_clientRequestImpactStatistics :: Lens.Lens' Insight (Prelude.Maybe RequestImpactStatistics)
insight_clientRequestImpactStatistics = Lens.lens (\Insight' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@Insight' {} a -> s {clientRequestImpactStatistics = a} :: Insight)

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insight_groupARN :: Lens.Lens' Insight (Prelude.Maybe Prelude.Text)
insight_groupARN = Lens.lens (\Insight' {groupARN} -> groupARN) (\s@Insight' {} a -> s {groupARN = a} :: Insight)

-- | The time, in Unix seconds, at which the insight began.
insight_startTime :: Lens.Lens' Insight (Prelude.Maybe Prelude.UTCTime)
insight_startTime = Lens.lens (\Insight' {startTime} -> startTime) (\s@Insight' {} a -> s {startTime = a} :: Insight) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Insight where
  parseJSON =
    Core.withObject
      "Insight"
      ( \x ->
          Insight'
            Prelude.<$> (x Core..:? "RootCauseServiceId")
            Prelude.<*> ( x
                            Core..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> ( x Core..:? "TopAnomalousServices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "InsightId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "GroupName")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "Categories" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ClientRequestImpactStatistics")
            Prelude.<*> (x Core..:? "GroupARN")
            Prelude.<*> (x Core..:? "StartTime")
      )

instance Prelude.Hashable Insight where
  hashWithSalt _salt Insight' {..} =
    _salt `Prelude.hashWithSalt` rootCauseServiceId
      `Prelude.hashWithSalt` rootCauseServiceRequestImpactStatistics
      `Prelude.hashWithSalt` topAnomalousServices
      `Prelude.hashWithSalt` insightId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` categories
      `Prelude.hashWithSalt` clientRequestImpactStatistics
      `Prelude.hashWithSalt` groupARN
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData Insight where
  rnf Insight' {..} =
    Prelude.rnf rootCauseServiceId
      `Prelude.seq` Prelude.rnf rootCauseServiceRequestImpactStatistics
      `Prelude.seq` Prelude.rnf topAnomalousServices
      `Prelude.seq` Prelude.rnf insightId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf clientRequestImpactStatistics
      `Prelude.seq` Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf startTime
