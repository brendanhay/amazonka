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
-- Module      : Network.AWS.XRay.Types.InsightSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ServiceId

-- | Information that describes an insight.
--
-- /See:/ 'newInsightSummary' smart constructor.
data InsightSummary = InsightSummary'
  { -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Core.Maybe RequestImpactStatistics,
    -- | The time, in Unix seconds, that the insight was last updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
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
    -- | Categories The categories that label and describe the type of insight.
    categories :: Core.Maybe [InsightCategory],
    -- | The insights unique identifier.
    insightId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestImpactStatistics', 'insightSummary_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insightSummary_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'lastUpdateTime', 'insightSummary_lastUpdateTime' - The time, in Unix seconds, that the insight was last updated.
--
-- 'groupName', 'insightSummary_groupName' - The name of the group that the insight belongs to.
--
-- 'startTime', 'insightSummary_startTime' - The time, in Unix seconds, at which the insight began.
--
-- 'endTime', 'insightSummary_endTime' - The time, in Unix seconds, at which the insight ended.
--
-- 'rootCauseServiceId', 'insightSummary_rootCauseServiceId' - Undocumented member.
--
-- 'state', 'insightSummary_state' - The current state of the insight.
--
-- 'summary', 'insightSummary_summary' - A brief description of the insight.
--
-- 'topAnomalousServices', 'insightSummary_topAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- 'categories', 'insightSummary_categories' - Categories The categories that label and describe the type of insight.
--
-- 'insightId', 'insightSummary_insightId' - The insights unique identifier.
--
-- 'groupARN', 'insightSummary_groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
newInsightSummary ::
  InsightSummary
newInsightSummary =
  InsightSummary'
    { clientRequestImpactStatistics =
        Core.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Core.Nothing,
      lastUpdateTime = Core.Nothing,
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
insightSummary_clientRequestImpactStatistics :: Lens.Lens' InsightSummary (Core.Maybe RequestImpactStatistics)
insightSummary_clientRequestImpactStatistics = Lens.lens (\InsightSummary' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightSummary' {} a -> s {clientRequestImpactStatistics = a} :: InsightSummary)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightSummary_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightSummary (Core.Maybe RequestImpactStatistics)
insightSummary_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightSummary' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightSummary' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightSummary)

-- | The time, in Unix seconds, that the insight was last updated.
insightSummary_lastUpdateTime :: Lens.Lens' InsightSummary (Core.Maybe Core.UTCTime)
insightSummary_lastUpdateTime = Lens.lens (\InsightSummary' {lastUpdateTime} -> lastUpdateTime) (\s@InsightSummary' {} a -> s {lastUpdateTime = a} :: InsightSummary) Core.. Lens.mapping Core._Time

-- | The name of the group that the insight belongs to.
insightSummary_groupName :: Lens.Lens' InsightSummary (Core.Maybe Core.Text)
insightSummary_groupName = Lens.lens (\InsightSummary' {groupName} -> groupName) (\s@InsightSummary' {} a -> s {groupName = a} :: InsightSummary)

-- | The time, in Unix seconds, at which the insight began.
insightSummary_startTime :: Lens.Lens' InsightSummary (Core.Maybe Core.UTCTime)
insightSummary_startTime = Lens.lens (\InsightSummary' {startTime} -> startTime) (\s@InsightSummary' {} a -> s {startTime = a} :: InsightSummary) Core.. Lens.mapping Core._Time

-- | The time, in Unix seconds, at which the insight ended.
insightSummary_endTime :: Lens.Lens' InsightSummary (Core.Maybe Core.UTCTime)
insightSummary_endTime = Lens.lens (\InsightSummary' {endTime} -> endTime) (\s@InsightSummary' {} a -> s {endTime = a} :: InsightSummary) Core.. Lens.mapping Core._Time

-- | Undocumented member.
insightSummary_rootCauseServiceId :: Lens.Lens' InsightSummary (Core.Maybe ServiceId)
insightSummary_rootCauseServiceId = Lens.lens (\InsightSummary' {rootCauseServiceId} -> rootCauseServiceId) (\s@InsightSummary' {} a -> s {rootCauseServiceId = a} :: InsightSummary)

-- | The current state of the insight.
insightSummary_state :: Lens.Lens' InsightSummary (Core.Maybe InsightState)
insightSummary_state = Lens.lens (\InsightSummary' {state} -> state) (\s@InsightSummary' {} a -> s {state = a} :: InsightSummary)

-- | A brief description of the insight.
insightSummary_summary :: Lens.Lens' InsightSummary (Core.Maybe Core.Text)
insightSummary_summary = Lens.lens (\InsightSummary' {summary} -> summary) (\s@InsightSummary' {} a -> s {summary = a} :: InsightSummary)

-- | The service within the insight that is most impacted by the incident.
insightSummary_topAnomalousServices :: Lens.Lens' InsightSummary (Core.Maybe [AnomalousService])
insightSummary_topAnomalousServices = Lens.lens (\InsightSummary' {topAnomalousServices} -> topAnomalousServices) (\s@InsightSummary' {} a -> s {topAnomalousServices = a} :: InsightSummary) Core.. Lens.mapping Lens._Coerce

-- | Categories The categories that label and describe the type of insight.
insightSummary_categories :: Lens.Lens' InsightSummary (Core.Maybe [InsightCategory])
insightSummary_categories = Lens.lens (\InsightSummary' {categories} -> categories) (\s@InsightSummary' {} a -> s {categories = a} :: InsightSummary) Core.. Lens.mapping Lens._Coerce

-- | The insights unique identifier.
insightSummary_insightId :: Lens.Lens' InsightSummary (Core.Maybe Core.Text)
insightSummary_insightId = Lens.lens (\InsightSummary' {insightId} -> insightId) (\s@InsightSummary' {} a -> s {insightId = a} :: InsightSummary)

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insightSummary_groupARN :: Lens.Lens' InsightSummary (Core.Maybe Core.Text)
insightSummary_groupARN = Lens.lens (\InsightSummary' {groupARN} -> groupARN) (\s@InsightSummary' {} a -> s {groupARN = a} :: InsightSummary)

instance Core.FromJSON InsightSummary where
  parseJSON =
    Core.withObject
      "InsightSummary"
      ( \x ->
          InsightSummary'
            Core.<$> (x Core..:? "ClientRequestImpactStatistics")
            Core.<*> ( x
                         Core..:? "RootCauseServiceRequestImpactStatistics"
                     )
            Core.<*> (x Core..:? "LastUpdateTime")
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

instance Core.Hashable InsightSummary

instance Core.NFData InsightSummary
