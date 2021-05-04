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
-- Module      : Network.AWS.XRay.Types.InsightSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The time, in Unix seconds, that the insight was last updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the group that the insight belongs to.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
    rootCauseServiceId :: Prelude.Maybe ServiceId,
    -- | The current state of the insight.
    state :: Prelude.Maybe InsightState,
    -- | A brief description of the insight.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    -- | Categories The categories that label and describe the type of insight.
    categories :: Prelude.Maybe [InsightCategory],
    -- | The insights unique identifier.
    insightId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      groupName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      rootCauseServiceId = Prelude.Nothing,
      state = Prelude.Nothing,
      summary = Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      categories = Prelude.Nothing,
      insightId = Prelude.Nothing,
      groupARN = Prelude.Nothing
    }

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightSummary_clientRequestImpactStatistics :: Lens.Lens' InsightSummary (Prelude.Maybe RequestImpactStatistics)
insightSummary_clientRequestImpactStatistics = Lens.lens (\InsightSummary' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightSummary' {} a -> s {clientRequestImpactStatistics = a} :: InsightSummary)

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightSummary_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightSummary (Prelude.Maybe RequestImpactStatistics)
insightSummary_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightSummary' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightSummary' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightSummary)

-- | The time, in Unix seconds, that the insight was last updated.
insightSummary_lastUpdateTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_lastUpdateTime = Lens.lens (\InsightSummary' {lastUpdateTime} -> lastUpdateTime) (\s@InsightSummary' {} a -> s {lastUpdateTime = a} :: InsightSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the group that the insight belongs to.
insightSummary_groupName :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_groupName = Lens.lens (\InsightSummary' {groupName} -> groupName) (\s@InsightSummary' {} a -> s {groupName = a} :: InsightSummary)

-- | The time, in Unix seconds, at which the insight began.
insightSummary_startTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_startTime = Lens.lens (\InsightSummary' {startTime} -> startTime) (\s@InsightSummary' {} a -> s {startTime = a} :: InsightSummary) Prelude.. Lens.mapping Prelude._Time

-- | The time, in Unix seconds, at which the insight ended.
insightSummary_endTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_endTime = Lens.lens (\InsightSummary' {endTime} -> endTime) (\s@InsightSummary' {} a -> s {endTime = a} :: InsightSummary) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
insightSummary_rootCauseServiceId :: Lens.Lens' InsightSummary (Prelude.Maybe ServiceId)
insightSummary_rootCauseServiceId = Lens.lens (\InsightSummary' {rootCauseServiceId} -> rootCauseServiceId) (\s@InsightSummary' {} a -> s {rootCauseServiceId = a} :: InsightSummary)

-- | The current state of the insight.
insightSummary_state :: Lens.Lens' InsightSummary (Prelude.Maybe InsightState)
insightSummary_state = Lens.lens (\InsightSummary' {state} -> state) (\s@InsightSummary' {} a -> s {state = a} :: InsightSummary)

-- | A brief description of the insight.
insightSummary_summary :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_summary = Lens.lens (\InsightSummary' {summary} -> summary) (\s@InsightSummary' {} a -> s {summary = a} :: InsightSummary)

-- | The service within the insight that is most impacted by the incident.
insightSummary_topAnomalousServices :: Lens.Lens' InsightSummary (Prelude.Maybe [AnomalousService])
insightSummary_topAnomalousServices = Lens.lens (\InsightSummary' {topAnomalousServices} -> topAnomalousServices) (\s@InsightSummary' {} a -> s {topAnomalousServices = a} :: InsightSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | Categories The categories that label and describe the type of insight.
insightSummary_categories :: Lens.Lens' InsightSummary (Prelude.Maybe [InsightCategory])
insightSummary_categories = Lens.lens (\InsightSummary' {categories} -> categories) (\s@InsightSummary' {} a -> s {categories = a} :: InsightSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The insights unique identifier.
insightSummary_insightId :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_insightId = Lens.lens (\InsightSummary' {insightId} -> insightId) (\s@InsightSummary' {} a -> s {insightId = a} :: InsightSummary)

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insightSummary_groupARN :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_groupARN = Lens.lens (\InsightSummary' {groupARN} -> groupARN) (\s@InsightSummary' {} a -> s {groupARN = a} :: InsightSummary)

instance Prelude.FromJSON InsightSummary where
  parseJSON =
    Prelude.withObject
      "InsightSummary"
      ( \x ->
          InsightSummary'
            Prelude.<$> (x Prelude..:? "ClientRequestImpactStatistics")
            Prelude.<*> ( x
                            Prelude..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> (x Prelude..:? "LastUpdateTime")
            Prelude.<*> (x Prelude..:? "GroupName")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "RootCauseServiceId")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Summary")
            Prelude.<*> ( x Prelude..:? "TopAnomalousServices"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "Categories"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "InsightId")
            Prelude.<*> (x Prelude..:? "GroupARN")
      )

instance Prelude.Hashable InsightSummary

instance Prelude.NFData InsightSummary
