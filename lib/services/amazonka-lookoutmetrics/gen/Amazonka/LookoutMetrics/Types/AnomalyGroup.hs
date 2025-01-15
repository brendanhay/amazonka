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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.MetricLevelImpact
import qualified Amazonka.Prelude as Prelude

-- | A group of anomalous metrics
--
-- /See:/ 'newAnomalyGroup' smart constructor.
data AnomalyGroup = AnomalyGroup'
  { -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The severity score of the group.
    anomalyGroupScore :: Prelude.Maybe Prelude.Double,
    -- | The end time for the group.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | A list of measures affected by the anomaly.
    metricLevelImpactList :: Prelude.Maybe [MetricLevelImpact],
    -- | The name of the primary affected measure for the group.
    primaryMetricName :: Prelude.Maybe Prelude.Text,
    -- | The start time for the group.
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyGroupId', 'anomalyGroup_anomalyGroupId' - The ID of the anomaly group.
--
-- 'anomalyGroupScore', 'anomalyGroup_anomalyGroupScore' - The severity score of the group.
--
-- 'endTime', 'anomalyGroup_endTime' - The end time for the group.
--
-- 'metricLevelImpactList', 'anomalyGroup_metricLevelImpactList' - A list of measures affected by the anomaly.
--
-- 'primaryMetricName', 'anomalyGroup_primaryMetricName' - The name of the primary affected measure for the group.
--
-- 'startTime', 'anomalyGroup_startTime' - The start time for the group.
newAnomalyGroup ::
  AnomalyGroup
newAnomalyGroup =
  AnomalyGroup'
    { anomalyGroupId = Prelude.Nothing,
      anomalyGroupScore = Prelude.Nothing,
      endTime = Prelude.Nothing,
      metricLevelImpactList = Prelude.Nothing,
      primaryMetricName = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The ID of the anomaly group.
anomalyGroup_anomalyGroupId :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_anomalyGroupId = Lens.lens (\AnomalyGroup' {anomalyGroupId} -> anomalyGroupId) (\s@AnomalyGroup' {} a -> s {anomalyGroupId = a} :: AnomalyGroup)

-- | The severity score of the group.
anomalyGroup_anomalyGroupScore :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Double)
anomalyGroup_anomalyGroupScore = Lens.lens (\AnomalyGroup' {anomalyGroupScore} -> anomalyGroupScore) (\s@AnomalyGroup' {} a -> s {anomalyGroupScore = a} :: AnomalyGroup)

-- | The end time for the group.
anomalyGroup_endTime :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_endTime = Lens.lens (\AnomalyGroup' {endTime} -> endTime) (\s@AnomalyGroup' {} a -> s {endTime = a} :: AnomalyGroup)

-- | A list of measures affected by the anomaly.
anomalyGroup_metricLevelImpactList :: Lens.Lens' AnomalyGroup (Prelude.Maybe [MetricLevelImpact])
anomalyGroup_metricLevelImpactList = Lens.lens (\AnomalyGroup' {metricLevelImpactList} -> metricLevelImpactList) (\s@AnomalyGroup' {} a -> s {metricLevelImpactList = a} :: AnomalyGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the primary affected measure for the group.
anomalyGroup_primaryMetricName :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_primaryMetricName = Lens.lens (\AnomalyGroup' {primaryMetricName} -> primaryMetricName) (\s@AnomalyGroup' {} a -> s {primaryMetricName = a} :: AnomalyGroup)

-- | The start time for the group.
anomalyGroup_startTime :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_startTime = Lens.lens (\AnomalyGroup' {startTime} -> startTime) (\s@AnomalyGroup' {} a -> s {startTime = a} :: AnomalyGroup)

instance Data.FromJSON AnomalyGroup where
  parseJSON =
    Data.withObject
      "AnomalyGroup"
      ( \x ->
          AnomalyGroup'
            Prelude.<$> (x Data..:? "AnomalyGroupId")
            Prelude.<*> (x Data..:? "AnomalyGroupScore")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> ( x
                            Data..:? "MetricLevelImpactList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PrimaryMetricName")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable AnomalyGroup where
  hashWithSalt _salt AnomalyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyGroupId
      `Prelude.hashWithSalt` anomalyGroupScore
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` metricLevelImpactList
      `Prelude.hashWithSalt` primaryMetricName
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData AnomalyGroup where
  rnf AnomalyGroup' {..} =
    Prelude.rnf anomalyGroupId `Prelude.seq`
      Prelude.rnf anomalyGroupScore `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf metricLevelImpactList `Prelude.seq`
            Prelude.rnf primaryMetricName `Prelude.seq`
              Prelude.rnf startTime
