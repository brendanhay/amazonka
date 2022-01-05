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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a group of anomalous metrics.
--
-- /See:/ 'newAnomalyGroupSummary' smart constructor.
data AnomalyGroupSummary = AnomalyGroupSummary'
  { -- | The start time for the group.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The severity score of the group.
    anomalyGroupScore :: Prelude.Maybe Prelude.Double,
    -- | The name of the primary affected measure for the group.
    primaryMetricName :: Prelude.Maybe Prelude.Text,
    -- | The end time for the group.
    endTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'anomalyGroupSummary_startTime' - The start time for the group.
--
-- 'anomalyGroupId', 'anomalyGroupSummary_anomalyGroupId' - The ID of the anomaly group.
--
-- 'anomalyGroupScore', 'anomalyGroupSummary_anomalyGroupScore' - The severity score of the group.
--
-- 'primaryMetricName', 'anomalyGroupSummary_primaryMetricName' - The name of the primary affected measure for the group.
--
-- 'endTime', 'anomalyGroupSummary_endTime' - The end time for the group.
newAnomalyGroupSummary ::
  AnomalyGroupSummary
newAnomalyGroupSummary =
  AnomalyGroupSummary'
    { startTime = Prelude.Nothing,
      anomalyGroupId = Prelude.Nothing,
      anomalyGroupScore = Prelude.Nothing,
      primaryMetricName = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The start time for the group.
anomalyGroupSummary_startTime :: Lens.Lens' AnomalyGroupSummary (Prelude.Maybe Prelude.Text)
anomalyGroupSummary_startTime = Lens.lens (\AnomalyGroupSummary' {startTime} -> startTime) (\s@AnomalyGroupSummary' {} a -> s {startTime = a} :: AnomalyGroupSummary)

-- | The ID of the anomaly group.
anomalyGroupSummary_anomalyGroupId :: Lens.Lens' AnomalyGroupSummary (Prelude.Maybe Prelude.Text)
anomalyGroupSummary_anomalyGroupId = Lens.lens (\AnomalyGroupSummary' {anomalyGroupId} -> anomalyGroupId) (\s@AnomalyGroupSummary' {} a -> s {anomalyGroupId = a} :: AnomalyGroupSummary)

-- | The severity score of the group.
anomalyGroupSummary_anomalyGroupScore :: Lens.Lens' AnomalyGroupSummary (Prelude.Maybe Prelude.Double)
anomalyGroupSummary_anomalyGroupScore = Lens.lens (\AnomalyGroupSummary' {anomalyGroupScore} -> anomalyGroupScore) (\s@AnomalyGroupSummary' {} a -> s {anomalyGroupScore = a} :: AnomalyGroupSummary)

-- | The name of the primary affected measure for the group.
anomalyGroupSummary_primaryMetricName :: Lens.Lens' AnomalyGroupSummary (Prelude.Maybe Prelude.Text)
anomalyGroupSummary_primaryMetricName = Lens.lens (\AnomalyGroupSummary' {primaryMetricName} -> primaryMetricName) (\s@AnomalyGroupSummary' {} a -> s {primaryMetricName = a} :: AnomalyGroupSummary)

-- | The end time for the group.
anomalyGroupSummary_endTime :: Lens.Lens' AnomalyGroupSummary (Prelude.Maybe Prelude.Text)
anomalyGroupSummary_endTime = Lens.lens (\AnomalyGroupSummary' {endTime} -> endTime) (\s@AnomalyGroupSummary' {} a -> s {endTime = a} :: AnomalyGroupSummary)

instance Core.FromJSON AnomalyGroupSummary where
  parseJSON =
    Core.withObject
      "AnomalyGroupSummary"
      ( \x ->
          AnomalyGroupSummary'
            Prelude.<$> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "AnomalyGroupId")
            Prelude.<*> (x Core..:? "AnomalyGroupScore")
            Prelude.<*> (x Core..:? "PrimaryMetricName")
            Prelude.<*> (x Core..:? "EndTime")
      )

instance Prelude.Hashable AnomalyGroupSummary where
  hashWithSalt _salt AnomalyGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` anomalyGroupId
      `Prelude.hashWithSalt` anomalyGroupScore
      `Prelude.hashWithSalt` primaryMetricName
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData AnomalyGroupSummary where
  rnf AnomalyGroupSummary' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf anomalyGroupId
      `Prelude.seq` Prelude.rnf anomalyGroupScore
      `Prelude.seq` Prelude.rnf primaryMetricName
      `Prelude.seq` Prelude.rnf endTime
