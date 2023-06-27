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
-- Module      : Amazonka.WellArchitected.Types.ConsolidatedReportMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ConsolidatedReportMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensMetric
import Amazonka.WellArchitected.Types.MetricType
import Amazonka.WellArchitected.Types.Risk

-- | A metric that contributes to the consolidated report.
--
-- /See:/ 'newConsolidatedReportMetric' smart constructor.
data ConsolidatedReportMetric = ConsolidatedReportMetric'
  { -- | The metrics for the lenses in the workload.
    lenses :: Prelude.Maybe [LensMetric],
    -- | The total number of lenses applied to the workload.
    lensesAppliedCount :: Prelude.Maybe Prelude.Natural,
    -- | The metric type of a metric in the consolidated report. Currently only
    -- WORKLOAD metric types are supported.
    metricType :: Prelude.Maybe MetricType,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    updatedAt :: Prelude.Maybe Data.POSIX,
    workloadArn :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConsolidatedReportMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lenses', 'consolidatedReportMetric_lenses' - The metrics for the lenses in the workload.
--
-- 'lensesAppliedCount', 'consolidatedReportMetric_lensesAppliedCount' - The total number of lenses applied to the workload.
--
-- 'metricType', 'consolidatedReportMetric_metricType' - The metric type of a metric in the consolidated report. Currently only
-- WORKLOAD metric types are supported.
--
-- 'riskCounts', 'consolidatedReportMetric_riskCounts' - Undocumented member.
--
-- 'updatedAt', 'consolidatedReportMetric_updatedAt' - Undocumented member.
--
-- 'workloadArn', 'consolidatedReportMetric_workloadArn' - Undocumented member.
--
-- 'workloadId', 'consolidatedReportMetric_workloadId' - Undocumented member.
--
-- 'workloadName', 'consolidatedReportMetric_workloadName' - Undocumented member.
newConsolidatedReportMetric ::
  ConsolidatedReportMetric
newConsolidatedReportMetric =
  ConsolidatedReportMetric'
    { lenses = Prelude.Nothing,
      lensesAppliedCount = Prelude.Nothing,
      metricType = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      workloadArn = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing
    }

-- | The metrics for the lenses in the workload.
consolidatedReportMetric_lenses :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe [LensMetric])
consolidatedReportMetric_lenses = Lens.lens (\ConsolidatedReportMetric' {lenses} -> lenses) (\s@ConsolidatedReportMetric' {} a -> s {lenses = a} :: ConsolidatedReportMetric) Prelude.. Lens.mapping Lens.coerced

-- | The total number of lenses applied to the workload.
consolidatedReportMetric_lensesAppliedCount :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe Prelude.Natural)
consolidatedReportMetric_lensesAppliedCount = Lens.lens (\ConsolidatedReportMetric' {lensesAppliedCount} -> lensesAppliedCount) (\s@ConsolidatedReportMetric' {} a -> s {lensesAppliedCount = a} :: ConsolidatedReportMetric)

-- | The metric type of a metric in the consolidated report. Currently only
-- WORKLOAD metric types are supported.
consolidatedReportMetric_metricType :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe MetricType)
consolidatedReportMetric_metricType = Lens.lens (\ConsolidatedReportMetric' {metricType} -> metricType) (\s@ConsolidatedReportMetric' {} a -> s {metricType = a} :: ConsolidatedReportMetric)

-- | Undocumented member.
consolidatedReportMetric_riskCounts :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
consolidatedReportMetric_riskCounts = Lens.lens (\ConsolidatedReportMetric' {riskCounts} -> riskCounts) (\s@ConsolidatedReportMetric' {} a -> s {riskCounts = a} :: ConsolidatedReportMetric) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
consolidatedReportMetric_updatedAt :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe Prelude.UTCTime)
consolidatedReportMetric_updatedAt = Lens.lens (\ConsolidatedReportMetric' {updatedAt} -> updatedAt) (\s@ConsolidatedReportMetric' {} a -> s {updatedAt = a} :: ConsolidatedReportMetric) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
consolidatedReportMetric_workloadArn :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe Prelude.Text)
consolidatedReportMetric_workloadArn = Lens.lens (\ConsolidatedReportMetric' {workloadArn} -> workloadArn) (\s@ConsolidatedReportMetric' {} a -> s {workloadArn = a} :: ConsolidatedReportMetric)

-- | Undocumented member.
consolidatedReportMetric_workloadId :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe Prelude.Text)
consolidatedReportMetric_workloadId = Lens.lens (\ConsolidatedReportMetric' {workloadId} -> workloadId) (\s@ConsolidatedReportMetric' {} a -> s {workloadId = a} :: ConsolidatedReportMetric)

-- | Undocumented member.
consolidatedReportMetric_workloadName :: Lens.Lens' ConsolidatedReportMetric (Prelude.Maybe Prelude.Text)
consolidatedReportMetric_workloadName = Lens.lens (\ConsolidatedReportMetric' {workloadName} -> workloadName) (\s@ConsolidatedReportMetric' {} a -> s {workloadName = a} :: ConsolidatedReportMetric)

instance Data.FromJSON ConsolidatedReportMetric where
  parseJSON =
    Data.withObject
      "ConsolidatedReportMetric"
      ( \x ->
          ConsolidatedReportMetric'
            Prelude.<$> (x Data..:? "Lenses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LensesAppliedCount")
            Prelude.<*> (x Data..:? "MetricType")
            Prelude.<*> (x Data..:? "RiskCounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "WorkloadArn")
            Prelude.<*> (x Data..:? "WorkloadId")
            Prelude.<*> (x Data..:? "WorkloadName")
      )

instance Prelude.Hashable ConsolidatedReportMetric where
  hashWithSalt _salt ConsolidatedReportMetric' {..} =
    _salt
      `Prelude.hashWithSalt` lenses
      `Prelude.hashWithSalt` lensesAppliedCount
      `Prelude.hashWithSalt` metricType
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` workloadArn
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` workloadName

instance Prelude.NFData ConsolidatedReportMetric where
  rnf ConsolidatedReportMetric' {..} =
    Prelude.rnf lenses
      `Prelude.seq` Prelude.rnf lensesAppliedCount
      `Prelude.seq` Prelude.rnf metricType
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf workloadArn
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadName
