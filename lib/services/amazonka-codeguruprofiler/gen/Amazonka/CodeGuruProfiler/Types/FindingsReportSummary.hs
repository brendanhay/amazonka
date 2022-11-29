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
-- Module      : Amazonka.CodeGuruProfiler.Types.FindingsReportSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.FindingsReportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about potential recommendations that might be created from
-- the analysis of profiling data.
--
-- /See:/ 'newFindingsReportSummary' smart constructor.
data FindingsReportSummary = FindingsReportSummary'
  { -- | The start time of the profile the analysis data is about. This is
    -- specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    profileStartTime :: Prelude.Maybe Core.POSIX,
    -- | The total number of different recommendations that were found by the
    -- analysis.
    totalNumberOfFindings :: Prelude.Maybe Prelude.Int,
    -- | The universally unique identifier (UUID) of the recommendation report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the profiling group that is associated with the analysis
    -- data.
    profilingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The end time of the period during which the metric is flagged as
    -- anomalous. This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    profileEndTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingsReportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileStartTime', 'findingsReportSummary_profileStartTime' - The start time of the profile the analysis data is about. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'totalNumberOfFindings', 'findingsReportSummary_totalNumberOfFindings' - The total number of different recommendations that were found by the
-- analysis.
--
-- 'id', 'findingsReportSummary_id' - The universally unique identifier (UUID) of the recommendation report.
--
-- 'profilingGroupName', 'findingsReportSummary_profilingGroupName' - The name of the profiling group that is associated with the analysis
-- data.
--
-- 'profileEndTime', 'findingsReportSummary_profileEndTime' - The end time of the period during which the metric is flagged as
-- anomalous. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
newFindingsReportSummary ::
  FindingsReportSummary
newFindingsReportSummary =
  FindingsReportSummary'
    { profileStartTime =
        Prelude.Nothing,
      totalNumberOfFindings = Prelude.Nothing,
      id = Prelude.Nothing,
      profilingGroupName = Prelude.Nothing,
      profileEndTime = Prelude.Nothing
    }

-- | The start time of the profile the analysis data is about. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
findingsReportSummary_profileStartTime :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.UTCTime)
findingsReportSummary_profileStartTime = Lens.lens (\FindingsReportSummary' {profileStartTime} -> profileStartTime) (\s@FindingsReportSummary' {} a -> s {profileStartTime = a} :: FindingsReportSummary) Prelude.. Lens.mapping Core._Time

-- | The total number of different recommendations that were found by the
-- analysis.
findingsReportSummary_totalNumberOfFindings :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Int)
findingsReportSummary_totalNumberOfFindings = Lens.lens (\FindingsReportSummary' {totalNumberOfFindings} -> totalNumberOfFindings) (\s@FindingsReportSummary' {} a -> s {totalNumberOfFindings = a} :: FindingsReportSummary)

-- | The universally unique identifier (UUID) of the recommendation report.
findingsReportSummary_id :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Text)
findingsReportSummary_id = Lens.lens (\FindingsReportSummary' {id} -> id) (\s@FindingsReportSummary' {} a -> s {id = a} :: FindingsReportSummary)

-- | The name of the profiling group that is associated with the analysis
-- data.
findingsReportSummary_profilingGroupName :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Text)
findingsReportSummary_profilingGroupName = Lens.lens (\FindingsReportSummary' {profilingGroupName} -> profilingGroupName) (\s@FindingsReportSummary' {} a -> s {profilingGroupName = a} :: FindingsReportSummary)

-- | The end time of the period during which the metric is flagged as
-- anomalous. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
findingsReportSummary_profileEndTime :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.UTCTime)
findingsReportSummary_profileEndTime = Lens.lens (\FindingsReportSummary' {profileEndTime} -> profileEndTime) (\s@FindingsReportSummary' {} a -> s {profileEndTime = a} :: FindingsReportSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FindingsReportSummary where
  parseJSON =
    Core.withObject
      "FindingsReportSummary"
      ( \x ->
          FindingsReportSummary'
            Prelude.<$> (x Core..:? "profileStartTime")
            Prelude.<*> (x Core..:? "totalNumberOfFindings")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "profilingGroupName")
            Prelude.<*> (x Core..:? "profileEndTime")
      )

instance Prelude.Hashable FindingsReportSummary where
  hashWithSalt _salt FindingsReportSummary' {..} =
    _salt `Prelude.hashWithSalt` profileStartTime
      `Prelude.hashWithSalt` totalNumberOfFindings
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` profilingGroupName
      `Prelude.hashWithSalt` profileEndTime

instance Prelude.NFData FindingsReportSummary where
  rnf FindingsReportSummary' {..} =
    Prelude.rnf profileStartTime
      `Prelude.seq` Prelude.rnf totalNumberOfFindings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf profilingGroupName
      `Prelude.seq` Prelude.rnf profileEndTime
