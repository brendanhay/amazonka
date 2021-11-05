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
-- Module      : Network.AWS.CodeGuruProfiler.Types.FindingsReportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruProfiler.Types.FindingsReportSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    -- | The end time of the period during which the metric is flagged as
    -- anomalous. This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    profileEndTime :: Prelude.Maybe Core.POSIX,
    -- | The universally unique identifier (UUID) of the recommendation report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the profiling group that is associated with the analysis
    -- data.
    profilingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The total number of different recommendations that were found by the
    -- analysis.
    totalNumberOfFindings :: Prelude.Maybe Prelude.Int
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
-- 'profileEndTime', 'findingsReportSummary_profileEndTime' - The end time of the period during which the metric is flagged as
-- anomalous. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'id', 'findingsReportSummary_id' - The universally unique identifier (UUID) of the recommendation report.
--
-- 'profilingGroupName', 'findingsReportSummary_profilingGroupName' - The name of the profiling group that is associated with the analysis
-- data.
--
-- 'totalNumberOfFindings', 'findingsReportSummary_totalNumberOfFindings' - The total number of different recommendations that were found by the
-- analysis.
newFindingsReportSummary ::
  FindingsReportSummary
newFindingsReportSummary =
  FindingsReportSummary'
    { profileStartTime =
        Prelude.Nothing,
      profileEndTime = Prelude.Nothing,
      id = Prelude.Nothing,
      profilingGroupName = Prelude.Nothing,
      totalNumberOfFindings = Prelude.Nothing
    }

-- | The start time of the profile the analysis data is about. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
findingsReportSummary_profileStartTime :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.UTCTime)
findingsReportSummary_profileStartTime = Lens.lens (\FindingsReportSummary' {profileStartTime} -> profileStartTime) (\s@FindingsReportSummary' {} a -> s {profileStartTime = a} :: FindingsReportSummary) Prelude.. Lens.mapping Core._Time

-- | The end time of the period during which the metric is flagged as
-- anomalous. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
findingsReportSummary_profileEndTime :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.UTCTime)
findingsReportSummary_profileEndTime = Lens.lens (\FindingsReportSummary' {profileEndTime} -> profileEndTime) (\s@FindingsReportSummary' {} a -> s {profileEndTime = a} :: FindingsReportSummary) Prelude.. Lens.mapping Core._Time

-- | The universally unique identifier (UUID) of the recommendation report.
findingsReportSummary_id :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Text)
findingsReportSummary_id = Lens.lens (\FindingsReportSummary' {id} -> id) (\s@FindingsReportSummary' {} a -> s {id = a} :: FindingsReportSummary)

-- | The name of the profiling group that is associated with the analysis
-- data.
findingsReportSummary_profilingGroupName :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Text)
findingsReportSummary_profilingGroupName = Lens.lens (\FindingsReportSummary' {profilingGroupName} -> profilingGroupName) (\s@FindingsReportSummary' {} a -> s {profilingGroupName = a} :: FindingsReportSummary)

-- | The total number of different recommendations that were found by the
-- analysis.
findingsReportSummary_totalNumberOfFindings :: Lens.Lens' FindingsReportSummary (Prelude.Maybe Prelude.Int)
findingsReportSummary_totalNumberOfFindings = Lens.lens (\FindingsReportSummary' {totalNumberOfFindings} -> totalNumberOfFindings) (\s@FindingsReportSummary' {} a -> s {totalNumberOfFindings = a} :: FindingsReportSummary)

instance Core.FromJSON FindingsReportSummary where
  parseJSON =
    Core.withObject
      "FindingsReportSummary"
      ( \x ->
          FindingsReportSummary'
            Prelude.<$> (x Core..:? "profileStartTime")
            Prelude.<*> (x Core..:? "profileEndTime")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "profilingGroupName")
            Prelude.<*> (x Core..:? "totalNumberOfFindings")
      )

instance Prelude.Hashable FindingsReportSummary

instance Prelude.NFData FindingsReportSummary
