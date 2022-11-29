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
-- Module      : Amazonka.CodeGuruProfiler.Types.ProfilingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.ProfilingStatus where

import Amazonka.CodeGuruProfiler.Types.AggregatedProfileTime
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Profiling status includes information about the last time a profile
-- agent pinged back, the last time a profile was received, and the
-- aggregation period and start time for the most recent aggregated
-- profile.
--
-- /See:/ 'newProfilingStatus' smart constructor.
data ProfilingStatus = ProfilingStatus'
  { -- | The date and time when the profiling agent most recently pinged back.
    -- Specify using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
    -- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
    latestAgentOrchestratedAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the most recent profile was received. Specify
    -- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
    -- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
    latestAgentProfileReportedAt :: Prelude.Maybe Core.POSIX,
    -- | An
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AggregatedProfileTime.html AggregatedProfileTime>
    -- object that contains the aggregation period and start time for an
    -- aggregated profile.
    latestAggregatedProfile :: Prelude.Maybe AggregatedProfileTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfilingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestAgentOrchestratedAt', 'profilingStatus_latestAgentOrchestratedAt' - The date and time when the profiling agent most recently pinged back.
-- Specify using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- 'latestAgentProfileReportedAt', 'profilingStatus_latestAgentProfileReportedAt' - The date and time when the most recent profile was received. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- 'latestAggregatedProfile', 'profilingStatus_latestAggregatedProfile' - An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AggregatedProfileTime.html AggregatedProfileTime>
-- object that contains the aggregation period and start time for an
-- aggregated profile.
newProfilingStatus ::
  ProfilingStatus
newProfilingStatus =
  ProfilingStatus'
    { latestAgentOrchestratedAt =
        Prelude.Nothing,
      latestAgentProfileReportedAt = Prelude.Nothing,
      latestAggregatedProfile = Prelude.Nothing
    }

-- | The date and time when the profiling agent most recently pinged back.
-- Specify using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
profilingStatus_latestAgentOrchestratedAt :: Lens.Lens' ProfilingStatus (Prelude.Maybe Prelude.UTCTime)
profilingStatus_latestAgentOrchestratedAt = Lens.lens (\ProfilingStatus' {latestAgentOrchestratedAt} -> latestAgentOrchestratedAt) (\s@ProfilingStatus' {} a -> s {latestAgentOrchestratedAt = a} :: ProfilingStatus) Prelude.. Lens.mapping Core._Time

-- | The date and time when the most recent profile was received. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
profilingStatus_latestAgentProfileReportedAt :: Lens.Lens' ProfilingStatus (Prelude.Maybe Prelude.UTCTime)
profilingStatus_latestAgentProfileReportedAt = Lens.lens (\ProfilingStatus' {latestAgentProfileReportedAt} -> latestAgentProfileReportedAt) (\s@ProfilingStatus' {} a -> s {latestAgentProfileReportedAt = a} :: ProfilingStatus) Prelude.. Lens.mapping Core._Time

-- | An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AggregatedProfileTime.html AggregatedProfileTime>
-- object that contains the aggregation period and start time for an
-- aggregated profile.
profilingStatus_latestAggregatedProfile :: Lens.Lens' ProfilingStatus (Prelude.Maybe AggregatedProfileTime)
profilingStatus_latestAggregatedProfile = Lens.lens (\ProfilingStatus' {latestAggregatedProfile} -> latestAggregatedProfile) (\s@ProfilingStatus' {} a -> s {latestAggregatedProfile = a} :: ProfilingStatus)

instance Core.FromJSON ProfilingStatus where
  parseJSON =
    Core.withObject
      "ProfilingStatus"
      ( \x ->
          ProfilingStatus'
            Prelude.<$> (x Core..:? "latestAgentOrchestratedAt")
            Prelude.<*> (x Core..:? "latestAgentProfileReportedAt")
            Prelude.<*> (x Core..:? "latestAggregatedProfile")
      )

instance Prelude.Hashable ProfilingStatus where
  hashWithSalt _salt ProfilingStatus' {..} =
    _salt
      `Prelude.hashWithSalt` latestAgentOrchestratedAt
      `Prelude.hashWithSalt` latestAgentProfileReportedAt
      `Prelude.hashWithSalt` latestAggregatedProfile

instance Prelude.NFData ProfilingStatus where
  rnf ProfilingStatus' {..} =
    Prelude.rnf latestAgentOrchestratedAt
      `Prelude.seq` Prelude.rnf latestAgentProfileReportedAt
      `Prelude.seq` Prelude.rnf latestAggregatedProfile
