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
-- Module      : Amazonka.MGN.Types.WaveAggregatedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.WaveAggregatedStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.WaveHealthStatus
import Amazonka.MGN.Types.WaveProgressStatus
import qualified Amazonka.Prelude as Prelude

-- | Wave aggregated status.
--
-- /See:/ 'newWaveAggregatedStatus' smart constructor.
data WaveAggregatedStatus = WaveAggregatedStatus'
  { -- | Wave aggregated status health status.
    healthStatus :: Prelude.Maybe WaveHealthStatus,
    -- | Wave aggregated status last update dateTime.
    lastUpdateDateTime :: Prelude.Maybe Prelude.Text,
    -- | Wave aggregated status progress status.
    progressStatus :: Prelude.Maybe WaveProgressStatus,
    -- | DateTime marking when the first source server in the wave started
    -- replication.
    replicationStartedDateTime :: Prelude.Maybe Prelude.Text,
    -- | Wave aggregated status total applications amount.
    totalApplications :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaveAggregatedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthStatus', 'waveAggregatedStatus_healthStatus' - Wave aggregated status health status.
--
-- 'lastUpdateDateTime', 'waveAggregatedStatus_lastUpdateDateTime' - Wave aggregated status last update dateTime.
--
-- 'progressStatus', 'waveAggregatedStatus_progressStatus' - Wave aggregated status progress status.
--
-- 'replicationStartedDateTime', 'waveAggregatedStatus_replicationStartedDateTime' - DateTime marking when the first source server in the wave started
-- replication.
--
-- 'totalApplications', 'waveAggregatedStatus_totalApplications' - Wave aggregated status total applications amount.
newWaveAggregatedStatus ::
  WaveAggregatedStatus
newWaveAggregatedStatus =
  WaveAggregatedStatus'
    { healthStatus =
        Prelude.Nothing,
      lastUpdateDateTime = Prelude.Nothing,
      progressStatus = Prelude.Nothing,
      replicationStartedDateTime = Prelude.Nothing,
      totalApplications = Prelude.Nothing
    }

-- | Wave aggregated status health status.
waveAggregatedStatus_healthStatus :: Lens.Lens' WaveAggregatedStatus (Prelude.Maybe WaveHealthStatus)
waveAggregatedStatus_healthStatus = Lens.lens (\WaveAggregatedStatus' {healthStatus} -> healthStatus) (\s@WaveAggregatedStatus' {} a -> s {healthStatus = a} :: WaveAggregatedStatus)

-- | Wave aggregated status last update dateTime.
waveAggregatedStatus_lastUpdateDateTime :: Lens.Lens' WaveAggregatedStatus (Prelude.Maybe Prelude.Text)
waveAggregatedStatus_lastUpdateDateTime = Lens.lens (\WaveAggregatedStatus' {lastUpdateDateTime} -> lastUpdateDateTime) (\s@WaveAggregatedStatus' {} a -> s {lastUpdateDateTime = a} :: WaveAggregatedStatus)

-- | Wave aggregated status progress status.
waveAggregatedStatus_progressStatus :: Lens.Lens' WaveAggregatedStatus (Prelude.Maybe WaveProgressStatus)
waveAggregatedStatus_progressStatus = Lens.lens (\WaveAggregatedStatus' {progressStatus} -> progressStatus) (\s@WaveAggregatedStatus' {} a -> s {progressStatus = a} :: WaveAggregatedStatus)

-- | DateTime marking when the first source server in the wave started
-- replication.
waveAggregatedStatus_replicationStartedDateTime :: Lens.Lens' WaveAggregatedStatus (Prelude.Maybe Prelude.Text)
waveAggregatedStatus_replicationStartedDateTime = Lens.lens (\WaveAggregatedStatus' {replicationStartedDateTime} -> replicationStartedDateTime) (\s@WaveAggregatedStatus' {} a -> s {replicationStartedDateTime = a} :: WaveAggregatedStatus)

-- | Wave aggregated status total applications amount.
waveAggregatedStatus_totalApplications :: Lens.Lens' WaveAggregatedStatus (Prelude.Maybe Prelude.Natural)
waveAggregatedStatus_totalApplications = Lens.lens (\WaveAggregatedStatus' {totalApplications} -> totalApplications) (\s@WaveAggregatedStatus' {} a -> s {totalApplications = a} :: WaveAggregatedStatus)

instance Data.FromJSON WaveAggregatedStatus where
  parseJSON =
    Data.withObject
      "WaveAggregatedStatus"
      ( \x ->
          WaveAggregatedStatus'
            Prelude.<$> (x Data..:? "healthStatus")
            Prelude.<*> (x Data..:? "lastUpdateDateTime")
            Prelude.<*> (x Data..:? "progressStatus")
            Prelude.<*> (x Data..:? "replicationStartedDateTime")
            Prelude.<*> (x Data..:? "totalApplications")
      )

instance Prelude.Hashable WaveAggregatedStatus where
  hashWithSalt _salt WaveAggregatedStatus' {..} =
    _salt
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` lastUpdateDateTime
      `Prelude.hashWithSalt` progressStatus
      `Prelude.hashWithSalt` replicationStartedDateTime
      `Prelude.hashWithSalt` totalApplications

instance Prelude.NFData WaveAggregatedStatus where
  rnf WaveAggregatedStatus' {..} =
    Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf lastUpdateDateTime
      `Prelude.seq` Prelude.rnf progressStatus
      `Prelude.seq` Prelude.rnf replicationStartedDateTime
      `Prelude.seq` Prelude.rnf totalApplications
