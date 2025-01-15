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
-- Module      : Amazonka.MGN.Types.ApplicationAggregatedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ApplicationAggregatedStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ApplicationHealthStatus
import Amazonka.MGN.Types.ApplicationProgressStatus
import qualified Amazonka.Prelude as Prelude

-- | Application aggregated status.
--
-- /See:/ 'newApplicationAggregatedStatus' smart constructor.
data ApplicationAggregatedStatus = ApplicationAggregatedStatus'
  { -- | Application aggregated status health status.
    healthStatus :: Prelude.Maybe ApplicationHealthStatus,
    -- | Application aggregated status last update dateTime.
    lastUpdateDateTime :: Prelude.Maybe Prelude.Text,
    -- | Application aggregated status progress status.
    progressStatus :: Prelude.Maybe ApplicationProgressStatus,
    -- | Application aggregated status total source servers amount.
    totalSourceServers :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationAggregatedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthStatus', 'applicationAggregatedStatus_healthStatus' - Application aggregated status health status.
--
-- 'lastUpdateDateTime', 'applicationAggregatedStatus_lastUpdateDateTime' - Application aggregated status last update dateTime.
--
-- 'progressStatus', 'applicationAggregatedStatus_progressStatus' - Application aggregated status progress status.
--
-- 'totalSourceServers', 'applicationAggregatedStatus_totalSourceServers' - Application aggregated status total source servers amount.
newApplicationAggregatedStatus ::
  ApplicationAggregatedStatus
newApplicationAggregatedStatus =
  ApplicationAggregatedStatus'
    { healthStatus =
        Prelude.Nothing,
      lastUpdateDateTime = Prelude.Nothing,
      progressStatus = Prelude.Nothing,
      totalSourceServers = Prelude.Nothing
    }

-- | Application aggregated status health status.
applicationAggregatedStatus_healthStatus :: Lens.Lens' ApplicationAggregatedStatus (Prelude.Maybe ApplicationHealthStatus)
applicationAggregatedStatus_healthStatus = Lens.lens (\ApplicationAggregatedStatus' {healthStatus} -> healthStatus) (\s@ApplicationAggregatedStatus' {} a -> s {healthStatus = a} :: ApplicationAggregatedStatus)

-- | Application aggregated status last update dateTime.
applicationAggregatedStatus_lastUpdateDateTime :: Lens.Lens' ApplicationAggregatedStatus (Prelude.Maybe Prelude.Text)
applicationAggregatedStatus_lastUpdateDateTime = Lens.lens (\ApplicationAggregatedStatus' {lastUpdateDateTime} -> lastUpdateDateTime) (\s@ApplicationAggregatedStatus' {} a -> s {lastUpdateDateTime = a} :: ApplicationAggregatedStatus)

-- | Application aggregated status progress status.
applicationAggregatedStatus_progressStatus :: Lens.Lens' ApplicationAggregatedStatus (Prelude.Maybe ApplicationProgressStatus)
applicationAggregatedStatus_progressStatus = Lens.lens (\ApplicationAggregatedStatus' {progressStatus} -> progressStatus) (\s@ApplicationAggregatedStatus' {} a -> s {progressStatus = a} :: ApplicationAggregatedStatus)

-- | Application aggregated status total source servers amount.
applicationAggregatedStatus_totalSourceServers :: Lens.Lens' ApplicationAggregatedStatus (Prelude.Maybe Prelude.Natural)
applicationAggregatedStatus_totalSourceServers = Lens.lens (\ApplicationAggregatedStatus' {totalSourceServers} -> totalSourceServers) (\s@ApplicationAggregatedStatus' {} a -> s {totalSourceServers = a} :: ApplicationAggregatedStatus)

instance Data.FromJSON ApplicationAggregatedStatus where
  parseJSON =
    Data.withObject
      "ApplicationAggregatedStatus"
      ( \x ->
          ApplicationAggregatedStatus'
            Prelude.<$> (x Data..:? "healthStatus")
            Prelude.<*> (x Data..:? "lastUpdateDateTime")
            Prelude.<*> (x Data..:? "progressStatus")
            Prelude.<*> (x Data..:? "totalSourceServers")
      )

instance Prelude.Hashable ApplicationAggregatedStatus where
  hashWithSalt _salt ApplicationAggregatedStatus' {..} =
    _salt
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` lastUpdateDateTime
      `Prelude.hashWithSalt` progressStatus
      `Prelude.hashWithSalt` totalSourceServers

instance Prelude.NFData ApplicationAggregatedStatus where
  rnf ApplicationAggregatedStatus' {..} =
    Prelude.rnf healthStatus `Prelude.seq`
      Prelude.rnf lastUpdateDateTime `Prelude.seq`
        Prelude.rnf progressStatus `Prelude.seq`
          Prelude.rnf totalSourceServers
