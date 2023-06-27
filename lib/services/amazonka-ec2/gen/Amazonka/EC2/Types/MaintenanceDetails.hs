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
-- Module      : Amazonka.EC2.Types.MaintenanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MaintenanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Details for Site-to-Site VPN tunnel endpoint maintenance events.
--
-- /See:/ 'newMaintenanceDetails' smart constructor.
data MaintenanceDetails = MaintenanceDetails'
  { -- | Timestamp of last applied maintenance.
    lastMaintenanceApplied :: Prelude.Maybe Data.ISO8601,
    -- | The timestamp after which Amazon Web Services will automatically apply
    -- maintenance.
    maintenanceAutoAppliedAfter :: Prelude.Maybe Data.ISO8601,
    -- | Verify existence of a pending maintenance.
    pendingMaintenance :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastMaintenanceApplied', 'maintenanceDetails_lastMaintenanceApplied' - Timestamp of last applied maintenance.
--
-- 'maintenanceAutoAppliedAfter', 'maintenanceDetails_maintenanceAutoAppliedAfter' - The timestamp after which Amazon Web Services will automatically apply
-- maintenance.
--
-- 'pendingMaintenance', 'maintenanceDetails_pendingMaintenance' - Verify existence of a pending maintenance.
newMaintenanceDetails ::
  MaintenanceDetails
newMaintenanceDetails =
  MaintenanceDetails'
    { lastMaintenanceApplied =
        Prelude.Nothing,
      maintenanceAutoAppliedAfter = Prelude.Nothing,
      pendingMaintenance = Prelude.Nothing
    }

-- | Timestamp of last applied maintenance.
maintenanceDetails_lastMaintenanceApplied :: Lens.Lens' MaintenanceDetails (Prelude.Maybe Prelude.UTCTime)
maintenanceDetails_lastMaintenanceApplied = Lens.lens (\MaintenanceDetails' {lastMaintenanceApplied} -> lastMaintenanceApplied) (\s@MaintenanceDetails' {} a -> s {lastMaintenanceApplied = a} :: MaintenanceDetails) Prelude.. Lens.mapping Data._Time

-- | The timestamp after which Amazon Web Services will automatically apply
-- maintenance.
maintenanceDetails_maintenanceAutoAppliedAfter :: Lens.Lens' MaintenanceDetails (Prelude.Maybe Prelude.UTCTime)
maintenanceDetails_maintenanceAutoAppliedAfter = Lens.lens (\MaintenanceDetails' {maintenanceAutoAppliedAfter} -> maintenanceAutoAppliedAfter) (\s@MaintenanceDetails' {} a -> s {maintenanceAutoAppliedAfter = a} :: MaintenanceDetails) Prelude.. Lens.mapping Data._Time

-- | Verify existence of a pending maintenance.
maintenanceDetails_pendingMaintenance :: Lens.Lens' MaintenanceDetails (Prelude.Maybe Prelude.Text)
maintenanceDetails_pendingMaintenance = Lens.lens (\MaintenanceDetails' {pendingMaintenance} -> pendingMaintenance) (\s@MaintenanceDetails' {} a -> s {pendingMaintenance = a} :: MaintenanceDetails)

instance Data.FromXML MaintenanceDetails where
  parseXML x =
    MaintenanceDetails'
      Prelude.<$> (x Data..@? "lastMaintenanceApplied")
      Prelude.<*> (x Data..@? "maintenanceAutoAppliedAfter")
      Prelude.<*> (x Data..@? "pendingMaintenance")

instance Prelude.Hashable MaintenanceDetails where
  hashWithSalt _salt MaintenanceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` lastMaintenanceApplied
      `Prelude.hashWithSalt` maintenanceAutoAppliedAfter
      `Prelude.hashWithSalt` pendingMaintenance

instance Prelude.NFData MaintenanceDetails where
  rnf MaintenanceDetails' {..} =
    Prelude.rnf lastMaintenanceApplied
      `Prelude.seq` Prelude.rnf maintenanceAutoAppliedAfter
      `Prelude.seq` Prelude.rnf pendingMaintenance
