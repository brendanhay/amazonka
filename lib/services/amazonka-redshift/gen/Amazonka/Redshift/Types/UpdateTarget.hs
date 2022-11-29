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
-- Module      : Amazonka.Redshift.Types.UpdateTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UpdateTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.SupportedOperation

-- | A maintenance track that you can switch the current track to.
--
-- /See:/ 'newUpdateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { -- | A list of operations supported by the maintenance track.
    supportedOperations :: Prelude.Maybe [SupportedOperation],
    -- | The name of the new maintenance track.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The cluster version for the new maintenance track.
    databaseVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supportedOperations', 'updateTarget_supportedOperations' - A list of operations supported by the maintenance track.
--
-- 'maintenanceTrackName', 'updateTarget_maintenanceTrackName' - The name of the new maintenance track.
--
-- 'databaseVersion', 'updateTarget_databaseVersion' - The cluster version for the new maintenance track.
newUpdateTarget ::
  UpdateTarget
newUpdateTarget =
  UpdateTarget'
    { supportedOperations =
        Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      databaseVersion = Prelude.Nothing
    }

-- | A list of operations supported by the maintenance track.
updateTarget_supportedOperations :: Lens.Lens' UpdateTarget (Prelude.Maybe [SupportedOperation])
updateTarget_supportedOperations = Lens.lens (\UpdateTarget' {supportedOperations} -> supportedOperations) (\s@UpdateTarget' {} a -> s {supportedOperations = a} :: UpdateTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new maintenance track.
updateTarget_maintenanceTrackName :: Lens.Lens' UpdateTarget (Prelude.Maybe Prelude.Text)
updateTarget_maintenanceTrackName = Lens.lens (\UpdateTarget' {maintenanceTrackName} -> maintenanceTrackName) (\s@UpdateTarget' {} a -> s {maintenanceTrackName = a} :: UpdateTarget)

-- | The cluster version for the new maintenance track.
updateTarget_databaseVersion :: Lens.Lens' UpdateTarget (Prelude.Maybe Prelude.Text)
updateTarget_databaseVersion = Lens.lens (\UpdateTarget' {databaseVersion} -> databaseVersion) (\s@UpdateTarget' {} a -> s {databaseVersion = a} :: UpdateTarget)

instance Core.FromXML UpdateTarget where
  parseXML x =
    UpdateTarget'
      Prelude.<$> ( x Core..@? "SupportedOperations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "SupportedOperation")
                  )
      Prelude.<*> (x Core..@? "MaintenanceTrackName")
      Prelude.<*> (x Core..@? "DatabaseVersion")

instance Prelude.Hashable UpdateTarget where
  hashWithSalt _salt UpdateTarget' {..} =
    _salt `Prelude.hashWithSalt` supportedOperations
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` databaseVersion

instance Prelude.NFData UpdateTarget where
  rnf UpdateTarget' {..} =
    Prelude.rnf supportedOperations
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf databaseVersion
