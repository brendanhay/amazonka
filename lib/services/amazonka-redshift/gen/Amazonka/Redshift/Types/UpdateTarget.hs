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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UpdateTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.SupportedOperation

-- | A maintenance track that you can switch the current track to.
--
-- /See:/ 'newUpdateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { -- | The cluster version for the new maintenance track.
    databaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the new maintenance track.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | A list of operations supported by the maintenance track.
    supportedOperations :: Prelude.Maybe [SupportedOperation]
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
-- 'databaseVersion', 'updateTarget_databaseVersion' - The cluster version for the new maintenance track.
--
-- 'maintenanceTrackName', 'updateTarget_maintenanceTrackName' - The name of the new maintenance track.
--
-- 'supportedOperations', 'updateTarget_supportedOperations' - A list of operations supported by the maintenance track.
newUpdateTarget ::
  UpdateTarget
newUpdateTarget =
  UpdateTarget'
    { databaseVersion = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      supportedOperations = Prelude.Nothing
    }

-- | The cluster version for the new maintenance track.
updateTarget_databaseVersion :: Lens.Lens' UpdateTarget (Prelude.Maybe Prelude.Text)
updateTarget_databaseVersion = Lens.lens (\UpdateTarget' {databaseVersion} -> databaseVersion) (\s@UpdateTarget' {} a -> s {databaseVersion = a} :: UpdateTarget)

-- | The name of the new maintenance track.
updateTarget_maintenanceTrackName :: Lens.Lens' UpdateTarget (Prelude.Maybe Prelude.Text)
updateTarget_maintenanceTrackName = Lens.lens (\UpdateTarget' {maintenanceTrackName} -> maintenanceTrackName) (\s@UpdateTarget' {} a -> s {maintenanceTrackName = a} :: UpdateTarget)

-- | A list of operations supported by the maintenance track.
updateTarget_supportedOperations :: Lens.Lens' UpdateTarget (Prelude.Maybe [SupportedOperation])
updateTarget_supportedOperations = Lens.lens (\UpdateTarget' {supportedOperations} -> supportedOperations) (\s@UpdateTarget' {} a -> s {supportedOperations = a} :: UpdateTarget) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML UpdateTarget where
  parseXML x =
    UpdateTarget'
      Prelude.<$> (x Data..@? "DatabaseVersion")
      Prelude.<*> (x Data..@? "MaintenanceTrackName")
      Prelude.<*> ( x
                      Data..@? "SupportedOperations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "SupportedOperation")
                  )

instance Prelude.Hashable UpdateTarget where
  hashWithSalt _salt UpdateTarget' {..} =
    _salt
      `Prelude.hashWithSalt` databaseVersion
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` supportedOperations

instance Prelude.NFData UpdateTarget where
  rnf UpdateTarget' {..} =
    Prelude.rnf databaseVersion `Prelude.seq`
      Prelude.rnf maintenanceTrackName `Prelude.seq`
        Prelude.rnf supportedOperations
