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
-- Module      : Network.AWS.Redshift.Types.UpdateTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UpdateTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedOperation

-- | A maintenance track that you can switch the current track to.
--
-- /See:/ 'newUpdateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { -- | A list of operations supported by the maintenance track.
    supportedOperations :: Core.Maybe [SupportedOperation],
    -- | The cluster version for the new maintenance track.
    databaseVersion :: Core.Maybe Core.Text,
    -- | The name of the new maintenance track.
    maintenanceTrackName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'databaseVersion', 'updateTarget_databaseVersion' - The cluster version for the new maintenance track.
--
-- 'maintenanceTrackName', 'updateTarget_maintenanceTrackName' - The name of the new maintenance track.
newUpdateTarget ::
  UpdateTarget
newUpdateTarget =
  UpdateTarget'
    { supportedOperations = Core.Nothing,
      databaseVersion = Core.Nothing,
      maintenanceTrackName = Core.Nothing
    }

-- | A list of operations supported by the maintenance track.
updateTarget_supportedOperations :: Lens.Lens' UpdateTarget (Core.Maybe [SupportedOperation])
updateTarget_supportedOperations = Lens.lens (\UpdateTarget' {supportedOperations} -> supportedOperations) (\s@UpdateTarget' {} a -> s {supportedOperations = a} :: UpdateTarget) Core.. Lens.mapping Lens._Coerce

-- | The cluster version for the new maintenance track.
updateTarget_databaseVersion :: Lens.Lens' UpdateTarget (Core.Maybe Core.Text)
updateTarget_databaseVersion = Lens.lens (\UpdateTarget' {databaseVersion} -> databaseVersion) (\s@UpdateTarget' {} a -> s {databaseVersion = a} :: UpdateTarget)

-- | The name of the new maintenance track.
updateTarget_maintenanceTrackName :: Lens.Lens' UpdateTarget (Core.Maybe Core.Text)
updateTarget_maintenanceTrackName = Lens.lens (\UpdateTarget' {maintenanceTrackName} -> maintenanceTrackName) (\s@UpdateTarget' {} a -> s {maintenanceTrackName = a} :: UpdateTarget)

instance Core.FromXML UpdateTarget where
  parseXML x =
    UpdateTarget'
      Core.<$> ( x Core..@? "SupportedOperations"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "SupportedOperation")
               )
      Core.<*> (x Core..@? "DatabaseVersion")
      Core.<*> (x Core..@? "MaintenanceTrackName")

instance Core.Hashable UpdateTarget

instance Core.NFData UpdateTarget
