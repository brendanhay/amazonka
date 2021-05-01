{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.MaintenanceTrack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.MaintenanceTrack where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.UpdateTarget

-- | Defines a maintenance track that determines which Amazon Redshift
-- version to apply during a maintenance window. If the value for
-- @MaintenanceTrack@ is @current@, the cluster is updated to the most
-- recently certified maintenance release. If the value is @trailing@, the
-- cluster is updated to the previously certified maintenance release.
--
-- /See:/ 'newMaintenanceTrack' smart constructor.
data MaintenanceTrack = MaintenanceTrack'
  { -- | An array of UpdateTarget objects to update with the maintenance track.
    updateTargets :: Prelude.Maybe [UpdateTarget],
    -- | The version number for the cluster release.
    databaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance track. Possible values are @current@ and
    -- @trailing@.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceTrack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateTargets', 'maintenanceTrack_updateTargets' - An array of UpdateTarget objects to update with the maintenance track.
--
-- 'databaseVersion', 'maintenanceTrack_databaseVersion' - The version number for the cluster release.
--
-- 'maintenanceTrackName', 'maintenanceTrack_maintenanceTrackName' - The name of the maintenance track. Possible values are @current@ and
-- @trailing@.
newMaintenanceTrack ::
  MaintenanceTrack
newMaintenanceTrack =
  MaintenanceTrack'
    { updateTargets = Prelude.Nothing,
      databaseVersion = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing
    }

-- | An array of UpdateTarget objects to update with the maintenance track.
maintenanceTrack_updateTargets :: Lens.Lens' MaintenanceTrack (Prelude.Maybe [UpdateTarget])
maintenanceTrack_updateTargets = Lens.lens (\MaintenanceTrack' {updateTargets} -> updateTargets) (\s@MaintenanceTrack' {} a -> s {updateTargets = a} :: MaintenanceTrack) Prelude.. Lens.mapping Prelude._Coerce

-- | The version number for the cluster release.
maintenanceTrack_databaseVersion :: Lens.Lens' MaintenanceTrack (Prelude.Maybe Prelude.Text)
maintenanceTrack_databaseVersion = Lens.lens (\MaintenanceTrack' {databaseVersion} -> databaseVersion) (\s@MaintenanceTrack' {} a -> s {databaseVersion = a} :: MaintenanceTrack)

-- | The name of the maintenance track. Possible values are @current@ and
-- @trailing@.
maintenanceTrack_maintenanceTrackName :: Lens.Lens' MaintenanceTrack (Prelude.Maybe Prelude.Text)
maintenanceTrack_maintenanceTrackName = Lens.lens (\MaintenanceTrack' {maintenanceTrackName} -> maintenanceTrackName) (\s@MaintenanceTrack' {} a -> s {maintenanceTrackName = a} :: MaintenanceTrack)

instance Prelude.FromXML MaintenanceTrack where
  parseXML x =
    MaintenanceTrack'
      Prelude.<$> ( x Prelude..@? "UpdateTargets"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "UpdateTarget")
                  )
      Prelude.<*> (x Prelude..@? "DatabaseVersion")
      Prelude.<*> (x Prelude..@? "MaintenanceTrackName")

instance Prelude.Hashable MaintenanceTrack

instance Prelude.NFData MaintenanceTrack
