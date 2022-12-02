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
-- Module      : Amazonka.FSx.Types.Snapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import {-# SOURCE #-} Amazonka.FSx.Types.AdministrativeAction
import Amazonka.FSx.Types.LifecycleTransitionReason
import Amazonka.FSx.Types.SnapshotLifecycle
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | A snapshot of an Amazon FSx for OpenZFS volume.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle status of the snapshot.
    --
    -- -   @PENDING@ - Amazon FSx hasn\'t started creating the snapshot.
    --
    -- -   @CREATING@ - Amazon FSx is creating the snapshot.
    --
    -- -   @DELETING@ - Amazon FSx is deleting the snapshot.
    --
    -- -   @AVAILABLE@ - The snapshot is fully available.
    lifecycle :: Prelude.Maybe SnapshotLifecycle,
    -- | A list of administrative actions for the file system that are in process
    -- or waiting to be processed. Administrative actions describe changes to
    -- the Amazon FSx system.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the volume that the snapshot is of.
    volumeId :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    lifecycleTransitionReason :: Prelude.Maybe LifecycleTransitionReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'snapshot_tags' - Undocumented member.
--
-- 'name', 'snapshot_name' - The name of the snapshot.
--
-- 'lifecycle', 'snapshot_lifecycle' - The lifecycle status of the snapshot.
--
-- -   @PENDING@ - Amazon FSx hasn\'t started creating the snapshot.
--
-- -   @CREATING@ - Amazon FSx is creating the snapshot.
--
-- -   @DELETING@ - Amazon FSx is deleting the snapshot.
--
-- -   @AVAILABLE@ - The snapshot is fully available.
--
-- 'administrativeActions', 'snapshot_administrativeActions' - A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
--
-- 'snapshotId', 'snapshot_snapshotId' - The ID of the snapshot.
--
-- 'creationTime', 'snapshot_creationTime' - Undocumented member.
--
-- 'volumeId', 'snapshot_volumeId' - The ID of the volume that the snapshot is of.
--
-- 'resourceARN', 'snapshot_resourceARN' - Undocumented member.
--
-- 'lifecycleTransitionReason', 'snapshot_lifecycleTransitionReason' - Undocumented member.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      administrativeActions = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      lifecycleTransitionReason = Prelude.Nothing
    }

-- | Undocumented member.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe (Prelude.NonEmpty Tag))
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the snapshot.
snapshot_name :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_name = Lens.lens (\Snapshot' {name} -> name) (\s@Snapshot' {} a -> s {name = a} :: Snapshot)

-- | The lifecycle status of the snapshot.
--
-- -   @PENDING@ - Amazon FSx hasn\'t started creating the snapshot.
--
-- -   @CREATING@ - Amazon FSx is creating the snapshot.
--
-- -   @DELETING@ - Amazon FSx is deleting the snapshot.
--
-- -   @AVAILABLE@ - The snapshot is fully available.
snapshot_lifecycle :: Lens.Lens' Snapshot (Prelude.Maybe SnapshotLifecycle)
snapshot_lifecycle = Lens.lens (\Snapshot' {lifecycle} -> lifecycle) (\s@Snapshot' {} a -> s {lifecycle = a} :: Snapshot)

-- | A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
snapshot_administrativeActions :: Lens.Lens' Snapshot (Prelude.Maybe [AdministrativeAction])
snapshot_administrativeActions = Lens.lens (\Snapshot' {administrativeActions} -> administrativeActions) (\s@Snapshot' {} a -> s {administrativeActions = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the snapshot.
snapshot_snapshotId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | Undocumented member.
snapshot_creationTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_creationTime = Lens.lens (\Snapshot' {creationTime} -> creationTime) (\s@Snapshot' {} a -> s {creationTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

-- | The ID of the volume that the snapshot is of.
snapshot_volumeId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_volumeId = Lens.lens (\Snapshot' {volumeId} -> volumeId) (\s@Snapshot' {} a -> s {volumeId = a} :: Snapshot)

-- | Undocumented member.
snapshot_resourceARN :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_resourceARN = Lens.lens (\Snapshot' {resourceARN} -> resourceARN) (\s@Snapshot' {} a -> s {resourceARN = a} :: Snapshot)

-- | Undocumented member.
snapshot_lifecycleTransitionReason :: Lens.Lens' Snapshot (Prelude.Maybe LifecycleTransitionReason)
snapshot_lifecycleTransitionReason = Lens.lens (\Snapshot' {lifecycleTransitionReason} -> lifecycleTransitionReason) (\s@Snapshot' {} a -> s {lifecycleTransitionReason = a} :: Snapshot)

instance Data.FromJSON Snapshot where
  parseJSON =
    Data.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> ( x Data..:? "AdministrativeActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "LifecycleTransitionReason")
      )

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` administrativeActions
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` lifecycleTransitionReason

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf administrativeActions
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf lifecycleTransitionReason
