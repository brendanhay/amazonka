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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A list of administrative actions for the file system that are in process
    -- or waiting to be processed. Administrative actions describe changes to
    -- the Amazon FSx system.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    creationTime :: Prelude.Maybe Data.POSIX,
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
    lifecycleTransitionReason :: Prelude.Maybe LifecycleTransitionReason,
    -- | The name of the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the volume that the snapshot is of.
    volumeId :: Prelude.Maybe Prelude.Text
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
-- 'administrativeActions', 'snapshot_administrativeActions' - A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
--
-- 'creationTime', 'snapshot_creationTime' - Undocumented member.
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
-- 'lifecycleTransitionReason', 'snapshot_lifecycleTransitionReason' - Undocumented member.
--
-- 'name', 'snapshot_name' - The name of the snapshot.
--
-- 'resourceARN', 'snapshot_resourceARN' - Undocumented member.
--
-- 'snapshotId', 'snapshot_snapshotId' - The ID of the snapshot.
--
-- 'tags', 'snapshot_tags' - Undocumented member.
--
-- 'volumeId', 'snapshot_volumeId' - The ID of the volume that the snapshot is of.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { administrativeActions = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lifecycleTransitionReason = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
snapshot_administrativeActions :: Lens.Lens' Snapshot (Prelude.Maybe [AdministrativeAction])
snapshot_administrativeActions = Lens.lens (\Snapshot' {administrativeActions} -> administrativeActions) (\s@Snapshot' {} a -> s {administrativeActions = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
snapshot_creationTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_creationTime = Lens.lens (\Snapshot' {creationTime} -> creationTime) (\s@Snapshot' {} a -> s {creationTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

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

-- | Undocumented member.
snapshot_lifecycleTransitionReason :: Lens.Lens' Snapshot (Prelude.Maybe LifecycleTransitionReason)
snapshot_lifecycleTransitionReason = Lens.lens (\Snapshot' {lifecycleTransitionReason} -> lifecycleTransitionReason) (\s@Snapshot' {} a -> s {lifecycleTransitionReason = a} :: Snapshot)

-- | The name of the snapshot.
snapshot_name :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_name = Lens.lens (\Snapshot' {name} -> name) (\s@Snapshot' {} a -> s {name = a} :: Snapshot)

-- | Undocumented member.
snapshot_resourceARN :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_resourceARN = Lens.lens (\Snapshot' {resourceARN} -> resourceARN) (\s@Snapshot' {} a -> s {resourceARN = a} :: Snapshot)

-- | The ID of the snapshot.
snapshot_snapshotId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | Undocumented member.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe (Prelude.NonEmpty Tag))
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the volume that the snapshot is of.
snapshot_volumeId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_volumeId = Lens.lens (\Snapshot' {volumeId} -> volumeId) (\s@Snapshot' {} a -> s {volumeId = a} :: Snapshot)

instance Data.FromJSON Snapshot where
  parseJSON =
    Data.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Prelude.<$> ( x
                            Data..:? "AdministrativeActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "LifecycleTransitionReason")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "VolumeId")
      )

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt
      `Prelude.hashWithSalt` administrativeActions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` lifecycleTransitionReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf administrativeActions
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf lifecycleTransitionReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeId
