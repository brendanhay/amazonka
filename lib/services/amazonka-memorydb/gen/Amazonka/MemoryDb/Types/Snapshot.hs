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
-- Module      : Amazonka.MemoryDb.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types.ClusterConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Represents a copy of an entire cluster as of the time when the snapshot
-- was taken.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The status of the snapshot. Valid values: creating | available |
    -- restoring | copying | deleting.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the cluster from which the snapshot was taken
    clusterConfiguration :: Prelude.Maybe ClusterConfiguration,
    -- | Indicates whether the snapshot is from an automatic backup (automated)
    -- or was created manually (manual).
    source :: Prelude.Maybe Prelude.Text
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
-- 'status', 'snapshot_status' - The status of the snapshot. Valid values: creating | available |
-- restoring | copying | deleting.
--
-- 'arn', 'snapshot_arn' - The ARN (Amazon Resource Name) of the snapshot.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'name', 'snapshot_name' - The name of the snapshot
--
-- 'clusterConfiguration', 'snapshot_clusterConfiguration' - The configuration of the cluster from which the snapshot was taken
--
-- 'source', 'snapshot_source' - Indicates whether the snapshot is from an automatic backup (automated)
-- or was created manually (manual).
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      clusterConfiguration = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The status of the snapshot. Valid values: creating | available |
-- restoring | copying | deleting.
snapshot_status :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The ARN (Amazon Resource Name) of the snapshot.
snapshot_arn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_arn = Lens.lens (\Snapshot' {arn} -> arn) (\s@Snapshot' {} a -> s {arn = a} :: Snapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The name of the snapshot
snapshot_name :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_name = Lens.lens (\Snapshot' {name} -> name) (\s@Snapshot' {} a -> s {name = a} :: Snapshot)

-- | The configuration of the cluster from which the snapshot was taken
snapshot_clusterConfiguration :: Lens.Lens' Snapshot (Prelude.Maybe ClusterConfiguration)
snapshot_clusterConfiguration = Lens.lens (\Snapshot' {clusterConfiguration} -> clusterConfiguration) (\s@Snapshot' {} a -> s {clusterConfiguration = a} :: Snapshot)

-- | Indicates whether the snapshot is from an automatic backup (automated)
-- or was created manually (manual).
snapshot_source :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_source = Lens.lens (\Snapshot' {source} -> source) (\s@Snapshot' {} a -> s {source = a} :: Snapshot)

instance Core.FromJSON Snapshot where
  parseJSON =
    Core.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ClusterConfiguration")
            Prelude.<*> (x Core..:? "Source")
      )

instance Prelude.Hashable Snapshot

instance Prelude.NFData Snapshot
