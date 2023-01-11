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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.ClusterConfiguration
import Amazonka.MemoryDb.Types.DataTieringStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a copy of an entire cluster as of the time when the snapshot
-- was taken.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The ARN (Amazon Resource Name) of the snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the cluster from which the snapshot was taken
    clusterConfiguration :: Prelude.Maybe ClusterConfiguration,
    -- | Enables data tiering. Data tiering is only supported for clusters using
    -- the r6gd node type. This parameter must be set when using r6gd nodes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
    dataTiering :: Prelude.Maybe DataTieringStatus,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is from an automatic backup (automated)
    -- or was created manually (manual).
    source :: Prelude.Maybe Prelude.Text,
    -- | The status of the snapshot. Valid values: creating | available |
    -- restoring | copying | deleting.
    status :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'snapshot_arn' - The ARN (Amazon Resource Name) of the snapshot.
--
-- 'clusterConfiguration', 'snapshot_clusterConfiguration' - The configuration of the cluster from which the snapshot was taken
--
-- 'dataTiering', 'snapshot_dataTiering' - Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'name', 'snapshot_name' - The name of the snapshot
--
-- 'source', 'snapshot_source' - Indicates whether the snapshot is from an automatic backup (automated)
-- or was created manually (manual).
--
-- 'status', 'snapshot_status' - The status of the snapshot. Valid values: creating | available |
-- restoring | copying | deleting.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { arn = Prelude.Nothing,
      clusterConfiguration = Prelude.Nothing,
      dataTiering = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the snapshot.
snapshot_arn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_arn = Lens.lens (\Snapshot' {arn} -> arn) (\s@Snapshot' {} a -> s {arn = a} :: Snapshot)

-- | The configuration of the cluster from which the snapshot was taken
snapshot_clusterConfiguration :: Lens.Lens' Snapshot (Prelude.Maybe ClusterConfiguration)
snapshot_clusterConfiguration = Lens.lens (\Snapshot' {clusterConfiguration} -> clusterConfiguration) (\s@Snapshot' {} a -> s {clusterConfiguration = a} :: Snapshot)

-- | Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
snapshot_dataTiering :: Lens.Lens' Snapshot (Prelude.Maybe DataTieringStatus)
snapshot_dataTiering = Lens.lens (\Snapshot' {dataTiering} -> dataTiering) (\s@Snapshot' {} a -> s {dataTiering = a} :: Snapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The name of the snapshot
snapshot_name :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_name = Lens.lens (\Snapshot' {name} -> name) (\s@Snapshot' {} a -> s {name = a} :: Snapshot)

-- | Indicates whether the snapshot is from an automatic backup (automated)
-- or was created manually (manual).
snapshot_source :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_source = Lens.lens (\Snapshot' {source} -> source) (\s@Snapshot' {} a -> s {source = a} :: Snapshot)

-- | The status of the snapshot. Valid values: creating | available |
-- restoring | copying | deleting.
snapshot_status :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

instance Data.FromJSON Snapshot where
  parseJSON =
    Data.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "ClusterConfiguration")
            Prelude.<*> (x Data..:? "DataTiering")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` clusterConfiguration
      `Prelude.hashWithSalt` dataTiering
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` status

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clusterConfiguration
      `Prelude.seq` Prelude.rnf dataTiering
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf status
