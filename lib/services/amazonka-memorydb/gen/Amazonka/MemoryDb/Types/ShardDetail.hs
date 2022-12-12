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
-- Module      : Amazonka.MemoryDb.Types.ShardDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ShardDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.ShardConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides details of a shard in a snapshot
--
-- /See:/ 'newShardDetail' smart constructor.
data ShardDetail = ShardDetail'
  { -- | The configuration details of the shard
    configuration :: Prelude.Maybe ShardConfiguration,
    -- | The name of the shard
    name :: Prelude.Maybe Prelude.Text,
    -- | The size of the shard\'s snapshot
    size :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the shard\'s snapshot was created
    snapshotCreationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShardDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'shardDetail_configuration' - The configuration details of the shard
--
-- 'name', 'shardDetail_name' - The name of the shard
--
-- 'size', 'shardDetail_size' - The size of the shard\'s snapshot
--
-- 'snapshotCreationTime', 'shardDetail_snapshotCreationTime' - The date and time that the shard\'s snapshot was created
newShardDetail ::
  ShardDetail
newShardDetail =
  ShardDetail'
    { configuration = Prelude.Nothing,
      name = Prelude.Nothing,
      size = Prelude.Nothing,
      snapshotCreationTime = Prelude.Nothing
    }

-- | The configuration details of the shard
shardDetail_configuration :: Lens.Lens' ShardDetail (Prelude.Maybe ShardConfiguration)
shardDetail_configuration = Lens.lens (\ShardDetail' {configuration} -> configuration) (\s@ShardDetail' {} a -> s {configuration = a} :: ShardDetail)

-- | The name of the shard
shardDetail_name :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.Text)
shardDetail_name = Lens.lens (\ShardDetail' {name} -> name) (\s@ShardDetail' {} a -> s {name = a} :: ShardDetail)

-- | The size of the shard\'s snapshot
shardDetail_size :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.Text)
shardDetail_size = Lens.lens (\ShardDetail' {size} -> size) (\s@ShardDetail' {} a -> s {size = a} :: ShardDetail)

-- | The date and time that the shard\'s snapshot was created
shardDetail_snapshotCreationTime :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.UTCTime)
shardDetail_snapshotCreationTime = Lens.lens (\ShardDetail' {snapshotCreationTime} -> snapshotCreationTime) (\s@ShardDetail' {} a -> s {snapshotCreationTime = a} :: ShardDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ShardDetail where
  parseJSON =
    Data.withObject
      "ShardDetail"
      ( \x ->
          ShardDetail'
            Prelude.<$> (x Data..:? "Configuration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "SnapshotCreationTime")
      )

instance Prelude.Hashable ShardDetail where
  hashWithSalt _salt ShardDetail' {..} =
    _salt `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` snapshotCreationTime

instance Prelude.NFData ShardDetail where
  rnf ShardDetail' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf snapshotCreationTime
