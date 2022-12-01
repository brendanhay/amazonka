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
import Amazonka.MemoryDb.Types.ShardConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides details of a shard in a snapshot
--
-- /See:/ 'newShardDetail' smart constructor.
data ShardDetail = ShardDetail'
  { -- | The name of the shard
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration details of the shard
    configuration :: Prelude.Maybe ShardConfiguration,
    -- | The date and time that the shard\'s snapshot was created
    snapshotCreationTime :: Prelude.Maybe Core.POSIX,
    -- | The size of the shard\'s snapshot
    size :: Prelude.Maybe Prelude.Text
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
-- 'name', 'shardDetail_name' - The name of the shard
--
-- 'configuration', 'shardDetail_configuration' - The configuration details of the shard
--
-- 'snapshotCreationTime', 'shardDetail_snapshotCreationTime' - The date and time that the shard\'s snapshot was created
--
-- 'size', 'shardDetail_size' - The size of the shard\'s snapshot
newShardDetail ::
  ShardDetail
newShardDetail =
  ShardDetail'
    { name = Prelude.Nothing,
      configuration = Prelude.Nothing,
      snapshotCreationTime = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The name of the shard
shardDetail_name :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.Text)
shardDetail_name = Lens.lens (\ShardDetail' {name} -> name) (\s@ShardDetail' {} a -> s {name = a} :: ShardDetail)

-- | The configuration details of the shard
shardDetail_configuration :: Lens.Lens' ShardDetail (Prelude.Maybe ShardConfiguration)
shardDetail_configuration = Lens.lens (\ShardDetail' {configuration} -> configuration) (\s@ShardDetail' {} a -> s {configuration = a} :: ShardDetail)

-- | The date and time that the shard\'s snapshot was created
shardDetail_snapshotCreationTime :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.UTCTime)
shardDetail_snapshotCreationTime = Lens.lens (\ShardDetail' {snapshotCreationTime} -> snapshotCreationTime) (\s@ShardDetail' {} a -> s {snapshotCreationTime = a} :: ShardDetail) Prelude.. Lens.mapping Core._Time

-- | The size of the shard\'s snapshot
shardDetail_size :: Lens.Lens' ShardDetail (Prelude.Maybe Prelude.Text)
shardDetail_size = Lens.lens (\ShardDetail' {size} -> size) (\s@ShardDetail' {} a -> s {size = a} :: ShardDetail)

instance Core.FromJSON ShardDetail where
  parseJSON =
    Core.withObject
      "ShardDetail"
      ( \x ->
          ShardDetail'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "SnapshotCreationTime")
            Prelude.<*> (x Core..:? "Size")
      )

instance Prelude.Hashable ShardDetail where
  hashWithSalt _salt ShardDetail' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` snapshotCreationTime
      `Prelude.hashWithSalt` size

instance Prelude.NFData ShardDetail where
  rnf ShardDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf snapshotCreationTime
      `Prelude.seq` Prelude.rnf size
