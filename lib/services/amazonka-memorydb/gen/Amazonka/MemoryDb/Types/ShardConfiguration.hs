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
-- Module      : Amazonka.MemoryDb.Types.ShardConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ShardConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Shard configuration options. Each shard configuration has the following:
-- Slots and ReplicaCount.
--
-- /See:/ 'newShardConfiguration' smart constructor.
data ShardConfiguration = ShardConfiguration'
  { -- | The number of read replica nodes in this shard.
    replicaCount :: Prelude.Maybe Prelude.Int,
    -- | A string that specifies the keyspace for a particular node group.
    -- Keyspaces range from 0 to 16,383. The string is in the format
    -- startkey-endkey.
    slots :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShardConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaCount', 'shardConfiguration_replicaCount' - The number of read replica nodes in this shard.
--
-- 'slots', 'shardConfiguration_slots' - A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- startkey-endkey.
newShardConfiguration ::
  ShardConfiguration
newShardConfiguration =
  ShardConfiguration'
    { replicaCount = Prelude.Nothing,
      slots = Prelude.Nothing
    }

-- | The number of read replica nodes in this shard.
shardConfiguration_replicaCount :: Lens.Lens' ShardConfiguration (Prelude.Maybe Prelude.Int)
shardConfiguration_replicaCount = Lens.lens (\ShardConfiguration' {replicaCount} -> replicaCount) (\s@ShardConfiguration' {} a -> s {replicaCount = a} :: ShardConfiguration)

-- | A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- startkey-endkey.
shardConfiguration_slots :: Lens.Lens' ShardConfiguration (Prelude.Maybe Prelude.Text)
shardConfiguration_slots = Lens.lens (\ShardConfiguration' {slots} -> slots) (\s@ShardConfiguration' {} a -> s {slots = a} :: ShardConfiguration)

instance Data.FromJSON ShardConfiguration where
  parseJSON =
    Data.withObject
      "ShardConfiguration"
      ( \x ->
          ShardConfiguration'
            Prelude.<$> (x Data..:? "ReplicaCount")
            Prelude.<*> (x Data..:? "Slots")
      )

instance Prelude.Hashable ShardConfiguration where
  hashWithSalt _salt ShardConfiguration' {..} =
    _salt `Prelude.hashWithSalt` replicaCount
      `Prelude.hashWithSalt` slots

instance Prelude.NFData ShardConfiguration where
  rnf ShardConfiguration' {..} =
    Prelude.rnf replicaCount
      `Prelude.seq` Prelude.rnf slots
