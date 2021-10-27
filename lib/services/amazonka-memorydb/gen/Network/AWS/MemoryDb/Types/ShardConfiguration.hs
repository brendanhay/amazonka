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
-- Module      : Network.AWS.MemoryDb.Types.ShardConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.ShardConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Shard configuration options. Each shard configuration has the following:
-- Slots and ReplicaCount.
--
-- /See:/ 'newShardConfiguration' smart constructor.
data ShardConfiguration = ShardConfiguration'
  { -- | A string that specifies the keyspace for a particular node group.
    -- Keyspaces range from 0 to 16,383. The string is in the format
    -- startkey-endkey.
    slots :: Prelude.Maybe Prelude.Text,
    -- | The number of read replica nodes in this shard.
    replicaCount :: Prelude.Maybe Prelude.Int
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
-- 'slots', 'shardConfiguration_slots' - A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- startkey-endkey.
--
-- 'replicaCount', 'shardConfiguration_replicaCount' - The number of read replica nodes in this shard.
newShardConfiguration ::
  ShardConfiguration
newShardConfiguration =
  ShardConfiguration'
    { slots = Prelude.Nothing,
      replicaCount = Prelude.Nothing
    }

-- | A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- startkey-endkey.
shardConfiguration_slots :: Lens.Lens' ShardConfiguration (Prelude.Maybe Prelude.Text)
shardConfiguration_slots = Lens.lens (\ShardConfiguration' {slots} -> slots) (\s@ShardConfiguration' {} a -> s {slots = a} :: ShardConfiguration)

-- | The number of read replica nodes in this shard.
shardConfiguration_replicaCount :: Lens.Lens' ShardConfiguration (Prelude.Maybe Prelude.Int)
shardConfiguration_replicaCount = Lens.lens (\ShardConfiguration' {replicaCount} -> replicaCount) (\s@ShardConfiguration' {} a -> s {replicaCount = a} :: ShardConfiguration)

instance Core.FromJSON ShardConfiguration where
  parseJSON =
    Core.withObject
      "ShardConfiguration"
      ( \x ->
          ShardConfiguration'
            Prelude.<$> (x Core..:? "Slots")
            Prelude.<*> (x Core..:? "ReplicaCount")
      )

instance Prelude.Hashable ShardConfiguration

instance Prelude.NFData ShardConfiguration
