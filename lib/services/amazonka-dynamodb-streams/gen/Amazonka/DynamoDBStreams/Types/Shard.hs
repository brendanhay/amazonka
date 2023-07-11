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
-- Module      : Amazonka.DynamoDBStreams.Types.Shard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.Shard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Internal
import Amazonka.DynamoDBStreams.Types.SequenceNumberRange
import qualified Amazonka.Prelude as Prelude

-- | A uniquely identified group of stream records within a stream.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The shard ID of the current shard\'s parent.
    parentShardId :: Prelude.Maybe Prelude.Text,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: Prelude.Maybe SequenceNumberRange,
    -- | The system-generated identifier for this shard.
    shardId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Shard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentShardId', 'shard_parentShardId' - The shard ID of the current shard\'s parent.
--
-- 'sequenceNumberRange', 'shard_sequenceNumberRange' - The range of possible sequence numbers for the shard.
--
-- 'shardId', 'shard_shardId' - The system-generated identifier for this shard.
newShard ::
  Shard
newShard =
  Shard'
    { parentShardId = Prelude.Nothing,
      sequenceNumberRange = Prelude.Nothing,
      shardId = Prelude.Nothing
    }

-- | The shard ID of the current shard\'s parent.
shard_parentShardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_parentShardId = Lens.lens (\Shard' {parentShardId} -> parentShardId) (\s@Shard' {} a -> s {parentShardId = a} :: Shard)

-- | The range of possible sequence numbers for the shard.
shard_sequenceNumberRange :: Lens.Lens' Shard (Prelude.Maybe SequenceNumberRange)
shard_sequenceNumberRange = Lens.lens (\Shard' {sequenceNumberRange} -> sequenceNumberRange) (\s@Shard' {} a -> s {sequenceNumberRange = a} :: Shard)

-- | The system-generated identifier for this shard.
shard_shardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_shardId = Lens.lens (\Shard' {shardId} -> shardId) (\s@Shard' {} a -> s {shardId = a} :: Shard)

instance Data.FromJSON Shard where
  parseJSON =
    Data.withObject
      "Shard"
      ( \x ->
          Shard'
            Prelude.<$> (x Data..:? "ParentShardId")
            Prelude.<*> (x Data..:? "SequenceNumberRange")
            Prelude.<*> (x Data..:? "ShardId")
      )

instance Prelude.Hashable Shard where
  hashWithSalt _salt Shard' {..} =
    _salt
      `Prelude.hashWithSalt` parentShardId
      `Prelude.hashWithSalt` sequenceNumberRange
      `Prelude.hashWithSalt` shardId

instance Prelude.NFData Shard where
  rnf Shard' {..} =
    Prelude.rnf parentShardId
      `Prelude.seq` Prelude.rnf sequenceNumberRange
      `Prelude.seq` Prelude.rnf shardId
