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
-- Module      : Network.AWS.DynamoDBStreams.Types.Shard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Shard where

import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A uniquely identified group of stream records within a stream.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The system-generated identifier for this shard.
    shardId :: Prelude.Maybe Prelude.Text,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: Prelude.Maybe SequenceNumberRange,
    -- | The shard ID of the current shard\'s parent.
    parentShardId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Shard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardId', 'shard_shardId' - The system-generated identifier for this shard.
--
-- 'sequenceNumberRange', 'shard_sequenceNumberRange' - The range of possible sequence numbers for the shard.
--
-- 'parentShardId', 'shard_parentShardId' - The shard ID of the current shard\'s parent.
newShard ::
  Shard
newShard =
  Shard'
    { shardId = Prelude.Nothing,
      sequenceNumberRange = Prelude.Nothing,
      parentShardId = Prelude.Nothing
    }

-- | The system-generated identifier for this shard.
shard_shardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_shardId = Lens.lens (\Shard' {shardId} -> shardId) (\s@Shard' {} a -> s {shardId = a} :: Shard)

-- | The range of possible sequence numbers for the shard.
shard_sequenceNumberRange :: Lens.Lens' Shard (Prelude.Maybe SequenceNumberRange)
shard_sequenceNumberRange = Lens.lens (\Shard' {sequenceNumberRange} -> sequenceNumberRange) (\s@Shard' {} a -> s {sequenceNumberRange = a} :: Shard)

-- | The shard ID of the current shard\'s parent.
shard_parentShardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_parentShardId = Lens.lens (\Shard' {parentShardId} -> parentShardId) (\s@Shard' {} a -> s {parentShardId = a} :: Shard)

instance Prelude.FromJSON Shard where
  parseJSON =
    Prelude.withObject
      "Shard"
      ( \x ->
          Shard'
            Prelude.<$> (x Prelude..:? "ShardId")
            Prelude.<*> (x Prelude..:? "SequenceNumberRange")
            Prelude.<*> (x Prelude..:? "ParentShardId")
      )

instance Prelude.Hashable Shard

instance Prelude.NFData Shard
