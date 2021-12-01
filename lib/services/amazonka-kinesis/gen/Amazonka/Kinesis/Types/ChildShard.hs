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
-- Module      : Amazonka.Kinesis.Types.ChildShard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.ChildShard where

import qualified Amazonka.Core as Core
import Amazonka.Kinesis.Types.HashKeyRange
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newChildShard' smart constructor.
data ChildShard = ChildShard'
  { shardId :: Prelude.Text,
    parentShards :: [Prelude.Text],
    hashKeyRange :: HashKeyRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChildShard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardId', 'childShard_shardId' - Undocumented member.
--
-- 'parentShards', 'childShard_parentShards' - Undocumented member.
--
-- 'hashKeyRange', 'childShard_hashKeyRange' - Undocumented member.
newChildShard ::
  -- | 'shardId'
  Prelude.Text ->
  -- | 'hashKeyRange'
  HashKeyRange ->
  ChildShard
newChildShard pShardId_ pHashKeyRange_ =
  ChildShard'
    { shardId = pShardId_,
      parentShards = Prelude.mempty,
      hashKeyRange = pHashKeyRange_
    }

-- | Undocumented member.
childShard_shardId :: Lens.Lens' ChildShard Prelude.Text
childShard_shardId = Lens.lens (\ChildShard' {shardId} -> shardId) (\s@ChildShard' {} a -> s {shardId = a} :: ChildShard)

-- | Undocumented member.
childShard_parentShards :: Lens.Lens' ChildShard [Prelude.Text]
childShard_parentShards = Lens.lens (\ChildShard' {parentShards} -> parentShards) (\s@ChildShard' {} a -> s {parentShards = a} :: ChildShard) Prelude.. Lens.coerced

-- | Undocumented member.
childShard_hashKeyRange :: Lens.Lens' ChildShard HashKeyRange
childShard_hashKeyRange = Lens.lens (\ChildShard' {hashKeyRange} -> hashKeyRange) (\s@ChildShard' {} a -> s {hashKeyRange = a} :: ChildShard)

instance Core.FromJSON ChildShard where
  parseJSON =
    Core.withObject
      "ChildShard"
      ( \x ->
          ChildShard'
            Prelude.<$> (x Core..: "ShardId")
            Prelude.<*> (x Core..:? "ParentShards" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "HashKeyRange")
      )

instance Prelude.Hashable ChildShard where
  hashWithSalt salt' ChildShard' {..} =
    salt' `Prelude.hashWithSalt` hashKeyRange
      `Prelude.hashWithSalt` parentShards
      `Prelude.hashWithSalt` shardId

instance Prelude.NFData ChildShard where
  rnf ChildShard' {..} =
    Prelude.rnf shardId
      `Prelude.seq` Prelude.rnf hashKeyRange
      `Prelude.seq` Prelude.rnf parentShards
