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
-- Module      : Network.AWS.Kinesis.Types.ChildShard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ChildShard where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types.HashKeyRange
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newChildShard' smart constructor.
data ChildShard = ChildShard'
  { shardId :: Core.Text,
    parentShards :: [Core.Text],
    hashKeyRange :: HashKeyRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'hashKeyRange'
  HashKeyRange ->
  ChildShard
newChildShard pShardId_ pHashKeyRange_ =
  ChildShard'
    { shardId = pShardId_,
      parentShards = Core.mempty,
      hashKeyRange = pHashKeyRange_
    }

-- | Undocumented member.
childShard_shardId :: Lens.Lens' ChildShard Core.Text
childShard_shardId = Lens.lens (\ChildShard' {shardId} -> shardId) (\s@ChildShard' {} a -> s {shardId = a} :: ChildShard)

-- | Undocumented member.
childShard_parentShards :: Lens.Lens' ChildShard [Core.Text]
childShard_parentShards = Lens.lens (\ChildShard' {parentShards} -> parentShards) (\s@ChildShard' {} a -> s {parentShards = a} :: ChildShard) Core.. Lens._Coerce

-- | Undocumented member.
childShard_hashKeyRange :: Lens.Lens' ChildShard HashKeyRange
childShard_hashKeyRange = Lens.lens (\ChildShard' {hashKeyRange} -> hashKeyRange) (\s@ChildShard' {} a -> s {hashKeyRange = a} :: ChildShard)

instance Core.FromJSON ChildShard where
  parseJSON =
    Core.withObject
      "ChildShard"
      ( \x ->
          ChildShard'
            Core.<$> (x Core..: "ShardId")
            Core.<*> (x Core..:? "ParentShards" Core..!= Core.mempty)
            Core.<*> (x Core..: "HashKeyRange")
      )

instance Core.Hashable ChildShard

instance Core.NFData ChildShard
