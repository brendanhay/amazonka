-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ChildShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ChildShard
  ( ChildShard (..),

    -- * Smart constructor
    mkChildShard,

    -- * Lenses
    csShardId,
    csParentShards,
    csHashKeyRange,
  )
where

import Network.AWS.Kinesis.Types.HashKeyRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkChildShard' smart constructor.
data ChildShard = ChildShard'
  { shardId :: Lude.Text,
    parentShards :: [Lude.Text],
    hashKeyRange :: HashKeyRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChildShard' with the minimum fields required to make a request.
--
-- * 'hashKeyRange' - Undocumented field.
-- * 'parentShards' - Undocumented field.
-- * 'shardId' - Undocumented field.
mkChildShard ::
  -- | 'shardId'
  Lude.Text ->
  -- | 'hashKeyRange'
  HashKeyRange ->
  ChildShard
mkChildShard pShardId_ pHashKeyRange_ =
  ChildShard'
    { shardId = pShardId_,
      parentShards = Lude.mempty,
      hashKeyRange = pHashKeyRange_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csShardId :: Lens.Lens' ChildShard Lude.Text
csShardId = Lens.lens (shardId :: ChildShard -> Lude.Text) (\s a -> s {shardId = a} :: ChildShard)
{-# DEPRECATED csShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'parentShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csParentShards :: Lens.Lens' ChildShard [Lude.Text]
csParentShards = Lens.lens (parentShards :: ChildShard -> [Lude.Text]) (\s a -> s {parentShards = a} :: ChildShard)
{-# DEPRECATED csParentShards "Use generic-lens or generic-optics with 'parentShards' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hashKeyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHashKeyRange :: Lens.Lens' ChildShard HashKeyRange
csHashKeyRange = Lens.lens (hashKeyRange :: ChildShard -> HashKeyRange) (\s a -> s {hashKeyRange = a} :: ChildShard)
{-# DEPRECATED csHashKeyRange "Use generic-lens or generic-optics with 'hashKeyRange' instead." #-}

instance Lude.FromJSON ChildShard where
  parseJSON =
    Lude.withObject
      "ChildShard"
      ( \x ->
          ChildShard'
            Lude.<$> (x Lude..: "ShardId")
            Lude.<*> (x Lude..:? "ParentShards" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "HashKeyRange")
      )
