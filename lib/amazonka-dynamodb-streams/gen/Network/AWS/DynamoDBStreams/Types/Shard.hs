-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Shard
  ( Shard (..),

    -- * Smart constructor
    mkShard,

    -- * Lenses
    sParentShardId,
    sSequenceNumberRange,
    sShardId,
  )
where

import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A uniquely identified group of stream records within a stream.
--
-- /See:/ 'mkShard' smart constructor.
data Shard = Shard'
  { parentShardId :: Lude.Maybe Lude.Text,
    sequenceNumberRange :: Lude.Maybe SequenceNumberRange,
    shardId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- * 'parentShardId' - The shard ID of the current shard's parent.
-- * 'sequenceNumberRange' - The range of possible sequence numbers for the shard.
-- * 'shardId' - The system-generated identifier for this shard.
mkShard ::
  Shard
mkShard =
  Shard'
    { parentShardId = Lude.Nothing,
      sequenceNumberRange = Lude.Nothing,
      shardId = Lude.Nothing
    }

-- | The shard ID of the current shard's parent.
--
-- /Note:/ Consider using 'parentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParentShardId :: Lens.Lens' Shard (Lude.Maybe Lude.Text)
sParentShardId = Lens.lens (parentShardId :: Shard -> Lude.Maybe Lude.Text) (\s a -> s {parentShardId = a} :: Shard)
{-# DEPRECATED sParentShardId "Use generic-lens or generic-optics with 'parentShardId' instead." #-}

-- | The range of possible sequence numbers for the shard.
--
-- /Note:/ Consider using 'sequenceNumberRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSequenceNumberRange :: Lens.Lens' Shard (Lude.Maybe SequenceNumberRange)
sSequenceNumberRange = Lens.lens (sequenceNumberRange :: Shard -> Lude.Maybe SequenceNumberRange) (\s a -> s {sequenceNumberRange = a} :: Shard)
{-# DEPRECATED sSequenceNumberRange "Use generic-lens or generic-optics with 'sequenceNumberRange' instead." #-}

-- | The system-generated identifier for this shard.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShardId :: Lens.Lens' Shard (Lude.Maybe Lude.Text)
sShardId = Lens.lens (shardId :: Shard -> Lude.Maybe Lude.Text) (\s a -> s {shardId = a} :: Shard)
{-# DEPRECATED sShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.FromJSON Shard where
  parseJSON =
    Lude.withObject
      "Shard"
      ( \x ->
          Shard'
            Lude.<$> (x Lude..:? "ParentShardId")
            Lude.<*> (x Lude..:? "SequenceNumberRange")
            Lude.<*> (x Lude..:? "ShardId")
      )
