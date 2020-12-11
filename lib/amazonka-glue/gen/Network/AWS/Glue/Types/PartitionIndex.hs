-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndex
  ( PartitionIndex (..),

    -- * Smart constructor
    mkPartitionIndex,

    -- * Lenses
    piKeys,
    piIndexName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure for a partition index.
--
-- /See:/ 'mkPartitionIndex' smart constructor.
data PartitionIndex = PartitionIndex'
  { keys ::
      Lude.NonEmpty Lude.Text,
    indexName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartitionIndex' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the partition index.
-- * 'keys' - The keys for the partition index.
mkPartitionIndex ::
  -- | 'keys'
  Lude.NonEmpty Lude.Text ->
  -- | 'indexName'
  Lude.Text ->
  PartitionIndex
mkPartitionIndex pKeys_ pIndexName_ =
  PartitionIndex' {keys = pKeys_, indexName = pIndexName_}

-- | The keys for the partition index.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piKeys :: Lens.Lens' PartitionIndex (Lude.NonEmpty Lude.Text)
piKeys = Lens.lens (keys :: PartitionIndex -> Lude.NonEmpty Lude.Text) (\s a -> s {keys = a} :: PartitionIndex)
{-# DEPRECATED piKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The name of the partition index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piIndexName :: Lens.Lens' PartitionIndex Lude.Text
piIndexName = Lens.lens (indexName :: PartitionIndex -> Lude.Text) (\s a -> s {indexName = a} :: PartitionIndex)
{-# DEPRECATED piIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON PartitionIndex where
  toJSON PartitionIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Keys" Lude..= keys),
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )
