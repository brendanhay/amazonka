{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
  ( BatchUpdatePartitionRequestEntry (..),

    -- * Smart constructor
    mkBatchUpdatePartitionRequestEntry,

    -- * Lenses
    buprePartitionInput,
    buprePartitionValueList,
  )
where

import Network.AWS.Glue.Types.PartitionInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains the values and structure used to update a partition.
--
-- /See:/ 'mkBatchUpdatePartitionRequestEntry' smart constructor.
data BatchUpdatePartitionRequestEntry = BatchUpdatePartitionRequestEntry'
  { -- | The structure used to update a partition.
    partitionInput :: PartitionInput,
    -- | A list of values defining the partitions.
    partitionValueList :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdatePartitionRequestEntry' with the minimum fields required to make a request.
--
-- * 'partitionInput' - The structure used to update a partition.
-- * 'partitionValueList' - A list of values defining the partitions.
mkBatchUpdatePartitionRequestEntry ::
  -- | 'partitionInput'
  PartitionInput ->
  BatchUpdatePartitionRequestEntry
mkBatchUpdatePartitionRequestEntry pPartitionInput_ =
  BatchUpdatePartitionRequestEntry'
    { partitionInput =
        pPartitionInput_,
      partitionValueList = Lude.mempty
    }

-- | The structure used to update a partition.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprePartitionInput :: Lens.Lens' BatchUpdatePartitionRequestEntry PartitionInput
buprePartitionInput = Lens.lens (partitionInput :: BatchUpdatePartitionRequestEntry -> PartitionInput) (\s a -> s {partitionInput = a} :: BatchUpdatePartitionRequestEntry)
{-# DEPRECATED buprePartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

-- | A list of values defining the partitions.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprePartitionValueList :: Lens.Lens' BatchUpdatePartitionRequestEntry [Lude.Text]
buprePartitionValueList = Lens.lens (partitionValueList :: BatchUpdatePartitionRequestEntry -> [Lude.Text]) (\s a -> s {partitionValueList = a} :: BatchUpdatePartitionRequestEntry)
{-# DEPRECATED buprePartitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead." #-}

instance Lude.ToJSON BatchUpdatePartitionRequestEntry where
  toJSON BatchUpdatePartitionRequestEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PartitionInput" Lude..= partitionInput),
            Lude.Just ("PartitionValueList" Lude..= partitionValueList)
          ]
      )
