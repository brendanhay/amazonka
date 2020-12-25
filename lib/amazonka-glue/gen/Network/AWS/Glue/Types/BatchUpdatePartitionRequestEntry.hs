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
    buprePartitionValueList,
    buprePartitionInput,
  )
where

import qualified Network.AWS.Glue.Types.PartitionInput as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains the values and structure used to update a partition.
--
-- /See:/ 'mkBatchUpdatePartitionRequestEntry' smart constructor.
data BatchUpdatePartitionRequestEntry = BatchUpdatePartitionRequestEntry'
  { -- | A list of values defining the partitions.
    partitionValueList :: [Types.ValueString],
    -- | The structure used to update a partition.
    partitionInput :: Types.PartitionInput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchUpdatePartitionRequestEntry' value with any optional fields omitted.
mkBatchUpdatePartitionRequestEntry ::
  -- | 'partitionInput'
  Types.PartitionInput ->
  BatchUpdatePartitionRequestEntry
mkBatchUpdatePartitionRequestEntry partitionInput =
  BatchUpdatePartitionRequestEntry'
    { partitionValueList =
        Core.mempty,
      partitionInput
    }

-- | A list of values defining the partitions.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprePartitionValueList :: Lens.Lens' BatchUpdatePartitionRequestEntry [Types.ValueString]
buprePartitionValueList = Lens.field @"partitionValueList"
{-# DEPRECATED buprePartitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead." #-}

-- | The structure used to update a partition.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprePartitionInput :: Lens.Lens' BatchUpdatePartitionRequestEntry Types.PartitionInput
buprePartitionInput = Lens.field @"partitionInput"
{-# DEPRECATED buprePartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

instance Core.FromJSON BatchUpdatePartitionRequestEntry where
  toJSON BatchUpdatePartitionRequestEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PartitionValueList" Core..= partitionValueList),
            Core.Just ("PartitionInput" Core..= partitionInput)
          ]
      )
