{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PartitionValueList
  ( PartitionValueList (..)
  -- * Smart constructor
  , mkPartitionValueList
  -- * Lenses
  , pvlValues
  ) where

import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a list of values defining partitions.
--
-- /See:/ 'mkPartitionValueList' smart constructor.
newtype PartitionValueList = PartitionValueList'
  { values :: [Types.ValueString]
    -- ^ The list of values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PartitionValueList' value with any optional fields omitted.
mkPartitionValueList
    :: PartitionValueList
mkPartitionValueList = PartitionValueList'{values = Core.mempty}

-- | The list of values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvlValues :: Lens.Lens' PartitionValueList [Types.ValueString]
pvlValues = Lens.field @"values"
{-# INLINEABLE pvlValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON PartitionValueList where
        toJSON PartitionValueList{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Values" Core..= values)])

instance Core.FromJSON PartitionValueList where
        parseJSON
          = Core.withObject "PartitionValueList" Core.$
              \ x ->
                PartitionValueList' Core.<$>
                  (x Core..:? "Values" Core..!= Core.mempty)
