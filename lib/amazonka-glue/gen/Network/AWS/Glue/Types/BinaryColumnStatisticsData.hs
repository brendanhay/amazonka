{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BinaryColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.BinaryColumnStatisticsData
  ( BinaryColumnStatisticsData (..)
  -- * Smart constructor
  , mkBinaryColumnStatisticsData
  -- * Lenses
  , bcsdMaximumLength
  , bcsdAverageLength
  , bcsdNumberOfNulls
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for bit sequence data values.
--
-- /See:/ 'mkBinaryColumnStatisticsData' smart constructor.
data BinaryColumnStatisticsData = BinaryColumnStatisticsData'
  { maximumLength :: Core.Natural
    -- ^ The size of the longest bit sequence in the column.
  , averageLength :: Core.Double
    -- ^ The average bit sequence length in the column.
  , numberOfNulls :: Core.Natural
    -- ^ The number of null values in the column.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BinaryColumnStatisticsData' value with any optional fields omitted.
mkBinaryColumnStatisticsData
    :: Core.Natural -- ^ 'maximumLength'
    -> Core.Double -- ^ 'averageLength'
    -> Core.Natural -- ^ 'numberOfNulls'
    -> BinaryColumnStatisticsData
mkBinaryColumnStatisticsData maximumLength averageLength
  numberOfNulls
  = BinaryColumnStatisticsData'{maximumLength, averageLength,
                                numberOfNulls}

-- | The size of the longest bit sequence in the column.
--
-- /Note:/ Consider using 'maximumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdMaximumLength :: Lens.Lens' BinaryColumnStatisticsData Core.Natural
bcsdMaximumLength = Lens.field @"maximumLength"
{-# INLINEABLE bcsdMaximumLength #-}
{-# DEPRECATED maximumLength "Use generic-lens or generic-optics with 'maximumLength' instead"  #-}

-- | The average bit sequence length in the column.
--
-- /Note:/ Consider using 'averageLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdAverageLength :: Lens.Lens' BinaryColumnStatisticsData Core.Double
bcsdAverageLength = Lens.field @"averageLength"
{-# INLINEABLE bcsdAverageLength #-}
{-# DEPRECATED averageLength "Use generic-lens or generic-optics with 'averageLength' instead"  #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdNumberOfNulls :: Lens.Lens' BinaryColumnStatisticsData Core.Natural
bcsdNumberOfNulls = Lens.field @"numberOfNulls"
{-# INLINEABLE bcsdNumberOfNulls #-}
{-# DEPRECATED numberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead"  #-}

instance Core.FromJSON BinaryColumnStatisticsData where
        toJSON BinaryColumnStatisticsData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaximumLength" Core..= maximumLength),
                  Core.Just ("AverageLength" Core..= averageLength),
                  Core.Just ("NumberOfNulls" Core..= numberOfNulls)])

instance Core.FromJSON BinaryColumnStatisticsData where
        parseJSON
          = Core.withObject "BinaryColumnStatisticsData" Core.$
              \ x ->
                BinaryColumnStatisticsData' Core.<$>
                  (x Core..: "MaximumLength") Core.<*> x Core..: "AverageLength"
                    Core.<*> x Core..: "NumberOfNulls"
