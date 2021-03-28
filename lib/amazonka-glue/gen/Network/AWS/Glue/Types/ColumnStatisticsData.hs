{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ColumnStatisticsData
  ( ColumnStatisticsData (..)
  -- * Smart constructor
  , mkColumnStatisticsData
  -- * Lenses
  , csdType
  , csdBinaryColumnStatisticsData
  , csdBooleanColumnStatisticsData
  , csdDateColumnStatisticsData
  , csdDecimalColumnStatisticsData
  , csdDoubleColumnStatisticsData
  , csdLongColumnStatisticsData
  , csdStringColumnStatisticsData
  ) where

import qualified Network.AWS.Glue.Types.BinaryColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.BooleanColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.ColumnStatisticsType as Types
import qualified Network.AWS.Glue.Types.DateColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.DecimalColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.DoubleColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.LongColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.StringColumnStatisticsData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the individual types of column statistics data. Only one data object should be set and indicated by the @Type@ attribute.
--
-- /See:/ 'mkColumnStatisticsData' smart constructor.
data ColumnStatisticsData = ColumnStatisticsData'
  { type' :: Types.ColumnStatisticsType
    -- ^ The type of column statistics data.
  , binaryColumnStatisticsData :: Core.Maybe Types.BinaryColumnStatisticsData
    -- ^ Binary column statistics data.
  , booleanColumnStatisticsData :: Core.Maybe Types.BooleanColumnStatisticsData
    -- ^ Boolean column statistics data.
  , dateColumnStatisticsData :: Core.Maybe Types.DateColumnStatisticsData
    -- ^ Date column statistics data.
  , decimalColumnStatisticsData :: Core.Maybe Types.DecimalColumnStatisticsData
    -- ^ Decimal column statistics data.
  , doubleColumnStatisticsData :: Core.Maybe Types.DoubleColumnStatisticsData
    -- ^ Double column statistics data.
  , longColumnStatisticsData :: Core.Maybe Types.LongColumnStatisticsData
    -- ^ Long column statistics data.
  , stringColumnStatisticsData :: Core.Maybe Types.StringColumnStatisticsData
    -- ^ String column statistics data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ColumnStatisticsData' value with any optional fields omitted.
mkColumnStatisticsData
    :: Types.ColumnStatisticsType -- ^ 'type\''
    -> ColumnStatisticsData
mkColumnStatisticsData type'
  = ColumnStatisticsData'{type',
                          binaryColumnStatisticsData = Core.Nothing,
                          booleanColumnStatisticsData = Core.Nothing,
                          dateColumnStatisticsData = Core.Nothing,
                          decimalColumnStatisticsData = Core.Nothing,
                          doubleColumnStatisticsData = Core.Nothing,
                          longColumnStatisticsData = Core.Nothing,
                          stringColumnStatisticsData = Core.Nothing}

-- | The type of column statistics data.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdType :: Lens.Lens' ColumnStatisticsData Types.ColumnStatisticsType
csdType = Lens.field @"type'"
{-# INLINEABLE csdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Binary column statistics data.
--
-- /Note:/ Consider using 'binaryColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdBinaryColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.BinaryColumnStatisticsData)
csdBinaryColumnStatisticsData = Lens.field @"binaryColumnStatisticsData"
{-# INLINEABLE csdBinaryColumnStatisticsData #-}
{-# DEPRECATED binaryColumnStatisticsData "Use generic-lens or generic-optics with 'binaryColumnStatisticsData' instead"  #-}

-- | Boolean column statistics data.
--
-- /Note:/ Consider using 'booleanColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdBooleanColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.BooleanColumnStatisticsData)
csdBooleanColumnStatisticsData = Lens.field @"booleanColumnStatisticsData"
{-# INLINEABLE csdBooleanColumnStatisticsData #-}
{-# DEPRECATED booleanColumnStatisticsData "Use generic-lens or generic-optics with 'booleanColumnStatisticsData' instead"  #-}

-- | Date column statistics data.
--
-- /Note:/ Consider using 'dateColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.DateColumnStatisticsData)
csdDateColumnStatisticsData = Lens.field @"dateColumnStatisticsData"
{-# INLINEABLE csdDateColumnStatisticsData #-}
{-# DEPRECATED dateColumnStatisticsData "Use generic-lens or generic-optics with 'dateColumnStatisticsData' instead"  #-}

-- | Decimal column statistics data.
--
-- /Note:/ Consider using 'decimalColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDecimalColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.DecimalColumnStatisticsData)
csdDecimalColumnStatisticsData = Lens.field @"decimalColumnStatisticsData"
{-# INLINEABLE csdDecimalColumnStatisticsData #-}
{-# DEPRECATED decimalColumnStatisticsData "Use generic-lens or generic-optics with 'decimalColumnStatisticsData' instead"  #-}

-- | Double column statistics data.
--
-- /Note:/ Consider using 'doubleColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDoubleColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.DoubleColumnStatisticsData)
csdDoubleColumnStatisticsData = Lens.field @"doubleColumnStatisticsData"
{-# INLINEABLE csdDoubleColumnStatisticsData #-}
{-# DEPRECATED doubleColumnStatisticsData "Use generic-lens or generic-optics with 'doubleColumnStatisticsData' instead"  #-}

-- | Long column statistics data.
--
-- /Note:/ Consider using 'longColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdLongColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.LongColumnStatisticsData)
csdLongColumnStatisticsData = Lens.field @"longColumnStatisticsData"
{-# INLINEABLE csdLongColumnStatisticsData #-}
{-# DEPRECATED longColumnStatisticsData "Use generic-lens or generic-optics with 'longColumnStatisticsData' instead"  #-}

-- | String column statistics data.
--
-- /Note:/ Consider using 'stringColumnStatisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdStringColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Core.Maybe Types.StringColumnStatisticsData)
csdStringColumnStatisticsData = Lens.field @"stringColumnStatisticsData"
{-# INLINEABLE csdStringColumnStatisticsData #-}
{-# DEPRECATED stringColumnStatisticsData "Use generic-lens or generic-optics with 'stringColumnStatisticsData' instead"  #-}

instance Core.FromJSON ColumnStatisticsData where
        toJSON ColumnStatisticsData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("BinaryColumnStatisticsData" Core..=) Core.<$>
                    binaryColumnStatisticsData,
                  ("BooleanColumnStatisticsData" Core..=) Core.<$>
                    booleanColumnStatisticsData,
                  ("DateColumnStatisticsData" Core..=) Core.<$>
                    dateColumnStatisticsData,
                  ("DecimalColumnStatisticsData" Core..=) Core.<$>
                    decimalColumnStatisticsData,
                  ("DoubleColumnStatisticsData" Core..=) Core.<$>
                    doubleColumnStatisticsData,
                  ("LongColumnStatisticsData" Core..=) Core.<$>
                    longColumnStatisticsData,
                  ("StringColumnStatisticsData" Core..=) Core.<$>
                    stringColumnStatisticsData])

instance Core.FromJSON ColumnStatisticsData where
        parseJSON
          = Core.withObject "ColumnStatisticsData" Core.$
              \ x ->
                ColumnStatisticsData' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..:? "BinaryColumnStatisticsData"
                    Core.<*> x Core..:? "BooleanColumnStatisticsData"
                    Core.<*> x Core..:? "DateColumnStatisticsData"
                    Core.<*> x Core..:? "DecimalColumnStatisticsData"
                    Core.<*> x Core..:? "DoubleColumnStatisticsData"
                    Core.<*> x Core..:? "LongColumnStatisticsData"
                    Core.<*> x Core..:? "StringColumnStatisticsData"
