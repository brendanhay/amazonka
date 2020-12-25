{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.FieldStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.FieldStats
  ( FieldStats (..),

    -- * Smart constructor
    mkFieldStats,

    -- * Lenses
    fsCount,
    fsMax,
    fsMean,
    fsMin,
    fsMissing,
    fsStddev,
    fsSum,
    fsSumOfSquares,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The statistics for a field calculated in the request.
--
-- /See:/ 'mkFieldStats' smart constructor.
data FieldStats = FieldStats'
  { -- | The number of documents that contain a value in the specified field in the result set.
    count :: Core.Maybe Core.Integer,
    -- | The maximum value found in the specified field in the result set.
    --
    -- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
    max :: Core.Maybe Types.String,
    -- | The average of the values found in the specified field in the result set.
    --
    -- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
    mean :: Core.Maybe Types.String,
    -- | The minimum value found in the specified field in the result set.
    --
    -- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
    min :: Core.Maybe Types.String,
    -- | The number of documents that do not contain a value in the specified field in the result set.
    missing :: Core.Maybe Core.Integer,
    -- | The standard deviation of the values in the specified field in the result set.
    stddev :: Core.Maybe Core.Double,
    -- | The sum of the field values across the documents in the result set. @null@ for date fields.
    sum :: Core.Maybe Core.Double,
    -- | The sum of all field values in the result set squared.
    sumOfSquares :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FieldStats' value with any optional fields omitted.
mkFieldStats ::
  FieldStats
mkFieldStats =
  FieldStats'
    { count = Core.Nothing,
      max = Core.Nothing,
      mean = Core.Nothing,
      min = Core.Nothing,
      missing = Core.Nothing,
      stddev = Core.Nothing,
      sum = Core.Nothing,
      sumOfSquares = Core.Nothing
    }

-- | The number of documents that contain a value in the specified field in the result set.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsCount :: Lens.Lens' FieldStats (Core.Maybe Core.Integer)
fsCount = Lens.field @"count"
{-# DEPRECATED fsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The maximum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMax :: Lens.Lens' FieldStats (Core.Maybe Types.String)
fsMax = Lens.field @"max"
{-# DEPRECATED fsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The average of the values found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'mean' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMean :: Lens.Lens' FieldStats (Core.Maybe Types.String)
fsMean = Lens.field @"mean"
{-# DEPRECATED fsMean "Use generic-lens or generic-optics with 'mean' instead." #-}

-- | The minimum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMin :: Lens.Lens' FieldStats (Core.Maybe Types.String)
fsMin = Lens.field @"min"
{-# DEPRECATED fsMin "Use generic-lens or generic-optics with 'min' instead." #-}

-- | The number of documents that do not contain a value in the specified field in the result set.
--
-- /Note:/ Consider using 'missing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMissing :: Lens.Lens' FieldStats (Core.Maybe Core.Integer)
fsMissing = Lens.field @"missing"
{-# DEPRECATED fsMissing "Use generic-lens or generic-optics with 'missing' instead." #-}

-- | The standard deviation of the values in the specified field in the result set.
--
-- /Note:/ Consider using 'stddev' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsStddev :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fsStddev = Lens.field @"stddev"
{-# DEPRECATED fsStddev "Use generic-lens or generic-optics with 'stddev' instead." #-}

-- | The sum of the field values across the documents in the result set. @null@ for date fields.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSum :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fsSum = Lens.field @"sum"
{-# DEPRECATED fsSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The sum of all field values in the result set squared.
--
-- /Note:/ Consider using 'sumOfSquares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSumOfSquares :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fsSumOfSquares = Lens.field @"sumOfSquares"
{-# DEPRECATED fsSumOfSquares "Use generic-lens or generic-optics with 'sumOfSquares' instead." #-}

instance Core.FromJSON FieldStats where
  parseJSON =
    Core.withObject "FieldStats" Core.$
      \x ->
        FieldStats'
          Core.<$> (x Core..:? "count")
          Core.<*> (x Core..:? "max")
          Core.<*> (x Core..:? "mean")
          Core.<*> (x Core..:? "min")
          Core.<*> (x Core..:? "missing")
          Core.<*> (x Core..:? "stddev")
          Core.<*> (x Core..:? "sum")
          Core.<*> (x Core..:? "sumOfSquares")
