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
    fsMax,
    fsMean,
    fsCount,
    fsMissing,
    fsStddev,
    fsMin,
    fsSumOfSquares,
    fsSum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The statistics for a field calculated in the request.
--
-- /See:/ 'mkFieldStats' smart constructor.
data FieldStats = FieldStats'
  { max :: Lude.Maybe Lude.Text,
    mean :: Lude.Maybe Lude.Text,
    count :: Lude.Maybe Lude.Integer,
    missing :: Lude.Maybe Lude.Integer,
    stddev :: Lude.Maybe Lude.Double,
    min :: Lude.Maybe Lude.Text,
    sumOfSquares :: Lude.Maybe Lude.Double,
    sum :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldStats' with the minimum fields required to make a request.
--
-- * 'count' - The number of documents that contain a value in the specified field in the result set.
-- * 'max' - The maximum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
-- * 'mean' - The average of the values found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
-- * 'min' - The minimum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
-- * 'missing' - The number of documents that do not contain a value in the specified field in the result set.
-- * 'stddev' - The standard deviation of the values in the specified field in the result set.
-- * 'sum' - The sum of the field values across the documents in the result set. @null@ for date fields.
-- * 'sumOfSquares' - The sum of all field values in the result set squared.
mkFieldStats ::
  FieldStats
mkFieldStats =
  FieldStats'
    { max = Lude.Nothing,
      mean = Lude.Nothing,
      count = Lude.Nothing,
      missing = Lude.Nothing,
      stddev = Lude.Nothing,
      min = Lude.Nothing,
      sumOfSquares = Lude.Nothing,
      sum = Lude.Nothing
    }

-- | The maximum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMax :: Lens.Lens' FieldStats (Lude.Maybe Lude.Text)
fsMax = Lens.lens (max :: FieldStats -> Lude.Maybe Lude.Text) (\s a -> s {max = a} :: FieldStats)
{-# DEPRECATED fsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The average of the values found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'mean' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMean :: Lens.Lens' FieldStats (Lude.Maybe Lude.Text)
fsMean = Lens.lens (mean :: FieldStats -> Lude.Maybe Lude.Text) (\s a -> s {mean = a} :: FieldStats)
{-# DEPRECATED fsMean "Use generic-lens or generic-optics with 'mean' instead." #-}

-- | The number of documents that contain a value in the specified field in the result set.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsCount :: Lens.Lens' FieldStats (Lude.Maybe Lude.Integer)
fsCount = Lens.lens (count :: FieldStats -> Lude.Maybe Lude.Integer) (\s a -> s {count = a} :: FieldStats)
{-# DEPRECATED fsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The number of documents that do not contain a value in the specified field in the result set.
--
-- /Note:/ Consider using 'missing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMissing :: Lens.Lens' FieldStats (Lude.Maybe Lude.Integer)
fsMissing = Lens.lens (missing :: FieldStats -> Lude.Maybe Lude.Integer) (\s a -> s {missing = a} :: FieldStats)
{-# DEPRECATED fsMissing "Use generic-lens or generic-optics with 'missing' instead." #-}

-- | The standard deviation of the values in the specified field in the result set.
--
-- /Note:/ Consider using 'stddev' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsStddev :: Lens.Lens' FieldStats (Lude.Maybe Lude.Double)
fsStddev = Lens.lens (stddev :: FieldStats -> Lude.Maybe Lude.Double) (\s a -> s {stddev = a} :: FieldStats)
{-# DEPRECATED fsStddev "Use generic-lens or generic-optics with 'stddev' instead." #-}

-- | The minimum value found in the specified field in the result set.
--
-- If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMin :: Lens.Lens' FieldStats (Lude.Maybe Lude.Text)
fsMin = Lens.lens (min :: FieldStats -> Lude.Maybe Lude.Text) (\s a -> s {min = a} :: FieldStats)
{-# DEPRECATED fsMin "Use generic-lens or generic-optics with 'min' instead." #-}

-- | The sum of all field values in the result set squared.
--
-- /Note:/ Consider using 'sumOfSquares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSumOfSquares :: Lens.Lens' FieldStats (Lude.Maybe Lude.Double)
fsSumOfSquares = Lens.lens (sumOfSquares :: FieldStats -> Lude.Maybe Lude.Double) (\s a -> s {sumOfSquares = a} :: FieldStats)
{-# DEPRECATED fsSumOfSquares "Use generic-lens or generic-optics with 'sumOfSquares' instead." #-}

-- | The sum of the field values across the documents in the result set. @null@ for date fields.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSum :: Lens.Lens' FieldStats (Lude.Maybe Lude.Double)
fsSum = Lens.lens (sum :: FieldStats -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: FieldStats)
{-# DEPRECATED fsSum "Use generic-lens or generic-optics with 'sum' instead." #-}

instance Lude.FromJSON FieldStats where
  parseJSON =
    Lude.withObject
      "FieldStats"
      ( \x ->
          FieldStats'
            Lude.<$> (x Lude..:? "max")
            Lude.<*> (x Lude..:? "mean")
            Lude.<*> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "missing")
            Lude.<*> (x Lude..:? "stddev")
            Lude.<*> (x Lude..:? "min")
            Lude.<*> (x Lude..:? "sumOfSquares")
            Lude.<*> (x Lude..:? "sum")
      )
