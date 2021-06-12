{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.FieldStats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.FieldStats where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The statistics for a field calculated in the request.
--
-- /See:/ 'newFieldStats' smart constructor.
data FieldStats = FieldStats'
  { -- | The average of the values found in the specified field in the result
    -- set.
    --
    -- If the field is numeric (@int@, @int-array@, @double@, or
    -- @double-array@), @mean@ is the string representation of a
    -- double-precision 64-bit floating point value. If the field is @date@ or
    -- @date-array@, @mean@ is the string representation of a date with the
    -- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
    -- yyyy-mm-ddTHH:mm:ss.SSSZ.
    mean :: Core.Maybe Core.Text,
    -- | The number of documents that do not contain a value in the specified
    -- field in the result set.
    missing :: Core.Maybe Core.Integer,
    -- | The sum of the field values across the documents in the result set.
    -- @null@ for date fields.
    sum :: Core.Maybe Core.Double,
    -- | The minimum value found in the specified field in the result set.
    --
    -- If the field is numeric (@int@, @int-array@, @double@, or
    -- @double-array@), @min@ is the string representation of a
    -- double-precision 64-bit floating point value. If the field is @date@ or
    -- @date-array@, @min@ is the string representation of a date with the
    -- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
    -- yyyy-mm-ddTHH:mm:ss.SSSZ.
    min :: Core.Maybe Core.Text,
    -- | The maximum value found in the specified field in the result set.
    --
    -- If the field is numeric (@int@, @int-array@, @double@, or
    -- @double-array@), @max@ is the string representation of a
    -- double-precision 64-bit floating point value. If the field is @date@ or
    -- @date-array@, @max@ is the string representation of a date with the
    -- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
    -- yyyy-mm-ddTHH:mm:ss.SSSZ.
    max :: Core.Maybe Core.Text,
    -- | The standard deviation of the values in the specified field in the
    -- result set.
    stddev :: Core.Maybe Core.Double,
    -- | The number of documents that contain a value in the specified field in
    -- the result set.
    count :: Core.Maybe Core.Integer,
    -- | The sum of all field values in the result set squared.
    sumOfSquares :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FieldStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mean', 'fieldStats_mean' - The average of the values found in the specified field in the result
-- set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @mean@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @mean@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- 'missing', 'fieldStats_missing' - The number of documents that do not contain a value in the specified
-- field in the result set.
--
-- 'sum', 'fieldStats_sum' - The sum of the field values across the documents in the result set.
-- @null@ for date fields.
--
-- 'min', 'fieldStats_min' - The minimum value found in the specified field in the result set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @min@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @min@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- 'max', 'fieldStats_max' - The maximum value found in the specified field in the result set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @max@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @max@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- 'stddev', 'fieldStats_stddev' - The standard deviation of the values in the specified field in the
-- result set.
--
-- 'count', 'fieldStats_count' - The number of documents that contain a value in the specified field in
-- the result set.
--
-- 'sumOfSquares', 'fieldStats_sumOfSquares' - The sum of all field values in the result set squared.
newFieldStats ::
  FieldStats
newFieldStats =
  FieldStats'
    { mean = Core.Nothing,
      missing = Core.Nothing,
      sum = Core.Nothing,
      min = Core.Nothing,
      max = Core.Nothing,
      stddev = Core.Nothing,
      count = Core.Nothing,
      sumOfSquares = Core.Nothing
    }

-- | The average of the values found in the specified field in the result
-- set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @mean@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @mean@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
fieldStats_mean :: Lens.Lens' FieldStats (Core.Maybe Core.Text)
fieldStats_mean = Lens.lens (\FieldStats' {mean} -> mean) (\s@FieldStats' {} a -> s {mean = a} :: FieldStats)

-- | The number of documents that do not contain a value in the specified
-- field in the result set.
fieldStats_missing :: Lens.Lens' FieldStats (Core.Maybe Core.Integer)
fieldStats_missing = Lens.lens (\FieldStats' {missing} -> missing) (\s@FieldStats' {} a -> s {missing = a} :: FieldStats)

-- | The sum of the field values across the documents in the result set.
-- @null@ for date fields.
fieldStats_sum :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fieldStats_sum = Lens.lens (\FieldStats' {sum} -> sum) (\s@FieldStats' {} a -> s {sum = a} :: FieldStats)

-- | The minimum value found in the specified field in the result set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @min@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @min@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
fieldStats_min :: Lens.Lens' FieldStats (Core.Maybe Core.Text)
fieldStats_min = Lens.lens (\FieldStats' {min} -> min) (\s@FieldStats' {} a -> s {min = a} :: FieldStats)

-- | The maximum value found in the specified field in the result set.
--
-- If the field is numeric (@int@, @int-array@, @double@, or
-- @double-array@), @max@ is the string representation of a
-- double-precision 64-bit floating point value. If the field is @date@ or
-- @date-array@, @max@ is the string representation of a date with the
-- format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339>:
-- yyyy-mm-ddTHH:mm:ss.SSSZ.
fieldStats_max :: Lens.Lens' FieldStats (Core.Maybe Core.Text)
fieldStats_max = Lens.lens (\FieldStats' {max} -> max) (\s@FieldStats' {} a -> s {max = a} :: FieldStats)

-- | The standard deviation of the values in the specified field in the
-- result set.
fieldStats_stddev :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fieldStats_stddev = Lens.lens (\FieldStats' {stddev} -> stddev) (\s@FieldStats' {} a -> s {stddev = a} :: FieldStats)

-- | The number of documents that contain a value in the specified field in
-- the result set.
fieldStats_count :: Lens.Lens' FieldStats (Core.Maybe Core.Integer)
fieldStats_count = Lens.lens (\FieldStats' {count} -> count) (\s@FieldStats' {} a -> s {count = a} :: FieldStats)

-- | The sum of all field values in the result set squared.
fieldStats_sumOfSquares :: Lens.Lens' FieldStats (Core.Maybe Core.Double)
fieldStats_sumOfSquares = Lens.lens (\FieldStats' {sumOfSquares} -> sumOfSquares) (\s@FieldStats' {} a -> s {sumOfSquares = a} :: FieldStats)

instance Core.FromJSON FieldStats where
  parseJSON =
    Core.withObject
      "FieldStats"
      ( \x ->
          FieldStats'
            Core.<$> (x Core..:? "mean")
            Core.<*> (x Core..:? "missing")
            Core.<*> (x Core..:? "sum")
            Core.<*> (x Core..:? "min")
            Core.<*> (x Core..:? "max")
            Core.<*> (x Core..:? "stddev")
            Core.<*> (x Core..:? "count")
            Core.<*> (x Core..:? "sumOfSquares")
      )

instance Core.Hashable FieldStats

instance Core.NFData FieldStats
