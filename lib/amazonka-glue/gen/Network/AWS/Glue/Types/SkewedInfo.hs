{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SkewedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SkewedInfo
  ( SkewedInfo (..),

    -- * Smart constructor
    mkSkewedInfo,

    -- * Lenses
    siSkewedColumnNames,
    siSkewedColumnValueLocationMaps,
    siSkewedColumnValues,
  )
where

import qualified Network.AWS.Glue.Types.ColumnValuesString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies skewed values in a table. Skewed values are those that occur with very high frequency.
--
-- /See:/ 'mkSkewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { -- | A list of names of columns that contain skewed values.
    skewedColumnNames :: Core.Maybe [Types.NameString],
    -- | A mapping of skewed values to the columns that contain them.
    skewedColumnValueLocationMaps :: Core.Maybe (Core.HashMap Types.ColumnValuesString Types.ColumnValuesString),
    -- | A list of values that appear so frequently as to be considered skewed.
    skewedColumnValues :: Core.Maybe [Types.ColumnValuesString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkewedInfo' value with any optional fields omitted.
mkSkewedInfo ::
  SkewedInfo
mkSkewedInfo =
  SkewedInfo'
    { skewedColumnNames = Core.Nothing,
      skewedColumnValueLocationMaps = Core.Nothing,
      skewedColumnValues = Core.Nothing
    }

-- | A list of names of columns that contain skewed values.
--
-- /Note:/ Consider using 'skewedColumnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnNames :: Lens.Lens' SkewedInfo (Core.Maybe [Types.NameString])
siSkewedColumnNames = Lens.field @"skewedColumnNames"
{-# DEPRECATED siSkewedColumnNames "Use generic-lens or generic-optics with 'skewedColumnNames' instead." #-}

-- | A mapping of skewed values to the columns that contain them.
--
-- /Note:/ Consider using 'skewedColumnValueLocationMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnValueLocationMaps :: Lens.Lens' SkewedInfo (Core.Maybe (Core.HashMap Types.ColumnValuesString Types.ColumnValuesString))
siSkewedColumnValueLocationMaps = Lens.field @"skewedColumnValueLocationMaps"
{-# DEPRECATED siSkewedColumnValueLocationMaps "Use generic-lens or generic-optics with 'skewedColumnValueLocationMaps' instead." #-}

-- | A list of values that appear so frequently as to be considered skewed.
--
-- /Note:/ Consider using 'skewedColumnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSkewedColumnValues :: Lens.Lens' SkewedInfo (Core.Maybe [Types.ColumnValuesString])
siSkewedColumnValues = Lens.field @"skewedColumnValues"
{-# DEPRECATED siSkewedColumnValues "Use generic-lens or generic-optics with 'skewedColumnValues' instead." #-}

instance Core.FromJSON SkewedInfo where
  toJSON SkewedInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkewedColumnNames" Core..=) Core.<$> skewedColumnNames,
            ("SkewedColumnValueLocationMaps" Core..=)
              Core.<$> skewedColumnValueLocationMaps,
            ("SkewedColumnValues" Core..=) Core.<$> skewedColumnValues
          ]
      )

instance Core.FromJSON SkewedInfo where
  parseJSON =
    Core.withObject "SkewedInfo" Core.$
      \x ->
        SkewedInfo'
          Core.<$> (x Core..:? "SkewedColumnNames")
          Core.<*> (x Core..:? "SkewedColumnValueLocationMaps")
          Core.<*> (x Core..:? "SkewedColumnValues")
