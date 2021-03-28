{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SetDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SetDimension
  ( SetDimension (..)
  -- * Smart constructor
  , mkSetDimension
  -- * Lenses
  , sdValues
  , sdDimensionType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DimensionType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the dimension type and values for a segment dimension.
--
-- /See:/ 'mkSetDimension' smart constructor.
data SetDimension = SetDimension'
  { values :: [Core.Text]
    -- ^ The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
  , dimensionType :: Core.Maybe Types.DimensionType
    -- ^ The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDimension' value with any optional fields omitted.
mkSetDimension
    :: SetDimension
mkSetDimension
  = SetDimension'{values = Core.mempty, dimensionType = Core.Nothing}

-- | The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdValues :: Lens.Lens' SetDimension [Core.Text]
sdValues = Lens.field @"values"
{-# INLINEABLE sdValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

-- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
--
-- /Note:/ Consider using 'dimensionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDimensionType :: Lens.Lens' SetDimension (Core.Maybe Types.DimensionType)
sdDimensionType = Lens.field @"dimensionType"
{-# INLINEABLE sdDimensionType #-}
{-# DEPRECATED dimensionType "Use generic-lens or generic-optics with 'dimensionType' instead"  #-}

instance Core.FromJSON SetDimension where
        toJSON SetDimension{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Values" Core..= values),
                  ("DimensionType" Core..=) Core.<$> dimensionType])

instance Core.FromJSON SetDimension where
        parseJSON
          = Core.withObject "SetDimension" Core.$
              \ x ->
                SetDimension' Core.<$>
                  (x Core..:? "Values" Core..!= Core.mempty) Core.<*>
                    x Core..:? "DimensionType"
