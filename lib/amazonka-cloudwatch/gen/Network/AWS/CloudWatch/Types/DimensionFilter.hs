{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DimensionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DimensionFilter
  ( DimensionFilter (..),

    -- * Smart constructor
    mkDimensionFilter,

    -- * Lenses
    dfName,
    dfValue,
  )
where

import qualified Network.AWS.CloudWatch.Types.DimensionName as Types
import qualified Network.AWS.CloudWatch.Types.DimensionValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents filters for a dimension.
--
-- /See:/ 'mkDimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { -- | The dimension name to be matched.
    name :: Types.DimensionName,
    -- | The value of the dimension to be matched.
    value :: Core.Maybe Types.DimensionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DimensionFilter' value with any optional fields omitted.
mkDimensionFilter ::
  -- | 'name'
  Types.DimensionName ->
  DimensionFilter
mkDimensionFilter name =
  DimensionFilter' {name, value = Core.Nothing}

-- | The dimension name to be matched.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DimensionFilter Types.DimensionName
dfName = Lens.field @"name"
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the dimension to be matched.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfValue :: Lens.Lens' DimensionFilter (Core.Maybe Types.DimensionValue)
dfValue = Lens.field @"value"
{-# DEPRECATED dfValue "Use generic-lens or generic-optics with 'value' instead." #-}
