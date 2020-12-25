{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricResult
  ( CurrentMetricResult (..),

    -- * Smart constructor
    mkCurrentMetricResult,

    -- * Lenses
    cmrCollections,
    cmrDimensions,
  )
where

import qualified Network.AWS.Connect.Types.CurrentMetricData as Types
import qualified Network.AWS.Connect.Types.Dimensions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a set of real-time metrics.
--
-- /See:/ 'mkCurrentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { -- | The set of metrics.
    collections :: Core.Maybe [Types.CurrentMetricData],
    -- | The dimensions for the metrics.
    dimensions :: Core.Maybe Types.Dimensions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CurrentMetricResult' value with any optional fields omitted.
mkCurrentMetricResult ::
  CurrentMetricResult
mkCurrentMetricResult =
  CurrentMetricResult'
    { collections = Core.Nothing,
      dimensions = Core.Nothing
    }

-- | The set of metrics.
--
-- /Note:/ Consider using 'collections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrCollections :: Lens.Lens' CurrentMetricResult (Core.Maybe [Types.CurrentMetricData])
cmrCollections = Lens.field @"collections"
{-# DEPRECATED cmrCollections "Use generic-lens or generic-optics with 'collections' instead." #-}

-- | The dimensions for the metrics.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrDimensions :: Lens.Lens' CurrentMetricResult (Core.Maybe Types.Dimensions)
cmrDimensions = Lens.field @"dimensions"
{-# DEPRECATED cmrDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Core.FromJSON CurrentMetricResult where
  parseJSON =
    Core.withObject "CurrentMetricResult" Core.$
      \x ->
        CurrentMetricResult'
          Core.<$> (x Core..:? "Collections") Core.<*> (x Core..:? "Dimensions")
