{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Group
  ( Group (..)
  -- * Smart constructor
  , mkGroup
  -- * Lenses
  , gKeys
  , gMetrics
  ) where

import qualified Network.AWS.CostExplorer.Types.Key as Types
import qualified Network.AWS.CostExplorer.Types.MetricName as Types
import qualified Network.AWS.CostExplorer.Types.MetricValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | One level of grouped data in the results.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { keys :: Core.Maybe [Types.Key]
    -- ^ The keys that are included in this group.
  , metrics :: Core.Maybe (Core.HashMap Types.MetricName Types.MetricValue)
    -- ^ The metrics that are included in this group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Group' value with any optional fields omitted.
mkGroup
    :: Group
mkGroup = Group'{keys = Core.Nothing, metrics = Core.Nothing}

-- | The keys that are included in this group.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKeys :: Lens.Lens' Group (Core.Maybe [Types.Key])
gKeys = Lens.field @"keys"
{-# INLINEABLE gKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

-- | The metrics that are included in this group.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gMetrics :: Lens.Lens' Group (Core.Maybe (Core.HashMap Types.MetricName Types.MetricValue))
gMetrics = Lens.field @"metrics"
{-# INLINEABLE gMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

instance Core.FromJSON Group where
        parseJSON
          = Core.withObject "Group" Core.$
              \ x ->
                Group' Core.<$> (x Core..:? "Keys") Core.<*> x Core..:? "Metrics"
