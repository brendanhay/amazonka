{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
  ( InstanceHealthSummary (..)
  -- * Smart constructor
  , mkInstanceHealthSummary
  -- * Lenses
  , ihsDegraded
  , ihsInfo
  , ihsNoData
  , ihsOk
  , ihsPending
  , ihsSevere
  , ihsUnknown
  , ihsWarning
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents summary information about the health of an instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /See:/ 'mkInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { degraded :: Core.Maybe Core.Int
    -- ^ __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
  , info :: Core.Maybe Core.Int
    -- ^ __Green.__ An operation is in progress on an instance.
  , noData :: Core.Maybe Core.Int
    -- ^ __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
  , ok :: Core.Maybe Core.Int
    -- ^ __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
  , pending :: Core.Maybe Core.Int
    -- ^ __Grey.__ An operation is in progress on an instance within the command timeout.
  , severe :: Core.Maybe Core.Int
    -- ^ __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
  , unknown :: Core.Maybe Core.Int
    -- ^ __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
  , warning :: Core.Maybe Core.Int
    -- ^ __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceHealthSummary' value with any optional fields omitted.
mkInstanceHealthSummary
    :: InstanceHealthSummary
mkInstanceHealthSummary
  = InstanceHealthSummary'{degraded = Core.Nothing,
                           info = Core.Nothing, noData = Core.Nothing, ok = Core.Nothing,
                           pending = Core.Nothing, severe = Core.Nothing,
                           unknown = Core.Nothing, warning = Core.Nothing}

-- | __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'degraded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsDegraded :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsDegraded = Lens.field @"degraded"
{-# INLINEABLE ihsDegraded #-}
{-# DEPRECATED degraded "Use generic-lens or generic-optics with 'degraded' instead"  #-}

-- | __Green.__ An operation is in progress on an instance.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInfo :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsInfo = Lens.field @"info"
{-# INLINEABLE ihsInfo #-}
{-# DEPRECATED info "Use generic-lens or generic-optics with 'info' instead"  #-}

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
--
-- /Note:/ Consider using 'noData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsNoData :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsNoData = Lens.field @"noData"
{-# INLINEABLE ihsNoData #-}
{-# DEPRECATED noData "Use generic-lens or generic-optics with 'noData' instead"  #-}

-- | __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
--
-- /Note:/ Consider using 'ok' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsOk :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsOk = Lens.field @"ok"
{-# INLINEABLE ihsOk #-}
{-# DEPRECATED ok "Use generic-lens or generic-optics with 'ok' instead"  #-}

-- | __Grey.__ An operation is in progress on an instance within the command timeout.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsPending :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsPending = Lens.field @"pending"
{-# INLINEABLE ihsPending #-}
{-# DEPRECATED pending "Use generic-lens or generic-optics with 'pending' instead"  #-}

-- | __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'severe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsSevere :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsSevere = Lens.field @"severe"
{-# INLINEABLE ihsSevere #-}
{-# DEPRECATED severe "Use generic-lens or generic-optics with 'severe' instead"  #-}

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
--
-- /Note:/ Consider using 'unknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsUnknown :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsUnknown = Lens.field @"unknown"
{-# INLINEABLE ihsUnknown #-}
{-# DEPRECATED unknown "Use generic-lens or generic-optics with 'unknown' instead"  #-}

-- | __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsWarning :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
ihsWarning = Lens.field @"warning"
{-# INLINEABLE ihsWarning #-}
{-# DEPRECATED warning "Use generic-lens or generic-optics with 'warning' instead"  #-}

instance Core.FromXML InstanceHealthSummary where
        parseXML x
          = InstanceHealthSummary' Core.<$>
              (x Core..@? "Degraded") Core.<*> x Core..@? "Info" Core.<*>
                x Core..@? "NoData"
                Core.<*> x Core..@? "Ok"
                Core.<*> x Core..@? "Pending"
                Core.<*> x Core..@? "Severe"
                Core.<*> x Core..@? "Unknown"
                Core.<*> x Core..@? "Warning"
