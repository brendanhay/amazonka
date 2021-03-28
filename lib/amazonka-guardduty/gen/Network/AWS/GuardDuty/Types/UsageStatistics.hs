{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.UsageStatistics
  ( UsageStatistics (..)
  -- * Smart constructor
  , mkUsageStatistics
  -- * Lenses
  , usSumByAccount
  , usSumByDataSource
  , usSumByResource
  , usTopResources
  ) where

import qualified Network.AWS.GuardDuty.Types.UsageAccountResult as Types
import qualified Network.AWS.GuardDuty.Types.UsageDataSourceResult as Types
import qualified Network.AWS.GuardDuty.Types.UsageResourceResult as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the result of GuardDuty usage. If a UsageStatisticType is provided the result for other types will be null. 
--
-- /See:/ 'mkUsageStatistics' smart constructor.
data UsageStatistics = UsageStatistics'
  { sumByAccount :: Core.Maybe [Types.UsageAccountResult]
    -- ^ The usage statistic sum organized by account ID.
  , sumByDataSource :: Core.Maybe [Types.UsageDataSourceResult]
    -- ^ The usage statistic sum organized by on data source.
  , sumByResource :: Core.Maybe [Types.UsageResourceResult]
    -- ^ The usage statistic sum organized by resource.
  , topResources :: Core.Maybe [Types.UsageResourceResult]
    -- ^ Lists the top 50 resources that have generated the most GuardDuty usage, in order from most to least expensive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageStatistics' value with any optional fields omitted.
mkUsageStatistics
    :: UsageStatistics
mkUsageStatistics
  = UsageStatistics'{sumByAccount = Core.Nothing,
                     sumByDataSource = Core.Nothing, sumByResource = Core.Nothing,
                     topResources = Core.Nothing}

-- | The usage statistic sum organized by account ID.
--
-- /Note:/ Consider using 'sumByAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByAccount :: Lens.Lens' UsageStatistics (Core.Maybe [Types.UsageAccountResult])
usSumByAccount = Lens.field @"sumByAccount"
{-# INLINEABLE usSumByAccount #-}
{-# DEPRECATED sumByAccount "Use generic-lens or generic-optics with 'sumByAccount' instead"  #-}

-- | The usage statistic sum organized by on data source.
--
-- /Note:/ Consider using 'sumByDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByDataSource :: Lens.Lens' UsageStatistics (Core.Maybe [Types.UsageDataSourceResult])
usSumByDataSource = Lens.field @"sumByDataSource"
{-# INLINEABLE usSumByDataSource #-}
{-# DEPRECATED sumByDataSource "Use generic-lens or generic-optics with 'sumByDataSource' instead"  #-}

-- | The usage statistic sum organized by resource.
--
-- /Note:/ Consider using 'sumByResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByResource :: Lens.Lens' UsageStatistics (Core.Maybe [Types.UsageResourceResult])
usSumByResource = Lens.field @"sumByResource"
{-# INLINEABLE usSumByResource #-}
{-# DEPRECATED sumByResource "Use generic-lens or generic-optics with 'sumByResource' instead"  #-}

-- | Lists the top 50 resources that have generated the most GuardDuty usage, in order from most to least expensive.
--
-- /Note:/ Consider using 'topResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTopResources :: Lens.Lens' UsageStatistics (Core.Maybe [Types.UsageResourceResult])
usTopResources = Lens.field @"topResources"
{-# INLINEABLE usTopResources #-}
{-# DEPRECATED topResources "Use generic-lens or generic-optics with 'topResources' instead"  #-}

instance Core.FromJSON UsageStatistics where
        parseJSON
          = Core.withObject "UsageStatistics" Core.$
              \ x ->
                UsageStatistics' Core.<$>
                  (x Core..:? "sumByAccount") Core.<*> x Core..:? "sumByDataSource"
                    Core.<*> x Core..:? "sumByResource"
                    Core.<*> x Core..:? "topResources"
