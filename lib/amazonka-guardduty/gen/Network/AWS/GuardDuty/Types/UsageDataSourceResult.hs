{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageDataSourceResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.UsageDataSourceResult
  ( UsageDataSourceResult (..)
  -- * Smart constructor
  , mkUsageDataSourceResult
  -- * Lenses
  , udsrDataSource
  , udsrTotal
  ) where

import qualified Network.AWS.GuardDuty.Types.DataSource as Types
import qualified Network.AWS.GuardDuty.Types.Total as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the result of usage based on data source type.
--
-- /See:/ 'mkUsageDataSourceResult' smart constructor.
data UsageDataSourceResult = UsageDataSourceResult'
  { dataSource :: Core.Maybe Types.DataSource
    -- ^ The data source type that generated usage.
  , total :: Core.Maybe Types.Total
    -- ^ Represents the total of usage for the specified data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageDataSourceResult' value with any optional fields omitted.
mkUsageDataSourceResult
    :: UsageDataSourceResult
mkUsageDataSourceResult
  = UsageDataSourceResult'{dataSource = Core.Nothing,
                           total = Core.Nothing}

-- | The data source type that generated usage.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrDataSource :: Lens.Lens' UsageDataSourceResult (Core.Maybe Types.DataSource)
udsrDataSource = Lens.field @"dataSource"
{-# INLINEABLE udsrDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | Represents the total of usage for the specified data source.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrTotal :: Lens.Lens' UsageDataSourceResult (Core.Maybe Types.Total)
udsrTotal = Lens.field @"total"
{-# INLINEABLE udsrTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

instance Core.FromJSON UsageDataSourceResult where
        parseJSON
          = Core.withObject "UsageDataSourceResult" Core.$
              \ x ->
                UsageDataSourceResult' Core.<$>
                  (x Core..:? "dataSource") Core.<*> x Core..:? "total"
