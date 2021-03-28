{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.QuotaSettings
  ( QuotaSettings (..)
  -- * Smart constructor
  , mkQuotaSettings
  -- * Lenses
  , qsLimit
  , qsOffset
  , qsPeriod
  ) where

import qualified Network.AWS.ApiGateway.Types.QuotaPeriodType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Quotas configured for a usage plan.
--
-- /See:/ 'mkQuotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of requests that can be made in a given time period.
  , offset :: Core.Maybe Core.Int
    -- ^ The number of requests subtracted from the given limit in the initial time period.
  , period :: Core.Maybe Types.QuotaPeriodType
    -- ^ The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuotaSettings' value with any optional fields omitted.
mkQuotaSettings
    :: QuotaSettings
mkQuotaSettings
  = QuotaSettings'{limit = Core.Nothing, offset = Core.Nothing,
                   period = Core.Nothing}

-- | The maximum number of requests that can be made in a given time period.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsLimit :: Lens.Lens' QuotaSettings (Core.Maybe Core.Int)
qsLimit = Lens.field @"limit"
{-# INLINEABLE qsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The number of requests subtracted from the given limit in the initial time period.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsOffset :: Lens.Lens' QuotaSettings (Core.Maybe Core.Int)
qsOffset = Lens.field @"offset"
{-# INLINEABLE qsOffset #-}
{-# DEPRECATED offset "Use generic-lens or generic-optics with 'offset' instead"  #-}

-- | The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsPeriod :: Lens.Lens' QuotaSettings (Core.Maybe Types.QuotaPeriodType)
qsPeriod = Lens.field @"period"
{-# INLINEABLE qsPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

instance Core.FromJSON QuotaSettings where
        toJSON QuotaSettings{..}
          = Core.object
              (Core.catMaybes
                 [("limit" Core..=) Core.<$> limit,
                  ("offset" Core..=) Core.<$> offset,
                  ("period" Core..=) Core.<$> period])

instance Core.FromJSON QuotaSettings where
        parseJSON
          = Core.withObject "QuotaSettings" Core.$
              \ x ->
                QuotaSettings' Core.<$>
                  (x Core..:? "limit") Core.<*> x Core..:? "offset" Core.<*>
                    x Core..:? "period"
