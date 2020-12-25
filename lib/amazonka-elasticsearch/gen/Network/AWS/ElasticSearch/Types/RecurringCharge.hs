{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.RecurringCharge
  ( RecurringCharge (..),

    -- * Smart constructor
    mkRecurringCharge,

    -- * Lenses
    rcRecurringChargeAmount,
    rcRecurringChargeFrequency,
  )
where

import qualified Network.AWS.ElasticSearch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the specific price and frequency of a recurring charges for a reserved Elasticsearch instance, or for a reserved Elasticsearch instance offering.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The monetary amount of the recurring charge.
    recurringChargeAmount :: Core.Maybe Core.Double,
    -- | The frequency of the recurring charge.
    recurringChargeFrequency :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecurringCharge' value with any optional fields omitted.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge'
    { recurringChargeAmount = Core.Nothing,
      recurringChargeFrequency = Core.Nothing
    }

-- | The monetary amount of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeAmount :: Lens.Lens' RecurringCharge (Core.Maybe Core.Double)
rcRecurringChargeAmount = Lens.field @"recurringChargeAmount"
{-# DEPRECATED rcRecurringChargeAmount "Use generic-lens or generic-optics with 'recurringChargeAmount' instead." #-}

-- | The frequency of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeFrequency :: Lens.Lens' RecurringCharge (Core.Maybe Types.String)
rcRecurringChargeFrequency = Lens.field @"recurringChargeFrequency"
{-# DEPRECATED rcRecurringChargeFrequency "Use generic-lens or generic-optics with 'recurringChargeFrequency' instead." #-}

instance Core.FromJSON RecurringCharge where
  parseJSON =
    Core.withObject "RecurringCharge" Core.$
      \x ->
        RecurringCharge'
          Core.<$> (x Core..:? "RecurringChargeAmount")
          Core.<*> (x Core..:? "RecurringChargeFrequency")
