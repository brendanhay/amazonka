{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.RetentionPeriod
  ( RetentionPeriod (..)
  -- * Smart constructor
  , mkRetentionPeriod
  -- * Lenses
  , rpNumberOfDays
  , rpUnlimited
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | How long, in days, message data is kept.
--
-- /See:/ 'mkRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { numberOfDays :: Core.Maybe Core.Natural
    -- ^ The number of days that message data is kept. The @unlimited@ parameter must be false.
  , unlimited :: Core.Maybe Core.Bool
    -- ^ If true, message data is kept indefinitely.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetentionPeriod' value with any optional fields omitted.
mkRetentionPeriod
    :: RetentionPeriod
mkRetentionPeriod
  = RetentionPeriod'{numberOfDays = Core.Nothing,
                     unlimited = Core.Nothing}

-- | The number of days that message data is kept. The @unlimited@ parameter must be false.
--
-- /Note:/ Consider using 'numberOfDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpNumberOfDays :: Lens.Lens' RetentionPeriod (Core.Maybe Core.Natural)
rpNumberOfDays = Lens.field @"numberOfDays"
{-# INLINEABLE rpNumberOfDays #-}
{-# DEPRECATED numberOfDays "Use generic-lens or generic-optics with 'numberOfDays' instead"  #-}

-- | If true, message data is kept indefinitely.
--
-- /Note:/ Consider using 'unlimited' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpUnlimited :: Lens.Lens' RetentionPeriod (Core.Maybe Core.Bool)
rpUnlimited = Lens.field @"unlimited"
{-# INLINEABLE rpUnlimited #-}
{-# DEPRECATED unlimited "Use generic-lens or generic-optics with 'unlimited' instead"  #-}

instance Core.FromJSON RetentionPeriod where
        toJSON RetentionPeriod{..}
          = Core.object
              (Core.catMaybes
                 [("numberOfDays" Core..=) Core.<$> numberOfDays,
                  ("unlimited" Core..=) Core.<$> unlimited])

instance Core.FromJSON RetentionPeriod where
        parseJSON
          = Core.withObject "RetentionPeriod" Core.$
              \ x ->
                RetentionPeriod' Core.<$>
                  (x Core..:? "numberOfDays") Core.<*> x Core..:? "unlimited"
