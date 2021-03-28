{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.InsightsConfiguration
  ( InsightsConfiguration (..)
  -- * Smart constructor
  , mkInsightsConfiguration
  -- * Lenses
  , icInsightsEnabled
  , icNotificationsEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure containing configurations related to insights.
--
-- /See:/ 'mkInsightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { insightsEnabled :: Core.Maybe Core.Bool
    -- ^ Set the InsightsEnabled value to true to enable insights or false to disable insights.
  , notificationsEnabled :: Core.Maybe Core.Bool
    -- ^ Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InsightsConfiguration' value with any optional fields omitted.
mkInsightsConfiguration
    :: InsightsConfiguration
mkInsightsConfiguration
  = InsightsConfiguration'{insightsEnabled = Core.Nothing,
                           notificationsEnabled = Core.Nothing}

-- | Set the InsightsEnabled value to true to enable insights or false to disable insights.
--
-- /Note:/ Consider using 'insightsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInsightsEnabled :: Lens.Lens' InsightsConfiguration (Core.Maybe Core.Bool)
icInsightsEnabled = Lens.field @"insightsEnabled"
{-# INLINEABLE icInsightsEnabled #-}
{-# DEPRECATED insightsEnabled "Use generic-lens or generic-optics with 'insightsEnabled' instead"  #-}

-- | Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
-- /Note:/ Consider using 'notificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icNotificationsEnabled :: Lens.Lens' InsightsConfiguration (Core.Maybe Core.Bool)
icNotificationsEnabled = Lens.field @"notificationsEnabled"
{-# INLINEABLE icNotificationsEnabled #-}
{-# DEPRECATED notificationsEnabled "Use generic-lens or generic-optics with 'notificationsEnabled' instead"  #-}

instance Core.FromJSON InsightsConfiguration where
        toJSON InsightsConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("InsightsEnabled" Core..=) Core.<$> insightsEnabled,
                  ("NotificationsEnabled" Core..=) Core.<$> notificationsEnabled])

instance Core.FromJSON InsightsConfiguration where
        parseJSON
          = Core.withObject "InsightsConfiguration" Core.$
              \ x ->
                InsightsConfiguration' Core.<$>
                  (x Core..:? "InsightsEnabled") Core.<*>
                    x Core..:? "NotificationsEnabled"
