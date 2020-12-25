{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
  ( LateDataRuleConfiguration (..),

    -- * Smart constructor
    mkLateDataRuleConfiguration,

    -- * Lenses
    ldrcDeltaTimeSessionWindowConfiguration,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information needed to configure a delta time session window.
--
-- /See:/ 'mkLateDataRuleConfiguration' smart constructor.
newtype LateDataRuleConfiguration = LateDataRuleConfiguration'
  { -- | The information needed to configure a delta time session window.
    deltaTimeSessionWindowConfiguration :: Core.Maybe Types.DeltaTimeSessionWindowConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LateDataRuleConfiguration' value with any optional fields omitted.
mkLateDataRuleConfiguration ::
  LateDataRuleConfiguration
mkLateDataRuleConfiguration =
  LateDataRuleConfiguration'
    { deltaTimeSessionWindowConfiguration =
        Core.Nothing
    }

-- | The information needed to configure a delta time session window.
--
-- /Note:/ Consider using 'deltaTimeSessionWindowConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrcDeltaTimeSessionWindowConfiguration :: Lens.Lens' LateDataRuleConfiguration (Core.Maybe Types.DeltaTimeSessionWindowConfiguration)
ldrcDeltaTimeSessionWindowConfiguration = Lens.field @"deltaTimeSessionWindowConfiguration"
{-# DEPRECATED ldrcDeltaTimeSessionWindowConfiguration "Use generic-lens or generic-optics with 'deltaTimeSessionWindowConfiguration' instead." #-}

instance Core.FromJSON LateDataRuleConfiguration where
  toJSON LateDataRuleConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("deltaTimeSessionWindowConfiguration" Core..=)
              Core.<$> deltaTimeSessionWindowConfiguration
          ]
      )

instance Core.FromJSON LateDataRuleConfiguration where
  parseJSON =
    Core.withObject "LateDataRuleConfiguration" Core.$
      \x ->
        LateDataRuleConfiguration'
          Core.<$> (x Core..:? "deltaTimeSessionWindowConfiguration")
