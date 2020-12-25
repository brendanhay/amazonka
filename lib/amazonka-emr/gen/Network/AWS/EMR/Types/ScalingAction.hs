{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingAction
  ( ScalingAction (..),

    -- * Smart constructor
    mkScalingAction,

    -- * Lenses
    saSimpleScalingPolicyConfiguration,
    saMarket,
  )
where

import qualified Network.AWS.EMR.Types.MarketType as Types
import qualified Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
-- /See:/ 'mkScalingAction' smart constructor.
data ScalingAction = ScalingAction'
  { -- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
    simpleScalingPolicyConfiguration :: Types.SimpleScalingPolicyConfiguration,
    -- | Not available for instance groups. Instance groups use the market type specified for the group.
    market :: Core.Maybe Types.MarketType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingAction' value with any optional fields omitted.
mkScalingAction ::
  -- | 'simpleScalingPolicyConfiguration'
  Types.SimpleScalingPolicyConfiguration ->
  ScalingAction
mkScalingAction simpleScalingPolicyConfiguration =
  ScalingAction'
    { simpleScalingPolicyConfiguration,
      market = Core.Nothing
    }

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
-- /Note:/ Consider using 'simpleScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSimpleScalingPolicyConfiguration :: Lens.Lens' ScalingAction Types.SimpleScalingPolicyConfiguration
saSimpleScalingPolicyConfiguration = Lens.field @"simpleScalingPolicyConfiguration"
{-# DEPRECATED saSimpleScalingPolicyConfiguration "Use generic-lens or generic-optics with 'simpleScalingPolicyConfiguration' instead." #-}

-- | Not available for instance groups. Instance groups use the market type specified for the group.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saMarket :: Lens.Lens' ScalingAction (Core.Maybe Types.MarketType)
saMarket = Lens.field @"market"
{-# DEPRECATED saMarket "Use generic-lens or generic-optics with 'market' instead." #-}

instance Core.FromJSON ScalingAction where
  toJSON ScalingAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SimpleScalingPolicyConfiguration"
                  Core..= simpleScalingPolicyConfiguration
              ),
            ("Market" Core..=) Core.<$> market
          ]
      )

instance Core.FromJSON ScalingAction where
  parseJSON =
    Core.withObject "ScalingAction" Core.$
      \x ->
        ScalingAction'
          Core.<$> (x Core..: "SimpleScalingPolicyConfiguration")
          Core.<*> (x Core..:? "Market")
