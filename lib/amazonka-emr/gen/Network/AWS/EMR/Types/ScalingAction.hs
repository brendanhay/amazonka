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
    saMarket,
    saSimpleScalingPolicyConfiguration,
  )
where

import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
-- /See:/ 'mkScalingAction' smart constructor.
data ScalingAction = ScalingAction'
  { market ::
      Lude.Maybe MarketType,
    simpleScalingPolicyConfiguration ::
      SimpleScalingPolicyConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingAction' with the minimum fields required to make a request.
--
-- * 'market' - Not available for instance groups. Instance groups use the market type specified for the group.
-- * 'simpleScalingPolicyConfiguration' - The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
mkScalingAction ::
  -- | 'simpleScalingPolicyConfiguration'
  SimpleScalingPolicyConfiguration ->
  ScalingAction
mkScalingAction pSimpleScalingPolicyConfiguration_ =
  ScalingAction'
    { market = Lude.Nothing,
      simpleScalingPolicyConfiguration =
        pSimpleScalingPolicyConfiguration_
    }

-- | Not available for instance groups. Instance groups use the market type specified for the group.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saMarket :: Lens.Lens' ScalingAction (Lude.Maybe MarketType)
saMarket = Lens.lens (market :: ScalingAction -> Lude.Maybe MarketType) (\s a -> s {market = a} :: ScalingAction)
{-# DEPRECATED saMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
-- /Note:/ Consider using 'simpleScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSimpleScalingPolicyConfiguration :: Lens.Lens' ScalingAction SimpleScalingPolicyConfiguration
saSimpleScalingPolicyConfiguration = Lens.lens (simpleScalingPolicyConfiguration :: ScalingAction -> SimpleScalingPolicyConfiguration) (\s a -> s {simpleScalingPolicyConfiguration = a} :: ScalingAction)
{-# DEPRECATED saSimpleScalingPolicyConfiguration "Use generic-lens or generic-optics with 'simpleScalingPolicyConfiguration' instead." #-}

instance Lude.FromJSON ScalingAction where
  parseJSON =
    Lude.withObject
      "ScalingAction"
      ( \x ->
          ScalingAction'
            Lude.<$> (x Lude..:? "Market")
            Lude.<*> (x Lude..: "SimpleScalingPolicyConfiguration")
      )

instance Lude.ToJSON ScalingAction where
  toJSON ScalingAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Market" Lude..=) Lude.<$> market,
            Lude.Just
              ( "SimpleScalingPolicyConfiguration"
                  Lude..= simpleScalingPolicyConfiguration
              )
          ]
      )
