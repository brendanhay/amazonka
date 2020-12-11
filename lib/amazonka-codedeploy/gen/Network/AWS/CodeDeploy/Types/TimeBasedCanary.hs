-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedCanary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedCanary
  ( TimeBasedCanary (..),

    -- * Smart constructor
    mkTimeBasedCanary,

    -- * Lenses
    tbcCanaryInterval,
    tbcCanaryPercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /See:/ 'mkTimeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { canaryInterval ::
      Lude.Maybe Lude.Int,
    canaryPercentage :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeBasedCanary' with the minimum fields required to make a request.
--
-- * 'canaryInterval' - The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
-- * 'canaryPercentage' - The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
mkTimeBasedCanary ::
  TimeBasedCanary
mkTimeBasedCanary =
  TimeBasedCanary'
    { canaryInterval = Lude.Nothing,
      canaryPercentage = Lude.Nothing
    }

-- | The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
--
-- /Note:/ Consider using 'canaryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbcCanaryInterval :: Lens.Lens' TimeBasedCanary (Lude.Maybe Lude.Int)
tbcCanaryInterval = Lens.lens (canaryInterval :: TimeBasedCanary -> Lude.Maybe Lude.Int) (\s a -> s {canaryInterval = a} :: TimeBasedCanary)
{-# DEPRECATED tbcCanaryInterval "Use generic-lens or generic-optics with 'canaryInterval' instead." #-}

-- | The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
--
-- /Note:/ Consider using 'canaryPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbcCanaryPercentage :: Lens.Lens' TimeBasedCanary (Lude.Maybe Lude.Int)
tbcCanaryPercentage = Lens.lens (canaryPercentage :: TimeBasedCanary -> Lude.Maybe Lude.Int) (\s a -> s {canaryPercentage = a} :: TimeBasedCanary)
{-# DEPRECATED tbcCanaryPercentage "Use generic-lens or generic-optics with 'canaryPercentage' instead." #-}

instance Lude.FromJSON TimeBasedCanary where
  parseJSON =
    Lude.withObject
      "TimeBasedCanary"
      ( \x ->
          TimeBasedCanary'
            Lude.<$> (x Lude..:? "canaryInterval")
            Lude.<*> (x Lude..:? "canaryPercentage")
      )

instance Lude.ToJSON TimeBasedCanary where
  toJSON TimeBasedCanary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("canaryInterval" Lude..=) Lude.<$> canaryInterval,
            ("canaryPercentage" Lude..=) Lude.<$> canaryPercentage
          ]
      )
