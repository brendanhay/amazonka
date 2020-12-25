{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /See:/ 'mkTimeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { -- | The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
    canaryInterval :: Core.Maybe Core.Int,
    -- | The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
    canaryPercentage :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimeBasedCanary' value with any optional fields omitted.
mkTimeBasedCanary ::
  TimeBasedCanary
mkTimeBasedCanary =
  TimeBasedCanary'
    { canaryInterval = Core.Nothing,
      canaryPercentage = Core.Nothing
    }

-- | The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
--
-- /Note:/ Consider using 'canaryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbcCanaryInterval :: Lens.Lens' TimeBasedCanary (Core.Maybe Core.Int)
tbcCanaryInterval = Lens.field @"canaryInterval"
{-# DEPRECATED tbcCanaryInterval "Use generic-lens or generic-optics with 'canaryInterval' instead." #-}

-- | The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
--
-- /Note:/ Consider using 'canaryPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbcCanaryPercentage :: Lens.Lens' TimeBasedCanary (Core.Maybe Core.Int)
tbcCanaryPercentage = Lens.field @"canaryPercentage"
{-# DEPRECATED tbcCanaryPercentage "Use generic-lens or generic-optics with 'canaryPercentage' instead." #-}

instance Core.FromJSON TimeBasedCanary where
  toJSON TimeBasedCanary {..} =
    Core.object
      ( Core.catMaybes
          [ ("canaryInterval" Core..=) Core.<$> canaryInterval,
            ("canaryPercentage" Core..=) Core.<$> canaryPercentage
          ]
      )

instance Core.FromJSON TimeBasedCanary where
  parseJSON =
    Core.withObject "TimeBasedCanary" Core.$
      \x ->
        TimeBasedCanary'
          Core.<$> (x Core..:? "canaryInterval")
          Core.<*> (x Core..:? "canaryPercentage")
