{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedLinear
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TimeBasedLinear
  ( TimeBasedLinear (..)
  -- * Smart constructor
  , mkTimeBasedLinear
  -- * Lenses
  , tblLinearInterval
  , tblLinearPercentage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /See:/ 'mkTimeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { linearInterval :: Core.Maybe Core.Int
    -- ^ The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
  , linearPercentage :: Core.Maybe Core.Int
    -- ^ The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimeBasedLinear' value with any optional fields omitted.
mkTimeBasedLinear
    :: TimeBasedLinear
mkTimeBasedLinear
  = TimeBasedLinear'{linearInterval = Core.Nothing,
                     linearPercentage = Core.Nothing}

-- | The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
--
-- /Note:/ Consider using 'linearInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tblLinearInterval :: Lens.Lens' TimeBasedLinear (Core.Maybe Core.Int)
tblLinearInterval = Lens.field @"linearInterval"
{-# INLINEABLE tblLinearInterval #-}
{-# DEPRECATED linearInterval "Use generic-lens or generic-optics with 'linearInterval' instead"  #-}

-- | The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
--
-- /Note:/ Consider using 'linearPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tblLinearPercentage :: Lens.Lens' TimeBasedLinear (Core.Maybe Core.Int)
tblLinearPercentage = Lens.field @"linearPercentage"
{-# INLINEABLE tblLinearPercentage #-}
{-# DEPRECATED linearPercentage "Use generic-lens or generic-optics with 'linearPercentage' instead"  #-}

instance Core.FromJSON TimeBasedLinear where
        toJSON TimeBasedLinear{..}
          = Core.object
              (Core.catMaybes
                 [("linearInterval" Core..=) Core.<$> linearInterval,
                  ("linearPercentage" Core..=) Core.<$> linearPercentage])

instance Core.FromJSON TimeBasedLinear where
        parseJSON
          = Core.withObject "TimeBasedLinear" Core.$
              \ x ->
                TimeBasedLinear' Core.<$>
                  (x Core..:? "linearInterval") Core.<*>
                    x Core..:? "linearPercentage"
