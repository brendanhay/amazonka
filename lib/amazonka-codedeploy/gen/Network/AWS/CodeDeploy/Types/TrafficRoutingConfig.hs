{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
  ( TrafficRoutingConfig (..)
  -- * Smart constructor
  , mkTrafficRoutingConfig
  -- * Lenses
  , trcTimeBasedCanary
  , trcTimeBasedLinear
  , trcType
  ) where

import qualified Network.AWS.CodeDeploy.Types.TimeBasedCanary as Types
import qualified Network.AWS.CodeDeploy.Types.TimeBasedLinear as Types
import qualified Network.AWS.CodeDeploy.Types.TrafficRoutingType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment, or from one Amazon ECS task set to another during an Amazon ECS deployment.
--
-- /See:/ 'mkTrafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { timeBasedCanary :: Core.Maybe Types.TimeBasedCanary
    -- ^ A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
  , timeBasedLinear :: Core.Maybe Types.TimeBasedLinear
    -- ^ A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
  , type' :: Core.Maybe Types.TrafficRoutingType
    -- ^ The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficRoutingConfig' value with any optional fields omitted.
mkTrafficRoutingConfig
    :: TrafficRoutingConfig
mkTrafficRoutingConfig
  = TrafficRoutingConfig'{timeBasedCanary = Core.Nothing,
                          timeBasedLinear = Core.Nothing, type' = Core.Nothing}

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /Note:/ Consider using 'timeBasedCanary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcTimeBasedCanary :: Lens.Lens' TrafficRoutingConfig (Core.Maybe Types.TimeBasedCanary)
trcTimeBasedCanary = Lens.field @"timeBasedCanary"
{-# INLINEABLE trcTimeBasedCanary #-}
{-# DEPRECATED timeBasedCanary "Use generic-lens or generic-optics with 'timeBasedCanary' instead"  #-}

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /Note:/ Consider using 'timeBasedLinear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcTimeBasedLinear :: Lens.Lens' TrafficRoutingConfig (Core.Maybe Types.TimeBasedLinear)
trcTimeBasedLinear = Lens.field @"timeBasedLinear"
{-# INLINEABLE trcTimeBasedLinear #-}
{-# DEPRECATED timeBasedLinear "Use generic-lens or generic-optics with 'timeBasedLinear' instead"  #-}

-- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcType :: Lens.Lens' TrafficRoutingConfig (Core.Maybe Types.TrafficRoutingType)
trcType = Lens.field @"type'"
{-# INLINEABLE trcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON TrafficRoutingConfig where
        toJSON TrafficRoutingConfig{..}
          = Core.object
              (Core.catMaybes
                 [("timeBasedCanary" Core..=) Core.<$> timeBasedCanary,
                  ("timeBasedLinear" Core..=) Core.<$> timeBasedLinear,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON TrafficRoutingConfig where
        parseJSON
          = Core.withObject "TrafficRoutingConfig" Core.$
              \ x ->
                TrafficRoutingConfig' Core.<$>
                  (x Core..:? "timeBasedCanary") Core.<*>
                    x Core..:? "timeBasedLinear"
                    Core.<*> x Core..:? "type"
