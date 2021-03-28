{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
  ( ScalableTargetAction (..)
  -- * Smart constructor
  , mkScalableTargetAction
  -- * Lenses
  , staMaxCapacity
  , staMinCapacity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the minimum and maximum capacity for a scheduled action.
--
-- /See:/ 'mkScalableTargetAction' smart constructor.
data ScalableTargetAction = ScalableTargetAction'
  { maxCapacity :: Core.Maybe Core.Int
    -- ^ The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service quotas may impose lower limits. Each service has its own default quotas for the maximum capacity of the resource. If you want to specify a higher limit, you can request an increase. For more information, consult the documentation for that service. For information about the default quotas for each service, see <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas> in the /Amazon Web Services General Reference/ .
  , minCapacity :: Core.Maybe Core.Int
    -- ^ The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB clusters, EMR clusters, and custom resources. For all other resources, the minimum value allowed is 1.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalableTargetAction' value with any optional fields omitted.
mkScalableTargetAction
    :: ScalableTargetAction
mkScalableTargetAction
  = ScalableTargetAction'{maxCapacity = Core.Nothing,
                          minCapacity = Core.Nothing}

-- | The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service quotas may impose lower limits. Each service has its own default quotas for the maximum capacity of the resource. If you want to specify a higher limit, you can request an increase. For more information, consult the documentation for that service. For information about the default quotas for each service, see <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas> in the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staMaxCapacity :: Lens.Lens' ScalableTargetAction (Core.Maybe Core.Int)
staMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE staMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

-- | The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB clusters, EMR clusters, and custom resources. For all other resources, the minimum value allowed is 1.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staMinCapacity :: Lens.Lens' ScalableTargetAction (Core.Maybe Core.Int)
staMinCapacity = Lens.field @"minCapacity"
{-# INLINEABLE staMinCapacity #-}
{-# DEPRECATED minCapacity "Use generic-lens or generic-optics with 'minCapacity' instead"  #-}

instance Core.FromJSON ScalableTargetAction where
        toJSON ScalableTargetAction{..}
          = Core.object
              (Core.catMaybes
                 [("MaxCapacity" Core..=) Core.<$> maxCapacity,
                  ("MinCapacity" Core..=) Core.<$> minCapacity])

instance Core.FromJSON ScalableTargetAction where
        parseJSON
          = Core.withObject "ScalableTargetAction" Core.$
              \ x ->
                ScalableTargetAction' Core.<$>
                  (x Core..:? "MaxCapacity") Core.<*> x Core..:? "MinCapacity"
