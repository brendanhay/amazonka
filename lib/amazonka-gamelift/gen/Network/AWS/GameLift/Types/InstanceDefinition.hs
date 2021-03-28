{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.InstanceDefinition
  ( InstanceDefinition (..)
  -- * Smart constructor
  , mkInstanceDefinition
  -- * Lenses
  , idInstanceType
  , idWeightedCapacity
  ) where

import qualified Network.AWS.GameLift.Types.GameServerGroupInstanceType as Types
import qualified Network.AWS.GameLift.Types.WeightedCapacity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__ 
--
-- An allowed instance type for a 'GameServerGroup' . All game server groups must have at least two instance types defined for it. GameLift FleetIQ periodically evaluates each defined instance type for viability. It then updates the Auto Scaling group with the list of viable instance types.
--
-- /See:/ 'mkInstanceDefinition' smart constructor.
data InstanceDefinition = InstanceDefinition'
  { instanceType :: Types.GameServerGroupInstanceType
    -- ^ An EC2 instance type designation.
  , weightedCapacity :: Core.Maybe Types.WeightedCapacity
    -- ^ Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceDefinition' value with any optional fields omitted.
mkInstanceDefinition
    :: Types.GameServerGroupInstanceType -- ^ 'instanceType'
    -> InstanceDefinition
mkInstanceDefinition instanceType
  = InstanceDefinition'{instanceType,
                        weightedCapacity = Core.Nothing}

-- | An EC2 instance type designation.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceType :: Lens.Lens' InstanceDefinition Types.GameServerGroupInstanceType
idInstanceType = Lens.field @"instanceType"
{-# INLINEABLE idInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idWeightedCapacity :: Lens.Lens' InstanceDefinition (Core.Maybe Types.WeightedCapacity)
idWeightedCapacity = Lens.field @"weightedCapacity"
{-# INLINEABLE idWeightedCapacity #-}
{-# DEPRECATED weightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead"  #-}

instance Core.FromJSON InstanceDefinition where
        toJSON InstanceDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceType" Core..= instanceType),
                  ("WeightedCapacity" Core..=) Core.<$> weightedCapacity])

instance Core.FromJSON InstanceDefinition where
        parseJSON
          = Core.withObject "InstanceDefinition" Core.$
              \ x ->
                InstanceDefinition' Core.<$>
                  (x Core..: "InstanceType") Core.<*> x Core..:? "WeightedCapacity"
