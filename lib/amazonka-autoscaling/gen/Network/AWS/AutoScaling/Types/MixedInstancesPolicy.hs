{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MixedInstancesPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MixedInstancesPolicy
  ( MixedInstancesPolicy (..)
  -- * Smart constructor
  , mkMixedInstancesPolicy
  -- * Lenses
  , mipInstancesDistribution
  , mipLaunchTemplate
  ) where

import qualified Network.AWS.AutoScaling.Types.InstancesDistribution as Types
import qualified Network.AWS.AutoScaling.Types.LaunchTemplate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a mixed instances policy for an Auto Scaling group. With mixed instances, your Auto Scaling group can provision a combination of On-Demand Instances and Spot Instances across multiple instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- You can create a mixed instances policy for a new Auto Scaling group, or you can create it for an existing group by updating the group to specify @MixedInstancesPolicy@ as the top-level parameter instead of a launch configuration or launch template.
--
-- /See:/ 'mkMixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'
  { instancesDistribution :: Core.Maybe Types.InstancesDistribution
    -- ^ Specifies the instances distribution. If not provided, the value for each parameter in @InstancesDistribution@ uses a default value.
  , launchTemplate :: Core.Maybe Types.LaunchTemplate
    -- ^ Specifies the launch template to use and optionally the instance types (overrides) that are used to provision EC2 instances to fulfill On-Demand and Spot capacities. Required when creating a mixed instances policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MixedInstancesPolicy' value with any optional fields omitted.
mkMixedInstancesPolicy
    :: MixedInstancesPolicy
mkMixedInstancesPolicy
  = MixedInstancesPolicy'{instancesDistribution = Core.Nothing,
                          launchTemplate = Core.Nothing}

-- | Specifies the instances distribution. If not provided, the value for each parameter in @InstancesDistribution@ uses a default value.
--
-- /Note:/ Consider using 'instancesDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipInstancesDistribution :: Lens.Lens' MixedInstancesPolicy (Core.Maybe Types.InstancesDistribution)
mipInstancesDistribution = Lens.field @"instancesDistribution"
{-# INLINEABLE mipInstancesDistribution #-}
{-# DEPRECATED instancesDistribution "Use generic-lens or generic-optics with 'instancesDistribution' instead"  #-}

-- | Specifies the launch template to use and optionally the instance types (overrides) that are used to provision EC2 instances to fulfill On-Demand and Spot capacities. Required when creating a mixed instances policy.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipLaunchTemplate :: Lens.Lens' MixedInstancesPolicy (Core.Maybe Types.LaunchTemplate)
mipLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE mipLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

instance Core.ToQuery MixedInstancesPolicy where
        toQuery MixedInstancesPolicy{..}
          = Core.maybe Core.mempty (Core.toQueryPair "InstancesDistribution")
              instancesDistribution
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplate")
                launchTemplate

instance Core.FromXML MixedInstancesPolicy where
        parseXML x
          = MixedInstancesPolicy' Core.<$>
              (x Core..@? "InstancesDistribution") Core.<*>
                x Core..@? "LaunchTemplate"
