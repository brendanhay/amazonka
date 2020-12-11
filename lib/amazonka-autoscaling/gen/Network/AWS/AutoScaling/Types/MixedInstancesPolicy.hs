-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MixedInstancesPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MixedInstancesPolicy
  ( MixedInstancesPolicy (..),

    -- * Smart constructor
    mkMixedInstancesPolicy,

    -- * Lenses
    mipLaunchTemplate,
    mipInstancesDistribution,
  )
where

import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchTemplate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a mixed instances policy for an Auto Scaling group. With mixed instances, your Auto Scaling group can provision a combination of On-Demand Instances and Spot Instances across multiple instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- You can create a mixed instances policy for a new Auto Scaling group, or you can create it for an existing group by updating the group to specify @MixedInstancesPolicy@ as the top-level parameter instead of a launch configuration or launch template.
--
-- /See:/ 'mkMixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'
  { launchTemplate ::
      Lude.Maybe LaunchTemplate,
    instancesDistribution ::
      Lude.Maybe InstancesDistribution
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MixedInstancesPolicy' with the minimum fields required to make a request.
--
-- * 'instancesDistribution' - Specifies the instances distribution. If not provided, the value for each parameter in @InstancesDistribution@ uses a default value.
-- * 'launchTemplate' - Specifies the launch template to use and optionally the instance types (overrides) that are used to provision EC2 instances to fulfill On-Demand and Spot capacities. Required when creating a mixed instances policy.
mkMixedInstancesPolicy ::
  MixedInstancesPolicy
mkMixedInstancesPolicy =
  MixedInstancesPolicy'
    { launchTemplate = Lude.Nothing,
      instancesDistribution = Lude.Nothing
    }

-- | Specifies the launch template to use and optionally the instance types (overrides) that are used to provision EC2 instances to fulfill On-Demand and Spot capacities. Required when creating a mixed instances policy.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipLaunchTemplate :: Lens.Lens' MixedInstancesPolicy (Lude.Maybe LaunchTemplate)
mipLaunchTemplate = Lens.lens (launchTemplate :: MixedInstancesPolicy -> Lude.Maybe LaunchTemplate) (\s a -> s {launchTemplate = a} :: MixedInstancesPolicy)
{-# DEPRECATED mipLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | Specifies the instances distribution. If not provided, the value for each parameter in @InstancesDistribution@ uses a default value.
--
-- /Note:/ Consider using 'instancesDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipInstancesDistribution :: Lens.Lens' MixedInstancesPolicy (Lude.Maybe InstancesDistribution)
mipInstancesDistribution = Lens.lens (instancesDistribution :: MixedInstancesPolicy -> Lude.Maybe InstancesDistribution) (\s a -> s {instancesDistribution = a} :: MixedInstancesPolicy)
{-# DEPRECATED mipInstancesDistribution "Use generic-lens or generic-optics with 'instancesDistribution' instead." #-}

instance Lude.FromXML MixedInstancesPolicy where
  parseXML x =
    MixedInstancesPolicy'
      Lude.<$> (x Lude..@? "LaunchTemplate")
      Lude.<*> (x Lude..@? "InstancesDistribution")

instance Lude.ToQuery MixedInstancesPolicy where
  toQuery MixedInstancesPolicy' {..} =
    Lude.mconcat
      [ "LaunchTemplate" Lude.=: launchTemplate,
        "InstancesDistribution" Lude.=: instancesDistribution
      ]
