{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetInstances
  ( TargetInstances (..),

    -- * Smart constructor
    mkTargetInstances,

    -- * Lenses
    tiAutoScalingGroups,
    tiEc2TagSet,
    tiTagFilters,
  )
where

import qualified Network.AWS.CodeDeploy.Types.AutoScalingGroupName as Types
import qualified Network.AWS.CodeDeploy.Types.EC2TagFilter as Types
import qualified Network.AWS.CodeDeploy.Types.EC2TagSet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the instances to be used in the replacement environment in a blue/green deployment.
--
-- /See:/ 'mkTargetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { -- | The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
    autoScalingGroups :: Core.Maybe [Types.AutoScalingGroupName],
    -- | Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
    ec2TagSet :: Core.Maybe Types.EC2TagSet,
    -- | The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
    tagFilters :: Core.Maybe [Types.EC2TagFilter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetInstances' value with any optional fields omitted.
mkTargetInstances ::
  TargetInstances
mkTargetInstances =
  TargetInstances'
    { autoScalingGroups = Core.Nothing,
      ec2TagSet = Core.Nothing,
      tagFilters = Core.Nothing
    }

-- | The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiAutoScalingGroups :: Lens.Lens' TargetInstances (Core.Maybe [Types.AutoScalingGroupName])
tiAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# DEPRECATED tiAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEc2TagSet :: Lens.Lens' TargetInstances (Core.Maybe Types.EC2TagSet)
tiEc2TagSet = Lens.field @"ec2TagSet"
{-# DEPRECATED tiEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTagFilters :: Lens.Lens' TargetInstances (Core.Maybe [Types.EC2TagFilter])
tiTagFilters = Lens.field @"tagFilters"
{-# DEPRECATED tiTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

instance Core.FromJSON TargetInstances where
  toJSON TargetInstances {..} =
    Core.object
      ( Core.catMaybes
          [ ("autoScalingGroups" Core..=) Core.<$> autoScalingGroups,
            ("ec2TagSet" Core..=) Core.<$> ec2TagSet,
            ("tagFilters" Core..=) Core.<$> tagFilters
          ]
      )

instance Core.FromJSON TargetInstances where
  parseJSON =
    Core.withObject "TargetInstances" Core.$
      \x ->
        TargetInstances'
          Core.<$> (x Core..:? "autoScalingGroups")
          Core.<*> (x Core..:? "ec2TagSet")
          Core.<*> (x Core..:? "tagFilters")
