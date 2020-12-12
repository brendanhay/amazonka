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
    tiEc2TagSet,
    tiTagFilters,
    tiAutoScalingGroups,
  )
where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the instances to be used in the replacement environment in a blue/green deployment.
--
-- /See:/ 'mkTargetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { ec2TagSet ::
      Lude.Maybe EC2TagSet,
    tagFilters :: Lude.Maybe [EC2TagFilter],
    autoScalingGroups :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetInstances' with the minimum fields required to make a request.
--
-- * 'autoScalingGroups' - The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
-- * 'ec2TagSet' - Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
-- * 'tagFilters' - The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
mkTargetInstances ::
  TargetInstances
mkTargetInstances =
  TargetInstances'
    { ec2TagSet = Lude.Nothing,
      tagFilters = Lude.Nothing,
      autoScalingGroups = Lude.Nothing
    }

-- | Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEc2TagSet :: Lens.Lens' TargetInstances (Lude.Maybe EC2TagSet)
tiEc2TagSet = Lens.lens (ec2TagSet :: TargetInstances -> Lude.Maybe EC2TagSet) (\s a -> s {ec2TagSet = a} :: TargetInstances)
{-# DEPRECATED tiEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTagFilters :: Lens.Lens' TargetInstances (Lude.Maybe [EC2TagFilter])
tiTagFilters = Lens.lens (tagFilters :: TargetInstances -> Lude.Maybe [EC2TagFilter]) (\s a -> s {tagFilters = a} :: TargetInstances)
{-# DEPRECATED tiTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

-- | The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiAutoScalingGroups :: Lens.Lens' TargetInstances (Lude.Maybe [Lude.Text])
tiAutoScalingGroups = Lens.lens (autoScalingGroups :: TargetInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroups = a} :: TargetInstances)
{-# DEPRECATED tiAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

instance Lude.FromJSON TargetInstances where
  parseJSON =
    Lude.withObject
      "TargetInstances"
      ( \x ->
          TargetInstances'
            Lude.<$> (x Lude..:? "ec2TagSet")
            Lude.<*> (x Lude..:? "tagFilters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "autoScalingGroups" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TargetInstances where
  toJSON TargetInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ec2TagSet" Lude..=) Lude.<$> ec2TagSet,
            ("tagFilters" Lude..=) Lude.<$> tagFilters,
            ("autoScalingGroups" Lude..=) Lude.<$> autoScalingGroups
          ]
      )
