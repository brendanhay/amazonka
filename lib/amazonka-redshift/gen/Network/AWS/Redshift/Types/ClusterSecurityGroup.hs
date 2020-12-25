{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroup
  ( ClusterSecurityGroup (..),

    -- * Smart constructor
    mkClusterSecurityGroup,

    -- * Lenses
    csgfClusterSecurityGroupName,
    csgfDescription,
    csgfEC2SecurityGroups,
    csgfIPRanges,
    csgfTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.EC2SecurityGroup as Types
import qualified Network.AWS.Redshift.Types.IPRange as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes a security group.
--
-- /See:/ 'mkClusterSecurityGroup' smart constructor.
data ClusterSecurityGroup = ClusterSecurityGroup'
  { -- | The name of the cluster security group to which the operation was applied.
    clusterSecurityGroupName :: Core.Maybe Types.String,
    -- | A description of the security group.
    description :: Core.Maybe Types.String,
    -- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
    eC2SecurityGroups :: Core.Maybe [Types.EC2SecurityGroup],
    -- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
    iPRanges :: Core.Maybe [Types.IPRange],
    -- | The list of tags for the cluster security group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSecurityGroup' value with any optional fields omitted.
mkClusterSecurityGroup ::
  ClusterSecurityGroup
mkClusterSecurityGroup =
  ClusterSecurityGroup'
    { clusterSecurityGroupName = Core.Nothing,
      description = Core.Nothing,
      eC2SecurityGroups = Core.Nothing,
      iPRanges = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the cluster security group to which the operation was applied.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfClusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroup (Core.Maybe Types.String)
csgfClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# DEPRECATED csgfClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | A description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDescription :: Lens.Lens' ClusterSecurityGroup (Core.Maybe Types.String)
csgfDescription = Lens.field @"description"
{-# DEPRECATED csgfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
--
-- /Note:/ Consider using 'eC2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfEC2SecurityGroups :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [Types.EC2SecurityGroup])
csgfEC2SecurityGroups = Lens.field @"eC2SecurityGroups"
{-# DEPRECATED csgfEC2SecurityGroups "Use generic-lens or generic-optics with 'eC2SecurityGroups' instead." #-}

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
--
-- /Note:/ Consider using 'iPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfIPRanges :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [Types.IPRange])
csgfIPRanges = Lens.field @"iPRanges"
{-# DEPRECATED csgfIPRanges "Use generic-lens or generic-optics with 'iPRanges' instead." #-}

-- | The list of tags for the cluster security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfTags :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [Types.Tag])
csgfTags = Lens.field @"tags"
{-# DEPRECATED csgfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ClusterSecurityGroup where
  parseXML x =
    ClusterSecurityGroup'
      Core.<$> (x Core..@? "ClusterSecurityGroupName")
      Core.<*> (x Core..@? "Description")
      Core.<*> ( x Core..@? "EC2SecurityGroups"
                   Core..<@> Core.parseXMLList "EC2SecurityGroup"
               )
      Core.<*> (x Core..@? "IPRanges" Core..<@> Core.parseXMLList "IPRange")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
