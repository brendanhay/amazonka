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
    csgfIPRanges,
    csgfEC2SecurityGroups,
    csgfDescription,
    csgfTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.IPRange
import Network.AWS.Redshift.Types.Tag

-- | Describes a security group.
--
-- /See:/ 'mkClusterSecurityGroup' smart constructor.
data ClusterSecurityGroup = ClusterSecurityGroup'
  { -- | The name of the cluster security group to which the operation was applied.
    clusterSecurityGroupName :: Lude.Maybe Lude.Text,
    -- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
    ipRanges :: Lude.Maybe [IPRange],
    -- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
    ec2SecurityGroups :: Lude.Maybe [EC2SecurityGroup],
    -- | A description of the security group.
    description :: Lude.Maybe Lude.Text,
    -- | The list of tags for the cluster security group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSecurityGroup' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroupName' - The name of the cluster security group to which the operation was applied.
-- * 'ipRanges' - A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
-- * 'ec2SecurityGroups' - A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
-- * 'description' - A description of the security group.
-- * 'tags' - The list of tags for the cluster security group.
mkClusterSecurityGroup ::
  ClusterSecurityGroup
mkClusterSecurityGroup =
  ClusterSecurityGroup'
    { clusterSecurityGroupName = Lude.Nothing,
      ipRanges = Lude.Nothing,
      ec2SecurityGroups = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the cluster security group to which the operation was applied.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfClusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroup (Lude.Maybe Lude.Text)
csgfClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: ClusterSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: ClusterSecurityGroup)
{-# DEPRECATED csgfClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfIPRanges :: Lens.Lens' ClusterSecurityGroup (Lude.Maybe [IPRange])
csgfIPRanges = Lens.lens (ipRanges :: ClusterSecurityGroup -> Lude.Maybe [IPRange]) (\s a -> s {ipRanges = a} :: ClusterSecurityGroup)
{-# DEPRECATED csgfIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

-- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
--
-- /Note:/ Consider using 'ec2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfEC2SecurityGroups :: Lens.Lens' ClusterSecurityGroup (Lude.Maybe [EC2SecurityGroup])
csgfEC2SecurityGroups = Lens.lens (ec2SecurityGroups :: ClusterSecurityGroup -> Lude.Maybe [EC2SecurityGroup]) (\s a -> s {ec2SecurityGroups = a} :: ClusterSecurityGroup)
{-# DEPRECATED csgfEC2SecurityGroups "Use generic-lens or generic-optics with 'ec2SecurityGroups' instead." #-}

-- | A description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDescription :: Lens.Lens' ClusterSecurityGroup (Lude.Maybe Lude.Text)
csgfDescription = Lens.lens (description :: ClusterSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterSecurityGroup)
{-# DEPRECATED csgfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The list of tags for the cluster security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfTags :: Lens.Lens' ClusterSecurityGroup (Lude.Maybe [Tag])
csgfTags = Lens.lens (tags :: ClusterSecurityGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ClusterSecurityGroup)
{-# DEPRECATED csgfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ClusterSecurityGroup where
  parseXML x =
    ClusterSecurityGroup'
      Lude.<$> (x Lude..@? "ClusterSecurityGroupName")
      Lude.<*> ( x Lude..@? "IPRanges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "IPRange")
               )
      Lude.<*> ( x Lude..@? "EC2SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EC2SecurityGroup")
               )
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
