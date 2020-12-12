{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSubnetGroup
  ( ClusterSubnetGroup (..),

    -- * Smart constructor
    mkClusterSubnetGroup,

    -- * Lenses
    csgVPCId,
    csgSubnets,
    csgClusterSubnetGroupName,
    csgSubnetGroupStatus,
    csgDescription,
    csgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.Tag

-- | Describes a subnet group.
--
-- /See:/ 'mkClusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { vpcId ::
      Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Subnet],
    clusterSubnetGroupName :: Lude.Maybe Lude.Text,
    subnetGroupStatus :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSubnetGroup' with the minimum fields required to make a request.
--
-- * 'clusterSubnetGroupName' - The name of the cluster subnet group.
-- * 'description' - The description of the cluster subnet group.
-- * 'subnetGroupStatus' - The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
-- * 'subnets' - A list of the VPC 'Subnet' elements.
-- * 'tags' - The list of tags for the cluster subnet group.
-- * 'vpcId' - The VPC ID of the cluster subnet group.
mkClusterSubnetGroup ::
  ClusterSubnetGroup
mkClusterSubnetGroup =
  ClusterSubnetGroup'
    { vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      clusterSubnetGroupName = Lude.Nothing,
      subnetGroupStatus = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The VPC ID of the cluster subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVPCId :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe Lude.Text)
csgVPCId = Lens.lens (vpcId :: ClusterSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of the VPC 'Subnet' elements.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnets :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe [Subnet])
csgSubnets = Lens.lens (subnets :: ClusterSubnetGroup -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The name of the cluster subnet group.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgClusterSubnetGroupName :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe Lude.Text)
csgClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: ClusterSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetGroupStatus :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe Lude.Text)
csgSubnetGroupStatus = Lens.lens (subnetGroupStatus :: ClusterSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroupStatus = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgSubnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead." #-}

-- | The description of the cluster subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe Lude.Text)
csgDescription = Lens.lens (description :: ClusterSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The list of tags for the cluster subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTags :: Lens.Lens' ClusterSubnetGroup (Lude.Maybe [Tag])
csgTags = Lens.lens (tags :: ClusterSubnetGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ClusterSubnetGroup)
{-# DEPRECATED csgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ClusterSubnetGroup where
  parseXML x =
    ClusterSubnetGroup'
      Lude.<$> (x Lude..@? "VpcId")
      Lude.<*> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Subnet")
               )
      Lude.<*> (x Lude..@? "ClusterSubnetGroupName")
      Lude.<*> (x Lude..@? "SubnetGroupStatus")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
