{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.IPRange
import Network.AWS.Redshift.Types.Tag

-- | Describes a security group.
--
-- /See:/ 'newClusterSecurityGroup' smart constructor.
data ClusterSecurityGroup = ClusterSecurityGroup'
  { -- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
    -- associated with this cluster security group.
    iPRanges :: Core.Maybe [IPRange],
    -- | The name of the cluster security group to which the operation was
    -- applied.
    clusterSecurityGroupName :: Core.Maybe Core.Text,
    -- | The list of tags for the cluster security group.
    tags :: Core.Maybe [Tag],
    -- | A list of EC2 security groups that are permitted to access clusters
    -- associated with this cluster security group.
    eC2SecurityGroups :: Core.Maybe [EC2SecurityGroup],
    -- | A description of the security group.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPRanges', 'clusterSecurityGroup_iPRanges' - A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
--
-- 'clusterSecurityGroupName', 'clusterSecurityGroup_clusterSecurityGroupName' - The name of the cluster security group to which the operation was
-- applied.
--
-- 'tags', 'clusterSecurityGroup_tags' - The list of tags for the cluster security group.
--
-- 'eC2SecurityGroups', 'clusterSecurityGroup_eC2SecurityGroups' - A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
--
-- 'description', 'clusterSecurityGroup_description' - A description of the security group.
newClusterSecurityGroup ::
  ClusterSecurityGroup
newClusterSecurityGroup =
  ClusterSecurityGroup'
    { iPRanges = Core.Nothing,
      clusterSecurityGroupName = Core.Nothing,
      tags = Core.Nothing,
      eC2SecurityGroups = Core.Nothing,
      description = Core.Nothing
    }

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
clusterSecurityGroup_iPRanges :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [IPRange])
clusterSecurityGroup_iPRanges = Lens.lens (\ClusterSecurityGroup' {iPRanges} -> iPRanges) (\s@ClusterSecurityGroup' {} a -> s {iPRanges = a} :: ClusterSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the cluster security group to which the operation was
-- applied.
clusterSecurityGroup_clusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroup (Core.Maybe Core.Text)
clusterSecurityGroup_clusterSecurityGroupName = Lens.lens (\ClusterSecurityGroup' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@ClusterSecurityGroup' {} a -> s {clusterSecurityGroupName = a} :: ClusterSecurityGroup)

-- | The list of tags for the cluster security group.
clusterSecurityGroup_tags :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [Tag])
clusterSecurityGroup_tags = Lens.lens (\ClusterSecurityGroup' {tags} -> tags) (\s@ClusterSecurityGroup' {} a -> s {tags = a} :: ClusterSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
clusterSecurityGroup_eC2SecurityGroups :: Lens.Lens' ClusterSecurityGroup (Core.Maybe [EC2SecurityGroup])
clusterSecurityGroup_eC2SecurityGroups = Lens.lens (\ClusterSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@ClusterSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: ClusterSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | A description of the security group.
clusterSecurityGroup_description :: Lens.Lens' ClusterSecurityGroup (Core.Maybe Core.Text)
clusterSecurityGroup_description = Lens.lens (\ClusterSecurityGroup' {description} -> description) (\s@ClusterSecurityGroup' {} a -> s {description = a} :: ClusterSecurityGroup)

instance Core.FromXML ClusterSecurityGroup where
  parseXML x =
    ClusterSecurityGroup'
      Core.<$> ( x Core..@? "IPRanges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "IPRange")
               )
      Core.<*> (x Core..@? "ClusterSecurityGroupName")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> ( x Core..@? "EC2SecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EC2SecurityGroup")
               )
      Core.<*> (x Core..@? "Description")

instance Core.Hashable ClusterSecurityGroup

instance Core.NFData ClusterSecurityGroup
