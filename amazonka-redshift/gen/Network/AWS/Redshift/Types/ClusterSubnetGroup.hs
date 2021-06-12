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
-- Module      : Network.AWS.Redshift.Types.ClusterSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSubnetGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.Tag

-- | Describes a subnet group.
--
-- /See:/ 'newClusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { -- | The name of the cluster subnet group.
    clusterSubnetGroupName :: Core.Maybe Core.Text,
    -- | The status of the cluster subnet group. Possible values are @Complete@,
    -- @Incomplete@ and @Invalid@.
    subnetGroupStatus :: Core.Maybe Core.Text,
    -- | The list of tags for the cluster subnet group.
    tags :: Core.Maybe [Tag],
    -- | The description of the cluster subnet group.
    description :: Core.Maybe Core.Text,
    -- | A list of the VPC Subnet elements.
    subnets :: Core.Maybe [Subnet],
    -- | The VPC ID of the cluster subnet group.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroupName', 'clusterSubnetGroup_clusterSubnetGroupName' - The name of the cluster subnet group.
--
-- 'subnetGroupStatus', 'clusterSubnetGroup_subnetGroupStatus' - The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
--
-- 'tags', 'clusterSubnetGroup_tags' - The list of tags for the cluster subnet group.
--
-- 'description', 'clusterSubnetGroup_description' - The description of the cluster subnet group.
--
-- 'subnets', 'clusterSubnetGroup_subnets' - A list of the VPC Subnet elements.
--
-- 'vpcId', 'clusterSubnetGroup_vpcId' - The VPC ID of the cluster subnet group.
newClusterSubnetGroup ::
  ClusterSubnetGroup
newClusterSubnetGroup =
  ClusterSubnetGroup'
    { clusterSubnetGroupName =
        Core.Nothing,
      subnetGroupStatus = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The name of the cluster subnet group.
clusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
clusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\ClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@ClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: ClusterSubnetGroup)

-- | The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
clusterSubnetGroup_subnetGroupStatus :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
clusterSubnetGroup_subnetGroupStatus = Lens.lens (\ClusterSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@ClusterSubnetGroup' {} a -> s {subnetGroupStatus = a} :: ClusterSubnetGroup)

-- | The list of tags for the cluster subnet group.
clusterSubnetGroup_tags :: Lens.Lens' ClusterSubnetGroup (Core.Maybe [Tag])
clusterSubnetGroup_tags = Lens.lens (\ClusterSubnetGroup' {tags} -> tags) (\s@ClusterSubnetGroup' {} a -> s {tags = a} :: ClusterSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the cluster subnet group.
clusterSubnetGroup_description :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
clusterSubnetGroup_description = Lens.lens (\ClusterSubnetGroup' {description} -> description) (\s@ClusterSubnetGroup' {} a -> s {description = a} :: ClusterSubnetGroup)

-- | A list of the VPC Subnet elements.
clusterSubnetGroup_subnets :: Lens.Lens' ClusterSubnetGroup (Core.Maybe [Subnet])
clusterSubnetGroup_subnets = Lens.lens (\ClusterSubnetGroup' {subnets} -> subnets) (\s@ClusterSubnetGroup' {} a -> s {subnets = a} :: ClusterSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The VPC ID of the cluster subnet group.
clusterSubnetGroup_vpcId :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
clusterSubnetGroup_vpcId = Lens.lens (\ClusterSubnetGroup' {vpcId} -> vpcId) (\s@ClusterSubnetGroup' {} a -> s {vpcId = a} :: ClusterSubnetGroup)

instance Core.FromXML ClusterSubnetGroup where
  parseXML x =
    ClusterSubnetGroup'
      Core.<$> (x Core..@? "ClusterSubnetGroupName")
      Core.<*> (x Core..@? "SubnetGroupStatus")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "Description")
      Core.<*> ( x Core..@? "Subnets" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Subnet")
               )
      Core.<*> (x Core..@? "VpcId")

instance Core.Hashable ClusterSubnetGroup

instance Core.NFData ClusterSubnetGroup
