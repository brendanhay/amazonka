{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.Tag

-- | Describes a subnet group.
--
-- /See:/ 'newClusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { -- | The name of the cluster subnet group.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster subnet group. Possible values are @Complete@,
    -- @Incomplete@ and @Invalid@.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the cluster subnet group.
    tags :: Prelude.Maybe [Tag],
    -- | The description of the cluster subnet group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of the VPC Subnet elements.
    subnets :: Prelude.Maybe [Subnet],
    -- | The VPC ID of the cluster subnet group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The name of the cluster subnet group.
clusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\ClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@ClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: ClusterSubnetGroup)

-- | The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
clusterSubnetGroup_subnetGroupStatus :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_subnetGroupStatus = Lens.lens (\ClusterSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@ClusterSubnetGroup' {} a -> s {subnetGroupStatus = a} :: ClusterSubnetGroup)

-- | The list of tags for the cluster subnet group.
clusterSubnetGroup_tags :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe [Tag])
clusterSubnetGroup_tags = Lens.lens (\ClusterSubnetGroup' {tags} -> tags) (\s@ClusterSubnetGroup' {} a -> s {tags = a} :: ClusterSubnetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the cluster subnet group.
clusterSubnetGroup_description :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_description = Lens.lens (\ClusterSubnetGroup' {description} -> description) (\s@ClusterSubnetGroup' {} a -> s {description = a} :: ClusterSubnetGroup)

-- | A list of the VPC Subnet elements.
clusterSubnetGroup_subnets :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe [Subnet])
clusterSubnetGroup_subnets = Lens.lens (\ClusterSubnetGroup' {subnets} -> subnets) (\s@ClusterSubnetGroup' {} a -> s {subnets = a} :: ClusterSubnetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The VPC ID of the cluster subnet group.
clusterSubnetGroup_vpcId :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_vpcId = Lens.lens (\ClusterSubnetGroup' {vpcId} -> vpcId) (\s@ClusterSubnetGroup' {} a -> s {vpcId = a} :: ClusterSubnetGroup)

instance Prelude.FromXML ClusterSubnetGroup where
  parseXML x =
    ClusterSubnetGroup'
      Prelude.<$> (x Prelude..@? "ClusterSubnetGroupName")
      Prelude.<*> (x Prelude..@? "SubnetGroupStatus")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> ( x Prelude..@? "Subnets" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Prelude..@? "VpcId")

instance Prelude.Hashable ClusterSubnetGroup

instance Prelude.NFData ClusterSubnetGroup
