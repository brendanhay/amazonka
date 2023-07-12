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
-- Module      : Amazonka.Redshift.Types.ClusterSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterSubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Subnet
import Amazonka.Redshift.Types.Tag

-- | Describes a subnet group.
--
-- /See:/ 'newClusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { -- | The name of the cluster subnet group.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The description of the cluster subnet group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster subnet group. Possible values are @Complete@,
    -- @Incomplete@ and @Invalid@.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of the VPC Subnet elements.
    subnets :: Prelude.Maybe [Subnet],
    -- | The list of tags for the cluster subnet group.
    tags :: Prelude.Maybe [Tag],
    -- | The VPC ID of the cluster subnet group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'clusterSubnetGroup_description' - The description of the cluster subnet group.
--
-- 'subnetGroupStatus', 'clusterSubnetGroup_subnetGroupStatus' - The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
--
-- 'subnets', 'clusterSubnetGroup_subnets' - A list of the VPC Subnet elements.
--
-- 'tags', 'clusterSubnetGroup_tags' - The list of tags for the cluster subnet group.
--
-- 'vpcId', 'clusterSubnetGroup_vpcId' - The VPC ID of the cluster subnet group.
newClusterSubnetGroup ::
  ClusterSubnetGroup
newClusterSubnetGroup =
  ClusterSubnetGroup'
    { clusterSubnetGroupName =
        Prelude.Nothing,
      description = Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing,
      subnets = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The name of the cluster subnet group.
clusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\ClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@ClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: ClusterSubnetGroup)

-- | The description of the cluster subnet group.
clusterSubnetGroup_description :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_description = Lens.lens (\ClusterSubnetGroup' {description} -> description) (\s@ClusterSubnetGroup' {} a -> s {description = a} :: ClusterSubnetGroup)

-- | The status of the cluster subnet group. Possible values are @Complete@,
-- @Incomplete@ and @Invalid@.
clusterSubnetGroup_subnetGroupStatus :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_subnetGroupStatus = Lens.lens (\ClusterSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@ClusterSubnetGroup' {} a -> s {subnetGroupStatus = a} :: ClusterSubnetGroup)

-- | A list of the VPC Subnet elements.
clusterSubnetGroup_subnets :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe [Subnet])
clusterSubnetGroup_subnets = Lens.lens (\ClusterSubnetGroup' {subnets} -> subnets) (\s@ClusterSubnetGroup' {} a -> s {subnets = a} :: ClusterSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The list of tags for the cluster subnet group.
clusterSubnetGroup_tags :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe [Tag])
clusterSubnetGroup_tags = Lens.lens (\ClusterSubnetGroup' {tags} -> tags) (\s@ClusterSubnetGroup' {} a -> s {tags = a} :: ClusterSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID of the cluster subnet group.
clusterSubnetGroup_vpcId :: Lens.Lens' ClusterSubnetGroup (Prelude.Maybe Prelude.Text)
clusterSubnetGroup_vpcId = Lens.lens (\ClusterSubnetGroup' {vpcId} -> vpcId) (\s@ClusterSubnetGroup' {} a -> s {vpcId = a} :: ClusterSubnetGroup)

instance Data.FromXML ClusterSubnetGroup where
  parseXML x =
    ClusterSubnetGroup'
      Prelude.<$> (x Data..@? "ClusterSubnetGroupName")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "SubnetGroupStatus")
      Prelude.<*> ( x
                      Data..@? "Subnets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Subnet")
                  )
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "VpcId")

instance Prelude.Hashable ClusterSubnetGroup where
  hashWithSalt _salt ClusterSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` subnetGroupStatus
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ClusterSubnetGroup where
  rnf ClusterSubnetGroup' {..} =
    Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf subnetGroupStatus
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
