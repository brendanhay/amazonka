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
-- Module      : Network.AWS.RDS.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSubnetGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.Subnet

-- | Contains the details of an Amazon RDS DB subnet group.
--
-- This data type is used as a response element in the
-- @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'newDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { -- | Provides the status of the DB subnet group.
    subnetGroupStatus :: Core.Maybe Core.Text,
    -- | The name of the DB subnet group.
    dbSubnetGroupName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the DB subnet group.
    dbSubnetGroupArn :: Core.Maybe Core.Text,
    -- | Provides the description of the DB subnet group.
    dbSubnetGroupDescription :: Core.Maybe Core.Text,
    -- | Contains a list of @Subnet@ elements.
    subnets :: Core.Maybe [Subnet],
    -- | Provides the VpcId of the DB subnet group.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroupStatus', 'dbSubnetGroup_subnetGroupStatus' - Provides the status of the DB subnet group.
--
-- 'dbSubnetGroupName', 'dbSubnetGroup_dbSubnetGroupName' - The name of the DB subnet group.
--
-- 'dbSubnetGroupArn', 'dbSubnetGroup_dbSubnetGroupArn' - The Amazon Resource Name (ARN) for the DB subnet group.
--
-- 'dbSubnetGroupDescription', 'dbSubnetGroup_dbSubnetGroupDescription' - Provides the description of the DB subnet group.
--
-- 'subnets', 'dbSubnetGroup_subnets' - Contains a list of @Subnet@ elements.
--
-- 'vpcId', 'dbSubnetGroup_vpcId' - Provides the VpcId of the DB subnet group.
newDBSubnetGroup ::
  DBSubnetGroup
newDBSubnetGroup =
  DBSubnetGroup'
    { subnetGroupStatus = Core.Nothing,
      dbSubnetGroupName = Core.Nothing,
      dbSubnetGroupArn = Core.Nothing,
      dbSubnetGroupDescription = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Provides the status of the DB subnet group.
dbSubnetGroup_subnetGroupStatus :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dbSubnetGroup_subnetGroupStatus = Lens.lens (\DBSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@DBSubnetGroup' {} a -> s {subnetGroupStatus = a} :: DBSubnetGroup)

-- | The name of the DB subnet group.
dbSubnetGroup_dbSubnetGroupName :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dbSubnetGroup_dbSubnetGroupName = Lens.lens (\DBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: DBSubnetGroup)

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dbSubnetGroup_dbSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dbSubnetGroup_dbSubnetGroupArn = Lens.lens (\DBSubnetGroup' {dbSubnetGroupArn} -> dbSubnetGroupArn) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupArn = a} :: DBSubnetGroup)

-- | Provides the description of the DB subnet group.
dbSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dbSubnetGroup_dbSubnetGroupDescription = Lens.lens (\DBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: DBSubnetGroup)

-- | Contains a list of @Subnet@ elements.
dbSubnetGroup_subnets :: Lens.Lens' DBSubnetGroup (Core.Maybe [Subnet])
dbSubnetGroup_subnets = Lens.lens (\DBSubnetGroup' {subnets} -> subnets) (\s@DBSubnetGroup' {} a -> s {subnets = a} :: DBSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | Provides the VpcId of the DB subnet group.
dbSubnetGroup_vpcId :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dbSubnetGroup_vpcId = Lens.lens (\DBSubnetGroup' {vpcId} -> vpcId) (\s@DBSubnetGroup' {} a -> s {vpcId = a} :: DBSubnetGroup)

instance Core.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Core.<$> (x Core..@? "SubnetGroupStatus")
      Core.<*> (x Core..@? "DBSubnetGroupName")
      Core.<*> (x Core..@? "DBSubnetGroupArn")
      Core.<*> (x Core..@? "DBSubnetGroupDescription")
      Core.<*> ( x Core..@? "Subnets" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Subnet")
               )
      Core.<*> (x Core..@? "VpcId")

instance Core.Hashable DBSubnetGroup

instance Core.NFData DBSubnetGroup
