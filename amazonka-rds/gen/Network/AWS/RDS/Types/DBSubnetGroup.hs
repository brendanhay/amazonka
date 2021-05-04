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
-- Module      : Network.AWS.RDS.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSubnetGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.Subnet

-- | Contains the details of an Amazon RDS DB subnet group.
--
-- This data type is used as a response element in the
-- @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'newDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { -- | Provides the status of the DB subnet group.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB subnet group.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB subnet group.
    dbSubnetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the description of the DB subnet group.
    dbSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @Subnet@ elements.
    subnets :: Prelude.Maybe [Subnet],
    -- | Provides the VpcId of the DB subnet group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { subnetGroupStatus = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      dbSubnetGroupArn = Prelude.Nothing,
      dbSubnetGroupDescription = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Provides the status of the DB subnet group.
dbSubnetGroup_subnetGroupStatus :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_subnetGroupStatus = Lens.lens (\DBSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@DBSubnetGroup' {} a -> s {subnetGroupStatus = a} :: DBSubnetGroup)

-- | The name of the DB subnet group.
dbSubnetGroup_dbSubnetGroupName :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupName = Lens.lens (\DBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: DBSubnetGroup)

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dbSubnetGroup_dbSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupArn = Lens.lens (\DBSubnetGroup' {dbSubnetGroupArn} -> dbSubnetGroupArn) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupArn = a} :: DBSubnetGroup)

-- | Provides the description of the DB subnet group.
dbSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupDescription = Lens.lens (\DBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: DBSubnetGroup)

-- | Contains a list of @Subnet@ elements.
dbSubnetGroup_subnets :: Lens.Lens' DBSubnetGroup (Prelude.Maybe [Subnet])
dbSubnetGroup_subnets = Lens.lens (\DBSubnetGroup' {subnets} -> subnets) (\s@DBSubnetGroup' {} a -> s {subnets = a} :: DBSubnetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Provides the VpcId of the DB subnet group.
dbSubnetGroup_vpcId :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_vpcId = Lens.lens (\DBSubnetGroup' {vpcId} -> vpcId) (\s@DBSubnetGroup' {} a -> s {vpcId = a} :: DBSubnetGroup)

instance Prelude.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Prelude.<$> (x Prelude..@? "SubnetGroupStatus")
      Prelude.<*> (x Prelude..@? "DBSubnetGroupName")
      Prelude.<*> (x Prelude..@? "DBSubnetGroupArn")
      Prelude.<*> (x Prelude..@? "DBSubnetGroupDescription")
      Prelude.<*> ( x Prelude..@? "Subnets" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Prelude..@? "VpcId")

instance Prelude.Hashable DBSubnetGroup

instance Prelude.NFData DBSubnetGroup
