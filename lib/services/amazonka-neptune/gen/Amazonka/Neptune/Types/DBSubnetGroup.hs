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
-- Module      : Amazonka.Neptune.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBSubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.Subnet
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an Amazon Neptune DB subnet group.
--
-- This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'newDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { -- | The Amazon Resource Name (ARN) for the DB subnet group.
    dbSubnetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the description of the DB subnet group.
    dbSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB subnet group.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the DB subnet group.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of Subnet elements.
    subnets :: Prelude.Maybe [Subnet],
    -- | Provides the VpcId of the DB subnet group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupArn', 'dbSubnetGroup_dbSubnetGroupArn' - The Amazon Resource Name (ARN) for the DB subnet group.
--
-- 'dbSubnetGroupDescription', 'dbSubnetGroup_dbSubnetGroupDescription' - Provides the description of the DB subnet group.
--
-- 'dbSubnetGroupName', 'dbSubnetGroup_dbSubnetGroupName' - The name of the DB subnet group.
--
-- 'subnetGroupStatus', 'dbSubnetGroup_subnetGroupStatus' - Provides the status of the DB subnet group.
--
-- 'subnets', 'dbSubnetGroup_subnets' - Contains a list of Subnet elements.
--
-- 'vpcId', 'dbSubnetGroup_vpcId' - Provides the VpcId of the DB subnet group.
newDBSubnetGroup ::
  DBSubnetGroup
newDBSubnetGroup =
  DBSubnetGroup'
    { dbSubnetGroupArn = Prelude.Nothing,
      dbSubnetGroupDescription = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dbSubnetGroup_dbSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupArn = Lens.lens (\DBSubnetGroup' {dbSubnetGroupArn} -> dbSubnetGroupArn) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupArn = a} :: DBSubnetGroup)

-- | Provides the description of the DB subnet group.
dbSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupDescription = Lens.lens (\DBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: DBSubnetGroup)

-- | The name of the DB subnet group.
dbSubnetGroup_dbSubnetGroupName :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupName = Lens.lens (\DBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: DBSubnetGroup)

-- | Provides the status of the DB subnet group.
dbSubnetGroup_subnetGroupStatus :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_subnetGroupStatus = Lens.lens (\DBSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@DBSubnetGroup' {} a -> s {subnetGroupStatus = a} :: DBSubnetGroup)

-- | Contains a list of Subnet elements.
dbSubnetGroup_subnets :: Lens.Lens' DBSubnetGroup (Prelude.Maybe [Subnet])
dbSubnetGroup_subnets = Lens.lens (\DBSubnetGroup' {subnets} -> subnets) (\s@DBSubnetGroup' {} a -> s {subnets = a} :: DBSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | Provides the VpcId of the DB subnet group.
dbSubnetGroup_vpcId :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_vpcId = Lens.lens (\DBSubnetGroup' {vpcId} -> vpcId) (\s@DBSubnetGroup' {} a -> s {vpcId = a} :: DBSubnetGroup)

instance Data.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Prelude.<$> (x Data..@? "DBSubnetGroupArn")
      Prelude.<*> (x Data..@? "DBSubnetGroupDescription")
      Prelude.<*> (x Data..@? "DBSubnetGroupName")
      Prelude.<*> (x Data..@? "SubnetGroupStatus")
      Prelude.<*> ( x
                      Data..@? "Subnets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Data..@? "VpcId")

instance Prelude.Hashable DBSubnetGroup where
  hashWithSalt _salt DBSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbSubnetGroupArn
      `Prelude.hashWithSalt` dbSubnetGroupDescription
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` subnetGroupStatus
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DBSubnetGroup where
  rnf DBSubnetGroup' {..} =
    Prelude.rnf dbSubnetGroupArn
      `Prelude.seq` Prelude.rnf dbSubnetGroupDescription
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf subnetGroupStatus
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId
