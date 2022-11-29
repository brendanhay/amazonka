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
-- Module      : Amazonka.RDS.Types.DBSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.EC2SecurityGroup
import Amazonka.RDS.Types.IPRange

-- | Contains the details for an Amazon RDS DB security group.
--
-- This data type is used as a response element in the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newDBSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { -- | Provides the Amazon Web Services ID of the owner of a specific DB
    -- security group.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Provides the description of the DB security group.
    dbSecurityGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the DB security group.
    dbSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @EC2SecurityGroup@ elements.
    eC2SecurityGroups :: Prelude.Maybe [EC2SecurityGroup],
    -- | The Amazon Resource Name (ARN) for the DB security group.
    dbSecurityGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @IPRange@ elements.
    iPRanges :: Prelude.Maybe [IPRange],
    -- | Provides the VpcId of the DB security group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'dbSecurityGroup_ownerId' - Provides the Amazon Web Services ID of the owner of a specific DB
-- security group.
--
-- 'dbSecurityGroupDescription', 'dbSecurityGroup_dbSecurityGroupDescription' - Provides the description of the DB security group.
--
-- 'dbSecurityGroupName', 'dbSecurityGroup_dbSecurityGroupName' - Specifies the name of the DB security group.
--
-- 'eC2SecurityGroups', 'dbSecurityGroup_eC2SecurityGroups' - Contains a list of @EC2SecurityGroup@ elements.
--
-- 'dbSecurityGroupArn', 'dbSecurityGroup_dbSecurityGroupArn' - The Amazon Resource Name (ARN) for the DB security group.
--
-- 'iPRanges', 'dbSecurityGroup_iPRanges' - Contains a list of @IPRange@ elements.
--
-- 'vpcId', 'dbSecurityGroup_vpcId' - Provides the VpcId of the DB security group.
newDBSecurityGroup ::
  DBSecurityGroup
newDBSecurityGroup =
  DBSecurityGroup'
    { ownerId = Prelude.Nothing,
      dbSecurityGroupDescription = Prelude.Nothing,
      dbSecurityGroupName = Prelude.Nothing,
      eC2SecurityGroups = Prelude.Nothing,
      dbSecurityGroupArn = Prelude.Nothing,
      iPRanges = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Provides the Amazon Web Services ID of the owner of a specific DB
-- security group.
dbSecurityGroup_ownerId :: Lens.Lens' DBSecurityGroup (Prelude.Maybe Prelude.Text)
dbSecurityGroup_ownerId = Lens.lens (\DBSecurityGroup' {ownerId} -> ownerId) (\s@DBSecurityGroup' {} a -> s {ownerId = a} :: DBSecurityGroup)

-- | Provides the description of the DB security group.
dbSecurityGroup_dbSecurityGroupDescription :: Lens.Lens' DBSecurityGroup (Prelude.Maybe Prelude.Text)
dbSecurityGroup_dbSecurityGroupDescription = Lens.lens (\DBSecurityGroup' {dbSecurityGroupDescription} -> dbSecurityGroupDescription) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupDescription = a} :: DBSecurityGroup)

-- | Specifies the name of the DB security group.
dbSecurityGroup_dbSecurityGroupName :: Lens.Lens' DBSecurityGroup (Prelude.Maybe Prelude.Text)
dbSecurityGroup_dbSecurityGroupName = Lens.lens (\DBSecurityGroup' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupName = a} :: DBSecurityGroup)

-- | Contains a list of @EC2SecurityGroup@ elements.
dbSecurityGroup_eC2SecurityGroups :: Lens.Lens' DBSecurityGroup (Prelude.Maybe [EC2SecurityGroup])
dbSecurityGroup_eC2SecurityGroups = Lens.lens (\DBSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@DBSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: DBSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the DB security group.
dbSecurityGroup_dbSecurityGroupArn :: Lens.Lens' DBSecurityGroup (Prelude.Maybe Prelude.Text)
dbSecurityGroup_dbSecurityGroupArn = Lens.lens (\DBSecurityGroup' {dbSecurityGroupArn} -> dbSecurityGroupArn) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupArn = a} :: DBSecurityGroup)

-- | Contains a list of @IPRange@ elements.
dbSecurityGroup_iPRanges :: Lens.Lens' DBSecurityGroup (Prelude.Maybe [IPRange])
dbSecurityGroup_iPRanges = Lens.lens (\DBSecurityGroup' {iPRanges} -> iPRanges) (\s@DBSecurityGroup' {} a -> s {iPRanges = a} :: DBSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | Provides the VpcId of the DB security group.
dbSecurityGroup_vpcId :: Lens.Lens' DBSecurityGroup (Prelude.Maybe Prelude.Text)
dbSecurityGroup_vpcId = Lens.lens (\DBSecurityGroup' {vpcId} -> vpcId) (\s@DBSecurityGroup' {} a -> s {vpcId = a} :: DBSecurityGroup)

instance Core.FromXML DBSecurityGroup where
  parseXML x =
    DBSecurityGroup'
      Prelude.<$> (x Core..@? "OwnerId")
      Prelude.<*> (x Core..@? "DBSecurityGroupDescription")
      Prelude.<*> (x Core..@? "DBSecurityGroupName")
      Prelude.<*> ( x Core..@? "EC2SecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EC2SecurityGroup")
                  )
      Prelude.<*> (x Core..@? "DBSecurityGroupArn")
      Prelude.<*> ( x Core..@? "IPRanges" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "IPRange")
                  )
      Prelude.<*> (x Core..@? "VpcId")

instance Prelude.Hashable DBSecurityGroup where
  hashWithSalt _salt DBSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` dbSecurityGroupDescription
      `Prelude.hashWithSalt` dbSecurityGroupName
      `Prelude.hashWithSalt` eC2SecurityGroups
      `Prelude.hashWithSalt` dbSecurityGroupArn
      `Prelude.hashWithSalt` iPRanges
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DBSecurityGroup where
  rnf DBSecurityGroup' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf dbSecurityGroupDescription
      `Prelude.seq` Prelude.rnf dbSecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroups
      `Prelude.seq` Prelude.rnf dbSecurityGroupArn
      `Prelude.seq` Prelude.rnf iPRanges
      `Prelude.seq` Prelude.rnf vpcId
