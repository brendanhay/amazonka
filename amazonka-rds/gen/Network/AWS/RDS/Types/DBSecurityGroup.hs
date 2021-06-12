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
-- Module      : Network.AWS.RDS.Types.DBSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSecurityGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.IPRange

-- | Contains the details for an Amazon RDS DB security group.
--
-- This data type is used as a response element in the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newDBSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { -- | Provides the AWS ID of the owner of a specific DB security group.
    ownerId :: Core.Maybe Core.Text,
    -- | Specifies the name of the DB security group.
    dbSecurityGroupName :: Core.Maybe Core.Text,
    -- | Contains a list of @IPRange@ elements.
    iPRanges :: Core.Maybe [IPRange],
    -- | Provides the description of the DB security group.
    dbSecurityGroupDescription :: Core.Maybe Core.Text,
    -- | Contains a list of @EC2SecurityGroup@ elements.
    eC2SecurityGroups :: Core.Maybe [EC2SecurityGroup],
    -- | Provides the VpcId of the DB security group.
    vpcId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the DB security group.
    dbSecurityGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'dbSecurityGroup_ownerId' - Provides the AWS ID of the owner of a specific DB security group.
--
-- 'dbSecurityGroupName', 'dbSecurityGroup_dbSecurityGroupName' - Specifies the name of the DB security group.
--
-- 'iPRanges', 'dbSecurityGroup_iPRanges' - Contains a list of @IPRange@ elements.
--
-- 'dbSecurityGroupDescription', 'dbSecurityGroup_dbSecurityGroupDescription' - Provides the description of the DB security group.
--
-- 'eC2SecurityGroups', 'dbSecurityGroup_eC2SecurityGroups' - Contains a list of @EC2SecurityGroup@ elements.
--
-- 'vpcId', 'dbSecurityGroup_vpcId' - Provides the VpcId of the DB security group.
--
-- 'dbSecurityGroupArn', 'dbSecurityGroup_dbSecurityGroupArn' - The Amazon Resource Name (ARN) for the DB security group.
newDBSecurityGroup ::
  DBSecurityGroup
newDBSecurityGroup =
  DBSecurityGroup'
    { ownerId = Core.Nothing,
      dbSecurityGroupName = Core.Nothing,
      iPRanges = Core.Nothing,
      dbSecurityGroupDescription = Core.Nothing,
      eC2SecurityGroups = Core.Nothing,
      vpcId = Core.Nothing,
      dbSecurityGroupArn = Core.Nothing
    }

-- | Provides the AWS ID of the owner of a specific DB security group.
dbSecurityGroup_ownerId :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbSecurityGroup_ownerId = Lens.lens (\DBSecurityGroup' {ownerId} -> ownerId) (\s@DBSecurityGroup' {} a -> s {ownerId = a} :: DBSecurityGroup)

-- | Specifies the name of the DB security group.
dbSecurityGroup_dbSecurityGroupName :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbSecurityGroup_dbSecurityGroupName = Lens.lens (\DBSecurityGroup' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupName = a} :: DBSecurityGroup)

-- | Contains a list of @IPRange@ elements.
dbSecurityGroup_iPRanges :: Lens.Lens' DBSecurityGroup (Core.Maybe [IPRange])
dbSecurityGroup_iPRanges = Lens.lens (\DBSecurityGroup' {iPRanges} -> iPRanges) (\s@DBSecurityGroup' {} a -> s {iPRanges = a} :: DBSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | Provides the description of the DB security group.
dbSecurityGroup_dbSecurityGroupDescription :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbSecurityGroup_dbSecurityGroupDescription = Lens.lens (\DBSecurityGroup' {dbSecurityGroupDescription} -> dbSecurityGroupDescription) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupDescription = a} :: DBSecurityGroup)

-- | Contains a list of @EC2SecurityGroup@ elements.
dbSecurityGroup_eC2SecurityGroups :: Lens.Lens' DBSecurityGroup (Core.Maybe [EC2SecurityGroup])
dbSecurityGroup_eC2SecurityGroups = Lens.lens (\DBSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@DBSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: DBSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | Provides the VpcId of the DB security group.
dbSecurityGroup_vpcId :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbSecurityGroup_vpcId = Lens.lens (\DBSecurityGroup' {vpcId} -> vpcId) (\s@DBSecurityGroup' {} a -> s {vpcId = a} :: DBSecurityGroup)

-- | The Amazon Resource Name (ARN) for the DB security group.
dbSecurityGroup_dbSecurityGroupArn :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbSecurityGroup_dbSecurityGroupArn = Lens.lens (\DBSecurityGroup' {dbSecurityGroupArn} -> dbSecurityGroupArn) (\s@DBSecurityGroup' {} a -> s {dbSecurityGroupArn = a} :: DBSecurityGroup)

instance Core.FromXML DBSecurityGroup where
  parseXML x =
    DBSecurityGroup'
      Core.<$> (x Core..@? "OwnerId")
      Core.<*> (x Core..@? "DBSecurityGroupName")
      Core.<*> ( x Core..@? "IPRanges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "IPRange")
               )
      Core.<*> (x Core..@? "DBSecurityGroupDescription")
      Core.<*> ( x Core..@? "EC2SecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EC2SecurityGroup")
               )
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "DBSecurityGroupArn")

instance Core.Hashable DBSecurityGroup

instance Core.NFData DBSecurityGroup
