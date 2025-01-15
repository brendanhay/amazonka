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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet

-- | Information about the subnet group for the database instance.
--
-- /See:/ 'newAwsRdsDbSubnetGroup' smart constructor.
data AwsRdsDbSubnetGroup = AwsRdsDbSubnetGroup'
  { -- | The ARN of the subnet group.
    dbSubnetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the subnet group.
    dbSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the subnet group.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets in the subnet group.
    subnets :: Prelude.Maybe [AwsRdsDbSubnetGroupSubnet],
    -- | The VPC ID of the subnet group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupArn', 'awsRdsDbSubnetGroup_dbSubnetGroupArn' - The ARN of the subnet group.
--
-- 'dbSubnetGroupDescription', 'awsRdsDbSubnetGroup_dbSubnetGroupDescription' - The description of the subnet group.
--
-- 'dbSubnetGroupName', 'awsRdsDbSubnetGroup_dbSubnetGroupName' - The name of the subnet group.
--
-- 'subnetGroupStatus', 'awsRdsDbSubnetGroup_subnetGroupStatus' - The status of the subnet group.
--
-- 'subnets', 'awsRdsDbSubnetGroup_subnets' - A list of subnets in the subnet group.
--
-- 'vpcId', 'awsRdsDbSubnetGroup_vpcId' - The VPC ID of the subnet group.
newAwsRdsDbSubnetGroup ::
  AwsRdsDbSubnetGroup
newAwsRdsDbSubnetGroup =
  AwsRdsDbSubnetGroup'
    { dbSubnetGroupArn =
        Prelude.Nothing,
      dbSubnetGroupDescription = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ARN of the subnet group.
awsRdsDbSubnetGroup_dbSubnetGroupArn :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroup_dbSubnetGroupArn = Lens.lens (\AwsRdsDbSubnetGroup' {dbSubnetGroupArn} -> dbSubnetGroupArn) (\s@AwsRdsDbSubnetGroup' {} a -> s {dbSubnetGroupArn = a} :: AwsRdsDbSubnetGroup)

-- | The description of the subnet group.
awsRdsDbSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroup_dbSubnetGroupDescription = Lens.lens (\AwsRdsDbSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@AwsRdsDbSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: AwsRdsDbSubnetGroup)

-- | The name of the subnet group.
awsRdsDbSubnetGroup_dbSubnetGroupName :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroup_dbSubnetGroupName = Lens.lens (\AwsRdsDbSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@AwsRdsDbSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: AwsRdsDbSubnetGroup)

-- | The status of the subnet group.
awsRdsDbSubnetGroup_subnetGroupStatus :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroup_subnetGroupStatus = Lens.lens (\AwsRdsDbSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@AwsRdsDbSubnetGroup' {} a -> s {subnetGroupStatus = a} :: AwsRdsDbSubnetGroup)

-- | A list of subnets in the subnet group.
awsRdsDbSubnetGroup_subnets :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe [AwsRdsDbSubnetGroupSubnet])
awsRdsDbSubnetGroup_subnets = Lens.lens (\AwsRdsDbSubnetGroup' {subnets} -> subnets) (\s@AwsRdsDbSubnetGroup' {} a -> s {subnets = a} :: AwsRdsDbSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID of the subnet group.
awsRdsDbSubnetGroup_vpcId :: Lens.Lens' AwsRdsDbSubnetGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroup_vpcId = Lens.lens (\AwsRdsDbSubnetGroup' {vpcId} -> vpcId) (\s@AwsRdsDbSubnetGroup' {} a -> s {vpcId = a} :: AwsRdsDbSubnetGroup)

instance Data.FromJSON AwsRdsDbSubnetGroup where
  parseJSON =
    Data.withObject
      "AwsRdsDbSubnetGroup"
      ( \x ->
          AwsRdsDbSubnetGroup'
            Prelude.<$> (x Data..:? "DbSubnetGroupArn")
            Prelude.<*> (x Data..:? "DbSubnetGroupDescription")
            Prelude.<*> (x Data..:? "DbSubnetGroupName")
            Prelude.<*> (x Data..:? "SubnetGroupStatus")
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsRdsDbSubnetGroup where
  hashWithSalt _salt AwsRdsDbSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbSubnetGroupArn
      `Prelude.hashWithSalt` dbSubnetGroupDescription
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` subnetGroupStatus
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsRdsDbSubnetGroup where
  rnf AwsRdsDbSubnetGroup' {..} =
    Prelude.rnf dbSubnetGroupArn `Prelude.seq`
      Prelude.rnf dbSubnetGroupDescription `Prelude.seq`
        Prelude.rnf dbSubnetGroupName `Prelude.seq`
          Prelude.rnf subnetGroupStatus `Prelude.seq`
            Prelude.rnf subnets `Prelude.seq`
              Prelude.rnf vpcId

instance Data.ToJSON AwsRdsDbSubnetGroup where
  toJSON AwsRdsDbSubnetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbSubnetGroupArn" Data..=)
              Prelude.<$> dbSubnetGroupArn,
            ("DbSubnetGroupDescription" Data..=)
              Prelude.<$> dbSubnetGroupDescription,
            ("DbSubnetGroupName" Data..=)
              Prelude.<$> dbSubnetGroupName,
            ("SubnetGroupStatus" Data..=)
              Prelude.<$> subnetGroupStatus,
            ("Subnets" Data..=) Prelude.<$> subnets,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
