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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupEc2SecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupIpRange

-- | Provides information about an Amazon RDS DB security group.
--
-- /See:/ 'newAwsRdsDbSecurityGroupDetails' smart constructor.
data AwsRdsDbSecurityGroupDetails = AwsRdsDbSecurityGroupDetails'
  { -- | The ARN for the DB security group.
    dbSecurityGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the description of the DB security group.
    dbSecurityGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the DB security group.
    dbSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of EC2 security groups.
    ec2SecurityGroups :: Prelude.Maybe [AwsRdsDbSecurityGroupEc2SecurityGroup],
    -- | Contains a list of IP ranges.
    ipRanges :: Prelude.Maybe [AwsRdsDbSecurityGroupIpRange],
    -- | Provides the Amazon Web Services ID of the owner of a specific DB
    -- security group.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Provides VPC ID associated with the DB security group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSecurityGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupArn', 'awsRdsDbSecurityGroupDetails_dbSecurityGroupArn' - The ARN for the DB security group.
--
-- 'dbSecurityGroupDescription', 'awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription' - Provides the description of the DB security group.
--
-- 'dbSecurityGroupName', 'awsRdsDbSecurityGroupDetails_dbSecurityGroupName' - Specifies the name of the DB security group.
--
-- 'ec2SecurityGroups', 'awsRdsDbSecurityGroupDetails_ec2SecurityGroups' - Contains a list of EC2 security groups.
--
-- 'ipRanges', 'awsRdsDbSecurityGroupDetails_ipRanges' - Contains a list of IP ranges.
--
-- 'ownerId', 'awsRdsDbSecurityGroupDetails_ownerId' - Provides the Amazon Web Services ID of the owner of a specific DB
-- security group.
--
-- 'vpcId', 'awsRdsDbSecurityGroupDetails_vpcId' - Provides VPC ID associated with the DB security group.
newAwsRdsDbSecurityGroupDetails ::
  AwsRdsDbSecurityGroupDetails
newAwsRdsDbSecurityGroupDetails =
  AwsRdsDbSecurityGroupDetails'
    { dbSecurityGroupArn =
        Prelude.Nothing,
      dbSecurityGroupDescription = Prelude.Nothing,
      dbSecurityGroupName = Prelude.Nothing,
      ec2SecurityGroups = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ARN for the DB security group.
awsRdsDbSecurityGroupDetails_dbSecurityGroupArn :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupDetails_dbSecurityGroupArn = Lens.lens (\AwsRdsDbSecurityGroupDetails' {dbSecurityGroupArn} -> dbSecurityGroupArn) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {dbSecurityGroupArn = a} :: AwsRdsDbSecurityGroupDetails)

-- | Provides the description of the DB security group.
awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription = Lens.lens (\AwsRdsDbSecurityGroupDetails' {dbSecurityGroupDescription} -> dbSecurityGroupDescription) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {dbSecurityGroupDescription = a} :: AwsRdsDbSecurityGroupDetails)

-- | Specifies the name of the DB security group.
awsRdsDbSecurityGroupDetails_dbSecurityGroupName :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupDetails_dbSecurityGroupName = Lens.lens (\AwsRdsDbSecurityGroupDetails' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {dbSecurityGroupName = a} :: AwsRdsDbSecurityGroupDetails)

-- | Contains a list of EC2 security groups.
awsRdsDbSecurityGroupDetails_ec2SecurityGroups :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe [AwsRdsDbSecurityGroupEc2SecurityGroup])
awsRdsDbSecurityGroupDetails_ec2SecurityGroups = Lens.lens (\AwsRdsDbSecurityGroupDetails' {ec2SecurityGroups} -> ec2SecurityGroups) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {ec2SecurityGroups = a} :: AwsRdsDbSecurityGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | Contains a list of IP ranges.
awsRdsDbSecurityGroupDetails_ipRanges :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe [AwsRdsDbSecurityGroupIpRange])
awsRdsDbSecurityGroupDetails_ipRanges = Lens.lens (\AwsRdsDbSecurityGroupDetails' {ipRanges} -> ipRanges) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {ipRanges = a} :: AwsRdsDbSecurityGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | Provides the Amazon Web Services ID of the owner of a specific DB
-- security group.
awsRdsDbSecurityGroupDetails_ownerId :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupDetails_ownerId = Lens.lens (\AwsRdsDbSecurityGroupDetails' {ownerId} -> ownerId) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {ownerId = a} :: AwsRdsDbSecurityGroupDetails)

-- | Provides VPC ID associated with the DB security group.
awsRdsDbSecurityGroupDetails_vpcId :: Lens.Lens' AwsRdsDbSecurityGroupDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupDetails_vpcId = Lens.lens (\AwsRdsDbSecurityGroupDetails' {vpcId} -> vpcId) (\s@AwsRdsDbSecurityGroupDetails' {} a -> s {vpcId = a} :: AwsRdsDbSecurityGroupDetails)

instance Data.FromJSON AwsRdsDbSecurityGroupDetails where
  parseJSON =
    Data.withObject
      "AwsRdsDbSecurityGroupDetails"
      ( \x ->
          AwsRdsDbSecurityGroupDetails'
            Prelude.<$> (x Data..:? "DbSecurityGroupArn")
            Prelude.<*> (x Data..:? "DbSecurityGroupDescription")
            Prelude.<*> (x Data..:? "DbSecurityGroupName")
            Prelude.<*> ( x Data..:? "Ec2SecurityGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IpRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    AwsRdsDbSecurityGroupDetails
  where
  hashWithSalt _salt AwsRdsDbSecurityGroupDetails' {..} =
    _salt `Prelude.hashWithSalt` dbSecurityGroupArn
      `Prelude.hashWithSalt` dbSecurityGroupDescription
      `Prelude.hashWithSalt` dbSecurityGroupName
      `Prelude.hashWithSalt` ec2SecurityGroups
      `Prelude.hashWithSalt` ipRanges
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsRdsDbSecurityGroupDetails where
  rnf AwsRdsDbSecurityGroupDetails' {..} =
    Prelude.rnf dbSecurityGroupArn
      `Prelude.seq` Prelude.rnf dbSecurityGroupDescription
      `Prelude.seq` Prelude.rnf dbSecurityGroupName
      `Prelude.seq` Prelude.rnf ec2SecurityGroups
      `Prelude.seq` Prelude.rnf ipRanges
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsRdsDbSecurityGroupDetails where
  toJSON AwsRdsDbSecurityGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbSecurityGroupArn" Data..=)
              Prelude.<$> dbSecurityGroupArn,
            ("DbSecurityGroupDescription" Data..=)
              Prelude.<$> dbSecurityGroupDescription,
            ("DbSecurityGroupName" Data..=)
              Prelude.<$> dbSecurityGroupName,
            ("Ec2SecurityGroups" Data..=)
              Prelude.<$> ec2SecurityGroups,
            ("IpRanges" Data..=) Prelude.<$> ipRanges,
            ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
