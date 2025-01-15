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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupEc2SecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupEc2SecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | EC2 security group information for an RDS DB security group.
--
-- /See:/ 'newAwsRdsDbSecurityGroupEc2SecurityGroup' smart constructor.
data AwsRdsDbSecurityGroupEc2SecurityGroup = AwsRdsDbSecurityGroupEc2SecurityGroup'
  { -- | Specifies the ID for the EC2 security group.
    ec2SecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the EC2 security group.
    ec2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the Amazon Web Services ID of the owner of the EC2 security
    -- group.
    ec2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the EC2 security group.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSecurityGroupEc2SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2SecurityGroupId', 'awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId' - Specifies the ID for the EC2 security group.
--
-- 'ec2SecurityGroupName', 'awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName' - Specifies the name of the EC2 security group.
--
-- 'ec2SecurityGroupOwnerId', 'awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId' - Provides the Amazon Web Services ID of the owner of the EC2 security
-- group.
--
-- 'status', 'awsRdsDbSecurityGroupEc2SecurityGroup_status' - Provides the status of the EC2 security group.
newAwsRdsDbSecurityGroupEc2SecurityGroup ::
  AwsRdsDbSecurityGroupEc2SecurityGroup
newAwsRdsDbSecurityGroupEc2SecurityGroup =
  AwsRdsDbSecurityGroupEc2SecurityGroup'
    { ec2SecurityGroupId =
        Prelude.Nothing,
      ec2SecurityGroupName =
        Prelude.Nothing,
      ec2SecurityGroupOwnerId =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the ID for the EC2 security group.
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId :: Lens.Lens' AwsRdsDbSecurityGroupEc2SecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId = Lens.lens (\AwsRdsDbSecurityGroupEc2SecurityGroup' {ec2SecurityGroupId} -> ec2SecurityGroupId) (\s@AwsRdsDbSecurityGroupEc2SecurityGroup' {} a -> s {ec2SecurityGroupId = a} :: AwsRdsDbSecurityGroupEc2SecurityGroup)

-- | Specifies the name of the EC2 security group.
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName :: Lens.Lens' AwsRdsDbSecurityGroupEc2SecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName = Lens.lens (\AwsRdsDbSecurityGroupEc2SecurityGroup' {ec2SecurityGroupName} -> ec2SecurityGroupName) (\s@AwsRdsDbSecurityGroupEc2SecurityGroup' {} a -> s {ec2SecurityGroupName = a} :: AwsRdsDbSecurityGroupEc2SecurityGroup)

-- | Provides the Amazon Web Services ID of the owner of the EC2 security
-- group.
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId :: Lens.Lens' AwsRdsDbSecurityGroupEc2SecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId = Lens.lens (\AwsRdsDbSecurityGroupEc2SecurityGroup' {ec2SecurityGroupOwnerId} -> ec2SecurityGroupOwnerId) (\s@AwsRdsDbSecurityGroupEc2SecurityGroup' {} a -> s {ec2SecurityGroupOwnerId = a} :: AwsRdsDbSecurityGroupEc2SecurityGroup)

-- | Provides the status of the EC2 security group.
awsRdsDbSecurityGroupEc2SecurityGroup_status :: Lens.Lens' AwsRdsDbSecurityGroupEc2SecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupEc2SecurityGroup_status = Lens.lens (\AwsRdsDbSecurityGroupEc2SecurityGroup' {status} -> status) (\s@AwsRdsDbSecurityGroupEc2SecurityGroup' {} a -> s {status = a} :: AwsRdsDbSecurityGroupEc2SecurityGroup)

instance
  Data.FromJSON
    AwsRdsDbSecurityGroupEc2SecurityGroup
  where
  parseJSON =
    Data.withObject
      "AwsRdsDbSecurityGroupEc2SecurityGroup"
      ( \x ->
          AwsRdsDbSecurityGroupEc2SecurityGroup'
            Prelude.<$> (x Data..:? "Ec2SecurityGroupId")
            Prelude.<*> (x Data..:? "Ec2SecurityGroupName")
            Prelude.<*> (x Data..:? "Ec2SecurityGroupOwnerId")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRdsDbSecurityGroupEc2SecurityGroup
  where
  hashWithSalt
    _salt
    AwsRdsDbSecurityGroupEc2SecurityGroup' {..} =
      _salt
        `Prelude.hashWithSalt` ec2SecurityGroupId
        `Prelude.hashWithSalt` ec2SecurityGroupName
        `Prelude.hashWithSalt` ec2SecurityGroupOwnerId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRdsDbSecurityGroupEc2SecurityGroup
  where
  rnf AwsRdsDbSecurityGroupEc2SecurityGroup' {..} =
    Prelude.rnf ec2SecurityGroupId `Prelude.seq`
      Prelude.rnf ec2SecurityGroupName `Prelude.seq`
        Prelude.rnf ec2SecurityGroupOwnerId `Prelude.seq`
          Prelude.rnf status

instance
  Data.ToJSON
    AwsRdsDbSecurityGroupEc2SecurityGroup
  where
  toJSON AwsRdsDbSecurityGroupEc2SecurityGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ec2SecurityGroupId" Data..=)
              Prelude.<$> ec2SecurityGroupId,
            ("Ec2SecurityGroupName" Data..=)
              Prelude.<$> ec2SecurityGroupName,
            ("Ec2SecurityGroupOwnerId" Data..=)
              Prelude.<$> ec2SecurityGroupOwnerId,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
