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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A VPC security groups that the DB instance belongs to.
--
-- /See:/ 'newAwsRdsDbInstanceVpcSecurityGroup' smart constructor.
data AwsRdsDbInstanceVpcSecurityGroup = AwsRdsDbInstanceVpcSecurityGroup'
  { -- | The status of the VPC security group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC security group.
    vpcSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbInstanceVpcSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbInstanceVpcSecurityGroup_status' - The status of the VPC security group.
--
-- 'vpcSecurityGroupId', 'awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId' - The name of the VPC security group.
newAwsRdsDbInstanceVpcSecurityGroup ::
  AwsRdsDbInstanceVpcSecurityGroup
newAwsRdsDbInstanceVpcSecurityGroup =
  AwsRdsDbInstanceVpcSecurityGroup'
    { status =
        Prelude.Nothing,
      vpcSecurityGroupId = Prelude.Nothing
    }

-- | The status of the VPC security group.
awsRdsDbInstanceVpcSecurityGroup_status :: Lens.Lens' AwsRdsDbInstanceVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceVpcSecurityGroup_status = Lens.lens (\AwsRdsDbInstanceVpcSecurityGroup' {status} -> status) (\s@AwsRdsDbInstanceVpcSecurityGroup' {} a -> s {status = a} :: AwsRdsDbInstanceVpcSecurityGroup)

-- | The name of the VPC security group.
awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId :: Lens.Lens' AwsRdsDbInstanceVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId = Lens.lens (\AwsRdsDbInstanceVpcSecurityGroup' {vpcSecurityGroupId} -> vpcSecurityGroupId) (\s@AwsRdsDbInstanceVpcSecurityGroup' {} a -> s {vpcSecurityGroupId = a} :: AwsRdsDbInstanceVpcSecurityGroup)

instance
  Data.FromJSON
    AwsRdsDbInstanceVpcSecurityGroup
  where
  parseJSON =
    Data.withObject
      "AwsRdsDbInstanceVpcSecurityGroup"
      ( \x ->
          AwsRdsDbInstanceVpcSecurityGroup'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VpcSecurityGroupId")
      )

instance
  Prelude.Hashable
    AwsRdsDbInstanceVpcSecurityGroup
  where
  hashWithSalt
    _salt
    AwsRdsDbInstanceVpcSecurityGroup' {..} =
      _salt
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` vpcSecurityGroupId

instance
  Prelude.NFData
    AwsRdsDbInstanceVpcSecurityGroup
  where
  rnf AwsRdsDbInstanceVpcSecurityGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcSecurityGroupId

instance Data.ToJSON AwsRdsDbInstanceVpcSecurityGroup where
  toJSON AwsRdsDbInstanceVpcSecurityGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            ("VpcSecurityGroupId" Data..=)
              Prelude.<$> vpcSecurityGroupId
          ]
      )
