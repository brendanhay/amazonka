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
-- Module      : Amazonka.DataSync.Types.Ec2Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Ec2Config where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The subnet and the security group that DataSync uses to access target
-- EFS file system. The subnet must have at least one mount target for that
-- file system. The security group that you provide needs to be able to
-- communicate with the security group on the mount target in the subnet
-- specified.
--
-- /See:/ 'newEc2Config' smart constructor.
data Ec2Config = Ec2Config'
  { -- | The ARN of the subnet and the security group that DataSync uses to
    -- access the target EFS file system.
    subnetArn :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the security groups that are
    -- configured for the Amazon EC2 resource.
    securityGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetArn', 'ec2Config_subnetArn' - The ARN of the subnet and the security group that DataSync uses to
-- access the target EFS file system.
--
-- 'securityGroupArns', 'ec2Config_securityGroupArns' - The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the Amazon EC2 resource.
newEc2Config ::
  -- | 'subnetArn'
  Prelude.Text ->
  -- | 'securityGroupArns'
  Prelude.NonEmpty Prelude.Text ->
  Ec2Config
newEc2Config pSubnetArn_ pSecurityGroupArns_ =
  Ec2Config'
    { subnetArn = pSubnetArn_,
      securityGroupArns =
        Lens.coerced Lens.# pSecurityGroupArns_
    }

-- | The ARN of the subnet and the security group that DataSync uses to
-- access the target EFS file system.
ec2Config_subnetArn :: Lens.Lens' Ec2Config Prelude.Text
ec2Config_subnetArn = Lens.lens (\Ec2Config' {subnetArn} -> subnetArn) (\s@Ec2Config' {} a -> s {subnetArn = a} :: Ec2Config)

-- | The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the Amazon EC2 resource.
ec2Config_securityGroupArns :: Lens.Lens' Ec2Config (Prelude.NonEmpty Prelude.Text)
ec2Config_securityGroupArns = Lens.lens (\Ec2Config' {securityGroupArns} -> securityGroupArns) (\s@Ec2Config' {} a -> s {securityGroupArns = a} :: Ec2Config) Prelude.. Lens.coerced

instance Core.FromJSON Ec2Config where
  parseJSON =
    Core.withObject
      "Ec2Config"
      ( \x ->
          Ec2Config'
            Prelude.<$> (x Core..: "SubnetArn")
            Prelude.<*> (x Core..: "SecurityGroupArns")
      )

instance Prelude.Hashable Ec2Config where
  hashWithSalt salt' Ec2Config' {..} =
    salt' `Prelude.hashWithSalt` securityGroupArns
      `Prelude.hashWithSalt` subnetArn

instance Prelude.NFData Ec2Config where
  rnf Ec2Config' {..} =
    Prelude.rnf subnetArn
      `Prelude.seq` Prelude.rnf securityGroupArns

instance Core.ToJSON Ec2Config where
  toJSON Ec2Config' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SubnetArn" Core..= subnetArn),
            Prelude.Just
              ("SecurityGroupArns" Core..= securityGroupArns)
          ]
      )
