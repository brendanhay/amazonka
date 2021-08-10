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
-- Module      : Network.AWS.ECS.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AwsVpcConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.AssignPublicIp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the networking details for a task or service.
--
-- /See:/ 'newAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { -- | Whether the task\'s elastic network interface receives a public IP
    -- address. The default value is @DISABLED@.
    assignPublicIp :: Prelude.Maybe AssignPublicIp,
    -- | The IDs of the security groups associated with the task or service. If
    -- you do not specify a security group, the default security group for the
    -- VPC is used. There is a limit of 5 security groups that can be specified
    -- per @AwsVpcConfiguration@.
    --
    -- All specified security groups must be from the same VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the subnets associated with the task or service. There is a
    -- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
    --
    -- All specified subnets must be from the same VPC.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignPublicIp', 'awsVpcConfiguration_assignPublicIp' - Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- 'securityGroups', 'awsVpcConfiguration_securityGroups' - The IDs of the security groups associated with the task or service. If
-- you do not specify a security group, the default security group for the
-- VPC is used. There is a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
--
-- 'subnets', 'awsVpcConfiguration_subnets' - The IDs of the subnets associated with the task or service. There is a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
newAwsVpcConfiguration ::
  AwsVpcConfiguration
newAwsVpcConfiguration =
  AwsVpcConfiguration'
    { assignPublicIp =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnets = Prelude.mempty
    }

-- | Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
awsVpcConfiguration_assignPublicIp :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe AssignPublicIp)
awsVpcConfiguration_assignPublicIp = Lens.lens (\AwsVpcConfiguration' {assignPublicIp} -> assignPublicIp) (\s@AwsVpcConfiguration' {} a -> s {assignPublicIp = a} :: AwsVpcConfiguration)

-- | The IDs of the security groups associated with the task or service. If
-- you do not specify a security group, the default security group for the
-- VPC is used. There is a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
awsVpcConfiguration_securityGroups :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe [Prelude.Text])
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The IDs of the subnets associated with the task or service. There is a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Lens._Coerce

instance Core.FromJSON AwsVpcConfiguration where
  parseJSON =
    Core.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Core..:? "assignPublicIp")
            Prelude.<*> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsVpcConfiguration

instance Prelude.NFData AwsVpcConfiguration

instance Core.ToJSON AwsVpcConfiguration where
  toJSON AwsVpcConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Core..=)
              Prelude.<$> assignPublicIp,
            ("securityGroups" Core..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnets" Core..= subnets)
          ]
      )
