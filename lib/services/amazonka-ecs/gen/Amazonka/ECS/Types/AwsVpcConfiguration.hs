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
-- Module      : Amazonka.ECS.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.AwsVpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types.AssignPublicIp
import qualified Amazonka.Prelude as Prelude

-- | An object representing the networking details for a task or service.
--
-- /See:/ 'newAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { -- | The IDs of the security groups associated with the task or service. If
    -- you don\'t specify a security group, the default security group for the
    -- VPC is used. There\'s a limit of 5 security groups that can be specified
    -- per @AwsVpcConfiguration@.
    --
    -- All specified security groups must be from the same VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Whether the task\'s elastic network interface receives a public IP
    -- address. The default value is @DISABLED@.
    assignPublicIp :: Prelude.Maybe AssignPublicIp,
    -- | The IDs of the subnets associated with the task or service. There\'s a
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
-- 'securityGroups', 'awsVpcConfiguration_securityGroups' - The IDs of the security groups associated with the task or service. If
-- you don\'t specify a security group, the default security group for the
-- VPC is used. There\'s a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
--
-- 'assignPublicIp', 'awsVpcConfiguration_assignPublicIp' - Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- 'subnets', 'awsVpcConfiguration_subnets' - The IDs of the subnets associated with the task or service. There\'s a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
newAwsVpcConfiguration ::
  AwsVpcConfiguration
newAwsVpcConfiguration =
  AwsVpcConfiguration'
    { securityGroups =
        Prelude.Nothing,
      assignPublicIp = Prelude.Nothing,
      subnets = Prelude.mempty
    }

-- | The IDs of the security groups associated with the task or service. If
-- you don\'t specify a security group, the default security group for the
-- VPC is used. There\'s a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
awsVpcConfiguration_securityGroups :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe [Prelude.Text])
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
awsVpcConfiguration_assignPublicIp :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe AssignPublicIp)
awsVpcConfiguration_assignPublicIp = Lens.lens (\AwsVpcConfiguration' {assignPublicIp} -> assignPublicIp) (\s@AwsVpcConfiguration' {} a -> s {assignPublicIp = a} :: AwsVpcConfiguration)

-- | The IDs of the subnets associated with the task or service. There\'s a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Lens.coerced

instance Core.FromJSON AwsVpcConfiguration where
  parseJSON =
    Core.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "assignPublicIp")
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsVpcConfiguration where
  hashWithSalt _salt AwsVpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` assignPublicIp
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData AwsVpcConfiguration where
  rnf AwsVpcConfiguration' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf assignPublicIp
      `Prelude.seq` Prelude.rnf subnets

instance Core.ToJSON AwsVpcConfiguration where
  toJSON AwsVpcConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("securityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("assignPublicIp" Core..=)
              Prelude.<$> assignPublicIp,
            Prelude.Just ("subnets" Core..= subnets)
          ]
      )
