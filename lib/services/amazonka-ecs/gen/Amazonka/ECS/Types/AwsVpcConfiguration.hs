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
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.AssignPublicIp
import qualified Amazonka.Prelude as Prelude

-- | An object representing the networking details for a task or service.
--
-- /See:/ 'newAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { -- | Whether the task\'s elastic network interface receives a public IP
    -- address. The default value is @DISABLED@.
    assignPublicIp :: Prelude.Maybe AssignPublicIp,
    -- | The IDs of the security groups associated with the task or service. If
    -- you don\'t specify a security group, the default security group for the
    -- VPC is used. There\'s a limit of 5 security groups that can be specified
    -- per @AwsVpcConfiguration@.
    --
    -- All specified security groups must be from the same VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
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
-- 'assignPublicIp', 'awsVpcConfiguration_assignPublicIp' - Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- 'securityGroups', 'awsVpcConfiguration_securityGroups' - The IDs of the security groups associated with the task or service. If
-- you don\'t specify a security group, the default security group for the
-- VPC is used. There\'s a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
--
-- 'subnets', 'awsVpcConfiguration_subnets' - The IDs of the subnets associated with the task or service. There\'s a
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
-- you don\'t specify a security group, the default security group for the
-- VPC is used. There\'s a limit of 5 security groups that can be specified
-- per @AwsVpcConfiguration@.
--
-- All specified security groups must be from the same VPC.
awsVpcConfiguration_securityGroups :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe [Prelude.Text])
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the subnets associated with the task or service. There\'s a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON AwsVpcConfiguration where
  parseJSON =
    Data.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Data..:? "assignPublicIp")
            Prelude.<*> (x Data..:? "securityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "subnets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsVpcConfiguration where
  hashWithSalt _salt AwsVpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` assignPublicIp
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData AwsVpcConfiguration where
  rnf AwsVpcConfiguration' {..} =
    Prelude.rnf assignPublicIp
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON AwsVpcConfiguration where
  toJSON AwsVpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Data..=)
              Prelude.<$> assignPublicIp,
            ("securityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnets" Data..= subnets)
          ]
      )
