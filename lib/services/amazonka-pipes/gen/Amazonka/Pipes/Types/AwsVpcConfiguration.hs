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
-- Module      : Amazonka.Pipes.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.AwsVpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.AssignPublicIp
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies the VPC subnets and security groups for the
-- task, and whether a public IP address is to be used. This structure is
-- relevant only for ECS tasks that use the @awsvpc@ network mode.
--
-- /See:/ 'newAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { -- | Specifies whether the task\'s elastic network interface receives a
    -- public IP address. You can specify @ENABLED@ only when @LaunchType@ in
    -- @EcsParameters@ is set to @FARGATE@.
    assignPublicIp :: Prelude.Maybe AssignPublicIp,
    -- | Specifies the security groups associated with the task. These security
    -- groups must all be in the same VPC. You can specify as many as five
    -- security groups. If you do not specify a security group, the default
    -- security group for the VPC is used.
    securityGroups :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | Specifies the subnets associated with the task. These subnets must all
    -- be in the same VPC. You can specify as many as 16 subnets.
    subnets :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignPublicIp', 'awsVpcConfiguration_assignPublicIp' - Specifies whether the task\'s elastic network interface receives a
-- public IP address. You can specify @ENABLED@ only when @LaunchType@ in
-- @EcsParameters@ is set to @FARGATE@.
--
-- 'securityGroups', 'awsVpcConfiguration_securityGroups' - Specifies the security groups associated with the task. These security
-- groups must all be in the same VPC. You can specify as many as five
-- security groups. If you do not specify a security group, the default
-- security group for the VPC is used.
--
-- 'subnets', 'awsVpcConfiguration_subnets' - Specifies the subnets associated with the task. These subnets must all
-- be in the same VPC. You can specify as many as 16 subnets.
newAwsVpcConfiguration ::
  AwsVpcConfiguration
newAwsVpcConfiguration =
  AwsVpcConfiguration'
    { assignPublicIp =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnets = Prelude.mempty
    }

-- | Specifies whether the task\'s elastic network interface receives a
-- public IP address. You can specify @ENABLED@ only when @LaunchType@ in
-- @EcsParameters@ is set to @FARGATE@.
awsVpcConfiguration_assignPublicIp :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe AssignPublicIp)
awsVpcConfiguration_assignPublicIp = Lens.lens (\AwsVpcConfiguration' {assignPublicIp} -> assignPublicIp) (\s@AwsVpcConfiguration' {} a -> s {assignPublicIp = a} :: AwsVpcConfiguration)

-- | Specifies the security groups associated with the task. These security
-- groups must all be in the same VPC. You can specify as many as five
-- security groups. If you do not specify a security group, the default
-- security group for the VPC is used.
awsVpcConfiguration_securityGroups :: Lens.Lens' AwsVpcConfiguration (Prelude.Maybe [Prelude.Text])
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the subnets associated with the task. These subnets must all
-- be in the same VPC. You can specify as many as 16 subnets.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON AwsVpcConfiguration where
  parseJSON =
    Data.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Data..:? "AssignPublicIp")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
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
          [ ("AssignPublicIp" Data..=)
              Prelude.<$> assignPublicIp,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )
