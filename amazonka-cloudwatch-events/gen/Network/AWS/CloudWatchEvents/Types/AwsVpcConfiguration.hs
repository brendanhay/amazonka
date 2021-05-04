{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration where

import Network.AWS.CloudWatchEvents.Types.AssignPublicIp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the subnets associated with the task. These subnets must all
    -- be in the same VPC. You can specify as many as 16 subnets.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the subnets associated with the task. These subnets must all
-- be in the same VPC. You can specify as many as 16 subnets.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromJSON AwsVpcConfiguration where
  parseJSON =
    Prelude.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Prelude..:? "AssignPublicIp")
            Prelude.<*> ( x Prelude..:? "SecurityGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Subnets" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsVpcConfiguration

instance Prelude.NFData AwsVpcConfiguration

instance Prelude.ToJSON AwsVpcConfiguration where
  toJSON AwsVpcConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AssignPublicIp" Prelude..=)
              Prelude.<$> assignPublicIp,
            ("SecurityGroups" Prelude..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("Subnets" Prelude..= subnets)
          ]
      )
