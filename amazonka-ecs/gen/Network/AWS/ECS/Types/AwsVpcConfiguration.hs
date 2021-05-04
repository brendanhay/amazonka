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
-- Module      : Network.AWS.ECS.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AwsVpcConfiguration where

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
awsVpcConfiguration_securityGroups = Lens.lens (\AwsVpcConfiguration' {securityGroups} -> securityGroups) (\s@AwsVpcConfiguration' {} a -> s {securityGroups = a} :: AwsVpcConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the subnets associated with the task or service. There is a
-- limit of 16 subnets that can be specified per @AwsVpcConfiguration@.
--
-- All specified subnets must be from the same VPC.
awsVpcConfiguration_subnets :: Lens.Lens' AwsVpcConfiguration [Prelude.Text]
awsVpcConfiguration_subnets = Lens.lens (\AwsVpcConfiguration' {subnets} -> subnets) (\s@AwsVpcConfiguration' {} a -> s {subnets = a} :: AwsVpcConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromJSON AwsVpcConfiguration where
  parseJSON =
    Prelude.withObject
      "AwsVpcConfiguration"
      ( \x ->
          AwsVpcConfiguration'
            Prelude.<$> (x Prelude..:? "assignPublicIp")
            Prelude.<*> ( x Prelude..:? "securityGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "subnets" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsVpcConfiguration

instance Prelude.NFData AwsVpcConfiguration

instance Prelude.ToJSON AwsVpcConfiguration where
  toJSON AwsVpcConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Prelude..=)
              Prelude.<$> assignPublicIp,
            ("securityGroups" Prelude..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnets" Prelude..= subnets)
          ]
      )
