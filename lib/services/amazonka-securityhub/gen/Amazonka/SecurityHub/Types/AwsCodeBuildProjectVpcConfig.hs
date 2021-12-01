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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectVpcConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the VPC configuration that CodeBuild accesses.
--
-- /See:/ 'newAwsCodeBuildProjectVpcConfig' smart constructor.
data AwsCodeBuildProjectVpcConfig = AwsCodeBuildProjectVpcConfig'
  { -- | A list of one or more security group IDs in your VPC.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A list of one or more subnet IDs in your VPC.
    subnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'awsCodeBuildProjectVpcConfig_securityGroupIds' - A list of one or more security group IDs in your VPC.
--
-- 'vpcId', 'awsCodeBuildProjectVpcConfig_vpcId' - The ID of the VPC.
--
-- 'subnets', 'awsCodeBuildProjectVpcConfig_subnets' - A list of one or more subnet IDs in your VPC.
newAwsCodeBuildProjectVpcConfig ::
  AwsCodeBuildProjectVpcConfig
newAwsCodeBuildProjectVpcConfig =
  AwsCodeBuildProjectVpcConfig'
    { securityGroupIds =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | A list of one or more security group IDs in your VPC.
awsCodeBuildProjectVpcConfig_securityGroupIds :: Lens.Lens' AwsCodeBuildProjectVpcConfig (Prelude.Maybe [Prelude.Text])
awsCodeBuildProjectVpcConfig_securityGroupIds = Lens.lens (\AwsCodeBuildProjectVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@AwsCodeBuildProjectVpcConfig' {} a -> s {securityGroupIds = a} :: AwsCodeBuildProjectVpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
awsCodeBuildProjectVpcConfig_vpcId :: Lens.Lens' AwsCodeBuildProjectVpcConfig (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectVpcConfig_vpcId = Lens.lens (\AwsCodeBuildProjectVpcConfig' {vpcId} -> vpcId) (\s@AwsCodeBuildProjectVpcConfig' {} a -> s {vpcId = a} :: AwsCodeBuildProjectVpcConfig)

-- | A list of one or more subnet IDs in your VPC.
awsCodeBuildProjectVpcConfig_subnets :: Lens.Lens' AwsCodeBuildProjectVpcConfig (Prelude.Maybe [Prelude.Text])
awsCodeBuildProjectVpcConfig_subnets = Lens.lens (\AwsCodeBuildProjectVpcConfig' {subnets} -> subnets) (\s@AwsCodeBuildProjectVpcConfig' {} a -> s {subnets = a} :: AwsCodeBuildProjectVpcConfig) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsCodeBuildProjectVpcConfig where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectVpcConfig"
      ( \x ->
          AwsCodeBuildProjectVpcConfig'
            Prelude.<$> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "Subnets" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectVpcConfig
  where
  hashWithSalt salt' AwsCodeBuildProjectVpcConfig' {..} =
    salt' `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData AwsCodeBuildProjectVpcConfig where
  rnf AwsCodeBuildProjectVpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToJSON AwsCodeBuildProjectVpcConfig where
  toJSON AwsCodeBuildProjectVpcConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("Subnets" Core..=) Prelude.<$> subnets
          ]
      )
