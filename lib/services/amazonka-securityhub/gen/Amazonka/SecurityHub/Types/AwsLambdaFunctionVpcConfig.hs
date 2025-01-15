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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionVpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VPC security groups and subnets that are attached to a Lambda
-- function.
--
-- /See:/ 'newAwsLambdaFunctionVpcConfig' smart constructor.
data AwsLambdaFunctionVpcConfig = AwsLambdaFunctionVpcConfig'
  { -- | A list of VPC security groups IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of VPC subnet IDs.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'awsLambdaFunctionVpcConfig_securityGroupIds' - A list of VPC security groups IDs.
--
-- 'subnetIds', 'awsLambdaFunctionVpcConfig_subnetIds' - A list of VPC subnet IDs.
--
-- 'vpcId', 'awsLambdaFunctionVpcConfig_vpcId' - The ID of the VPC.
newAwsLambdaFunctionVpcConfig ::
  AwsLambdaFunctionVpcConfig
newAwsLambdaFunctionVpcConfig =
  AwsLambdaFunctionVpcConfig'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A list of VPC security groups IDs.
awsLambdaFunctionVpcConfig_securityGroupIds :: Lens.Lens' AwsLambdaFunctionVpcConfig (Prelude.Maybe [Prelude.Text])
awsLambdaFunctionVpcConfig_securityGroupIds = Lens.lens (\AwsLambdaFunctionVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@AwsLambdaFunctionVpcConfig' {} a -> s {securityGroupIds = a} :: AwsLambdaFunctionVpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC subnet IDs.
awsLambdaFunctionVpcConfig_subnetIds :: Lens.Lens' AwsLambdaFunctionVpcConfig (Prelude.Maybe [Prelude.Text])
awsLambdaFunctionVpcConfig_subnetIds = Lens.lens (\AwsLambdaFunctionVpcConfig' {subnetIds} -> subnetIds) (\s@AwsLambdaFunctionVpcConfig' {} a -> s {subnetIds = a} :: AwsLambdaFunctionVpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
awsLambdaFunctionVpcConfig_vpcId :: Lens.Lens' AwsLambdaFunctionVpcConfig (Prelude.Maybe Prelude.Text)
awsLambdaFunctionVpcConfig_vpcId = Lens.lens (\AwsLambdaFunctionVpcConfig' {vpcId} -> vpcId) (\s@AwsLambdaFunctionVpcConfig' {} a -> s {vpcId = a} :: AwsLambdaFunctionVpcConfig)

instance Data.FromJSON AwsLambdaFunctionVpcConfig where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionVpcConfig"
      ( \x ->
          AwsLambdaFunctionVpcConfig'
            Prelude.<$> ( x
                            Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsLambdaFunctionVpcConfig where
  hashWithSalt _salt AwsLambdaFunctionVpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsLambdaFunctionVpcConfig where
  rnf AwsLambdaFunctionVpcConfig' {..} =
    Prelude.rnf securityGroupIds `Prelude.seq`
      Prelude.rnf subnetIds `Prelude.seq`
        Prelude.rnf vpcId

instance Data.ToJSON AwsLambdaFunctionVpcConfig where
  toJSON AwsLambdaFunctionVpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
