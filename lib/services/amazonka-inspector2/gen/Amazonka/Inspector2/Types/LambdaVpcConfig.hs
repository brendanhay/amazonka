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
-- Module      : Amazonka.Inspector2.Types.LambdaVpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VPC security groups and subnets that are attached to an AWS Lambda
-- function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- /See:/ 'newLambdaVpcConfig' smart constructor.
data LambdaVpcConfig = LambdaVpcConfig'
  { -- | The VPC security groups and subnets that are attached to an AWS Lambda
    -- function. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of VPC subnet IDs.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'lambdaVpcConfig_securityGroupIds' - The VPC security groups and subnets that are attached to an AWS Lambda
-- function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
--
-- 'subnetIds', 'lambdaVpcConfig_subnetIds' - A list of VPC subnet IDs.
--
-- 'vpcId', 'lambdaVpcConfig_vpcId' - The ID of the VPC.
newLambdaVpcConfig ::
  LambdaVpcConfig
newLambdaVpcConfig =
  LambdaVpcConfig'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The VPC security groups and subnets that are attached to an AWS Lambda
-- function. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings>.
lambdaVpcConfig_securityGroupIds :: Lens.Lens' LambdaVpcConfig (Prelude.Maybe [Prelude.Text])
lambdaVpcConfig_securityGroupIds = Lens.lens (\LambdaVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@LambdaVpcConfig' {} a -> s {securityGroupIds = a} :: LambdaVpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC subnet IDs.
lambdaVpcConfig_subnetIds :: Lens.Lens' LambdaVpcConfig (Prelude.Maybe [Prelude.Text])
lambdaVpcConfig_subnetIds = Lens.lens (\LambdaVpcConfig' {subnetIds} -> subnetIds) (\s@LambdaVpcConfig' {} a -> s {subnetIds = a} :: LambdaVpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
lambdaVpcConfig_vpcId :: Lens.Lens' LambdaVpcConfig (Prelude.Maybe Prelude.Text)
lambdaVpcConfig_vpcId = Lens.lens (\LambdaVpcConfig' {vpcId} -> vpcId) (\s@LambdaVpcConfig' {} a -> s {vpcId = a} :: LambdaVpcConfig)

instance Data.FromJSON LambdaVpcConfig where
  parseJSON =
    Data.withObject
      "LambdaVpcConfig"
      ( \x ->
          LambdaVpcConfig'
            Prelude.<$> ( x
                            Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable LambdaVpcConfig where
  hashWithSalt _salt LambdaVpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData LambdaVpcConfig where
  rnf LambdaVpcConfig' {..} =
    Prelude.rnf securityGroupIds `Prelude.seq`
      Prelude.rnf subnetIds `Prelude.seq`
        Prelude.rnf vpcId
