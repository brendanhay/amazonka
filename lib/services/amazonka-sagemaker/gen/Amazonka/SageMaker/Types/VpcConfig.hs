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
-- Module      : Amazonka.SageMaker.Types.VpcConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.VpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a VPC that your training jobs and hosted models have access
-- to. Control access to and from your training and model containers by
-- configuring the VPC. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the
    -- security groups for the VPC that is specified in the @Subnets@ field.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the subnets in the VPC to which you want to connect your
    -- training job or model. For information about the availability of
    -- specific instance types, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones>.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. Specify the
-- security groups for the VPC that is specified in the @Subnets@ field.
--
-- 'subnets', 'vpcConfig_subnets' - The ID of the subnets in the VPC to which you want to connect your
-- training job or model. For information about the availability of
-- specific instance types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones>.
newVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfig
newVpcConfig pSecurityGroupIds_ pSubnets_ =
  VpcConfig'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_,
      subnets = Lens.coerced Lens.# pSubnets_
    }

-- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the
-- security groups for the VPC that is specified in the @Subnets@ field.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Prelude.. Lens.coerced

-- | The ID of the subnets in the VPC to which you want to connect your
-- training job or model. For information about the availability of
-- specific instance types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones>.
vpcConfig_subnets :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_subnets = Lens.lens (\VpcConfig' {subnets} -> subnets) (\s@VpcConfig' {} a -> s {subnets = a} :: VpcConfig) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfig where
  parseJSON =
    Data.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Prelude.<$> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable VpcConfig where
  hashWithSalt _salt VpcConfig' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData VpcConfig where
  rnf VpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )
