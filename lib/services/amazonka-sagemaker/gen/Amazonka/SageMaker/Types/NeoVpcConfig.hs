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
-- Module      : Amazonka.SageMaker.Types.NeoVpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NeoVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VpcConfig configuration object that specifies the VPC that you want
-- the compilation jobs to connect to. For more information on controlling
-- access to your Amazon S3 buckets used for compilation job, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/neo-vpc.html Give Amazon SageMaker Compilation Jobs Access to Resources in Your Amazon VPC>.
--
-- /See:/ 'newNeoVpcConfig' smart constructor.
data NeoVpcConfig = NeoVpcConfig'
  { -- | The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
    -- the security groups for the VPC that is specified in the @Subnets@
    -- field.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the subnets in the VPC that you want to connect the
    -- compilation job to for accessing the model in Amazon S3.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NeoVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'neoVpcConfig_securityGroupIds' - The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
-- the security groups for the VPC that is specified in the @Subnets@
-- field.
--
-- 'subnets', 'neoVpcConfig_subnets' - The ID of the subnets in the VPC that you want to connect the
-- compilation job to for accessing the model in Amazon S3.
newNeoVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  NeoVpcConfig
newNeoVpcConfig pSecurityGroupIds_ pSubnets_ =
  NeoVpcConfig'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_,
      subnets = Lens.coerced Lens.# pSubnets_
    }

-- | The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
-- the security groups for the VPC that is specified in the @Subnets@
-- field.
neoVpcConfig_securityGroupIds :: Lens.Lens' NeoVpcConfig (Prelude.NonEmpty Prelude.Text)
neoVpcConfig_securityGroupIds = Lens.lens (\NeoVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@NeoVpcConfig' {} a -> s {securityGroupIds = a} :: NeoVpcConfig) Prelude.. Lens.coerced

-- | The ID of the subnets in the VPC that you want to connect the
-- compilation job to for accessing the model in Amazon S3.
neoVpcConfig_subnets :: Lens.Lens' NeoVpcConfig (Prelude.NonEmpty Prelude.Text)
neoVpcConfig_subnets = Lens.lens (\NeoVpcConfig' {subnets} -> subnets) (\s@NeoVpcConfig' {} a -> s {subnets = a} :: NeoVpcConfig) Prelude.. Lens.coerced

instance Data.FromJSON NeoVpcConfig where
  parseJSON =
    Data.withObject
      "NeoVpcConfig"
      ( \x ->
          NeoVpcConfig'
            Prelude.<$> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable NeoVpcConfig where
  hashWithSalt _salt NeoVpcConfig' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData NeoVpcConfig where
  rnf NeoVpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON NeoVpcConfig where
  toJSON NeoVpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )
