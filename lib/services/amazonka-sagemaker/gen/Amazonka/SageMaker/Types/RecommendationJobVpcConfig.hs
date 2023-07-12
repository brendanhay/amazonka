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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobVpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inference Recommender provisions SageMaker endpoints with access to VPC
-- in the inference recommendation job.
--
-- /See:/ 'newRecommendationJobVpcConfig' smart constructor.
data RecommendationJobVpcConfig = RecommendationJobVpcConfig'
  { -- | The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
    -- the security groups for the VPC that is specified in the @Subnets@
    -- field.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the subnets in the VPC to which you want to connect your
    -- model.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'recommendationJobVpcConfig_securityGroupIds' - The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
-- the security groups for the VPC that is specified in the @Subnets@
-- field.
--
-- 'subnets', 'recommendationJobVpcConfig_subnets' - The ID of the subnets in the VPC to which you want to connect your
-- model.
newRecommendationJobVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  RecommendationJobVpcConfig
newRecommendationJobVpcConfig
  pSecurityGroupIds_
  pSubnets_ =
    RecommendationJobVpcConfig'
      { securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        subnets = Lens.coerced Lens.# pSubnets_
      }

-- | The VPC security group IDs. IDs have the form of @sg-xxxxxxxx@. Specify
-- the security groups for the VPC that is specified in the @Subnets@
-- field.
recommendationJobVpcConfig_securityGroupIds :: Lens.Lens' RecommendationJobVpcConfig (Prelude.NonEmpty Prelude.Text)
recommendationJobVpcConfig_securityGroupIds = Lens.lens (\RecommendationJobVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@RecommendationJobVpcConfig' {} a -> s {securityGroupIds = a} :: RecommendationJobVpcConfig) Prelude.. Lens.coerced

-- | The ID of the subnets in the VPC to which you want to connect your
-- model.
recommendationJobVpcConfig_subnets :: Lens.Lens' RecommendationJobVpcConfig (Prelude.NonEmpty Prelude.Text)
recommendationJobVpcConfig_subnets = Lens.lens (\RecommendationJobVpcConfig' {subnets} -> subnets) (\s@RecommendationJobVpcConfig' {} a -> s {subnets = a} :: RecommendationJobVpcConfig) Prelude.. Lens.coerced

instance Data.FromJSON RecommendationJobVpcConfig where
  parseJSON =
    Data.withObject
      "RecommendationJobVpcConfig"
      ( \x ->
          RecommendationJobVpcConfig'
            Prelude.<$> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable RecommendationJobVpcConfig where
  hashWithSalt _salt RecommendationJobVpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData RecommendationJobVpcConfig where
  rnf RecommendationJobVpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON RecommendationJobVpcConfig where
  toJSON RecommendationJobVpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )
