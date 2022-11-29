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
-- Module      : Amazonka.DeviceFarm.Types.TestGridVpcConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridVpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC security groups and subnets that are attached to a project.
--
-- /See:/ 'newTestGridVpcConfig' smart constructor.
data TestGridVpcConfig = TestGridVpcConfig'
  { -- | A list of VPC security group IDs in your Amazon VPC.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | A list of VPC subnet IDs in your Amazon VPC.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the Amazon VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestGridVpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'testGridVpcConfig_securityGroupIds' - A list of VPC security group IDs in your Amazon VPC.
--
-- 'subnetIds', 'testGridVpcConfig_subnetIds' - A list of VPC subnet IDs in your Amazon VPC.
--
-- 'vpcId', 'testGridVpcConfig_vpcId' - The ID of the Amazon VPC.
newTestGridVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  TestGridVpcConfig
newTestGridVpcConfig
  pSecurityGroupIds_
  pSubnetIds_
  pVpcId_ =
    TestGridVpcConfig'
      { securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        subnetIds = Lens.coerced Lens.# pSubnetIds_,
        vpcId = pVpcId_
      }

-- | A list of VPC security group IDs in your Amazon VPC.
testGridVpcConfig_securityGroupIds :: Lens.Lens' TestGridVpcConfig (Prelude.NonEmpty Prelude.Text)
testGridVpcConfig_securityGroupIds = Lens.lens (\TestGridVpcConfig' {securityGroupIds} -> securityGroupIds) (\s@TestGridVpcConfig' {} a -> s {securityGroupIds = a} :: TestGridVpcConfig) Prelude.. Lens.coerced

-- | A list of VPC subnet IDs in your Amazon VPC.
testGridVpcConfig_subnetIds :: Lens.Lens' TestGridVpcConfig (Prelude.NonEmpty Prelude.Text)
testGridVpcConfig_subnetIds = Lens.lens (\TestGridVpcConfig' {subnetIds} -> subnetIds) (\s@TestGridVpcConfig' {} a -> s {subnetIds = a} :: TestGridVpcConfig) Prelude.. Lens.coerced

-- | The ID of the Amazon VPC.
testGridVpcConfig_vpcId :: Lens.Lens' TestGridVpcConfig Prelude.Text
testGridVpcConfig_vpcId = Lens.lens (\TestGridVpcConfig' {vpcId} -> vpcId) (\s@TestGridVpcConfig' {} a -> s {vpcId = a} :: TestGridVpcConfig)

instance Core.FromJSON TestGridVpcConfig where
  parseJSON =
    Core.withObject
      "TestGridVpcConfig"
      ( \x ->
          TestGridVpcConfig'
            Prelude.<$> (x Core..: "securityGroupIds")
            Prelude.<*> (x Core..: "subnetIds")
            Prelude.<*> (x Core..: "vpcId")
      )

instance Prelude.Hashable TestGridVpcConfig where
  hashWithSalt _salt TestGridVpcConfig' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData TestGridVpcConfig where
  rnf TestGridVpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToJSON TestGridVpcConfig where
  toJSON TestGridVpcConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("securityGroupIds" Core..= securityGroupIds),
            Prelude.Just ("subnetIds" Core..= subnetIds),
            Prelude.Just ("vpcId" Core..= vpcId)
          ]
      )
