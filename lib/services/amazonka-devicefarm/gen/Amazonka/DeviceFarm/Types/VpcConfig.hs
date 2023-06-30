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
-- Module      : Amazonka.DeviceFarm.Types.VpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.VpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the VPC configuration data necessary to interface with AWS
-- Device Farm\'s services.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | An array of one or more security groups IDs in your Amazon VPC.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | An array of one or more subnet IDs in your Amazon VPC.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the Amazon VPC.
    vpcId :: Prelude.Text
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
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - An array of one or more security groups IDs in your Amazon VPC.
--
-- 'subnetIds', 'vpcConfig_subnetIds' - An array of one or more subnet IDs in your Amazon VPC.
--
-- 'vpcId', 'vpcConfig_vpcId' - The ID of the Amazon VPC.
newVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  VpcConfig
newVpcConfig pSecurityGroupIds_ pSubnetIds_ pVpcId_ =
  VpcConfig'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_,
      subnetIds = Lens.coerced Lens.# pSubnetIds_,
      vpcId = pVpcId_
    }

-- | An array of one or more security groups IDs in your Amazon VPC.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Prelude.. Lens.coerced

-- | An array of one or more subnet IDs in your Amazon VPC.
vpcConfig_subnetIds :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_subnetIds = Lens.lens (\VpcConfig' {subnetIds} -> subnetIds) (\s@VpcConfig' {} a -> s {subnetIds = a} :: VpcConfig) Prelude.. Lens.coerced

-- | The ID of the Amazon VPC.
vpcConfig_vpcId :: Lens.Lens' VpcConfig Prelude.Text
vpcConfig_vpcId = Lens.lens (\VpcConfig' {vpcId} -> vpcId) (\s@VpcConfig' {} a -> s {vpcId = a} :: VpcConfig)

instance Data.FromJSON VpcConfig where
  parseJSON =
    Data.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Prelude.<$> (x Data..: "securityGroupIds")
            Prelude.<*> (x Data..: "subnetIds")
            Prelude.<*> (x Data..: "vpcId")
      )

instance Prelude.Hashable VpcConfig where
  hashWithSalt _salt VpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfig where
  rnf VpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("securityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("subnetIds" Data..= subnetIds),
            Prelude.Just ("vpcId" Data..= vpcId)
          ]
      )
