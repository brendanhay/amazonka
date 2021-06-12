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
-- Module      : Network.AWS.CodeBuild.Types.VpcConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.VpcConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | A list of one or more security groups IDs in your Amazon VPC.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The ID of the Amazon VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | A list of one or more subnet IDs in your Amazon VPC.
    subnets :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - A list of one or more security groups IDs in your Amazon VPC.
--
-- 'vpcId', 'vpcConfig_vpcId' - The ID of the Amazon VPC.
--
-- 'subnets', 'vpcConfig_subnets' - A list of one or more subnet IDs in your Amazon VPC.
newVpcConfig ::
  VpcConfig
newVpcConfig =
  VpcConfig'
    { securityGroupIds = Core.Nothing,
      vpcId = Core.Nothing,
      subnets = Core.Nothing
    }

-- | A list of one or more security groups IDs in your Amazon VPC.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Amazon VPC.
vpcConfig_vpcId :: Lens.Lens' VpcConfig (Core.Maybe Core.Text)
vpcConfig_vpcId = Lens.lens (\VpcConfig' {vpcId} -> vpcId) (\s@VpcConfig' {} a -> s {vpcId = a} :: VpcConfig)

-- | A list of one or more subnet IDs in your Amazon VPC.
vpcConfig_subnets :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vpcConfig_subnets = Lens.lens (\VpcConfig' {subnets} -> subnets) (\s@VpcConfig' {} a -> s {subnets = a} :: VpcConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON VpcConfig where
  parseJSON =
    Core.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Core.<$> (x Core..:? "securityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpcId")
            Core.<*> (x Core..:? "subnets" Core..!= Core.mempty)
      )

instance Core.Hashable VpcConfig

instance Core.NFData VpcConfig

instance Core.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("securityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("vpcId" Core..=) Core.<$> vpcId,
            ("subnets" Core..=) Core.<$> subnets
          ]
      )
