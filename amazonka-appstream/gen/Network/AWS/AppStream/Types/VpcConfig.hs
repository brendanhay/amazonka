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
-- Module      : Network.AWS.AppStream.Types.VpcConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VpcConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes VPC configuration information for fleets and image builders.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | The identifiers of the security groups for the fleet or image builder.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The identifiers of the subnets to which a network interface is attached
    -- from the fleet instance or image builder instance. Fleet instances use
    -- one or more subnets. Image builder instances use one subnet.
    subnetIds :: Core.Maybe [Core.Text]
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
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - The identifiers of the security groups for the fleet or image builder.
--
-- 'subnetIds', 'vpcConfig_subnetIds' - The identifiers of the subnets to which a network interface is attached
-- from the fleet instance or image builder instance. Fleet instances use
-- one or more subnets. Image builder instances use one subnet.
newVpcConfig ::
  VpcConfig
newVpcConfig =
  VpcConfig'
    { securityGroupIds = Core.Nothing,
      subnetIds = Core.Nothing
    }

-- | The identifiers of the security groups for the fleet or image builder.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Core.. Lens.mapping Lens._Coerce

-- | The identifiers of the subnets to which a network interface is attached
-- from the fleet instance or image builder instance. Fleet instances use
-- one or more subnets. Image builder instances use one subnet.
vpcConfig_subnetIds :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vpcConfig_subnetIds = Lens.lens (\VpcConfig' {subnetIds} -> subnetIds) (\s@VpcConfig' {} a -> s {subnetIds = a} :: VpcConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON VpcConfig where
  parseJSON =
    Core.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Core.<$> (x Core..:? "SecurityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
      )

instance Core.Hashable VpcConfig

instance Core.NFData VpcConfig

instance Core.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("SubnetIds" Core..=) Core.<$> subnetIds
          ]
      )
