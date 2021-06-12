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
-- Module      : Network.AWS.Lambda.Types.VpcConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.VpcConfigResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The VPC security groups and subnets that are attached to a Lambda
-- function.
--
-- /See:/ 'newVpcConfigResponse' smart constructor.
data VpcConfigResponse = VpcConfigResponse'
  { -- | A list of VPC security groups IDs.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | A list of VPC subnet IDs.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigResponse_securityGroupIds' - A list of VPC security groups IDs.
--
-- 'subnetIds', 'vpcConfigResponse_subnetIds' - A list of VPC subnet IDs.
--
-- 'vpcId', 'vpcConfigResponse_vpcId' - The ID of the VPC.
newVpcConfigResponse ::
  VpcConfigResponse
newVpcConfigResponse =
  VpcConfigResponse'
    { securityGroupIds = Core.Nothing,
      subnetIds = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A list of VPC security groups IDs.
vpcConfigResponse_securityGroupIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Core.Text])
vpcConfigResponse_securityGroupIds = Lens.lens (\VpcConfigResponse' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigResponse' {} a -> s {securityGroupIds = a} :: VpcConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of VPC subnet IDs.
vpcConfigResponse_subnetIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Core.Text])
vpcConfigResponse_subnetIds = Lens.lens (\VpcConfigResponse' {subnetIds} -> subnetIds) (\s@VpcConfigResponse' {} a -> s {subnetIds = a} :: VpcConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC.
vpcConfigResponse_vpcId :: Lens.Lens' VpcConfigResponse (Core.Maybe Core.Text)
vpcConfigResponse_vpcId = Lens.lens (\VpcConfigResponse' {vpcId} -> vpcId) (\s@VpcConfigResponse' {} a -> s {vpcId = a} :: VpcConfigResponse)

instance Core.FromJSON VpcConfigResponse where
  parseJSON =
    Core.withObject
      "VpcConfigResponse"
      ( \x ->
          VpcConfigResponse'
            Core.<$> (x Core..:? "SecurityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VpcId")
      )

instance Core.Hashable VpcConfigResponse

instance Core.NFData VpcConfigResponse
