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
-- Module      : Network.AWS.ElasticSearch.Types.VPCOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- /See:/ 'newVPCOptions' smart constructor.
data VPCOptions = VPCOptions'
  { -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VPCOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vPCOptions_securityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- 'subnetIds', 'vPCOptions_subnetIds' - Specifies the subnets for VPC endpoint.
newVPCOptions ::
  VPCOptions
newVPCOptions =
  VPCOptions'
    { securityGroupIds = Core.Nothing,
      subnetIds = Core.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
vPCOptions_securityGroupIds :: Lens.Lens' VPCOptions (Core.Maybe [Core.Text])
vPCOptions_securityGroupIds = Lens.lens (\VPCOptions' {securityGroupIds} -> securityGroupIds) (\s@VPCOptions' {} a -> s {securityGroupIds = a} :: VPCOptions) Core.. Lens.mapping Lens._Coerce

-- | Specifies the subnets for VPC endpoint.
vPCOptions_subnetIds :: Lens.Lens' VPCOptions (Core.Maybe [Core.Text])
vPCOptions_subnetIds = Lens.lens (\VPCOptions' {subnetIds} -> subnetIds) (\s@VPCOptions' {} a -> s {subnetIds = a} :: VPCOptions) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable VPCOptions

instance Core.NFData VPCOptions

instance Core.ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("SubnetIds" Core..=) Core.<$> subnetIds
          ]
      )
