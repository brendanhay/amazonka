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
-- Module      : Amazonka.OpenSearch.Types.VPCOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VPCOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options to specify the subnets and security groups for the VPC endpoint.
-- For more information, see
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
--
-- /See:/ 'newVPCOptions' smart constructor.
data VPCOptions = VPCOptions'
  { -- | The security groups for the VPC endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The subnets for the VPC endpoint.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vPCOptions_securityGroupIds' - The security groups for the VPC endpoint.
--
-- 'subnetIds', 'vPCOptions_subnetIds' - The subnets for the VPC endpoint.
newVPCOptions ::
  VPCOptions
newVPCOptions =
  VPCOptions'
    { securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The security groups for the VPC endpoint.
vPCOptions_securityGroupIds :: Lens.Lens' VPCOptions (Prelude.Maybe [Prelude.Text])
vPCOptions_securityGroupIds = Lens.lens (\VPCOptions' {securityGroupIds} -> securityGroupIds) (\s@VPCOptions' {} a -> s {securityGroupIds = a} :: VPCOptions) Prelude.. Lens.mapping Lens.coerced

-- | The subnets for the VPC endpoint.
vPCOptions_subnetIds :: Lens.Lens' VPCOptions (Prelude.Maybe [Prelude.Text])
vPCOptions_subnetIds = Lens.lens (\VPCOptions' {subnetIds} -> subnetIds) (\s@VPCOptions' {} a -> s {subnetIds = a} :: VPCOptions) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable VPCOptions where
  hashWithSalt _salt VPCOptions' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VPCOptions where
  rnf VPCOptions' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Core.ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Core..=) Prelude.<$> subnetIds
          ]
      )
