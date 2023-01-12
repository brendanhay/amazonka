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
-- Module      : Amazonka.ElasticSearch.Types.VPCOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.VPCOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- /See:/ 'newVPCOptions' smart constructor.
data VPCOptions = VPCOptions'
  { -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the subnets for VPC endpoint.
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
-- 'securityGroupIds', 'vPCOptions_securityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- 'subnetIds', 'vPCOptions_subnetIds' - Specifies the subnets for VPC endpoint.
newVPCOptions ::
  VPCOptions
newVPCOptions =
  VPCOptions'
    { securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
vPCOptions_securityGroupIds :: Lens.Lens' VPCOptions (Prelude.Maybe [Prelude.Text])
vPCOptions_securityGroupIds = Lens.lens (\VPCOptions' {securityGroupIds} -> securityGroupIds) (\s@VPCOptions' {} a -> s {securityGroupIds = a} :: VPCOptions) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the subnets for VPC endpoint.
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

instance Data.ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds
          ]
      )
