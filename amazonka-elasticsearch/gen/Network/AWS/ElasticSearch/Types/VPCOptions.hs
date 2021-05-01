{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
vPCOptions_securityGroupIds = Lens.lens (\VPCOptions' {securityGroupIds} -> securityGroupIds) (\s@VPCOptions' {} a -> s {securityGroupIds = a} :: VPCOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the subnets for VPC endpoint.
vPCOptions_subnetIds :: Lens.Lens' VPCOptions (Prelude.Maybe [Prelude.Text])
vPCOptions_subnetIds = Lens.lens (\VPCOptions' {subnetIds} -> subnetIds) (\s@VPCOptions' {} a -> s {subnetIds = a} :: VPCOptions) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable VPCOptions

instance Prelude.NFData VPCOptions

instance Prelude.ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Prelude..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Prelude..=) Prelude.<$> subnetIds
          ]
      )
