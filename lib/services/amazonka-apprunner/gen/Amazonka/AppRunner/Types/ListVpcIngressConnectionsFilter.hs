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
-- Module      : Amazonka.AppRunner.Types.ListVpcIngressConnectionsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ListVpcIngressConnectionsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns a list of VPC Ingress Connections based on the filter provided.
-- It can return either @ServiceArn@ or @VpcEndpointId@, or both.
--
-- /See:/ 'newListVpcIngressConnectionsFilter' smart constructor.
data ListVpcIngressConnectionsFilter = ListVpcIngressConnectionsFilter'
  { -- | The ID of a VPC Endpoint to filter by.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a service to filter by.
    serviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcIngressConnectionsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointId', 'listVpcIngressConnectionsFilter_vpcEndpointId' - The ID of a VPC Endpoint to filter by.
--
-- 'serviceArn', 'listVpcIngressConnectionsFilter_serviceArn' - The Amazon Resource Name (ARN) of a service to filter by.
newListVpcIngressConnectionsFilter ::
  ListVpcIngressConnectionsFilter
newListVpcIngressConnectionsFilter =
  ListVpcIngressConnectionsFilter'
    { vpcEndpointId =
        Prelude.Nothing,
      serviceArn = Prelude.Nothing
    }

-- | The ID of a VPC Endpoint to filter by.
listVpcIngressConnectionsFilter_vpcEndpointId :: Lens.Lens' ListVpcIngressConnectionsFilter (Prelude.Maybe Prelude.Text)
listVpcIngressConnectionsFilter_vpcEndpointId = Lens.lens (\ListVpcIngressConnectionsFilter' {vpcEndpointId} -> vpcEndpointId) (\s@ListVpcIngressConnectionsFilter' {} a -> s {vpcEndpointId = a} :: ListVpcIngressConnectionsFilter)

-- | The Amazon Resource Name (ARN) of a service to filter by.
listVpcIngressConnectionsFilter_serviceArn :: Lens.Lens' ListVpcIngressConnectionsFilter (Prelude.Maybe Prelude.Text)
listVpcIngressConnectionsFilter_serviceArn = Lens.lens (\ListVpcIngressConnectionsFilter' {serviceArn} -> serviceArn) (\s@ListVpcIngressConnectionsFilter' {} a -> s {serviceArn = a} :: ListVpcIngressConnectionsFilter)

instance
  Prelude.Hashable
    ListVpcIngressConnectionsFilter
  where
  hashWithSalt
    _salt
    ListVpcIngressConnectionsFilter' {..} =
      _salt `Prelude.hashWithSalt` vpcEndpointId
        `Prelude.hashWithSalt` serviceArn

instance
  Prelude.NFData
    ListVpcIngressConnectionsFilter
  where
  rnf ListVpcIngressConnectionsFilter' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf serviceArn

instance Data.ToJSON ListVpcIngressConnectionsFilter where
  toJSON ListVpcIngressConnectionsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcEndpointId" Data..=) Prelude.<$> vpcEndpointId,
            ("ServiceArn" Data..=) Prelude.<$> serviceArn
          ]
      )
