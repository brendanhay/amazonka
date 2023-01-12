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
-- Module      : Amazonka.OpenSearch.Types.VpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VpcEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.VPCDerivedInfo
import Amazonka.OpenSearch.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | The connection endpoint for connecting to an Amazon OpenSearch Service
-- domain through a proxy.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The Amazon Resource Name (ARN) of the domain associated with the
    -- endpoint.
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The connection endpoint ID for connecting to the domain.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The current status of the endpoint.
    status :: Prelude.Maybe VpcEndpointStatus,
    -- | The unique identifier of the endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The creator of the endpoint.
    vpcEndpointOwner :: Prelude.Maybe Prelude.Text,
    -- | Options to specify the subnets and security groups for an Amazon
    -- OpenSearch Service VPC endpoint.
    vpcOptions :: Prelude.Maybe VPCDerivedInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainArn', 'vpcEndpoint_domainArn' - The Amazon Resource Name (ARN) of the domain associated with the
-- endpoint.
--
-- 'endpoint', 'vpcEndpoint_endpoint' - The connection endpoint ID for connecting to the domain.
--
-- 'status', 'vpcEndpoint_status' - The current status of the endpoint.
--
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The unique identifier of the endpoint.
--
-- 'vpcEndpointOwner', 'vpcEndpoint_vpcEndpointOwner' - The creator of the endpoint.
--
-- 'vpcOptions', 'vpcEndpoint_vpcOptions' - Options to specify the subnets and security groups for an Amazon
-- OpenSearch Service VPC endpoint.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { domainArn = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      vpcEndpointOwner = Prelude.Nothing,
      vpcOptions = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the domain associated with the
-- endpoint.
vpcEndpoint_domainArn :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_domainArn = Lens.lens (\VpcEndpoint' {domainArn} -> domainArn) (\s@VpcEndpoint' {} a -> s {domainArn = a} :: VpcEndpoint)

-- | The connection endpoint ID for connecting to the domain.
vpcEndpoint_endpoint :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_endpoint = Lens.lens (\VpcEndpoint' {endpoint} -> endpoint) (\s@VpcEndpoint' {} a -> s {endpoint = a} :: VpcEndpoint)

-- | The current status of the endpoint.
vpcEndpoint_status :: Lens.Lens' VpcEndpoint (Prelude.Maybe VpcEndpointStatus)
vpcEndpoint_status = Lens.lens (\VpcEndpoint' {status} -> status) (\s@VpcEndpoint' {} a -> s {status = a} :: VpcEndpoint)

-- | The unique identifier of the endpoint.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

-- | The creator of the endpoint.
vpcEndpoint_vpcEndpointOwner :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointOwner = Lens.lens (\VpcEndpoint' {vpcEndpointOwner} -> vpcEndpointOwner) (\s@VpcEndpoint' {} a -> s {vpcEndpointOwner = a} :: VpcEndpoint)

-- | Options to specify the subnets and security groups for an Amazon
-- OpenSearch Service VPC endpoint.
vpcEndpoint_vpcOptions :: Lens.Lens' VpcEndpoint (Prelude.Maybe VPCDerivedInfo)
vpcEndpoint_vpcOptions = Lens.lens (\VpcEndpoint' {vpcOptions} -> vpcOptions) (\s@VpcEndpoint' {} a -> s {vpcOptions = a} :: VpcEndpoint)

instance Data.FromJSON VpcEndpoint where
  parseJSON =
    Data.withObject
      "VpcEndpoint"
      ( \x ->
          VpcEndpoint'
            Prelude.<$> (x Data..:? "DomainArn")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VpcEndpointId")
            Prelude.<*> (x Data..:? "VpcEndpointOwner")
            Prelude.<*> (x Data..:? "VpcOptions")
      )

instance Prelude.Hashable VpcEndpoint where
  hashWithSalt _salt VpcEndpoint' {..} =
    _salt `Prelude.hashWithSalt` domainArn
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcEndpointOwner
      `Prelude.hashWithSalt` vpcOptions

instance Prelude.NFData VpcEndpoint where
  rnf VpcEndpoint' {..} =
    Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcEndpointOwner
      `Prelude.seq` Prelude.rnf vpcOptions
