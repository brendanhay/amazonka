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
-- Module      : Amazonka.OsIs.Types.VpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.VpcEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types.VpcOptions
import qualified Amazonka.Prelude as Prelude

-- | An OpenSearch Ingestion-managed VPC endpoint that will access one or
-- more pipelines.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The unique identifier of the endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID for your VPC. Amazon Web Services PrivateLink generates this
    -- value when you create a VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC, including associated subnets and security
    -- groups.
    vpcOptions :: Prelude.Maybe VpcOptions
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
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The unique identifier of the endpoint.
--
-- 'vpcId', 'vpcEndpoint_vpcId' - The ID for your VPC. Amazon Web Services PrivateLink generates this
-- value when you create a VPC.
--
-- 'vpcOptions', 'vpcEndpoint_vpcOptions' - Information about the VPC, including associated subnets and security
-- groups.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { vpcEndpointId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcOptions = Prelude.Nothing
    }

-- | The unique identifier of the endpoint.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

-- | The ID for your VPC. Amazon Web Services PrivateLink generates this
-- value when you create a VPC.
vpcEndpoint_vpcId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcId = Lens.lens (\VpcEndpoint' {vpcId} -> vpcId) (\s@VpcEndpoint' {} a -> s {vpcId = a} :: VpcEndpoint)

-- | Information about the VPC, including associated subnets and security
-- groups.
vpcEndpoint_vpcOptions :: Lens.Lens' VpcEndpoint (Prelude.Maybe VpcOptions)
vpcEndpoint_vpcOptions = Lens.lens (\VpcEndpoint' {vpcOptions} -> vpcOptions) (\s@VpcEndpoint' {} a -> s {vpcOptions = a} :: VpcEndpoint)

instance Data.FromJSON VpcEndpoint where
  parseJSON =
    Data.withObject
      "VpcEndpoint"
      ( \x ->
          VpcEndpoint'
            Prelude.<$> (x Data..:? "VpcEndpointId")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "VpcOptions")
      )

instance Prelude.Hashable VpcEndpoint where
  hashWithSalt _salt VpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcOptions

instance Prelude.NFData VpcEndpoint where
  rnf VpcEndpoint' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcOptions
