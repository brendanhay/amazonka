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
-- Module      : Amazonka.RedshiftServerLess.Types.VpcEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.VpcEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.NetworkInterface

-- | The connection endpoint for connecting to Amazon Redshift Serverless
-- through the proxy.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The connection endpoint ID for connecting to Amazon Redshift Serverless.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The VPC identifier that the endpoint is associated with.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces of the endpoint. Also known as an
    -- interface endpoint.
    networkInterfaces :: Prelude.Maybe [NetworkInterface]
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
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The connection endpoint ID for connecting to Amazon Redshift Serverless.
--
-- 'vpcId', 'vpcEndpoint_vpcId' - The VPC identifier that the endpoint is associated with.
--
-- 'networkInterfaces', 'vpcEndpoint_networkInterfaces' - One or more network interfaces of the endpoint. Also known as an
-- interface endpoint.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { vpcEndpointId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | The connection endpoint ID for connecting to Amazon Redshift Serverless.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

-- | The VPC identifier that the endpoint is associated with.
vpcEndpoint_vpcId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcId = Lens.lens (\VpcEndpoint' {vpcId} -> vpcId) (\s@VpcEndpoint' {} a -> s {vpcId = a} :: VpcEndpoint)

-- | One or more network interfaces of the endpoint. Also known as an
-- interface endpoint.
vpcEndpoint_networkInterfaces :: Lens.Lens' VpcEndpoint (Prelude.Maybe [NetworkInterface])
vpcEndpoint_networkInterfaces = Lens.lens (\VpcEndpoint' {networkInterfaces} -> networkInterfaces) (\s@VpcEndpoint' {} a -> s {networkInterfaces = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VpcEndpoint where
  parseJSON =
    Data.withObject
      "VpcEndpoint"
      ( \x ->
          VpcEndpoint'
            Prelude.<$> (x Data..:? "vpcEndpointId")
            Prelude.<*> (x Data..:? "vpcId")
            Prelude.<*> ( x Data..:? "networkInterfaces"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VpcEndpoint where
  hashWithSalt _salt VpcEndpoint' {..} =
    _salt `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkInterfaces

instance Prelude.NFData VpcEndpoint where
  rnf VpcEndpoint' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkInterfaces
