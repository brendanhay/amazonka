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
-- Module      : Network.AWS.Redshift.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Endpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.VpcEndpoint

-- | Describes a connection endpoint.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The DNS address of the Cluster.
    address :: Core.Maybe Core.Text,
    -- | Describes a connection endpoint.
    vpcEndpoints :: Core.Maybe [VpcEndpoint],
    -- | The port that the database engine is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpoint_address' - The DNS address of the Cluster.
--
-- 'vpcEndpoints', 'endpoint_vpcEndpoints' - Describes a connection endpoint.
--
-- 'port', 'endpoint_port' - The port that the database engine is listening on.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { address = Core.Nothing,
      vpcEndpoints = Core.Nothing,
      port = Core.Nothing
    }

-- | The DNS address of the Cluster.
endpoint_address :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | Describes a connection endpoint.
endpoint_vpcEndpoints :: Lens.Lens' Endpoint (Core.Maybe [VpcEndpoint])
endpoint_vpcEndpoints = Lens.lens (\Endpoint' {vpcEndpoints} -> vpcEndpoints) (\s@Endpoint' {} a -> s {vpcEndpoints = a} :: Endpoint) Core.. Lens.mapping Lens._Coerce

-- | The port that the database engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Core.<$> (x Core..@? "Address")
      Core.<*> ( x Core..@? "VpcEndpoints" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "VpcEndpoint")
               )
      Core.<*> (x Core..@? "Port")

instance Core.Hashable Endpoint

instance Core.NFData Endpoint
