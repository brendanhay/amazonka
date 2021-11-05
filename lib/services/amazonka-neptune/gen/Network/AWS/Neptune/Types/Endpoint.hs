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
-- Module      : Amazonka.Neptune.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a connection endpoint.
--
-- For the data structure that represents Amazon Neptune DB cluster
-- endpoints, see @DBClusterEndpoint@.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DNS address of the DB instance.
    address :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'endpoint_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'address', 'endpoint_address' - Specifies the DNS address of the DB instance.
--
-- 'port', 'endpoint_port' - Specifies the port that the database engine is listening on.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { hostedZoneId = Prelude.Nothing,
      address = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
endpoint_hostedZoneId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_hostedZoneId = Lens.lens (\Endpoint' {hostedZoneId} -> hostedZoneId) (\s@Endpoint' {} a -> s {hostedZoneId = a} :: Endpoint)

-- | Specifies the DNS address of the DB instance.
endpoint_address :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | Specifies the port that the database engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Prelude.<$> (x Core..@? "HostedZoneId")
      Prelude.<*> (x Core..@? "Address")
      Prelude.<*> (x Core..@? "Port")

instance Prelude.Hashable Endpoint

instance Prelude.NFData Endpoint
