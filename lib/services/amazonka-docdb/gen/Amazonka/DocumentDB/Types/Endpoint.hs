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
-- Module      : Amazonka.DocumentDB.Types.Endpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network information for accessing a cluster or instance. Client programs
-- must specify a valid endpoint to access these Amazon DocumentDB
-- resources.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DNS address of the instance.
    address :: Prelude.Maybe Prelude.Text
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
-- 'port', 'endpoint_port' - Specifies the port that the database engine is listening on.
--
-- 'hostedZoneId', 'endpoint_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'address', 'endpoint_address' - Specifies the DNS address of the instance.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { port = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      address = Prelude.Nothing
    }

-- | Specifies the port that the database engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
endpoint_hostedZoneId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_hostedZoneId = Lens.lens (\Endpoint' {hostedZoneId} -> hostedZoneId) (\s@Endpoint' {} a -> s {hostedZoneId = a} :: Endpoint)

-- | Specifies the DNS address of the instance.
endpoint_address :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

instance Data.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Prelude.<$> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "HostedZoneId")
      Prelude.<*> (x Data..@? "Address")

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` address

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf address
