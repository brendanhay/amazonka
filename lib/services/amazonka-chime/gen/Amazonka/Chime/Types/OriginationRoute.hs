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
-- Module      : Amazonka.Chime.Types.OriginationRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.OriginationRoute where

import Amazonka.Chime.Types.OriginationRouteProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Origination routes define call distribution properties for your SIP
-- hosts to receive inbound calls using your Amazon Chime Voice Connector.
-- Limit: Ten origination routes for each Amazon Chime Voice Connector.
--
-- The parameters listed below are not required, but you must use at least
-- one.
--
-- /See:/ 'newOriginationRoute' smart constructor.
data OriginationRoute = OriginationRoute'
  { -- | The designated origination route port. Defaults to 5060.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The FQDN or IP address to contact for origination traffic.
    host :: Prelude.Maybe Prelude.Text,
    -- | The priority associated with the host, with 1 being the highest
    -- priority. Higher priority hosts are attempted first.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The weight associated with the host. If hosts are equal in priority,
    -- calls are redistributed among them based on their relative weight.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The protocol to use for the origination route. Encryption-enabled Amazon
    -- Chime Voice Connectors use TCP protocol by default.
    protocol :: Prelude.Maybe OriginationRouteProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginationRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'originationRoute_port' - The designated origination route port. Defaults to 5060.
--
-- 'host', 'originationRoute_host' - The FQDN or IP address to contact for origination traffic.
--
-- 'priority', 'originationRoute_priority' - The priority associated with the host, with 1 being the highest
-- priority. Higher priority hosts are attempted first.
--
-- 'weight', 'originationRoute_weight' - The weight associated with the host. If hosts are equal in priority,
-- calls are redistributed among them based on their relative weight.
--
-- 'protocol', 'originationRoute_protocol' - The protocol to use for the origination route. Encryption-enabled Amazon
-- Chime Voice Connectors use TCP protocol by default.
newOriginationRoute ::
  OriginationRoute
newOriginationRoute =
  OriginationRoute'
    { port = Prelude.Nothing,
      host = Prelude.Nothing,
      priority = Prelude.Nothing,
      weight = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The designated origination route port. Defaults to 5060.
originationRoute_port :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Natural)
originationRoute_port = Lens.lens (\OriginationRoute' {port} -> port) (\s@OriginationRoute' {} a -> s {port = a} :: OriginationRoute)

-- | The FQDN or IP address to contact for origination traffic.
originationRoute_host :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Text)
originationRoute_host = Lens.lens (\OriginationRoute' {host} -> host) (\s@OriginationRoute' {} a -> s {host = a} :: OriginationRoute)

-- | The priority associated with the host, with 1 being the highest
-- priority. Higher priority hosts are attempted first.
originationRoute_priority :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Natural)
originationRoute_priority = Lens.lens (\OriginationRoute' {priority} -> priority) (\s@OriginationRoute' {} a -> s {priority = a} :: OriginationRoute)

-- | The weight associated with the host. If hosts are equal in priority,
-- calls are redistributed among them based on their relative weight.
originationRoute_weight :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Natural)
originationRoute_weight = Lens.lens (\OriginationRoute' {weight} -> weight) (\s@OriginationRoute' {} a -> s {weight = a} :: OriginationRoute)

-- | The protocol to use for the origination route. Encryption-enabled Amazon
-- Chime Voice Connectors use TCP protocol by default.
originationRoute_protocol :: Lens.Lens' OriginationRoute (Prelude.Maybe OriginationRouteProtocol)
originationRoute_protocol = Lens.lens (\OriginationRoute' {protocol} -> protocol) (\s@OriginationRoute' {} a -> s {protocol = a} :: OriginationRoute)

instance Data.FromJSON OriginationRoute where
  parseJSON =
    Data.withObject
      "OriginationRoute"
      ( \x ->
          OriginationRoute'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "Host")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "Weight")
            Prelude.<*> (x Data..:? "Protocol")
      )

instance Prelude.Hashable OriginationRoute where
  hashWithSalt _salt OriginationRoute' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData OriginationRoute where
  rnf OriginationRoute' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON OriginationRoute where
  toJSON OriginationRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("Host" Data..=) Prelude.<$> host,
            ("Priority" Data..=) Prelude.<$> priority,
            ("Weight" Data..=) Prelude.<$> weight,
            ("Protocol" Data..=) Prelude.<$> protocol
          ]
      )
