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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.OriginationRoute where

import Amazonka.Chime.Types.OriginationRouteProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | The priority associated with the host, with 1 being the highest
    -- priority. Higher priority hosts are attempted first.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The weight associated with the host. If hosts are equal in priority,
    -- calls are redistributed among them based on their relative weight.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The protocol to use for the origination route. Encryption-enabled Amazon
    -- Chime Voice Connectors use TCP protocol by default.
    protocol :: Prelude.Maybe OriginationRouteProtocol,
    -- | The FQDN or IP address to contact for origination traffic.
    host :: Prelude.Maybe Prelude.Text,
    -- | The designated origination route port. Defaults to 5060.
    port :: Prelude.Maybe Prelude.Natural
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
-- 'priority', 'originationRoute_priority' - The priority associated with the host, with 1 being the highest
-- priority. Higher priority hosts are attempted first.
--
-- 'weight', 'originationRoute_weight' - The weight associated with the host. If hosts are equal in priority,
-- calls are redistributed among them based on their relative weight.
--
-- 'protocol', 'originationRoute_protocol' - The protocol to use for the origination route. Encryption-enabled Amazon
-- Chime Voice Connectors use TCP protocol by default.
--
-- 'host', 'originationRoute_host' - The FQDN or IP address to contact for origination traffic.
--
-- 'port', 'originationRoute_port' - The designated origination route port. Defaults to 5060.
newOriginationRoute ::
  OriginationRoute
newOriginationRoute =
  OriginationRoute'
    { priority = Prelude.Nothing,
      weight = Prelude.Nothing,
      protocol = Prelude.Nothing,
      host = Prelude.Nothing,
      port = Prelude.Nothing
    }

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

-- | The FQDN or IP address to contact for origination traffic.
originationRoute_host :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Text)
originationRoute_host = Lens.lens (\OriginationRoute' {host} -> host) (\s@OriginationRoute' {} a -> s {host = a} :: OriginationRoute)

-- | The designated origination route port. Defaults to 5060.
originationRoute_port :: Lens.Lens' OriginationRoute (Prelude.Maybe Prelude.Natural)
originationRoute_port = Lens.lens (\OriginationRoute' {port} -> port) (\s@OriginationRoute' {} a -> s {port = a} :: OriginationRoute)

instance Core.FromJSON OriginationRoute where
  parseJSON =
    Core.withObject
      "OriginationRoute"
      ( \x ->
          OriginationRoute'
            Prelude.<$> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "Weight")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "Host")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable OriginationRoute

instance Prelude.NFData OriginationRoute

instance Core.ToJSON OriginationRoute where
  toJSON OriginationRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Priority" Core..=) Prelude.<$> priority,
            ("Weight" Core..=) Prelude.<$> weight,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("Host" Core..=) Prelude.<$> host,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
