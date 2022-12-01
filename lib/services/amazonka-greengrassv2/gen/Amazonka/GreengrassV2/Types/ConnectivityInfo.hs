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
-- Module      : Amazonka.GreengrassV2.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ConnectivityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an endpoint and port where client devices can
-- connect to an MQTT broker on a Greengrass core device.
--
-- /See:/ 'newConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { -- | The port where the MQTT broker operates on the core device. This port is
    -- typically 8883, which is the default port for the MQTT broker component
    -- that runs on core devices.
    portNumber :: Prelude.Maybe Prelude.Natural,
    -- | Additional metadata to provide to client devices that connect to this
    -- core device.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | An ID for the connectivity information.
    id :: Prelude.Maybe Prelude.Text,
    -- | The IP address or DNS address where client devices can connect to an
    -- MQTT broker on the Greengrass core device.
    hostAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectivityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portNumber', 'connectivityInfo_portNumber' - The port where the MQTT broker operates on the core device. This port is
-- typically 8883, which is the default port for the MQTT broker component
-- that runs on core devices.
--
-- 'metadata', 'connectivityInfo_metadata' - Additional metadata to provide to client devices that connect to this
-- core device.
--
-- 'id', 'connectivityInfo_id' - An ID for the connectivity information.
--
-- 'hostAddress', 'connectivityInfo_hostAddress' - The IP address or DNS address where client devices can connect to an
-- MQTT broker on the Greengrass core device.
newConnectivityInfo ::
  ConnectivityInfo
newConnectivityInfo =
  ConnectivityInfo'
    { portNumber = Prelude.Nothing,
      metadata = Prelude.Nothing,
      id = Prelude.Nothing,
      hostAddress = Prelude.Nothing
    }

-- | The port where the MQTT broker operates on the core device. This port is
-- typically 8883, which is the default port for the MQTT broker component
-- that runs on core devices.
connectivityInfo_portNumber :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Natural)
connectivityInfo_portNumber = Lens.lens (\ConnectivityInfo' {portNumber} -> portNumber) (\s@ConnectivityInfo' {} a -> s {portNumber = a} :: ConnectivityInfo)

-- | Additional metadata to provide to client devices that connect to this
-- core device.
connectivityInfo_metadata :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_metadata = Lens.lens (\ConnectivityInfo' {metadata} -> metadata) (\s@ConnectivityInfo' {} a -> s {metadata = a} :: ConnectivityInfo)

-- | An ID for the connectivity information.
connectivityInfo_id :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_id = Lens.lens (\ConnectivityInfo' {id} -> id) (\s@ConnectivityInfo' {} a -> s {id = a} :: ConnectivityInfo)

-- | The IP address or DNS address where client devices can connect to an
-- MQTT broker on the Greengrass core device.
connectivityInfo_hostAddress :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_hostAddress = Lens.lens (\ConnectivityInfo' {hostAddress} -> hostAddress) (\s@ConnectivityInfo' {} a -> s {hostAddress = a} :: ConnectivityInfo)

instance Core.FromJSON ConnectivityInfo where
  parseJSON =
    Core.withObject
      "ConnectivityInfo"
      ( \x ->
          ConnectivityInfo'
            Prelude.<$> (x Core..:? "PortNumber")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "HostAddress")
      )

instance Prelude.Hashable ConnectivityInfo where
  hashWithSalt _salt ConnectivityInfo' {..} =
    _salt `Prelude.hashWithSalt` portNumber
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` hostAddress

instance Prelude.NFData ConnectivityInfo where
  rnf ConnectivityInfo' {..} =
    Prelude.rnf portNumber
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf hostAddress

instance Core.ToJSON ConnectivityInfo where
  toJSON ConnectivityInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortNumber" Core..=) Prelude.<$> portNumber,
            ("Metadata" Core..=) Prelude.<$> metadata,
            ("Id" Core..=) Prelude.<$> id,
            ("HostAddress" Core..=) Prelude.<$> hostAddress
          ]
      )
