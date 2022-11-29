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
-- Module      : Amazonka.Greengrass.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.ConnectivityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a Greengrass core\'s connectivity.
--
-- /See:/ 'newConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { -- | The port of the Greengrass core. Usually 8883.
    portNumber :: Prelude.Maybe Prelude.Int,
    -- | Metadata for this endpoint.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connectivity information.
    id :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the Greengrass core. Can be an IP address or DNS.
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
-- 'portNumber', 'connectivityInfo_portNumber' - The port of the Greengrass core. Usually 8883.
--
-- 'metadata', 'connectivityInfo_metadata' - Metadata for this endpoint.
--
-- 'id', 'connectivityInfo_id' - The ID of the connectivity information.
--
-- 'hostAddress', 'connectivityInfo_hostAddress' - The endpoint for the Greengrass core. Can be an IP address or DNS.
newConnectivityInfo ::
  ConnectivityInfo
newConnectivityInfo =
  ConnectivityInfo'
    { portNumber = Prelude.Nothing,
      metadata = Prelude.Nothing,
      id = Prelude.Nothing,
      hostAddress = Prelude.Nothing
    }

-- | The port of the Greengrass core. Usually 8883.
connectivityInfo_portNumber :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Int)
connectivityInfo_portNumber = Lens.lens (\ConnectivityInfo' {portNumber} -> portNumber) (\s@ConnectivityInfo' {} a -> s {portNumber = a} :: ConnectivityInfo)

-- | Metadata for this endpoint.
connectivityInfo_metadata :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_metadata = Lens.lens (\ConnectivityInfo' {metadata} -> metadata) (\s@ConnectivityInfo' {} a -> s {metadata = a} :: ConnectivityInfo)

-- | The ID of the connectivity information.
connectivityInfo_id :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_id = Lens.lens (\ConnectivityInfo' {id} -> id) (\s@ConnectivityInfo' {} a -> s {id = a} :: ConnectivityInfo)

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
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
