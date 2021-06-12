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
-- Module      : Network.AWS.Greengrass.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectivityInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a Greengrass core\'s connectivity.
--
-- /See:/ 'newConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { -- | The ID of the connectivity information.
    id :: Core.Maybe Core.Text,
    -- | Metadata for this endpoint.
    metadata :: Core.Maybe Core.Text,
    -- | The port of the Greengrass core. Usually 8883.
    portNumber :: Core.Maybe Core.Int,
    -- | The endpoint for the Greengrass core. Can be an IP address or DNS.
    hostAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectivityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'connectivityInfo_id' - The ID of the connectivity information.
--
-- 'metadata', 'connectivityInfo_metadata' - Metadata for this endpoint.
--
-- 'portNumber', 'connectivityInfo_portNumber' - The port of the Greengrass core. Usually 8883.
--
-- 'hostAddress', 'connectivityInfo_hostAddress' - The endpoint for the Greengrass core. Can be an IP address or DNS.
newConnectivityInfo ::
  ConnectivityInfo
newConnectivityInfo =
  ConnectivityInfo'
    { id = Core.Nothing,
      metadata = Core.Nothing,
      portNumber = Core.Nothing,
      hostAddress = Core.Nothing
    }

-- | The ID of the connectivity information.
connectivityInfo_id :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
connectivityInfo_id = Lens.lens (\ConnectivityInfo' {id} -> id) (\s@ConnectivityInfo' {} a -> s {id = a} :: ConnectivityInfo)

-- | Metadata for this endpoint.
connectivityInfo_metadata :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
connectivityInfo_metadata = Lens.lens (\ConnectivityInfo' {metadata} -> metadata) (\s@ConnectivityInfo' {} a -> s {metadata = a} :: ConnectivityInfo)

-- | The port of the Greengrass core. Usually 8883.
connectivityInfo_portNumber :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Int)
connectivityInfo_portNumber = Lens.lens (\ConnectivityInfo' {portNumber} -> portNumber) (\s@ConnectivityInfo' {} a -> s {portNumber = a} :: ConnectivityInfo)

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
connectivityInfo_hostAddress :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
connectivityInfo_hostAddress = Lens.lens (\ConnectivityInfo' {hostAddress} -> hostAddress) (\s@ConnectivityInfo' {} a -> s {hostAddress = a} :: ConnectivityInfo)

instance Core.FromJSON ConnectivityInfo where
  parseJSON =
    Core.withObject
      "ConnectivityInfo"
      ( \x ->
          ConnectivityInfo'
            Core.<$> (x Core..:? "Id")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (x Core..:? "PortNumber")
            Core.<*> (x Core..:? "HostAddress")
      )

instance Core.Hashable ConnectivityInfo

instance Core.NFData ConnectivityInfo

instance Core.ToJSON ConnectivityInfo where
  toJSON ConnectivityInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Id" Core..=) Core.<$> id,
            ("Metadata" Core..=) Core.<$> metadata,
            ("PortNumber" Core..=) Core.<$> portNumber,
            ("HostAddress" Core..=) Core.<$> hostAddress
          ]
      )
