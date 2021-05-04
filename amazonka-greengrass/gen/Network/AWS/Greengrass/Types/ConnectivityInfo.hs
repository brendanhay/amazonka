{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Greengrass core\'s connectivity.
--
-- /See:/ 'newConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { -- | The ID of the connectivity information.
    id :: Prelude.Maybe Prelude.Text,
    -- | Metadata for this endpoint.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The port of the Greengrass core. Usually 8883.
    portNumber :: Prelude.Maybe Prelude.Int,
    -- | The endpoint for the Greengrass core. Can be an IP address or DNS.
    hostAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { id = Prelude.Nothing,
      metadata = Prelude.Nothing,
      portNumber = Prelude.Nothing,
      hostAddress = Prelude.Nothing
    }

-- | The ID of the connectivity information.
connectivityInfo_id :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_id = Lens.lens (\ConnectivityInfo' {id} -> id) (\s@ConnectivityInfo' {} a -> s {id = a} :: ConnectivityInfo)

-- | Metadata for this endpoint.
connectivityInfo_metadata :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_metadata = Lens.lens (\ConnectivityInfo' {metadata} -> metadata) (\s@ConnectivityInfo' {} a -> s {metadata = a} :: ConnectivityInfo)

-- | The port of the Greengrass core. Usually 8883.
connectivityInfo_portNumber :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Int)
connectivityInfo_portNumber = Lens.lens (\ConnectivityInfo' {portNumber} -> portNumber) (\s@ConnectivityInfo' {} a -> s {portNumber = a} :: ConnectivityInfo)

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
connectivityInfo_hostAddress :: Lens.Lens' ConnectivityInfo (Prelude.Maybe Prelude.Text)
connectivityInfo_hostAddress = Lens.lens (\ConnectivityInfo' {hostAddress} -> hostAddress) (\s@ConnectivityInfo' {} a -> s {hostAddress = a} :: ConnectivityInfo)

instance Prelude.FromJSON ConnectivityInfo where
  parseJSON =
    Prelude.withObject
      "ConnectivityInfo"
      ( \x ->
          ConnectivityInfo'
            Prelude.<$> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Metadata")
            Prelude.<*> (x Prelude..:? "PortNumber")
            Prelude.<*> (x Prelude..:? "HostAddress")
      )

instance Prelude.Hashable ConnectivityInfo

instance Prelude.NFData ConnectivityInfo

instance Prelude.ToJSON ConnectivityInfo where
  toJSON ConnectivityInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Id" Prelude..=) Prelude.<$> id,
            ("Metadata" Prelude..=) Prelude.<$> metadata,
            ("PortNumber" Prelude..=) Prelude.<$> portNumber,
            ("HostAddress" Prelude..=) Prelude.<$> hostAddress
          ]
      )
