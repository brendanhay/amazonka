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
-- Module      : Amazonka.IoTSecureTunneling.Types.Tunnel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.Tunnel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types.ConnectionState
import Amazonka.IoTSecureTunneling.Types.DestinationConfig
import Amazonka.IoTSecureTunneling.Types.Tag
import Amazonka.IoTSecureTunneling.Types.TimeoutConfig
import Amazonka.IoTSecureTunneling.Types.TunnelStatus
import qualified Amazonka.Prelude as Prelude

-- | A connection between a source computer and a destination device.
--
-- /See:/ 'newTunnel' smart constructor.
data Tunnel = Tunnel'
  { -- | A list of tag metadata associated with the secure tunnel.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The last time the tunnel was updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The connection state of the destination application.
    destinationConnectionState :: Prelude.Maybe ConnectionState,
    -- | The status of a tunnel. Valid values are: Open and Closed.
    status :: Prelude.Maybe TunnelStatus,
    -- | A description of the tunnel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The destination configuration that specifies the thing name of the
    -- destination device and a service name that the local proxy uses to
    -- connect to the destination application.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | A unique alpha-numeric ID that identifies a tunnel.
    tunnelId :: Prelude.Maybe Prelude.Text,
    -- | Timeout configuration for the tunnel.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | The Amazon Resource Name (ARN) of a tunnel.
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the tunnel was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The connection state of the source application.
    sourceConnectionState :: Prelude.Maybe ConnectionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tunnel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'tunnel_tags' - A list of tag metadata associated with the secure tunnel.
--
-- 'lastUpdatedAt', 'tunnel_lastUpdatedAt' - The last time the tunnel was updated.
--
-- 'destinationConnectionState', 'tunnel_destinationConnectionState' - The connection state of the destination application.
--
-- 'status', 'tunnel_status' - The status of a tunnel. Valid values are: Open and Closed.
--
-- 'description', 'tunnel_description' - A description of the tunnel.
--
-- 'destinationConfig', 'tunnel_destinationConfig' - The destination configuration that specifies the thing name of the
-- destination device and a service name that the local proxy uses to
-- connect to the destination application.
--
-- 'tunnelId', 'tunnel_tunnelId' - A unique alpha-numeric ID that identifies a tunnel.
--
-- 'timeoutConfig', 'tunnel_timeoutConfig' - Timeout configuration for the tunnel.
--
-- 'tunnelArn', 'tunnel_tunnelArn' - The Amazon Resource Name (ARN) of a tunnel.
--
-- 'createdAt', 'tunnel_createdAt' - The time when the tunnel was created.
--
-- 'sourceConnectionState', 'tunnel_sourceConnectionState' - The connection state of the source application.
newTunnel ::
  Tunnel
newTunnel =
  Tunnel'
    { tags = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      destinationConnectionState = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      tunnelId = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      sourceConnectionState = Prelude.Nothing
    }

-- | A list of tag metadata associated with the secure tunnel.
tunnel_tags :: Lens.Lens' Tunnel (Prelude.Maybe (Prelude.NonEmpty Tag))
tunnel_tags = Lens.lens (\Tunnel' {tags} -> tags) (\s@Tunnel' {} a -> s {tags = a} :: Tunnel) Prelude.. Lens.mapping Lens.coerced

-- | The last time the tunnel was updated.
tunnel_lastUpdatedAt :: Lens.Lens' Tunnel (Prelude.Maybe Prelude.UTCTime)
tunnel_lastUpdatedAt = Lens.lens (\Tunnel' {lastUpdatedAt} -> lastUpdatedAt) (\s@Tunnel' {} a -> s {lastUpdatedAt = a} :: Tunnel) Prelude.. Lens.mapping Data._Time

-- | The connection state of the destination application.
tunnel_destinationConnectionState :: Lens.Lens' Tunnel (Prelude.Maybe ConnectionState)
tunnel_destinationConnectionState = Lens.lens (\Tunnel' {destinationConnectionState} -> destinationConnectionState) (\s@Tunnel' {} a -> s {destinationConnectionState = a} :: Tunnel)

-- | The status of a tunnel. Valid values are: Open and Closed.
tunnel_status :: Lens.Lens' Tunnel (Prelude.Maybe TunnelStatus)
tunnel_status = Lens.lens (\Tunnel' {status} -> status) (\s@Tunnel' {} a -> s {status = a} :: Tunnel)

-- | A description of the tunnel.
tunnel_description :: Lens.Lens' Tunnel (Prelude.Maybe Prelude.Text)
tunnel_description = Lens.lens (\Tunnel' {description} -> description) (\s@Tunnel' {} a -> s {description = a} :: Tunnel)

-- | The destination configuration that specifies the thing name of the
-- destination device and a service name that the local proxy uses to
-- connect to the destination application.
tunnel_destinationConfig :: Lens.Lens' Tunnel (Prelude.Maybe DestinationConfig)
tunnel_destinationConfig = Lens.lens (\Tunnel' {destinationConfig} -> destinationConfig) (\s@Tunnel' {} a -> s {destinationConfig = a} :: Tunnel)

-- | A unique alpha-numeric ID that identifies a tunnel.
tunnel_tunnelId :: Lens.Lens' Tunnel (Prelude.Maybe Prelude.Text)
tunnel_tunnelId = Lens.lens (\Tunnel' {tunnelId} -> tunnelId) (\s@Tunnel' {} a -> s {tunnelId = a} :: Tunnel)

-- | Timeout configuration for the tunnel.
tunnel_timeoutConfig :: Lens.Lens' Tunnel (Prelude.Maybe TimeoutConfig)
tunnel_timeoutConfig = Lens.lens (\Tunnel' {timeoutConfig} -> timeoutConfig) (\s@Tunnel' {} a -> s {timeoutConfig = a} :: Tunnel)

-- | The Amazon Resource Name (ARN) of a tunnel.
tunnel_tunnelArn :: Lens.Lens' Tunnel (Prelude.Maybe Prelude.Text)
tunnel_tunnelArn = Lens.lens (\Tunnel' {tunnelArn} -> tunnelArn) (\s@Tunnel' {} a -> s {tunnelArn = a} :: Tunnel)

-- | The time when the tunnel was created.
tunnel_createdAt :: Lens.Lens' Tunnel (Prelude.Maybe Prelude.UTCTime)
tunnel_createdAt = Lens.lens (\Tunnel' {createdAt} -> createdAt) (\s@Tunnel' {} a -> s {createdAt = a} :: Tunnel) Prelude.. Lens.mapping Data._Time

-- | The connection state of the source application.
tunnel_sourceConnectionState :: Lens.Lens' Tunnel (Prelude.Maybe ConnectionState)
tunnel_sourceConnectionState = Lens.lens (\Tunnel' {sourceConnectionState} -> sourceConnectionState) (\s@Tunnel' {} a -> s {sourceConnectionState = a} :: Tunnel)

instance Data.FromJSON Tunnel where
  parseJSON =
    Data.withObject
      "Tunnel"
      ( \x ->
          Tunnel'
            Prelude.<$> (x Data..:? "tags")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "destinationConnectionState")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "destinationConfig")
            Prelude.<*> (x Data..:? "tunnelId")
            Prelude.<*> (x Data..:? "timeoutConfig")
            Prelude.<*> (x Data..:? "tunnelArn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "sourceConnectionState")
      )

instance Prelude.Hashable Tunnel where
  hashWithSalt _salt Tunnel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` destinationConnectionState
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` tunnelId
      `Prelude.hashWithSalt` timeoutConfig
      `Prelude.hashWithSalt` tunnelArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` sourceConnectionState

instance Prelude.NFData Tunnel where
  rnf Tunnel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf destinationConnectionState
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf tunnelId
      `Prelude.seq` Prelude.rnf timeoutConfig
      `Prelude.seq` Prelude.rnf tunnelArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf sourceConnectionState
