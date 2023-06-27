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
-- Module      : Amazonka.DirectConnect.Types.Lag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Lag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.Connection
import Amazonka.DirectConnect.Types.HasLogicalRedundancy
import Amazonka.DirectConnect.Types.LagState
import Amazonka.DirectConnect.Types.MacSecKey
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a link aggregation group (LAG).
--
-- /See:/ 'newLag' smart constructor.
data Lag = Lag'
  { -- | Indicates whether the LAG can host other connections.
    allowsHostedConnections :: Prelude.Maybe Prelude.Bool,
    -- | The Direct Connect endpoint that hosts the LAG.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that hosts the LAG.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The connections bundled by the LAG.
    connections :: Prelude.Maybe [Connection],
    -- | The individual bandwidth of the physical connections bundled by the LAG.
    -- The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Prelude.Maybe Prelude.Text,
    -- | The LAG MAC Security (MACsec) encryption mode.
    --
    -- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the LAG supports a secondary BGP peer in the same
    -- address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | Indicates whether jumbo frames are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The name of the LAG.
    lagName :: Prelude.Maybe Prelude.Text,
    -- | The state of the LAG. The following are the possible values:
    --
    -- -   @requested@: The initial state of a LAG. The LAG stays in the
    --     requested state until the Letter of Authorization (LOA) is
    --     available.
    --
    -- -   @pending@: The LAG has been approved and is being initialized.
    --
    -- -   @available@: The network link is established and the LAG is ready
    --     for use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The LAG is being deleted.
    --
    -- -   @deleted@: The LAG is deleted.
    --
    -- -   @unknown@: The state of the LAG is not available.
    lagState :: Prelude.Maybe LagState,
    -- | The location of the LAG.
    location :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the LAG supports MAC Security (MACsec).
    macSecCapable :: Prelude.Maybe Prelude.Bool,
    -- | The MAC Security (MACsec) security keys associated with the LAG.
    macSecKeys :: Prelude.Maybe [MacSecKey],
    -- | The minimum number of physical dedicated connections that must be
    -- operational for the LAG itself to be operational.
    minimumLinks :: Prelude.Maybe Prelude.Int,
    -- | The number of physical dedicated connections bundled by the LAG, up to a
    -- maximum of 10.
    numberOfConnections :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Amazon Web Services account that owns the LAG.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the LAG.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the LAG.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Lag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsHostedConnections', 'lag_allowsHostedConnections' - Indicates whether the LAG can host other connections.
--
-- 'awsDevice', 'lag_awsDevice' - The Direct Connect endpoint that hosts the LAG.
--
-- 'awsDeviceV2', 'lag_awsDeviceV2' - The Direct Connect endpoint that hosts the LAG.
--
-- 'awsLogicalDeviceId', 'lag_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'connections', 'lag_connections' - The connections bundled by the LAG.
--
-- 'connectionsBandwidth', 'lag_connectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG.
-- The possible values are 1Gbps and 10Gbps.
--
-- 'encryptionMode', 'lag_encryptionMode' - The LAG MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
--
-- 'hasLogicalRedundancy', 'lag_hasLogicalRedundancy' - Indicates whether the LAG supports a secondary BGP peer in the same
-- address family (IPv4\/IPv6).
--
-- 'jumboFrameCapable', 'lag_jumboFrameCapable' - Indicates whether jumbo frames are supported.
--
-- 'lagId', 'lag_lagId' - The ID of the LAG.
--
-- 'lagName', 'lag_lagName' - The name of the LAG.
--
-- 'lagState', 'lag_lagState' - The state of the LAG. The following are the possible values:
--
-- -   @requested@: The initial state of a LAG. The LAG stays in the
--     requested state until the Letter of Authorization (LOA) is
--     available.
--
-- -   @pending@: The LAG has been approved and is being initialized.
--
-- -   @available@: The network link is established and the LAG is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The LAG is being deleted.
--
-- -   @deleted@: The LAG is deleted.
--
-- -   @unknown@: The state of the LAG is not available.
--
-- 'location', 'lag_location' - The location of the LAG.
--
-- 'macSecCapable', 'lag_macSecCapable' - Indicates whether the LAG supports MAC Security (MACsec).
--
-- 'macSecKeys', 'lag_macSecKeys' - The MAC Security (MACsec) security keys associated with the LAG.
--
-- 'minimumLinks', 'lag_minimumLinks' - The minimum number of physical dedicated connections that must be
-- operational for the LAG itself to be operational.
--
-- 'numberOfConnections', 'lag_numberOfConnections' - The number of physical dedicated connections bundled by the LAG, up to a
-- maximum of 10.
--
-- 'ownerAccount', 'lag_ownerAccount' - The ID of the Amazon Web Services account that owns the LAG.
--
-- 'providerName', 'lag_providerName' - The name of the service provider associated with the LAG.
--
-- 'region', 'lag_region' - The Amazon Web Services Region where the connection is located.
--
-- 'tags', 'lag_tags' - The tags associated with the LAG.
newLag ::
  Lag
newLag =
  Lag'
    { allowsHostedConnections = Prelude.Nothing,
      awsDevice = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      connections = Prelude.Nothing,
      connectionsBandwidth = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      lagId = Prelude.Nothing,
      lagName = Prelude.Nothing,
      lagState = Prelude.Nothing,
      location = Prelude.Nothing,
      macSecCapable = Prelude.Nothing,
      macSecKeys = Prelude.Nothing,
      minimumLinks = Prelude.Nothing,
      numberOfConnections = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      providerName = Prelude.Nothing,
      region = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Indicates whether the LAG can host other connections.
lag_allowsHostedConnections :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_allowsHostedConnections = Lens.lens (\Lag' {allowsHostedConnections} -> allowsHostedConnections) (\s@Lag' {} a -> s {allowsHostedConnections = a} :: Lag)

-- | The Direct Connect endpoint that hosts the LAG.
lag_awsDevice :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsDevice = Lens.lens (\Lag' {awsDevice} -> awsDevice) (\s@Lag' {} a -> s {awsDevice = a} :: Lag)

-- | The Direct Connect endpoint that hosts the LAG.
lag_awsDeviceV2 :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsDeviceV2 = Lens.lens (\Lag' {awsDeviceV2} -> awsDeviceV2) (\s@Lag' {} a -> s {awsDeviceV2 = a} :: Lag)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
lag_awsLogicalDeviceId :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsLogicalDeviceId = Lens.lens (\Lag' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@Lag' {} a -> s {awsLogicalDeviceId = a} :: Lag)

-- | The connections bundled by the LAG.
lag_connections :: Lens.Lens' Lag (Prelude.Maybe [Connection])
lag_connections = Lens.lens (\Lag' {connections} -> connections) (\s@Lag' {} a -> s {connections = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

-- | The individual bandwidth of the physical connections bundled by the LAG.
-- The possible values are 1Gbps and 10Gbps.
lag_connectionsBandwidth :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_connectionsBandwidth = Lens.lens (\Lag' {connectionsBandwidth} -> connectionsBandwidth) (\s@Lag' {} a -> s {connectionsBandwidth = a} :: Lag)

-- | The LAG MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
lag_encryptionMode :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_encryptionMode = Lens.lens (\Lag' {encryptionMode} -> encryptionMode) (\s@Lag' {} a -> s {encryptionMode = a} :: Lag)

-- | Indicates whether the LAG supports a secondary BGP peer in the same
-- address family (IPv4\/IPv6).
lag_hasLogicalRedundancy :: Lens.Lens' Lag (Prelude.Maybe HasLogicalRedundancy)
lag_hasLogicalRedundancy = Lens.lens (\Lag' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Lag' {} a -> s {hasLogicalRedundancy = a} :: Lag)

-- | Indicates whether jumbo frames are supported.
lag_jumboFrameCapable :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_jumboFrameCapable = Lens.lens (\Lag' {jumboFrameCapable} -> jumboFrameCapable) (\s@Lag' {} a -> s {jumboFrameCapable = a} :: Lag)

-- | The ID of the LAG.
lag_lagId :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_lagId = Lens.lens (\Lag' {lagId} -> lagId) (\s@Lag' {} a -> s {lagId = a} :: Lag)

-- | The name of the LAG.
lag_lagName :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_lagName = Lens.lens (\Lag' {lagName} -> lagName) (\s@Lag' {} a -> s {lagName = a} :: Lag)

-- | The state of the LAG. The following are the possible values:
--
-- -   @requested@: The initial state of a LAG. The LAG stays in the
--     requested state until the Letter of Authorization (LOA) is
--     available.
--
-- -   @pending@: The LAG has been approved and is being initialized.
--
-- -   @available@: The network link is established and the LAG is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The LAG is being deleted.
--
-- -   @deleted@: The LAG is deleted.
--
-- -   @unknown@: The state of the LAG is not available.
lag_lagState :: Lens.Lens' Lag (Prelude.Maybe LagState)
lag_lagState = Lens.lens (\Lag' {lagState} -> lagState) (\s@Lag' {} a -> s {lagState = a} :: Lag)

-- | The location of the LAG.
lag_location :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_location = Lens.lens (\Lag' {location} -> location) (\s@Lag' {} a -> s {location = a} :: Lag)

-- | Indicates whether the LAG supports MAC Security (MACsec).
lag_macSecCapable :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_macSecCapable = Lens.lens (\Lag' {macSecCapable} -> macSecCapable) (\s@Lag' {} a -> s {macSecCapable = a} :: Lag)

-- | The MAC Security (MACsec) security keys associated with the LAG.
lag_macSecKeys :: Lens.Lens' Lag (Prelude.Maybe [MacSecKey])
lag_macSecKeys = Lens.lens (\Lag' {macSecKeys} -> macSecKeys) (\s@Lag' {} a -> s {macSecKeys = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

-- | The minimum number of physical dedicated connections that must be
-- operational for the LAG itself to be operational.
lag_minimumLinks :: Lens.Lens' Lag (Prelude.Maybe Prelude.Int)
lag_minimumLinks = Lens.lens (\Lag' {minimumLinks} -> minimumLinks) (\s@Lag' {} a -> s {minimumLinks = a} :: Lag)

-- | The number of physical dedicated connections bundled by the LAG, up to a
-- maximum of 10.
lag_numberOfConnections :: Lens.Lens' Lag (Prelude.Maybe Prelude.Int)
lag_numberOfConnections = Lens.lens (\Lag' {numberOfConnections} -> numberOfConnections) (\s@Lag' {} a -> s {numberOfConnections = a} :: Lag)

-- | The ID of the Amazon Web Services account that owns the LAG.
lag_ownerAccount :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_ownerAccount = Lens.lens (\Lag' {ownerAccount} -> ownerAccount) (\s@Lag' {} a -> s {ownerAccount = a} :: Lag)

-- | The name of the service provider associated with the LAG.
lag_providerName :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_providerName = Lens.lens (\Lag' {providerName} -> providerName) (\s@Lag' {} a -> s {providerName = a} :: Lag)

-- | The Amazon Web Services Region where the connection is located.
lag_region :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_region = Lens.lens (\Lag' {region} -> region) (\s@Lag' {} a -> s {region = a} :: Lag)

-- | The tags associated with the LAG.
lag_tags :: Lens.Lens' Lag (Prelude.Maybe (Prelude.NonEmpty Tag))
lag_tags = Lens.lens (\Lag' {tags} -> tags) (\s@Lag' {} a -> s {tags = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Lag where
  parseJSON =
    Data.withObject
      "Lag"
      ( \x ->
          Lag'
            Prelude.<$> (x Data..:? "allowsHostedConnections")
            Prelude.<*> (x Data..:? "awsDevice")
            Prelude.<*> (x Data..:? "awsDeviceV2")
            Prelude.<*> (x Data..:? "awsLogicalDeviceId")
            Prelude.<*> (x Data..:? "connections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "connectionsBandwidth")
            Prelude.<*> (x Data..:? "encryptionMode")
            Prelude.<*> (x Data..:? "hasLogicalRedundancy")
            Prelude.<*> (x Data..:? "jumboFrameCapable")
            Prelude.<*> (x Data..:? "lagId")
            Prelude.<*> (x Data..:? "lagName")
            Prelude.<*> (x Data..:? "lagState")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "macSecCapable")
            Prelude.<*> (x Data..:? "macSecKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "minimumLinks")
            Prelude.<*> (x Data..:? "numberOfConnections")
            Prelude.<*> (x Data..:? "ownerAccount")
            Prelude.<*> (x Data..:? "providerName")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "tags")
      )

instance Prelude.Hashable Lag where
  hashWithSalt _salt Lag' {..} =
    _salt
      `Prelude.hashWithSalt` allowsHostedConnections
      `Prelude.hashWithSalt` awsDevice
      `Prelude.hashWithSalt` awsDeviceV2
      `Prelude.hashWithSalt` awsLogicalDeviceId
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` connectionsBandwidth
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` hasLogicalRedundancy
      `Prelude.hashWithSalt` jumboFrameCapable
      `Prelude.hashWithSalt` lagId
      `Prelude.hashWithSalt` lagName
      `Prelude.hashWithSalt` lagState
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` macSecCapable
      `Prelude.hashWithSalt` macSecKeys
      `Prelude.hashWithSalt` minimumLinks
      `Prelude.hashWithSalt` numberOfConnections
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Lag where
  rnf Lag' {..} =
    Prelude.rnf allowsHostedConnections
      `Prelude.seq` Prelude.rnf awsDevice
      `Prelude.seq` Prelude.rnf awsDeviceV2
      `Prelude.seq` Prelude.rnf awsLogicalDeviceId
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf connectionsBandwidth
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf hasLogicalRedundancy
      `Prelude.seq` Prelude.rnf jumboFrameCapable
      `Prelude.seq` Prelude.rnf lagId
      `Prelude.seq` Prelude.rnf lagName
      `Prelude.seq` Prelude.rnf lagState
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf macSecCapable
      `Prelude.seq` Prelude.rnf macSecKeys
      `Prelude.seq` Prelude.rnf minimumLinks
      `Prelude.seq` Prelude.rnf numberOfConnections
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf tags
