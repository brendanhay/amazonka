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
-- Module      : Network.AWS.DirectConnect.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connection where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.MacSecKey
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Direct Connect connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connection supports MAC Security (MACsec).
    macSecCapable :: Prelude.Maybe Prelude.Bool,
    -- | The MAC Security (MACsec) port link status of the connection.
    --
    -- The valid values are @Encryption Up@, which means that there is an
    -- active Connection Key Name, or @Encryption Down@.
    portEncryptionStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int,
    -- | The location of the connection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connection supports a secondary BGP peer in the
    -- same address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent call to DescribeLoa for this connection.
    loaIssueTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the Direct Connect service provider associated with the
    -- connection.
    partnerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) connection encryption mode.
    --
    -- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the account that owns the connection.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) security keys associated with the connection.
    macSecKeys :: Prelude.Maybe [MacSecKey],
    -- | The name of the service provider associated with the connection.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the physical connection.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The state of the connection. The following are the possible values:
    --
    -- -   @ordering@: The initial state of a hosted connection provisioned on
    --     an interconnect. The connection stays in the ordering state until
    --     the owner of the hosted connection confirms or declines the
    --     connection order.
    --
    -- -   @requested@: The initial state of a standard connection. The
    --     connection stays in the requested state until the Letter of
    --     Authorization (LOA) is sent to the customer.
    --
    -- -   @pending@: The connection has been approved and is being
    --     initialized.
    --
    -- -   @available@: The network link is up and the connection is ready for
    --     use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The connection is being deleted.
    --
    -- -   @deleted@: The connection has been deleted.
    --
    -- -   @rejected@: A hosted connection in the @ordering@ state enters the
    --     @rejected@ state if it is deleted by the customer.
    --
    -- -   @unknown@: The state of the connection is not available.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The tags associated with the connection.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'connection_lagId' - The ID of the LAG.
--
-- 'macSecCapable', 'connection_macSecCapable' - Indicates whether the connection supports MAC Security (MACsec).
--
-- 'portEncryptionStatus', 'connection_portEncryptionStatus' - The MAC Security (MACsec) port link status of the connection.
--
-- The valid values are @Encryption Up@, which means that there is an
-- active Connection Key Name, or @Encryption Down@.
--
-- 'vlan', 'connection_vlan' - The ID of the VLAN.
--
-- 'location', 'connection_location' - The location of the connection.
--
-- 'awsDevice', 'connection_awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'hasLogicalRedundancy', 'connection_hasLogicalRedundancy' - Indicates whether the connection supports a secondary BGP peer in the
-- same address family (IPv4\/IPv6).
--
-- 'connectionId', 'connection_connectionId' - The ID of the connection.
--
-- 'awsLogicalDeviceId', 'connection_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'loaIssueTime', 'connection_loaIssueTime' - The time of the most recent call to DescribeLoa for this connection.
--
-- 'partnerName', 'connection_partnerName' - The name of the Direct Connect service provider associated with the
-- connection.
--
-- 'connectionName', 'connection_connectionName' - The name of the connection.
--
-- 'encryptionMode', 'connection_encryptionMode' - The MAC Security (MACsec) connection encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
--
-- 'bandwidth', 'connection_bandwidth' - The bandwidth of the connection.
--
-- 'jumboFrameCapable', 'connection_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'ownerAccount', 'connection_ownerAccount' - The ID of the account that owns the connection.
--
-- 'region', 'connection_region' - The Region where the connection is located.
--
-- 'macSecKeys', 'connection_macSecKeys' - The MAC Security (MACsec) security keys associated with the connection.
--
-- 'providerName', 'connection_providerName' - The name of the service provider associated with the connection.
--
-- 'awsDeviceV2', 'connection_awsDeviceV2' - The Direct Connect endpoint that terminates the physical connection.
--
-- 'connectionState', 'connection_connectionState' - The state of the connection. The following are the possible values:
--
-- -   @ordering@: The initial state of a hosted connection provisioned on
--     an interconnect. The connection stays in the ordering state until
--     the owner of the hosted connection confirms or declines the
--     connection order.
--
-- -   @requested@: The initial state of a standard connection. The
--     connection stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
--
-- -   @pending@: The connection has been approved and is being
--     initialized.
--
-- -   @available@: The network link is up and the connection is ready for
--     use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The connection is being deleted.
--
-- -   @deleted@: The connection has been deleted.
--
-- -   @rejected@: A hosted connection in the @ordering@ state enters the
--     @rejected@ state if it is deleted by the customer.
--
-- -   @unknown@: The state of the connection is not available.
--
-- 'tags', 'connection_tags' - The tags associated with the connection.
newConnection ::
  Connection
newConnection =
  Connection'
    { lagId = Prelude.Nothing,
      macSecCapable = Prelude.Nothing,
      portEncryptionStatus = Prelude.Nothing,
      vlan = Prelude.Nothing,
      location = Prelude.Nothing,
      awsDevice = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      loaIssueTime = Prelude.Nothing,
      partnerName = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      bandwidth = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      region = Prelude.Nothing,
      macSecKeys = Prelude.Nothing,
      providerName = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the LAG.
connection_lagId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_lagId = Lens.lens (\Connection' {lagId} -> lagId) (\s@Connection' {} a -> s {lagId = a} :: Connection)

-- | Indicates whether the connection supports MAC Security (MACsec).
connection_macSecCapable :: Lens.Lens' Connection (Prelude.Maybe Prelude.Bool)
connection_macSecCapable = Lens.lens (\Connection' {macSecCapable} -> macSecCapable) (\s@Connection' {} a -> s {macSecCapable = a} :: Connection)

-- | The MAC Security (MACsec) port link status of the connection.
--
-- The valid values are @Encryption Up@, which means that there is an
-- active Connection Key Name, or @Encryption Down@.
connection_portEncryptionStatus :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_portEncryptionStatus = Lens.lens (\Connection' {portEncryptionStatus} -> portEncryptionStatus) (\s@Connection' {} a -> s {portEncryptionStatus = a} :: Connection)

-- | The ID of the VLAN.
connection_vlan :: Lens.Lens' Connection (Prelude.Maybe Prelude.Int)
connection_vlan = Lens.lens (\Connection' {vlan} -> vlan) (\s@Connection' {} a -> s {vlan = a} :: Connection)

-- | The location of the connection.
connection_location :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_location = Lens.lens (\Connection' {location} -> location) (\s@Connection' {} a -> s {location = a} :: Connection)

-- | The Direct Connect endpoint on which the physical connection terminates.
connection_awsDevice :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_awsDevice = Lens.lens (\Connection' {awsDevice} -> awsDevice) (\s@Connection' {} a -> s {awsDevice = a} :: Connection)

-- | Indicates whether the connection supports a secondary BGP peer in the
-- same address family (IPv4\/IPv6).
connection_hasLogicalRedundancy :: Lens.Lens' Connection (Prelude.Maybe HasLogicalRedundancy)
connection_hasLogicalRedundancy = Lens.lens (\Connection' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Connection' {} a -> s {hasLogicalRedundancy = a} :: Connection)

-- | The ID of the connection.
connection_connectionId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionId = Lens.lens (\Connection' {connectionId} -> connectionId) (\s@Connection' {} a -> s {connectionId = a} :: Connection)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
connection_awsLogicalDeviceId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_awsLogicalDeviceId = Lens.lens (\Connection' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@Connection' {} a -> s {awsLogicalDeviceId = a} :: Connection)

-- | The time of the most recent call to DescribeLoa for this connection.
connection_loaIssueTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_loaIssueTime = Lens.lens (\Connection' {loaIssueTime} -> loaIssueTime) (\s@Connection' {} a -> s {loaIssueTime = a} :: Connection) Prelude.. Lens.mapping Core._Time

-- | The name of the Direct Connect service provider associated with the
-- connection.
connection_partnerName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_partnerName = Lens.lens (\Connection' {partnerName} -> partnerName) (\s@Connection' {} a -> s {partnerName = a} :: Connection)

-- | The name of the connection.
connection_connectionName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionName = Lens.lens (\Connection' {connectionName} -> connectionName) (\s@Connection' {} a -> s {connectionName = a} :: Connection)

-- | The MAC Security (MACsec) connection encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
connection_encryptionMode :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_encryptionMode = Lens.lens (\Connection' {encryptionMode} -> encryptionMode) (\s@Connection' {} a -> s {encryptionMode = a} :: Connection)

-- | The bandwidth of the connection.
connection_bandwidth :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_bandwidth = Lens.lens (\Connection' {bandwidth} -> bandwidth) (\s@Connection' {} a -> s {bandwidth = a} :: Connection)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
connection_jumboFrameCapable :: Lens.Lens' Connection (Prelude.Maybe Prelude.Bool)
connection_jumboFrameCapable = Lens.lens (\Connection' {jumboFrameCapable} -> jumboFrameCapable) (\s@Connection' {} a -> s {jumboFrameCapable = a} :: Connection)

-- | The ID of the account that owns the connection.
connection_ownerAccount :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_ownerAccount = Lens.lens (\Connection' {ownerAccount} -> ownerAccount) (\s@Connection' {} a -> s {ownerAccount = a} :: Connection)

-- | The Region where the connection is located.
connection_region :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_region = Lens.lens (\Connection' {region} -> region) (\s@Connection' {} a -> s {region = a} :: Connection)

-- | The MAC Security (MACsec) security keys associated with the connection.
connection_macSecKeys :: Lens.Lens' Connection (Prelude.Maybe [MacSecKey])
connection_macSecKeys = Lens.lens (\Connection' {macSecKeys} -> macSecKeys) (\s@Connection' {} a -> s {macSecKeys = a} :: Connection) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service provider associated with the connection.
connection_providerName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_providerName = Lens.lens (\Connection' {providerName} -> providerName) (\s@Connection' {} a -> s {providerName = a} :: Connection)

-- | The Direct Connect endpoint that terminates the physical connection.
connection_awsDeviceV2 :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_awsDeviceV2 = Lens.lens (\Connection' {awsDeviceV2} -> awsDeviceV2) (\s@Connection' {} a -> s {awsDeviceV2 = a} :: Connection)

-- | The state of the connection. The following are the possible values:
--
-- -   @ordering@: The initial state of a hosted connection provisioned on
--     an interconnect. The connection stays in the ordering state until
--     the owner of the hosted connection confirms or declines the
--     connection order.
--
-- -   @requested@: The initial state of a standard connection. The
--     connection stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
--
-- -   @pending@: The connection has been approved and is being
--     initialized.
--
-- -   @available@: The network link is up and the connection is ready for
--     use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The connection is being deleted.
--
-- -   @deleted@: The connection has been deleted.
--
-- -   @rejected@: A hosted connection in the @ordering@ state enters the
--     @rejected@ state if it is deleted by the customer.
--
-- -   @unknown@: The state of the connection is not available.
connection_connectionState :: Lens.Lens' Connection (Prelude.Maybe ConnectionState)
connection_connectionState = Lens.lens (\Connection' {connectionState} -> connectionState) (\s@Connection' {} a -> s {connectionState = a} :: Connection)

-- | The tags associated with the connection.
connection_tags :: Lens.Lens' Connection (Prelude.Maybe (Prelude.NonEmpty Tag))
connection_tags = Lens.lens (\Connection' {tags} -> tags) (\s@Connection' {} a -> s {tags = a} :: Connection) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Core..:? "lagId")
            Prelude.<*> (x Core..:? "macSecCapable")
            Prelude.<*> (x Core..:? "portEncryptionStatus")
            Prelude.<*> (x Core..:? "vlan")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "awsDevice")
            Prelude.<*> (x Core..:? "hasLogicalRedundancy")
            Prelude.<*> (x Core..:? "connectionId")
            Prelude.<*> (x Core..:? "awsLogicalDeviceId")
            Prelude.<*> (x Core..:? "loaIssueTime")
            Prelude.<*> (x Core..:? "partnerName")
            Prelude.<*> (x Core..:? "connectionName")
            Prelude.<*> (x Core..:? "encryptionMode")
            Prelude.<*> (x Core..:? "bandwidth")
            Prelude.<*> (x Core..:? "jumboFrameCapable")
            Prelude.<*> (x Core..:? "ownerAccount")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "macSecKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "providerName")
            Prelude.<*> (x Core..:? "awsDeviceV2")
            Prelude.<*> (x Core..:? "connectionState")
            Prelude.<*> (x Core..:? "tags")
      )

instance Prelude.Hashable Connection

instance Prelude.NFData Connection
